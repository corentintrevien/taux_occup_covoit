library("data.table")
library("sf")
library("rjson")
library("httr")
library("curl")
library("dplyr")
library("stringr")
library("lubridate")
library("plyr")
library("writexl")
library("readxl")


get_ign_dist <- function(coord){
  coord <- as.numeric(coord)
  url <- paste0("https://itineraire.ign.fr/simple/1.0.0/route?resource=bdtopo-osrm&profile=car&optimization=fastest&start=",
                coord[1],",",coord[2],"&end=",coord[3],",",coord[4],
                "&intermediates=&constraints=&geometryFormat=polyline&getSteps=false&getBbox=false")
  resp <- GET(url, encoding = "UTF-8")
  itin_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  return(itin_json$distance)
}

options(scipen=15)
#Sys.setenv(http_proxy = "proxy-rie.ac.i2:8080")
#Sys.setenv(https_proxy = "proxy-rie.ac.i2:8080")

#Chargement des données du mois de septembre 2021
if(!file.exists("2021-09-vf.csv")){
  curl_download("https://www.data.gouv.fr/fr/datasets/r/d92d1dd8-d826-4e1b-af2e-a1275fa6afda","2021-09-vf.csv", mode="wb")
}

#Lecture des données du mois de septembre 
data_09_2021 <- fread("2021-09-vf.csv",colClasses = c("journey_id"="character","journey_start_insee"="character"))
data_09_2021$journey_start_insee <- str_pad(data_09_2021$journey_start_insee ,5,pad="0")

data_09_2021 <- subset(data_09_2021,select=c("journey_id","trip_id","journey_start_datetime", "journey_start_lon",
                                             "journey_start_lat","journey_end_datetime","journey_end_lon",
                                             "journey_end_lat","passenger_seats","journey_start_insee"))

#Degré de précision de chaques coordonnées 
data_09_2021_char <- fread("2021-09-vf.csv",colClasses = "character")
data_09_2021_char$precision_start <- unlist(lapply(str_split(data_09_2021_char$journey_start_lon,'\\.'),function(l) nchar(l[[2]])))
data_09_2021_char$precision_end <- unlist(lapply(str_split(data_09_2021_char$journey_end_lon,'\\.'),function(l) nchar(l[[2]])))
data_09_2021 <- left_join(data_09_2021,data_09_2021_char[,c("journey_id","precision_start","precision_end")])


#Nombre de trips avec plusieurs journeys
data_09_2021[,nb_journey := .N ,by="trip_id"]
#Si passenger_seat manquant, alors il est fixé à 1 
data_09_2021[is.na(passenger_seats),"passenger_seats"] <- 1
#Nombre d'origine-destination différents dans le trajet
data_09_2021$itin <- with(data_09_2021,paste(journey_start_lon,journey_start_lat,journey_end_lon,journey_end_lat,sep="/"))
data_09_2021[,nb_ori_dest := length(unique(itin)),by="trip_id"]


#Distance à vol d'oiseau de chaque "journey" 
data_09_2021$distance_vo = as.numeric(st_distance(st_as_sf(data_09_2021,coords=c("journey_end_lon","journey_end_lat"),crs=4326),
                                              st_as_sf(data_09_2021,coords=c("journey_start_lon","journey_start_lat"),crs=4326),by_element =T))

#Couples lon,lat différents 
data_09_2021_xy <- data_09_2021[,c("journey_end_lon","journey_end_lat","journey_start_lon","journey_start_lat")]
data_09_2021_xy <- data_09_2021_xy[!duplicated(data_09_2021_xy),]
data_09_2021_xy <- data_09_2021_xy[order(journey_end_lon,journey_end_lat,journey_start_lon,journey_start_lat),]
  
#Distance routière de chaque "journey" à partir de l'API IGN d'itinéraire (itinéraire le plus rapide, en voiture)
dir.create('Itin')
#Séparation des données en "paquets" de 5000 points de départ et d'arrivée 
data_09_2021_xy$paq <- 1:nrow(data_09_2021_xy) %/% 5000

# for(p in 0:9 ){
#   print(paste("Paquet",p))
#   od_lon_lat <- data_09_2021_xy[paq==p,]
#   
#   dist_routiere_ign <- lapply(1:nrow(od_lon_lat),function(l){
#     dcv <- od_lon_lat[l,]
#     url <- with(dcv,paste0("https://itineraire.ign.fr/simple/1.0.0/route?resource=bdtopo-osrm&profile=car&optimization=fastest&start=",
#                            dcv$journey_start_lon,",",dcv$journey_start_lat,"&end=",dcv$journey_end_lon,",",dcv$journey_end_lat,
#                            "&intermediates=&constraints=&geometryFormat=polyline&getSteps=false&getBbox=false"))
#     
#     resp <- GET(url, encoding = "UTF-8")
#     itin_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
#     return(itin_json$distance)
#   })
#   od_lon_lat$distance_ign <- unlist(dist_routiere_ign)
#   print(Sys.time()-deb)
#   fwrite(subset(od_lon_lat,select=-paq),paste0("Itin/distance_ign_",p,".csv.gz"))
# }

distance_ign <- lapply(0:9,function(p) fread(paste0("Itin/distance_ign_",p,".csv.gz")))
distance_ign <- rbindlist(distance_ign)

#Ajout des distances IGN à la table initiale
data_09_2021 <- left_join(data_09_2021,distance_ign)

#La distance du conducteur correspond au trajet le plus long 
data_09_2021[,distance_vo_cond := max(distance_vo),by="trip_id"]
data_09_2021[,distance_ign_cond := max(distance_ign),by="trip_id"]


####Itinéraire de chaque trip et distance chaque journey####

#Méthode 4 : Taux d'occupation par trajet IGN en prenant en compte le trajet 
#Diffère de la méthode précédente uniquement pour les trajets où certains passagers ont des lieux de départ et d'arrivée différents
data_09_2021 <- data_09_2021[order(trip_id),]
#Point de départ et d'arrivée du conducteur : les plus éloignés l'un de l'autre
trip_itin <- data_09_2021[nb_ori_dest>1,]
trip_itin <- split(trip_itin,trip_itin$trip_id)


# x <-10
# trip <- trip_itin[["27b2fe73-7db1-46f7-9397-e1ae83b60310"]]
# 
# #for(x in 0:(length(trip_itin) %/% 1000)){
# for(x in c(0,6)){  
#   dist_itin <- lapply(trip_itin[(1000*x+1):min(1000*(x+1),length(trip_itin))],function(trip){
#     tid <- trip[1,]$trip_id
#     print(paste(tid,x))
#     journey_dist <- c( dist_itin = NA ,dist_itin_tot= NA, journey_id= "ERROR", itin= NA, trip_id =tid)
#     try({
#       trip$journey_start_pt <- with(trip,paste(journey_start_datetime,journey_start_lon,journey_start_lat,sep="/"))
#       trip$journey_end_pt <- with(trip,paste(journey_end_datetime,journey_end_lon,journey_end_lat,sep="/"))
#       
#       step_orig <- plyr::rename(trip[,c("trip_id","journey_start_datetime","journey_start_lon","journey_start_lat","journey_id")]
#                                 ,c("journey_start_datetime"="datetime","journey_start_lon"="lon","journey_start_lat"="lat"))
#       step_orig$type <- "orig"
#       step_dest <- plyr::rename(trip[,c("trip_id","journey_end_datetime","journey_end_lon","journey_end_lat","journey_id")]
#                                 ,c("journey_end_datetime"="datetime","journey_end_lon"="lon","journey_end_lat"="lat"))
#       step_dest$type <- "dest"
#       
#       step <- rbind(step_orig,step_dest)
#       step$pt <- with(step,paste(datetime,lon,lat,sep="/"))
#       step <- step[order(trip_id,datetime),]
#       #Points de passage du trip
#       variantes <- step[,c("datetime","pt")]
#       variantes <- variantes[!duplicated(variantes),]
#       variantes <- split(variantes$pt,variantes$datetime)
#       
#       variantes <- lapply(variantes,function(p) replicate(length(p),p,simplify=FALSE))
#       
#       variantes <- unlist(variantes , recursive=FALSE)
#       variantes <- expand.grid(variantes)
#       #Vérification des itinéraires : passage par tous les points
#       nbs <- apply(variantes,1,function(p) length(unique(p)))
#       variantes <- variantes[nbs==ncol(variantes),]
#       #Vérification des itinéraires : départ d'un journey avant l'arrivée
#       #Les journey j sont-ils bien ordonnés dans la variante v ? 
#       orders <- unlist(lapply(1:nrow(variantes),function(v){
#         min(unlist(lapply(1:nrow(trip),function(j) which(trip$journey_start_pt[j] == variantes[v,]) 
#                           <= which(trip$journey_end_pt[j] == variantes[v,]))))
#       }))
#       variantes <- variantes[orders==1,]
#       #Numérotation des variantes
#       setDT(variantes)
#       variantes[,v:=1:.N]
#       #Passage en colonne 
#       variantes <- melt(variantes,id.vars="v")
#       variantes <- subset(variantes,select = - variable)
#       variantes[,n:=1:.N,by="v"]
#       variantes <- variantes[order(v,n)]
#       #varianteséparation des coordonnées 
#       variantes <- cbind(variantes[duplicated(variantes$v,fromLast=TRUE),c("v","n")],
#                          pt1 = variantes$value[duplicated(variantes$v,fromLast=TRUE)],
#                          pt2 = variantes$value[duplicated(variantes$v)])
#       variantes[,c("dt1","lon1","lat1")] <- as.data.table(do.call(rbind,str_split(variantes$pt1,"/")))
#       variantes[,c("dt2","lon2","lat2")] <- as.data.table(do.call(rbind,str_split(variantes$pt2,"/")))
#       #Distance IGN entre les points 
#       unique_seg <- variantes[,c("lon1","lat1","lon2","lat2")]
#       unique_seg <- unique_seg[!duplicated(unique_seg),]
#       unique_seg$dist_ign <- apply(unique_seg,1,get_ign_dist)
#       variantes <- left_join(variantes,unique_seg, by = c("lon1", "lat1", "lon2", "lat2"))
#       #Sélection de l'itinéraire le plus rapide 
#       variantes[,dist_ign_tot := sum(dist_ign),by="v"]
#       variantes<- variantes[dist_ign_tot == min(dist_ign_tot),]
#       #Reconsitution de la distance chaque journey
#       journey_dist <- lapply(1:nrow(trip),function(j){
#         vj <- variantes[which(variantes$pt1 == trip[j,]$journey_start_pt):which(variantes$pt2 == trip[j,]$journey_end_pt),]
#         vj <- c(journey_id = trip[j,]$journey_id,
#                 trip_id = tid,
#                 dist_itin=sum(vj$dist_ign),dist_itin_tot= vj$dist_ign_tot[1],
#                 itin=paste(vj[1:(nrow(vj)-1),]$pt2,collapse = ";"))
#         return(vj)
#       })
#       journey_dist <- do.call(rbind,journey_dist)
#     })
#     
#     return(journey_dist)})
#   
#   dist_itin <- do.call(rbind,dist_itin)
#   dist_itin <- as.data.frame(dist_itin)
#   
#   fwrite(dist_itin,paste0("Itin/2021-09-vf-itin-",x,".csv.gz"))
# }

#Regroupement des paquets de fichiers
list_itin_files <- sort(list.files("Itin"))
list_itin_files <- list_itin_files[substr(list_itin_files,1,15)=="2021-09-vf-itin"]
data_itin <- lapply(paste0("Itin/",list_itin_files),fread,colClasses= c("journey_id"="character"))
data_itin <- rbindlist(data_itin)
data_itin$dist_itin <- as.numeric(data_itin$dist_itin)

data_09_2021 <- left_join(subset(data_09_2021,select = -c(itin)),subset(data_itin,select = -c(itin)))
#data_09_2021[is.na(dist_itin) & data_09_2021$nb_ori_dest>1,]

data_09_2021$distance_etp <- 
  ifelse(is.na(data_09_2021$dist_itin),data_09_2021$distance_ign,data_09_2021$dist_itin)
data_09_2021$distance_etp_cond <- 
  ifelse(is.na(data_09_2021$dist_itin_tot),data_09_2021$distance_ign_cond,data_09_2021$dist_itin_tot)
data_09_2021 <- subset(data_09_2021,select = -c(dist_itin_tot,dist_itin))
                          
fwrite(data_09_2021,"2021-09-vf-distances.csv.gz")

##############################################
#Effet de l'approximation de 70 ou 700 mètres#
##############################################

data_09_2021$itin_2d <- with(data_09_2021,paste(round(journey_start_lon ,2),round(journey_start_lat,2),
                                                round(journey_end_lon,2),round(journey_end_lat,2),sep="/"))


carreaux_domtrav_RP2017 <- fread("C:/Users/corentin.trevien/Desktop/Velo/Data/carreaux_domtrav_RP2017.csv.gz")
carreaux_domtrav_RP2017$itin_2d <-
  with(carreaux_domtrav_RP2017,paste(round(lon_lr,2),round(lat_lr,2),round(lon_lt,2),round(lat_lt,2),sep="/"))

#Selection des itinéraires présents dans la table de septembre
itin_test_RP2017 <- carreaux_domtrav_RP2017[itin_2d %in% data_09_2021$itin_2d,c("itin_2d","lon_lr","lat_lr","lon_lt","lat_lt")]
itin_test_RP2017 <- itin_test_RP2017[!duplicated(itin_2d),]
itin_test_RP2017$distance_vo <- as.numeric(st_distance(st_as_sf(itin_test_RP2017,coords=c("lon_lr","lat_lr"),crs=4326),
                                                st_as_sf(itin_test_RP2017,coords=c("lon_lt","lat_lt"),crs=4326),by_element =T))

#Calcul des distances selon 3 modalités : 
rp_precision_dist_ign <- lapply(1:nrow(itin_test_RP2017),function(l){
  
  get_ign_dist <- function(coord){
    coord <- as.numeric(coord)
    url <- paste0("https://itineraire.ign.fr/simple/1.0.0/route?resource=bdtopo-osrm&profile=car&optimization=fastest&start=",
                  coord[1],",",coord[2],"&end=",coord[3],",",coord[4],
                  "&intermediates=&constraints=&geometryFormat=polyline&getSteps=false&getBbox=false")
    resp <- GET(url, encoding = "UTF-8")
    itin_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    return(itin_json$distance)
  }
  
  return(c(dist_ign_2d = get_ign_dist(round(itin_test_RP2017[l,c("lon_lr","lat_lr","lon_lt","lat_lt")],2)),
           dist_ign_3d = get_ign_dist(round(itin_test_RP2017[l,c("lon_lr","lat_lr","lon_lt","lat_lt")],3)),
           dist_ign = get_ign_dist(itin_test_RP2017[l,c("lon_lr","lat_lr","lon_lt","lat_lt")]),
           itin_2d = itin_test_RP2017[l,]$itin_2d))
})

rp_precision_dist_ign <- do.call(rbind,rp_precision_dist_ign)
rp_precision_dist_ign <- as.data.table(rp_precision_dist_ign)
rp_precision_dist_ign <- left_join(rp_precision_dist_ign,itin_test_RP2017)
fwrite(rp_precision_dist_ign,"Itin/precision_rp_dist_ign.csv.gz")

###########################
#Statistiques descriptives#
###########################

list_quant <-c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
tr_dist <- c(-Inf,5000,10000,25000,50000,Inf)
label_tr_dist <- c("Moins de 5 km","5 à moins de 10 km","10 à moins de 25 km","25 à moins de 50 km","50 km et plus")

####PRECISION DES DISTANCES####

#Approximation des coordonnées 
precision_rp_dist_ign <- fread("Itin/precision_rp_dist_ign.csv.gz")
precision_rp_dist_ign <- precision_rp_dist_ign[dist_ign>0,]
precision_rp_dist_ign$dist_ign_tr <- cut(precision_rp_dist_ign$dist_ign,tr_dist,label_tr_dist,include.lowest = T)

rp_dist_moy <- aggregate(data=precision_rp_dist_ign,cbind('Distance exacte'=dist_ign,'Coordonnées 3 décimales'=dist_ign_3d,
                                                          'Coordonnées 2 décimales'=dist_ign_2d)~dist_ign_tr,
                         FUN= function(x) round(mean(x)/1000,2))

erreur_rel_2d  <- aggregate(data=precision_rp_dist_ign,
                            cbind('P'=round(100*dist_ign_2d/dist_ign-100,1))~dist_ign_tr,FUN=quantile,
                            probs = list_quant)
erreur_rel_2d <- cbind(dist_ign_tr = erreur_rel_2d$dist_ign_tr,as.data.table(erreur_rel_2d[,2]))


erreur_rel_3d  <- aggregate(data=precision_rp_dist_ign,
                            cbind('P'=round(100*dist_ign_3d/dist_ign-100,1))~dist_ign_tr,FUN=quantile,
                            probs = list_quant)
erreur_rel_3d <- cbind(dist_ign_tr = erreur_rel_3d$dist_ign_tr,as.data.table(erreur_rel_3d[,2]))

#Répartition des trajets selon leur distance et le degré de précision des coordonnées
data_09_2021 <- fread("2021-09-vf-distances.csv.gz",colClasses = c("journey_start_insee"="character"))
data_09_2021$dist_tr <- cut(data_09_2021$distance_etp,tr_dist,label_tr_dist,include.lowest = T)

part_coord_3d <- aggregate(data=data_09_2021,cbind('Départ'=precision_start==3,"Arrivée"=precision_end==3)~dist_tr,
          FUN=function(x) round(mean(x)*100,1))  

rep_trajets <- aggregate(data=data_09_2021,distance_etp~dist_tr,FUN=function(x) c(n=length(x),sum=sum(x)))                                         
rep_trajets <- as.data.table(rep_trajets)
rep_trajets[,trajets := round(distance_etp.n/sum(distance_etp.n)*100,1) ]
rep_trajets[,distances := round(distance_etp.sum/sum(distance_etp.sum)*100,1) ]
rep_trajets <- subset(rep_trajets,select = -c(distance_etp.sum,distance_etp.n))

write_xlsx(list("Distances moyennes RP"=rp_dist_moy,"Erreur 2 décimales (en %)"=erreur_rel_2d,
                "Erreur 3 décimales (en %)"=erreur_rel_3d,
  "Part coord. 3 décimales"=part_coord_3d,"Journey par distance (en %)"=rep_trajets),
           "stat_precisions_distance.xlsx")


####DONNEES DU REGISTRE####

#Composition_communale des EPCI
if(!file.exists("Intercommunalite-Metropole_au_01-01-2021.xlsx")){
  curl_download("https://www.insee.fr/fr/statistiques/fichier/2510634/Intercommunalite_Metropole_au_01-01-2021.zip",
                "Intercommunalite_Metropole_au_01-01-2021.zip", mode="wb")
  unzip("Intercommunalite_Metropole_au_01-01-2021.zip")
  file.remove("Intercommunalite-Metropole_au_01-01-2021.xlsx")
}

com_epci <- read_xlsx("Intercommunalite-Metropole_au_01-01-2021.xlsx","Composition_communale",skip=5)
setDT(com_epci)

#Méthodes : ign = distance IGN simple ; etp = distance IGN avec reconstitution du trajet ; vo = distance à vol d'oiseau
#dist_pas_XXX : distance moyenne par passager (conducteur compris) avec la méthode XXX 
#dist_veh_XXX : distance moyenne par véhicule avec la méthode XXX 
#nb_dep_pas : nombre de passagers (conducteur compris)
#nb_dep_veh : nombre de déplacements par véhicule (conducteur compris)
#tx_occup_XXX : taux d'occupation avec la méthode XXX 
# >>>> méthode supplémentaire : dep = sans prise en compte de la distance

data_09_2021$total <- 'Total'
data_09_2021$depcom <- data_09_2021$journey_start_insee
data_09_2021[depcom %in% 75101:75120,'depcom'] <- 75056
data_09_2021[depcom %in% 69381:69389,'depcom'] <- 69123
data_09_2021[depcom %in% 13201:13216,'depcom'] <- 13055
data_09_2021$epci <- com_epci[match(data_09_2021$depcom ,com_epci$CODGEO),"EPCI"]
data_09_2021[is.na(epci),"epci"] <- '999999999'


stat_occup <- function(var){
  stat_occup <- join(aggregate(data=data_09_2021,
                               as.formula(paste0("cbind('distance_etp' = distance_etp*passenger_seats,
                                                     'distance_ign' = distance_ign*passenger_seats,
                                                     'distance_vo' = distance_vo*passenger_seats,
                                                     passenger_seats)~",var)),FUN=sum),
                     aggregate(data=data_09_2021[!duplicated(trip_id)],
                               as.formula(paste0("cbind(distance_etp_cond,distance_ign_cond,distance_vo_cond)~",var)),FUN=sum))
  
  stat_occup <- join(stat_occup,aggregate(data=data_09_2021[!duplicated(trip_id)],
                                          as.formula(paste0("cbind('nb_dep_veh'=trip_id)~",var)),FUN=length)   )
  
  stat_occup$nb_dep_pas <- with(stat_occup,nb_dep_veh+passenger_seats) 
  
  meth <- 'vo'
  for(meth in c("vo","ign","etp")){
    #Distances
    stat_occup[[paste0("dist_veh_",meth)]] <- stat_occup[[paste0("distance_",meth,"_cond")]]/stat_occup$nb_dep_veh 
    stat_occup[[paste0("dist_veh_",meth)]] <- round(stat_occup[[paste0("dist_veh_",meth)]]/1000,2)
    stat_occup[[paste0("dist_pas_",meth)]] <- (stat_occup[[paste0("distance_",meth,"_cond")]]+stat_occup[[paste0("distance_",meth)]])/stat_occup$nb_dep_pas 
    stat_occup[[paste0("dist_pas_",meth)]] <- round(stat_occup[[paste0("dist_pas_",meth)]]/1000,2)
    #Taux d'occupation
    stat_occup[[paste0("tx_occup_",meth)]] <- (stat_occup[[paste0("distance_",meth,"_cond")]]+stat_occup[[paste0("distance_",meth)]])/stat_occup[[paste0("distance_",meth,"_cond")]]
  }
  
  stat_occup$tx_occup_dep <- with(stat_occup,nb_dep_pas/nb_dep_veh)
  stat_occup <- subset(stat_occup,select=-c(distance_etp,distance_ign,distance_vo,passenger_seats,
                                            distance_etp_cond,distance_ign_cond,distance_vo_cond))
  return(stat_occup)
}




erreur_occup <- function(var){
  
  stat_epci <- stat_occup(var)
  stat_epci$erreur_tx_occup_dep <- stat_epci$tx_occup_dep-stat_epci$tx_occup_etp
  stat_epci$erreur_tx_occup_vo <- stat_epci$tx_occup_vo-stat_epci$tx_occup_etp
  stat_epci$erreur_tx_occup_ign <- stat_epci$tx_occup_ign-stat_epci$tx_occup_etp
  
  stat_epci$erreur_rel_dist_veh_vo <- 100*(stat_epci$dist_veh_vo/stat_epci$dist_veh_etp - 1)
  stat_epci$erreur_rel_dist_pas_vo <- 100*(stat_epci$dist_pas_vo/stat_epci$dist_pas_etp - 1)
  stat_epci$erreur_rel_dist_veh_ign <- 100*(stat_epci$dist_veh_ign/stat_epci$dist_veh_etp - 1)
  stat_epci$erreur_rel_dist_pas_ign <- 100*(stat_epci$dist_pas_ign/stat_epci$dist_pas_etp - 1)
  
  distrib_erreur <- rbind(quantile(stat_epci$erreur_tx_occup_dep ,probs = list_quant,na.rm=T),
                          quantile(stat_epci$erreur_tx_occup_vo ,probs = list_quant,na.rm=T),
                          quantile(stat_epci$erreur_tx_occup_ign ,probs = list_quant,na.rm=T),
                          quantile(stat_epci$erreur_rel_dist_veh_vo ,probs = list_quant,na.rm=T),
                          quantile(stat_epci$erreur_rel_dist_pas_vo ,probs = list_quant,na.rm=T),
                          quantile(stat_epci$erreur_rel_dist_veh_ign ,probs = list_quant,na.rm=T),
                          quantile(stat_epci$erreur_rel_dist_pas_ign ,probs = list_quant,na.rm=T))
  
  list_err <- c("Erreur taux d'occupation deplacement","Erreur taux d'occupation vol d'oiseau",
                "Erreur taux d'occupation IGN","Erreur rel. distance passager vol d'oiseau",
                "Erreur rel. distance véhicule vol d'oiseau","Erreur rel. distance passager IGN",
                "Erreur rel. distance véhicule IGN")
  distrib_erreur <- as.data.frame(distrib_erreur)
  distrib_erreur <- cbind(list_err,distrib_erreur)
  return(distrib_erreur)
}

rbind(stat_occup('dist_tr'),
      plyr::rename(stat_occup('total'),c("total"="dist_tr")))

erreur_occup('epci')
erreur_occup('trip_id')


