library("data.table")
library("sf")
library("rjson")
library("httr")
library("curl")
library("dplyr")
#Sys.setenv(http_proxy = "proxy-rie.ac.i2:8080")
#Sys.setenv(https_proxy = "proxy-rie.ac.i2:8080")

#Chargement des données du mois de septembre 2021
if(!file.exists("2021-09-vf.csv")){
  curl_download("https://www.data.gouv.fr/fr/datasets/r/d92d1dd8-d826-4e1b-af2e-a1275fa6afda","2021-09-vf.csv", mode="wb")
}

#Lecture des données du mois de septembre 
data_09_2021 <- fread("2021-09-vf.csv")

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

deb <- Sys.time()
for(p in 0:9 ){
  print(paste("Paquet",p))
  od_lon_lat <- data_09_2021_xy[paq==p,]
  dist_routiere_ign <- lapply(1:nrow(od_lon_lat),function(l){
    dcv <- od_lon_lat[l,]
    url <- with(dcv,paste0("https://itineraire.ign.fr/simple/1.0.0/route?resource=bdtopo-osrm&profile=car&optimization=fastest&start=",
                           dcv$journey_start_lon,",",dcv$journey_start_lat,"&end=",dcv$journey_end_lon,",",dcv$journey_end_lat,
                           "&intermediates=&constraints=&geometryFormat=polyline&getSteps=false&getBbox=false"))
    
    resp <- GET(url, encoding = "UTF-8")
    itin_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
    return(itin_json$distance)
  })
  od_lon_lat$distance_ign <- unlist(dist_routiere_ign)
  print(Sys.time()-deb)
  fwrite(subset(od_lon_lat,select=-paq),paste0("Itin/distance_ign_",p,".csv.gz"))
}

distance_ign <- lapply(0:9,function(p) fread(paste0("Itin/distance_ign_",p,".csv.gz")))
distance_ign <- rbindlist(distance_ign)

#Ajout des distances IGN à la table initiale
data_09_2021 <- left_join(data_09_2021,distance_ign)

#Le conducteur est attribué au trajet le plus long 
data_09_2021 <- data_09_2021[order(trip_id,-distance_vo),]
data_09_2021$max_distance_vo<- !duplicated(data_09_2021$trip_id)
#Si passenger_seat manquant, alors il est fixé à 1 
data_09_2021[is.na(passenger_seats),"passenger_seats"] <- 1
data_09_2021$nb_occup_vo <- with(data_09_2021,ifelse(max_distance_vo==TRUE,passenger_seats+1,passenger_seats)) 

#Méthode 1: Taux d'occupation par trajet 
sum(data_09_2021$nb_occup_vo)/length(unique(data_09_2021$trip_id))

#Méthode 2: Taux d'occupation par distance VO
sum(data_09_2021$nb_occup_vo*data_09_2021$distance_vo)/sum(data_09_2021[!duplicated(trip_id),]$distance_vo)

#Méthode 3: Taux d'occupation par trajet IGN
data_09_2021 <- data_09_2021[order(trip_id,-distance_ign),]
data_09_2021$max_distance_ign<- !duplicated(data_09_2021$trip_id)
data_09_2021$nb_occup_ign <- with(data_09_2021,ifelse(max_distance_ign==TRUE,passenger_seats+1,passenger_seats)) 
                              
sum(data_09_2021$nb_occup_ign*data_09_2021$distance_ign)/sum(data_09_2021[!duplicated(trip_id),]$distance_ign)
                                 
#Méthode 4 : 


quantile(data_09_2021$distance_ign/data_09_2021$distance_vo,probs=0:10/10)

#Effet de l'approximation de 70 ou 700 mètres 

carreaux_domtrav_RP2017 <- fread("Data/carreaux_domtrav_RP2017.csv.gz")


###################################################################################
#Passage des
data_09_2021_char <- fread("2021-09-vf.csv",colClasses = "character")

#Part des points arrondis à l

data_09_2021[,nb_journey := .N, by = "trip_id"]
#Nombre de journey par trip 
count_trip <- plyr::count(data_09_2021[,c("trip_id")])
count_trip <- plyr::count(count_trip$freq)
setDT(count_trip)
count_trip[,pct := round(100*freq/sum(freq),digits=1)]

#Heure de départ et d'arrivée : bien réparti dans la journée (pour échelonner les étapes)
plyr::count(data_09_2021$journey_start_time)
mean(is.na(data_09_2021$journey_start_time))

#Nombre de couples origine-destination : 47850
sum(!duplicated(data_09_2021[,c("journey_end_lon","journey_end_lat","journey_start_lon","journey_start_lat")]))
sum(!duplicated(data_09_2021[,c("journey_start_insee","journey_end_insee")]))

#Distance

#Distance max 
data_09_2021[,max_dist_vo_trip := max(dist_vo),by="trip_id"]
data_09_2021[,min_dist_vo_trip := min(dist_vo),by="trip_id"]
data_09_2021[,nb_journey := .N,by="trip_id"]

quantile(data=data_09_2021[nb_journey>1 & !duplicated(trip_id),], data_09_2021$min_dist_vo_trip/data_09_2021$max_dist_vo_trip,probs=0:20/100)
#Pour 82 % des trips avec plusieurs journey, la distance est identique pour tout le monde 

od_lon_lat <- data_09_2021[,c("journey_end_lon","journey_end_lat","journey_start_lon","journey_start_lat")]
od_lon_lat <- od_lon_lat[!duplicated(od_lon_lat),]
od_lon_lat <- od_lon_lat[1:10000,]

deb <- Sys.time()

Sys.time() - deb

od_lon_lat$dist_routiere_ign <- unlist(dist_routiere_ign) 

#Part des trajets gélocalisés à 70m/700m 

mean(data_09_2021$dist_vo==0)

#Etapes 

step_data <- rbind(plyr::rename(data_09_2021[,c("trip_id","journey_start_datetime","journey_start_lon","journey_start_lat","nb_journey")],
                                c("journey_start_datetime"="step_datetime","journey_start_lon"="lon","journey_start_lat"="lat")),
                   plyr::rename(data_09_2021[,c("trip_id","journey_end_datetime","journey_end_lon","journey_end_lat","nb_journey")],
                                c("journey_end_datetime"="step_datetime","journey_end_lon"="lon","journey_end_lat"="lat")))
step_data <- step_data[!duplicated(step_data),]
step_data <- step_data[order(trip_id,step_datetime),]
mean(duplicated(step_data[nb_journey>1,c('trip_id','step_datetime')]))
mean(duplicated(step_data[nb_journey>1,c('trip_id','lon','lat')]))

data_09_2021

