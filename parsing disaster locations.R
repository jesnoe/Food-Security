# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(tidygeocoder)
library(gridExtra)
library(colmaps)
library(lubridate)
library(sf)
library(sp)

disaster <- read_xlsx("Food Security/public_emdat_2024-09-17.xlsx")
disaster_Afg <- disaster %>% filter(Country == "Afghanistan") # 214
disaster_Afg$Location %>% head
disaster_Afg$Location <- gsub(";", ",", disaster_Afg$Location)
disaster_Afg$Location <- gsub(" &", ",", disaster_Afg$Location)
disaster_Afg %>% select(Location) %>% view

loc_row6 <- (disaster_Afg$Location[6] %>% strsplit(", "))[[1]]

# can we believe ArcGIS query results?
ex1 <-  geo(address="Hilmand, Afghanistan", method = "arcgis")
ex2 <-  geo(address=paste0(loc_row6, ", Afghanistan"), method = "arcgis")
ex2
ex2_rev_geocode <- reverse_geocode(ex2, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

# Hindu Kush montain (Wakhan district, Badakhshan province), Souchi Bala Payan, Yawan villages (Yawan district, Badakhshan province),
# Souch village (Jorm district, Badakhshan province), Jurm, Malang Ab villages (Baharak district, Badakhshan province), Rostaq district (Takhar province),
# Kabul district (Kabul province), Khuram Wa Sarbagh, Aybak districts (Samagan province)
loc_row15 <- (disaster_Afg$Location[15] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", Afghanistan"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view
ex4 <-  geo(address=paste0("Yawan district, Badakhshan province", ", Afghanistan"), method = "arcgis")
reverse_geocode(ex4, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

disaster_Afg %>% filter(grepl("\\(", Location)) %>% select(Location) %>% view
disaster_Afg %>% filter(grepl("mountain", Location)) %>% select(Location)
disaster_Afg %>% filter(grepl("montain", Location)) %>% select(Location)
disaster_Afg %>% filter(grepl("region", Location)) %>% select(Location)

disaster_Afg_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_Afg)) {
  DisNo._i <- disaster_Afg$DisNo.[i]
  locations_split <- strsplit(disaster_Afg$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  locations_split <- locations_split[!(locations_split %in% c("Northern", "Southern", "Eastern", "Western"))]
  locations_split <- sapply(locations_split, function(x) ifelse(grepl("region", x),
                                                                substr(x,
                                                                       gregexpr("\\(", x)[[1]][1] + 1,
                                                                       gregexpr("\\)", x)[[1]][1] - 1),
                                                                x))
  locations_split <- strsplit(locations_split, "\\b, \\b|\\b and \\b")
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ", Afghanistan"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo._i, address)
    disaster_Afg_separate_locations <- rbind(disaster_Afg_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 15.16602 mins
disaster_Afg_separate_locations
disaster_Afg_separate_locations %>% view


disaster_Afg_separate_locations %>% filter(CountryCode != "AFG") %>% select(address, addr, lat, long) %>% as.data.frame %>% unique
# Charsadra, Afghanistan -> Charsada, Afghanistan
# Burka, Afghanistan -> Baghlan, Afghanistan
# Mullah Jan villages (Burka district, Afghanistan -> Baghlan, Afghanistan
# Shahrak, Afghanistan -> Ghowr, Afghanistan (Sharak)
# Zaazi Aryoub district Marikhil village, Afghanistan -> Paktia, Afghanistan (Aryob Zazi)
# Saripul, Afghanistan -> Sar-E Pol, Afghanistan
ex_TJK <-  geo(address=paste0("Sar-e Pol, Afghanistan "), method = "arcgis")
reverse_geocode(ex_TJK, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view
Badakhshan_geocode <- (disaster_Afg_separate_locations %>% filter(grepl("Badakhshan", address)))[1,]
Charsadra_geocode <- reverse_geocode(geo(address=paste0("Charsada, Afghanistan "), method = "arcgis"),
                                    lat = lat, long = long, address = addr, method = "arcgis", full_results = T)
Burka_geocode <- reverse_geocode(geo(address=paste0("Baghlan, Afghanistan "), method = "arcgis"),
                                 lat = lat, long = long, address = addr, method = "arcgis", full_results = T)
Shahrak_geocode <- reverse_geocode(geo(address=paste0("Ghowr, Afghanistan "), method = "arcgis"),
                                   lat = lat, long = long, address = addr, method = "arcgis", full_results = T)
Aryoub_geocode <- reverse_geocode(geo(address=paste0("Paktia, Afghanistan "), method = "arcgis"),
                                   lat = lat, long = long, address = addr, method = "arcgis", full_results = T)
Saripul_geocode <- reverse_geocode(geo(address=paste0("Sar-e Pol, Afghanistan "), method = "arcgis"),
                                   lat = lat, long = long, address = addr, method = "arcgis", full_results = T)
  
wrong_name_index <- which(disaster_Afg_separate_locations$CountryCode != "AFG")
Badak_index <- grep("Badak", disaster_Afg_separate_locations[wrong_name_index,]$address)
Charsadra_index <- grep("Charsadra", disaster_Afg_separate_locations[wrong_name_index,]$address)
Burka_index <- grep("Burka", disaster_Afg_separate_locations[wrong_name_index,]$address)
Shahrak_index <- grep("Shahrak", disaster_Afg_separate_locations[wrong_name_index,]$address)
Aryoub_index <- grep("Aryoub", disaster_Afg_separate_locations[wrong_name_index,]$address)
Saripul_index <- grep("Saripul", disaster_Afg_separate_locations[wrong_name_index,]$address)

disaster_Afg_separate_locations[wrong_name_index,][Badak_index,-1] <- Badakhshan_geocode[,-1]
disaster_Afg_separate_locations[wrong_name_index,][Charsadra_index,-1] <- Charsadra_geocode
disaster_Afg_separate_locations[wrong_name_index,][Burka_index,-1] <- Burka_geocode
disaster_Afg_separate_locations[wrong_name_index,][Shahrak_index,-1] <- Shahrak_geocode
disaster_Afg_separate_locations[wrong_name_index,][Aryoub_index,-1] <- Aryoub_geocode
disaster_Afg_separate_locations[wrong_name_index,][Saripul_index,-1] <- Saripul_geocode

# disaster_Afg_separate_locations %>% 
#   filter(CountryCode == "AFG") %>% 
#   write.csv("Food Security/Disaster Afghanistan locations.csv", row.names=F)
