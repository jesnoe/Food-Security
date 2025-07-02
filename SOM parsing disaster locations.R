# setwd("C:/Users/User/Documents/R")
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
disaster_SOM <- disaster %>% filter(Country == "Somalia") # 70
disaster_SOM <- disaster_SOM %>% filter(!is.na(Location))
disaster_SOM$Location %>% head
disaster_SOM$Location <- gsub(";", ",", disaster_SOM$Location)
# disaster_SOM$Location <- gsub(" &", ",", disaster_SOM$Location)

disaster_SOM$Location[22]
loc_row15 <- (disaster_SOM$Location[22] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", Somalia"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

# disaster_SOM %>% filter(grepl("\\(", Location)) %>% select(Location) %>% view
# disaster_SOM %>% filter(grepl("mountain", Location)) %>% select(Location)
# disaster_SOM %>% filter(grepl("montain", Location)) %>% select(Location)
# disaster_SOM %>% filter(grepl("region", Location)) %>% select(Location)

disaster_SOM_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_SOM)) {
  DisNo._i <- disaster_SOM$DisNo.[i]
  locations_split <- strsplit(disaster_SOM$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ",Somalia"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo., address)
    disaster_SOM_separate_locations <- rbind(disaster_SOM_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 5.49272 mins
disaster_SOM_separate_locations %>% select(DisNo., address, Region)
disaster_SOM_separate_locations %>% view
disaster_SOM_separate_locations$Region <- stri_trans_general(disaster_SOM_separate_locations$Region, "Latin-ASCII")
disaster_SOM_separate_locations$Region %>% unique %>% sort

disaster_SOM_separate_locations %>% filter(CntryName != "Somalia") %>% select(address, Subregion, Region, CntryName)
wrong_name_index <- which(disaster_SOM_separate_locations$CntryName != "Somalia")
disaster_SOM_separate_locations$CntryName[wrong_name_index] <- "Somalia"
disaster_SOM_separate_locations <- disaster_SOM_separate_locations %>% filter(Region != "চট্টগ্রাম")
disaster_SOM_separate_locations$Region %>% unique %>% sort

# disaster_SOM_separate_locations <- disaster_SOM_separate_locations %>% 
#   mutate(
#     Region = gsub("-", " ", Region),
#     Region = gsub("Occidental", "Oriental", Region),
#     Region = ifelse(Region == "Bandundu", "Kwilu", Region),
#     Region = ifelse(Region == "Bas Congo", "Kongo Central", Region),    
#     Region = ifelse(Region == "Orientale", "Kasai Oriental", Region),
#   )
# disaster_SOM_separate_locations$Region %>% unique %>% sort


disaster_SOM_separate_locations %>%
  write.csv("Food Security/Disaster SOM locations.csv", row.names=F)
