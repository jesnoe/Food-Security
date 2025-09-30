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
disaster_MOZ <- disaster %>% filter(Country == "Mozambique") # 66
disaster_MOZ <- disaster_MOZ %>% filter(!is.na(Location))
disaster_MOZ$Location %>% head
disaster_MOZ$Location <- gsub(";", ",", disaster_MOZ$Location)
# disaster_MOZ$Location <- gsub(" &", ",", disaster_MOZ$Location)

disaster_MOZ$Location[22]
loc_row15 <- (disaster_MOZ$Location[22] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", Mozambique"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

disaster_MOZ_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_MOZ)) {
  DisNo._i <- disaster_MOZ$DisNo.[i]
  locations_split <- strsplit(disaster_MOZ$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ", Mozambique"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo., address)
    disaster_MOZ_separate_locations <- rbind(disaster_MOZ_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 8.763111 mins
disaster_MOZ_separate_locations %>% select(DisNo., address, Region)
disaster_MOZ_separate_locations %>% view
disaster_MOZ_separate_locations$Region <- stri_trans_general(disaster_MOZ_separate_locations$Region, "Latin-ASCII")
disaster_MOZ_separate_locations$Region %>% unique %>% sort

disaster_MOZ_separate_locations %>% filter(CntryName != "Mozambique") %>% select(address, Subregion, Region, CntryName)
wrong_name_index <- which(disaster_MOZ_separate_locations$CntryName != "Mozambique")
disaster_MOZ_separate_locations$CntryName[wrong_name_index] <- "Mozambique"
disaster_MOZ_separate_locations$Region %>% unique %>% sort
disaster_MOZ_separate_locations <- disaster_MOZ_separate_locations %>% filter(Region != "")

disaster_MOZ_separate_locations %>%
  write.csv("Food Security/Disaster MOZ locations.csv", row.names=F)
