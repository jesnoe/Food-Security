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
disaster_CAF <- disaster %>% filter(Country == "Central African Republic") # 66
disaster_CAF <- disaster_CAF %>% filter(!is.na(Location))
disaster_CAF$Location %>% head
disaster_CAF$Location <- gsub(";", ",", disaster_CAF$Location)
# disaster_CAF$Location <- gsub(" &", ",", disaster_CAF$Location)

disaster_CAF$Location[22]
loc_row15 <- (disaster_CAF$Location[22] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", Central African Republic"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

disaster_CAF_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_CAF)) {
  DisNo._i <- disaster_CAF$DisNo.[i]
  locations_split <- strsplit(disaster_CAF$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ", Central African Republic"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo., address)
    disaster_CAF_separate_locations <- rbind(disaster_CAF_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 2.433658 mins
disaster_CAF_separate_locations %>% select(DisNo., address, Region)
disaster_CAF_separate_locations %>% view
disaster_CAF_separate_locations$Region <- stri_trans_general(disaster_CAF_separate_locations$Region, "Latin-ASCII")
disaster_CAF_separate_locations$Region %>% unique %>% sort

disaster_CAF_separate_locations <- disaster_CAF_separate_locations %>% filter(CntryName == "République Centrafricaine") %>% mutate(CntryName = "Central African Republic")
disaster_CAF_separate_locations %>% filter(CntryName != "Central African Republic") %>% select(address, Subregion, Region, CntryName)
wrong_name_index <- which(disaster_CAF_separate_locations$CntryName != "Central African Republic")
disaster_CAF_separate_locations$Region[which(disaster_CAF_separate_locations$Region == "Ouham-Pende")] <- "Ouham Pende"
disaster_CAF_separate_locations$Region[which(disaster_CAF_separate_locations$Region == "Ombella-M'Poko")] <- "Ombella M'Poko"
disaster_CAF_separate_locations$Region %>% unique %>% sort

disaster_CAF_separate_locations %>%
  write.csv("Food Security/Disaster CAF locations.csv", row.names=F)
