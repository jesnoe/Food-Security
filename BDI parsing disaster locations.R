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
disaster_BDI <- disaster %>% filter(Country == "Burundi") # 70
disaster_BDI <- disaster_BDI %>% filter(!is.na(Location))
disaster_BDI$Location %>% head
disaster_BDI$Location <- gsub(";", ",", disaster_BDI$Location)
# disaster_BDI$Location <- gsub(" &", ",", disaster_BDI$Location)

disaster_BDI$Location[22]
loc_row15 <- (disaster_BDI$Location[22] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", Burundi"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

# disaster_BDI %>% filter(grepl("\\(", Location)) %>% select(Location) %>% view
# disaster_BDI %>% filter(grepl("mountain", Location)) %>% select(Location)
# disaster_BDI %>% filter(grepl("montain", Location)) %>% select(Location)
# disaster_BDI %>% filter(grepl("region", Location)) %>% select(Location)

disaster_BDI_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_BDI)) {
  DisNo._i <- disaster_BDI$DisNo.[i]
  locations_split <- strsplit(disaster_BDI$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ",Burundi"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo., address)
    disaster_BDI_separate_locations <- rbind(disaster_BDI_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 3.182751 mins
disaster_BDI_separate_locations %>% select(DisNo., address, Region)
disaster_BDI_separate_locations %>% view
disaster_BDI_separate_locations$Region <- stri_trans_general(disaster_BDI_separate_locations$Region, "Latin-ASCII")
disaster_BDI_separate_locations$Region %>% unique %>% sort

disaster_BDI_separate_locations %>% filter(CntryName != "Burundi") %>% select(address, Subregion, Region, CntryName)
wrong_name_index <- which(disaster_BDI_separate_locations$CntryName != "Burundi")
disaster_BDI_separate_locations$Region[wrong_name_index[-6]] <- c("Cibitoke", "Cibitoke", "Cibitoke", "Bujumbura Rural", "Bujumbura Rural")
disaster_BDI_separate_locations$CntryName[wrong_name_index] <- "Burundi"
disaster_BDI_separate_locations <- disaster_BDI_separate_locations[-wrong_name_index[6],]

disaster_BDI_separate_locations$Region %>% unique %>% sort

# disaster_BDI_separate_locations <- disaster_BDI_separate_locations %>% 
#   mutate(
#     Region = gsub("-", " ", Region),
#     Region = gsub("Occidental", "Oriental", Region),
#     Region = ifelse(Region == "Bandundu", "Kwilu", Region),
#     Region = ifelse(Region == "Bas Congo", "Kongo Central", Region),    
#     Region = ifelse(Region == "Orientale", "Kasai Oriental", Region),
#   )
# disaster_BDI_separate_locations$Region %>% unique %>% sort


disaster_BDI_separate_locations %>%
  write.csv("Food Security/Disaster BDI locations.csv", row.names=F)
