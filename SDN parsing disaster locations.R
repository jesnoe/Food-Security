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
disaster_SDN <- disaster %>% filter(Country == "Sudan") # 214
disaster_SDN %>% filter(is.na(Location)) # 8 African disasters (Epidemic, Infestation, Drought, Water)
disaster_SDN$Location %>% head
# disaster_SDN$Location <- gsub(";", ",", disaster_SDN$Location)
# disaster_SDN$Location <- gsub(" &", ",", disaster_SDN$Location)


loc_row15 <- (disaster_SDN$Location[2] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", Sudan"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

# disaster_SDN %>% filter(grepl("\\(", Location)) %>% select(Location) %>% view
# disaster_SDN %>% filter(grepl("mountain", Location)) %>% select(Location)
# disaster_SDN %>% filter(grepl("montain", Location)) %>% select(Location)
# disaster_SDN %>% filter(grepl("region", Location)) %>% select(Location)

disaster_SDN_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_SDN)) {
  DisNo._i <- disaster_SDN$DisNo.[i]
  locations_split <- strsplit(disaster_SDN$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  # locations_split <- locations_split[!(locations_split %in% c("Northern", "Southern", "Eastern", "Western"))]
  # locations_split <- sapply(locations_split, function(x) ifelse(grepl("region", x),
  #                                                               substr(x,
  #                                                                      gregexpr("\\(", x)[[1]][1] + 1,
  #                                                                      gregexpr("\\)", x)[[1]][1] - 1),
  #                                                               x))
  # locations_split <- strsplit(locations_split, "\\b, \\b|\\b and \\b")
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ", Sudan"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo., address)
    disaster_SDN_separate_locations <- rbind(disaster_SDN_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 5.920379 mins
disaster_SDN_separate_locations
disaster_SDN_separate_locations %>% view
disaster_SDN_separate_locations %>% filter(CntryName != "Sudan") %>% select(address, Subregion, Region, CntryName) %>%
  filter(CntryName != "South Sudan") %>% print(n=23)

Addabah_index <- which(disaster_SDN_separate_locations$address == "Addabah district (Northern province), Sudan")
disaster_SDN_separate_locations$Subregion[Addabah_index] <- "Addabah"
disaster_SDN_separate_locations$Region[Addabah_index] <- "Northern"
disaster_SDN_separate_locations$CntryName[Addabah_index] <- "Sudan"
Southern_Kordofan_index <- which(disaster_SDN_separate_locations$Region == "Southern Kordofan")
disaster_SDN_separate_locations$CntryName[Southern_Kordofan_index] <- "Sudan"

disaster_SDN_separate_locations <- disaster_SDN_separate_locations %>% filter(CntryName == "Sudan")

# disaster_SDN_separate_locations %>% 
#   filter(CountryCode == "SDN") %>% 
#   write.csv("Food Security/Disaster Sudan locations.csv", row.names=F)
