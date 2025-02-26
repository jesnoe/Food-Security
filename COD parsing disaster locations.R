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
# disaster_PAK <- disaster %>% filter(Country == "Pakistan")
disaster_COD <- disaster %>% filter(Country == "Democratic Republic of the Congo") # 214
disaster_COD <- disaster_COD %>% filter(!is.na(Location))
disaster_COD$Location %>% head
disaster_COD$Location <- gsub(";", ",", disaster_COD$Location)
# disaster_COD$Location <- gsub(" &", ",", disaster_COD$Location)


loc_row15 <- (disaster_COD$Location[4] %>% strsplit("\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T))[[1]]
ex3 <-  geo(address=paste0(loc_row15, ", République Démocratique du Congo"), method = "arcgis")
reverse_geocode(ex3, lat = lat, long = long, address = addr, method = "arcgis", full_results = T) %>% view

# disaster_COD %>% filter(grepl("\\(", Location)) %>% select(Location) %>% view
# disaster_COD %>% filter(grepl("mountain", Location)) %>% select(Location)
# disaster_COD %>% filter(grepl("montain", Location)) %>% select(Location)
# disaster_COD %>% filter(grepl("region", Location)) %>% select(Location)

disaster_COD_separate_locations <- tibble()
start.t <- Sys.time()
for (i in 1:nrow(disaster_COD)) {
  DisNo._i <- disaster_COD$DisNo.[i]
  locations_split <- strsplit(disaster_COD$Location[i], "\\([^)]+,(*SKIP)(*FAIL)|,\\s*", perl=T)[[1]]
  # locations_split <- locations_split[!(locations_split %in% c("Northern", "Southern", "Eastern", "Western"))]
  # locations_split <- sapply(locations_split, function(x) ifelse(grepl("region", x),
  #                                                               substr(x,
  #                                                                      gregexpr("\\(", x)[[1]][1] + 1,
  #                                                                      gregexpr("\\)", x)[[1]][1] - 1),
  #                                                               x))
  # locations_split <- strsplit(locations_split, "\\b, \\b|\\b and \\b")
  for (location in locations_split) {
    ArcGIS_coords <- geo(address=paste0(location, ", République Démocratique du Congo"),
                         method = "arcgis", 
                         unique_only = T)
    ArcGIS_rev_geocode <- reverse_geocode(ArcGIS_coords,
                                          lat = lat, 
                                          long = long, 
                                          address = addr,
                                          method = "arcgis",
                                          full_results = T) %>% 
      mutate(DisNo.=DisNo._i) %>% relocate(DisNo., address)
    disaster_COD_separate_locations <- rbind(disaster_COD_separate_locations, ArcGIS_rev_geocode)
  }
  if (i %% 10 == 0) print(paste0(i, "th row complete"))
}
end.t <- Sys.time()
end.t - start.t # 6.531196 mins
disaster_COD_separate_locations %>% select(DisNo., address, Region)
disaster_COD_separate_locations %>% view
disaster_COD_separate_locations$Region <- stri_trans_general(disaster_COD_separate_locations$Region, "Latin-ASCII")
disaster_COD_separate_locations$Region %>% unique %>% sort

disaster_COD_separate_locations %>% filter(CntryName != "République Démocratique du Congo") %>% select(address, Subregion, Region, CntryName)
lake_index <- which(disaster_COD_separate_locations$CntryName != "République Démocratique du Congo")
disaster_COD_separate_locations$Region[lake_index[-(4:5)]] <- "Nord Kivu"
disaster_COD_separate_locations$Region[lake_index[4:5]] <- "Ituri"
disaster_COD_separate_locations$CntryName[lake_index] <- "République Démocratique du Congo"
disaster_COD_separate_locations$CountryCode <- "COD"

disaster_COD_separate_locations$Region %>% unique %>% sort

disaster_COD_separate_locations <- disaster_COD_separate_locations %>% 
  mutate(
    Region = gsub("-", " ", Region),
    Region = gsub("Occidental", "Oriental", Region),
    Region = ifelse(Region == "Bandundu", "Kwilu", Region),
    Region = ifelse(Region == "Bas Congo", "Kongo Central", Region),    
    Region = ifelse(Region == "Orientale", "Kasai Oriental", Region),
  )
disaster_COD_separate_locations$Region %>% unique %>% sort


disaster_COD_separate_locations %>%
  write.csv("Food Security/Disaster COD locations.csv", row.names=F)
