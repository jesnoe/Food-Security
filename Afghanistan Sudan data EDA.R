# setwd("/Users/R")
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

{
afg_map <- read_sf("Food Security/geoBoundaries-AFG-ADM1.geojson")
SDN_map <- read_sf("Food Security/geoBoundaries-SDN-ADM2.geojson")
disaster <- read_xlsx("Food Security/public_emdat_2024-09-17.xlsx")
FSI <- read_xlsx("Food Security/fsi-2017.xlsx")[,1:16] %>% mutate(Year = year(Year))
for (i in 2018:2023) {
  if(i %in%  c(2021, 2023)) {
    FSI_year <- read_xlsx(paste0("Food Security/fsi-", i, ".xlsx"))[,1:16]
  }else{
    FSI_year <- read_xlsx(paste0("Food Security/fsi-", i, ".xlsx"))[,1:16] %>% mutate(Year = year(Year))
  }
  FSI <- rbind(FSI, FSI_year)
}
conflict_ME <- read.csv("Food Security/conflict_2017-01-01-2024-10-06-Caucasus_and_Central_Asia.csv") %>% as_tibble

IPC_AS <- read_xlsx("Food Security/Asia - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
  filter(!is.na(Country)) %>% filter(is.na(Population))
IPC_AS$Area_Phase <- IPC_AS %>%
  select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
  apply(1, function(x) ifelse(sum(is.na(x)) == 5, 0, which.max(x))) %>% as.factor
IPC_AS$Date <- as.Date(paste(IPC_AS$Date, "01"), format="%b %Y %d")
IPC_AS$Year <- year(IPC_AS$Date) %>% as.factor
IPC_AS$Month <- month(IPC_AS$Date) %>% as.factor
IPC_AS <- IPC_AS %>% relocate(Country:Area_id, Year, Month)
IPC_AS$Subarea <- gsub(" c_[0-9]+$", "", IPC_AS$Subarea)
IPC_AS$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_AS$Subarea)

IPC_Afg <- IPC_AS %>% filter(Country == "Afghanistan") # 549
FSI_Afg <- FSI %>% filter(Country == "Afghanistan")
disaster_Afg <- disaster %>% filter(Country == "Afghanistan") # 214
disaster_Afg_separate_locations <- read.csv("Food Security/Disaster Afghanistan locations.csv") %>% as_tibble

disaster_Afg <- disaster_Afg_separate_locations %>%
  select(DisNo., lat, long, Subregion, Region) %>% 
  left_join(disaster_Afg %>%
              select(-Region, -Subregion) %>% 
              rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
conflict_Afg <- conflict_ME %>% filter(country == "Afghanistan") # 66,500 # much more than CAR
conflict_Afg$event_date <- as.Date(conflict_Afg$event_date, format="%d %B %Y")
conflict_Afg$month <- month(conflict_Afg$event_date) %>% as.factor
conflict_Afg <- conflict_Afg %>% relocate(event_id_cnty, event_date, year, month)

IPC_AF <- read_xlsx("Food Security/East and Central Africa - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
  filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population))
IPC_AF$Area_Phase <- IPC_AF %>% select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>% apply(1, function(x) which.max(x)) %>% as.factor
IPC_AF$Date <- as.Date(paste(IPC_AF$Date, "01"), format="%b %Y %d")
IPC_AF$Year <- year(IPC_AF$Date) %>% as.factor
IPC_AF$Month <- month(IPC_AF$Date) %>% as.factor
IPC_AF <- IPC_AF %>% relocate(Country:Area_id, Year, Month)

IPC_SDN <- IPC_AF %>% filter(Country == "Sudan") # 1057, 18 states
}

#### NEED aggregate phase populations in 05-2017 and 01-2018
FSI_Afg
IPC_Afg
conflict_Afg_n_events <- conflict_Afg %>% 
  group_by(year, admin1) %>% 
  summarise(n_events=n())

disaster_Afg_monthly_events <- disaster_Afg %>% 
  group_by(Region, `Start Year`, `Start Month`) %>% 
  summarise(n_events=n()) %>% 
  rename(year=`Start Year`, month=`Start Month`)

disaster_Afg_annual_events <- disaster_Afg %>% 
  group_by(Region, `Start Year`) %>% 
  summarise(n_events=n()) %>% 
  rename(year=`Start Year`)

conflict_Afg$event_type %>% table
disaster_Afg$`Disaster Type` %>% table

year_prev <- 0
IPC_Afg_year_month <- IPC_Afg %>% select(Year, Month) %>% filter(Year != "2024") %>% arrange(Year, Month) %>% unique 
for (i in 1:nrow(IPC_Afg_year_month)) {
  year_i <- IPC_Afg_year_month$Year[i] %>% as.character %>% as.numeric
  month_i <- IPC_Afg_year_month$Month[i] %>% as.character %>% as.numeric
  
  ICP_map_i <- afg_map %>% left_join(IPC_Afg %>% filter(Year == year_i & Month == month_i) %>% select(Subarea, Phase_3above_ratio) %>% rename(shapeName=Subarea),
                        by="shapeName") %>% 
    ggplot() + geom_sf(aes(fill=Phase_3above_ratio)) +
    scale_fill_viridis_c(limits=c(0,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  # ggsave(paste0("Food Security/Figs/AFG IPC ", year_i, " ", month_i, ".png"), ICP_map_i, scale=1)
  
  if (year_i == year_prev) next
  conflict_map_i <- afg_map %>% left_join(conflict_Afg_n_events %>% filter(year == year_i) %>% rename(shapeName=admin1),
                        by="shapeName") %>% 
    ggplot() + geom_sf(aes(fill=n_events)) +
    scale_fill_viridis_c(limits=c(0,1500)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  
  disaster_map_i <- afg_map %>% left_join(disaster_Afg_annual_events %>% filter(year == year_i) %>% rename(shapeName=Region),
                                          by="shapeName") %>% 
    ggplot() + geom_sf(aes(fill=n_events)) +
    scale_fill_viridis_c(limits=c(0,17)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  year_prev <- year_i
  # ggsave(paste0("Food Security/Figs/AFG n_conflicts ", year_i, ".png"), conflict_map_i, scale=1)
  # ggsave(paste0("Food Security/Figs/AFG n_disasters ", year_i, " ", ".png"), disaster_map_i, scale=1)
}

year_prev <- 0
IPC_SDN_year_month <- IPC_SDN %>% select(Year, Month) %>% filter(Year != "2024") %>% arrange(Year, Month) %>% unique 
for (i in 1:nrow(IPC_SDN_year_month)) {
  year_i <- IPC_SDN_year_month$Year[i] %>% as.character %>% as.numeric
  month_i <- IPC_SDN_year_month$Month[i] %>% as.character %>% as.numeric
  
  IPC_map_i <- SDN_map %>% left_join(IPC_SDN %>% filter(Year == year_i & Month == month_i) %>% select(Subarea, Phase_3above_ratio) %>% rename(shapeName=Subarea),
                                     by="shapeName") %>% 
    ggplot() + geom_sf(aes(fill=Phase_3above_ratio)) +
    scale_fill_viridis_c(limits=c(0,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Food Security/Figs/maps/SDN IPC ", year_i, " ", month_i, ".png"), IPC_map_i, scale=1)
  year_prev <- year_i
}
