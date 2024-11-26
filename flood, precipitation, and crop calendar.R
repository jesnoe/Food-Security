# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(tidygeocoder)
library(gridExtra)
library(colmaps)
library(lubridate)
library(fpp2)
library(sf)
library(sp)
library(reshape2)
{
disaster <- read_xlsx("Food Security/public_emdat_2024-09-17.xlsx")
disaster_Afg <- disaster %>% filter(Country == "Afghanistan") %>% # 214
  rename(year=`Start Year`, month=`Start Month`)
disaster_Afg_separate_locations <- read.csv("Food Security/Disaster Afghanistan locations.csv") %>% as_tibble %>% 
  mutate(Region = gsub("Badakhshān", "Badakhshan", Region)) %>%
  mutate(Region = gsub("Bamian", "Bamyan", Region)) %>% 
  mutate(Region = gsub("Bādghīs", "Badghis", Region)) %>% 
  mutate(Region = gsub("Baghlān", "Baghlan", Region)) %>% 
  mutate(Region = gsub("Farāh", "Farah", Region)) %>% 
  mutate(Region = gsub("Ghaznī", "Ghazni", Region)) %>% 
  mutate(Region = gsub("Ghōr", "Ghor", Region)) %>%
  mutate(Region = gsub("Ghowr", "Ghor", Region)) %>%
  mutate(Region = gsub("Kābul", "Kabul", Region)) %>%
  mutate(Region = gsub("Kabol", "Kabul", Region)) %>% 
  mutate(Region = gsub("Kandahār", "Kandahar", Region)) %>% 
  mutate(Region = gsub("Konarha", "Kandahar", Region)) %>% 
  mutate(Region = gsub("Konduz", "Kunduz", Region)) %>% 
  mutate(Region = gsub("Lowgar", "Logar", Region)) %>% 
  mutate(Region = gsub("Parvan", "Parwan", Region)) %>% 
  mutate(Region = gsub("Nangarhār", "Nangarhar", Region)) %>%
  mutate(Region = gsub("Oruzgan", "Uruzgan", Region)) %>%
  mutate(Region = gsub("Quandahar", "Kandahar", Region)) %>%
  mutate(Region = gsub("Sar-e Pul", "Sar-e Pol", Region)) %>%
  mutate(Region = gsub("Sar-e Pol", "Sar_e_Pol", Region)) %>% 
  mutate(Region = gsub("Vardak", "Wardak", Region)) %>%
  mutate(Region = gsub("Zabol", "Zabul", Region))

disaster_Afg <- disaster_Afg_separate_locations %>%
  select(DisNo., lat, long, Subregion, Region) %>% 
  left_join(disaster_Afg %>%
              select(-Region, -Subregion) %>% 
              rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
disaster_SDN <- disaster %>% filter(Country == "Sudan") %>%  # 318
  rename(year=`Start Year`, month=`Start Month`)
disaster_SDN_separate_locations <- read.csv("Food Security/Disaster Sudan locations.csv") %>% as_tibble
disaster_SDN <- disaster_SDN_separate_locations %>%
  select(DisNo., lat, long, Subregion, Region) %>% 
  left_join(disaster_SDN %>%
              select(-Region, -Subregion) %>% 
              rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
crop_maize <- read.csv("Food Security/crop calendar - maize.csv") %>% as_tibble
crop_millet <- read.csv("Food Security/crop calendar - millet.csv") %>% as_tibble
crop_rice <- read.csv("Food Security/crop calendar - rice.csv") %>% as_tibble
crop_sorghum <- read.csv("Food Security/crop calendar - sorghum.csv") %>% as_tibble
crop_soybean <- read.csv("Food Security/crop calendar - soybean.csv") %>% as_tibble
crop_wheat <- read.csv("Food Security/crop calendar - wheat.csv") %>% as_tibble
}

disaster_AFG_type <- disaster_Afg %>% filter(year > 2016) %>%  group_by(year, month, `Disaster Type`) %>% summarize(n_event=n(), n_affected=`Total Affected`[1])
disaster_AFG_type %>% ggplot() +
  geom_line(aes(x=paste(year, "_", month),
                y=n_event,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  geom_point(aes(x=paste(year, "_", month),
                 y=n_event,
                 group=`Disaster Type`,
                 color=`Disaster Type`)) +
  scale_color_viridis_d(option="F") +
  labs(x="year_month", y="n_disasters", title="AFG n_disasters") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

disaster_AFG_type %>% ggplot() +
  geom_line(aes(x=paste(year, "_", month),
                y=n_affected,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  geom_point(aes(x=paste(year, "_", month),
                 y=n_affected,
                 group=`Disaster Type`,
                 color=`Disaster Type`)) +
  scale_color_viridis_d(option="F") +
  labs(x="year_month", y="n_affected", title="AFG n_affected") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

disaster_AFG_type %>% filter(`Disaster Type` != "Drought") %>% 
  ggplot() +
  geom_line(aes(x=paste(year, "_", month),
                y=n_affected,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  geom_point(aes(x=paste(year, "_", month),
                 y=n_affected,
                 group=`Disaster Type`,
                 color=`Disaster Type`)) +
  scale_color_viridis_d(option="F") +
  labs(x="year_month", y="n_affected", title="AFG n_affected") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

disaster_Afg %>% filter(year < 2016 & `Disaster Type` == "Drought") %>% view

disaster_SDN_type <- disaster_SDN %>% filter(year > 2016) %>%  group_by(year, month, `Disaster Type`) %>% summarize(n_event=n(), n_affected=`Total Affected`[1])
disaster_SDN_type %>% ggplot() +
  geom_line(aes(x=paste(year, "_", month),
                y=n_event,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  geom_point(aes(x=paste(year, "_", month),
                y=n_event,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  scale_color_viridis_d(option="F") +
  labs(x="year_month", y="n_disasters", title="SDN n_disasters") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

disaster_SDN_type %>% ggplot() +
  geom_line(aes(x=paste(year, "_", month),
                y=n_affected,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  geom_point(aes(x=paste(year, "_", month),
                 y=n_affected,
                 group=`Disaster Type`,
                 color=`Disaster Type`)) +
  scale_color_viridis_d(option="F") +
  labs(x="year_month", y="n_affected", title="SDN n_affected") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

disaster_SDN_type %>% filter(`Disaster Type` != "Drought") %>% 
  ggplot() +
  geom_line(aes(x=paste(year, "_", month),
                y=n_affected,
                group=`Disaster Type`,
                color=`Disaster Type`)) +
  geom_point(aes(x=paste(year, "_", month),
                 y=n_affected,
                 group=`Disaster Type`,
                 color=`Disaster Type`)) +
  scale_color_viridis_d(option="F") +
  labs(x="year_month", y="n_affected", title="SDN n_affected") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## crop calendar
crop_maize %>% names
crop_millet %>% names
crop_rice %>% names
crop_sorghum %>% names
crop_soybean %>% names
crop_wheat %>% names
crop_wheat %>% select(Iran, Uzbekistan) %>% print(n=25)
crop_calendar_AFG <- tibble(date=crop_millet$category,
                            millet=ifelse(crop_millet$Pakistan == 4, 1, 0),
                            wheat=ifelse(crop_wheat$Iran == 4, 1, 0)) %>% 
  mutate(wheat=ifelse(crop_wheat$Uzbekistan == 4, 1, wheat))

crop_calendar_SDN <- tibble(date=crop_maize$category)
crop_calendar_SDN$maize <- crop_maize %>% select(Ethiopia...Belg.Season, Ethiopia...Meher.Season) %>% apply(1, function(x) ifelse(4 %in% x, 1, 0))
crop_calendar_SDN$millet <- crop_millet %>% select(Chad, Ethiopia, Sudan) %>% apply(1, function(x) ifelse(4 %in% x, 1, 0))
crop_calendar_SDN$sorghum <- crop_sorghum %>% select(Chad, Egypt) %>% apply(1, function(x) ifelse(4 %in% x, 1, 0))
crop_calendar_SDN$wheat <- crop_wheat %>% select(Egypt) %>% apply(1, function(x) ifelse(4 %in% x, 1, 0))
