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

{
afg_map <- read_sf("Food Security/geoBoundaries-AFG-ADM1.geojson")
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
  mutate(Region = gsub("Vardak", "Wardak", Region)) %>%
  mutate(Region = gsub("Zabol", "Zabul", Region))

disaster_Afg <- disaster_Afg_separate_locations %>%
  select(DisNo., lat, long, Subregion, Region) %>% 
  left_join(disaster_Afg %>%
              select(-Region, -Subregion) %>% 
              rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
conflict_Afg <- conflict_ME %>% filter(country == "Afghanistan") # 66,500 # much more than CAR
conflict_Afg$event_date <- as.Date(conflict_Afg$event_date, format="%d %B %Y")
conflict_Afg$month <- month(conflict_Afg$event_date)
conflict_Afg <- conflict_Afg %>% 
  relocate(event_id_cnty, event_date, year, month) %>% 
  mutate(admin1 = gsub("Urozgan", "Uruzgan", admin1))
}

FSI_Afg
AFG_provinces <- c("Badakhshan", "Baghlan", "Balkh", "Bamiyan", "Daikondi", "Farah", "Faryab", "Ghazni", "Ghor", "Helmand", "Herat", "Kandahar", "Kapisa",
                   "Kunduz", "Lagh-man", "Nangarhar", "Nimroz", "Nuristan", "Paktia", "Paktika", "Panjshir", "Sar-e Pul", "Samangan", "Tachar", "Uruzgan",
                   "Wardak", "Zabul")
for (province in afg_map$shapeName) {
  print(paste(province, IPC_Afg %>% filter(Subarea == province) %>% nrow))
}

afg_map$shapeName %>% sort
afg_map$shapeName[which(afg_map$shapeName == "Ghanzi")] <- "Ghazni"
IPC_Afg <- IPC_Afg %>% 
  mutate(across(Area:Subarea, function(x) gsub("Jawzjan", "Jowzjan", x))) %>% 
  mutate(across(Area:Subarea, function(x) gsub("Hirat", "Herat", x))) %>% 
  mutate(across(Area:Subarea, function(x) gsub("Hilmand", "Helmand", x))) %>%
  mutate(across(Area:Subarea, function(x) gsub("Nimroz", "Nimruz", x))) %>% 
  mutate(across(Area:Subarea, function(x) gsub("Paktya", "Paktia", x))) %>% 
  mutate(across(Area:Subarea, function(x) gsub("Panjsher", "Panjshir", x))) %>% 
  mutate(across(Area:Subarea, function(x) gsub("Sari pul", "Sar-e Pol", x))) %>% 
  mutate(across(Area:Subarea, function(x) gsub(" Urban", "", x)))

IPC_Afg %>% filter(!(Area %in% afg_map$shapeName)) %>% pull(Area) %>% unique
IPC_Afg %>% filter(!(Subarea %in% afg_map$shapeName)) %>% pull(Subarea) %>% unique %>% sort

IPC_Afg <- IPC_Afg %>% 
  mutate(Area = ifelse(is.na(Area) & Subarea %in% afg_map$shapeName, Subarea, Area))

IPC_Afg_provinces <- IPC_Afg %>%
  group_by(Area, Year, Month) %>% 
  summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
  mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
IPC_Afg_provinces <- IPC_Afg_provinces %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
  pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio) %>% 
  relocate(Area, `Phase_3+ratio_2017_5`, `Phase_3+ratio_2017_9`)
IPC_Afg_provinces[,-1] %>% apply(2, function(x) sum(is.na(x)))

IPC_Afg %>% filter(Year == 2017 & Month == 5)

conflict_Afg$sub_event_type %>% table # 24 types
conflict_Afg %>% filter(event_type == "Protests") %>% pull(sub_event_type) %>% table
conflict_Afg %>% filter(event_type == "Strategic developments") %>% pull(sub_event_type) %>% table
conflict_Afg %>% filter(event_type == "Violence against civilians") %>% pull(sub_event_type) %>% table

  mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric)) %>% unique %>% arrange(Year, Month)
conflict_Afg_n_events <- conflict_Afg %>% 
  filter(!(year == 2024 & month > 3)) %>% 
  filter(!(year == 2017 & month < 3)) %>% 
  group_by(year, month, admin1, event_type) %>% 
  summarise(n_events=n()) %>% 
  rename(Area=admin1) %>% 
  arrange(year, month)

IPC_Afg_year_month <- IPC_Afg %>% select(Year, Month) %>% unique %>%
  mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric)) %>% arrange(Year, Month)
prev_year <- 2017; prev_month <- 3
for (i in 2:nrow(IPC_Afg_year_month)) {
  year_i <- IPC_Afg_year_month$Year[i]
  month_i <- IPC_Afg_year_month$Month[i]
  prev_months_index <- which(conflict_Afg_n_events$year == prev_year & conflict_Afg_n_events$month == prev_month)
  prev_months_index <- prev_months_index[length(prev_months_index)]
  months_index_i <- which(conflict_Afg_n_events$year == year_i & conflict_Afg_n_events$month == month_i)[1]
  if (year_i != prev_year) conflict_Afg_n_events$year[(prev_months_index+1):months_index_i] <- year_i
  conflict_Afg_n_events$month[(prev_months_index+1):months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}

conflict_Afg_n_events <- conflict_Afg_n_events %>% 
  group_by(year, month, Area, event_type) %>% 
  summarise(n_events=sum(n_events)) %>% 
  pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
conflict_Afg_n_events

disaster_Afg$`Disaster Type` %>% table
disaster_Afg_monthly_events <- disaster_Afg %>% 
  filter(!(year >= 2024 & month > 3)) %>%
  filter(year > 2016) %>% 
  filter(!(year == 2017 & month < 3)) %>% 
  group_by(Region, year, month) %>% 
  summarise(n_disasters=n()) %>% 
  rename(Area=Region) %>% 
  mutate(year_month=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>% 
  arrange(year, month)

prev_year <- 2017; prev_month <- 3
for (i in 2:nrow(IPC_Afg_year_month)) {
  prev_year_month_i <- as.Date(paste(prev_month, prev_year, "01"), format="%m %Y %d")
  year_i <- IPC_Afg_year_month$Year[i]
  month_i <- IPC_Afg_year_month$Month[i]
  year_month_i <- as.Date(paste(month_i, year_i, "01"), format="%m %Y %d")
  months_index_i <- which(disaster_Afg_monthly_events$year_month > prev_year_month_i & disaster_Afg_monthly_events$year_month <= year_month_i)
  disaster_Afg_monthly_events$year[months_index_i] <- year_i
  disaster_Afg_monthly_events$month[months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}
disaster_Afg_monthly_events <- disaster_Afg_monthly_events %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_disasters=sum(n_disasters)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)

disaster_Afg_monthly_events



lm(`Phase_3+ratio_2024_3`~.-Area,
   data = IPC_Afg_provinces %>% select(`Phase_3+ratio_2024_3`:`Phase_3+ratio_2021_10`)) %>% summary

# Is "Phase_3+ratio" affected by previous periods?
for (i in 15:9) {
  dependent_var <- names(IPC_Afg_provinces)[i]
  autoregression_i <- lm(formula(paste0("`", dependent_var, "`~.")),
     data = IPC_Afg_provinces[,(i-4):i])
  print(dependent_var)
  print(summary(autoregression_i))
}

IPC_ncol <- ncol(IPC_Afg_provinces)
conflict_ncol <- ncol(conflict_Afg_n_events) # 84
disaster_ncol <- ncol(disaster_Afg_monthly_events) # 13

conflict_Afg$event_type %>% table # 6 types
reg_data_i <- IPC_Afg_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
  left_join(conflict_Afg_n_events[,c(1, conflict_ncol:(conflict_ncol-6*1))], by="Area") %>% 
  left_join(disaster_Afg_monthly_events[,c(1, disaster_ncol:(disaster_ncol-3))], by="Area")

# lm(`Phase_3+ratio_2024_3`~.,)
FSI_Afg
