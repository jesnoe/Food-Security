# setwd("/Users/R")
# setwd("C:/Users/User/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(tidygeocoder)
library(gridExtra)
library(lubridate)
library(fpp2)
library(sf)
library(sp)
library(reshape2)
library(glmnet)
library(psych)

{# https://www.geoboundaries.org/countryDownloads.html
  COD_map <- read_sf("Food Security/geoBoundaries-COD-ADM1.geojson") %>% 
    mutate(
      shapeName = stri_trans_general(shapeName, "Latin-ASCII"),
      shapeName = gsub("-", " ", shapeName),
      shapeName = gsub("Mai Ndombe", "Mai-Ndombe", shapeName),
      shapeName = gsub("Lower", "Bas", shapeName),
      shapeName = gsub("Upper", "Haut", shapeName),
      shapeName = gsub("North", "Nord", shapeName),
      shapeName = gsub("South", "Sud", shapeName)
    )
  COD_map$shapeName %>% sort
  # IPC_COD$Area %>% unique %>% sort
  
  COD_map$shapeName[which(COD_map$shapeName == "Ghanzi")] <- "Ghazni"
  COD_map$shapeName[which(COD_map$shapeName == "Sar-e Pol")] <- "Sar_e_Pol"
  # COD_adm_codes <- read_xlsx(("Food Security/COD adm2 boundaries.xlsx")) %>% select(ADM1_EN, ADM1_PCODE) %>% unique %>% arrange(ADM1_EN)
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Hilmand")] <- "Helmand"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Hirat")] <- "Herat"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Jawzjan")] <- "Jowzjan"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Jawzjan")] <- "Jowzjan"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Maidan Wardak")] <- "Wardak"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Nimroz")] <- "Nimruz"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Paktya")] <- "Paktia"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Panjsher")] <- "Panjshir"
  # COD_adm_codes$ADM1_EN[which(COD_adm_codes$ADM1_EN == "Sar-e-Pul")] <- "Sar_e_Pol"
    
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
  conflict_AF <- read.csv("Food Security/Africa_conflict_1997-2024_Sep13.csv") %>% as_tibble
  
  IPC_AF <- read_xlsx("Food Security/East and Central Africa - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population))
  IPC_AF$Area_Phase <- IPC_AF %>% select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>% apply(1, function(x) which.max(x)) %>% as.factor
  IPC_AF$Date <- as.Date(paste(IPC_AF$Date, "01"), format="%b %Y %d")
  IPC_AF$Year <- year(IPC_AF$Date) %>% as.factor
  IPC_AF$Month <- month(IPC_AF$Date) %>% as.factor
  IPC_AF <- IPC_AF %>% relocate(Country:Area_id, Year, Month)
  IPC_AF$Subarea <- gsub(" c_[0-9]+$", "", IPC_AF$Subarea)
  IPC_AF$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_AF$Subarea)
  
  IPC_COD <- IPC_AF %>% filter(Country == "Congo, DRC") %>%  # 1058
    mutate(Subarea = stri_trans_general(Subarea, "Latin-ASCII"),
           Subarea = gsub("-", " ", Subarea),
           Subarea = str_to_title(Subarea),
           Subarea = gsub("Mai Ndombe", "Mai-Ndombe", Subarea),
           Area = ifelse(is.na(Area) & Subarea %in% COD_map$shapeName, Subarea, Area),
           Area = stri_trans_general(Area, "Latin-ASCII"),
           Area = gsub("-", " ", Area),
           Area = gsub("Mai Ndombe", "Mai-Ndombe", Area))
  IPC_COD$Area[c(which(IPC_COD$Subarea == "Kasai Central"), which(IPC_COD$Subarea == "Tanganyka"))] <- c("Central Kasai", "Tanganyika")
  IPC_COD$Date %>% unique %>% sort # "2017-06-01" "2018-06-01" "2019-06-01" "2020-07-01" "2021-02-01" "2021-09-01" "2022-07-01" "2023-08-01"
  
  IPC_COD_provinces_long <- IPC_COD %>%
    group_by(Area, Year, Month) %>% 
    summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
    mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
  
  IPC_COD_provinces <- IPC_COD_provinces_long %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
    pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio) %>% 
    relocate(Area, `Phase_3+ratio_2017_6`)
  
  IPC_COD_year_month <- IPC_COD %>% select(Year, Month) %>% unique %>%
    mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric),
           year_month = paste(Year, Month, sep="_")) %>%
    arrange(Year, Month)
  
  IPC_COD_year_month_list <- list(ym_t = IPC_COD_year_month)
  prev_IPC_COD_year_month <- IPC_COD_year_month
  for (i in 1:12) {
    IPC_COD_year_month_i <- prev_IPC_COD_year_month %>% 
      mutate(Month = Month - 1,
             Year = ifelse(Month > 0, Year, Year - 1),
             Month = ifelse(Month > 0, Month, 12),
             year_month = paste(Year, Month, sep="_"))
    IPC_COD_year_month_list[[paste0("ym_t_", i)]] <- IPC_COD_year_month_i
    prev_IPC_COD_year_month <- IPC_COD_year_month_i
  }
  
  FSI_COD <- FSI %>% filter(Country == "Congo Democratic Republic")
  disaster_COD <- disaster %>% filter(Country == "Democratic Republic of the Congo") %>% # 299
    rename(year=`Start Year`, month=`Start Month`)
  disaster_COD_separate_locations <- read.csv("Food Security/Disaster COD locations.csv") %>% as_tibble
  
  disaster_COD <- disaster_COD_separate_locations %>%
    select(DisNo., lat, long, Subregion, Region) %>% 
    left_join(disaster_COD %>%
                select(-Region, -Subregion) %>% 
                rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
  conflict_COD <- conflict_AF %>% filter(country == "Democratic Republic of Congo") # 32,538
  conflict_COD$event_date <- as.Date(conflict_COD$event_date, format="%m/%d/%Y")
  conflict_COD$month <- month(conflict_COD$event_date)
  conflict_COD <- conflict_COD %>% relocate(event_id_cnty, event_date, year, month)
  
  conflict_COD$admin1 %>% unique %>% sort
  
  conflict_COD <- conflict_COD %>% 
    relocate(event_id_cnty, event_date, year, month) %>% 
    mutate(admin1 = gsub("-", " ", admin1),
           admin1 = gsub("Mai Ndombe", "Mai-Ndombe", admin1))
  
  # from https://ipad.fas.usda.gov/countrysummary/Default.aspx?id=CG
  COD_corn <- c(5:7, 10:12)
  COD_south_corn <- 3:5 # Extreme South: Haut Katanga Province
  COD_rice <- c(1:3, 7, 8)
  COD_south_rice <- 1:3
  corn_seasons <- tibble(month=1:12,
                         plant = c(rep(1,3), rep(0,2), rep(1,4), rep(0,3)),
                         grow = c(0,0, rep(1,3), 0,0, rep(1,3), 0,0),
                         harvest = c(rep(0,4), rep(1,3), 0,0, rep(1,3)),
                         none = c(1,0,0, rep(1,3), rep(0,5), 1))
  seasons <- c("plant", "grow", "harvest", "none")
  corn_seasons$season <- apply(corn_seasons, 1, function(x) paste(seasons[which(x[-1]==1)], collapse="/"))
  corn_seasons$season_south <- c(rep("grow",2), rep("harvest",3), rep("none",5), rep("plant",2))
  gsub("/none", "", corn_seasons$season) %>% unique # 5 unique seasons
  
  rice_seasons <- tibble(month=1:12,
                         season=c(rep("harvest", 3), rep("plant",2), "grow", rep("harvest",2), rep("plant", 2), rep("grow", 2)),
                         season_south=c(rep("harvest",3), rep("none",5), rep("plant",2), rep("grow",2)))
  
  time_since_harvest <- function(month., crop, country_harvest_season) {
    if (crop == "wheat_barley") {
      result <- ifelse(month. > country_harvest_season$wheat_barley[3], month. - country_harvest_season$wheat_barley[3],
                       ifelse(month. < country_harvest_season$wheat_barley[1], month. + 12 - country_harvest_season$wheat_barley[3], 0))
    }
    if (crop == "corn_rice") {
      result <- ifelse(month. > country_harvest_season$corn_rice[3], month. - country_harvest_season$corn_rice[3],
                       ifelse(month. < country_harvest_season$corn_rice[1], month. + 12 - country_harvest_season$corn_rice[3], 0))
    }
    return(result)
  }
  COD_harvest <- list(corn=COD_corn,
                      corn_south=COD_south_corn,
                      rice=COD_rice,
                      rice_south=COD_south_rice)
  
  conflict_types <- conflict_COD %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type)
  no_crop_areas_COD <- c("Mongala", "Tshuapa", "Kasai Oriental", "Kinshasa", "Ituri", "Sud Kivu")
}

n_t <- nrow(IPC_COD_year_month)
oldest_year <- IPC_COD_year_month$Year[1]-1; oldest_month <- IPC_COD_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_COD_year_month$Year[n_t]; latest_month <- IPC_COD_year_month$Month[n_t]

disaster_COD %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>% 
  pull(`Disaster Type`) %>% table %>% as_tibble %>% rename(disaster=".")

conflict_NAT <- conflict_COD
disaster_NAT <- disaster_COD
IPC_NAT_provinces <- IPC_COD_provinces
IPC_NAT_provinces_long <- IPC_COD_provinces_long
IPC_NAT_year_month <- IPC_COD_year_month
IPC_NAT_year_month_list <- IPC_COD_year_month_list
crop_seasons <- corn_seasons
no_crop_areas <- no_crop_areas_COD

n_t <- nrow(IPC_COD_year_month)
oldest_year <- IPC_COD_year_month$Year[1]-1; oldest_month <- IPC_COD_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_COD_year_month$Year[n_t]; latest_month <- IPC_COD_year_month$Month[n_t]

lagged_reg_data_list <- list()
for (i in 1:5) {
  m <- i
  lagged_reg_data_list[[paste0("months_", i)]] <- lagged_data_by_m(m, 5) # min_t = 5
}

lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2$lagged_reg_data %>% select(-n_floods_t, -n_droughts_t)) %>% summary()
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2$lagged_reg_data %>% select(-n_disasters_t)) %>% summary()


IPC_COD_phase3_long <- IPC_COD_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  mutate(IPC_diff = Phase_3above_ratio - lag(Phase_3above_ratio))# %>% 
  # left_join(corn_seasons, by="month")


conflict_map <- function(month_aggr, filter_=F, growing_region=F) {
  IPC_COD_phase3_long_month <- IPC_COD_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_COD_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  if (is.character(filter_)) IPC_COD_phase3_long_month <- IPC_COD_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_COD_phase3_long_month <- IPC_COD_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  IPC_COD_phase3_long_month %>% ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=n_events, group=event_type, color=event_type)) +
    ggtitle(filter_)
}


conflict_diff_map <- function(month_aggr, filter_=F, growing_region=F) {
  IPC_COD_phase3_long_month <- IPC_COD_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_COD_aggr %>% select(-year_month),
              by=c("Area", "year", "month")) 
  
  if (is.character(filter_)) IPC_COD_phase3_long_month <- IPC_COD_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_COD_phase3_long_month <- IPC_COD_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  IPC_COD_phase3_long_month %>% ggplot() +
    geom_point(aes(x=IPC_diff, y=n_events, group=event_type, color=event_type))
}

conflict_sub_n_events <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_COD_phase3_long_conflict <- left_join(IPC_COD_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_COD_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=n_events, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_sub_fatalities <- function(month_aggr,conflict_type, growing_region=F) {
  IPC_COD_phase3_long_conflict <- left_join(IPC_COD_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_COD_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_n_events_harvest <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_COD_phase3_long_conflict <- left_join(IPC_COD_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_COD_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=season, color=season)) +
      ggtitle(conflict_type)
  }else{
    IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=season, color=season)) +
      ggtitle(conflict_type) 
  }
}

conflict_fatalities_harvest <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_COD_phase3_long_conflict <- left_join(IPC_COD_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_COD_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=fatalities, group=season, color=season)) +
      ggtitle(conflict_type)
  }else{
    IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=season, color=season)) +
      ggtitle(conflict_type) 
  }
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))}

conflict_sub_n_events_diff <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_COD_phase3_long_conflict <- left_join(IPC_COD_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_COD_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=IPC_diff, y=n_events, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_sub_fatalities_diff <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_COD_phase3_long_conflict <- left_join(IPC_COD_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_COD_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_COD_phase3_long_conflict <- IPC_COD_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_COD_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=IPC_diff, y=fatalities, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_n_events_national <- function(month_aggr, filter_=F, growing_region=F, diff_=F) {
  IPC_COD_phase3_long_month <- IPC_COD_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_COD_aggr %>% select(-year_month) %>% 
                group_by(year, month, event_type) %>%
                summarize(n_events = sum(n_events)),
              by=c("year", "month"))
  
  if (is.character(filter_)) IPC_COD_phase3_long_month <- IPC_COD_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_COD_phase3_long_month <- IPC_COD_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_COD_phase3_long_month %>% ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=Area, color=Area)) +
      ggtitle(filter_)
  }else{
    IPC_COD_phase3_long_month %>% ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=Area, color=Area)) +
      ggtitle(filter_)
  }
}


conflict_COD %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)

conflict_map(1)
conflict_map(1, growing_region = T)
conflict_map(1, "Battles")
conflict_map(1, "Explosions/Remote violence")
conflict_map(1, "Protests")
conflict_map(1, "Riots")
conflict_map(1, "Strategic developments")
conflict_map(1, "Violence against civilians")

conflict_diff_map(1)
conflict_diff_map(2)
conflict_diff_map(3)
conflict_diff_map(4)
conflict_diff_map(1, "Battles")
conflict_diff_map(1, "Explosions/Remote violence")
conflict_diff_map(1, "Protests")
conflict_diff_map(1, "Riots")
conflict_diff_map(1, "Strategic developments")
conflict_diff_map(1, "Violence against civilians")

conflict_6types <- unique(conflict_types$event_type)

month_aggr_ <- 1
conflict_sub_n_events(month_aggr_, conflict_6types[1])
conflict_sub_n_events(month_aggr_, conflict_6types[1], growing_region = T)
conflict_sub_n_events(month_aggr_, conflict_6types[2])
conflict_sub_n_events(month_aggr_, conflict_6types[2], growing_region = T)
conflict_sub_n_events(month_aggr_, conflict_6types[3])
conflict_sub_n_events(month_aggr_, conflict_6types[3], growing_region = T)
conflict_sub_n_events(month_aggr_, conflict_6types[4])
conflict_sub_n_events(month_aggr_, conflict_6types[4], growing_region = T)
conflict_sub_n_events(month_aggr_, conflict_6types[5])
conflict_sub_n_events(month_aggr_, conflict_6types[5], growing_region = T)
conflict_sub_n_events(month_aggr_, conflict_6types[6])
conflict_sub_n_events(month_aggr_, conflict_6types[6], growing_region = T)

conflict_sub_fatalities(month_aggr_, conflict_6types[1])
conflict_sub_fatalities(month_aggr_, conflict_6types[1], growing_region = T)
conflict_sub_fatalities(month_aggr_, conflict_6types[2])
conflict_sub_fatalities(month_aggr_, conflict_6types[2], growing_region = T)
conflict_sub_fatalities(month_aggr_, conflict_6types[3])
conflict_sub_fatalities(month_aggr_, conflict_6types[3], growing_region = T)
conflict_sub_fatalities(month_aggr_, conflict_6types[4])
conflict_sub_fatalities(month_aggr_, conflict_6types[4], growing_region = T)
conflict_sub_fatalities(month_aggr_, conflict_6types[5])
conflict_sub_fatalities(month_aggr_, conflict_6types[5], growing_region = T)
conflict_sub_fatalities(month_aggr_, conflict_6types[6])
conflict_sub_fatalities(month_aggr_, conflict_6types[6], growing_region = T)

conflict_sub_n_events_diff(month_aggr_, conflict_6types[1])
conflict_sub_n_events_diff(month_aggr_, conflict_6types[1], growing_region = T)
conflict_sub_n_events_diff(month_aggr_, conflict_6types[2])
conflict_sub_n_events_diff(month_aggr_, conflict_6types[2], growing_region = T)
conflict_sub_n_events_diff(month_aggr_, conflict_6types[3])
conflict_sub_n_events_diff(month_aggr_, conflict_6types[3], growing_region = T)
conflict_sub_n_events_diff(month_aggr_, conflict_6types[4])
conflict_sub_n_events_diff(month_aggr_, conflict_6types[4], growing_region = T)
conflict_sub_n_events_diff(month_aggr_, conflict_6types[5])
conflict_sub_n_events_diff(month_aggr_, conflict_6types[5], growing_region = T)
conflict_sub_n_events_diff(month_aggr_, conflict_6types[6])
conflict_sub_n_events_diff(month_aggr_, conflict_6types[6], growing_region = T)

conflict_sub_fatalities_diff(month_aggr_, conflict_6types[1])
conflict_sub_fatalities_diff(month_aggr_, conflict_6types[2])
conflict_sub_fatalities_diff(month_aggr_, conflict_6types[3])
conflict_sub_fatalities_diff(month_aggr_, conflict_6types[4])
conflict_sub_fatalities_diff(month_aggr_, conflict_6types[5])
conflict_sub_fatalities_diff(month_aggr_, conflict_6types[6])

conflict_n_events_harvest(month_aggr_, conflict_6types[1])
conflict_n_events_harvest(month_aggr_, conflict_6types[2])
conflict_n_events_harvest(month_aggr_, conflict_6types[3])
conflict_n_events_harvest(month_aggr_, conflict_6types[4])
conflict_n_events_harvest(month_aggr_, conflict_6types[5])
conflict_n_events_harvest(month_aggr_, conflict_6types[6])

conflict_n_events_harvest(month_aggr_, conflict_6types[1], diff_=T)
conflict_n_events_harvest(month_aggr_, conflict_6types[2], diff_=T)
conflict_n_events_harvest(month_aggr_, conflict_6types[3], diff_=T)
conflict_n_events_harvest(month_aggr_, conflict_6types[4], diff_=T)
conflict_n_events_harvest(month_aggr_, conflict_6types[5], diff_=T)
conflict_n_events_harvest(month_aggr_, conflict_6types[6], diff_=T)

conflict_fatalities_harvest(month_aggr_, conflict_6types[1])
conflict_fatalities_harvest(month_aggr_, conflict_6types[2])
conflict_fatalities_harvest(month_aggr_, conflict_6types[3])
conflict_fatalities_harvest(month_aggr_, conflict_6types[4])
conflict_fatalities_harvest(month_aggr_, conflict_6types[5])
conflict_fatalities_harvest(month_aggr_, conflict_6types[6])

conflict_fatalities_harvest(month_aggr_, conflict_6types[1], diff_=T)
conflict_fatalities_harvest(month_aggr_, conflict_6types[2], diff_=T)
conflict_fatalities_harvest(month_aggr_, conflict_6types[3], diff_=T)
conflict_fatalities_harvest(month_aggr_, conflict_6types[4], diff_=T)
conflict_fatalities_harvest(month_aggr_, conflict_6types[5], diff_=T)
conflict_fatalities_harvest(month_aggr_, conflict_6types[6], diff_=T)

conflict_n_events_national(month_aggr_, conflict_6types[1])
conflict_n_events_national(month_aggr_, conflict_6types[1], growing_region = T)
conflict_n_events_national(month_aggr_, conflict_6types[2])
conflict_n_events_national(month_aggr_, conflict_6types[2], growing_region = T)
conflict_n_events_national(month_aggr_, conflict_6types[3])
conflict_n_events_national(month_aggr_, conflict_6types[4])
conflict_n_events_national(month_aggr_, conflict_6types[5])
conflict_n_events_national(month_aggr_, conflict_6types[6])

conflict_n_events_national(month_aggr_, conflict_6types[1], diff_=T)
conflict_n_events_national(month_aggr_, conflict_6types[1], diff_=T, growing_region = T)
conflict_n_events_national(month_aggr_, conflict_6types[2], diff_=T)
conflict_n_events_national(month_aggr_, conflict_6types[3], diff_=T)
conflict_n_events_national(month_aggr_, conflict_6types[4], diff_=T)
conflict_n_events_national(month_aggr_, conflict_6types[5], diff_=T)
conflict_n_events_national(month_aggr_, conflict_6types[6], diff_=T)