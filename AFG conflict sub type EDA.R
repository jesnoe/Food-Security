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
library(SPEI)

{
  AFG_map <- read_sf("Food Security/geoBoundaries-AFG-ADM1.geojson")
  AFG_map$shapeName[which(AFG_map$shapeName == "Ghanzi")] <- "Ghazni"
  AFG_map$shapeName[which(AFG_map$shapeName == "Sar-e Pol")] <- "Sar_e_Pol"
  
  # https://data.humdata.org/dataset/afg-rainfall-subnational
  afg_adm_codes <- read_xlsx(("Food Security/AFG adm2 boundaries.xlsx")) %>% select(ADM1_EN, ADM1_PCODE) %>% unique %>% arrange(ADM1_EN)
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Hilmand")] <- "Helmand"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Hirat")] <- "Herat"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Jawzjan")] <- "Jowzjan"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Jawzjan")] <- "Jowzjan"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Maidan Wardak")] <- "Wardak"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Nimroz")] <- "Nimruz"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Paktya")] <- "Paktia"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Panjsher")] <- "Panjshir"
  afg_adm_codes$ADM1_EN[which(afg_adm_codes$ADM1_EN == "Sar-e-Pul")] <- "Sar_e_Pol"
  afg_precipitation <- read.csv("Food security/afg-rainfall-adm2-full.csv") %>% as_tibble %>% 
    mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
    filter(date >= as.Date("8/1/2016", "%m/%d/%Y") & date < as.Date("5/1/2024", "%m/%d/%Y")) %>% 
    mutate(ADM1_PCODE = substr(ADM2_PCODE, 1, 4))
  afg_precipitation <- afg_precipitation %>% 
    group_by(date, ADM1_PCODE) %>% 
    summarize(across(rfh:r3h_avg, function(x) sum(x))) %>% 
    mutate(year = year(date),
           month = month(date),
           year_month = paste(year, month, sep="_")) %>% 
    group_by(ADM1_PCODE, year, month, year_month) %>% 
    summarize(across(c(date, rfh:r3h_avg), function(x) rev(x)[1])) %>% 
    left_join(afg_adm_codes %>% rename(Area=ADM1_EN), by="ADM1_PCODE")
  
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
  conflict_ME <- read.csv("Food Security/conflict_2016-01-01-2024-12-31-Caucasus_and_Central_Asia.csv") %>% as_tibble
  
  IPC_AS <- read_xlsx("Food Security/Asia - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(is.na(Population))
  IPC_AS$Area_Phase <- IPC_AS %>%
    select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
    apply(1, function(x) ifelse(sum(is.na(x)) == 5, 0, which.max(x))) %>% as.factor
  IPC_AS$Date <- as.Date(paste(IPC_AS$Date, "01"), format="%b %Y %d")
  IPC_AS$Year <- year(IPC_AS$Date) %>% as.factor
  IPC_AS$Month <- month(IPC_AS$Date) %>% as.factor
  IPC_AS <- IPC_AS %>% relocate(Country:Area_id, Year, Month)
  IPC_AS$Subarea <- gsub(" c_[0-9]+$", "", IPC_AS$Subarea)
  IPC_AS$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_AS$Subarea)
  
  IPC_AFG <- IPC_AS %>% filter(Country == "Afghanistan") # 549
  IPC_AFG <- IPC_AFG %>% 
    mutate(across(Area:Subarea, function(x) gsub("Jawzjan", "Jowzjan", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Hirat", "Herat", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Hilmand", "Helmand", x))) %>%
    mutate(across(Area:Subarea, function(x) gsub("Nimroz", "Nimruz", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Paktya", "Paktia", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Panjsher", "Panjshir", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Sari pul", "Sar_e_Pol", x))) %>%
    mutate(across(Area:Subarea, function(x) gsub(" Urban", "", x)))
  
  IPC_AFG <- IPC_AFG %>% 
    mutate(Area = ifelse(is.na(Area) & Subarea %in% AFG_map$shapeName, Subarea, Area))
  IPC_AFG_provinces_long <- IPC_AFG %>%
    group_by(Area, Year, Month) %>% 
    summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
    mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
  
  IPC_AFG_provinces <- IPC_AFG_provinces_long %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
    pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio) %>% 
    relocate(Area, `Phase_3+ratio_2017_5`, `Phase_3+ratio_2017_9`)
  
  IPC_AFG_year_month <- IPC_AFG %>% select(Year, Month) %>% unique %>%
    mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric),
           year_month = paste(Year, Month, sep="_")) %>%
    arrange(Year, Month)
  
  IPC_AFG_year_month_list <- list(ym_t = IPC_AFG_year_month)
  prev_IPC_AFG_year_month <- IPC_AFG_year_month
  for (i in 1:12) {
    IPC_AFG_year_month_i <- prev_IPC_AFG_year_month %>% 
      mutate(Month = Month - 1,
             Year = ifelse(Month > 0, Year, Year - 1),
             Month = ifelse(Month > 0, Month, 12),
             year_month = paste(Year, Month, sep="_"))
    IPC_AFG_year_month_list[[paste0("ym_t_", i)]] <- IPC_AFG_year_month_i
    prev_IPC_AFG_year_month <- IPC_AFG_year_month_i
  }
  
  FSI_AFG <- FSI %>% filter(Country == "Afghanistan")
  disaster_AFG <- disaster %>% filter(Country == "Afghanistan") %>% # 214
    rename(year=`Start Year`, month=`Start Month`)
  disaster_AFG_separate_locations <- read.csv("Food Security/Disaster Afghanistan locations.csv") %>% as_tibble %>% 
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
  
  disaster_AFG <- disaster_AFG_separate_locations %>%
    select(DisNo., lat, long, Subregion, Region) %>% 
    left_join(disaster_AFG %>%
                select(-Region, -Subregion) %>% 
                rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
  conflict_AFG <- conflict_ME %>% filter(country == "Afghanistan") # 66,500 # much more than CAR
  conflict_AFG$event_date <- as.Date(conflict_AFG$event_date, format="%d %B %Y")
  conflict_AFG$month <- month(conflict_AFG$event_date)
  conflict_AFG <- conflict_AFG %>% 
    relocate(event_id_cnty, event_date, year, month) %>% 
    mutate(admin1 = gsub("Urozgan", "Uruzgan", admin1)) %>% 
    mutate(admin1 = gsub("Sar-e Pol", "Sar_e_Pol", admin1))
  
  # https://ipad.fas.usda.gov/countrysummary/Default.aspx?id=AF
  # plant: 10~12, growing: 1~4, harvest: 5~7
  AFG_wheat_barley <- 5:7
  AFG_corn_rice <- 8:10
  
  crop_seasons_AFG <- tibble(month=1:12,
                             season=c(rep("grow", 4), rep("harvest", 3), rep("none", 2), rep("plant", 3)))
  no_crop_areas_AFG <- c("Ghor", "Nimruz", "Nuristan", "Panjshir")
  
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
  AFG_harvest <- list(wheat_barley=AFG_wheat_barley,
                      corn_rice=AFG_corn_rice)

  # https://data.humdata.org/dataset/afg-rainfall-subnational
  precipitation_AFG <- read.csv("Food security/afg-rainfall-adm2-full.csv") %>% as_tibble %>% 
    mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
    filter(date >= as.Date("5/1/2016", "%m/%d/%Y") & date < as.Date("4/1/2024", "%m/%d/%Y")) %>% 
    mutate(ADM1_PCODE = substr(ADM2_PCODE, 1, 4))
  precipitation_AFG <- precipitation_AFG %>% 
    group_by(date, ADM1_PCODE) %>% 
    summarize(across(rfh:r3h_avg, function(x) sum(x))) %>% 
    mutate(year = year(date),
           month = month(date),
           year_month = paste(year, month, sep="_")) %>% 
    group_by(ADM1_PCODE, year, month, year_month) %>% 
    summarize(across(c(date, rfh:r3h_avg), function(x) rev(x)[1])) %>% # only use 21st records in each month
    left_join(afg_adm_codes %>% rename(Area=ADM1_EN), by="ADM1_PCODE")
  
  rainfall_tbl <- tibble()
  for (adm1 in unique(precipitation_AFG$Area)) {
    rainfall_tbl_area <- precipitation_AFG %>% filter(Area == adm1) %>% select(r1h, r3h)
    rainfall_tbl_area$spi_1h <- spi(rainfall_tbl_area$r1h, 12)$fitted
    rainfall_tbl_area$spi_3h <- spi(rainfall_tbl_area$r3h, 12)$fitted
    rainfall_tbl <- bind_rows(rainfall_tbl, rainfall_tbl_area)
  }
  precipitation_AFG <- left_join(precipitation_AFG, rainfall_tbl %>% select(-r1h, -r3h), by=c("ADM1_PCODE", "year", "month"))
}
{
n_t <- nrow(IPC_AFG_year_month)
oldest_year <- IPC_AFG_year_month$Year[1]-1; oldest_month <- IPC_AFG_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_AFG_year_month$Year[n_t]; latest_month <- IPC_AFG_year_month$Month[n_t]

disaster_AFG %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>% 
  pull(`Disaster Type`) %>% table %>% as_tibble %>% rename(disaster=".")

conflict_NAT <- conflict_AFG
disaster_NAT <- disaster_AFG
IPC_NAT_provinces <- IPC_AFG_provinces
IPC_NAT_provinces_long <- IPC_AFG_provinces_long
IPC_NAT_year_month <- IPC_AFG_year_month
IPC_NAT_year_month_list <- IPC_AFG_year_month_list
crop_seasons <- crop_seasons_AFG
no_crop_areas <- no_crop_areas_AFG
conflict_types <- conflict_AFG %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type)
load("Food Security/lagged_reg_data_list_AFG.RData")
}


conflict_AFG$event_type %>% table
conflict_AFG$sub_event_type %>% table

conflict_AFG %>% ggplot +
  geom_bar(aes(x=event_type, fill=sub_event_type)) + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
conflict_AFG %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)

conflict_AFG %>% arrange(year, month) # the oldest date is 2017-01-31



month_lag_ <- 4
# IPC vs. conflict
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=IPC_diff)) +
  geom_point()

# IPC vs. conflict_diff
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict_diff
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=IPC_diff)) +
  geom_point()

# regression with filtered data
month_lag_ <- 4
IPC_AFG_phase3_long_lagged <- IPC_AFG_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  filter(year > 2017) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(crop_seasons_AFG %>% mutate(season = as.factor(season)), by="month") %>% 
  left_join(lagged_reg_data_list_AFG[[month_lag_]]$conflict_NAT_aggr %>%
              select(-year_month) %>% 
              mutate(event_type = ifelse(event_type %in% c("Battles", "Explosions/Remote violence"),
                                         "Battles_explosions",
                                         ifelse(event_type %in% c("Protests", "Riots"), "Protests_Riots",
                                                "etc.")
              )) %>% 
              group_by(Area, event_type, year, month) %>% 
              summarize(n_events = sum(n_events), fatalities = sum(fatalities)),
            by=c("Area", "year", "month")) %>% 
  pivot_wider(names_from = event_type, values_from = c("n_events", "fatalities")) %>% 
  left_join(lagged_reg_data_list_AFG[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))

IPC_AFG_phase3_long_lagged[is.na(IPC_AFG_phase3_long_lagged)] <- 0
IPC_AFG_phase3_long_lagged <- IPC_AFG_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
  # n_row = 416

lm_conflict_AFG <- lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged[,4:17])
lm_conflict_AFG %>% summary
lm_conflict_diff_AFG <- lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_AFG %>% summary

lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~.+season*n_disaster1, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~.+season*n_events_Battles_explosions, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~.+season*n_disaster1+season*n_events_Battles_explosions, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary

lm(Phase_3above_ratio~.-n_disasters, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~.-n_disasters-affected-deaths, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~.+season*n_disaster1-n_disasters-affected-deaths, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~.-n_disaster1-n_disaster2, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary

lm(Phase_3above_ratio~.-affected-deaths, data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.))) %>% summary
lm(Phase_3above_ratio~.-n_disasters, data=IPC_AFG_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.))) %>% summary
lm(Phase_3above_ratio~.-affected-deaths, data=IPC_AFG_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.))) %>% summary

IPC_AFG_year_month
conflict_map_AFG <- function(i) {
  AFG_map %>% left_join(IPC_AFG_phase3_long_lagged %>%
                          filter(year == IPC_AFG_year_month$Year[i] & month == IPC_AFG_year_month$Month[i]) %>%
                          select(Area, n_events_Battles_explosions) %>% rename(shapeName=Area),
                        by="shapeName") %>% 
    ggplot() + geom_sf(aes(fill=n_events_Battles_explosions)) +
    scale_fill_viridis_c() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
}

conflict_map_AFG(1)
conflict_map_AFG(2)
conflict_map_AFG(3)
conflict_map_AFG(4)
conflict_map_AFG(5)
conflict_map_AFG(6)
conflict_map_AFG(7)
conflict_map_AFG(8)
conflict_map_AFG(9)
conflict_map_AFG(10)

## regression with rainfall data
IPC_AFG_phase3_long_lagged_rainfall <- left_join(IPC_AFG_phase3_long_lagged, precipitation_AFG, by=c("Area", "year", "month"))
# write.csv(IPC_AFG_phase3_long_lagged_rainfall, "Food Security/IPC_AFG_phase3_long_lagged_rainfall.csv", row.names = F)
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
            spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
     select(-spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_class = ifelse(spi_1h > 1, "wet",
                                  ifelse(spi_1h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
            ) %>%
     select(-spi_1h)) %>% summary


lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
            spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
     select(-spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_class = ifelse(spi_3h > 1, "wet",
                                  ifelse(spi_3h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_3h)) %>% summary # different significance with varying month_lag_ and Armed clash only

lm(Phase_3above_ratio~.+season*r3h, data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~.+season*spi_3h, data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary

# regression with filtered data (Battles = Armed clash only)
month_lag_ <- 4
IPC_AFG_phase3_long_lagged <- IPC_AFG_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(lagged_reg_data_list_AFG[[month_lag_]]$conflict_sub_NAT_aggr %>%
              filter(event_type != "Battles" | (event_type == "Battles" & sub_event_type == "Armed clash")) %>% 
              select(-year_month) %>% 
              mutate(event_type = ifelse(event_type %in% c("Battles", "Explosions/Remote violence"),
                                         "Battles_explosions",
                                         ifelse(event_type %in% c("Protests", "Riots"), "Protests_Riots",
                                                "etc.")
              )) %>% 
              group_by(Area, event_type, year, month) %>% 
              summarize(n_events = sum(n_events), fatalities = sum(fatalities)),
            by=c("Area", "year", "month")) %>% 
  mutate(event_type = ifelse(is.na(event_type), "etc.", event_type)) %>% 
  complete(year, month, event_type) %>%
  left_join(crop_seasons_AFG %>% mutate(season = as.factor(season)), by="month") %>% 
  filter(!is.na(Phase_3above_ratio)) %>% 
  pivot_wider(names_from = event_type, values_from = c("n_events", "fatalities")) %>% 
  left_join(lagged_reg_data_list_AFG[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))
IPC_AFG_phase3_long_lagged[is.na(IPC_AFG_phase3_long_lagged)] <- 0
IPC_AFG_phase3_long_lagged <- IPC_AFG_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         IPC_t_2 = lag(Phase_3above_ratio, 2),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
IPC_AFG_phase3_long_lagged[is.na(IPC_AFG_phase3_long_lagged)] <- 0

# n_row = 416
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged[,4:17]) %>% summary # without IPC_t_2
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged[,4:18]) %>% summary # with IPC_t_2 (insignificant)
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.))) %>% summary

## regression with rainfall data
IPC_AFG_phase3_long_lagged_rainfall <- left_join(IPC_AFG_phase3_long_lagged, precipitation_AFG, by=c("Area", "year", "month"))
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
            spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
     select(-spi_1h)) %>% summary ##
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_class = ifelse(spi_1h > 1, "wet",
                                  ifelse(spi_1h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_1h)) %>% summary

lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
            spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
     select(-spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_class = ifelse(spi_3h > 1, "wet",
                                  ifelse(spi_3h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_3h)) %>% summary 

lm(Phase_3above_ratio~.+season*r3h, data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~.+season*spi_3h, data=IPC_AFG_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary

## conflict and disaster ts plots
lagged_months <- 4
IPC_AFG_provinces_long %>% 
  mutate(year=as.Date(paste(Month, Year, "01"), format="%m %Y %d")) %>% 
  ggplot() + ylim(0, 1) +
  geom_line(aes(x=year, y=Phase_3above_ratio, group=Area, color=Area))
ggsave("Food Security/Figs/ts plots/AFG/food insecurity ts plot AFG.png", scale=1)

conflict_AFG_monthly_aggr_by_type <- lagged_reg_data_list_CAF[[lagged_months]]$conflict_sub_NAT_aggr %>% group_by(year, month, event_type) %>%
  summarize(n_events = sum(n_events),
            fatalities = sum(fatalities)) %>% 
  arrange(year, month) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d"))

conflict_AFG_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=fatalities, group=event_type, color=event_type))
ggsave("Food Security/Figs/ts plots/AFG/conflict fatalities ts plot AFG.png", scale=1)

conflict_AFG_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_events, group=event_type, color=event_type))
ggsave("Food Security/Figs/ts plots/AFG/conflict n_events ts plot AFG.png", scale=1)

# disaster1 <- "Flood"; disaster2 <- "Drought"
disaster_AFG_monthly_aggr_by_type <- disaster_AFG %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>%  
  group_by(year, month, `Disaster Type`) %>% 
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T)) %>% 
  rename(type=`Disaster Type`) %>% 
  mutate(year_month = paste(year, month, sep="_")) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>%
  arrange(year, month)

disaster_AFG_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=type, color=type))
ggsave("Food Security/Figs/ts plots/AFG/disaster affected ts plot AFG.png", scale=1)

disaster_AFG_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=type, color=type))
ggsave("Food Security/Figs/ts plots/AFG/disaster deaths ts plot AFG.png", scale=1)

disaster_AFG_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=type, color=type))
ggsave("Food Security/Figs/ts plots/AFG/disaster n_disasters ts plot AFG.png", scale=1)

disaster_AFG_monthly_aggr_by_Area <- disaster_AFG %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>%  
  group_by(Region, year, month) %>%
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T)) %>% 
  rename(Area=Region) %>%
  mutate(year_month = paste(year, month, sep="_")) %>% 
  mutate(year_month=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>%
  arrange(year, month)

disaster_AFG_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=Area, color=Area))

disaster_AFG_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=Area, color=Area))

disaster_AFG_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=Area, color=Area))

lagged_data_by_m <- function(lagged_months, min_t) { # min_t: the number of year_months in lagged_reg_data from the most recent one
  conflict_AFG_aggr <- conflict_AFG %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_AFG_aggr <- conflict_AFG_aggr[,-(1:2)] %>% 
    complete(year_month,
             event_type) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  conflict_AFG_aggr[is.na(conflict_AFG_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_AFG_year_month)) {
    year_i <- IPC_AFG_year_month$Year[i]
    month_i <- IPC_AFG_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_AFG_aggr$year == prev_year & conflict_AFG_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_AFG_aggr$year == year_i & conflict_AFG_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_AFG_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type) %>% 
      summarize(n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_AFG_aggr <- lagged_conflict[[IPC_AFG_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_AFG_aggr <- bind_rows(conflict_AFG_aggr, tbl)
  }
  conflict_AFG_aggr <- conflict_AFG_aggr[-1,]
  
  conflict_AFG_n_events <- conflict_AFG_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(n_events=sum(n_events)) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
  conflict_AFG_n_events[is.na(conflict_AFG_n_events)] <- 0
  
  conflict_AFG_fatalities <- conflict_AFG_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
  conflict_AFG_fatalities[is.na(conflict_AFG_fatalities)] <- 0
  
  conflict_sub_AFG_aggr <- conflict_AFG %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, sub_event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_sub_AFG_aggr <- conflict_sub_AFG_aggr[,-(1:2)] %>% 
    complete(year_month,
             sub_event_type) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    left_join(conflict_types, by="sub_event_type") %>% 
    arrange(year, month)
  
  conflict_sub_AFG_aggr %>% filter(Area == "Badakhshan" & year_month == "2017_1")
  
  conflict_sub_AFG_aggr[is.na(conflict_sub_AFG_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_AFG_year_month)) {
    year_i <- IPC_AFG_year_month$Year[i]
    month_i <- IPC_AFG_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_sub_AFG_aggr$year == prev_year & conflict_sub_AFG_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_sub_AFG_aggr$year == year_i & conflict_sub_AFG_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_sub_AFG_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type, sub_event_type) %>% 
      summarize(event_type = event_type[1],
                n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_sub_AFG_aggr <- lagged_conflict[[IPC_AFG_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_sub_AFG_aggr <- bind_rows(conflict_sub_AFG_aggr, tbl)
  }
  conflict_sub_AFG_aggr <- conflict_sub_AFG_aggr[-1,]
  
  disaster_AFG_monthly_aggr <- disaster_AFG %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>%  
    group_by(Region, year, month) %>% 
    summarise(n_disasters=n(),
              affected=sum(`Total Affected`, na.rm=T),
              deaths=sum(`Total Deaths`, na.rm=T),
              n_floods=sum(`Disaster Type` == "Flood"),
              n_droughts=sum(`Disaster Type` == "Drought")) %>% 
    rename(Area=Region) %>% 
    mutate(year_month = paste(year, month, sep="_")) %>% 
    # mutate(year_month=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>% 
    arrange(year, month)
  
  disaster_AFG_monthly_aggr <- disaster_AFG_monthly_aggr[,-(2:3)] %>% 
    complete(year_month = lapply(IPC_AFG_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  disaster_AFG_monthly_aggr[is.na(disaster_AFG_monthly_aggr)] <- 0
  
  lagged_disaster <- list()
  for (i in 1:nrow(IPC_AFG_year_month)) {
    year_i <- IPC_AFG_year_month$Year[i]
    month_i <- IPC_AFG_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(disaster_AFG_monthly_aggr$year == prev_year & disaster_AFG_monthly_aggr$month == prev_month)[1]
    months_index_i <- rev(which(disaster_AFG_monthly_aggr$year == year_i & disaster_AFG_monthly_aggr$month == month_i))[1]
    lagged_disaster[[year_month_i]] <- disaster_AFG_monthly_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area) %>% 
      summarize(across(n_disasters:n_droughts, function(x) sum(x))) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  disaster_AFG_monthly_aggr <- lagged_disaster[[IPC_AFG_year_month$year_month[1]]][1,]
  for (tbl in lagged_disaster) {
    disaster_AFG_monthly_aggr <- bind_rows(disaster_AFG_monthly_aggr, tbl)
  }
  disaster_AFG_monthly_aggr <- disaster_AFG_monthly_aggr[-1,]
  
  
  disaster_AFG_monthly_events <- disaster_AFG_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disasters=sum(n_disasters)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
  disaster_AFG_monthly_events[is.na(disaster_AFG_monthly_events)] <- 0
  
  disaster_AFG_monthly_flood <- disaster_AFG_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_floods=sum(n_floods)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_floods_", names_from = c(year, month), values_from = n_floods)
  disaster_AFG_monthly_flood[is.na(disaster_AFG_monthly_flood)] <- 0
  
  disaster_AFG_monthly_drought <- disaster_AFG_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_droughts=sum(n_droughts)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_droughts_", names_from = c(year, month), values_from = n_droughts)
  disaster_AFG_monthly_drought[is.na(disaster_AFG_monthly_drought)] <- 0
  
  disaster_AFG_monthly_affected <- disaster_AFG_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
  disaster_AFG_monthly_affected[is.na(disaster_AFG_monthly_affected)] <- 0
  
  disaster_AFG_monthly_deaths <- disaster_AFG_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_deaths=log(1+sum(deaths, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "log_deaths_", names_from = c(year, month), values_from = log_deaths)
  disaster_AFG_monthly_deaths[is.na(disaster_AFG_monthly_deaths)] <- 0
  
  
  # regression data
  IPC_ncol <- ncol(IPC_AFG_provinces)
  conflict_ncol <- ncol(conflict_AFG_n_events) # 85
  conflict_fatalities_ncol <- ncol(conflict_AFG_fatalities) # 85
  disaster_ncol <- ncol(disaster_AFG_monthly_events) # 15
  disaster_affected_ncol <- ncol(disaster_AFG_monthly_affected) # 15
  names(conflict_AFG_n_events)[-1] <- paste0("c_n_events_", names(conflict_AFG_n_events)[-1])
  names(conflict_AFG_fatalities)[-1] <- paste0("log_fatal_", names(conflict_AFG_fatalities)[-1])
  reg_data_i <- IPC_AFG_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
    left_join(conflict_AFG_n_events[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>%
    left_join(conflict_AFG_fatalities[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>% 
    left_join(disaster_AFG_monthly_events[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_AFG_monthly_flood[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_AFG_monthly_drought[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_AFG_monthly_affected[,c(1, disaster_ncol)], by="Area") %>%
    left_join(disaster_AFG_monthly_deaths[,c(1, disaster_ncol)], by="Area") %>% 
    mutate(year = rev(IPC_AFG_year_month$Year)[1])
  names(reg_data_i) <- gsub("2024_3", "t", names(reg_data_i))
  names(reg_data_i) <- gsub("2023_10", "t_1", names(reg_data_i))
  names(reg_data_i) <- gsub("2023_4", "t_2", names(reg_data_i))
  names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
  lagged_reg_data <- reg_data_i[,-1]
  lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_AFG_year_month$Year[14], IPC_AFG_year_month$Month[14], 1), format="%Y %m %d") -
                                             as.Date(paste(IPC_AFG_year_month$Year[13], IPC_AFG_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
  lagged_reg_data$wheat_barley <- time_since_harvest(IPC_AFG_year_month$Month[14], "wheat_barley", AFG_harvest)
  reg_data_names <- names(lagged_reg_data)
  for (i in 1:min_t) {
    IPC_col_index <- IPC_ncol - i
    conflict_col_index <- conflict_ncol - 6*i
    disaster_col_index <- disaster_ncol - i
    reg_data_i <- IPC_AFG_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
      left_join(conflict_AFG_n_events[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>%
      left_join(conflict_AFG_fatalities[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>% 
      left_join(disaster_AFG_monthly_events[,c(1, disaster_col_index)], by="Area") %>%
      left_join(disaster_AFG_monthly_flood[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_AFG_monthly_drought[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_AFG_monthly_affected[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_AFG_monthly_deaths[,c(1, disaster_col_index)], by="Area") %>% 
      mutate(year = rev(IPC_AFG_year_month$Year)[IPC_col_index])
    reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_AFG_year_month$Year[14-i], IPC_AFG_year_month$Month[14-i], 1), format="%Y %m %d") -
                                          as.Date(paste(IPC_AFG_year_month$Year[13-i], IPC_AFG_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
    
    reg_data_i$wheat_barley <- time_since_harvest(IPC_AFG_year_month$Month[14-i], "wheat_barley", AFG_harvest)
    reg_data_i <- as.matrix(reg_data_i[,-1])
    colnames(reg_data_i) <- reg_data_names
    lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
  }
  lagged_reg_data[is.na(lagged_reg_data)] <- 0
  
  result <- list()
  result$lagged_reg_data <- lagged_reg_data
  result$conflict_AFG_aggr <- conflict_AFG_aggr
  result$conflict_sub_AFG_aggr <- conflict_sub_AFG_aggr
  result$disaster_AFG_monthly_aggr <- disaster_AFG_monthly_aggr
  return(result)
} # function end

n_t <- nrow(IPC_AFG_year_month)
oldest_year <- IPC_AFG_year_month$Year[1]-1; oldest_month <- IPC_AFG_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_AFG_year_month$Year[n_t]; latest_month <- IPC_AFG_year_month$Month[n_t]

lagged_reg_data_list <- list()
for (i in 1:4) {
  m <- i
  lagged_reg_data_list[[paste0("months_", i)]] <- lagged_data_by_m(m, 9) # min_t = 9
}


IPC_AFG_phase3_long <- IPC_AFG_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  mutate(IPC_diff = Phase_3above_ratio - lag(Phase_3above_ratio)) %>% 
  left_join(crop_seasons, by="month")


conflict_map <- function(month_aggr, filter_=F, growing_region=F) {
  IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_AFG_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  if (is.character(filter_)) IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  IPC_AFG_phase3_long_month %>% ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=n_events, group=event_type, color=event_type)) +
    ggtitle(filter_)
}


conflict_diff_map <- function(month_aggr, filter_=F, growing_region=F) {
  IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_AFG_aggr %>% select(-year_month),
              by=c("Area", "year", "month")) 
  
  if (is.character(filter_)) IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  IPC_AFG_phase3_long_month %>% ggplot() +
    geom_point(aes(x=IPC_diff, y=n_events, group=event_type, color=event_type))
}

conflict_sub_n_events <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=n_events, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_sub_fatalities <- function(month_aggr,conflict_type, growing_region=F) {
  IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_n_events_harvest <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=season, color=season)) +
      ggtitle(conflict_type)
  }else{
    IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=season, color=season)) +
      ggtitle(conflict_type) 
  }
}

conflict_fatalities_harvest <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=fatalities, group=season, color=season)) +
      ggtitle(conflict_type)
  }else{
    IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=season, color=season)) +
      ggtitle(conflict_type) 
  }
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))}

conflict_sub_n_events_diff <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=IPC_diff, y=n_events, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_sub_fatalities_diff <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_AFG_phase3_long_conflict <- IPC_AFG_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_AFG_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=IPC_diff, y=fatalities, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_n_events_national <- function(month_aggr, filter_=F, growing_region=F, diff_=F) {
  IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_AFG_aggr %>% select(-year_month) %>% 
                group_by(year, month, event_type) %>%
                summarize(n_events = sum(n_events)),
              by=c("year", "month"))
  
  if (is.character(filter_)) IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_AFG_phase3_long_month %>% ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=Area, color=Area)) +
      ggtitle(filter_)
  }else{
    IPC_AFG_phase3_long_month %>% ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=Area, color=Area)) +
      ggtitle(filter_)
  }
}


conflict_AFG %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)

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

no_crop_areas
IPC_AFG_phase3_long %>% filter(IPC_diff > 0.3)

# https://ipad.fas.usda.gov/countrysummary/Default.aspx?id=AF
# plant: 10~12, growing: 1~4, harvest: 5~7
IPC_AFG_phase3_long_conflict <- left_join(IPC_AFG_phase3_long,
                                          lagged_reg_data_list[[month_aggr]]$conflict_sub_AFG_aggr,
                                          by=c("Area", "year", "month"))

conflict_sub_fatalities(1, conflict_6types[1])

disaster_map <- function(month_aggr, disaster_type) {
  IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$disaster_AFG_monthly_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  names(IPC_AFG_phase3_long_month)[which(names(IPC_AFG_phase3_long_month) == disaster_type)] <- "y_val"
  
  IPC_AFG_phase3_long_month %>% ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=y_val, group=season, color=season)) +
    ylab(disaster_type)
}

disaster_diff_map <- function(month_aggr, disaster_type) {
  IPC_AFG_phase3_long_month <- IPC_AFG_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$disaster_AFG_monthly_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  names(IPC_AFG_phase3_long_month)[which(names(IPC_AFG_phase3_long_month) == disaster_type)] <- "y_val"
  
  IPC_AFG_phase3_long_month %>% ggplot() +
    geom_point(aes(x=IPC_diff, y=y_val, group=season, color=season)) +
    ylab(disaster_type)
}

month_aggr_ <- 2
disaster_types <- names(lagged_reg_data_list$months_1$disaster_AFG_monthly_aggr)[2:6]
disaster_map(month_aggr_, disaster_types[2])
disaster_map(month_aggr_, disaster_types[3])
disaster_map(month_aggr_, disaster_types[4])
disaster_map(month_aggr_, disaster_types[5])

disaster_diff_map(month_aggr_, disaster_types[2])
disaster_diff_map(month_aggr_, disaster_types[3]) ### higher diff with higher death?
disaster_diff_map(month_aggr_, disaster_types[4])
disaster_diff_map(month_aggr_, disaster_types[5])

# harvest season?

# may need to divide Battles, explosions further
conflict_AFG %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)
conflict_AFG %>% group_by()

# Fragile State Index (FSI) ts plot
FSI_AFG %>%
  select(-Country, -Rank, -Total) %>% 
  pivot_longer(-Year, names_to = "Indicator", values_to = "FSI") %>% 
  ggplot() +
  geom_line(aes(x=Year, y=FSI, group=Indicator, color=Indicator)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="Fragile State Index")




