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

{
  afg_map <- read_sf("Food Security/geoBoundaries-AFG-ADM1.geojson")
  afg_map$shapeName[which(afg_map$shapeName == "Ghanzi")] <- "Ghazni"
  afg_map$shapeName[which(afg_map$shapeName == "Sar-e Pol")] <- "Sar_e_Pol"
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
  IPC_Afg <- IPC_Afg %>% 
    mutate(across(Area:Subarea, function(x) gsub("Jawzjan", "Jowzjan", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Hirat", "Herat", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Hilmand", "Helmand", x))) %>%
    mutate(across(Area:Subarea, function(x) gsub("Nimroz", "Nimruz", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Paktya", "Paktia", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Panjsher", "Panjshir", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub("Sari pul", "Sar_e_Pol", x))) %>%
    mutate(across(Area:Subarea, function(x) gsub(" Urban", "", x)))
  
  IPC_Afg <- IPC_Afg %>% 
    mutate(Area = ifelse(is.na(Area) & Subarea %in% afg_map$shapeName, Subarea, Area))
  IPC_Afg_provinces_long <- IPC_Afg %>%
    group_by(Area, Year, Month) %>% 
    summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
    mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
  
  IPC_Afg_provinces <- IPC_Afg_provinces_long %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
    pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio) %>% 
    relocate(Area, `Phase_3+ratio_2017_5`, `Phase_3+ratio_2017_9`)
  
  IPC_Afg_year_month <- IPC_Afg %>% select(Year, Month) %>% unique %>%
    mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric),
           year_month = paste(Year, Month, sep="_")) %>%
    arrange(Year, Month)
  
  IPC_Afg_year_month_list <- list(ym_t = IPC_Afg_year_month)
  prev_IPC_Afg_year_month <- IPC_Afg_year_month
  for (i in 1:12) {
    IPC_Afg_year_month_i <- prev_IPC_Afg_year_month %>% 
      mutate(Month = Month - 1,
             Year = ifelse(Month > 0, Year, Year - 1),
             Month = ifelse(Month > 0, Month, 12),
             year_month = paste(Year, Month, sep="_"))
    IPC_Afg_year_month_list[[paste0("ym_t_", i)]] <- IPC_Afg_year_month_i
    prev_IPC_Afg_year_month <- IPC_Afg_year_month_i
  }
  
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
    mutate(Region = gsub("Sar-e Pol", "Sar_e_Pol", Region)) %>% 
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
    mutate(admin1 = gsub("Urozgan", "Uruzgan", admin1)) %>% 
    mutate(admin1 = gsub("Sar-e Pol", "Sar_e_Pol", admin1))
  
  AFG_wheat_barley <- 5:7
  AFG_corn_rice <- 8:10
  
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
}

lagged_data_by_m <- function(lagged_months, min_t) {
  conflict_Afg_aggr <- conflict_Afg %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_Afg_aggr <- conflict_Afg_aggr[,-(1:2)] %>% 
    complete(year_month = lapply(IPC_Afg_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist,
             event_type = unique(conflict_Afg_aggr$event_type)) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  conflict_Afg_aggr[is.na(conflict_Afg_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_Afg_year_month)) {
    year_i <- IPC_Afg_year_month$Year[i]
    month_i <- IPC_Afg_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_Afg_aggr$year == prev_year & conflict_Afg_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_Afg_aggr$year == year_i & conflict_Afg_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_Afg_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type) %>% 
      summarize(n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_Afg_aggr <- lagged_conflict[[IPC_Afg_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_Afg_aggr <- bind_rows(conflict_Afg_aggr, tbl)
  }
  conflict_Afg_aggr <- conflict_Afg_aggr[-1,]
  
  conflict_Afg_n_events <- conflict_Afg_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(n_events=sum(n_events)) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
  conflict_Afg_n_events[is.na(conflict_Afg_n_events)] <- 0
  
  conflict_Afg_fatalities <- conflict_Afg_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
  conflict_Afg_fatalities[is.na(conflict_Afg_fatalities)] <- 0
  
  
  disaster_Afg_monthly_aggr <- disaster_Afg %>% 
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
  
  disaster_Afg_monthly_aggr <- disaster_Afg_monthly_aggr[,-(2:3)] %>% 
    complete(year_month = lapply(IPC_Afg_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  disaster_Afg_monthly_aggr[is.na(disaster_Afg_monthly_aggr)] <- 0
  
  lagged_disaster <- list()
  for (i in 1:nrow(IPC_Afg_year_month)) {
    year_i <- IPC_Afg_year_month$Year[i]
    month_i <- IPC_Afg_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(disaster_Afg_monthly_aggr$year == prev_year & disaster_Afg_monthly_aggr$month == prev_month)[1]
    months_index_i <- rev(which(disaster_Afg_monthly_aggr$year == year_i & disaster_Afg_monthly_aggr$month == month_i))[1]
    lagged_disaster[[year_month_i]] <- disaster_Afg_monthly_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area) %>% 
      summarize(across(n_disasters:n_droughts, function(x) sum(x))) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  disaster_Afg_monthly_aggr <- lagged_disaster[[IPC_Afg_year_month$year_month[1]]][1,]
  for (tbl in lagged_disaster) {
    disaster_Afg_monthly_aggr <- bind_rows(disaster_Afg_monthly_aggr, tbl)
  }
  disaster_Afg_monthly_aggr <- disaster_Afg_monthly_aggr[-1,]
  
  
  disaster_Afg_monthly_events <- disaster_Afg_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disasters=sum(n_disasters)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
  disaster_Afg_monthly_events[is.na(disaster_Afg_monthly_events)] <- 0
  
  disaster_Afg_monthly_flood <- disaster_Afg_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_floods=sum(n_floods)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_floods_", names_from = c(year, month), values_from = n_floods)
  disaster_Afg_monthly_flood[is.na(disaster_Afg_monthly_flood)] <- 0
  
  disaster_Afg_monthly_drought <- disaster_Afg_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_droughts=sum(n_droughts)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_droughts_", names_from = c(year, month), values_from = n_droughts)
  disaster_Afg_monthly_drought[is.na(disaster_Afg_monthly_drought)] <- 0
  
  disaster_Afg_monthly_affected <- disaster_Afg_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
  disaster_Afg_monthly_affected[is.na(disaster_Afg_monthly_affected)] <- 0
  
  disaster_Afg_monthly_deaths <- disaster_Afg_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_deaths=log(1+sum(deaths, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "log_deaths_", names_from = c(year, month), values_from = log_deaths)
  disaster_Afg_monthly_deaths[is.na(disaster_Afg_monthly_deaths)] <- 0
  
  
  # regression data
  IPC_ncol <- ncol(IPC_Afg_provinces)
  conflict_ncol <- ncol(conflict_Afg_n_events) # 85
  conflict_fatalities_ncol <- ncol(conflict_Afg_fatalities) # 85
  disaster_ncol <- ncol(disaster_Afg_monthly_events) # 15
  disaster_affected_ncol <- ncol(disaster_Afg_monthly_affected) # 15
  names(conflict_Afg_n_events)[-1] <- paste0("c_n_events_", names(conflict_Afg_n_events)[-1])
  names(conflict_Afg_fatalities)[-1] <- paste0("log_fatal_", names(conflict_Afg_fatalities)[-1])
  reg_data_i <- IPC_Afg_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
    left_join(conflict_Afg_n_events[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>%
    left_join(conflict_Afg_fatalities[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>% 
    left_join(disaster_Afg_monthly_events[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_Afg_monthly_flood[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_Afg_monthly_drought[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_Afg_monthly_affected[,c(1, disaster_ncol)], by="Area") %>%
    left_join(disaster_Afg_monthly_deaths[,c(1, disaster_ncol)], by="Area") %>% 
    mutate(year = rev(IPC_Afg_year_month$Year)[1])
  names(reg_data_i) <- gsub("2024_3", "t", names(reg_data_i))
  names(reg_data_i) <- gsub("2023_10", "t_1", names(reg_data_i))
  names(reg_data_i) <- gsub("2023_4", "t_2", names(reg_data_i))
  names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
  lagged_reg_data <- reg_data_i[,-1]
  lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_Afg_year_month$Year[14], IPC_Afg_year_month$Month[14], 1), format="%Y %m %d") -
                                             as.Date(paste(IPC_Afg_year_month$Year[13], IPC_Afg_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
  lagged_reg_data$wheat_barley <- time_since_harvest(IPC_Afg_year_month$Month[14], "wheat_barley", AFG_harvest)
  reg_data_names <- names(lagged_reg_data)
  for (i in 1:min_t) {
    IPC_col_index <- IPC_ncol - i
    conflict_col_index <- conflict_ncol - 6*i
    disaster_col_index <- disaster_ncol - i
    reg_data_i <- IPC_Afg_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
      left_join(conflict_Afg_n_events[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>%
      left_join(conflict_Afg_fatalities[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>% 
      left_join(disaster_Afg_monthly_events[,c(1, disaster_col_index)], by="Area") %>%
      left_join(disaster_Afg_monthly_flood[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_Afg_monthly_drought[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_Afg_monthly_affected[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_Afg_monthly_deaths[,c(1, disaster_col_index)], by="Area") %>% 
      mutate(year = rev(IPC_Afg_year_month$Year)[IPC_col_index])
    reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_Afg_year_month$Year[14-i], IPC_Afg_year_month$Month[14-i], 1), format="%Y %m %d") -
                                          as.Date(paste(IPC_Afg_year_month$Year[13-i], IPC_Afg_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
    
    reg_data_i$wheat_barley <- time_since_harvest(IPC_Afg_year_month$Month[14-i], "wheat_barley", AFG_harvest)
    reg_data_i <- as.matrix(reg_data_i[,-1])
    colnames(reg_data_i) <- reg_data_names
    lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
  }
  lagged_reg_data[is.na(lagged_reg_data)] <- 0
  
  result <- list()
  result$lagged_reg_data <- lagged_reg_data
  result$conflict_Afg_aggr <- conflict_Afg_aggr
  result$disaster_Afg_monthly_aggr <- disaster_Afg_monthly_aggr
  return(result)
} # function end

n_t <- nrow(IPC_Afg_year_month)
oldest_year <- IPC_Afg_year_month$Year[1]-1; oldest_month <- IPC_Afg_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_Afg_year_month$Year[n_t]; latest_month <- IPC_Afg_year_month$Month[n_t]

lagged_reg_data_list <- list()
for (i in 1:12) {
  m <- i
  lagged_reg_data_list[[paste0("months_", i)]] <- lagged_data_by_m(m, 9) # min_t = 9
}

lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2$lagged_reg_data %>% select(-n_floods_t, -n_droughts_t)) %>% summary()
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2$lagged_reg_data %>% select(-n_disasters_t)) %>% summary()

lagged_reg_data_list$months_1$disaster_Afg_monthly_aggr

lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_floods_t, data=lagged_reg_data_list$months_8$lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_floods_t+wheat_barley, data=lagged_reg_data_list$months_8$lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_floods_t*wheat_barley, data=lagged_reg_data_list$months_8$lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+floods_harvest,
   data=lagged_reg_data_list$months_8$lagged_reg_data %>% 
     mutate(floods_harvest = n_floods_t * wheat_barley)) %>% summary()

lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_floods_t*harvest,
   data=lagged_reg_data_list$months_8$lagged_reg_data %>% 
     mutate(harvest = wheat_barley < 4)) %>% summary()


lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_droughts_t, data=lagged_reg_data_list$months_8$lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_droughts_t+wheat_barley, data=lagged_reg_data_list$months_8$lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+n_droughts_t*wheat_barley, data=lagged_reg_data_list$months_8$lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+floods_harvest,
   data=lagged_reg_data_list$months_8$lagged_reg_data %>% 
     mutate(floods_harvest = n_droughts_t * wheat_barley)) %>% summary()

lagged_reg_data_list$months_8$lagged_reg_data %>% select(n_droughts_t, wheat_barley)
lagged_reg_data_list$months_8$lagged_reg_data$n_droughts_t * lagged_reg_data_list$months_8$lagged_reg_data$wheat_barley

disaster_term_reg_data <- list()
for (month_ in 1:12) {
  disaster_term_reg_data[[paste0("lag_month_", month_)]] <- left_join(IPC_Afg_provinces_long %>% mutate(Year = as.character(Year) %>% as.numeric,
                                                                                                        Month = as.character(Month) %>% as.numeric) %>% 
                                                                        select(Area, Year, Month, Phase_3above_ratio),
                                                                      lagged_reg_data_list[[month_]]$disaster_Afg_monthly_aggr %>% 
                                                                        rename(Year = year, Month=month) %>% 
                                                                        select(-year_month),
                                                                      by=c("Area", "Year", "Month"))
  # for (j in 1:nrow(IPC_Afg_year_month)) {
  #   year_j <- IPC_Afg_year_month$Year[j]
  #   month_j <- IPC_Afg_year_month$Month[j]
  #   IPC_Afg_provinces_long_j <- IPC_Afg_provinces_long %>% 
  #     filter(Year == year_j & Month == month_j) %>% 
  #     select(Area, Phase_3above_ratio)
  #   disaster_j <- lagged_reg_data_list$months_1$disaster_Afg_monthly_aggr %>% 
  #     filter(year == year_j & month == month_j) %>% 
  #     select(Area:n_droughts)
  #   disaster_term_reg_data_mj <- left_join(IPC_Afg_provinces_long_j, disaster_j, by="Area")
  #   disaster_term_reg_data[[paste0("lag_month_", month_)]][[paste0("ym_", IPC_Afg_year_month$year_month[j])]] <- disaster_term_reg_data_mj
  # }
}
lapply(disaster_term_reg_data, function(x) lm(Phase_3above_ratio~n_floods, x[,-(1:2)]) %>% summary)

lapply(disaster_term_reg_data, function(x) lm(Phase_3above_ratio~n_droughts, x[,-(1:2)]) %>% summary)
disaster_term_reg_data$lag_month_1

lagged_reg_data_list$months_1$lagged_reg_data
no_crop_areas <- c("Ghor", "Nimruz", "Nuristan", "Panjshir")
no_crop_index <- map(which(IPC_Afg_provinces$Area %in% no_crop_areas), function(x) x + 34*(0:9)) %>% unlist %>% sort

lm_month_aggr_floods <- lapply(lagged_reg_data_list,
                               function(x) lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+month_diff+n_floods_t,
                                              x$lagged_reg_data %>%
                                                select(`Phase_3+ratio_t`, `Phase_3+ratio_t_1`, month_diff, n_floods_t)) %>% summary)
lm_month_aggr_floods_harvesting_areas <- lapply(lagged_reg_data_list,
                                                function(x) lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+month_diff+n_floods_t,
                                                               x$lagged_reg_data[-no_crop_index,] %>% 
                                                                 select(`Phase_3+ratio_t`, `Phase_3+ratio_t_1`, month_diff, n_floods_t)) %>% summary)

lm_month_aggr_floods$months_1$adj.r.squared
# sapply(lm_month_aggr_floods, function(x) x$coefficients[,1]) %>% 
#   write.csv("Food Security/Regression results/disaster long short term/floods reg coefs.csv")
lm_month_aggr_floods_sig <- sapply(lm_month_aggr_floods, function(x) ifelse(x$coefficients[,4] > 0.05, 0, 1))
lm_month_aggr_floods_sig <- rbind(lm_month_aggr_floods_sig, sapply(lm_month_aggr_floods, function(x) x$adj.r.squared))
row.names(lm_month_aggr_floods_sig)[5] <- "Adj_R_squared"
# write.csv(lm_month_aggr_floods_sig, "Food Security/Regression results/disaster long short term/floods reg sig.csv")

# sapply(lm_month_aggr_floods_harvesting_areas, function(x) x$coefficients[,1]) %>%
#   write.csv("Food Security/Regression results/disaster long short term/floods reg coefs harvesting areas.csv")
lm_month_aggr_floods_harvesting_areas_sig <- sapply(lm_month_aggr_floods_harvesting_areas, function(x) ifelse(x$coefficients[,4] > 0.05, 0, 1))
lm_month_aggr_floods_harvesting_areas_sig <- rbind(lm_month_aggr_floods_harvesting_areas_sig, 
                                                   sapply(lm_month_aggr_floods_harvesting_areas, function(x) x$adj.r.squared))
row.names(lm_month_aggr_floods_harvesting_areas_sig)[5] <- "Adj_R_squared"
# write.csv(lm_month_aggr_floods_harvesting_areas_sig, "Food Security/Regression results/disaster long short term/floods reg sig harvesting areas.csv")

lm_month_aggr_droughts <- lapply(lagged_reg_data_list,
                                 function(x) lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+month_diff+n_droughts_t,
                                            x$lagged_reg_data %>% 
                                              select(`Phase_3+ratio_t`, `Phase_3+ratio_t_1`, month_diff, n_droughts_t)) %>% summary)
lm_month_aggr_droughts_harvesting_areas <- lapply(lagged_reg_data_list,
                                                  function(x) lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+month_diff+n_droughts_t,
                                            x$lagged_reg_data[-no_crop_index,] %>% 
                                              select(`Phase_3+ratio_t`, `Phase_3+ratio_t_1`, month_diff, n_droughts_t)) %>% summary)

# sapply(lm_month_aggr_droughts[-1], function(x) x$coefficients[,1]) %>% 
#   write.csv("Food Security/Regression results/disaster long short term/droughts reg coefs.csv")
lm_month_aggr_droughts_sig <- sapply(lm_month_aggr_droughts[-1], function(x) ifelse(x$coefficients[,4] > 0.05, 0, 1))
lm_month_aggr_droughts_sig <- rbind(lm_month_aggr_droughts_sig, sapply(lm_month_aggr_droughts[-1], function(x) x$adj.r.squared))
row.names(lm_month_aggr_droughts_sig)[5] <- "Adj_R_squared"
# write.csv(lm_month_aggr_droughts_sig, "Food Security/Regression results/disaster long short term/droughts reg sig.csv")

# sapply(lm_month_aggr_droughts_harvesting_areas[-1], function(x) x$coefficients[,1]) %>%
#   write.csv("Food Security/Regression results/disaster long short term/droughts reg coefs harvesting areas.csv")
lm_month_aggr_droughts_harvesting_areas_sig <- sapply(lm_month_aggr_droughts_harvesting_areas[-1], function(x) ifelse(x$coefficients[,4] > 0.05, 0, 1))
lm_month_aggr_droughts_harvesting_areas_sig <- rbind(lm_month_aggr_droughts_harvesting_areas_sig, 
                                                     sapply(lm_month_aggr_droughts_harvesting_areas[-1], function(x) x$adj.r.squared))
row.names(lm_month_aggr_droughts_harvesting_areas_sig)[5] <- "Adj_R_squared"
# write.csv(lm_month_aggr_droughts_harvesting_areas_sig, "Food Security/Regression results/disaster long short term/droughts reg sig harvesting areas.csv")

lapply(lagged_reg_data_list, function(x) lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`+month_diff+n_floods_t+n_droughts_t,
                                            x$lagged_reg_data %>% select(`Phase_3+ratio_t`, `Phase_3+ratio_t_1`, month_diff, n_floods_t, n_droughts_t)) %>% summary)



lagged_reg_data_lm <- lapply(lagged_reg_data_list,
       function(x) lm(`Phase_3+ratio_t`~., data=x %>% select(-n_disasters_t)) %>% summary())

lagged_reg_data_lm

  ## month1 and longer for drought and flood.
long_month_drought_reg_data <- lagged_reg_data_list$months_2
drought_month <- 6
long_month_drought_reg_data$n_droughts_t <- lagged_reg_data_list[[drought_month]]$n_droughts_t
long_month_drought_reg_data$n_floods_t <- lagged_reg_data_list[[drought_month]]$n_floods_t
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2 %>% select(-n_disasters_t, -affected_t, -log_deaths_t)) %>% summary()
lm(`Phase_3+ratio_t`~., data=long_month_drought_reg_data %>% select(-n_disasters_t, -affected_t, -log_deaths_t)) %>% summary()

lagged_reg_data_lm_significance <- tibble(var_name = rownames(lagged_reg_data_lm$months_12$coefficients))
for (i in 1:12) {
  lm_summary <- lagged_reg_data_lm[[i]]$coefficients %>%
    as.data.frame %>% 
    select(-`t value`) %>% 
    rename(p_value = `Pr(>|t|)`) %>% 
    mutate(sig. = ifelse(p_value > 0.1, "",
                         ifelse(p_value > 0.05, ".",
                                ifelse(p_value > 0.01, "*",
                                       ifelse(p_value > 0.001, "**", "***")))))
  lm_summary$var_name <- row.names(lm_summary)
  write.csv(lm_summary, paste0("Food Security/Regression results/regression with ", i, "-month lagged data.csv"), row.names = F)
  lagged_reg_data_lm_significance[[paste0("sig.", i)]] <- left_join(lagged_reg_data_lm_significance,
                                                                    lm_summary %>% select(var_name, sig.) %>% 
                                                                      mutate(sig.=ifelse(lm_summary$sig. == "", 0, 1)),
                                                                    by="var_name") %>% pull(sig.)
}
lagged_reg_data_lm_significance

lagged_reg_data_coefs <- tibble(var_name = rownames(lagged_reg_data_lm$months_12$coefficients))
lagged_reg_data_R2 <- c()
for (i in 1:12) {
  lm_summary <- lagged_reg_data_lm[[i]]$coefficients %>%
    as.data.frame %>% 
    select(Estimate)
    lm_summary$var_name <- row.names(lm_summary)
  lagged_reg_data_coefs <- left_join(lagged_reg_data_coefs, lm_summary, by="var_name")
  lagged_reg_data_R2 <- c(lagged_reg_data_R2, lagged_reg_data_lm[[i]]$r.squared)
}
names(lagged_reg_data_coefs)[-1] <- paste0("coefs_", 1:12)
lagged_reg_data_coefs <- bind_rows(lagged_reg_data_coefs, lagged_reg_data_coefs[1,])
lagged_reg_data_coefs$var_name[23] <- "R_squared"
lagged_reg_data_coefs_mat <- lagged_reg_data_coefs[,-1] %>% as.matrix
lagged_reg_data_coefs_mat[23,] <- lagged_reg_data_R2
lagged_reg_data_coefs[,-1] <- lagged_reg_data_coefs_mat
lagged_reg_data_coefs

lm(`Phase_3+ratio_t`~., data=lagged_reg_data %>% select(-n_floods_t, -n_droughts_t)) %>% summary()
lm(`Phase_3+ratio_t`~., data=lagged_reg_data %>% select(-n_disasters_t)) %>% summary()
lm(`Phase_3+ratio_t`~`Phase_3+ratio_t_1`, data=lagged_reg_data) %>% summary()

lagged_reg_data %>% apply(2, function(x) table(x) %>% length) %>% sort
lagged_reg_data %>% str


# Fragile State Index (FSI) ts plot
FSI_Afg %>%
  select(-Country, -Rank, -Total) %>% 
  pivot_longer(-Year, names_to = "Indicator", values_to = "FSI") %>% 
  ggplot() +
  geom_line(aes(x=Year, y=FSI, group=Indicator, color=Indicator)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="Fragile State Index")

# precipitation
  # ts plot
afg_precipitation %>% 
  ggplot() +
  geom_line(aes(x=date, y=rfh, group=Area, color=Area)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="rfh: 10 day rainfall [mm]") -> rfh_ts_plot

afg_precipitation %>% 
  ggplot() +
  geom_line(aes(x=date, y=rfh_avg, group=Area, color=Area)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="rfh_avg: rainfall long term average [mm]") -> rfh_avg_ts_plot

afg_precipitation %>% 
  ggplot() +
  geom_line(aes(x=date, y=r1h, group=Area, color=Area)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="r1h: rainfall 1-month rolling aggregation [mm]") -> r1h_ts_plot

afg_precipitation %>% 
  ggplot() +
  geom_line(aes(x=date, y=r1h_avg, group=Area, color=Area)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="r1h_avg: r1h long term average  [mm]") -> r1h_avg_ts_plot

afg_precipitation %>% 
  ggplot() +
  geom_line(aes(x=date, y=r3h, group=Area, color=Area)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="r3h: rainfall 3-month rolling aggregation [mm]") -> r3h_ts_plot

afg_precipitation %>% 
  ggplot() +
  geom_line(aes(x=date, y=r3h_avg, group=Area, color=Area)) + 
  scale_color_viridis_d(option="H") +
  labs(x="Year", y="r3h_avg: r3h long term average  [mm]") -> r3h_avg_ts_plot

ggsave("Food Security/Fig/precipitation/rfh_ts_plot.png", rfh_ts_plot, width=17, height=12, unit="cm")
ggsave("Food Security/Fig/precipitation/rfh_avg_ts_plot.png", rfh_avg_ts_plot, width=17, height=12, unit="cm")
ggsave("Food Security/Fig/precipitation/r1h_ts_plot.png", r1h_ts_plot, width=17, height=12, unit="cm")
ggsave("Food Security/Fig/precipitation/r1h_avg_ts_plot.png", r1h_avg_ts_plot, width=17, height=12, unit="cm")
ggsave("Food Security/Fig/precipitation/r3h_ts_plot.png", r3h_ts_plot, width=17, height=12, unit="cm")
ggsave("Food Security/Fig/precipitation/r3h_avg_ts_plot.png", r3h_avg_ts_plot, width=17, height=12, unit="cm")

  # regerssion
min_t_ <- 9
precipitation_reg_data <- left_join(IPC_Afg_provinces_long %>%
            mutate(year_month = paste(Year, Month, sep="_")) %>% 
            select(Area, year_month, Phase_3above_ratio),
          afg_precipitation %>% select(Area, year_month, rfh:r3h_avg)) %>% 
  select(Phase_3above_ratio, rfh:r3h_avg)
precipitation_reg_data <- precipitation_reg_data[,-(1:2)]
lm(Phase_3above_ratio~., data=precipitation_reg_data) %>% summary()
lm(Phase_3above_ratio~., data=precipitation_reg_data %>% select(Phase_3above_ratio, rfh)) %>% summary()
lm(Phase_3above_ratio~., data=precipitation_reg_data %>% select(Phase_3above_ratio, rfh_avg)) %>% summary()
lm(Phase_3above_ratio~., data=precipitation_reg_data %>% select(Phase_3above_ratio, r1h)) %>% summary()
lm(Phase_3above_ratio~., data=precipitation_reg_data %>% select(Phase_3above_ratio, r1h_avg)) %>% summary()
lm(Phase_3above_ratio~., data=precipitation_reg_data %>% select(Phase_3above_ratio, r3h)) %>% summary()
lm(Phase_3above_ratio~., data=precipitation_reg_data %>% select(Phase_3above_ratio, r3h_avg)) %>% summary()

# NOT working below. Matching error?
# afg_precipitation_lagged <- afg_precipitation[1,6:11]
# for (i in n_t:(n_t-min_t_)) {
#   afg_precipitation_i <- afg_precipitation %>%
#     filter(year_month == IPC_Afg_year_month$year_month[i]) %>% 
#     arrange(Area)
#   afg_precipitation_lagged <- bind_rows(afg_precipitation_lagged, afg_precipitation_i[, 6:11])
# }
# afg_precipitation_lagged <- afg_precipitation_lagged[-1,]
# 
# prec_reg_data <- bind_cols(lagged_reg_data%>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`), afg_precipitation_lagged)
# lm(`Phase_3+ratio_t`~., data=prec_reg_data) %>% summary()
# lm(`Phase_3+ratio_t`~., data=prec_reg_data %>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, rfh)) %>% summary()
# lm(`Phase_3+ratio_t`~., data=prec_reg_data %>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, rfh_avg)) %>% summary()
# lm(`Phase_3+ratio_t`~., data=prec_reg_data %>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, r1h)) %>% summary()
# lm(`Phase_3+ratio_t`~., data=prec_reg_data %>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, r1h_avg)) %>% summary()
# lm(`Phase_3+ratio_t`~., data=prec_reg_data %>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, r3h)) %>% summary()
# lm(`Phase_3+ratio_t`~., data=prec_reg_data %>% select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, r3h_avg)) %>% summary()

precipitation_corr <- cor(afg_precipitation[, 6:11]) %>% melt
precipitation_corr %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                       values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, 1)),
                       limits=c(-1, 1)) +
  labs(title="Correlations of AFG data") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




