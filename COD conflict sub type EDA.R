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
    mutate(Area = ifelse(is.na(Area) & Subarea %in% COD_map$shapeName, Subarea, Area),
           Area = stri_trans_general(Area, "Latin-ASCII"),
           Area = gsub("-", " ", Area),
           Area = gsub("Mai Ndombe", "Mai-Ndombe", Area))
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
  no_crop_areas <- c("Mongala", "Tshuapa", "Kasai Oriental", "Kinshasa", "Ituri", "Sud Kivu")
}

lagged_data_by_m <- function(lagged_months, min_t) {
  conflict_COD_aggr <- conflict_COD %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_COD_aggr <- conflict_COD_aggr[,-(1:2)] %>% 
    complete(year_month = lapply(IPC_COD_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist,
             event_type = unique(conflict_COD_aggr$event_type)) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  conflict_COD_aggr[is.na(conflict_COD_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_COD_year_month)) {
    year_i <- IPC_COD_year_month$Year[i]
    month_i <- IPC_COD_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_COD_aggr$year == prev_year & conflict_COD_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_COD_aggr$year == year_i & conflict_COD_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_COD_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type) %>% 
      summarize(n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_COD_aggr <- lagged_conflict[[IPC_COD_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_COD_aggr <- bind_rows(conflict_COD_aggr, tbl)
  }
  conflict_COD_aggr <- conflict_COD_aggr[-1,]
  
  conflict_COD_n_events <- conflict_COD_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(n_events=sum(n_events)) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
  conflict_COD_n_events[is.na(conflict_COD_n_events)] <- 0
  
  conflict_COD_fatalities <- conflict_COD_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
  conflict_COD_fatalities[is.na(conflict_COD_fatalities)] <- 0
  
  conflict_sub_COD_aggr <- conflict_COD %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, sub_event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_sub_COD_aggr <- conflict_sub_COD_aggr[,-(1:2)] %>% 
    complete(year_month,
             sub_event_type) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    left_join(conflict_types, by="sub_event_type") %>% 
    arrange(year, month)
  
  
  conflict_sub_COD_aggr[is.na(conflict_sub_COD_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_COD_year_month)) {
    year_i <- IPC_COD_year_month$Year[i]
    month_i <- IPC_COD_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_sub_COD_aggr$year == prev_year & conflict_sub_COD_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_sub_COD_aggr$year == year_i & conflict_sub_COD_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_sub_COD_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type, sub_event_type) %>% 
      summarize(event_type = event_type[1],
                n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_sub_COD_aggr <- lagged_conflict[[IPC_COD_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_sub_COD_aggr <- bind_rows(conflict_sub_COD_aggr, tbl)
  }
  conflict_sub_COD_aggr <- conflict_sub_COD_aggr[-1,]
  
  disaster_COD_monthly_aggr <- disaster_COD %>% 
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
  
  disaster_COD_monthly_aggr <- disaster_COD_monthly_aggr[,-(2:3)] %>% 
    complete(year_month = lapply(IPC_COD_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  disaster_COD_monthly_aggr[is.na(disaster_COD_monthly_aggr)] <- 0
  
  lagged_disaster <- list()
  for (i in 1:nrow(IPC_COD_year_month)) {
    year_i <- IPC_COD_year_month$Year[i]
    month_i <- IPC_COD_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(disaster_COD_monthly_aggr$year == prev_year & disaster_COD_monthly_aggr$month == prev_month)[1]
    months_index_i <- rev(which(disaster_COD_monthly_aggr$year == year_i & disaster_COD_monthly_aggr$month == month_i))[1]
    lagged_disaster[[year_month_i]] <- disaster_COD_monthly_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area) %>% 
      summarize(across(n_disasters:n_droughts, function(x) sum(x))) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  disaster_COD_monthly_aggr <- lagged_disaster[[IPC_COD_year_month$year_month[1]]][1,]
  for (tbl in lagged_disaster) {
    disaster_COD_monthly_aggr <- bind_rows(disaster_COD_monthly_aggr, tbl)
  }
  disaster_COD_monthly_aggr <- disaster_COD_monthly_aggr[-1,]
  
  
  disaster_COD_monthly_events <- disaster_COD_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disasters=sum(n_disasters)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
  disaster_COD_monthly_events[is.na(disaster_COD_monthly_events)] <- 0
  
  disaster_COD_monthly_flood <- disaster_COD_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_floods=sum(n_floods)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_floods_", names_from = c(year, month), values_from = n_floods)
  disaster_COD_monthly_flood[is.na(disaster_COD_monthly_flood)] <- 0
  
  disaster_COD_monthly_drought <- disaster_COD_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_droughts=sum(n_droughts)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_droughts_", names_from = c(year, month), values_from = n_droughts)
  disaster_COD_monthly_drought[is.na(disaster_COD_monthly_drought)] <- 0
  
  disaster_COD_monthly_affected <- disaster_COD_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
  disaster_COD_monthly_affected[is.na(disaster_COD_monthly_affected)] <- 0
  
  disaster_COD_monthly_deaths <- disaster_COD_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_deaths=log(1+sum(deaths, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "log_deaths_", names_from = c(year, month), values_from = log_deaths)
  disaster_COD_monthly_deaths[is.na(disaster_COD_monthly_deaths)] <- 0
  
  
  # regression data
  IPC_ncol <- ncol(IPC_COD_provinces)
  conflict_ncol <- ncol(conflict_COD_n_events) # 49
  conflict_fatalities_ncol <- ncol(conflict_COD_fatalities) # 49
  disaster_ncol <- ncol(disaster_COD_monthly_events) # 9
  disaster_affected_ncol <- ncol(disaster_COD_monthly_affected) # 9
  names(conflict_COD_n_events)[-1] <- paste0("c_n_events_", names(conflict_COD_n_events)[-1])
  names(conflict_COD_fatalities)[-1] <- paste0("log_fatal_", names(conflict_COD_fatalities)[-1])
  reg_data_i <- IPC_COD_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
    left_join(conflict_COD_n_events[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>%
    left_join(conflict_COD_fatalities[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>% 
    left_join(disaster_COD_monthly_events[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_COD_monthly_flood[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_COD_monthly_drought[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_COD_monthly_affected[,c(1, disaster_ncol)], by="Area") %>%
    left_join(disaster_COD_monthly_deaths[,c(1, disaster_ncol)], by="Area") %>% 
    mutate(year = rev(IPC_COD_year_month$Year)[1])
  names(reg_data_i) <- gsub(IPC_COD_year_month$year_month[IPC_ncol-1], "t", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_COD_year_month$year_month[IPC_ncol-2], "t_1", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_COD_year_month$year_month[IPC_ncol-3], "t_2", names(reg_data_i))
  names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
  lagged_reg_data <- reg_data_i[,-1]
  lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_COD_year_month$Year[14], IPC_COD_year_month$Month[14], 1), format="%Y %m %d") -
                                             as.Date(paste(IPC_COD_year_month$Year[13], IPC_COD_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
  # lagged_reg_data$wheat_barley <- time_since_harvest(IPC_COD_year_month$Month[14], "wheat_barley", COD_harvest)
  reg_data_names <- names(lagged_reg_data)
  for (i in 1:min_t) {
    IPC_col_index <- IPC_ncol - i
    conflict_col_index <- conflict_ncol - 6*i
    disaster_col_index <- disaster_ncol - i
    reg_data_i <- IPC_COD_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
      left_join(conflict_COD_n_events[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>%
      left_join(conflict_COD_fatalities[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>% 
      left_join(disaster_COD_monthly_events[,c(1, disaster_col_index)], by="Area") %>%
      left_join(disaster_COD_monthly_flood[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_COD_monthly_drought[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_COD_monthly_affected[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_COD_monthly_deaths[,c(1, disaster_col_index)], by="Area") %>% 
      mutate(year = rev(IPC_COD_year_month$Year)[IPC_col_index])
    reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_COD_year_month$Year[14-i], IPC_COD_year_month$Month[14-i], 1), format="%Y %m %d") -
                                          as.Date(paste(IPC_COD_year_month$Year[13-i], IPC_COD_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
    
    # reg_data_i$wheat_barley <- time_since_harvest(IPC_COD_year_month$Month[14-i], "wheat_barley", COD_harvest)
    reg_data_i <- as.matrix(reg_data_i[,-1])
    colnames(reg_data_i) <- reg_data_names
    lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
  }
  lagged_reg_data[is.na(lagged_reg_data)] <- 0
  
  result <- list()
  result$lagged_reg_data <- lagged_reg_data
  result$conflict_COD_aggr <- conflict_COD_aggr
  result$conflict_sub_COD_aggr <- conflict_sub_COD_aggr
  result$disaster_COD_monthly_aggr <- disaster_COD_monthly_aggr
  return(result)
} # function end

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

COD_harvest
corn_seasons <- tibble(month=1:12,
                       season=c("none", rep("plant", 2), rep("grow", 2), rep("harvest", 2), rep("none", 2), rep("plant", 3), "none"))
rice_seasons <- tibble(month=1:12,
                       season=c(rep("grow", 4), rep("harvest", 3), rep("none", 2), rep("plant", 3)))

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