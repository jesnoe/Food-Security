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

lagged_data_by_m <- function(lagged_months, min_t, disaster1="Flood", disaster2="Drought") {
  conflict_NAT_aggr <- conflict_NAT %>% 
    filter(year > oldest_year - 1) %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_NAT_aggr <- conflict_NAT_aggr[,-(1:2)] %>% 
    complete(year_month = lapply(IPC_NAT_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist,
             event_type = unique(conflict_NAT_aggr$event_type)) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  conflict_NAT_aggr[is.na(conflict_NAT_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_NAT_year_month)) {
    year_i <- IPC_NAT_year_month$Year[i]
    month_i <- IPC_NAT_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_NAT_aggr$year == prev_year & conflict_NAT_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_NAT_aggr$year == year_i & conflict_NAT_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_NAT_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type) %>% 
      summarize(n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_NAT_aggr <- lagged_conflict[[IPC_NAT_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_NAT_aggr <- bind_rows(conflict_NAT_aggr, tbl)
  }
  conflict_NAT_aggr <- conflict_NAT_aggr[-1,]
  
  result <- list()
  result$conflict_NAT_aggr <- conflict_NAT_aggr
  
  conflict_NAT_aggr$event_type <- ifelse(conflict_NAT_aggr$event_type %in% c("Battles", "Explosions/Remote violence"),
                                         "Battles_explosions",
                                         ifelse(conflict_NAT_aggr$event_type %in% c("Protests", "Riots"), "Protests_Riots",
                                                "etc.")
                                         )
  conflict_NAT_n_events <- conflict_NAT_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(n_events=sum(n_events)) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
  conflict_NAT_n_events[is.na(conflict_NAT_n_events)] <- 0
  
  conflict_NAT_fatalities <- conflict_NAT_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
  conflict_NAT_fatalities[is.na(conflict_NAT_fatalities)] <- 0
  
  
  conflict_sub_NAT_aggr <- conflict_NAT %>% 
    filter(year > oldest_year - 1) %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, sub_event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_sub_NAT_aggr <- conflict_sub_NAT_aggr[,-(1:2)] %>% 
    complete(year_month,
             sub_event_type) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    left_join(conflict_types, by="sub_event_type") %>% 
    arrange(year, month)
  
  conflict_sub_NAT_aggr[is.na(conflict_sub_NAT_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_NAT_year_month)) {
    year_i <- IPC_NAT_year_month$Year[i]
    month_i <- IPC_NAT_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_sub_NAT_aggr$year == prev_year & conflict_sub_NAT_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_sub_NAT_aggr$year == year_i & conflict_sub_NAT_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_sub_NAT_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type, sub_event_type) %>% 
      summarize(event_type = event_type[1],
                n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_sub_NAT_aggr <- lagged_conflict[[IPC_NAT_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_sub_NAT_aggr <- bind_rows(conflict_sub_NAT_aggr, tbl)
  }
  conflict_sub_NAT_aggr <- conflict_sub_NAT_aggr[-1,]
  
  result$conflict_sub_NAT_aggr <- conflict_sub_NAT_aggr
  
  disaster_NAT_monthly_aggr <- disaster_NAT %>% 
    filter(year > oldest_year - 1) %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>%  
    group_by(Region, year, month) %>% 
    summarise(n_disasters=n(),
              affected=sum(`Total Affected`, na.rm=T),
              deaths=sum(`Total Deaths`, na.rm=T),
              n_disaster1=sum(`Disaster Type` == disaster1),
              n_disaster2=sum(`Disaster Type` == disaster2)) %>% 
    rename(Area=Region) %>% 
    mutate(year_month = paste(year, month, sep="_")) %>% 
    # mutate(year_month=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>% 
    arrange(year, month)
  
  disaster_NAT_monthly_aggr <- disaster_NAT_monthly_aggr[,-(2:3)] %>% 
    complete(year_month = lapply(IPC_NAT_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  disaster_NAT_monthly_aggr[is.na(disaster_NAT_monthly_aggr)] <- 0
  
  lagged_disaster <- list()
  for (i in 1:nrow(IPC_NAT_year_month)) {
    year_i <- IPC_NAT_year_month$Year[i]
    month_i <- IPC_NAT_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(disaster_NAT_monthly_aggr$year == prev_year & disaster_NAT_monthly_aggr$month == prev_month)[1]
    months_index_i <- rev(which(disaster_NAT_monthly_aggr$year == year_i & disaster_NAT_monthly_aggr$month == month_i))[1]
    lagged_disaster[[year_month_i]] <- disaster_NAT_monthly_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area) %>% 
      summarize(across(n_disasters:n_disaster2, function(x) sum(x))) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  disaster_NAT_monthly_aggr <- lagged_disaster[[IPC_NAT_year_month$year_month[1]]][1,]
  for (tbl in lagged_disaster) {
    disaster_NAT_monthly_aggr <- bind_rows(disaster_NAT_monthly_aggr, tbl)
  }
  disaster_NAT_monthly_aggr <- disaster_NAT_monthly_aggr[-1,]
  
  result$disaster_NAT_monthly_aggr <- disaster_NAT_monthly_aggr
  
  disaster_NAT_monthly_events <- disaster_NAT_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disasters=sum(n_disasters)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
  disaster_NAT_monthly_events[is.na(disaster_NAT_monthly_events)] <- 0
  
  disaster_NAT_monthly_disaster1 <- disaster_NAT_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disaster1=sum(n_disaster1)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disaster1_", names_from = c(year, month), values_from = n_disaster1)
  disaster_NAT_monthly_disaster1[is.na(disaster_NAT_monthly_disaster1)] <- 0
  
  disaster_NAT_monthly_disaster2 <- disaster_NAT_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disaster2=sum(n_disaster2)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disaster2_", names_from = c(year, month), values_from = n_disaster2)
  disaster_NAT_monthly_disaster2[is.na(disaster_NAT_monthly_disaster2)] <- 0
  
  disaster_NAT_monthly_affected <- disaster_NAT_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
  disaster_NAT_monthly_affected[is.na(disaster_NAT_monthly_affected)] <- 0
  
  disaster_NAT_monthly_deaths <- disaster_NAT_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_deaths=log(1+sum(deaths, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "log_deaths_", names_from = c(year, month), values_from = log_deaths)
  disaster_NAT_monthly_deaths[is.na(disaster_NAT_monthly_deaths)] <- 0
  
  
  # regression data
  IPC_ncol <- ncol(IPC_NAT_provinces)
  IPC_n_months <- nrow(IPC_NAT_year_month)
  conflict_ncol <- ncol(conflict_NAT_n_events)
  conflict_fatalities_ncol <- ncol(conflict_NAT_fatalities)
  n_conflict_types <- unique(conflict_NAT_aggr$event_type) %>% length
  disaster_ncol <- ncol(disaster_NAT_monthly_events) # 9
  disaster_affected_ncol <- ncol(disaster_NAT_monthly_affected) # 9
  names(conflict_NAT_n_events)[-1] <- paste0("c_n_events_", names(conflict_NAT_n_events)[-1])
  names(conflict_NAT_fatalities)[-1] <- paste0("log_fatal_", names(conflict_NAT_fatalities)[-1])
  reg_data_i <- IPC_NAT_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
    left_join(conflict_NAT_n_events[,c(1, conflict_ncol:(conflict_ncol-n_conflict_types+1))], by="Area") %>%
    left_join(conflict_NAT_fatalities[,c(1, conflict_ncol:(conflict_ncol-n_conflict_types+1))], by="Area") %>% 
    left_join(disaster_NAT_monthly_events[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_NAT_monthly_disaster1[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_NAT_monthly_disaster2[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_NAT_monthly_affected[,c(1, disaster_ncol)], by="Area") %>%
    left_join(disaster_NAT_monthly_deaths[,c(1, disaster_ncol)], by="Area") %>% 
    mutate(year = rev(IPC_NAT_year_month$Year)[1],
           month = rev(IPC_NAT_year_month$Month)[1])
  names(reg_data_i) <- gsub(IPC_NAT_year_month$year_month[IPC_ncol-1], "t", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_NAT_year_month$year_month[IPC_ncol-2], "t_1", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_NAT_year_month$year_month[IPC_ncol-3], "t_2", names(reg_data_i))
  names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
  lagged_reg_data <- reg_data_i[,-1]
  lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_NAT_year_month$Year[IPC_n_months], IPC_NAT_year_month$Month[IPC_n_months], 1), format="%Y %m %d") -
                                             as.Date(paste(IPC_NAT_year_month$Year[IPC_n_months-1], IPC_NAT_year_month$Month[IPC_n_months-1], 1), format="%Y %m %d")) %/% 30
  n_areas <- nrow(reg_data_i)
  # lagged_reg_data$wheat_barley <- time_since_harvest(IPC_NAT_year_month$Month[14], "wheat_barley", NAT_harvest)
  reg_data_names <- names(lagged_reg_data)
  for (i in 1:min_t) {
    IPC_col_index <- IPC_ncol - i
    conflict_col_index <- conflict_ncol - n_conflict_types*i
    disaster_col_index <- disaster_ncol - i
    reg_data_i <- IPC_NAT_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
      left_join(conflict_NAT_n_events[,c(1, conflict_col_index:(conflict_col_index-n_conflict_types+1))], by="Area") %>%
      left_join(conflict_NAT_fatalities[,c(1, conflict_col_index:(conflict_col_index-n_conflict_types+1))], by="Area") %>% 
      left_join(disaster_NAT_monthly_events[,c(1, disaster_col_index)], by="Area") %>%
      left_join(disaster_NAT_monthly_disaster1[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_NAT_monthly_disaster2[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_NAT_monthly_affected[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_NAT_monthly_deaths[,c(1, disaster_col_index)], by="Area") %>% 
      mutate(year = rev(IPC_NAT_year_month$Year)[IPC_col_index],
             month = rev(IPC_NAT_year_month$Month)[IPC_col_index])
    reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_NAT_year_month$Year[IPC_n_months-i], IPC_NAT_year_month$Month[IPC_n_months-i], 1), format="%Y %m %d") -
                                          as.Date(paste(IPC_NAT_year_month$Year[IPC_n_months-1-i], IPC_NAT_year_month$Month[IPC_n_months-1-i], 1), format="%Y %m %d")) %/% 30
    
    # reg_data_i$wheat_barley <- time_since_harvest(IPC_NAT_year_month$Month[14-i], "wheat_barley", NAT_harvest)
    reg_data_i <- as.matrix(reg_data_i[,-1])
    colnames(reg_data_i) <- reg_data_names
    lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
  }
  lagged_reg_data[is.na(lagged_reg_data)] <- 0
  lagged_reg_data$IPC_diff <- lagged_reg_data$`Phase_3+ratio_t` - lagged_reg_data$`Phase_3+ratio_t_1`
  
  n_rows <- nrow(lagged_reg_data)
  reg_data_first_month <- matrix(NA, n_areas, 6) %>% as_tibble
  names(reg_data_first_month) <- reg_data_names[4:9]
  conflict_t_1 <- bind_rows(reg_data_first_month, lagged_reg_data[(n_areas+1):n_rows, 4:9] - lagged_reg_data[1:(n_rows - n_areas), 4:9] %>% as.matrix)
  names(conflict_t_1) <- paste0(reg_data_names[4:9], "_diff")
  lagged_reg_data <- bind_cols(lagged_reg_data, conflict_t_1)
  
  
  result$lagged_reg_data <- lagged_reg_data
  return(result)
} # function end

conflict_map <- function(month_aggr, filter_=F, growing_region=F) {
  IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_NAT_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  if (is.character(filter_)) IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  IPC_NAT_phase3_long_month %>% ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=n_events, group=event_type, color=event_type)) +
    ggtitle(filter_)
}


conflict_diff_map <- function(month_aggr, filter_=F, growing_region=F) {
  IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_NAT_aggr %>% select(-year_month),
              by=c("Area", "year", "month")) 
  
  if (is.character(filter_)) IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  IPC_NAT_phase3_long_month %>% ggplot() +
    geom_point(aes(x=IPC_diff, y=n_events, group=event_type, color=event_type))
}

conflict_sub_n_events <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=n_events, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_sub_fatalities <- function(month_aggr,conflict_type, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_n_events_harvest <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=season, color=season)) +
      ggtitle(conflict_type)
  }else{
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=season, color=season)) +
      ggtitle(conflict_type) 
  }
}

conflict_n_events_year <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=year, color=year)) +
      ggtitle(conflict_type)
  }else{
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=year, color=year)) +
      ggtitle(conflict_type) 
  }
}

conflict_fatalities_harvest <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=fatalities, group=season, color=season)) +
      ggtitle(conflict_type)
  }else{
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=season, color=season)) +
      ggtitle(conflict_type) 
  }
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
}

conflict_fatalities_year <- function(month_aggr, conflict_type, diff_=F, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=IPC_diff, y=fatalities, group=year, color=year)) +
      ggtitle(conflict_type)
  }else{
    IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
      ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=fatalities, group=year, color=year)) +
      ggtitle(conflict_type) 
  }
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
}

conflict_sub_n_events_diff <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=IPC_diff, y=n_events, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_sub_fatalities_diff <- function(month_aggr, conflict_type, growing_region=F) {
  IPC_NAT_phase3_long_conflict <- left_join(IPC_NAT_phase3_long,
                                            lagged_reg_data_list[[month_aggr]]$conflict_sub_NAT_aggr,
                                            by=c("Area", "year", "month"))
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))
  
  IPC_NAT_phase3_long_conflict %>% filter(event_type == conflict_type) %>% 
    ggplot() +
    geom_point(aes(x=IPC_diff, y=fatalities, group=sub_event_type, color=sub_event_type)) +
    ggtitle(conflict_type)
}

conflict_n_events_national <- function(month_aggr, filter_=F, growing_region=F, diff_=F) {
  IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$conflict_NAT_aggr %>% select(-year_month) %>% 
                group_by(year, month, event_type) %>%
                summarize(n_events = sum(n_events)),
              by=c("year", "month"))
  
  if (is.character(filter_)) IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long_month %>% filter(event_type == filter_) 
  else filter_ <- ""
  
  if (growing_region) IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long_month %>% filter(!(Area %in% no_crop_areas))
  
  if (diff_) {
    IPC_NAT_phase3_long_month %>% ggplot() +
      geom_point(aes(x=IPC_diff, y=n_events, group=Area, color=Area)) +
      ggtitle(filter_)
  }else{
    IPC_NAT_phase3_long_month %>% ggplot() +
      geom_point(aes(x=Phase_3above_ratio, y=n_events, group=Area, color=Area)) +
      ggtitle(filter_)
  }
}

disaster_map <- function(month_aggr, disaster_var) {
  IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$disaster_NAT_monthly_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  names(IPC_NAT_phase3_long_month)[which(names(IPC_NAT_phase3_long_month) == disaster_var)] <- "y_val"
  
  if (disaster_var == "n_disaster1") y_lab <- paste0("n_", disaster_type1)
  else if (disaster_var == "n_disaster2") y_lab <- paste0("n_", disaster_type2) 
  else y_lab <- disaster_var
  
  IPC_NAT_phase3_long_month %>% ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=y_val, group=season, color=season)) +
    ylab(y_lab)
}

disaster_diff_map <- function(month_aggr, disaster_var) {
  IPC_NAT_phase3_long_month <- IPC_NAT_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$disaster_NAT_monthly_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  names(IPC_NAT_phase3_long_month)[which(names(IPC_NAT_phase3_long_month) == disaster_var)] <- "y_val"
  
  if (disaster_var == "n_disaster1") y_lab <- paste0("n_", disaster_type1)
  else if (disaster_var == "n_disaster2") y_lab <- paste0("n_", disaster_type2) 
  else y_lab <- disaster_var
  
  IPC_NAT_phase3_long_month %>% ggplot() +
    geom_point(aes(x=IPC_diff, y=y_val, group=season, color=season)) +
    ylab(y_lab)
}

# conflict_NAT <- conflict_
# disaster_NAT <- disaster_
# IPC_NAT_provinces <- IPC_NAT_provinces
# IPC_NAT_provinces_long <- IPC_NAT_provinces_long
# IPC_NAT_year_month <- IPC_NAT_year_month
# IPC_NAT_year_month_list <- IPC_NAT_year_month_list
# crop_seasons <- crop_seasons
# conflict_types <- conflict_NAT %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type)

n_t <- nrow(IPC_NAT_year_month)
oldest_year <- IPC_NAT_year_month$Year[1]-1; oldest_month <- IPC_NAT_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_NAT_year_month$Year[n_t]; latest_month <- IPC_NAT_year_month$Month[n_t]

min(conflict_NAT$event_date)
IPC_NAT_year_month
month_lag <- 4 # AFG
month_lag <- 5 # SDN, COD

disaster_NAT %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>% 
  pull(`Disaster Type`) %>% table
disaster_type1 <- "Flood"
disaster_type2 <- "Epidemic" # SDN, COD
disaster_type2 <- "Drought" # AFG
# min_t + 1: the number of year_months in lagged_reg_data from the most recent one
min_t_ <- 9 # AFG
min_t_ <- 3 # SDN
min_t_ <- 5 # COD
lagged_reg_data_list <- list()
for (i in 1:month_lag) {
  lagged_reg_data_list[[paste0("months_", i)]] <- lagged_data_by_m(i, min_t_, disaster1=disaster_type1, disaster2 = disaster_type2)
}

# lagged_reg_data_list_AFG <- lagged_reg_data_list
# save("lagged_reg_data_list_AFG", file="Food Security/lagged_reg_data_list_AFG.RData")
# lagged_reg_data_list_SDN <- lagged_reg_data_list
# save("lagged_reg_data_list_SDN", file="Food Security/lagged_reg_data_list_SDN.RData")
# lagged_reg_data_list_COD <- lagged_reg_data_list
# save("lagged_reg_data_list_COD", file="Food Security/lagged_reg_data_list_COD.RData")

load("Food Security/lagged_reg_data_list_AFG.RData")
load("Food Security/lagged_reg_data_list_SDN.RData")
load("Food Security/lagged_reg_data_list_COD.RData")

month_lag_ <- 4

lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% summary
lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data %>% summary
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% summary

lagged_reg_data_list_AFG[[month_lag_]]$disaster_NAT_monthly_aggr %>% summary
lagged_reg_data_list_SDN[[month_lag_]]$disaster_NAT_monthly_aggr %>% summary
lagged_reg_data_list_COD[[month_lag_]]$disaster_NAT_monthly_aggr %>% summary


lagged_reg_data_list_AFG$months_1

disaster_Afg %>% 
  filter(year > 2016 - 1) %>% 
  filter(!(year == 2024 & month > 3)) %>% 
  filter(!(year == 2016 & month < 5)) %>% 
  pull(`Disaster Type`) %>% table
disaster_SDN %>% 
  filter(year > 2018 - 1) %>% 
  filter(!(year == 2024 & month > 4)) %>% 
  filter(!(year == 2018 & month < 6)) %>% 
  pull(`Disaster Type`) %>% table
disaster_COD %>% 
  filter(year > 2016 - 1) %>% 
  filter(!(year == 2023 & month > 8)) %>% 
  filter(!(year == 2016 & month < 6)) %>% 
  pull(`Disaster Type`) %>% table

disaster_Afg %>% 
  filter(year > 2016 - 1) %>% 
  filter(!(year == 2024 & month > 3)) %>% 
  filter(!(year == 2016 & month < 5)) %>% 
  filter(`Disaster Type` == "Drought") %>% 
  select(year, month) %>% unique
IPC_Afg_year_month
disaster_SDN %>% 
  filter(year > 2018 - 1) %>% 
  filter(!(year == 2024 & month > 4)) %>% 
  filter(!(year == 2018 & month < 6)) %>% 
  filter(`Disaster Type` == "Epidemic") %>% 
  select(year, month) %>% unique
IPC_SDN_year_month
disaster_COD %>% 
  filter(year > 2016 - 1) %>% 
  filter(!(year == 2023 & month > 8)) %>% 
  filter(!(year == 2016 & month < 6)) %>% 
  filter(`Disaster Type` == "Epidemic") %>% 
  select(year, month) %>% unique
IPC_COD_year_month



# lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% select(-n_disaster1_t, -n_disaster2_t) %>% mutate(year = as.factor(year))) %>% summary()
# lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data %>% select(-n_disaster1_t, -n_disaster2_t) %>% mutate(year = as.factor(year))) %>% summary()
# lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% select(-n_disaster1_t, -n_disaster2_t) %>% mutate(year = as.factor(year))) %>% summary()

reg_result_AFG <- lm(`Phase_3+ratio_t`~.-IPC_diff-year-month, data=lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>%
                       select(-(c_n_events_etc._t_diff:log_fatal_Battles_explosions_t_diff), -n_disaster1_t, -n_disaster2_t))
reg_result_SDN <- lm(`Phase_3+ratio_t`~.-IPC_diff-year-month, data=lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data %>% 
                       select(-(c_n_events_etc._t_diff:log_fatal_Battles_explosions_t_diff), -n_disaster1_t, -n_disaster2_t))
reg_result_COD <- lm(`Phase_3+ratio_t`~.-IPC_diff-year-month, data=lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
                       select(-(c_n_events_etc._t_diff:log_fatal_Battles_explosions_t_diff), -n_disaster1_t, -n_disaster2_t))

reg_result_AFG_conflict_diff <- lm(`Phase_3+ratio_t`~.-IPC_diff-year-month, data=lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>%
                       select(-(c_n_events_etc._t:log_fatal_Battles_explosions_t), -n_disaster1_t, -n_disaster2_t))
reg_result_SDN_conflict_diff <- lm(`Phase_3+ratio_t`~.-IPC_diff-year-month, data=lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data %>% 
                       select(-(c_n_events_etc._t:log_fatal_Battles_explosions_t), -n_disaster1_t, -n_disaster2_t))
reg_result_COD_conflict_diff <- lm(`Phase_3+ratio_t`~.-IPC_diff-year-month, data=lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
                       select(-(c_n_events_etc._t:log_fatal_Battles_explosions_t), -n_disaster1_t, -n_disaster2_t))

reg_result_AFG_all_diff <- lm(IPC_diff~.-year-month, data=lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data[,-(1:3)] %>%
                                     select(-(c_n_events_etc._t:log_fatal_Battles_explosions_t), -n_disaster1_t, -n_disaster2_t))
reg_result_SDN_all_diff <- lm(IPC_diff~.-year-month, data=lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data[,-(1:3)] %>% 
                                     select(-(c_n_events_etc._t:log_fatal_Battles_explosions_t), -n_disaster1_t, -n_disaster2_t))
reg_result_COD_all_diff <- lm(IPC_diff~.-year-month, data=lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data[,-(1:3)] %>% 
                                     select(-(c_n_events_etc._t:log_fatal_Battles_explosions_t), -n_disaster1_t, -n_disaster2_t))

reg_result_AFG %>% summary()
reg_result_SDN %>% summary()
reg_result_COD %>% summary()

reg_result_AFG_conflict_diff %>% summary()
reg_result_SDN_conflict_diff %>% summary()
reg_result_COD_conflict_diff %>% summary()

reg_result_AFG_all_diff %>% summary()
reg_result_SDN_all_diff %>% summary()
reg_result_COD_all_diff %>% summary()

reg_result_AFG$model %>%
  mutate(residual=reg_result_AFG$residuals,
         year = as.Date(paste(year, month, "01"), format="%Y %m %d")) %>% 
  ggplot() + ggtitle("AFG residuals by months with n_disasters and death") +
  geom_point(aes(x=year, y=residual))
reg_result_SDN$model %>%
  mutate(residual=reg_result_SDN$residuals,
         year = as.Date(paste(year, month, "01"), format="%Y %m %d")) %>% 
  ggplot() + ggtitle("SDN residuals by months with n_disasters and death") +
  geom_point(aes(x=year, y=residual))
reg_result_COD$model %>%
  mutate(residual=reg_result_COD$residuals,
         year = as.Date(paste(year, month, "01"), format="%Y %m %d")) %>%  
  ggplot() + ggtitle("COD residuals by months with n_disasters and death") +
  geom_point(aes(x=year, y=residual))


reg_result_AFG <- lm(`Phase_3+ratio_t`~.-year-month, data=lagged_reg_data_list_AFG[[month_lag_]]$lagged_reg_data %>% select(-n_disasters_t, -affected_t, -log_deaths_t))
reg_result_SDN <- lm(`Phase_3+ratio_t`~.-year-month, data=lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data %>% select(-n_disasters_t, -affected_t, -log_deaths_t))
reg_result_COD <- lm(`Phase_3+ratio_t`~.-year-month, data=lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% select(-n_disasters_t, -affected_t, -log_deaths_t))

reg_result_AFG %>% summary()
reg_result_SDN %>% summary()
reg_result_COD %>% summary()

reg_result_AFG$model %>%
  mutate(residual=reg_result_AFG$residuals,
         year = as.Date(paste(year, month, "01"), format="%Y %m %d")) %>% 
  ggplot() + ggtitle("AFG residuals by months with disaster 1 & 2") +
  geom_point(aes(x=year, y=residual))
reg_result_SDN$model %>%
  mutate(residual=reg_result_SDN$residuals,
         year = as.Date(paste(year, month, "01"), format="%Y %m %d")) %>% 
  ggplot() + ggtitle("SDN residuals by months with disaster 1 & 2") +
  geom_point(aes(x=year, y=residual))
reg_result_COD$model %>%
  mutate(residual=reg_result_COD$residuals,
         year = as.Date(paste(year, month, "01"), format="%Y %m %d")) %>%  
  ggplot() + ggtitle("COD residuals by months with disaster 1 & 2") +
  geom_point(aes(x=year, y=residual))



lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data$n_disaster1_t
lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data$n_disaster2_t
lagged_reg_data_list_SDN[[month_lag_]]$lagged_reg_data$month_diff

lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data$n_disaster1_t
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data$n_disaster2_t
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data$month_diff


lagged_reg_data_list <- lagged_reg_data_list_COD
IPC_NAT_phase3_long <- IPC_NAT_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  mutate(IPC_diff = Phase_3above_ratio - lag(Phase_3above_ratio)) %>% 
  left_join(crop_seasons, by="month")

# For COD
IPC_NAT_phase3_long <- IPC_NAT_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  mutate(IPC_diff = Phase_3above_ratio - lag(Phase_3above_ratio)) %>% 
  left_join(crop_seasons, by="month") %>% 
  mutate(season = ifelse(Area == "Haut Katanga", season_south, season))

conflict_NAT %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)

month_aggr_ <- 4 # AFG
conflict_map(month_aggr_)
conflict_map(month_aggr_, growing_region = T)
conflict_map(month_aggr_, "Battles")
conflict_map(month_aggr_, "Battles", growing_region = T)
conflict_map(month_aggr_, "Explosions/Remote violence")
conflict_map(month_aggr_, "Protests")
conflict_map(month_aggr_, "Riots")
conflict_map(month_aggr_, "Strategic developments")
conflict_map(month_aggr_, "Violence against civilians")

conflict_diff_map(month_aggr_)
conflict_diff_map(month_aggr_, "Battles")
conflict_diff_map(month_aggr_, "Explosions/Remote violence")
conflict_diff_map(month_aggr_, "Protests")
conflict_diff_map(month_aggr_, "Riots")
conflict_diff_map(month_aggr_, "Strategic developments")
conflict_diff_map(month_aggr_, "Violence against civilians")

conflict_6types <- unique(conflict_types$event_type)

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

conflict_n_events_year(month_aggr_, conflict_6types[1])

conflict_n_events_year(month_aggr_, conflict_6types[1], diff_=T)

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

# Disaster EDA
disaster_vars <- names(lagged_reg_data_list$months_1$disaster_NAT_monthly_aggr)[2:6]
disaster_map(month_aggr_, disaster_vars[1])
disaster_map(month_aggr_, disaster_vars[2])
disaster_map(month_aggr_, disaster_vars[3])
disaster_map(month_aggr_, disaster_vars[4])
disaster_map(month_aggr_, disaster_vars[5])

disaster_Afg %>% 
  filter(year > 2016 - 1) %>% 
  filter(!(year == 2024 & month > 3)) %>% 
  filter(!(year == 2016 & month < 5)) %>% 
  filter(`Disaster Type` == "Flood") %>% 
  select(year, month) %>% unique


disaster_diff_map(month_aggr_, disaster_vars[1])
disaster_diff_map(month_aggr_, disaster_vars[2])
disaster_diff_map(month_aggr_, disaster_vars[3]) ### higher diff with higher death?
disaster_diff_map(month_aggr_, disaster_vars[4])
disaster_diff_map(month_aggr_, disaster_vars[5])
