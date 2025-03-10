
lagged_data_by_m <- function(lagged_months, min_t) {
  conflict_NAT_aggr <- conflict_NAT %>% 
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
  
  disaster_NAT_monthly_aggr <- disaster_NAT %>% 
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
      summarize(across(n_disasters:n_droughts, function(x) sum(x))) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  disaster_NAT_monthly_aggr <- lagged_disaster[[IPC_NAT_year_month$year_month[1]]][1,]
  for (tbl in lagged_disaster) {
    disaster_NAT_monthly_aggr <- bind_rows(disaster_NAT_monthly_aggr, tbl)
  }
  disaster_NAT_monthly_aggr <- disaster_NAT_monthly_aggr[-1,]
  
  
  disaster_NAT_monthly_events <- disaster_NAT_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disasters=sum(n_disasters)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
  disaster_NAT_monthly_events[is.na(disaster_NAT_monthly_events)] <- 0
  
  disaster_NAT_monthly_flood <- disaster_NAT_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_floods=sum(n_floods)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_floods_", names_from = c(year, month), values_from = n_floods)
  disaster_NAT_monthly_flood[is.na(disaster_NAT_monthly_flood)] <- 0
  
  disaster_NAT_monthly_drought <- disaster_NAT_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_droughts=sum(n_droughts)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_droughts_", names_from = c(year, month), values_from = n_droughts)
  disaster_NAT_monthly_drought[is.na(disaster_NAT_monthly_drought)] <- 0
  
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
  conflict_ncol <- ncol(conflict_NAT_n_events) # 49
  conflict_fatalities_ncol <- ncol(conflict_NAT_fatalities) # 49
  disaster_ncol <- ncol(disaster_NAT_monthly_events) # 9
  disaster_affected_ncol <- ncol(disaster_NAT_monthly_affected) # 9
  names(conflict_NAT_n_events)[-1] <- paste0("c_n_events_", names(conflict_NAT_n_events)[-1])
  names(conflict_NAT_fatalities)[-1] <- paste0("log_fatal_", names(conflict_NAT_fatalities)[-1])
  reg_data_i <- IPC_NAT_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
    left_join(conflict_NAT_n_events[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>%
    left_join(conflict_NAT_fatalities[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>% 
    left_join(disaster_NAT_monthly_events[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_NAT_monthly_flood[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_NAT_monthly_drought[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_NAT_monthly_affected[,c(1, disaster_ncol)], by="Area") %>%
    left_join(disaster_NAT_monthly_deaths[,c(1, disaster_ncol)], by="Area") %>% 
    mutate(year = rev(IPC_NAT_year_month$Year)[1])
  names(reg_data_i) <- gsub(IPC_NAT_year_month$year_month[IPC_ncol-1], "t", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_NAT_year_month$year_month[IPC_ncol-2], "t_1", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_NAT_year_month$year_month[IPC_ncol-3], "t_2", names(reg_data_i))
  names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
  lagged_reg_data <- reg_data_i[,-1]
  lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_NAT_year_month$Year[14], IPC_NAT_year_month$Month[14], 1), format="%Y %m %d") -
                                             as.Date(paste(IPC_NAT_year_month$Year[13], IPC_NAT_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
  # lagged_reg_data$wheat_barley <- time_since_harvest(IPC_NAT_year_month$Month[14], "wheat_barley", NAT_harvest)
  reg_data_names <- names(lagged_reg_data)
  for (i in 1:min_t) {
    IPC_col_index <- IPC_ncol - i
    conflict_col_index <- conflict_ncol - 6*i
    disaster_col_index <- disaster_ncol - i
    reg_data_i <- IPC_NAT_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
      left_join(conflict_NAT_n_events[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>%
      left_join(conflict_NAT_fatalities[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>% 
      left_join(disaster_NAT_monthly_events[,c(1, disaster_col_index)], by="Area") %>%
      left_join(disaster_NAT_monthly_flood[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_NAT_monthly_drought[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_NAT_monthly_affected[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_NAT_monthly_deaths[,c(1, disaster_col_index)], by="Area") %>% 
      mutate(year = rev(IPC_NAT_year_month$Year)[IPC_col_index])
    reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_NAT_year_month$Year[14-i], IPC_NAT_year_month$Month[14-i], 1), format="%Y %m %d") -
                                          as.Date(paste(IPC_NAT_year_month$Year[13-i], IPC_NAT_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
    
    # reg_data_i$wheat_barley <- time_since_harvest(IPC_NAT_year_month$Month[14-i], "wheat_barley", NAT_harvest)
    reg_data_i <- as.matrix(reg_data_i[,-1])
    colnames(reg_data_i) <- reg_data_names
    lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
  }
  lagged_reg_data[is.na(lagged_reg_data)] <- 0
  
  result <- list()
  result$lagged_reg_data <- lagged_reg_data
  result$conflict_NAT_aggr <- conflict_NAT_aggr
  result$conflict_sub_NAT_aggr <- conflict_sub_NAT_aggr
  result$disaster_NAT_monthly_aggr <- disaster_NAT_monthly_aggr
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
  if (growing_region) IPC_NAT_phase3_long_conflict <- IPC_NAT_phase3_long_conflict %>% filter(!(Area %in% no_crop_areas))}

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

disaster_map <- function(month_aggr, disaster_type) {
  IPC_Afg_phase3_long_month <- IPC_Afg_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$disaster_Afg_monthly_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  names(IPC_Afg_phase3_long_month)[which(names(IPC_Afg_phase3_long_month) == disaster_type)] <- "y_val"
  
  IPC_Afg_phase3_long_month %>% ggplot() +
    geom_point(aes(x=Phase_3above_ratio, y=y_val, group=season, color=season)) +
    ylab(disaster_type)
}

disaster_diff_map <- function(month_aggr, disaster_type) {
  IPC_Afg_phase3_long_month <- IPC_Afg_phase3_long %>% 
    left_join(lagged_reg_data_list[[month_aggr]]$disaster_Afg_monthly_aggr %>% select(-year_month),
              by=c("Area", "year", "month"))
  
  names(IPC_Afg_phase3_long_month)[which(names(IPC_Afg_phase3_long_month) == disaster_type)] <- "y_val"
  
  IPC_Afg_phase3_long_month %>% ggplot() +
    geom_point(aes(x=IPC_diff, y=y_val, group=season, color=season)) +
    ylab(disaster_type)
}

conflict_NAT <- conflict_
disaster_NAT <- disaster_
IPC_NAT_provinces <- IPC_NAT_provinces
IPC_NAT_provinces_long <- IPC_NAT_provinces_long
IPC_NAT_year_month <- IPC_NAT_year_month
IPC_NAT_year_month_list <- IPC_NAT_year_month_list

n_t <- nrow(IPC_NAT_year_month)
oldest_year <- IPC_NAT_year_month$Year[1]-1; oldest_month <- IPC_NAT_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_NAT_year_month$Year[n_t]; latest_month <- IPC_NAT_year_month$Month[n_t]

lagged_reg_data_list <- list()
for (i in 1:5) {
  m <- i
  lagged_reg_data_list[[paste0("months_", i)]] <- lagged_data_by_m(m, 5) # min_t = 5
}

lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2$lagged_reg_data %>% select(-n_floods_t, -n_droughts_t)) %>% summary()
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2$lagged_reg_data %>% select(-n_disasters_t)) %>% summary()

IPC_NAT_phase3_long <- IPC_NAT_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  mutate(IPC_diff = Phase_3above_ratio - lag(Phase_3above_ratio),) %>% 
  left_join(crop_seasons, by="month")




corn_seasons <- tibble(month=1:12,
                       season=c("none", rep("plant", 2), rep("grow", 2), rep("harvest", 2), rep("none", 2), rep("plant", 3), "none"))
rice_seasons <- tibble(month=1:12,
                       season=c(rep("grow", 4), rep("harvest", 3), rep("none", 2), rep("plant", 3)))

conflict_NAT %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)

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