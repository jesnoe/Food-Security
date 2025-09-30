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

{# https://www.geoboundaries.org/countryDownloads.html
  CAF_map <- read_sf("Food Security/geoBoundaries-CAF-ADM1.geojson")
  CAF_map$shapeName %>% sort
  
  # https://data.humdata.org/dataset/cod-ab-caf
  CAF_adm_codes <- read_xlsx(("Food Security/CAF adm2 boundaries.xlsx")) %>% mutate(ADM1_EN = stri_trans_general(ADM1_FR, "Latin-ASCII")) %>% select(ADM1_EN, ADM1_PCODE) %>% unique %>% arrange(ADM1_EN)
  # CAF_adm_codes$ADM1_EN[which(CAF_adm_codes$ADM1_EN == "Hiraan")] <- "Hiran"
  
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
  conflict_AF <- read.csv("Food Security/Africa_conflict_1997-2024_Sep13.csv") %>% as_tibble
  
  IPC_AF <- read_xlsx("Food Security/East and Central Africa - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population))
  IPC_AF$Area_Phase <- IPC_AF %>% select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>% apply(1, function(x) which.max(x)) %>% as.factor
  IPC_AF$Date <- as.Date(paste(IPC_AF$Date, "01"), format="%b %Y %d")
  IPC_AF$Year <- year(IPC_AF$Date) %>% as.factor
  IPC_AF$Month <- month(IPC_AF$Date) %>% as.factor
  IPC_AF <- IPC_AF %>% relocate(Country:Area_id, Year, Month)
  
  IPC_CAF <- IPC_AF %>% filter(Country == "Central African Republic") %>% 
    mutate(Area = stri_trans_general(Area, "ASCII")) %>% filter(Area %in% CAF_adm_codes$ADM1_EN)# 421, 17 regions
  # IPC_CAF$Area %>% unique %>% sort
  
  IPC_CAF_provinces_long <- IPC_CAF %>%
    group_by(Area, Year, Month) %>% 
    summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
    mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
  
  CAF_map$shapeName %>% unique %>% sort
  IPC_CAF$Area %>% unique %>% sort
  CAF_adm_codes$ADM1_EN %>% unique %>% sort
  
  IPC_CAF_provinces <- IPC_CAF_provinces_long %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
    pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio)
  
  IPC_CAF_year_month <- IPC_CAF %>% select(Year, Month) %>% unique %>%
    mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric),
           year_month = paste(Year, Month, sep="_")) %>%
    arrange(Year, Month)
  
  IPC_CAF_year_month_list <- list(ym_t = IPC_CAF_year_month)
  prev_IPC_CAF_year_month <- IPC_CAF_year_month
  for (i in 1:12) {
    IPC_CAF_year_month_i <- prev_IPC_CAF_year_month %>% 
      mutate(Month = Month - 1,
             Year = ifelse(Month > 0, Year, Year - 1),
             Month = ifelse(Month > 0, Month, 12),
             year_month = paste(Year, Month, sep="_"))
    IPC_CAF_year_month_list[[paste0("ym_t_", i)]] <- IPC_CAF_year_month_i
    prev_IPC_CAF_year_month <- IPC_CAF_year_month_i
  }
  
  FSI_CAF <- FSI %>% filter(Country == "Central African Republic")
  disaster_CAF <- disaster %>% filter(Country == "Central African Republic") %>%  # 66
    rename(year=`Start Year`, month=`Start Month`)
  disaster_CAF_separate_locations <- read.csv("Food Security/Disaster CAF locations.csv") %>% as_tibble
  
  disaster_CAF <- disaster_CAF_separate_locations %>%
    select(DisNo., lat, long, Subregion, Region) %>% 
    left_join(disaster_CAF %>%
                select(-Region, -Subregion) %>% 
                rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
  
  conflict_CAF <- conflict_AF %>% filter(country == "Central African Republic") # 7,384
  conflict_CAF$admin1 %>% unique %>% sort
  conflict_CAF <- conflict_CAF %>% filter(admin1 != "") # 45,143
  conflict_CAF$event_date <- as.Date(conflict_CAF$event_date, format="%m/%d/%Y")
  conflict_CAF$month <- month(conflict_CAF$event_date)
  conflict_CAF <- conflict_CAF %>% relocate(event_id_cnty, event_date, year, month)
  conflict_CAF$admin1[which(conflict_CAF$admin1 == "Ouham-Pende")] <- "Ouham Pende"
  conflict_CAF$admin1[which(conflict_CAF$admin1 == "Ombella-M'Poko")] <- "Ombella M'Poko"
  
  # from https://ipad.fas.usda.gov/countrysummary/Default.aspx?id=CT
  # peanut - plant: 4~6, harvest: 7~10
  # corn - plant: 3~6 and 10, harvest: 7~9  
  # integrated - plant: 3~6, harvest: 7~10
  crop_seasons_CAF <- tibble(month=1:12,
                             season=c(rep("none", 2), rep("plant", 4), rep("harvest", 4), rep("none", 2)))
  no_crop_areas_CAF <- c()
  
  # CAF_sorghum <- c(11:12)
  
  time_since_harvest <- function(month., country_harvest_season) {
    result <- ifelse(month. %in% 10:12, month. - 10,
                     ifelse(month. %in% 1:6, month. + 12 - 10, 0))
    return(result)
  }
  # CAF_harvest <- list(sorghum=CAF_sorghum)
  
  # https://data.humdata.org/dataset/caf-rainfall-subnational
  precipitation_CAF <- read.csv("Food security/caf-rainfall-adm2-full.csv") %>% as_tibble %>% 
    mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
    filter(date >= as.Date("1/1/2019", "%m/%d/%Y") & date < as.Date("10/1/2023", "%m/%d/%Y")) %>% 
    mutate(ADM1_PCODE = substr(ADM2_PCODE, 1, 4))
  precipitation_CAF <- precipitation_CAF %>% 
    group_by(date, ADM1_PCODE) %>% 
    summarize(across(rfh:r3h_avg, function(x) sum(x))) %>% 
    mutate(year = year(date),
           month = month(date),
           year_month = paste(year, month, sep="_")) %>% 
    group_by(ADM1_PCODE, year, month, year_month) %>% 
    summarize(across(c(date, rfh:r3h_avg), function(x) rev(x)[1])) %>% 
    left_join(CAF_adm_codes %>% rename(Area=ADM1_EN), by="ADM1_PCODE")
  
  rainfall_tbl <- tibble()
  for (adm1 in unique(precipitation_CAF$Area)) {
    rainfall_tbl_area <- precipitation_CAF %>% filter(Area == adm1) %>% select(r1h, r3h)
    rainfall_tbl_area$spi_1h <- spi(rainfall_tbl_area$r1h, 12)$fitted
    rainfall_tbl_area$spi_3h <- spi(rainfall_tbl_area$r3h, 12)$fitted
    rainfall_tbl <- bind_rows(rainfall_tbl, rainfall_tbl_area)
  }
  precipitation_CAF <- left_join(precipitation_CAF, rainfall_tbl %>% select(-r1h, -r3h), by=c("ADM1_PCODE", "year", "month"))
}
{
  n_t <- nrow(IPC_CAF_year_month)
  oldest_year <- IPC_CAF_year_month$Year[1]-1; oldest_month <- IPC_CAF_year_month$Month[1] # year - 1 to gen t-12 at most
  latest_year <- IPC_CAF_year_month$Year[n_t]; latest_month <- IPC_CAF_year_month$Month[n_t]
  
  disaster_CAF %>% 
    filter(year > oldest_year - 1) %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    pull(`Disaster Type`) %>% table %>% as_tibble %>% rename(disaster=".")
  
  conflict_NAT <- conflict_CAF
  disaster_NAT <- disaster_CAF
  IPC_NAT_provinces <- IPC_CAF_provinces
  IPC_NAT_provinces_long <- IPC_CAF_provinces_long
  IPC_NAT_year_month <- IPC_CAF_year_month
  IPC_NAT_year_month_list <- IPC_CAF_year_month_list
  crop_seasons <- crop_seasons_CAF
  no_crop_areas <- no_crop_areas_CAF
  conflict_types <- conflict_CAF %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type)
  load("Food Security/lagged_reg_data_list_CAF.RData")
}

conflict_CAF$event_type %>% table
conflict_CAF$sub_event_type %>% table

conflict_CAF %>% ggplot +
  geom_bar(aes(x=event_type, fill=sub_event_type)) + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
conflict_CAF %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)



month_lag_ <- 4
# IPC vs. conflict
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=IPC_diff)) +
  geom_point()

# IPC vs. conflict_diff
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict_diff
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_CAF[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=IPC_diff)) +
  geom_point()

# regression with filtered data
month_lag_ <- 4
IPC_CAF_phase3_long_lagged <- IPC_CAF_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(crop_seasons_CAF %>% mutate(season = as.factor(season)), by="month") %>% 
  left_join(lagged_reg_data_list_CAF[[month_lag_]]$conflict_NAT_aggr %>%
              select(-year_month) %>% 
              mutate(event_type = ifelse(event_type %in% c("Battles", "Explosions/Remote violence"),
                                         "Battles_explosions",
                                         ifelse(event_type %in% c("Protests", "Riots"), "Protests_Riots",
                                                "etc.")
              )) %>% 
              group_by(Area, event_type, year, month) %>% 
              summarize(n_events = sum(n_events), fatalities = sum(fatalities)),
            by=c("Area", "year", "month"))  %>% 
  filter(!is.na(event_type)) %>% 
  pivot_wider(names_from = event_type, values_from = c("n_events", "fatalities")) %>% 
  left_join(lagged_reg_data_list_CAF[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))

IPC_CAF_phase3_long_lagged[is.na(IPC_CAF_phase3_long_lagged)] <- 0
IPC_CAF_phase3_long_lagged <- IPC_CAF_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
# n_row = 107
lm_conflict_CAF <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged[,4:17])
lm_conflict_CAF %>% summary
lm_conflict_diff_CAF <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_CAF %>% summary

IPC_CAF_year_month
conflict_map_CAF <- function(i) {
  CAF_map %>% left_join(IPC_CAF_phase3_long_lagged %>%
                          filter(year == IPC_CAF_year_month$Year[i] & month == IPC_CAF_year_month$Month[i]) %>%
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

conflict_map_CAF(1)
conflict_map_CAF(2)
conflict_map_CAF(3)
conflict_map_CAF(4)
conflict_map_CAF(5)
conflict_map_CAF(6)

IPC_CAF_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% print(n=18)
CAF_Area_ordered_by_n_events <- IPC_CAF_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% pull(Area)

lm_conflict_CAF_high_violence <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged %>%
                                      filter(Area %in% CAF_Area_ordered_by_n_events[1:7]) %>% 
                                      select(-(Area:month), -(n_events_Battles_explosions_diff:fatalities_etc._diff)))
lm_conflict_diff_CAF_high_violence <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged %>% 
                                           filter(Area %in% CAF_Area_ordered_by_n_events[1:7]) %>% 
                                           select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))

lm_conflict_CAF_high_violence %>% summary
lm_conflict_diff_CAF_high_violence %>% summary

CAF_high_violence_index <- which(names(lm_conflict_CAF$residuals) %>% as.numeric %in% which(IPC_CAF_phase3_long_lagged$Area %in% CAF_Area_ordered_by_n_events[1:7]))
lm_conflict_CAF$residuals[CAF_high_violence_index] %>% summary
mean((lm_conflict_CAF$residuals)^2)
mean((lm_conflict_CAF$residuals[CAF_high_violence_index])^2) # 0.004328112
lm_conflict_CAF_y_bar <- mean(lm_conflict_CAF$model$Phase_3above_ratio[CAF_high_violence_index])
1-sum((lm_conflict_CAF$residuals[CAF_high_violence_index])^2)/sum((lm_conflict_CAF$model$Phase_3above_ratio[CAF_high_violence_index]-lm_conflict_CAF_y_bar)^2) # R^2: 0.8073256
lm_conflict_diff_CAF$residuals[CAF_high_violence_index] %>% summary
mean((lm_conflict_diff_CAF$residuals)^2)
mean((lm_conflict_diff_CAF$residuals[CAF_high_violence_index])^2) # 0.006769709
lm_conflict_CAF_high_violence$residuals %>% summary
lm_conflict_diff_CAF_high_violence$residuals %>% summary
mean((lm_conflict_CAF_high_violence$residuals)^2) # 0.004122871
mean((lm_conflict_diff_CAF_high_violence$residuals)^2) # 0.006010389

ggplot(data.frame(x=1:length(CAF_high_violence_index), residuals=lm_conflict_CAF$residuals[CAF_high_violence_index])) +
  geom_point(aes(x=x, y=residuals)) + ylim(-0.2, 0.2) + ggtitle("Residuals in highly conflicted areas of global model (CAF)") +
  geom_abline(slope=0, intercept=mean((lm_conflict_CAF$residuals[CAF_high_violence_index])^2))

ggplot(data.frame(x=1:length(CAF_high_violence_index), residuals=lm_conflict_CAF_high_violence$residuals)) +
  geom_point(aes(x=x, y=residuals)) + ylim(-0.2, 0.2) + ggtitle("Residuals of local model (CAF)") +
  geom_abline(slope=0, intercept=mean((lm_conflict_CAF_high_violence$residuals)^2))

## regression with rainfall data
IPC_CAF_phase3_long_lagged_rainfall <- left_join(IPC_CAF_phase3_long_lagged, precipitation_CAF, by=c("Area", "year", "month"))
# write.csv(IPC_CAF_phase3_long_lagged_rainfall, "Food Security/IPC_CAF_phase3_long_lagged_rainfall.csv", row.names = F)
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
            spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
     select(-spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_class = ifelse(spi_1h > 1, "wet",
                                  ifelse(spi_1h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_1h)) %>% summary

lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
            spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
     select(-spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_class = ifelse(spi_3h > 1, "wet",
                                  ifelse(spi_3h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_3h)) %>% summary 

lm(Phase_3above_ratio~.+season*r3h, data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~.+season*spi_3h, data=IPC_CAF_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary

# regression with filtered data (Battles = Armed clash only)
month_lag_ <- 4
IPC_CAF_phase3_long_lagged <- IPC_CAF_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(lagged_reg_data_list_CAF[[month_lag_]]$conflict_sub_NAT_aggr %>%
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
  left_join(crop_seasons_CAF %>% mutate(season = as.factor(season)), by="month") %>% 
  filter(!is.na(Phase_3above_ratio)) %>% 
  pivot_wider(names_from = event_type, values_from = c("n_events", "fatalities")) %>% 
  left_join(lagged_reg_data_list_CAF[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))
IPC_CAF_phase3_long_lagged[is.na(IPC_CAF_phase3_long_lagged)] <- 0
IPC_CAF_phase3_long_lagged <- IPC_CAF_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         IPC_t_2 = lag(Phase_3above_ratio, 2),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
IPC_CAF_phase3_long_lagged[is.na(IPC_CAF_phase3_long_lagged)] <- 0
# n_row = 107

lm_conflict_CAF <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged[,4:17]) # without IPC_t_2
lm_conflict_CAF %>% summary
lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged[,4:18]) %>% summary # with IPC_t_2 (insignificant)
lm_conflict_diff_CAF <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_CAF %>% summary

CAF_Area_ordered_by_n_events <- IPC_CAF_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% pull(Area)

lm_conflict_CAF_high_violence <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged %>%
                                      filter(Area %in% CAF_Area_ordered_by_n_events[1:7]) %>% 
                                      select(-(Area:month), -(n_events_Battles_explosions_diff:fatalities_etc._diff)))
lm_conflict_diff_CAF_high_violence <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged %>% 
                                           filter(Area %in% CAF_Area_ordered_by_n_events[1:7]) %>% 
                                           select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))

lm_conflict_CAF_high_violence %>% summary
lm_conflict_diff_CAF_high_violence %>% summary

lm_conflict_CAF$residuals[(names(lm_conflict_CAF$residuals) %>% as.numeric) %in% which(IPC_CAF_phase3_long_lagged$Area %in% CAF_Area_ordered_by_n_events)] %>% summary
lm_conflict_diff_CAF$residuals[(names(lm_conflict_diff_CAF$residuals) %>% as.numeric) %in% which(IPC_CAF_phase3_long_lagged$Area %in% CAF_Area_ordered_by_n_events)] %>% summary
lm_conflict_CAF_high_violence$residuals %>% summary
lm_conflict_diff_CAF_high_violence$residuals %>% summary

# ts plots
lagged_months <- 4
IPC_CAF_provinces_long %>% 
  mutate(year=as.Date(paste(Month, Year, "01"), format="%m %Y %d")) %>% 
  ggplot() + ylim(0, 1) +
  geom_line(aes(x=year, y=Phase_3above_ratio, group=Area, color=Area))
ggsave("Food Security/Figs/ts plots/CAF/food insecurity ts plot CAF.png", scale=1)

conflict_CAF_monthly_aggr_by_type <- lagged_reg_data_list_CAF[[lagged_months]]$conflict_sub_NAT_aggr %>% group_by(year, month, event_type) %>%
  summarize(n_events = sum(n_events),
            fatalities = sum(fatalities)) %>% 
  arrange(year, month) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d"))

conflict_CAF_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=fatalities, group=event_type, color=event_type))
ggsave("Food Security/Figs/ts plots/CAF/conflict fatalities ts plot CAF.png", scale=1)

conflict_CAF_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_events, group=event_type, color=event_type))
ggsave("Food Security/Figs/ts plots/CAF/conflict n_events ts plot CAF.png", scale=1)

disaster1 <- "Flood"; disaster2 <- "Epidemic"
disaster_CAF_monthly_aggr_by_type <- disaster_CAF %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>%  
  group_by(year, month, `Disaster Type`) %>% 
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T)) %>% 
  rename(type=`Disaster Type`) %>% 
  mutate(year_month = paste(year, month, sep="_")) %>% 
  arrange(year, month) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d"))

disaster_CAF_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=type, color=type))
ggsave("Food Security/Figs/ts plots/CAF/disaster affected ts plot CAF.png", scale=1)

disaster_CAF_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=type, color=type))
ggsave("Food Security/Figs/ts plots/CAF/disaster deaths ts plot CAF.png", scale=1)

disaster_CAF_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=type, color=type))
ggsave("Food Security/Figs/ts plots/CAF/disaster n_disasters ts plot CAF.png", scale=1)

disaster_CAF_monthly_aggr_by_type %>% 
  # filter(type != "Flood") %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=type, color=type))

disaster_CAF_monthly_aggr_by_type %>% 
  # filter(type != "Flood") %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=type, color=type))

disaster_CAF_monthly_aggr_by_type %>% 
  # filter(type != "Flood") %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=type, color=type))

disaster_CAF_monthly_aggr_by_Area <- disaster_CAF %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>%  
  group_by(Region, year, month) %>%
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T)) %>% 
  rename(Area=Region) %>%
  mutate(year_month = paste(year, month, sep="_")) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>%
  arrange(year, month)

disaster_CAF_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=Area, color=Area))

disaster_CAF_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=Area, color=Area))

disaster_CAF_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=Area, color=Area))

