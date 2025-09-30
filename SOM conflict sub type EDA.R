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
  SOM_map <- read_sf("Food Security/geoBoundaries-SOM-ADM1.geojson")
  SOM_map$shapeName[which(SOM_map$shapeName == "Hiiraan")] <- "Hiraan"
  SOM_map$shapeName %>% sort
  
  # https://data.humdata.org/dataset/cod-ab-som
  SOM_adm_codes <- read_xlsx(("Food Security/SOM adm2 boundaries.xlsx")) %>% select(ADM1_EN, ADM1_PCODE) %>% unique %>% arrange(ADM1_EN)
  # SOM_adm_codes$ADM1_EN[which(SOM_adm_codes$ADM1_EN == "Hiraan")] <- "Hiran"

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
  
  IPC_SOM <- IPC_AF %>% filter(Country == "Somalia") # 2746, 18 regions
  IPC_SOM$Area[which(IPC_SOM$Area == "Juba Dhexe")] <- "Middle Juba"
  IPC_SOM$Area[which(IPC_SOM$Area == "Juba Hoose")] <- "Lower Juba"
  IPC_SOM$Area[which(IPC_SOM$Area == "Shabelle Dhexe")] <- "Middle Shabelle"
  IPC_SOM$Area[which(IPC_SOM$Area == "Shabelle Hoose")] <- "Lower Shabelle"
  IPC_SOM$Area %>% unique %>% sort
  
  IPC_SOM_provinces_long <- IPC_SOM %>%
    group_by(Area, Year, Month) %>% 
    summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
    mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
           Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
  
  SOM_map$shapeName %>% unique %>% sort
  IPC_SOM$Area %>% unique %>% sort
  SOM_adm_codes$ADM1_EN %>% unique %>% sort
  
  IPC_SOM_provinces <- IPC_SOM_provinces_long %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
    pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio)
  
  IPC_SOM_year_month <- IPC_SOM %>% select(Year, Month) %>% unique %>%
    mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric),
           year_month = paste(Year, Month, sep="_")) %>%
    arrange(Year, Month)
  
  IPC_SOM_year_month_list <- list(ym_t = IPC_SOM_year_month)
  prev_IPC_SOM_year_month <- IPC_SOM_year_month
  for (i in 1:12) {
    IPC_SOM_year_month_i <- prev_IPC_SOM_year_month %>% 
      mutate(Month = Month - 1,
             Year = ifelse(Month > 0, Year, Year - 1),
             Month = ifelse(Month > 0, Month, 12),
             year_month = paste(Year, Month, sep="_"))
    IPC_SOM_year_month_list[[paste0("ym_t_", i)]] <- IPC_SOM_year_month_i
    prev_IPC_SOM_year_month <- IPC_SOM_year_month_i
  }
  
  FSI_SOM <- FSI %>% filter(Country == "Somalia")
  FSI_SOM <- FSI %>% filter(Country == "Somalia")
  disaster_SOM <- disaster %>% filter(Country == "Somalia") %>%  # 97
    rename(year=`Start Year`, month=`Start Month`)
  disaster_SOM_separate_locations <- read.csv("Food Security/Disaster SOM locations.csv") %>% as_tibble
  
  disaster_SOM <- disaster_SOM_separate_locations %>%
    select(DisNo., lat, long, Subregion, Region) %>% 
    left_join(disaster_SOM %>%
                select(-Region, -Subregion) %>% 
                rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
  
  conflict_SOM <- conflict_AF %>% filter(country == "Somalia") # 45,151
  conflict_SOM$admin1 %>% unique %>% sort
  conflict_SOM <- conflict_SOM %>% filter(admin1 != "") # 45,143
  conflict_SOM$event_date <- as.Date(conflict_SOM$event_date, format="%m/%d/%Y")
  conflict_SOM$month <- month(conflict_SOM$event_date)
  conflict_SOM <- conflict_SOM %>% relocate(event_id_cnty, event_date, year, month)
  conflict_SOM$admin1[which(conflict_SOM$admin1 == "Al Jazirah")] <- "Gezira"
  
  # from https://ipad.fas.usda.gov/countrysummary/Default.aspx?id=SO
  # corn - plant: 4 and 9, growing: 5~7 and 10~2, harvest: 3 and 8
  # sorghum - plant: 4 and 10, growing: 5~7 and 11~2, harvest: 3 and 8
  # integrated - plant: 4 and 9~10, growing: 5~7 and 11~2, harvest: 3 and 8
  crop_seasons_SOM <- tibble(month=1:12,
                             season=c(rep("grow", 2), "harvest", "plant", rep("grow", 3), "harvest", rep("plant", 2), rep("grow", 2)))
  no_crop_areas_SOM <- c()
  
  # SOM_sorghum <- c(11:12)
  
  time_since_harvest <- function(month., country_harvest_season) {
    result <- ifelse(month. %in% 8:12, month. - 8,
                     ifelse(month. %in% 1:2, month. + 12 - 8, month. - 3))
    return(result)
  }
  # SOM_harvest <- list(sorghum=SOM_sorghum)
  
  # https://data.humdata.org/dataset/som-rainfall-subnational
  precipitation_SOM <- read.csv("Food security/som-rainfall-adm2-full.csv") %>% as_tibble %>% 
    mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
    filter(date >= as.Date("1/1/2017", "%m/%d/%Y") & date < as.Date("2/1/2024", "%m/%d/%Y")) %>% 
    mutate(ADM1_PCODE = substr(ADM2_PCODE, 1, 4))
  precipitation_SOM <- precipitation_SOM %>% 
    group_by(date, ADM1_PCODE) %>% 
    summarize(across(rfh:r3h_avg, function(x) sum(x))) %>% 
    mutate(year = year(date),
           month = month(date),
           year_month = paste(year, month, sep="_")) %>% 
    group_by(ADM1_PCODE, year, month, year_month) %>% 
    summarize(across(c(date, rfh:r3h_avg), function(x) rev(x)[1])) %>% 
    left_join(SOM_adm_codes %>% rename(Area=ADM1_EN), by="ADM1_PCODE")
  
  rainfall_tbl <- tibble()
  for (adm1 in unique(precipitation_SOM$Area)) {
    rainfall_tbl_area <- precipitation_SOM %>% filter(Area == adm1) %>% select(r1h, r3h)
    rainfall_tbl_area$spi_1h <- spi(rainfall_tbl_area$r1h, 12)$fitted
    rainfall_tbl_area$spi_3h <- spi(rainfall_tbl_area$r3h, 12)$fitted
    rainfall_tbl <- bind_rows(rainfall_tbl, rainfall_tbl_area)
  }
  precipitation_SOM <- left_join(precipitation_SOM, rainfall_tbl %>% select(-r1h, -r3h), by=c("ADM1_PCODE", "year", "month"))
}
{
n_t <- nrow(IPC_SOM_year_month)
oldest_year <- IPC_SOM_year_month$Year[1]-1; oldest_month <- IPC_SOM_year_month$Month[1] # year - 1 to gen t-12 at most
latest_year <- IPC_SOM_year_month$Year[n_t]; latest_month <- IPC_SOM_year_month$Month[n_t]

disaster_SOM %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>% 
  pull(`Disaster Type`) %>% table %>% as_tibble %>% rename(disaster=".")

conflict_NAT <- conflict_SOM
disaster_NAT <- disaster_SOM
IPC_NAT_provinces <- IPC_SOM_provinces
IPC_NAT_provinces_long <- IPC_SOM_provinces_long
IPC_NAT_year_month <- IPC_SOM_year_month
IPC_NAT_year_month_list <- IPC_SOM_year_month_list
crop_seasons <- crop_seasons_SOM
no_crop_areas <- no_crop_areas_SOM
conflict_types <- conflict_SOM %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type)
load("Food Security/lagged_reg_data_list_SOM.RData")
}

conflict_SOM$event_type %>% table
conflict_SOM$sub_event_type %>% table

conflict_SOM %>% ggplot +
  geom_bar(aes(x=event_type, fill=sub_event_type)) + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
conflict_SOM %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)



month_lag_ <- 4
# IPC vs. conflict
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=IPC_diff)) +
  geom_point()

# IPC vs. conflict_diff
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict_diff
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_SOM[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=IPC_diff)) +
  geom_point()

# regression with filtered data
month_lag_ <- 4
IPC_SOM_phase3_long_lagged <- IPC_SOM_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(crop_seasons_SOM %>% mutate(season = as.factor(season)), by="month") %>% 
  left_join(lagged_reg_data_list_SOM[[month_lag_]]$conflict_NAT_aggr %>%
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
  left_join(lagged_reg_data_list_SOM[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))

IPC_SOM_phase3_long_lagged[is.na(IPC_SOM_phase3_long_lagged)] <- 0
IPC_SOM_phase3_long_lagged <- IPC_SOM_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
  # n_row = 107
lm_conflict_SOM <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged[,4:17])
lm_conflict_SOM %>% summary
lm_conflict_diff_SOM <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_SOM %>% summary

IPC_SOM_year_month
conflict_map_SOM <- function(i) {
  SOM_map %>% left_join(IPC_SOM_phase3_long_lagged %>%
                          filter(year == IPC_SOM_year_month$Year[i] & month == IPC_SOM_year_month$Month[i]) %>%
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

conflict_map_SOM(1)
conflict_map_SOM(2)
conflict_map_SOM(3)
conflict_map_SOM(4)
conflict_map_SOM(5)
conflict_map_SOM(6)

IPC_SOM_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% print(n=18)
SOM_Area_ordered_by_n_events <- IPC_SOM_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% pull(Area)

lm_conflict_SOM_high_violence <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged %>%
     filter(Area %in% SOM_Area_ordered_by_n_events[1:7]) %>% 
     select(-(Area:month), -(n_events_Battles_explosions_diff:fatalities_etc._diff)))
lm_conflict_diff_SOM_high_violence <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged %>% 
     filter(Area %in% SOM_Area_ordered_by_n_events[1:7]) %>% 
     select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))

lm_conflict_SOM_high_violence %>% summary
lm_conflict_diff_SOM_high_violence %>% summary

SOM_high_violence_index <- which(names(lm_conflict_SOM$residuals) %>% as.numeric %in% which(IPC_SOM_phase3_long_lagged$Area %in% SOM_Area_ordered_by_n_events[1:7]))
lm_conflict_SOM$residuals[SOM_high_violence_index] %>% summary
mean((lm_conflict_SOM$residuals)^2)
mean((lm_conflict_SOM$residuals[SOM_high_violence_index])^2) # 0.004328112
lm_conflict_SOM_y_bar <- mean(lm_conflict_SOM$model$Phase_3above_ratio[SOM_high_violence_index])
1-sum((lm_conflict_SOM$residuals[SOM_high_violence_index])^2)/sum((lm_conflict_SOM$model$Phase_3above_ratio[SOM_high_violence_index]-lm_conflict_SOM_y_bar)^2) # R^2: 0.8073256
lm_conflict_diff_SOM$residuals[SOM_high_violence_index] %>% summary
mean((lm_conflict_diff_SOM$residuals)^2)
mean((lm_conflict_diff_SOM$residuals[SOM_high_violence_index])^2) # 0.006769709
lm_conflict_SOM_high_violence$residuals %>% summary
lm_conflict_diff_SOM_high_violence$residuals %>% summary
mean((lm_conflict_SOM_high_violence$residuals)^2) # 0.004122871
mean((lm_conflict_diff_SOM_high_violence$residuals)^2) # 0.006010389

ggplot(data.frame(x=1:length(SOM_high_violence_index), residuals=lm_conflict_SOM$residuals[SOM_high_violence_index])) +
  geom_point(aes(x=x, y=residuals)) + ylim(-0.2, 0.2) + ggtitle("Residuals in highly conflicted areas of global model (SOM)") +
  geom_abline(slope=0, intercept=mean((lm_conflict_SOM$residuals[SOM_high_violence_index])^2))

ggplot(data.frame(x=1:length(SOM_high_violence_index), residuals=lm_conflict_SOM_high_violence$residuals)) +
  geom_point(aes(x=x, y=residuals)) + ylim(-0.2, 0.2) + ggtitle("Residuals of local model (SOM)") +
  geom_abline(slope=0, intercept=mean((lm_conflict_SOM_high_violence$residuals)^2))

## regression with rainfall data
IPC_SOM_phase3_long_lagged_rainfall <- left_join(IPC_SOM_phase3_long_lagged, precipitation_SOM, by=c("Area", "year", "month"))
# write.csv(IPC_SOM_phase3_long_lagged_rainfall, "Food Security/IPC_SOM_phase3_long_lagged_rainfall.csv", row.names = F)
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
            spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
     select(-spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_class = ifelse(spi_1h > 1, "wet",
                                  ifelse(spi_1h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_1h)) %>% summary

lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
            spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
     select(-spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_class = ifelse(spi_3h > 1, "wet",
                                  ifelse(spi_3h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_3h)) %>% summary 

lm(Phase_3above_ratio~.+season*r3h, data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~.+season*spi_3h, data=IPC_SOM_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary

# regression with filtered data (Battles = Armed clash only)
month_lag_ <- 4
IPC_SOM_phase3_long_lagged <- IPC_SOM_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(lagged_reg_data_list_SOM[[month_lag_]]$conflict_sub_NAT_aggr %>%
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
  left_join(crop_seasons_SOM %>% mutate(season = as.factor(season)), by="month") %>% 
  filter(!is.na(Phase_3above_ratio)) %>% 
  pivot_wider(names_from = event_type, values_from = c("n_events", "fatalities")) %>% 
  left_join(lagged_reg_data_list_SOM[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))
IPC_SOM_phase3_long_lagged[is.na(IPC_SOM_phase3_long_lagged)] <- 0
IPC_SOM_phase3_long_lagged <- IPC_SOM_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         IPC_t_2 = lag(Phase_3above_ratio, 2),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
IPC_SOM_phase3_long_lagged[is.na(IPC_SOM_phase3_long_lagged)] <- 0
# n_row = 107

lm_conflict_SOM <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged[,4:17]) # without IPC_t_2
lm_conflict_SOM %>% summary
lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged[,4:18]) %>% summary # with IPC_t_2 (insignificant)
lm_conflict_diff_SOM <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_SOM %>% summary

SOM_Area_ordered_by_n_events <- IPC_SOM_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% pull(Area)

lm_conflict_SOM_high_violence <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged %>%
                                      filter(Area %in% SOM_Area_ordered_by_n_events[1:7]) %>% 
                                      select(-(Area:month), -(n_events_Battles_explosions_diff:fatalities_etc._diff)))
lm_conflict_diff_SOM_high_violence <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged %>% 
                                           filter(Area %in% SOM_Area_ordered_by_n_events[1:7]) %>% 
                                           select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))

lm_conflict_SOM_high_violence %>% summary
lm_conflict_diff_SOM_high_violence %>% summary

lm_conflict_SOM$residuals[(names(lm_conflict_SOM$residuals) %>% as.numeric) %in% which(IPC_SOM_phase3_long_lagged$Area %in% SOM_Area_ordered_by_n_events)] %>% summary
lm_conflict_diff_SOM$residuals[(names(lm_conflict_diff_SOM$residuals) %>% as.numeric) %in% which(IPC_SOM_phase3_long_lagged$Area %in% SOM_Area_ordered_by_n_events)] %>% summary
lm_conflict_SOM_high_violence$residuals %>% summary
lm_conflict_diff_SOM_high_violence$residuals %>% summary

# ts plots
lagged_months <- 4
IPC_SOM_provinces_long %>% 
  mutate(year=as.Date(paste(Month, Year, "01"), format="%m %Y %d")) %>% 
  ggplot() + ylim(0, 1) +
  geom_line(aes(x=year, y=Phase_3above_ratio, group=Area, color=Area))
ggsave("Food Security/Figs/ts plots/SOM/food insecurity ts plot SOM.png", scale=1)

conflict_SOM_monthly_aggr_by_type <- lagged_reg_data_list_CAF[[lagged_months]]$conflict_sub_NAT_aggr %>% group_by(year, month, event_type) %>%
  summarize(n_events = sum(n_events),
            fatalities = sum(fatalities)) %>% 
  arrange(year, month) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d"))

conflict_SOM_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=fatalities, group=event_type, color=event_type))
ggsave("Food Security/Figs/ts plots/SOM/conflict fatalities ts plot SOM.png", scale=1)

conflict_SOM_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_events, group=event_type, color=event_type))
ggsave("Food Security/Figs/ts plots/SOM/conflict n_events ts plot SOM.png", scale=1)

disaster1 <- "Flood"; disaster2 <- "Epidemic"
disaster_SOM_monthly_aggr_by_type <- disaster_SOM %>% 
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

disaster_SOM_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=type, color=type))
ggsave("Food Security/Figs/ts plots/SOM/disaster affected ts plot SOM.png", scale=1)

disaster_SOM_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=type, color=type))
ggsave("Food Security/Figs/ts plots/SOM/disaster deaths ts plot SOM.png", scale=1)

disaster_SOM_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=type, color=type))
ggsave("Food Security/Figs/ts plots/SOM/disaster n_disasters ts plot SOM.png", scale=1)

disaster_SOM_monthly_aggr_by_type %>% 
  filter(type != "Flood") %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=type, color=type))

disaster_SOM_monthly_aggr_by_type %>% 
  filter(type != "Flood") %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=type, color=type))

disaster_SOM_monthly_aggr_by_type %>% 
  filter(type != "Flood") %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=type, color=type))

disaster_SOM_monthly_aggr_by_Area <- disaster_SOM %>% 
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

disaster_SOM_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=Area, color=Area))

disaster_SOM_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=Area, color=Area))

disaster_SOM_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=Area, color=Area))

lagged_data_by_m <- function(lagged_months, min_t) {
  conflict_SOM_aggr <- conflict_SOM %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_SOM_aggr <- conflict_SOM_aggr[,-(1:2)] %>% 
    complete(year_month = lapply(IPC_SOM_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist,
             event_type = unique(conflict_SOM_aggr$event_type)) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  conflict_SOM_aggr[is.na(conflict_SOM_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_SOM_year_month)) {
    year_i <- IPC_SOM_year_month$Year[i]
    month_i <- IPC_SOM_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_SOM_aggr$year == prev_year & conflict_SOM_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_SOM_aggr$year == year_i & conflict_SOM_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_SOM_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type) %>% 
      summarize(n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_SOM_aggr <- lagged_conflict[[IPC_SOM_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_SOM_aggr <- bind_rows(conflict_SOM_aggr, tbl)
  }
  conflict_SOM_aggr <- conflict_SOM_aggr[-1,]
  
  conflict_SOM_n_events <- conflict_SOM_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(n_events=sum(n_events)) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
  conflict_SOM_n_events[is.na(conflict_SOM_n_events)] <- 0
  
  conflict_SOM_fatalities <- conflict_SOM_aggr %>% 
    group_by(year, month, Area, event_type) %>% 
    summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
  conflict_SOM_fatalities[is.na(conflict_SOM_fatalities)] <- 0
  
  conflict_sub_SOM_aggr <- conflict_SOM %>% 
    filter(!(year == latest_year & month > latest_month)) %>% 
    filter(!(year == oldest_year & month < oldest_month)) %>% 
    group_by(year, month, admin1, sub_event_type) %>% 
    summarise(n_events=n(),
              fatalities=sum(fatalities, na.rm=T)) %>% 
    rename(Area=admin1) %>% 
    arrange(year, month) %>% 
    mutate(year_month = paste(year, month, sep="_"))
  conflict_sub_SOM_aggr <- conflict_sub_SOM_aggr[,-(1:2)] %>% 
    complete(year_month,
             sub_event_type) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    left_join(conflict_types, by="sub_event_type") %>% 
    arrange(year, month)
  
  
  conflict_sub_SOM_aggr[is.na(conflict_sub_SOM_aggr)] <- 0
  
  lagged_conflict <- list()
  for (i in 1:nrow(IPC_SOM_year_month)) {
    year_i <- IPC_SOM_year_month$Year[i]
    month_i <- IPC_SOM_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(conflict_sub_SOM_aggr$year == prev_year & conflict_sub_SOM_aggr$month == prev_month)[1]
    months_index_i <- rev(which(conflict_sub_SOM_aggr$year == year_i & conflict_sub_SOM_aggr$month == month_i))[1]
    lagged_conflict[[year_month_i]] <- conflict_sub_SOM_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area, event_type, sub_event_type) %>% 
      summarize(event_type = event_type[1],
                n_events=sum(n_events),
                fatalities=sum(fatalities)) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  conflict_sub_SOM_aggr <- lagged_conflict[[IPC_SOM_year_month$year_month[1]]][1,]
  for (tbl in lagged_conflict) {
    conflict_sub_SOM_aggr <- bind_rows(conflict_sub_SOM_aggr, tbl)
  }
  conflict_sub_SOM_aggr <- conflict_sub_SOM_aggr[-1,]
  
  disaster_SOM_monthly_aggr <- disaster_SOM %>% 
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
  
  disaster_SOM_monthly_aggr <- disaster_SOM_monthly_aggr[,-(2:3)] %>% 
    complete(year_month = lapply(IPC_SOM_year_month_list[1:(lagged_months+1)], function(x) x$year_month) %>% unlist) %>%
    mutate(month = substr(year_month, 6, str_length(year_month)) %>% as.numeric,
           year = substr(year_month, 1, 4) %>% as.numeric) %>% 
    arrange(year, month)
  disaster_SOM_monthly_aggr[is.na(disaster_SOM_monthly_aggr)] <- 0
  
  lagged_disaster <- list()
  for (i in 1:nrow(IPC_SOM_year_month)) {
    year_i <- IPC_SOM_year_month$Year[i]
    month_i <- IPC_SOM_year_month$Month[i]
    year_month_i <- paste(year_i, month_i, sep="_")
    prev_month <- ifelse(month_i > lagged_months, month_i - lagged_months, month_i + 12 - lagged_months)
    prev_year <- ifelse(month_i > lagged_months, year_i, year_i - 1)
    prev_months_index <- which(disaster_SOM_monthly_aggr$year == prev_year & disaster_SOM_monthly_aggr$month == prev_month)[1]
    months_index_i <- rev(which(disaster_SOM_monthly_aggr$year == year_i & disaster_SOM_monthly_aggr$month == month_i))[1]
    lagged_disaster[[year_month_i]] <- disaster_SOM_monthly_aggr[prev_months_index:months_index_i,] %>% 
      select(-year_month, -month, -year) %>% 
      group_by(Area) %>% 
      summarize(across(n_disasters:n_droughts, function(x) sum(x))) %>% 
      mutate(year_month = year_month_i,
             year = year_i,
             month = month_i)
  }
  
  disaster_SOM_monthly_aggr <- lagged_disaster[[IPC_SOM_year_month$year_month[1]]][1,]
  for (tbl in lagged_disaster) {
    disaster_SOM_monthly_aggr <- bind_rows(disaster_SOM_monthly_aggr, tbl)
  }
  disaster_SOM_monthly_aggr <- disaster_SOM_monthly_aggr[-1,]
  
  
  disaster_SOM_monthly_events <- disaster_SOM_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_disasters=sum(n_disasters)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
  disaster_SOM_monthly_events[is.na(disaster_SOM_monthly_events)] <- 0
  
  disaster_SOM_monthly_flood <- disaster_SOM_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_floods=sum(n_floods)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_floods_", names_from = c(year, month), values_from = n_floods)
  disaster_SOM_monthly_flood[is.na(disaster_SOM_monthly_flood)] <- 0
  
  disaster_SOM_monthly_drought <- disaster_SOM_monthly_aggr %>% 
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(n_droughts=sum(n_droughts)) %>% 
    pivot_wider(id_cols=Area, names_prefix = "n_droughts_", names_from = c(year, month), values_from = n_droughts)
  disaster_SOM_monthly_drought[is.na(disaster_SOM_monthly_drought)] <- 0
  
  disaster_SOM_monthly_affected <- disaster_SOM_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
  disaster_SOM_monthly_affected[is.na(disaster_SOM_monthly_affected)] <- 0
  
  disaster_SOM_monthly_deaths <- disaster_SOM_monthly_aggr %>%
    select(-year_month) %>% 
    group_by(year, month, Area) %>% 
    summarise(log_deaths=log(1+sum(deaths, na.rm=T))) %>% 
    pivot_wider(id_cols=Area, names_prefix = "log_deaths_", names_from = c(year, month), values_from = log_deaths)
  disaster_SOM_monthly_deaths[is.na(disaster_SOM_monthly_deaths)] <- 0
  
  
  # regression data
  IPC_ncol <- ncol(IPC_SOM_provinces)
  conflict_ncol <- ncol(conflict_SOM_n_events) # 49
  conflict_fatalities_ncol <- ncol(conflict_SOM_fatalities) # 49
  disaster_ncol <- ncol(disaster_SOM_monthly_events) # 9
  disaster_affected_ncol <- ncol(disaster_SOM_monthly_affected) # 9
  names(conflict_SOM_n_events)[-1] <- paste0("c_n_events_", names(conflict_SOM_n_events)[-1])
  names(conflict_SOM_fatalities)[-1] <- paste0("log_fatal_", names(conflict_SOM_fatalities)[-1])
  reg_data_i <- IPC_SOM_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
    left_join(conflict_SOM_n_events[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>%
    left_join(conflict_SOM_fatalities[,c(1, conflict_ncol:(conflict_ncol-6+1))], by="Area") %>% 
    left_join(disaster_SOM_monthly_events[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_SOM_monthly_flood[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_SOM_monthly_drought[,c(1, disaster_ncol)], by="Area") %>% 
    left_join(disaster_SOM_monthly_affected[,c(1, disaster_ncol)], by="Area") %>%
    left_join(disaster_SOM_monthly_deaths[,c(1, disaster_ncol)], by="Area") %>% 
    mutate(year = rev(IPC_SOM_year_month$Year)[1])
  names(reg_data_i) <- gsub(IPC_SOM_year_month$year_month[IPC_ncol-1], "t", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_SOM_year_month$year_month[IPC_ncol-2], "t_1", names(reg_data_i))
  names(reg_data_i) <- gsub(IPC_SOM_year_month$year_month[IPC_ncol-3], "t_2", names(reg_data_i))
  names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
  lagged_reg_data <- reg_data_i[,-1]
  lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_SOM_year_month$Year[14], IPC_SOM_year_month$Month[14], 1), format="%Y %m %d") -
                                             as.Date(paste(IPC_SOM_year_month$Year[13], IPC_SOM_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
  # lagged_reg_data$wheat_barley <- time_since_harvest(IPC_SOM_year_month$Month[14], "wheat_barley", SOM_harvest)
  reg_data_names <- names(lagged_reg_data)
  for (i in 1:min_t) {
    IPC_col_index <- IPC_ncol - i
    conflict_col_index <- conflict_ncol - 6*i
    disaster_col_index <- disaster_ncol - i
    reg_data_i <- IPC_SOM_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
      left_join(conflict_SOM_n_events[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>%
      left_join(conflict_SOM_fatalities[,c(1, conflict_col_index:(conflict_col_index-6+1))], by="Area") %>% 
      left_join(disaster_SOM_monthly_events[,c(1, disaster_col_index)], by="Area") %>%
      left_join(disaster_SOM_monthly_flood[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_SOM_monthly_drought[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_SOM_monthly_affected[,c(1, disaster_col_index)], by="Area") %>% 
      left_join(disaster_SOM_monthly_deaths[,c(1, disaster_col_index)], by="Area") %>% 
      mutate(year = rev(IPC_SOM_year_month$Year)[IPC_col_index])
    reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_SOM_year_month$Year[14-i], IPC_SOM_year_month$Month[14-i], 1), format="%Y %m %d") -
                                          as.Date(paste(IPC_SOM_year_month$Year[13-i], IPC_SOM_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
    
    # reg_data_i$wheat_barley <- time_since_harvest(IPC_SOM_year_month$Month[14-i], "wheat_barley", SOM_harvest)
    reg_data_i <- as.matrix(reg_data_i[,-1])
    colnames(reg_data_i) <- reg_data_names
    lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
  }
  lagged_reg_data[is.na(lagged_reg_data)] <- 0
  
  result <- list()
  result$lagged_reg_data <- lagged_reg_data
  result$conflict_SOM_aggr <- conflict_SOM_aggr
  result$conflict_sub_SOM_aggr <- conflict_sub_SOM_aggr
  result$disaster_SOM_monthly_aggr <- disaster_SOM_monthly_aggr
  return(result)
} # function end
