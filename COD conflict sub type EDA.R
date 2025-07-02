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
  COD_map <- read_sf("Food Security/geoBoundaries-COD-ADM1.geojson") %>% 
    mutate(
      shapeName = stri_trans_general(shapeName, "Latin-ASCII"),
      shapeName = gsub("-", " ", shapeName),
      shapeName = gsub("Mai Ndombe", "Mai-Ndombe", shapeName),
      shapeName = gsub("Lower", "Bas", shapeName),
      shapeName = gsub("Upper", "Haut", shapeName),
      shapeName = gsub("North", "Nord", shapeName),
      shapeName = gsub("South", "Sud", shapeName),
      shapeName = gsub("Central Kasai", "Kasai Central", shapeName)
    )
  
  # https://data.humdata.org/dataset/cod-ab-cod
  COD_adm_codes <- read_xlsx("Food Security/COD adm2 boundaries.xlsx") %>% 
    mutate(
      ADM1_FR = stri_trans_general(ADM1_FR, "Latin-ASCII"),
      ADM1_FR = gsub("-", " ", ADM1_FR),
      ADM1_FR = gsub("Mai Ndombe", "Mai-Ndombe", ADM1_FR),
      ADM1_FR = gsub("Lower", "Bas", ADM1_FR),
      ADM1_FR = gsub("Upper", "Haut", ADM1_FR),
      ADM1_FR = gsub("North", "Nord", ADM1_FR),
      ADM1_FR = gsub("South", "Sud", ADM1_FR),
      ADM1_FR = gsub("Central Kasai", "Kasai Central", ADM1_FR)
    ) %>% 
    select(ADM1_FR, ADM1_PCODE) %>% unique %>% arrange(ADM1_FR)
    
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
  IPC_COD$Area[c(which(IPC_COD$Subarea == "Central Kasai"), which(IPC_COD$Subarea == "Tanganyka"))] <- c("Kasai Central", "Tanganyika")
  IPC_COD$Date %>% unique %>% sort # "2017-06-01" "2018-06-01" "2019-06-01" "2020-07-01" "2021-02-01" "2021-09-01" "2022-07-01" "2023-08-01"
  
  COD_map$shapeName %>% unique %>% sort
  IPC_COD$Area %>% unique %>% sort
  COD_adm_codes$ADM1_FR %>% unique %>% sort
  
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
  corn_seasons_COD <- tibble(month=1:12,
                             plant = c(rep(1,3), rep(0,2), rep(1,4), rep(0,3)),
                             grow = c(0,0, rep(1,3), 0,0, rep(1,3), 0,0),
                             harvest = c(rep(0,4), rep(1,3), 0,0, rep(1,3)),
                             none = c(1,0,0, rep(1,3), rep(0,5), 1))
  seasons <- c("plant", "grow", "harvest", "none")
  corn_seasons_COD$season <- apply(corn_seasons_COD, 1, function(x) paste(seasons[which(x[-c(1, 5)]==1)], collapse="/"))
  corn_seasons_COD$season <- ifelse(grepl("/", corn_seasons_COD$season), 
                                    substr(corn_seasons_COD$season, str_locate(corn_seasons_COD$season, "/")[,1] + 1, str_length(corn_seasons_COD$season)),
                                    corn_seasons_COD$season)
  corn_seasons_COD$season_south <- c(rep("grow",2), rep("harvest",3), rep("none",5), rep("plant",2))
  corn_seasons_COD <- corn_seasons_COD %>% select(month, season, season_south)
  gsub("/none", "", corn_seasons_COD$season) %>% unique # 5 unique seasons
  
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
  
  no_crop_areas_COD <- c("Mongala", "Tshuapa", "Kasai Oriental", "Kinshasa", "Ituri", "Sud Kivu")
  
  # https://data.humdata.org/dataset/cod-rainfall-subnational
  precipitation_COD <- read.csv("Food security/cod-rainfall-adm2-full.csv") %>% as_tibble %>% 
    mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
    filter(date >= as.Date("6/1/2016", "%m/%d/%Y") & date < as.Date("9/1/2023", "%m/%d/%Y")) %>% 
    mutate(ADM1_PCODE = substr(ADM2_PCODE, 1, 4))
  precipitation_COD <- precipitation_COD %>% 
    group_by(date, ADM1_PCODE) %>% 
    summarize(across(rfh:r3h_avg, function(x) sum(x))) %>% 
    mutate(year = year(date),
           month = month(date),
           year_month = paste(year, month, sep="_")) %>% 
    group_by(ADM1_PCODE, year, month, year_month) %>% 
    summarize(across(c(date, rfh:r3h_avg), function(x) rev(x)[1])) %>% 
    left_join(COD_adm_codes %>% rename(Area=ADM1_FR), by="ADM1_PCODE")
  
  rainfall_tbl <- tibble()
  for (adm1 in unique(precipitation_COD$Area)) {
    rainfall_tbl_area <- precipitation_COD %>% filter(Area == adm1) %>% select(r1h, r3h)
    rainfall_tbl_area <- rainfall_tbl_area
    rainfall_tbl_area$spi_1h <- spi(rainfall_tbl_area$r1h, 12)$fitted
    rainfall_tbl_area$spi_3h <- spi(rainfall_tbl_area$r3h, 12)$fitted
    rainfall_tbl <- bind_rows(rainfall_tbl, rainfall_tbl_area)
  }
  precipitation_COD <- left_join(precipitation_COD, rainfall_tbl %>% select(-r1h, -r3h), by=c("ADM1_PCODE", "year", "month"))
}
{
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
crop_seasons <- corn_seasons_COD
no_crop_areas <- no_crop_areas_COD
conflict_types <- conflict_COD %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type)
load("Food Security/lagged_reg_data_list_COD.RData")
}

conflict_COD$event_type %>% table
conflict_COD$sub_event_type %>% table

conflict_COD %>% ggplot +
  geom_bar(aes(x=event_type, fill=sub_event_type)) + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
conflict_COD %>% select(event_type, sub_event_type) %>% unique %>% arrange(event_type, sub_event_type) %>% print(n=24)


month_lag_ <- 4
# IPC vs. conflict
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t, y=IPC_diff)) +
  geom_point()

lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t, y=IPC_diff)) +
  geom_point()

# IPC vs. conflict_diff
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=`Phase_3+ratio_t`)) +
  geom_point()

# IPC_diff vs. conflict_diff
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Battles_explosions_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_Protests_Riots_t_diff, y=IPC_diff)) +
  geom_point()
lagged_reg_data_list_COD[[month_lag_]]$lagged_reg_data %>% 
  ggplot(aes(x=c_n_events_etc._t_diff, y=IPC_diff)) +
  geom_point()


# regression with filtered data
month_lag_ <- 4
IPC_COD_phase3_long_lagged <- IPC_COD_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(corn_seasons_COD, by="month") %>% 
  mutate(season = ifelse(Area == "Haut Katanga", season_south, season)) %>% select(-season_south) %>% 
  left_join(lagged_reg_data_list_COD[[month_lag_]]$conflict_NAT_aggr %>%
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
  left_join(lagged_reg_data_list_COD[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))

IPC_COD_phase3_long_lagged[is.na(IPC_COD_phase3_long_lagged)] <- 0
IPC_COD_phase3_long_lagged <- IPC_COD_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)

lm_conflict_COD <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged[,4:17])
lm_conflict_COD %>% summary
lm_conflict_diff_COD <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_COD %>% summary

IPC_COD_year_month
conflict_map_COD <- function(i) {
  COD_map %>% left_join(IPC_COD_phase3_long_lagged %>%
                          filter(year == IPC_COD_year_month$Year[i] & month == IPC_COD_year_month$Month[i]) %>%
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

conflict_map_COD(3)
conflict_map_COD(4)
conflict_map_COD(5)
conflict_map_COD(6)
conflict_map_COD(7)
conflict_map_COD(8)

IPC_COD_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% print(n=26)
COD_Area_ordered_by_n_events <- IPC_COD_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% pull(Area)

lm_conflict_COD_high_violence <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>%
     filter(Area %in% COD_Area_ordered_by_n_events[1:6]) %>% 
     select(-(Area:month), -(n_events_Battles_explosions_diff:fatalities_etc._diff)))
lm_conflict_diff_COD_high_violence <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>% 
     filter(Area %in% COD_Area_ordered_by_n_events[1:6]) %>% 
     select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_COD_high_violence %>% summary
lm_conflict_diff_COD_high_violence %>% summary

COD_high_violence_index <- which(names(lm_conflict_COD$residuals) %>% as.numeric %in% which(IPC_COD_phase3_long_lagged$Area %in% COD_Area_ordered_by_n_events[1:6]))
lm_conflict_COD$residuals[COD_high_violence_index] %>% summary
mean((lm_conflict_COD$residuals)^2)
mean((lm_conflict_COD$residuals[COD_high_violence_index])^2) # 0.006060401
lm_conflict_COD_y_bar <- mean(lm_conflict_COD$model$Phase_3above_ratio[COD_high_violence_index])
1-sum((lm_conflict_COD$residuals[COD_high_violence_index])^2)/sum((lm_conflict_COD$model$Phase_3above_ratio[COD_high_violence_index]-lm_conflict_COD_y_bar)^2) # R^2: 0.4982445
lm_conflict_diff_COD$residuals[COD_high_violence_index] %>% summary
mean((lm_conflict_diff_COD$residuals)^2)
mean((lm_conflict_diff_COD$residuals[COD_high_violence_index])^2) # 0.005902252
lm_conflict_COD_high_violence$residuals %>% summary
lm_conflict_diff_COD_high_violence$residuals %>% summary
mean((lm_conflict_COD_high_violence$residuals)^2) # 0.004088291
mean((lm_conflict_diff_COD_high_violence$residuals)^2) # 0.004071313

ggplot(data.frame(x=1:length(COD_high_violence_index), residuals=lm_conflict_COD$residuals[COD_high_violence_index])) +
  geom_point(aes(x=x, y=residuals)) + ylim(-0.2, 0.35) + ggtitle("Residuals in highly conflicted areas of global model (COD)") +
  geom_abline(slope=0, intercept=mean((lm_conflict_COD$residuals[COD_high_violence_index])^2))

ggplot(data.frame(x=1:length(COD_high_violence_index), residuals=lm_conflict_COD_high_violence$residuals)) +
  geom_point(aes(x=x, y=residuals)) + ylim(-0.2, 0.35) + ggtitle("Residuals of local model (COD)") +
  geom_abline(slope=0, intercept=mean((lm_conflict_COD_high_violence$residuals)^2))

## regression with rainfall data
IPC_COD_phase3_long_lagged_rainfall <- left_join(IPC_COD_phase3_long_lagged, precipitation_COD, by=c("Area", "year", "month"))
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
            spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
     select(-spi_1h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
     mutate(spi_1h_class = ifelse(spi_1h > 1, "wet",
                                  ifelse(spi_1h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_1h)) %>% summary

lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
            spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
     select(-spi_3h)) %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
     mutate(spi_3h_class = ifelse(spi_3h > 1, "wet",
                                  ifelse(spi_3h < -1, "dry", "normal")) %>%
              factor(levels=c("normal", "wet", "dry"))
     ) %>%
     select(-spi_3h)) %>% summary 

lm(Phase_3above_ratio~.+season*r3h, data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, r3h)) %>% summary
lm(Phase_3above_ratio~.+season*spi_3h, data=IPC_COD_phase3_long_lagged_rainfall %>%
     select(Phase_3above_ratio:n_disaster2, spi_3h)) %>% summary

lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.))) %>% summary

# regression with filtered data (Battles = Armed clash only)
IPC_COD_phase3_long_lagged <- IPC_COD_provinces_long %>% 
  mutate(year = as.character(Year) %>% as.numeric,
         month = as.character(Month) %>% as.numeric) %>% 
  ungroup(Year) %>% 
  select(Area, year, month, Phase_3above_ratio) %>% 
  left_join(lagged_reg_data_list_COD[[month_lag_]]$conflict_sub_NAT_aggr %>%
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
  left_join(corn_seasons_COD, by="month") %>% 
  mutate(season = ifelse(Area == "Haut Katanga", season_south, season) %>% as.factor) %>% select(-season_south) %>% 
  filter(!is.na(Phase_3above_ratio)) %>% 
  pivot_wider(names_from = event_type, values_from = c("n_events", "fatalities")) %>% 
  left_join(lagged_reg_data_list_COD[[month_lag_]]$disaster_NAT_monthly_aggr %>% select(-year_month), by=c("Area", "year", "month"))
IPC_COD_phase3_long_lagged[is.na(IPC_COD_phase3_long_lagged)] <- 0
IPC_COD_phase3_long_lagged <- IPC_COD_phase3_long_lagged %>% 
  mutate(IPC_t_1 = lag(Phase_3above_ratio),
         IPC_t_2 = lag(Phase_3above_ratio, 2),
         across(n_events_Battles_explosions:fatalities_etc., function(x) x - lag(x), .names="{col}_diff")) %>% 
  relocate(Area, year, month, Phase_3above_ratio, IPC_t_1) %>% 
  ungroup(Area)
IPC_COD_phase3_long_lagged[is.na(IPC_COD_phase3_long_lagged)] <- 0

lm_conflict_COD <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged[,4:17]) # without IPC_t_2
lm_conflict_COD %>% summary
lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged[,4:18]) %>% summary # with IPC_t_2 (insignificant)
lm_conflict_diff_COD <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>% select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_diff_COD %>% summary

IPC_COD_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% print(n=26)
COD_Area_ordered_by_n_events <- IPC_COD_phase3_long_lagged %>% group_by(Area) %>% summarize(across(n_events_Battles_explosions:n_events_etc., function(x) sum(x))) %>%
  arrange(desc(n_events_Battles_explosions)) %>% pull(Area)

lm_conflict_COD_high_violence <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>%
                                      filter(Area %in% COD_Area_ordered_by_n_events[1:6]) %>% 
                                      select(-(Area:month), -(n_events_Battles_explosions_diff:fatalities_etc._diff)))
lm_conflict_diff_COD_high_violence <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged %>% 
                                           filter(Area %in% COD_Area_ordered_by_n_events[1:6]) %>% 
                                           select(-(Area:month), -(n_events_Battles_explosions:fatalities_etc.)))
lm_conflict_COD_high_violence %>% summary
lm_conflict_diff_COD_high_violence %>% summary

lm_conflict_COD$residuals[(names(lm_conflict_COD$residuals) %>% as.numeric) %in% which(IPC_COD_phase3_long_lagged$Area %in% COD_Area_ordered_by_n_events)] %>% summary
lm_conflict_diff_COD$residuals[(names(lm_conflict_diff_COD$residuals) %>% as.numeric) %in% which(IPC_COD_phase3_long_lagged$Area %in% COD_Area_ordered_by_n_events)] %>% summary
lm_conflict_COD_high_violence$residuals %>% summary
lm_conflict_diff_COD_high_violence$residuals %>% summary

###
lagged_months <- 1
disaster1 <- "Flood"; disaster2 <- "Epidemic"
disaster_COD_monthly_aggr_by_type <- disaster_COD %>% 
  filter(year > oldest_year - 1) %>% 
  filter(!(year == latest_year & month > latest_month)) %>% 
  filter(!(year == oldest_year & month < oldest_month)) %>%  
  group_by(year, month, `Disaster Type`) %>% 
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T)) %>% 
  rename(type=`Disaster Type`) %>% 
  mutate(year_month = paste(year , month, sep="_")) %>% 
  mutate(year=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>%
  arrange(year, month)

disaster_COD_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=type, color=type))

disaster_COD_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=type, color=type))

disaster_COD_monthly_aggr_by_type %>% 
  ggplot() +
  geom_line(aes(x=year, y=n_disasters, group=type, color=type))

disaster_COD_monthly_aggr_by_Area <- disaster_COD %>% 
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

disaster_COD_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=affected, group=Area, color=Area))

disaster_COD_monthly_aggr_by_Area %>% 
  ggplot() +
  geom_line(aes(x=year, y=deaths, group=Area, color=Area))


##
lagged_reg_data_list_COD$months_1$disaster_NAT_monthly_aggr


####
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
    summarize(across(n_disasters:n_disaster2, function(x) sum(x))) %>% 
    mutate(year_month = year_month_i,
           year = year_i,
           month = month_i)
}

disaster_COD_monthly_aggr <- lagged_disaster[[IPC_COD_year_month$year_month[1]]][1,]
for (tbl in lagged_disaster) {
  disaster_COD_monthly_aggr <- bind_rows(disaster_COD_monthly_aggr, tbl)
}
disaster_COD_monthly_aggr <- disaster_COD_monthly_aggr[-1,]

result$disaster_COD_monthly_aggr <- disaster_COD_monthly_aggr

disaster_COD_monthly_events <- disaster_COD_monthly_aggr %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_disasters=sum(n_disasters)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
disaster_COD_monthly_events[is.na(disaster_COD_monthly_events)] <- 0

disaster_COD_monthly_disaster1 <- disaster_COD_monthly_aggr %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_disaster1=sum(n_disaster1)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_disaster1_", names_from = c(year, month), values_from = n_disaster1)
disaster_COD_monthly_disaster1[is.na(disaster_COD_monthly_disaster1)] <- 0

disaster_COD_monthly_disaster2 <- disaster_COD_monthly_aggr %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_disaster2=sum(n_disaster2)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_disaster2_", names_from = c(year, month), values_from = n_disaster2)
disaster_COD_monthly_disaster2[is.na(disaster_COD_monthly_disaster2)] <- 0

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