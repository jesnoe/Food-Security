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
    mutate(across(Area:Subarea, function(x) gsub("Sari pul", "Sar-e Pol", x))) %>%
    mutate(across(Area:Subarea, function(x) gsub("Sar-e Pol", "Sar_e_Pol", x))) %>% 
    mutate(across(Area:Subarea, function(x) gsub(" Urban", "", x)))
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
}

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

IPC_Afg_provinces[,-1] %>% apply(2, function(x) sum(is.na(x))) # 30 missing in 05-0217 and 01-2018

IPC_Afg %>% filter(Year == 2017 & Month == 5)

conflict_Afg$event_type %>% table # 6 types
conflict_Afg$sub_event_type %>% table # 24 types
conflict_Afg %>% filter(event_type == "Protests") %>% pull(sub_event_type) %>% table
conflict_Afg %>% filter(event_type == "Strategic developments") %>% pull(sub_event_type) %>% table
conflict_Afg %>% filter(event_type == "Violence against civilians") %>% pull(sub_event_type) %>% table

conflict_Afg_aggr <- conflict_Afg %>% 
  filter(!(year == 2024 & month > 3)) %>% 
  filter(!(year == 2017 & month < 3)) %>% 
  group_by(year, month, admin1, event_type) %>% 
  summarise(n_events=n(),
            fatalities=sum(fatalities, na.rm=T)) %>% 
  rename(Area=admin1) %>% 
  arrange(year, month)

IPC_Afg_year_month <- IPC_Afg %>% select(Year, Month) %>% unique %>%
  mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric)) %>% arrange(Year, Month)
prev_year <- 2017; prev_month <- 3
for (i in 2:nrow(IPC_Afg_year_month)) {
  year_i <- IPC_Afg_year_month$Year[i]
  month_i <- IPC_Afg_year_month$Month[i]
  prev_months_index <- which(conflict_Afg_aggr$year == prev_year & conflict_Afg_aggr$month == prev_month)
  prev_months_index <- prev_months_index[length(prev_months_index)]
  months_index_i <- which(conflict_Afg_aggr$year == year_i & conflict_Afg_aggr$month == month_i)[1]
  if (year_i != prev_year) conflict_Afg_aggr$year[(prev_months_index+1):months_index_i] <- year_i
  conflict_Afg_aggr$month[(prev_months_index+1):months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}

conflict_Afg$event_type %>% table # 6 types
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

disaster_Afg$`Disaster Type` %>% table
disaster_Afg_monthly_aggr <- disaster_Afg %>% 
  filter(!(year >= 2024 & month > 3)) %>%
  filter(year > 2016) %>% 
  filter(!(year == 2017 & month < 3)) %>% 
  group_by(Region, year, month) %>% 
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T),
            n_floods=sum(`Disaster Type` == "Flood"),
            n_droughts=sum(`Disaster Type` == "Drought")) %>% 
  rename(Area=Region) %>% 
  mutate(year_month=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>% 
  arrange(year, month)

prev_year <- 2017; prev_month <- 3
for (i in 2:nrow(IPC_Afg_year_month)) {
  prev_year_month_i <- as.Date(paste(prev_month, prev_year, "01"), format="%m %Y %d")
  year_i <- IPC_Afg_year_month$Year[i]
  month_i <- IPC_Afg_year_month$Month[i]
  year_month_i <- as.Date(paste(month_i, year_i, "01"), format="%m %Y %d")
  months_index_i <- which(disaster_Afg_monthly_aggr$year_month > prev_year_month_i & disaster_Afg_monthly_aggr$year_month <= year_month_i)
  disaster_Afg_monthly_aggr$year[months_index_i] <- year_i
  disaster_Afg_monthly_aggr$month[months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}

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


# regression
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

IPC_ncol <- ncol(IPC_Afg_provinces)
conflict_ncol <- ncol(conflict_Afg_n_events) # 84
conflict_fatalities_ncol <- ncol(conflict_Afg_fatalities) # 84
disaster_ncol <- ncol(disaster_Afg_monthly_events) # 13
disaster_affected_ncol <- ncol(disaster_Afg_monthly_affected) # 13
names(conflict_Afg_n_events)[-1] <- paste0("c_n_events_", names(conflict_Afg_n_events)[-1])
names(conflict_Afg_fatalities)[-1] <- paste0("log_fatal_", names(conflict_Afg_fatalities)[-1])
reg_data_i <- IPC_Afg_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
  left_join(conflict_Afg_n_events[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>%
  left_join(conflict_Afg_fatalities[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>% 
  left_join(disaster_Afg_monthly_events[,c(1, disaster_ncol:(disaster_ncol-2))], by="Area") %>% 
  left_join(disaster_Afg_monthly_affected[,c(1, disaster_ncol:(disaster_ncol-2))], by="Area") %>%
  left_join(disaster_Afg_monthly_deaths[,c(1, disaster_ncol:(disaster_ncol-2))], by="Area")
names(reg_data_i) <- gsub("2024_3", "t", names(reg_data_i))
names(reg_data_i) <- gsub("2023_10", "t_1", names(reg_data_i))
names(reg_data_i) <- gsub("2023_4", "t_2", names(reg_data_i))
names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
lagged_reg_data <- reg_data_i[,-1]
lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_Afg_year_month$Year[14], IPC_Afg_year_month$Month[14], 1), format="%Y %m %d") -
                                           as.Date(paste(IPC_Afg_year_month$Year[13], IPC_Afg_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
lagged_reg_data$wheat_barley <- time_since_harvest(IPC_Afg_year_month$Month[14], "wheat_barley", AFG_harvest)
# lagged_reg_data$corn_rice <- time_since_harvest(IPC_Afg_year_month$Month[14], "corn_rice", AFG_harvest)
reg_data_names <- names(lagged_reg_data)
for (i in 1:9) {
  IPC_col_index <- IPC_ncol - i
  conflict_col_index <- conflict_ncol - 6*i
  disaster_col_index <- disaster_ncol - i
  reg_data_i <- IPC_Afg_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
    left_join(conflict_Afg_n_events[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>%
    left_join(conflict_Afg_fatalities[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>% 
    left_join(disaster_Afg_monthly_events[,c(1, disaster_col_index:(disaster_col_index-2))], by="Area") %>% 
    left_join(disaster_Afg_monthly_affected[,c(1, disaster_col_index:(disaster_col_index-2))], by="Area") %>% 
    left_join(disaster_Afg_monthly_deaths[,c(1, disaster_col_index:(disaster_col_index-2))], by="Area")
  reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_Afg_year_month$Year[14-i], IPC_Afg_year_month$Month[14-i], 1), format="%Y %m %d") -
                                        as.Date(paste(IPC_Afg_year_month$Year[13-i], IPC_Afg_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
  
  reg_data_i$wheat_barley <- time_since_harvest(IPC_Afg_year_month$Month[14-i], "wheat_barley", AFG_harvest)
  # reg_data_i$corn_rice <- time_since_harvest(IPC_Afg_year_month$Month[14-i], "corn_rice", AFG_harvest)
  reg_data_i <- as.matrix(reg_data_i[,-1])
  colnames(reg_data_i) <- reg_data_names
  lagged_reg_data <- bind_rows(lagged_reg_data, reg_data_i %>% as_tibble)
}
lagged_reg_data[is.na(lagged_reg_data)] <- 0
lagged_reg_data %>% apply(2, function(x) table(x) %>% length) %>% sort
lagged_reg_data %>% str

lagged_reg_data_corr <- cor(lagged_reg_data) %>% melt
lagged_reg_data_corr %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                       values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, 1)),
                       limits=c(-1, 1)) +
  labs(title="Correlations of AFG data") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Factor Analysis
eigen_reg_data <- eigen(cor(lagged_reg_data[,-1]))
eigen_reg_data$values
fa_IPC_t <- fa(lagged_reg_data[,-1], nfactors = 10, rotate = "varimax")
fa_IPC_t$loadings

conflict_reg_data <- lagged_reg_data %>% select(c_n_events_Riots_t:log_fatal_Battles_t_1)
eigen_conflict <- eigen(cor(conflict_reg_data))
eigen_conflict$values
fa_conflict <- fa(conflict_reg_data, nfactors = 6, rotate = "varimax")
fa_conflict$loadings

disaster_reg_data <- lagged_reg_data %>% select(n_disasters_t:log_deaths_t_2)
eigen_disaster <- eigen(cor(disaster_reg_data))
eigen_disaster$values
fa_disaster <- fa(disaster_reg_data, nfactors = 3, rotate = "varimax")
fa_disaster$loadings

conflict_disaster_reg_data <- lagged_reg_data %>% select(c_n_events_Riots_t:log_deaths_t_2)
eigen_conflict_disaster <- eigen(cor(conflict_disaster_reg_data))
eigen_conflict_disaster$values
fa_conflict_disaster <- fa(conflict_disaster_reg_data, nfactors = 9, rotate = "varimax")
fa_conflict_disaster$loadings
fa_conflict_disaster$scores

# Factor regression
conflict_factor_scores <- fa_conflict$scores
colnames(conflict_factor_scores) <- paste0("conflict_", colnames(conflict_factor_scores))
disaster_factor_scores <- fa_disaster$scores
colnames(disaster_factor_scores) <- paste0("disaster_", colnames(disaster_factor_scores))

conflict_disaster_factors_separate <- lagged_reg_data %>%
  select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, month_diff, wheat_barley) %>%
  bind_cols(conflict_factor_scores) %>% 
  bind_cols(disaster_factor_scores)

conflict_disaster_factors_together <- lagged_reg_data %>%
  select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, month_diff, wheat_barley) %>%
  bind_cols(fa_conflict_disaster$scores) # %>% 
  # relocate(`Phase_3+ratio_t`:wheat_barley, MR1:MR9)

lm(`Phase_3+ratio_t`~., data=lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~., data=conflict_disaster_factors_separate) %>% summary()
lm(`Phase_3+ratio_t`~., data=conflict_disaster_factors_together) %>% summary()

lm(`Phase_3+ratio_t`~.-month_diff, data=lagged_reg_data) %>% summary()

lm(`Phase_3+ratio_t`~., data=lagged_reg_data[1:306,]) %>% summary() # regression without 2019-09
lm(`Phase_3+ratio_t`~., data=lagged_reg_data[1:272,]) %>% summary() # regression without 2019-09 and 2020-04

lm(c_n_events_Riots_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Riots_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(c_n_events_Violence_against_civilians_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Violence_against_civilians_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(c_n_events_Strategic_developments_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Protests_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Explosions_Remote_violence_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Battles_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Riots_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Riots_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(log_fatal_Violence_against_civilians_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Violence_against_civilians_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(log_fatal_Strategic_developments_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Protests_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Explosions_Remote_violence_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Explosions_Remote_violence_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(log_fatal_Battles_t~., data=lagged_reg_data) %>% summary()
lm(n_disasters_t~., data=lagged_reg_data) %>% summary()
lm(n_disasters_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(affected_t~., data=lagged_reg_data) %>% summary()
lm(affected_t~.-month_diff, data=lagged_reg_data) %>% summary()

# LASSO
cv.glmnet(lagged_reg_data %>% select(-`Phase_3+ratio_t`) %>% as.matrix,
          lagged_reg_data$`Phase_3+ratio_t`,
          alpha=1)
IPC_t_lasso <- glmnet(lagged_reg_data %>% select(-`Phase_3+ratio_t`),
                      lagged_reg_data$`Phase_3+ratio_t`,
                      lambda=0.002203,
                      alpha=1)
IPC_t_lasso$beta
glmnet(`Phase_3+ratio_t`~.-month_diff, data=lagged_reg_data) %>% summary()
lm(c_n_events_Riots_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Riots_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(c_n_events_Violence_against_civilians_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Violence_against_civilians_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(c_n_events_Strategic_developments_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Protests_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Explosions_Remote_violence_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Battles_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Riots_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Riots_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(log_fatal_Violence_against_civilians_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Violence_against_civilians_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(log_fatal_Strategic_developments_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Protests_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Explosions_Remote_violence_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Battles_t~., data=lagged_reg_data) %>% summary()
lm(n_disasters_t~., data=lagged_reg_data) %>% summary()
lm(n_disasters_t~.-month_diff, data=lagged_reg_data) %>% summary()
lm(affected_t~., data=lagged_reg_data) %>% summary()
lm(affected_t~.-month_diff, data=lagged_reg_data) %>% summary()

# try without May 2017 data? High conflict during harvest season

lagged_reg_data %>% ggplot() +
  geom_point(aes(x=wheat_barley, y=`Phase_3+ratio_t`))


## lagged reg data with flood and drought
IPC_ncol <- ncol(IPC_Afg_provinces)
conflict_ncol <- ncol(conflict_Afg_n_events) # 84
conflict_fatalities_ncol <- ncol(conflict_Afg_fatalities) # 84
disaster_ncol <- ncol(disaster_Afg_monthly_events) # 13
disaster_affected_ncol <- ncol(disaster_Afg_monthly_affected) # 13
# names(conflict_Afg_n_events)[-1] <- paste0("c_n_events_", names(conflict_Afg_n_events)[-1])
# names(conflict_Afg_fatalities)[-1] <- paste0("log_fatal_", names(conflict_Afg_fatalities)[-1])
reg_data_flood_drought_i <- IPC_Afg_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
  left_join(conflict_Afg_n_events[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>%
  left_join(conflict_Afg_fatalities[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>% 
  left_join(disaster_Afg_monthly_flood[,c(1, disaster_ncol:(disaster_ncol-2))], by="Area") %>% 
  left_join(disaster_Afg_monthly_drought[,c(1, disaster_ncol:(disaster_ncol-2))], by="Area")
names(reg_data_flood_drought_i) <- gsub("2024_3", "t", names(reg_data_flood_drought_i))
names(reg_data_flood_drought_i) <- gsub("2023_10", "t_1", names(reg_data_flood_drought_i))
names(reg_data_flood_drought_i) <- gsub("2023_4", "t_2", names(reg_data_flood_drought_i))
names(reg_data_flood_drought_i) <- gsub("[/ ]", "_", names(reg_data_flood_drought_i))
lagged_reg_data_flood_drought <- reg_data_flood_drought_i[,-1]
lagged_reg_data_flood_drought$month_diff <- as.numeric(as.Date(paste(IPC_Afg_year_month$Year[14], IPC_Afg_year_month$Month[14], 1), format="%Y %m %d") -
                                                         as.Date(paste(IPC_Afg_year_month$Year[13], IPC_Afg_year_month$Month[13], 1), format="%Y %m %d")) %/% 30
lagged_reg_data_flood_drought$wheat_barley <- time_since_harvest(IPC_Afg_year_month$Month[14], "wheat_barley", AFG_harvest)
reg_data_flood_drought_names <- names(lagged_reg_data_flood_drought)
for (i in 1:9) {
  IPC_col_index <- IPC_ncol - i
  conflict_col_index <- conflict_ncol - 6*i
  disaster_col_index <- disaster_ncol - i
  reg_data_flood_drought_i <- IPC_Afg_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
    left_join(conflict_Afg_n_events[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>%
    left_join(conflict_Afg_fatalities[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>% 
    left_join(disaster_Afg_monthly_flood[,c(1, disaster_col_index:(disaster_col_index-2))], by="Area") %>% 
    left_join(disaster_Afg_monthly_drought[,c(1, disaster_col_index:(disaster_col_index-2))], by="Area")
  
  reg_data_flood_drought_i$month_diff <- as.numeric(as.Date(paste(IPC_Afg_year_month$Year[14-i], IPC_Afg_year_month$Month[14-i], 1), format="%Y %m %d") -
                                                      as.Date(paste(IPC_Afg_year_month$Year[13-i], IPC_Afg_year_month$Month[13-i], 1), format="%Y %m %d")) %/% 30
  
  reg_data_flood_drought_i$wheat_barley <- time_since_harvest(IPC_Afg_year_month$Month[14-i], "wheat_barley", AFG_harvest)
  reg_data_flood_drought_i <- as.matrix(reg_data_flood_drought_i[,-1])
  colnames(reg_data_flood_drought_i) <- reg_data_flood_drought_names
  lagged_reg_data_flood_drought <- bind_rows(lagged_reg_data_flood_drought, reg_data_flood_drought_i %>% as_tibble)
}
lagged_reg_data_flood_drought[is.na(lagged_reg_data_flood_drought)] <- 0
lagged_reg_data_flood_drought


# -month_diff
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_flood_drought) %>% summary()
lm(`Phase_3+ratio_t`~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()

lm(`Phase_3+ratio_t`~., data=lagged_reg_data_flood_drought[1:306,]) %>% summary() # regression without 2019-09
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_flood_drought[1:272,]) %>% summary() # regression without 2019-09 and 2020-04

lm(c_n_events_Riots_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Riots_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Violence_against_civilians_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Violence_against_civilians_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Strategic_developments_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Protests_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Explosions_Remote_violence_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(c_n_events_Battles_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Riots_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Riots_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Violence_against_civilians_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Violence_against_civilians_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Strategic_developments_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Protests_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Explosions_Remote_violence_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Explosions_Remote_violence_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()
lm(log_fatal_Battles_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(n_disasters_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(n_disasters_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()
lm(affected_t~., data=lagged_reg_data_flood_drought) %>% summary()
lm(affected_t~.-month_diff, data=lagged_reg_data_flood_drought) %>% summary()


model1 <- lm(`Phase_3+ratio_t`~., data=lagged_reg_data_flood_drought) <- lm(`Phase_3+ratio_t`~., data=lagged_reg_data_flood_drought)
model1$residuals
data.frame(index=1:340, res=model1$residuals) %>% 
  ggplot() +
  geom_point(aes(x=index, y=res)) +
  geom_vline(xintercept=34*(1:9))
