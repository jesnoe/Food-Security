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
  SDN_map <- read_sf("Food Security/geoBoundaries-SDN-ADM1.geojson")
  SDN_map$shapeName[which(SDN_map$shapeName == "Abyei PCA")] <- "Abyei"
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
  
  IPC_SDN <- IPC_AF %>% filter(Country == "Sudan") # 1057, 18 states
  IPC_SDN$Area[which(IPC_SDN$Area == "Aj Jazirah")] <- "Gezira"
  
  FSI_SDN <- FSI %>% filter(Country == "Sudan")
  disaster_SDN <- disaster %>% filter(Country == "Sudan") %>%  # 318
    rename(year=`Start Year`, month=`Start Month`)
  disaster_SDN_separate_locations <- read.csv("Food Security/Disaster Sudan locations.csv") %>% as_tibble
  # disaster_SDN_separate_locations <- disaster_SDN_separate_locations %>% 
  #   mutate(Region = gsub("Badakhshān", "Badakhshan", Region))
  
  disaster_SDN <- disaster_SDN_separate_locations %>%
    select(DisNo., lat, long, Subregion, Region) %>% 
    left_join(disaster_SDN %>%
                select(-Region, -Subregion) %>% 
                rename(event_lat=Latitude, event_long=Longitude), by="DisNo.")
  disaster_SDN$Region[which(disaster_SDN$Region == "El Gezira")] <- "Gezira"
  disaster_SDN$Region[which(disaster_SDN$Region == "Nile")] <- "River Nile"
  
  conflict_SDN <- conflict_AF %>% filter(country == "Sudan") # 66,500 # much more than CAR
  conflict_SDN$event_date <- as.Date(conflict_SDN$event_date, format="%m/%d/%Y")
  conflict_SDN$month <- month(conflict_SDN$event_date) %>% as.factor
  conflict_SDN <- conflict_SDN %>% relocate(event_id_cnty, event_date, year, month)
  conflict_SDN$admin1[which(conflict_SDN$admin1 == "Al Jazirah")] <- "Gezira"
}


IPC_SDN <- IPC_SDN %>% 
  mutate(Area = ifelse(is.na(Area) & Subarea %in% SDN_map$shapeName, Subarea, Area))
IPC_SDN_year_month <- IPC_SDN %>% select(Year, Month) %>% unique %>%
  mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric)) %>% arrange(Year, Month)
IPC_SDN_provinces <- IPC_SDN %>%
  filter((Year == 2019 & Month == 6) | (Year == 2020 & Month == 6) | (Year == 2022 & Month == 5)) %>% 
  group_by(Area, Year, Month) %>% 
  summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
  mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))

IPC_SDN_provinces <- IPC_SDN_provinces %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
  pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio)

conflict_SDN$event_type %>% table # 6 types
conflict_SDN$sub_event_type %>% table # 24 types
conflict_SDN %>% filter(event_type == "Protests") %>% pull(sub_event_type) %>% table
conflict_SDN %>% filter(event_type == "Strategic developments") %>% pull(sub_event_type) %>% table
conflict_SDN %>% filter(event_type == "Violence against civilians") %>% pull(sub_event_type) %>% table

conflict_SDN_aggr <- conflict_SDN %>%
  mutate(month=as.character(month) %>% as.numeric) %>% 
  filter(year > 2018) %>% 
  filter(year < 2023) %>% 
  filter(!(year == 2022 & month > 5)) %>% 
  filter(!(year == 2019 & month < 6)) %>% 
  group_by(year, month, admin1, event_type) %>% 
  summarise(n_events=n(),
            fatalities=sum(fatalities, na.rm=T)) %>% 
  rename(Area=admin1) %>% 
  arrange(year, month)

IPC_SDN_year_month <- IPC_SDN_year_month[c(1,2,4),]
prev_year <- 2019; prev_month <- 6
for (i in 2:nrow(IPC_SDN_year_month)) {
  year_i <- IPC_SDN_year_month$Year[i]
  month_i <- IPC_SDN_year_month$Month[i]
  prev_months_index <- which(conflict_SDN_aggr$year == prev_year & conflict_SDN_aggr$month == prev_month)
  prev_months_index <- prev_months_index[length(prev_months_index)]
  months_index_i <- which(conflict_SDN_aggr$year == year_i & conflict_SDN_aggr$month == month_i)[1]
  if (year_i != prev_year) conflict_SDN_aggr$year[(prev_months_index+1):months_index_i] <- year_i
  conflict_SDN_aggr$month[(prev_months_index+1):months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}

conflict_SDN$event_type %>% table # 6 types
conflict_SDN_n_events <- conflict_SDN_aggr %>% 
  group_by(year, month, Area, event_type) %>% 
  summarise(n_events=sum(n_events)) %>% 
  pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
conflict_SDN_n_events$`c_n_events_Explosions/Remote violence_2019_6` <- 0
conflict_SDN_n_events[is.na(conflict_SDN_n_events)] <- 0
conflict_SDN_n_events <- relocate

conflict_SDN_fatalities <- conflict_SDN_aggr %>% 
  group_by(year, month, Area, event_type) %>% 
  summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
  pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
conflict_SDN_fatalities[is.na(conflict_SDN_fatalities)] <- 0

disaster_SDN$`Disaster Type` %>% table
disaster_SDN_monthly_aggr <- disaster_SDN %>% 
  filter(year > 2018) %>% 
  filter(!(year == 2024 & month > 4)) %>% 
  filter(!(year == 2019 & month < 6)) %>%  
  group_by(Region, year, month) %>% 
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T),
            deaths=sum(`Total Deaths`, na.rm=T),
            n_floods=sum(`Disaster Type` == "Flood"),
            n_droughts=sum(`Disaster Type` == "Drought")) %>% 
  rename(Area=Region) %>% 
  mutate(year_month=as.Date(paste(month, year, "01"), format="%m %Y %d")) %>% 
  arrange(year, month)

prev_year <- 2019; prev_month <- 6
for (i in 2:nrow(IPC_SDN_year_month)) {
  prev_year_month_i <- as.Date(paste(prev_month, prev_year, "01"), format="%m %Y %d")
  year_i <- IPC_SDN_year_month$Year[i]
  month_i <- IPC_SDN_year_month$Month[i]
  year_month_i <- as.Date(paste(month_i, year_i, "01"), format="%m %Y %d")
  months_index_i <- which(disaster_SDN_monthly_aggr$year_month > prev_year_month_i & disaster_SDN_monthly_aggr$year_month <= year_month_i)
  disaster_SDN_monthly_aggr$year[months_index_i] <- year_i
  disaster_SDN_monthly_aggr$month[months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}

disaster_SDN_monthly_events <- disaster_SDN_monthly_aggr %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_disasters=sum(n_disasters)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_disasters_", names_from = c(year, month), values_from = n_disasters)
disaster_SDN_monthly_events[is.na(disaster_SDN_monthly_events)] <- 0

disaster_SDN_monthly_flood <- disaster_SDN_monthly_aggr %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_floods=sum(n_floods)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_floods_", names_from = c(year, month), values_from = n_floods)
disaster_SDN_monthly_flood[is.na(disaster_SDN_monthly_flood)] <- 0

disaster_SDN_monthly_drought <- disaster_SDN_monthly_aggr %>% 
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(n_droughts=sum(n_droughts)) %>% 
  pivot_wider(id_cols=Area, names_prefix = "n_droughts_", names_from = c(year, month), values_from = n_droughts)
disaster_SDN_monthly_drought[is.na(disaster_SDN_monthly_drought)] <- 0

disaster_SDN_monthly_affected <- disaster_SDN_monthly_aggr %>%
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
  pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
disaster_SDN_monthly_affected[is.na(disaster_SDN_monthly_affected)] <- 0

disaster_SDN_monthly_deaths <- disaster_SDN_monthly_aggr %>%
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(log_deaths=log(1+sum(deaths, na.rm=T))) %>% 
  pivot_wider(id_cols=Area, names_prefix = "log_deaths_", names_from = c(year, month), values_from = log_deaths)
disaster_SDN_monthly_deaths[is.na(disaster_SDN_monthly_deaths)] <- 0


# regression
SDN_sorghum <- 11:12
SDN_peanut_millet <- c(1, 9:12)

time_since_harvest <- function(month., crop, country_harvest_season) {
  if (crop == "sorghum") {
    result <- ifelse(month. %in% 11:12, 0, ifelse(month. > 5, 11 - month., month.))
  }
  if (crop == "peanut_millet") {
    result <- ifelse(month. %in% 11:12, 0,
                     ifelse(month. > 5, 9-month., month.-1))
  }
  return(result)
}
SDN_harvest <- list(sorghum=SDN_sorghum,
                    peanut_millet=SDN_peanut_millet)

IPC_ncol <- ncol(IPC_SDN_provinces)
conflict_ncol <- ncol(conflict_SDN_n_events) # 18
conflict_fatalities_ncol <- ncol(conflict_SDN_fatalities) # 18
disaster_ncol <- ncol(disaster_SDN_monthly_events) # 4
disaster_affected_ncol <- ncol(disaster_SDN_monthly_affected) # 4
names(conflict_SDN_n_events)[-1] <- paste0("c_n_events_", names(conflict_SDN_n_events)[-1])
names(conflict_SDN_fatalities)[-1] <- paste0("log_fatal_", names(conflict_SDN_fatalities)[-1])
reg_data_i <- IPC_SDN_provinces[,c(1, IPC_ncol:(IPC_ncol-1))] %>% 
  left_join(conflict_SDN_n_events[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>%
  left_join(conflict_SDN_fatalities[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>% 
  left_join(disaster_SDN_monthly_events[,c(1, disaster_ncol:(disaster_ncol-1))], by="Area") %>%
  left_join(disaster_SDN_monthly_affected[,c(1, disaster_ncol:(disaster_ncol-1))], by="Area") %>%
  left_join(disaster_SDN_monthly_deaths[,c(1, disaster_ncol:(disaster_ncol-1))], by="Area")
names(reg_data_i) <- gsub("2022_5", "t", names(reg_data_i))
names(reg_data_i) <- gsub("2020_6", "t_1", names(reg_data_i))
names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
lagged_reg_data <- reg_data_i[,-1]
lagged_reg_data$month_diff <- as.numeric(as.Date(paste(IPC_SDN_year_month$Year[3], IPC_SDN_year_month$Month[3], 1), format="%Y %m %d") -
                                           as.Date(paste(IPC_SDN_year_month$Year[2], IPC_SDN_year_month$Month[2], 1), format="%Y %m %d")) %/% 30
lagged_reg_data$sorghum <- time_since_harvest(IPC_SDN_year_month$Month[3], "sorghum", SDN_harvest)
# lagged_reg_data$peanut_millet <- time_since_harvest(IPC_SDN_year_month$Month[14], "peanut_millet", SDN_harvest)
reg_data_names <- names(lagged_reg_data)
for (i in 1:2) {
  IPC_col_index <- IPC_ncol - i
  conflict_col_index <- conflict_ncol - 6*i
  disaster_col_index <- disaster_ncol - i
  reg_data_i <- IPC_SDN_provinces[,c(1, IPC_col_index:(IPC_col_index-1))] %>% 
    left_join(conflict_SDN_n_events[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>%
    left_join(conflict_SDN_fatalities[,c(1, conflict_col_index:(conflict_col_index-6*2+2))], by="Area") %>% 
    left_join(disaster_SDN_monthly_events[,c(1, disaster_col_index:(disaster_col_index-1))], by="Area") %>%
    left_join(disaster_SDN_monthly_affected[,c(1, disaster_col_index:(disaster_col_index-1))], by="Area") %>%
    left_join(disaster_SDN_monthly_deaths[,c(1, disaster_col_index:(disaster_col_index-1))], by="Area")
  reg_data_i$month_diff <- as.numeric(as.Date(paste(IPC_SDN_year_month$Year[3-i], IPC_SDN_year_month$Month[3-i], 1), format="%Y %m %d") -
                                        as.Date(paste(IPC_SDN_year_month$Year[2-i], IPC_SDN_year_month$Month[2-i], 1), format="%Y %m %d")) %/% 30
  
  reg_data_i$sorghum <- time_since_harvest(IPC_SDN_year_month$Month[3-i], "sorghum", SDN_harvest)
  # reg_data_i$corn_rice <- time_since_harvest(IPC_SDN_year_month$Month[14-i], "corn_rice", SDN_harvest)
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
  labs(title="Correlations of SDN data") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Factor Analysis
eigen_reg_data <- eigen(cor(lagged_reg_data[,-1]))
eigen_reg_data$values
fa_IPC_t <- fa(lagged_reg_data[,-1], nfactors = 8, rotate = "varimax")
fa_IPC_t$loadings

conflict_reg_data <- lagged_reg_data %>% select(c_n_events_Riots_t:log_fatal_Battles_t_1)
eigen_conflict <- eigen(cor(conflict_reg_data %>% select(c_n_events_Riots_t:c_n_events_Battles_t)))
eigen_conflict$values
fa_conflict_t <- fa(conflict_reg_data %>% select(c_n_events_Riots_t:c_n_events_Battles_t), nfactors = 3, rotate = "varimax")
fa_conflict_t$loadings[1:6,]

disaster_reg_data <- lagged_reg_data %>% select(n_disasters_t:log_deaths_t_2)
eigen_disaster <- eigen(cor(disaster_reg_data %>% select(n_disasters_t, affected_t, log_deaths_t)))
eigen_disaster$values
fa_disaster_t <- fa(disaster_reg_data %>% select(n_disasters_t, affected_t, log_deaths_t), nfactors = 1, rotate = "varimax")
fa_disaster_t$loadings

conflict_disaster_reg_data <- lagged_reg_data %>% select(c_n_events_Riots_t:log_deaths_t_2)
eigen_conflict_disaster <- eigen(cor(conflict_disaster_reg_data))
eigen_conflict_disaster$values
fa_conflict_disaster <- fa(conflict_disaster_reg_data, nfactors = 9, rotate = "varimax")
fa_conflict_disaster$loadings
fa_conflict_disaster$scores

# Factor regression
conflict_factor_scores <- cbind(conflict_reg_data %>% select(c_n_events_Riots_t:c_n_events_Battles_t) %>% as.matrix %*% fa_conflict_t$loadings[1:6,],
                                conflict_reg_data %>% select(c_n_events_Riots_t_1,
                                                             c_n_events_Explosions_Remote_violence_t_1,
                                                             c_n_events_Violence_against_civilians_t_1,
                                                             c_n_events_Strategic_developments_t_1,
                                                             c_n_events_Protests_t_1,
                                                             c_n_events_Battles_t_1) %>% as.matrix %*% fa_conflict_t$loadings[1:6,])
colnames(conflict_factor_scores) <- c(paste0("conflict_factor_", 1:3, "_t"), paste0("conflict_factor_", 1:3, "_t_1"))
disaster_factor_scores <- cbind(disaster_reg_data %>% select(n_disasters_t, affected_t, log_deaths_t) %>% as.matrix %*% fa_disaster_t$loadings,
                                disaster_reg_data %>% select(n_disasters_t_1, affected_t_1, log_deaths_t_1) %>% as.matrix %*% fa_disaster_t$loadings,
                                disaster_reg_data %>% select(n_disasters_t_2, affected_t_2, log_deaths_t_2) %>% as.matrix %*% fa_disaster_t$loadings)
colnames(disaster_factor_scores) <- paste0("disaster_factor_t", c("", "_1", "_2"))

conflict_disaster_factors_separate <- lagged_reg_data %>%
  select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, month_diff, sorghum) %>%
  bind_cols(conflict_factor_scores)# %>% 
# bind_cols(disaster_factor_scores)

conflict_disaster_factors_together <- lagged_reg_data %>%
  select(`Phase_3+ratio_t`:`Phase_3+ratio_t_2`, month_diff, wheat_barley) %>%
  bind_cols(fa_conflict_disaster$scores) # %>% 
# relocate(`Phase_3+ratio_t`:wheat_barley, MR1:MR9)

factor_regression <- lm(`Phase_3+ratio_t`~., data=conflict_disaster_factors_separate) %>% summary()
factor_regression$residuals
year_month_reg <- gsub("Phase_3\\+ratio_", "", names(IPC_SDN_provinces)[4:7])[4:1]
factor_reg_res <- tibble(shapeName=IPC_SDN_provinces$Area)
for (i in 1:length(year_month_reg)) {
  year_month_i <- year_month_reg[i]
  k <- 18*(i-1)
  factor_reg_res[[paste0("res_", year_month_i)]] <- factor_regression$residuals[(k+1):(k+18)]
}
factor_reg_res

max(factor_regression$residuals); min(factor_regression$residuals)
for (i in 1:length(year_month_reg)) {
  year_month_i <- names(factor_reg_res)[i+1]
  res_map_i <- SDN_map %>% left_join(factor_reg_res[, c(1, 1+i)], by="shapeName") %>% 
    rename(residual=year_month_i) %>% 
    ggplot() + geom_sf(aes(fill=residual)) +
    labs(title=paste0("residuals in ", year_month_reg[i])) +
    scale_fill_viridis_c(limits=c(-0.15,0.18)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          line = element_blank())
  ggsave(paste0("Food Security/Figs/regression residuals/SDN factor regression residuals ", year_month_reg[i], ".png"), res_map_i, scale=1)
}

lm(`Phase_3+ratio_t`~., data=lagged_reg_data) %>% summary()
lm(`Phase_3+ratio_t`~., data=conflict_disaster_factors_separate) %>% summary()
lm(`Phase_3+ratio_t`~., data=conflict_disaster_factors_together) %>% summary()

lm(`Phase_3+ratio_t`~.-month_diff, data=lagged_reg_data) %>% summary()

lm(`Phase_3+ratio_t`~., data=lagged_reg_data[1:306,]) %>% summary() # regression without 2019-09
lm(`Phase_3+ratio_t`~., data=lagged_reg_data[1:272,]) %>% summary() # regression without 2019-09 and 2020-04

