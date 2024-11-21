# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(tidygeocoder)
library(gridExtra)
library(colmaps)
library(lubridate)
library(sf)
library(sp)
library(reshape2)

{# https://www.geoboundaries.org/countryDownloads.html -> "sdn_adm_cbs_nic_ssa_20200831_SHP.zip"
  SDN_map <- read_sf("Food Security/geoBoundaries-SDN-ADM2.geojson")
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
  IPC_SDN$Area %>% unique
  
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
  conflict_SDN <- conflict_AF %>% filter(country == "Sudan") # 66,500 # much more than CAR
  conflict_SDN$event_date <- as.Date(conflict_SDN$event_date, format="%m/%d/%Y")
  conflict_SDN$month <- month(conflict_SDN$event_date) %>% as.factor
  conflict_SDN <- conflict_SDN %>% relocate(event_id_cnty, event_date, year, month)
}

#### NEED aggregate phase populations in 05-2017 and 01-2018
FSI_SDN
IPC_SDN

IPC_SDN %>% filter(!(Area %in% disaster_SDN$Region)) %>% pull(Area) %>% unique %>% sort
IPC_SDN %>% filter(!(Area %in% conflict_SDN$admin1)) %>% pull(Area) %>% unique %>% sort
IPC_SDN <- IPC_SDN %>% mutate(Area = gsub("Aj Jazirah", "Al Jazirah", Area))
conflict_SDN %>% filter(!(admin1 %in% disaster_SDN$Region)) %>% pull(admin1) %>% unique %>% sort
conflict_SDN %>% filter(!(admin1 %in% IPC_SDN$Area)) %>% pull(admin1) %>% unique %>% sort
disaster_SDN$Region %>% unique %>% sort
# IPC_SDN %>% filter(!(Subarea %in% SDN_map$shapeName)) %>% pull(Subarea) %>% unique %>% sort

IPC_SDN <- IPC_SDN %>% 
  mutate(Area = ifelse(is.na(Area) & Subarea %in% SDN_map$shapeName, Subarea, Area))

IPC_SDN_provinces <- IPC_SDN %>%
  group_by(Area, Year, Month) %>% 
  summarise(across(c(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, Phase_3above), sum)) %>% 
  mutate(Phase_1_ratio = Phase_1/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_2_ratio = Phase_2/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_3_ratio = Phase_3/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_4_ratio = Phase_4/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_5_ratio = Phase_5/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T),
         Phase_3above_ratio = Phase_3above/sum(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5, na.rm = T))
IPC_SDN_provinces <- IPC_SDN_provinces %>% select(Area, Year, Month, Phase_3above_ratio) %>% 
  pivot_wider(id_cols=Area, names_prefix="Phase_3+ratio_", names_from = c("Year", "Month"), values_from = Phase_3above_ratio) %>% 
  relocate(Area, `Phase_3+ratio_2019_6`, `Phase_3+ratio_2020_6`)

IPC_SDN_provinces[,-1] %>% apply(2, function(x) sum(is.na(x))) # 30 missing in 05-0217 and 01-2018
IPC_SDN_provinces[is.na(IPC_SDN_provinces)] <- 0

IPC_SDN %>% filter(Year == 2019 & Month == 6)

conflict_SDN$event_type %>% table # 6 types
conflict_SDN$sub_event_type %>% table # 24 types
conflict_SDN %>% filter(event_type == "Protests") %>% pull(sub_event_type) %>% table
conflict_SDN %>% filter(event_type == "Strategic developments") %>% pull(sub_event_type) %>% table
conflict_SDN %>% filter(event_type == "Violence against civilians") %>% pull(sub_event_type) %>% table

conflict_SDN_aggr <- conflict_SDN %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month)) %>% 
  filter(year > 2018) %>% 
  filter(!(year == 2024 & month > 3)) %>% 
  filter(!(year == 2019 & month < 6)) %>%
  group_by(year, month, admin1, event_type) %>% 
  summarise(n_events=n(),
            fatalities=sum(fatalities, na.rm=T)) %>% 
  rename(Area=admin1) %>% 
  arrange(year, month)

IPC_SDN_year_month <- IPC_SDN %>% select(Year, Month) %>% unique %>%
  mutate(across(c(Year, Month), function(x) as.character(x) %>% as.numeric)) %>% arrange(Year, Month)
prev_year <- 2019; prev_month <- 6
for (i in 2:nrow(IPC_SDN_year_month)) {
  year_i <- IPC_SDN_year_month$Year[i]
  month_i <- IPC_SDN_year_month$Month[i]
  prev_months_index <- which(conflict_SDN_aggr$year == prev_year & conflict_SDN_aggr$month == prev_month)
  prev_months_index <- prev_months_index[length(prev_months_index)]
  months_index_i <- which(conflict_SDN_aggr$year == year_i & conflict_SDN_aggr$month == month_i)[1]
  if (is.na(months_index_i)) months_index_i <- rev(which(conflict_SDN_aggr$year == year_i & conflict_SDN_aggr$month == month_i-1))[1]
  if (year_i != prev_year) conflict_SDN_aggr$year[(prev_months_index+1):months_index_i] <- year_i
  conflict_SDN_aggr$month[(prev_months_index+1):months_index_i] <- month_i
  prev_year <- year_i; prev_month <- month_i
}

conflict_SDN$event_type %>% table # 6 types
conflict_SDN_n_events <- conflict_SDN_aggr %>% 
  group_by(year, month, Area, event_type) %>% 
  summarise(n_events=sum(n_events)) %>% 
  pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = n_events)
conflict_SDN_n_events[is.na(conflict_SDN_n_events)] <- 0

conflict_SDN_fatalities <- conflict_SDN_aggr %>% 
  group_by(year, month, Area, event_type) %>% 
  summarise(log_fatalities=log(1+sum(fatalities, na.rm=T))) %>% 
  pivot_wider(id_cols=Area, names_from = c(event_type, year, month), values_from = log_fatalities)
conflict_SDN_fatalities[is.na(conflict_SDN_fatalities)] <- 0

disaster_SDN$`Disaster Type` %>% table
disaster_SDN_monthly_aggr <- disaster_SDN %>% 
  filter(!(year >= 2024 & month > 3)) %>%
  filter(year > 2018) %>% 
  filter(!(year == 2019 & month < 6)) %>% 
  group_by(Region, year, month) %>% 
  summarise(n_disasters=n(),
            affected=sum(`Total Affected`, na.rm=T)) %>% 
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

disaster_SDN_monthly_affected <- disaster_SDN_monthly_aggr %>%
  select(-year_month) %>% 
  group_by(year, month, Area) %>% 
  summarise(log_affected=log(1+sum(affected, na.rm=T))) %>% 
  pivot_wider(id_cols=Area, names_prefix = "affected_", names_from = c(year, month), values_from = log_affected)
disaster_SDN_monthly_affected[is.na(disaster_SDN_monthly_affected)] <- 0

# time series plot
IPC_SDN_ts <- IPC_SDN_provinces %>% 
  pivot_longer(cols=-Area, names_to = "year_month", values_to = "Phase_3_or_high") %>% 
  mutate(year_month=gsub("Phase_3\\+ratio_", "", year_month) %>% as.factor)

IPC_SDN_ts_plot <- IPC_SDN_ts %>% ggplot() +
  geom_line(aes(x=year_month,
                y=Phase_3_or_high,
                group=Area,
                color=Area)) +
  geom_point(aes(x=year_month,
                 y=Phase_3_or_high,
                 group=Area,
                 color=Area)) +
  scale_color_viridis_d(option="H") +
  labs(x="year_month", y="ratio of Phase_3_or_high")
# ggsave(paste0("Food Security/Figs/ts plot/SDN/SDN food insecurity ts.png"), IPC_SDN_ts_plot, width=25, height=12, unit="cm")

conflicts_SDN_ts <- conflict_SDN_n_events %>% 
  pivot_longer(cols=-Area, names_sep="_", names_to = c("conflict", "year", "month"), values_to = "n_conflicts") %>% 
  mutate(year_month=paste(year, month, sep="_") %>% as.factor,
         conflict = conflict %>% as.factor) %>% 
  select(-year, -month) %>% relocate(year_month)

disaster_SDN_ts <- disaster_SDN_monthly_events %>% 
  pivot_longer(cols=-Area, names_to = "year_month", values_to = "n_disasters") %>% 
  mutate(year_month=gsub("n_disasters_", "", year_month) %>% as.factor)
names(disaster_SDN_ts)

year_month <- disaster_SDN_ts$year_month
conflict_types <- conflicts_SDN_ts$conflict %>% unique
conflict_plots <- list()
for (i in 1:6) {
  conflict_type <- conflict_types[i]
  conflicts_SDN_ts_i <- conflicts_SDN_ts %>%
    filter(conflict == conflict_type)
  conflict_plot_i <- conflicts_SDN_ts_i %>% 
    ggplot() +
    geom_line(aes(x=year_month,
                  y=n_conflicts,
                  group=Area,
                  color=Area)) +
    scale_color_viridis_d(option="H") +
    labs(x="year_month", y="# of conflict events")
  conflict_plots[[as.character(conflict_type)]] <- conflict_plot_i
  conflict_type <- gsub("/", "-", conflict_type)
  # ggsave(paste0("Food Security/Figs/ts plot/SDN/SDN n_conflicts ", conflict_type, ".png"), conflict_plot_i, width=25, height=12, unit="cm")
}
conflict_plots$`Explosions/Remote violence`
affected

disaster_plot <- disaster_SDN_ts %>% ggplot() +
  geom_line(aes(x=year_month,
                y=n_disasters,
                group=Area,
                color=Area)) +
  scale_color_viridis_d(option="H") +
  labs(x="year_month", y="# of disaster events")
# ggsave(paste0("Food Security/Figs/ts plot/SDN/SDN n_disasters.png"), disaster_plot, width=25, height=12, unit="cm")


conflicts_SDN_fatalities_ts <- conflict_SDN_fatalities %>% 
  pivot_longer(cols=-Area, names_sep="_", names_to = c("conflict", "year", "month"), values_to = "log_fatalities") %>% 
  mutate(year_month=paste(year, month, sep="_") %>% as.factor,
         conflict = conflict %>% as.factor) %>% 
  select(-year, -month) %>% relocate(year_month)

disaster_SDN_affected_ts <- disaster_SDN_monthly_affected %>% 
  pivot_longer(cols=-Area, names_to = "year_month", values_to = "log_affected") %>% 
  mutate(year_month=gsub("affected_", "", year_month) %>% as.factor)

year_month <- disaster_SDN_ts$year_month
conflict_types <- conflicts_SDN_ts$conflict %>% unique
for (i in 1:6) {
  conflict_type <- conflict_types[i]
  conflicts_SDN_ts_i <- conflicts_SDN_fatalities_ts %>%
    filter(conflict == conflict_type)
  conflict_plot_i <- conflicts_SDN_ts_i %>% 
    ggplot() +
    geom_line(aes(x=year_month,
                  y=log_fatalities,
                  group=Area,
                  color=Area)) +
    scale_color_viridis_d(option="H") +
    labs(x="year_month", y="log conflict fatalities")
  conflict_type <- gsub("/", "-", conflict_type)
  # ggsave(paste0("Food Security/Figs/ts plot/SDN/SDN log fatalities ", conflict_type, ".png"), conflict_plot_i, width=25, height=12, unit="cm")
}

disaster_affected_plot <- disaster_SDN_affected_ts %>% ggplot() +
  geom_line(aes(x=year_month,
                y=log_affected,
                group=Area,
                color=Area)) +
  scale_color_viridis_d(option="H") +
  labs(x="year_month", y="log disaster casualties")
# ggsave(paste0("Food Security/Figs/ts plot/SDN/SDN log affected.png"), disaster_affected_plot, width=25, height=12, unit="cm")



# regression
lm(`Phase_3+ratio_2024_4`~.-Area,
   data = IPC_SDN_provinces %>% select(`Phase_3+ratio_2024_4`:`Phase_3+ratio_2019_6`)) %>% summary

# Is "Phase_3+ratio" affected by previous periods?
for (i in 15:9) {
  dependent_var <- names(IPC_SDN_provinces)[i]
  autoregression_i <- lm(formula(paste0("`", dependent_var, "`~.")),
                         data = IPC_SDN_provinces[,(i-4):i])
  print(dependent_var)
  print(summary(autoregression_i))
}

lagged4_IPC <- IPC_SDN_provinces[,7:(7-4)]
names(lagged4_IPC) <- c(paste0("V", 1:5))
for (i in 7:6) { #238 rows
  data_i <- IPC_SDN_provinces[,i:(i-4)] %>% as.matrix
  colnames(data_i) <- NULL
  lagged4_IPC <- bind_rows(lagged4_IPC, data_i %>% as_tibble)
}
names(lagged4_IPC) <- c("Phase3+_ratio_t", paste0("Phase3+_ratio_t_", 1:4))
lm(`Phase3+_ratio_t`~., data=lagged4_IPC) %>% summary()


IPC_ncol <- ncol(IPC_SDN_provinces)
conflict_ncol <- ncol(conflict_SDN_n_events) # 36
conflict_fatalities_ncol <- ncol(conflict_SDN_fatalities) # 36
disaster_ncol <- ncol(disaster_SDN_monthly_events) # 4
disaster_affected_ncol <- ncol(disaster_SDN_monthly_affected) # 4
names(conflict_SDN_n_events)[-1] <- paste0("c_n_events_", names(conflict_SDN_n_events)[-1])
names(conflict_SDN_fatalities)[-1] <- paste0("log_fatal_", names(conflict_SDN_fatalities)[-1])
reg_data_i <- IPC_SDN_provinces[,c(1, IPC_ncol:(IPC_ncol-2))] %>% 
  left_join(conflict_SDN_n_events[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>%
  left_join(conflict_SDN_fatalities[,c(1, conflict_ncol:(conflict_ncol-6*2+1))], by="Area") %>% 
  left_join(disaster_SDN_monthly_events[,c(1, disaster_ncol:(disaster_ncol-1))], by="Area") %>% 
  left_join(disaster_SDN_monthly_affected[,c(1, disaster_ncol:(disaster_ncol-1))], by="Area")
names(reg_data_i) <- gsub("2024_4", "t", names(reg_data_i))
names(reg_data_i) <- gsub("2023_6", "t_1", names(reg_data_i))
names(reg_data_i) <- gsub("2022_5", "t_2", names(reg_data_i))
names(reg_data_i) <- gsub("2020_6", "t_4", names(reg_data_i))
names(reg_data_i) <- gsub("[/ ]", "_", names(reg_data_i))
lagged_reg_data <- reg_data_i[,-1]
reg_data_names <- names(lagged_reg_data)
for (i in 1:3) {
  IPC_col_index <- IPC_ncol - i
  conflict_col_index <- conflict_ncol - 6*i
  disaster_col_index <- disaster_ncol
  reg_data_i <- IPC_SDN_provinces[,c(1, IPC_col_index:(IPC_col_index-2))] %>% 
    left_join(conflict_SDN_n_events[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>%
    left_join(conflict_SDN_fatalities[,c(1, conflict_col_index:(conflict_col_index-6*2+1))], by="Area") %>% 
    left_join(disaster_SDN_monthly_events[,c(1, disaster_col_index:(disaster_col_index-1))], by="Area") %>% 
    left_join(disaster_SDN_monthly_affected[,c(1, disaster_col_index:(disaster_col_index-1))], by="Area")
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


lm(`Phase_3+ratio_t`~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Riots_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Violence_against_civilians_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Strategic_developments_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Protests_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Explosions_Remote_violence_t~., data=lagged_reg_data) %>% summary()
lm(c_n_events_Battles_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Riots_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Violence_against_civilians_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Strategic_developments_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Protests_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Explosions_Remote_violence_t~., data=lagged_reg_data) %>% summary()
lm(log_fatal_Battles_t~., data=lagged_reg_data) %>% summary()
# lm(n_disasters_t_2~., data=lagged_reg_data) %>% summary()
# lm(affected_t_2~., data=lagged_reg_data) %>% summary()