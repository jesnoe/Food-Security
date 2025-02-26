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

lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2 %>% select(-n_floods_t, -n_droughts_t)) %>% summary()
lm(`Phase_3+ratio_t`~., data=lagged_reg_data_list$months_2 %>% select(-n_disasters_t)) %>% summary()

lagged_reg_data_lm <- lapply(lagged_reg_data_list,
                             function(x) lm(`Phase_3+ratio_t`~., data=x %>% select(-n_disasters_t)) %>% summary())

lagged_reg_data_lm



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

  ## SPEI
afg_precipitation
spei(afg_precipitation$rfh, 12)

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


precipitation_corr <- cor(afg_precipitation[, 6:11]) %>% melt
precipitation_corr %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradientn(colors = c("blue","skyblue","grey40", "yellow","red"),
                       values = scales::rescale(c(-1, -.Machine$double.eps, 0 , .Machine$double.eps, 1)),
                       limits=c(-1, 1)) +
  labs(title="Correlations of AFG data") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))







