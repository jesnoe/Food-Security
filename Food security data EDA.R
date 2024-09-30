# setwd("/Users/R")
# setwd("C:/Users/gkfrj/Documents/R")
library(readxl)
library(stringi)
library(tidyverse)
library(gridExtra)
library(colmaps)
library(lubridate)

# How does the Population Tracking Tool work? (https://www.ipcinfo.org/ipc-country-analysis/population-tracking-tool/faqs/en/)
# One of the key features of IPC analyses is that they offer data on the current food security situation in a given country,
# but they also offer projections to give the audience an idea of how the situation could progress over time based on key drivers.
# With this in mind, the IPC Population Tracking Tool offers data for the current, first projection and second projection, side-by-side,
# for each country. This can be viewed by scrolling to the right of the page.

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
conflict_LA <- read.csv("Food Security/LatinAmerica_conflict_2018-2024_Sep20.csv") %>% as_tibble
IPC <- read_xlsx("Food Security/Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
  filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population)) %>% 
  mutate()
IPC$Area_Phase <- IPC %>% select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>% apply(1, function(x) which.max(x)) %>% as.factor
IPC$Date <- as.Date(paste(IPC$Date, "01"), format="%b %Y %d")
IPC$Year <- year(IPC$Date) %>% as.factor
IPC$Month <- month(IPC$Date) %>% as.factor
IPC <- IPC %>% relocate(Country:Area_id, Year, Month)


disaster$`Start Year` %>% unique     # 2001~2024
conflict_AF$year %>% unique %>% sort # 1997~2024
conflict_LA$year %>% unique %>% sort # 2018~2024
IPC$Year %>% unique %>% sort         # 2017~2024

IPC_Col <- IPC %>% filter(Country == "Colombia") # 0
FSI_Col <- FSI %>% filter(Country == "Colombia")
disaster_Col <- disaster %>% filter(Country == "Colombia") # 198
conflict_Col <- conflict_LA %>% filter(country == "Colombia") # 28,230

IPC_CAR <- IPC %>% filter(Country == "Central African Republic") # 515
FSI_CAR <- FSI %>% filter(Country == "Central African Republic")
disaster_CAR <- disaster %>% filter(Country == "Central African Republic") # 66
conflict_CAR <- conflict_AF %>% filter(country == "Central African Republic") # 7,384

disaster_Col$Location %>% head(10)
disaster_CAR$Location %>% head(10)

# The Central African Republic is divided into 20 prefectures (14 administrative prefectures and 2 economic prefectures ) and one autonomous commune.
# The prefectures (préfectures) are further divided into 84 sub-prefectures (sous-préfectures).
IPC_CAR %>% group_by(Year, Month) %>% summarize(n_subareas=Subarea %>% unique %>% length)

IPC_CAR_national_populations <- IPC_CAR %>%
  select(Year, Month, Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
  aggregate(.~Year+Month, sum) %>% 
  arrange(Year, Month)
IPC_CAR_national_populations$sum_populations <- IPC_CAR_national_populations %>%
  apply(1, function(x) sum(as.numeric(x[3:7])))

IPC_CAR_national_ratio <- IPC_CAR_national_populations %>% 
  mutate(Phase_1 = Phase_1 / tot_populations,
         Phase_2 = Phase_2 / tot_populations,
         Phase_3 = Phase_3 / tot_populations,
         Phase_4 = Phase_4 / tot_populations,
         Phase_5 = Phase_5 / tot_populations) %>% select(-tot_populations)

disaster_CAR %>% group_by(`Start Year`, `Start Month`) %>% summarize(n_obs=n()) %>% as.data.frame
disaster_n_obs <- full_join(disaster_CAR %>% group_by(`Start Year`) %>% summarize(CAR_n_obs=n()),
                            disaster_Col %>% group_by(`Start Year`) %>% summarize(Col_n_obs=n()),
                            by="Start Year") %>% arrange(`Start Year`) %>% as.data.frame
names(disaster_n_obs)[1] <- "year"

disaster_n_obs %>% ggplot() +
  geom_line(aes(x=year, y=Col_n_obs, color="Colombia")) +
  geom_line(aes(x=year, y=CAR_n_obs, color="CAR")) +
  labs(y="Annual # of disaster events") +
  scale_color_manual(name = "Country", values = c("CAR" = "black", "Colombia" = "red"))
  
conflict_n_obs <- full_join(conflict_CAR %>% group_by(year) %>% summarize(CAR_n_obs=n()),
                            conflict_Col %>% group_by(year) %>% summarize(Col_n_obs=n()),
                            by="year") %>% arrange(year) %>% as.data.frame
conflict_n_obs %>% ggplot() +
  geom_line(aes(x=year, y=Col_n_obs, color="Colombia")) +
  geom_line(aes(x=year, y=CAR_n_obs, color="CAR")) +
  labs(y="Annual # of conflict events") +
  scale_color_manual(name = "Country", values = c("CAR" = "black", "Colombia" = "red"))
