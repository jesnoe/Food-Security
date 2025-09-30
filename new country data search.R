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
  conflict_ME <- read.csv("Food Security/conflict_2016-01-01-2025-05-11-Middle_East.csv") %>% as_tibble
  conflict_CA <- read.csv("Food Security/conflict_2017-01-01-2024-10-06-Caucasus_and_Central_Asia.csv") %>% as_tibble
  conflict_LA <- read.csv("Food Security/conflict_2016-01-01-2025-05-04-South_America.csv") %>% as_tibble
  conflict_AF <- read.csv("Food Security/Africa_conflict_1997-2024_Sep13.csv") %>% as_tibble
  
  IPC_AS <- read_xlsx("Food Security/Asia - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(is.na(Population))
  IPC_AS$Area_Phase <- IPC_AS %>%
    select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
    apply(1, function(x) ifelse(sum(is.na(x)) == 5, 0, which.max(x))) %>% as.factor
  IPC_AS$Date <- as.Date(paste(IPC_AS$Date, "01"), format="%b %Y %d")
  IPC_AS$Year <- year(IPC_AS$Date) %>% as.factor
  IPC_AS$Month <- month(IPC_AS$Date) %>% as.factor
  IPC_AS <- IPC_AS %>% relocate(Country:Area_id, Year, Month)
  IPC_AS$Subarea <- gsub(" c_[0-9]+$", "", IPC_AS$Subarea)
  IPC_AS$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_AS$Subarea)
  
  IPC_ME <- read_xlsx("Food Security/Middle East and North Africa - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Phase_1))
  IPC_ME$Area_Phase <- IPC_ME %>%
    select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
    apply(1, function(x) ifelse(sum(is.na(x)) == 5, 0, which.max(x))) %>% as.factor
  IPC_ME$Date <- as.Date(paste(IPC_ME$Date, "01"), format="%b %Y %d")
  IPC_ME$Year <- year(IPC_ME$Date) %>% as.factor
  IPC_ME$Month <- month(IPC_ME$Date) %>% as.factor
  IPC_ME <- IPC_ME %>% relocate(Country:Area_id, Year, Month)
  IPC_ME$Subarea <- gsub(" c_[0-9]+$", "", IPC_ME$Subarea)
  IPC_ME$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_ME$Subarea)
  
  IPC_AF <- read_xlsx("Food Security/East and Central Africa - Acute Food Security Phase Classification (IPC) Data 2017-2024.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population))
  IPC_AF$Area_Phase <- IPC_AF %>% select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>% apply(1, function(x) which.max(x)) %>% as.factor
  IPC_AF$Date <- as.Date(paste(IPC_AF$Date, "01"), format="%b %Y %d")
  IPC_AF$Year <- year(IPC_AF$Date) %>% as.factor
  IPC_AF$Month <- month(IPC_AF$Date) %>% as.factor
  IPC_AF <- IPC_AF %>% relocate(Country:Area_id, Year, Month)
  IPC_AF$Subarea <- gsub(" c_[0-9]+$", "", IPC_AF$Subarea)
  IPC_AF$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_AF$Subarea)
  
  IPC_SA <- read_xlsx("Food Security/Southern Africa - Acute Food Security Phase Classification (IPC) Data 2017-2025.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population))
  IPC_SA$Area_Phase <- IPC_SA %>%
    select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
    apply(1, function(x) ifelse(sum(is.na(x)) == 5, 0, which.max(x))) %>% as.factor
  IPC_SA$Date <- as.Date(paste(IPC_SA$Date, "01"), format="%b %Y %d")
  IPC_SA$Year <- year(IPC_SA$Date) %>% as.factor
  IPC_SA$Month <- month(IPC_SA$Date) %>% as.factor
  IPC_SA <- IPC_SA %>% relocate(Country:Area_id, Year, Month)
  IPC_SA$Subarea <- gsub(" c_[0-9]+$", "", IPC_SA$Subarea)
  IPC_SA$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_SA$Subarea)
  
  IPC_LA <- read_xlsx("Food Security/Latin America and the Caribbean - Acute Food Security Phase Classification (IPC) Data 2017-2025.xlsx") %>% 
    filter(!is.na(Country)) %>% filter(!is.na(Area_id)) %>% filter(!is.na(Population))
  IPC_LA$Area_Phase <- IPC_LA %>%
    select(Phase_1, Phase_2, Phase_3, Phase_4, Phase_5) %>%
    apply(1, function(x) ifelse(sum(is.na(x)) == 5, 0, which.max(x))) %>% as.factor
  IPC_LA$Date <- as.Date(paste(IPC_LA$Date, "01"), format="%b %Y %d")
  IPC_LA$Year <- year(IPC_LA$Date) %>% as.factor
  IPC_LA$Month <- month(IPC_LA$Date) %>% as.factor
  IPC_LA <- IPC_LA %>% relocate(Country:Area_id, Year, Month)
  IPC_LA$Subarea <- gsub(" c_[0-9]+$", "", IPC_LA$Subarea)
  IPC_LA$Subarea <- gsub("_[0-9,A-z]+$", "", IPC_LA$Subarea)
}

n_IPC <- bind_rows(IPC_AS %>% group_by(Country) %>% summarize(n_obs = n()),
                   IPC_ME %>% group_by(Country) %>% summarize(n_obs = n()),
                   IPC_AF %>% group_by(Country) %>% summarize(n_obs = n()),
                   IPC_SA %>% group_by(Country) %>% summarize(n_obs = n()),
                   IPC_LA %>% group_by(Country) %>% summarize(n_obs = n()))
n_IPC %>% arrange(Country) %>% print(n=36)

n_month_IPC <- bind_rows(IPC_AS %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length),
                         IPC_ME %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length),
                         IPC_AF %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length),
                         IPC_SA %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length),
                         IPC_LA %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length))
n_month_IPC %>% arrange(Country) %>% print(n=36)

IPC_AS %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length, IPC_med = median(Phase_3above_ratio), IPC_var = var(Phase_3above_ratio))
IPC_ME %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length, IPC_med = median(Phase_3above_ratio), IPC_var = var(Phase_3above_ratio))
IPC_AF %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length, IPC_med = median(Phase_3above_ratio), IPC_var = var(Phase_3above_ratio))
IPC_SA %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length, IPC_med = median(Phase_3above_ratio), IPC_var = var(Phase_3above_ratio))
IPC_LA %>% group_by(Country) %>% summarize(n_months = unique(Date) %>% length, IPC_med = median(Phase_3above_ratio), IPC_var = var(Phase_3above_ratio))

med_IPC <- bind_rows(IPC_AS %>% group_by(Country) %>% summarize(med_IPC = median(Phase_3above_ratio)),
                     IPC_ME %>% group_by(Country) %>% summarize(med_IPC = median(Phase_3above_ratio)),
                     IPC_AF %>% group_by(Country) %>% summarize(med_IPC = median(Phase_3above_ratio)),
                     IPC_SA %>% group_by(Country) %>% summarize(med_IPC = median(Phase_3above_ratio)),
                     IPC_LA %>% group_by(Country) %>% summarize(med_IPC = median(Phase_3above_ratio)))
med_IPC %>% arrange(Country) %>% print(n=36)
med_IPC %>% arrange(desc(med_IPC)) %>% print(n=36)

conflict_AF %>% group_by(country) %>% summarize(n_obs = n()) %>% print(n=57)
conflict_CA %>% group_by(country) %>% summarize(n_obs = n())
conflict_ME %>% group_by(country) %>% summarize(n_obs = n())
conflict_LA %>% group_by(country) %>% summarize(n_obs = n())

cand_countries <- c("Burundi", "Madagascar", "Somalia", "Afghanistan", "Mozambique", "Kenya", "Central African Republic")
disaster %>% filter(Country %in% cand_countries) %>% 
  group_by(Country) %>% summarize(n_obs=n())
med_IPC %>% filter(Country %in% cand_countries)
n_IPC %>% filter(Country %in% cand_countries)
conflict_AF %>% group_by(country) %>% summarize(n_obs = n()) %>% filter(country %in% cand_countries)
