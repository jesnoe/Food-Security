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

precipitation_AFG
precipitation_AFG %>% ggplot() + geom_line(aes(x=date, y=r1h, color=Area)) + ggtitle("AFG rainfall 1-month aggr.")
precipitation_AFG %>% ggplot() + geom_line(aes(x=date, y=spi_1h, color=Area)) + ggtitle("AFG SPI 1-month aggr.")
precipitation_AFG %>% ggplot() + geom_line(aes(x=date, y=r3h, color=Area)) + ggtitle("AFG rainfall 3-month aggr.")
precipitation_AFG %>% ggplot() + geom_line(aes(x=date, y=spi_3h, color=Area)) + ggtitle("AFG SPI 3-month aggr.")

precipitation_SDN
precipitation_SDN %>% ggplot() + geom_line(aes(x=date, y=r1h, color=Area)) + ggtitle("SDN rainfall 1-month aggr.")
precipitation_SDN %>% ggplot() + geom_line(aes(x=date, y=spi_1h, color=Area)) + ggtitle("SDN SPI 1-month aggr.")
precipitation_SDN %>% ggplot() + geom_line(aes(x=date, y=r3h, color=Area)) + ggtitle("SDN rainfall 3-month aggr.")
precipitation_SDN %>% ggplot() + geom_line(aes(x=date, y=spi_3h, color=Area)) + ggtitle("SDN SPI 3-month aggr.")

precipitation_COD
precipitation_COD %>% ggplot() + geom_line(aes(x=date, y=r1h, color=Area)) + ggtitle("COD rainfall 1-month aggr.")
precipitation_COD %>% ggplot() + geom_line(aes(x=date, y=spi_1h, color=Area)) + ggtitle("COD SPI 1-month aggr.")
precipitation_COD %>% ggplot() + geom_line(aes(x=date, y=r3h, color=Area)) + ggtitle("COD rainfall 3-month aggr.")
precipitation_COD %>% ggplot() + geom_line(aes(x=date, y=spi_3h, color=Area)) + ggtitle("COD SPI 3-month aggr.")

IPC_precipitation_AFG <- IPC_AFG_provinces_long %>% 
  rename(year = Year, month = Month) %>% 
  mutate(year = as.character(year) %>% as.numeric,
         month = as.character(month) %>% as.numeric) %>% 
  left_join(precipitation_AFG %>% ungroup(ADM1_PCODE) %>% select(-ADM1_PCODE, -date), by=c("Area", "year", "month"))

lm(Phase_3above_ratio~r1h, data=IPC_precipitation_AFG) %>% summary
lm(Phase_3above_ratio~spi_1h, data=IPC_precipitation_AFG) %>% summary
lm(Phase_3above_ratio~r3h, data=IPC_precipitation_AFG) %>% summary
lm(Phase_3above_ratio~spi_3h, data=IPC_precipitation_AFG) %>% summary


IPC_precipitation_SDN <- IPC_SDN_provinces_long %>% 
  rename(year = Year, month = Month) %>% 
  mutate(year = as.character(year) %>% as.numeric,
         month = as.character(month) %>% as.numeric) %>% 
  left_join(precipitation_SDN %>% ungroup(ADM1_PCODE) %>% select(-ADM1_PCODE, -date), by=c("Area", "year", "month"))

lm(Phase_3above_ratio~r1h, data=IPC_precipitation_SDN) %>% summary
lm(Phase_3above_ratio~spi_1h, data=IPC_precipitation_SDN) %>% summary
lm(Phase_3above_ratio~r3h, data=IPC_precipitation_SDN) %>% summary
lm(Phase_3above_ratio~spi_3h, data=IPC_precipitation_SDN) %>% summary


IPC_precipitation_COD <- IPC_COD_provinces_long %>% 
  rename(year = Year, month = Month) %>% 
  mutate(year = as.character(year) %>% as.numeric,
         month = as.character(month) %>% as.numeric) %>% 
  left_join(precipitation_COD %>% ungroup(ADM1_PCODE) %>% select(-ADM1_PCODE, -date), by=c("Area", "year", "month"))

lm(Phase_3above_ratio~r1h, data=IPC_precipitation_COD) %>% summary
lm(Phase_3above_ratio~spi_1h, data=IPC_precipitation_COD) %>% summary
lm(Phase_3above_ratio~r3h, data=IPC_precipitation_COD) %>% summary
lm(Phase_3above_ratio~spi_3h, data=IPC_precipitation_COD) %>% summary
