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

load("Food Security/lagged_reg_data_list_AFG.RData")
load("Food Security/lagged_reg_data_list_SDN.RData")
load("Food Security/lagged_reg_data_list_COD.RData")
load("Food Security/lagged_reg_data_list_SOM.RData")
load("Food Security/lagged_reg_data_list_CAF.RData")
load("Food Security/lagged_reg_data_list_MOZ.RData")

residual_map <- function(NAT_map, NAT_phase3_long_lagged_rainfall, reg_rainfall, NAT_code) {
  reg_rainfall_residuals <- NAT_phase3_long_lagged_rainfall %>%
    select(Area:month) %>% 
    mutate(residual = reg_rainfall$residuals)
  max_res <- max(reg_rainfall_residuals$residual)
  min_res <- min(reg_rainfall_residuals$residual)
  reg_rainfall_year_months <- reg_rainfall_residuals %>% select(year, month) %>% unique %>% arrange(year, month)
  for (m in 1:nrow(reg_rainfall_year_months)) {
    year_m <- reg_rainfall_year_months$year[m]
    month_m <- reg_rainfall_year_months$month[m]
    residual_m <- reg_rainfall_residuals %>% filter(year == year_m, month == month_m)
    
    residual_map_m <- left_join(NAT_map %>% rename(Area=shapeName),
                                residual_m,
                                by="Area") %>% 
      ggplot() + geom_sf(aes(fill=residual)) +
      scale_fill_gradientn(colors = c("blue","skyblue","grey30","yellow","red"),
                           values = scales::rescale(c(min_res, min_res/2,  0, max_res/2, max_res)),
                           limits = c(min_res, max_res),
                           na.value = "white") +
      ggtitle(sprintf("%s (%i-%i)", NAT_code, year_m, month_m)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            line = element_blank())
    ggsave(sprintf("Food Security/Figs/rainfall regression residuals/%s/rainfall regression residuals %s (%i-%i).png", NAT_code, NAT_code, year_m, month_m), residual_map_m, scale = 1)
  }
}
{
AFG_map <- read_sf("Food Security/geoBoundaries-AFG-ADM1.geojson")
AFG_map$shapeName[which(AFG_map$shapeName == "Ghanzi")] <- "Ghazni"
AFG_map$shapeName[which(AFG_map$shapeName == "Sar-e Pol")] <- "Sar_e_Pol"
SDN_map <- read_sf("Food Security/geoBoundaries-SDN-ADM1.geojson")
SDN_map$shapeName[which(SDN_map$shapeName == "Abyei PCA")] <- "Abyei"
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
SOM_map <- read_sf("Food Security/geoBoundaries-SOM-ADM1.geojson")
SOM_map$shapeName[which(SOM_map$shapeName == "Hiiraan")] <- "Hiraan"
CAF_map <- read_sf("Food Security/geoBoundaries-CAF-ADM1.geojson")
MOZ_map <- read_sf("Food Security/geoBoundaries-MOZ-ADM1.geojson")

IPC_AFG_phase3_long_lagged_rainfall <- read.csv("Food Security/IPC_AFG_phase3_long_lagged_rainfall.csv") %>% as_tibble %>% filter(!if_any(Phase_3above_ratio:spi_3h, is.na))
IPC_SDN_phase3_long_lagged_rainfall <- read.csv("Food Security/IPC_SDN_phase3_long_lagged_rainfall.csv") %>% as_tibble %>% filter(!if_any(Phase_3above_ratio:spi_3h, is.na))
IPC_COD_phase3_long_lagged_rainfall <- read.csv("Food Security/IPC_COD_phase3_long_lagged_rainfall.csv") %>% as_tibble %>% filter(!if_any(Phase_3above_ratio:spi_3h, is.na))
IPC_SOM_phase3_long_lagged_rainfall <- read.csv("Food Security/IPC_SOM_phase3_long_lagged_rainfall.csv") %>% as_tibble %>% filter(!if_any(Phase_3above_ratio:spi_3h, is.na))
IPC_CAF_phase3_long_lagged_rainfall <- read.csv("Food Security/IPC_CAF_phase3_long_lagged_rainfall.csv") %>% as_tibble %>% filter(!if_any(Phase_3above_ratio:spi_3h, is.na))
IPC_MOZ_phase3_long_lagged_rainfall <- read.csv("Food Security/IPC_MOZ_phase3_long_lagged_rainfall.csv") %>% as_tibble %>% filter(!if_any(Phase_3above_ratio:spi_3h, is.na))
}

AFG_reg_rainfall_spi_1h <- lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
                                mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
                                       spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
                                select(-spi_1h))
AFG_reg_rainfall_spi_3h <- lm(Phase_3above_ratio~., data=IPC_AFG_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
                                mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
                                       spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
                                select(-spi_3h))


SDN_reg_rainfall_spi_1h <- lm(Phase_3above_ratio~., data=IPC_SDN_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
                                mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
                                       spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
                                select(-spi_1h))
SDN_reg_rainfall_spi_3h <- lm(Phase_3above_ratio~., data=IPC_SDN_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
                                mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
                                       spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
                                select(-spi_3h))

COD_reg_rainfall_spi_1h <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
                                mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
                                       spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
                                select(-spi_1h))
COD_reg_rainfall_spi_3h <- lm(Phase_3above_ratio~., data=IPC_COD_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
                                mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
                                       spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
                                select(-spi_3h))

SOM_reg_rainfall_spi_1h <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
                                mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
                                       spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
                                select(-spi_1h))
SOM_reg_rainfall_spi_3h <- lm(Phase_3above_ratio~., data=IPC_SOM_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
                                mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
                                       spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
                                select(-spi_3h))

CAF_reg_rainfall_spi_1h <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
                                mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
                                       spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
                                select(-spi_1h))
CAF_reg_rainfall_spi_3h <- lm(Phase_3above_ratio~., data=IPC_CAF_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
                                mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
                                       spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
                                select(-spi_3h))

MOZ_reg_rainfall_spi_1h <- lm(Phase_3above_ratio~., data=IPC_MOZ_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_1h) %>% 
                                mutate(spi_1h_pos = ifelse(spi_1h < 0, 0, spi_1h),
                                       spi_1h_neg = ifelse(spi_1h < 0, spi_1h, 0)) %>%
                                select(-spi_1h))
MOZ_reg_rainfall_spi_3h <- lm(Phase_3above_ratio~., data=IPC_MOZ_phase3_long_lagged_rainfall %>%
                                select(Phase_3above_ratio:n_disaster2, spi_3h) %>% 
                                mutate(spi_3h_pos = ifelse(spi_3h < 0, 0, spi_3h),
                                       spi_3h_neg = ifelse(spi_3h < 0, spi_3h, 0)) %>%
                                select(-spi_3h))

  # residual maps
residual_map(AFG_map, IPC_AFG_phase3_long_lagged_rainfall, AFG_reg_rainfall_spi_3h, "AFG")
residual_map(SDN_map, IPC_SDN_phase3_long_lagged_rainfall, SDN_reg_rainfall_spi_3h, "SDN")
residual_map(COD_map, IPC_COD_phase3_long_lagged_rainfall, COD_reg_rainfall_spi_3h, "COD")
residual_map(SOM_map, IPC_SOM_phase3_long_lagged_rainfall, SOM_reg_rainfall_spi_3h, "SOM")
residual_map(CAF_map, IPC_CAF_phase3_long_lagged_rainfall, CAF_reg_rainfall_spi_3h, "CAF")
residual_map(MOZ_map, IPC_MOZ_phase3_long_lagged_rainfall, MOZ_reg_rainfall_spi_3h, "MOZ")


residual_plot <- function(reg_model, var, country) {
  plot_data <- tibble(residual = reg_model$residuals,
                      variable = reg_model$model %>% as_tibble %>% pull(var))
  
  plot_data %>% ggplot() +
    geom_point(aes(x=variable, y=residual)) +
    labs(title = paste(country, var, "residuals"),
         x = var)
}

var_ <- "spi_1h_pos"
var_ <- "IPC_t_1"
var_ <- "season"
var_ <- "n_events_Battles_explosions"
var_ <- "n_events_Protests_Riots"
var_ <- "n_events_etc."
var_ <- "fatalities_Battles_explosions"
var_ <- "n_disasters"
residual_plot(AFG_reg_rainfall_spi_1h, var_, "AFG")
residual_plot(SDN_reg_rainfall_spi_1h, var_, "SDN")
residual_plot(COD_reg_rainfall_spi_1h, var_, "COD")
residual_plot(SOM_reg_rainfall_spi_1h, var_, "SOM")
residual_plot(CAF_reg_rainfall_spi_1h, var_, "CAF")
residual_plot(MOZ_reg_rainfall_spi_1h, var_, "MOZ")

var_ <- "spi_3h_pos"
var_ <- "IPC_t_1"
var_ <- "season"
var_ <- "n_events_Battles_explosions"
var_ <- "n_events_Protests_Riots"
var_ <- "n_events_etc."
var_ <- "fatalities_Battles_explosions"
var_ <- "n_disasters"
residual_plot(AFG_reg_rainfall_spi_3h, var_, "AFG")
residual_plot(SDN_reg_rainfall_spi_3h, var_, "SDN")
residual_plot(COD_reg_rainfall_spi_3h, var_, "COD")
residual_plot(SOM_reg_rainfall_spi_3h, var_, "SOM")
residual_plot(CAF_reg_rainfall_spi_3h, var_, "CAF")
residual_plot(MOZ_reg_rainfall_spi_3h, var_, "MOZ")
