## Agreggatting ERA5land
library(geobr)
library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(viridis)
library(patchwork)
library(exactextractr)
library(foreach)
library(doParallel)
library("cowplot")
library("plotly")
library("gganimate")
library("RColorBrewer")
library("transformr")
library("readr")
library(stars)

# setwd("~/Desktop/era5land_aggregration/")

source("Scripts/generating_aggregation.R")

# no_axis <- theme(axis.title = element_blank(),
#                  axis.text =  element_blank(),
#                  axis.ticks = element_blank())
# 
# plt_fun<-function(x, title = NULL){
#   x %>%
#        ggplot()+
#        geom_sf(size = .1, fill="#2D3E50", color="#FEBF57", show.legend = FALSE)+
#        labs(subtitle = title)+
#        theme_minimal()+
#        no_axis
# }

# Download raw ERA5 land, hourly, 2tm, crop in Brazil limits
# t2m
# t2m_years_names<-list.files("Data/Raw_ERA5Land/", pattern = "t2m")
# t2m_years_names<-str_c("Data/Raw_ERA5Land/", t2m_years_names)
# 
# t2m_years_list<-lapply(t2m_years_names, brick)
# years<-seq(2009, 2020, 1)
# 
# names(t2m_years_list)<-years

# Relative Humidity (in %)
# rh_years_names<-list.files("Data/Raw_ERA5/", pattern = "rh")
# rh_years_names<-str_c("Data/Raw_ERA5/", rh_years_names)
# 
# rh_years_list<-lapply(rh_years_names, brick)
# years<-seq(2009, 2020, 1)
# 
# names(rh_years_list)<-years
# RERODAR 2009 E 2010

# # dewpoint temperature 2m (in K)
# d2m_years_names<-list.files("Data/Raw_ERA5Land/", pattern = "dpt2m_sp")
# d2m_years_names<-str_c("Data/Raw_ERA5Land/", d2m_years_names)
# 
# # d2m_years_list<-vector("list", 12)
# # d2m_years_list[[1]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2009.nc", varname = "d2m")
# # d2m_years_list[[2]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2010.nc", varname = "d2m")
# # d2m_years_list[[3]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2011.nc", varname = "d2m")
# # d2m_years_list[[4]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2012.nc", varname = "d2m")
# # d2m_years_list[[5]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2013.nc", varname = "d2m")
# # d2m_years_list[[6]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2014.nc", varname = "d2m")
# # d2m_years_list[[7]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2015.nc", varname = "d2m")
# # d2m_years_list[[8]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2016.nc", varname = "d2m")
# # d2m_years_list[[9]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2017.nc", varname = "d2m")
# # d2m_years_list[[10]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2018.nc", varname = "d2m")
# # d2m_years_list[[11]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2019.nc", varname = "d2m")
# # d2m_years_list[[12]]<-brick("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2020.nc", varname = "d2m")
# 
# d2m_years_list<-lapply(d2m_years_names, brick, varname = "d2m")
# years<-seq(2009, 2020, 1)
# 
# names(d2m_years_list)<-years

# preciptation in (mm)
# precip_years_names<-list.files("Data/Raw_ERA5Land/", pattern = "precip")
# precip_years_names<-str_c("Data/Raw_ERA5Land/", precip_years_names)
# 
# precip_years_list<-lapply(precip_years_names, brick)
# years<-seq(2009, 2020, 1)
# 
# names(precip_years_list)<-years

# Relative Humidity near-ground in (%)
years<-seq(2009, 2020, 1)
rh_files_names<-intersect(list.files(path = "Data/Raw_ERA5Land/Daily_aggregated/", 
                           pattern = "era5land_daily_rh2m_y"),
                          list.files("Data/Raw_ERA5Land/Daily_aggregated/", pattern = ".rds"))
rh_files_names<-str_c("Data/Raw_ERA5Land/Daily_aggregated/", rh_files_names)

rh_years_list<-lapply(rh_files_names, function(x){
  y<-read_rds(x)
  y<-as(y, "Raster")
  return(y)
})

names(rh_years_list)<-years

# To change the file name and go as before
## t2m
# years_list<-t2m_years_list
## Relative Humiity
# years_list<-rh_years_list
## d2m
# years_list<-d2m_years_list
## precip
# years_list<-precip_years_list
## rh2m
years_list<-rh_years_list

# Municipality level aggregations
# Download sf
municipality_map <- read_municipality(year = 2010,simplified = TRUE, showProgress = FALSE)
for (i in names(years_list)) {
  adm_aggreg<-
    # generating_aggregation(climate_file = years_list[[i]], 
    #                        adm_limits = municipality_map, 
    #                        year = i)
  # # t2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_municipality_t2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # # rh
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5_aggregated/era5_municipality_rh_", i, ".csv.xz"), 
  #                         compression = -9))
  # d2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_municipality_d2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # precip
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_municipality_precip_", i, ".csv.xz"), 
  #                         compression = -9))
  # rh2m
  generating_aggregation(climate_file = years_list[[i]], 
                         adm_limits = municipality_map, 
                         year = i, 
                         hourly = FALSE)
  write_csv(x = adm_aggreg, 
            file = xzfile(paste0("Data/ERA5Land_aggregated/era5_municipality_rh2m_", i, ".csv.xz"), 
                          compression = -9))
  # beep(4)
}

# States level aggregations
# Download sf
states_map <- read_state(year = 2010, simplified = T, showProgress = FALSE)
for (i in names(years_list)) {
  adm_aggreg<-
    # generating_aggregation(climate_file = years_list[[i]],
    #                        adm_limits = states_map,
    #                        year = i)
  # # t2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_states_t2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # rh
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5_aggregated/era5_states_rh_", i, ".csv.xz"), 
  #                         compression = -9))
  # d2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_states_d2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # precip
  # write_csv(x = adm_aggreg,
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_states_precip_", i, ".csv.xz"),
  #                         compression = -9))
  # rh2m
  generating_aggregation(climate_file = years_list[[i]],
                         adm_limits = states_map,
                         year = i,
                         hourly = FALSE)
  write_csv(x = adm_aggreg,
            file = xzfile(paste0("Data/ERA5Land_aggregated/era5_states_rh2m_", i, ".csv.xz"),
                          compression = -9))
  # beep(4)
}

# Macro Health regions level aggregations
# Download sf
macro_health_map <- read_health_region(year = 2013, macro = T, simplified = T, showProgress = F)

# macro_health 
for (i in names(years_list)) {
  adm_aggreg<-
    # generating_aggregation(climate_file = years_list[[i]], 
    #                        adm_limits = macro_health_map, 
    #                        year = i)
  # # t2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_macro_health_t2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # rh
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5_aggregated/era5_macro_health_rh_", i, ".csv.xz"), 
  #                         compression = -9))
  # d2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_macro_health_d2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # precip
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_macro_health_precip_", i, ".csv.xz"), 
  #                         compression = -9))
  # rh2m
  generating_aggregation(climate_file = years_list[[i]],
                         adm_limits = states_map,
                         year = i,
                         hourly = FALSE)
  write_csv(x = adm_aggreg,
            file = xzfile(paste0("Data/ERA5Land_aggregated/era5_macro_health_rh2m_", i, ".csv.xz"),
                          compression = -9))
}

# health_regions 
health_regions_map <- read_health_region(year = 2013, macro = F, simplified = T, showProgress = F)
# datas_perdidas<-c(names(years_list)[9:12])
for (i in names(years_list)) {
  adm_aggreg<-
    # generating_aggregation(climate_file = years_list[[i]], 
    #                        adm_limits = health_regions_map, 
    #                        year = i)
  # # t2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_health_regions_t2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # rh
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5_aggregated/era5_health_regions_rh_", i, ".csv.xz"), 
  #                         compression = -9))
  # d2m
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_health_regions_d2m_", i, ".csv.xz"), 
  #                         compression = -9))
  # precip
  # write_csv(x = adm_aggreg, 
  #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_health_regions_precip_", i, ".csv.xz"), 
  #                         compression = -9))
  # rh2m
  generating_aggregation(climate_file = years_list[[i]],
                         adm_limits = states_map,
                         year = i,
                         hourly = FALSE)
  write_csv(x = adm_aggreg,
            file = xzfile(paste0("Data/ERA5Land_aggregated/era5_health_regions_rh2m_", i, ".csv.xz"),
                          compression = -9))
  
}

#