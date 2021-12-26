rm(list = ls())

## Agreggatting ERA5land
library(tidyverse)
library(sf)
library(ncdf4)
library(stars)
library(cubelyr)

##
# CHECK THE PATHS!
# Stars 0.5-3

# d2m files
d2m_first_year<-read_stars("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2009.nc", 
                           sub = "d2m",
                           quiet = T, 
                           proxy = F)
# t2m files
t2m_first_year<-read_stars("Data/Raw_ERA5Land/era5land_hourly_t2m_y2009_extracted03092021.nc", 
                           quiet = T, 
                           proxy = F)

# Daily aggegation
d2m_daily<-aggregate(d2m_first_year, "day", FUN = mean)
t2m_daily<-aggregate(t2m_first_year, "day", FUN = mean)

# Saving Files
write_stars(obj = d2m_daily, "d2m_daily_y2009.RDS")
write_stars(obj = t2m_daily, "t2m_daily_y2009.RDS")