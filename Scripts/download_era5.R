library(geobr)
library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(exactextractr)
library(foreach)
library(doParallel)
library("readr")
library(KrigR)

API_user<-"90893"
API_key<-"2dc19fd6-ef5e-4b81-9ef1-e6f8cd147ce8"

era5_land_Dir.data<-"Data/Raw_ERA5Land/"
era5_Dir.data<-"Data/Raw_ERA5/"

variables_era5<-Variable_List(DataSet = "era5")
variables_era5_land<-Variable_List(DataSet = "era5-land")

name_saved_era5_land<-"era5land_hourly_total_precip"
name_saved_era5<-"era5land_hourly_total_precip"

brasil_map<-read_country(year = 2010, simplified = T, showProgress = FALSE)
extent_map<-extent(brasil_map)

precip_test<-download_ERA(
  Variable = 'total_precipitation', # Specifying the climate variable
  DataSet = 'era5-land', # specifying the DataSet
  DateStart = '2020-01-01', # Date Start
  DateStop = '2020-12-31', # Date Stop
  TResolution = 'hour', # Time Resolution
  TStep = 1, # Time Step
  Extent = extent_map, # Passing the extent object, or shape.file object
  Dir = era5_land_Dir.data, # Directory to be saved the .nc files
  API_User = API_user, # User API
  API_Key = API_key, # Key API
  verbose = TRUE, 
  PrecipFix = TRUE, 
  # Type = 'reanalysis', 
  FileName = paste0(name_saved_era5_land, "_y2020"), 
  Cores = 13)
