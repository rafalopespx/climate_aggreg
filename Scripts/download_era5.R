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

name_saved_era5_land<-"era5land_hourly_t2m"
name_saved_era5<-"era5land_hourly_t2m"

brasil_map<-read_country(year = 2010, simplified = T, showProgress = FALSE)
extent_map<-extent(brasil_map)

t2m_data<-CDownloadS(Variable = "2m temperature", 
                     DataSet = "reanalysis-era5-land", 
                     DateStart = "2000-01-01", 
                     DateStop = "2001-01-01", 
                     Extent = brasil_map,
                     Dir = era5_land_Dir.data,
                     FileName = name_saved_era5_land, 
                     FileExtension = ".nc", 
                     API_User = API_user, 
                     API_Key = API_key)
