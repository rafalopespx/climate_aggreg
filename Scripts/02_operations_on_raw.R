rm(list = ls())

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
library(humidity)
library(stars)
library(cubelyr)

source("Scripts/generating_aggregation.R")

# # d2m names
# file.list<-list.files("Data/Raw_ERA5Land/", pattern = "dpt2m_sp")
# file.list<-str_c("Data/Raw_ERA5Land/", file.list)
# 
# # t2m names
# t2m_years_names<-list.files("Data/Raw_ERA5Land/", pattern = "_t2m_")
# t2m_years_names<-str_c("Data/Raw_ERA5Land/", t2m_years_names)
# 
# # t2m and d2m
# d2m_first_year<-read_stars("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2009.nc", 
#                            sub = "d2m", 
#                            proxy = F, 
#                            quiet = T)

# d2m files
# d2m_list<-lapply(file.list, read_stars, sub = "d2m", quiet = T, proxy = T)
d2m_first_year<-read_stars("Data/Raw_ERA5Land/era5land_hourly_dpt2m_sp_y2009.nc", sub = "d2m", proxy = F)
# t2m files
# t2m_years_list<-lapply(t2m_years_names, read_stars, quiet = T, proxy = T)
t2m_first_year<-read_stars("Data/Raw_ERA5Land/era5land_hourly_t2m_y2009_extracted03092021.nc")

# Daily aggegation
d2m_daily<-aggregate(d2m_first_year, "day", FUN = mean)
t2m_daily<-aggregate(t2m_first_year, "day", FUN = mean)


# year-one test
d2m_year_one_month_one<-d2m_list[[1]][,,,1:744]
dates<-st_get_dimension_values(d2m_year_one_month_one, 3)
dates<-as.Date(dates)
dates<-unique(dates)
dates<-dates[1:31]
plot(d2m_year_one_month_one[,,,1:10], downsample = F, col=viridis(10))
d2m_month_one_mean<-aggregate(d2m_year_one_month_one, by = dates, FUN=mean)

# Operating on Layers
e_years_list<-es_years_list<-rh_years_list<-rh_pack_years_list<-vector("list", 12)

# A whole year test
# t2m_year_one<-read_stars(t2m_years_names[1], proxy = FALSE)
# d2m_year_one<-read_stars(file.list[1], proxy = FALSE, sub = "d2m")

SVP.ClaCla_test<-function (t) 
{
  # stopifnot(is.numeric(t))
  Es <- Es.T0 * exp((L/Rw) * (1/T0 - 1/t))
  return(Es)
}

RH_test<-function (t, Td, isK = TRUE) 
{
  # stopifnot(is.numeric(t))
  # stopifnot(is.numeric(Td))
  stopifnot(is.logical(isK))
  if (isK == FALSE) {
    Td <- C2K(Td)
    t <- C2K(t)
  }
  e <- SVP.ClaCla_test(Td)
  Es <- SVP.ClaCla_test(t)
  psi <- e/Es * 100
  return(psi)
}

## Operanting with RasterStacks on its layers  
# dp2tm_test<-split(d2m_list[[1]], 1:48)
# t2m_test<-split(t2m_years_list[[1]], 1:48)
Es<-st_apply(t2m_year_one, 1:2, SVP.ClaCla_test)
e<-st_apply(dp2tm_test, 1:2, SVP.ClaCla_test)
rh<-e/Es*100
plot(rh)
plot(d2m_list[[1]], max.plot = 12)

### Operating with RasterStacks on its layers, but parallelized
cores<-parallel::detectCores()-1
doParallel::registerDoParallel(cores)
system.time({
  rh_test2<-foreach(i=1:48) %dopar% {
    t2m_band<-subset(t2m_years_list[[1]], i)
    dp2tm_band<-subset(dpt2m_years_list[[1]], i)
    RH_test(t = t2m_band, Td = dp2tm_band)
  }
})[3]
stopImplicitCluster()

rh_test2<-brick(rh_test2)

## Operating with RasterBricks
dp2tm_test2<-dpt2m_years_list[[1]]
t2m_test2<-t2m_years_list[[1]]
system.time({
  e<-calc(dp2tm_test2, SVP.ClaCla_test)
  Es<-calc(t2m_test2, SVP.ClaCla_test)
})[3]

### Operating with RasterBricks, but parallelized
cores<-parallel::detectCores()-1
doParallel::registerDoParallel(cores)
system.time({
  rh_test3<-foreach(i=1:8760) %dopar% {
    t2m_band<-subset(t2m_years_list[[1]], i)
    dp2tm_band<-subset(dpt2m_years_list[[1]], i)
    RH_test(t = t2m_band, Td = dp2tm_band)
  }
})[3]
stopImplicitCluster()

rh_test2<-brick(rh_test2)

plot(rh_test)
plot(rh_test2)

all.equal(rh_test$layer.1, rh_test2$layer.1)

cores<-parallel::detectCores()-1
doParallel::registerDoParallel(cores)
# print("Ow loko meo! Paralellizou ae! Vai Kasinaooooo!")
rh_list_year<-
  foreach::foreach (i=1:12, 
                    # .combine = 'bind_rows',
                    .verbose = T
                    # .packages = c("exactextractr", "tibble"), 
                    # .export = "adm_limits"
  ) %dopar% {
  dpt2m_brick<-dpt2m_years_list[[i]]
  t2m_brick<-t2m_years_list[[i]]
  dpt2m_nlayers<-nlayers(dpt2m_years_list[[i]])
  t2m_nlayers<-nlayers(dpt2m_years_list[[i]])
  
  dpt2m_band<-subset(dpt2m_brick, 1:length(dpt2m_nlayers))
  t2m_band<-subset(t2m_brick, 1:length(t2m_nlayers))
  
  rh_
  
}



