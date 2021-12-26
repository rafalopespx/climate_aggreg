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

# source("Scripts/generating_aggregation.R")

## years
years<-2009:2020

## functions to calculate RH
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

# # d2m names
d2m_years_names<-list.files("Data/Raw_ERA5Land/Daily_aggregated/", pattern = "d2m_sp")
d2m_years_names<-str_c("Data/Raw_ERA5Land/Daily_aggregated/", d2m_years_names)

d2m_years_list<-lapply(d2m_years_names, readRDS)
names(d2m_years_list)<-years

plot(d2m_years_list$`2009`[,1:2,,], col=viridis(10))
# 
# # t2m names
t2m_years_names<-list.files("Data/Raw_ERA5Land/Daily_aggregated/", pattern = "_t2m_")
t2m_years_names<-str_c("Data/Raw_ERA5Land/Daily_aggregated/", t2m_years_names)

t2m_years_list<-lapply(t2m_years_names, readRDS)
names(t2m_years_list)<-years

plot(t2m_years_list$`2010`[,1:2,,], col=viridis(10))


## Operanting on Stars
## Vapour Pressure for t2m, Es, looping
Es_list<-lapply(t2m_years_list, function(x){
  # time_indexes<-st_get_dimension_values(x, 1)
  Es<-st_apply(x, c("x", "y"), 
               FUN = SVP.ClaCla_test, 
               .fname = "time", 
               PROGRESS = T)
  # Es<-st_set_dimensions(Es, 1, values = time_indexes)
  write_rds(Es, 
            file = paste0("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_es2m_y", names(x), ".rds"))
  return(Es)
  }
  )
## Vapour Pressure for d2m, e, looping
e_list<-lapply(d2m_years_list, function(x){
  # time_indexes<-st_get_dimension_values(x, 1)
  e<-st_apply(x, c("x", "y"), 
               FUN = SVP.ClaCla_test, 
               .fname = "time", 
               PROGRESS = T)
  # e<-st_set_dimensions(e, 1, values = time_indexes)
  write_rds(e, 
            file = paste0("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_e2m_y", names(x), ".rds"))
  return(e)
}
)

rh_list<-vector("list", 12)
for (i in 1:length(years)) {
  # ## T2m file
  # t2m<-readRDS(t2m_years_names[i])
  # ## D2m file
  # d2m<-readRDS(d2m_years_names[i])
  # ## Vapour Pressure for t2m
  # time_indexes<-st_get_dimension_values(t2m, 1)
  # Es<-st_apply(t2m, c("x", "y"), 
  #              FUN = SVP.ClaCla_test, 
  #              .fname = "time", 
  #              PROGRESS = T)
  # Es<-st_set_dimensions(Es, 1, values = time_indexes)
  # write_rds(Es, 
  #           file = paste0("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_es2m_y", years[i], ".rds"))
  # 
  # ## Vapour Pressure for d2m
  # e<-st_apply(d2m, c("x", "y"), 
  #             FUN = SVP.ClaCla_test, 
  #             .fname = "time", 
  #             PROGRESS = T)
  # e<-st_set_dimensions(e, 1, values = time_indexes)
  # write_rds(e, 
  #           file = paste0("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_e2m_y", years[i], ".rds"))
  
  ## Relative humidity list
  rh_list[[i]]<-e_list[[i]]/Es_list[[i]]*100
  
  ## Saving files
  # write_stars(rh_list[[i]], 
  #             dsn = paste0("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_rh2m_y", years[i], ".tif")
  # )
  write_rds(rh_list[[i]],
          file = paste0("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_rh2m_y", years[i], ".rds"))
  # rm(list = list("t2m", "d2m", "Es", "e"))
}

#

# rh_test<-read_rds("Data/Raw_ERA5Land/Daily_aggregated/era5land_daily_rh2m_y2009.rds")
# 
# plot(rh_test[,1:2,,], col = turbo(100))
# all.equal(rh_test, rh_list[[1]])
# 
# rh_list_names<-list.files("Data/Raw_ERA5Land/Daily_aggregated/", pattern = "?.rds")
# rh_list_names<-str_c("Data/Raw_ERA5Land/Daily_aggregated/", rh_list_names)
# 
# rh_list2<-lapply(rh_list_names, read_stars)
# 
# rh_one_row<-RH_test(t2m_years_list[[1]], d2m_years_list[[1]], isK = T)
# all.equal(rh, rh_one_row)
# plot(rh[,1:2,,], key.pos = 1, col = turbo(100), lwd = 2, key.length = 0.8)
# rh_contour_lines<-st_contour(st_as_stars(rh), contour_lines = T)
# contour(rh[,1:2,,])
# plot(d2m_list[[1]], max.plot = 12)
# 
# ## Esturturar worflow para ter arquvios por ano, repensar generating_aggeragation, salvar como .nc, e ler como RasterBrick na hora de fazer a agregação espacial
# 
# z.raster = as(rh_list[[1]], "Raster")
# z2 = st_as_stars(z.raster)
# 
# l =  st_contour(z2, 
#                 contour_lines = TRUE)
# plot(l[1], key.pos = 1, pal = sf.colors, lwd = 2, key.length = 0.8)
# 
# 
# index_max = function(x) ifelse(all(is.na(x)), NA, which.max(x))
# st_apply(d2m_years_list[[1]], "geom", index_max) %>%
#   mutate(when = st_get_dimension_values(d2m_years_list[[1]], "time")[.$index_max]) %>%
#   select(when) %>%
#   plot(key.pos = 1, main = "time of maximum dewpoint temperature")
# 
# 
# 
# 
