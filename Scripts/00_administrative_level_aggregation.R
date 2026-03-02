## Agreggatting ERA5land
library(geobr)
library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
# library(viridis)
# library(patchwork)
library(exactextractr)
library(foreach)
library(doParallel)
# library("cowplot")
# library("plotly")
# library("gganimate")
# library("RColorBrewer")
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

# Read in files
years<-seq(2021, 2024, 1)
files_names<-list.files(path = "Data/", 
                           pattern = "202", full.names = T)

## Extent of Brazil
brazil <- read_country(year = 2020,simplified = F)

years_list<-lapply(files_names, function(x){
  y<-brick(x)
  # y<-as(y, "Raster")
  return(y)
})
names(years_list) <- years

# Municipality level aggregations
# Download sf
municipality_map <- read_municipality(year = 2020, simplified = FALSE, showProgress = FALSE, cache = FALSE)
for (i in names(years_list)) {
  
  # Define the ID columns you want to keep (non-pivot columns)
  id_cols <- c("code_muni", "name_muni", "code_state", "abbrev_state", 
               "name_state", "code_region", "name_region")
  
  adm_limits <- municipality_map
  dmean <- years_list[[i]]
  adm_limits <- st_transform(adm_limits, st_crs(dmean))
  
  indexes <- seq(as.Date(paste0(i, "-01-01")), 
                 as.Date(paste0(i, "-12-31")), 
                 "1 day")
  
  cat("Initializing year: ", i, "\n")
  
  mean_weighted <- exact_extract(dmean, adm_limits, 'weighted_mean', weights = area(dmean))
  mean_weighted <- setNames(mean_weighted, as.character(indexes))
  
  adm_aggreg <- cbind(adm_limits |> st_drop_geometry(), mean_weighted)
  
  # Dynamically select only id_cols that actually exist in the data frame
  existing_id_cols <- intersect(id_cols, names(adm_aggreg))
  
  # Pivot using the detected date columns (anything not in id_cols)
  adm_aggreg <- adm_aggreg |> 
    pivot_longer(
      cols = -any_of(existing_id_cols),  # pivots everything not in id list
      names_to = "date",
      values_to = "weighted_mean"
    ) |> 
    mutate(date = as.Date(date))  # ensure date column is Date type, not character
  
  cat("Saving .csv\n")
  vroom::vroom_write(x = adm_aggreg, 
            file = paste0("ERA5Land_aggregated/era5land_municipality_t2m_", i, ".csv.xz"))
   cat("Finisehd for year: ", i, "\n")
}

# # States level aggregations
# # Download sf
# states_map <- read_state(year = 2010, simplified = T, showProgress = FALSE)
# for (i in names(years_list)) {
#   adm_aggreg<-
#     # generating_aggregation(climate_file = years_list[[i]],
#     #                        adm_limits = states_map,
#     #                        year = i)
#   # # t2m
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_states_t2m_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # rh
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5_aggregated/era5_states_rh_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # d2m
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_states_d2m_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # precip
#   # write_csv(x = adm_aggreg,
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_states_precip_", i, ".csv.xz"),
#   #                         compression = -9))
#   # rh2m
#   generating_aggregation(climate_file = years_list[[i]],
#                          adm_limits = states_map,
#                          year = i,
#                          hourly = FALSE)
#   write_csv(x = adm_aggreg,
#             file = xzfile(paste0("Data/ERA5Land_aggregated/era5_states_rh2m_", i, ".csv.xz"),
#                           compression = -9))
#   # beep(4)
# }
# 
# # Macro Health regions level aggregations
# # Download sf
# macro_health_map <- read_health_region(year = 2013, macro = T, simplified = T, showProgress = F)
# 
# # macro_health 
# for (i in names(years_list)) {
#   adm_aggreg<-
#     # generating_aggregation(climate_file = years_list[[i]], 
#     #                        adm_limits = macro_health_map, 
#     #                        year = i)
#   # # t2m
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_macro_health_t2m_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # rh
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5_aggregated/era5_macro_health_rh_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # d2m
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_macro_health_d2m_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # precip
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_macro_health_precip_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # rh2m
#   generating_aggregation(climate_file = years_list[[i]],
#                          adm_limits = states_map,
#                          year = i,
#                          hourly = FALSE)
#   write_csv(x = adm_aggreg,
#             file = xzfile(paste0("Data/ERA5Land_aggregated/era5_macro_health_rh2m_", i, ".csv.xz"),
#                           compression = -9))
# }
# 
# # health_regions 
# health_regions_map <- read_health_region(year = 2013, macro = F, simplified = T, showProgress = F)
# # datas_perdidas<-c(names(years_list)[9:12])
# for (i in names(years_list)) {
#   adm_aggreg<-
#     # generating_aggregation(climate_file = years_list[[i]], 
#     #                        adm_limits = health_regions_map, 
#     #                        year = i)
#   # # t2m
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5land_health_regions_t2m_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # rh
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5_aggregated/era5_health_regions_rh_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # d2m
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_health_regions_d2m_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # precip
#   # write_csv(x = adm_aggreg, 
#   #           file = xzfile(paste0("Data/ERA5Land_aggregated/era5_health_regions_precip_", i, ".csv.xz"), 
#   #                         compression = -9))
#   # rh2m
#   generating_aggregation(climate_file = years_list[[i]],
#                          adm_limits = states_map,
#                          year = i,
#                          hourly = FALSE)
#   write_csv(x = adm_aggreg,
#             file = xzfile(paste0("Data/ERA5Land_aggregated/era5_health_regions_rh2m_", i, ".csv.xz"),
#                           compression = -9))
#   
# }
# 
# #