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

source("Scripts/generating_aggregation.R")

no_axis <- theme(axis.title = element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks = element_blank())

# Download sf
municipality_map <- read_municipality(year = 2010,simplified = TRUE, showProgress = FALSE)
states_map <- read_state(year = 2010, simplified = T, showProgress = FALSE)
macro_region_map <- read_health_region(year = 2013, macro = T, simplified = T, showProgress = FALSE)
# metro_region_map <- read_metro_area(year = 2018, simplified = T, showProgress = F)
meso_region_map <- read_intermediate_region(year = 2019, simplified = T, showProgress = F)
micro_region_map <- read_immediate_region(year = 2019, simplified = T, showProgress = F)
biomes_map <- read_biomes(year = 2019, simplified = T, showProgress = F)

biomes_map %>% 
  ggplot() +
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Biomes", size=8) +
  theme_minimal() +
  no_axis

municipality_map %>% 
  ggplot() +
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalities", size=8) +
  theme_minimal() +
  no_axis

states_map %>% 
  ggplot() +
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis

macro_region_map %>% 
  filter(code_state == 35) %>% 
  ggplot() +
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Macro Health Regions", size=8) +
  theme_minimal() +
  no_axis

meso_region_map %>% 
  ggplot() +
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Meso Regions", size=8) +
  theme_minimal() +
  no_axis

micro_region_map %>% 
  # filter(code_state == 35) %>% 
  ggplot() +
  geom_sf(data = municipality_map_test %>% 
            filter(date == "2020-03-22"),
          aes(fill = temp_mean),
          size=.1) +
  geom_sf(fill=NA, color="white", size=.15, show.legend = FALSE) +
  scale_fill_viridis_c(option = "turbo", 
                       # alpha = 0.8, 
                       aesthetics = c("fill", "color"), 
                       name = "Mean Temperature (Celsius)")+
  labs(subtitle="Meso Regions", size=8) +
  theme_minimal() +
  no_axis

metro_region_map %>% 
  ggplot() +
  geom_sf(fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Metro Regions", size=8) +
  theme_minimal() +
  no_axis

# municipality_map

# Download raw ERA5 land, hourly, 2tm, crop in Brazil limits
y2020 <- brick("Raw ERA5Land/era5land_hourly_t2m_y2020_extracted16072021.nc")

## same crs
municipality_map <- st_transform(municipality_map, crs = st_crs(y2020))

### deriving daily mean on the raster first
## days
indices <- format(as.Date(names(y2020), format = "X%Y.%m.%d"))
indices_unique<-unique(indices)
indices_smart<-sort(rep(1:366, 24))

system.time(
  d2020mean   <- stackApply(y2020, indices_smart, fun = mean)
)
# user  system elapsed 
# 95.743   2.751  98.438 

names(d2020mean)<-indices_unique

system.time(
  d2020max   <- stackApply(y2020, indices_smart, fun = max)
)

# user  system elapsed 
# 86.312   2.774  89.027 

names(d2020max)<-indices_unique

system.time(
  d2020min   <- stackApply(y2020, indices_smart, fun = min)
)

# user  system elapsed 
# 84.201   2.755  86.916 

## extracting from the raster daily, pre processed
## MEAN
# municipality_list_2020 <- list()


## Parallel For
cores<-detectCores()-1
registerDoParallel(cores)
system.time(
  municipality_list_2020 <- 
    foreach (i=1:nlayers(d2020mean), .combine = 'bind_rows') %dopar% {
      # band <- subset(d2020mean, i)
      area_weights<-raster::area(d2020mean)
      date <- indices_unique[i]
      mean <- exact_extract(subset(d2020mean, i), municipality_map, "weighted_mean",  
                         weights = area_weights,
                         append_cols = TRUE) ## select columns if dont want all
      max <- exact_extract(subset(d2020max, i), municipality_map, "weighted_mean",  
                            weights = area_weights,
                            append_cols = TRUE) ## select columns if dont want all
      min <- exact_extract(subset(d2020min, i), municipality_map, "weighted_mean",  
                           weights = area_weights,
                           append_cols = TRUE) ## select columns if dont want all
      c <- tibble(tibble(date), mean, 
                  weighted_max = max$weighted_mean, 
                  weighted_min = min$weighted_mean)
    }
)
stopImplicitCluster()
# user   system  elapsed 
# 4420.800   62.806  441.832

# municipality_list_2020<-municipality_list_2020_foreach

# Putting on Celsius scale
municipality_list_2020 <- 
  municipality_list_2020 %>% 
  mutate(
    date      = lubridate::ymd(date),
    temp_mean = weighted_mean - 273.15, 
    temp_max = weighted_max - 273.15,
    temp_min = weighted_min - 273.15,
  ) %>% 
  dplyr::select(name_muni, code_muni, abbrev_state, code_state, 
                date, temp_mean, temp_max, temp_min)

# Arranging by municipality code
municipality_list_2020 <- 
  municipality_list_2020 %>% 
  arrange(code_muni, date) %>% 
  group_by(code_muni) %>% 
  mutate(
    temp_mean_ma = zoo::rollapply(temp_mean,7, mean, align='right', fill=NA),
    temp_max_ma = zoo::rollapply(temp_max,7, mean, align='right', fill=NA),
    temp_min_ma = zoo::rollapply(temp_min,7, mean, align='right', fill=NA),
    time    = 1:n()
  )

# Saving
write_csv(x = municipality_list_2020, file = "ERA5Land_aggregated/era5land_municipality_t2m_2020.csv")

# saveRDS(municipality_list_2020_pre_mean, "~/Dropbox/AirPollution/Excess_Brazil_HeatWaveCOVID/Data/ERA5/Processed_Data/municipality_list_2020_pre_mean.rds", compress = TRUE)

# layers_test<-36
# 
# system.time(
#   for (i in 1:layers_test){
#     band <- subset(d2020mean, i)
#     
#     date <- names(band)
#     b <- exact_extract(band, municipality_map, "weighted_mean",  
#                        weights = raster::area(band),
#                        append_cols = TRUE) ## select columns if dont want all
#     c <- tibble(tibble(date), b)
#     
#     municipality_list_2020_pre_mean[[i]] <- c
#   }
# )
# 
# # user  system elapsed 
# # 188.105  10.879 197.148 
# 
# municipality_list_2020_for <- bind_rows(municipality_list_2020_pre_mean)