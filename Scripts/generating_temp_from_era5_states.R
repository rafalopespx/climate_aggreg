## Agreggatting ERA5land
library(geobr)
library(tidyverse)
library(sf)
library(raster)
library(ncdf4)
library(viridis)
library(patchwork)
library(exactextractr)

no_axis <- theme(axis.title = element_blank(),
                   axis.text =  element_blank(),
                   axis.ticks = element_blank())

# Download sf
states_map    <- read_state(year = 2010,simplified = TRUE, showProgress = FALSE)

states_map_plot<-
  ggplot() +
  geom_sf(data=states_map, color="#2D3E50", fill="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis
states_map_plot

# Download raw ERA5 land, hourly, 2tm, crop in Brazil limits

y2020 <- brick("Raw ERA5Land/era5land_hourly_t2m_y2020_extracted16072021.nc")


## same crs

states_map <- st_transform(states_map, crs = st_crs(y2020))

## extracting
states_list_2020 <- list()

# Benchmarking
layers_test<-100
## Sequential for
system.time({
  for (i in 1:layers_test){
    band <- subset(y2020, i)
    
    date_time <- getZ(band)
    b <- exact_extract(band, states_map, "weighted_mean",  
                       weights = raster::area(band),
                       append_cols = TRUE) ## select columns if dont want all
    c <- tibble(tibble(date_time), b)
    
    states_list_2020[[i]] <- c
  } 
})[3]
# elapsed 
# 16.773

states_list_2020_for <- bind_rows(states_list_2020)

## Parallel For
library(foreach)
library(doParallel)
cores<-detectCores()-1
registerDoParallel(cores)
states_list_2020 <- list()
system.time({
  states_list_2020<-foreach (i=1:nlayers(y2020), .combine = 'bind_rows') %dopar% {
    band <- subset(y2020, i)
    date_time <- getZ(band)
    b <- exact_extract(band, states_map, "weighted_mean",  
                       weights = raster::area(band),
                       append_cols = TRUE) ## select columns if dont want all
    c <- tibble(tibble(date_time), b)
  }
})[3]
# elapsed 
# 3.244
stopImplicitCluster()

# states_list_2020_foreach <- bind_rows(states_list_2020)
# 
# all.equal(states_list_2020_for, states_list_2020_foreach)
# TRUE

states_list_2020 <- 
states_list_2020 %>% 
  mutate(
    date_time = lubridate::parse_date_time(date_time, orders = "Ymd HMS"),
    date      = lubridate::as_date(date_time),
    temp      = weighted_mean - 273.15
    ) %>% 
  group_by(abbrev_state, date) %>% 
  summarise(
    code_state  = first(code_state),
    code_region = first(code_region),
    name_region = first(name_region),
    temp_mean = mean(temp),
    temp_max  = max(temp),
    temp_min  = min(temp)
  ) %>% 
  dplyr::select(name_region, code_region, abbrev_state, code_state, 
         date, temp_mean, temp_max, temp_min)


states_list_2020 <- 
  states_list_2020 %>% 
  arrange(abbrev_state, date) %>% 
  group_by(abbrev_state) %>% 
  mutate(
    temp_mean_ma = zoo::rollapply(temp_mean,7, mean, align='right', fill=NA),
    temp_max_ma  = zoo::rollapply(temp_max ,7, mean, align='right', fill=NA),
    temp_min_ma  = zoo::rollapply(temp_min ,7, mean, align='right', fill=NA),
    time    = 1:n()
  )

saveRDS(states_list_2020, "~/Dropbox/AirPollution/Excess_Brazil_HeatWaveCOVID/Data/ERA5/Processed_Data/states_list_2020.rds", compress = TRUE)



## extracting from the raster daily, pre processed
states_list_2020_pre <- list()

for (i in 1:nlayers(d2020mean)){
  band <- subset(d2020mean, i)
  
  date <- names(band)
  b <- exact_extract(band, states_map, "weighted_mean",  
                     weights = raster::area(band),
                     append_cols = TRUE) ## select columns if dont want all
  c <- tibble(tibble(date), b)
  
  states_list_2020_pre[[i]] <- c
}

states_list_2020_pre <- bind_rows(states_list_2020_pre)

states_list_2020_pre <- 
  states_list_2020_pre %>% 
  mutate(
    date      = lubridate::ymd(str_sub(date, 7, 16)),
    temp_mean = weighted_mean - 273.15
  ) %>% 
  dplyr::select(name_region, code_region, abbrev_state, code_state, 
                date, temp_mean)


states_list_2020_pre <- 
  states_list_2020_pre %>% 
  arrange(abbrev_state, date) %>% 
  group_by(abbrev_state) %>% 
  mutate(
    temp_mean_ma = zoo::rollapply(temp_mean,7, mean, align='right', fill=NA),
    time    = 1:n()
  )

saveRDS(states_list_2020_pre, "~/Dropbox/AirPollution/Excess_Brazil_HeatWaveCOVID/Data/ERA5/Processed_Data/states_list_2020_pre_mean.rds", compress = TRUE)
