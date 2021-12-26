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

no_axis <- theme(axis.title = element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks = element_blank())

# Download sf
municipality_map <- read_municipality(year = 2010,simplified = TRUE, showProgress = FALSE)
health_regions_map <- read_health_region(year = 2013, macro = F, simplified = T, showProgress = FALSE)
# municipality_map

# Read ERA5Land Aggregated
municipality_list_2020<-read_csv("Data/ERA5Land_aggregated/era5land_municipality_t2m_2020.csv.xz")
health_list_2020<-read_csv("Data/ERA5Land_aggregated/era5land_health_regions_t2m_2020.csv.xz")
municipality_list_2020
health_list_2020

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

health_list_2020<-
  health_list_2020 %>% 
  mutate(
    date      = lubridate::ymd(date),
    temp_mean = weighted_mean - 273.15, 
    temp_max = weighted_max - 273.15,
    temp_min = weighted_min - 273.15,
  ) %>% 
  dplyr::select(name_health_region, code_health_region, abbrev_state, code_state, 
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

health_list_2020<-
  health_list_2020 %>% 
  arrange(code_health_region, date) %>% 
  group_by(code_health_region) %>% 
  mutate(
    temp_mean_ma = zoo::rollapply(temp_mean,7, mean, align='right', fill=NA),
    temp_max_ma = zoo::rollapply(temp_max,7, mean, align='right', fill=NA),
    temp_min_ma = zoo::rollapply(temp_min,7, mean, align='right', fill=NA),
    time    = 1:n()
  )

municipality_map_test<-municipality_list_2020 %>% 
  dplyr::select(date, name_muni, code_muni, temp_mean, temp_max, temp_min)

health_map_test<-health_list_2020 %>% 
  dplyr::select(date, name_health_region, code_health_region, temp_mean, temp_max, temp_min)

municipality_map_test <- municipality_map %>% 
  left_join(municipality_map_test, by = c("name_muni", "code_muni"))

health_map_test <- health_regions_map %>% 
  mutate(code_health_region = as.double(code_health_region)) %>% 
  left_join(health_map_test, by = c("name_health_region", "code_health_region"))

outubro<-seq.Date(from = as.Date("2020-10-01", "%Y-%m-%d"), to = as.Date("2020-10-30", "%Y-%m-%d"), by = "day")

dates<-unique(municipality_map_test$date)
dates<-format(dates, "%Y_%m_%d")

# temp_max<-round(as.numeric(municipality_list_2020$temp_max), 2)
# temp_min<-round(as.numeric(municipality_list_2020$temp_min), 2)
# 
# tmean<-municipality_map_test$temp_mean
# tmax<-municipality_map_test$temp_max
# tmin<-municipality_map_test$temp_min

ic95_mean<-c(quantile(municipality_map_test$temp_mean, 
                      probs = 0.0275, na.rm = T), 
             quantile(municipality_map_test$temp_mean, 
                      probs = 0.975, na.rm = T))
ic95_mean<-c(quantile(health_map_test$temp_mean, 
                      probs = 0.0275, na.rm = T), 
             quantile(health_map_test$temp_mean, 
                      probs = 0.975, na.rm = T))
ic95_max<-c(quantile(municipality_map_test$temp_max, 
                     probs = 0.0275, na.rm = T), 
            quantile(municipality_map_test$temp_max, 
                     probs = 0.975, na.rm = T))
ic95_min<-c(quantile(municipality_map_test$temp_min, 
                     probs = 0.0275, na.rm = T), 
            quantile(municipality_map_test$temp_min, 
                     probs = 0.975, na.rm = T))

mean_list<-list()
max_list<-list()
min_list<-list()
# for (i in 1:length(dates)) {
  data<-health_map_test %>% 
    filter(date %in% outubro)
  
  # Mean
  plt_mean<-data %>%
    ggplot(aes(fill = temp_mean, color = temp_mean))+
    geom_sf(size=.1) +
    scale_fill_viridis_c(option = "turbo", 
                         # alpha = 0.8, 
                         aesthetics = c("fill", "color"), 
                         name = "Mean Temperature (Celsius)", 
                         limits = ic95_mean, oob = scales::squish)+
    labs(caption = "Made by @rafalpx, with data from ERA5-Land (ECMWF)")+
    theme_minimal() +
    theme(legend.position = "bottom")+
    no_axis+
  #   theme(legend.position = "bottom",
  #         legend.key.width = unit(4, "cm"), 
  #         legend.title = element_blank(),
  #         # legend.justification = "center",
  #         # legend.title.align = 0.5
  # )+
    facet_wrap(date~., nrow = 6, ncol = 5)
  plt_mean
  # ggsave(plt_mean,
  #        filename = paste0("plots/2020/plot_t2m_mean_weighted_", dates[i], ".png"),
  #        width = 6,
  #        height = 8,
  #        dpi = 300)
  # Max
  plt_max<-data %>%
    ggplot(aes(fill = temp_max, color = temp_max))+
    geom_sf(size=.1) +
    scale_fill_viridis_c(option = "turbo", 
                         # alpha = 0.8, 
                         aesthetics = c("fill", "color"), 
                         name = "Max Temperature (Celsius)")+
    labs(title=dates[i], caption = "Made by @rafalpx, with data from ERA5-Land (ECMWF)")+
    theme_minimal() +
    theme(legend.position = "bottom", 
          legend.key.width = unit(4, "cm"), 
          legend.justification = "center",
          legend.title.align = 0.5)
  # ggsave(plt_max,
  #        filename = paste0("plots/2020/plot_t2m_max_weighted_", dates[i], ".png"),
  #        width = 6,
  #        height = 8,
  #        dpi = 300)
  # Min
  plt_min<-data %>%
    ggplot(aes(fill = temp_min, color = temp_min))+
    geom_sf(size=.1) +
    scale_fill_viridis_c(option = "turbo", 
                         # alpha = 0.8, 
                         aesthetics = c("fill", "color"), 
                         name = "Mean Temperature (Celsius)")+
    labs(title=dates[i], caption = "Made by @rafalpx, with data from ERA5-Land (ECMWF)")+
    theme_minimal() +
    theme(legend.position = "bottom", 
          legend.key.width = unit(4, "cm"), 
          legend.justification = "center",
          legend.title.align = 0.5)
  # ggsave(plt_min,
  #        filename = paste0("plots/2020/plot_t2m_min_weighted_", dates[i], ".png"),
  #        width = 6,
  #        height = 8,
  #        dpi = 300)
  # mean_list[[i]]<-plt_mean
  # max_list[[i]]<-plt_max
  # min_list[[i]]<-plt_min
  # print(paste0(dates[i], " day finished!"))
# }
