rm(list = ls())
gc()

## Loading
library(tidyverse)
library(vroom)


## List all the aggregated t2m files from ERA5Land
list.files <- list.files(path = "ERA5Land_aggregated/", pattern = "era5land_municipality_t2m_", full.names = T)

weighted_aggreg <- lapply(list.files, vroom::vroom) |> bind_rows()

vroom_write(file = "Data/era5land_municipality_t2m_2000_2025.csv.xz",
            x = weighted_aggreg)

weighted_aggreg_month <- weighted_aggreg |> 
  mutate(date_month = make_date(year = year(date), month = month(date), day = "01")) |> 
  group_by(date_month, code_muni) |> 
  summarise(monthly_mean = mean(weighted_mean),
            monthly_min = min(weighted_mean),
            monthly_max = max(weighted_mean))

vroom_write(file = "Data/era5land_municipality_t2m_monthly_2000_2025.csv.xz",
            x = weighted_aggreg_month)

weighted_aggreg_month <- weighted_aggreg_month |> 
  mutate(month = month(date_month), yeear = year(date_month)) |> 
  left_join(adm_limits |> st_drop_geometry()) |> 
  filter(!is.na(name_region))

weighted_aggreg_month <- vroom("Data/era5land_municipality_t2m_monthly_2000_2025.csv.xz") |> 
  mutate(monthly_mean = monthly_mean - 273.15,
    anomaly = (monthly_mean - mean(monthly_mean, na.rm = T))) |> 
  mutate(name_region = case_when(substr(code_muni, 1,1) == "1" ~ "North",
                                 substr(code_muni, 1,1) == "2" ~ "Northeast",
                                 substr(code_muni, 1,1) == "5" ~ "Center-West",
                                 substr(code_muni, 1,1) == "3" ~ "Southeast",
                                 substr(code_muni, 1,1) == "4" ~ "South"))

ggplot(data = weighted_aggreg_month|> 
         mutate(name_region = factor(name_region, 
                                     levels = c("North", "Northeast", "Center-West", "Southeast", "South"))),
       aes(x = date_month, y = anomaly, color = anomaly))+
  geom_line()+
  theme_void()+
  khroma::scale_color_smoothrainbow(name = "Mean temperature")+
  coord_polar()+
  facet_wrap(~name_region, nrow = 1)+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = unit(1, "cm"))+
  guides(fill = guide_colorbar())

## Drawing a comparison with CRUTS for Brazil 2000-2024
library(tidyverse)
library(vroom)
cruts <- vroom::vroom("Data/brazil_muni_2000_2024_monthly_CRU.csv") |>
  mutate(date_month = make_date(year = year, month = month, day = "01")) |> 
  select(date_month, code_muni, year, month, temp) |> 
  rename(t2m_cru = temp)
  
era5ts <- vroom::vroom("Data/era5land_municipality_t2m_monthly_2000_2025.csv.xz") |> 
  mutate(year = year(date_month), month = month(date_month), monthly_mean = monthly_mean - 273.15) |> 
  select(date_month, code_muni, year, month, monthly_mean) |> 
  rename(t2m_era = monthly_mean)

aggreg_cruts_era5 <- era5ts |> dplyr::left_join(cruts) |> 
  mutate(name_region = case_when(substr(code_muni, 1,1) == "1" ~ "North",
                                 substr(code_muni, 1,1) == "2" ~ "Northeast",
                                 substr(code_muni, 1,1) == "5" ~ "Center-West",
                                 substr(code_muni, 1,1) == "3" ~ "Southeast",
                                 substr(code_muni, 1,1) == "4" ~ "South"))

aggreg_cruts_era5 |> 
  filter(year <= 2024, !is.na(t2m_cru), !is.na(t2m_era)) |> 
  mutate(diff = (t2m_era - t2m_cru), 
         name_region = factor(name_region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) |> 
  ggplot(aes(x = t2m_era, y = t2m_cru, color = if_else(diff>0, "bias CRUTS", "bias ERA5"), group = year))+
  geom_abline(slope = 1, intercept = 0)+
  geom_point(shape = '.', alpha = 0.20)+
  facet_wrap(~name_region, nrow = 1)+
  theme_minimal()+
  theme(legend.position = "none")

library(okcolors)
library(ggpubr)
my_oka <- c("#45924C","#F0CC36","#E96CD2","#F44F49","#4561CF")

aggreg_cruts_era5 |> 
  mutate(name_region = factor(name_region, levels = c("North", "Northeast", "Center-West", "Southeast", "South"))) |> 
  pivot_longer(cols = c(t2m_cru, t2m_era)) %>%
  ggplot(mapping = aes(x = value, y = ..count..,
                       color = name, fill = name)) +
  geom_histogram(stat = "bin", bins = 100,
                 alpha = 0.7,
                 position = "identity") +
  scale_color_manual(values = alpha(c( my_oka[1:2], "gray40"), 1)) + 
  scale_fill_manual(values = alpha(c( my_oka[1:2], "gray40"), 0.6)) + 
  labs(x = "Temperature range\n[°C]", y = "Count", color = "dataset", 
       fill = "dataset",
       title = "Comparing CRUTS x ERA5Land", 
       subtitle = "Overall distribution shown in gray")+
  theme_minimal()+
  facet_wrap(~name_region, ncol = 1, scales = "free_y")


