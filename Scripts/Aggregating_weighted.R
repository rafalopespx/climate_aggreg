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
