#### generating max, min, heatwave indicator



## data from anual mean, percentiles, per state
threshold_anual_mean <- 
  tmp_inmet_1520 %>% 
  mutate(tmp_dry = ifelse(tmp_dry < 0, NA_real_, tmp_dry),
         year    = lubridate::year(date)) %>% 
  group_by(state, date) %>% 
  summarise(mean_tmp = mean(tmp_dry, na.rm = TRUE),
            year     = first(year)) %>% 
  group_by(state, year) %>% 
  summarise(mean = mean(mean_tmp),
            p90  = quantile(mean_tmp, .900, na.rm = TRUE),
            p925 = quantile(mean_tmp, .925, na.rm = TRUE),
            p95  = quantile(mean_tmp, .950, na.rm = TRUE),
            p975 = quantile(mean_tmp, .975, na.rm = TRUE),
            p99  = quantile(mean_tmp, .990, na.rm = TRUE))


threshold_anual_max <- 
  tmp_inmet_1520 %>% 
  mutate(tmp_dry = ifelse(tmp_dry < 0, NA_real_, tmp_dry),
         year    = lubridate::year(date)) %>% 
  group_by(state, date) %>% 
  summarise(max_tmp = max(tmp_dry, na.rm = TRUE),
            year     = first(year)) %>% 
  group_by(state, year) %>% 
  summarise(mean = mean(max_tmp),
            p90  = quantile(max_tmp, .900, na.rm = TRUE),
            p925 = quantile(max_tmp, .925, na.rm = TRUE),
            p95  = quantile(max_tmp, .950, na.rm = TRUE),
            p975 = quantile(max_tmp, .975, na.rm = TRUE),
            p99  = quantile(max_tmp, .990, na.rm = TRUE))


write_csv(threshold_anual_mean, "Data/INMET/threshold_anual_mean.csv")
write_csv(threshold_anual_max,  "Data/INMET/threshold_anual_max.csv")

## Preparing states to define indicator for 2020

tmp_inmet_2020_state <- 
  tmp_inmet_2020  %>% 
  group_by(state) %>% 
  mutate(tmp_dry = ifelse(tmp_dry < 0, NA_real_, tmp_dry)) %>% 
  group_by(state, date) %>% 
  summarise(mean_tmp = mean(tmp_dry, na.rm = TRUE),
            max_tmp  = max(tmp_dry , na.rm = TRUE))

## choosing, between the 2016 to 2019, highest per percentile, mean
threshold_mean_state <- threshold_anual_mean %>% 
  filter(year >= 2016) %>% 
  group_by(state) %>% 
  summarise(p90  = max(p90 , na.rm = TRUE),
            p925 = max(p925, na.rm = TRUE),
            p95  = max(p95 , na.rm = TRUE),
            p975 = max(p975, na.rm = TRUE),
            p99  = max(p99 , na.rm = TRUE))

## choosing, between the 2016 to 2019, highest per percentile, max
threshold_max_state <- threshold_anual_max %>% 
  filter(year >= 2016) %>% 
  group_by(state) %>% 
  summarise(p90  = max(p90 , na.rm = TRUE),
            p925 = max(p925, na.rm = TRUE),
            p95  = max(p95 , na.rm = TRUE),
            p975 = max(p975, na.rm = TRUE),
            p99  = max(p99 , na.rm = TRUE))


## creating a loop
## HeatWave, 1 , 2, 3 and 4 days long MEAN

### 97.5
states <- as.character(threshold_mean_state$state)

heat_wave_p975_mean_1 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_mean_state$p975[threshold_mean_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, mean_tmp),
    numDays   = 1)
  heat_wave_p975_mean_1[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw1_p975_mean = hw, hw1_number_p975_mean = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_mean_2 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_mean_state$p975[threshold_mean_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, mean_tmp),
    numDays   = 2)
  heat_wave_p975_mean_2[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw2_p975_mean = hw, hw2_number_p975_mean = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_mean_3 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_mean_state$p975[threshold_mean_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, mean_tmp),
    numDays   = 3)
  heat_wave_p975_mean_3[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw3_p975_mean = hw, hw3_number_p975_mean = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_mean_4 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_mean_state$p975[threshold_mean_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, mean_tmp),
    numDays   = 4)
  heat_wave_p975_mean_4[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw4_p975_mean = hw, hw4_number_p975_mean = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_mean <- bind_rows(heat_wave_p975_mean_1) %>% 
  left_join(bind_rows(heat_wave_p975_mean_2), by = c("state", "date", "tmpd")) %>% 
  left_join(bind_rows(heat_wave_p975_mean_3), by = c("state", "date", "tmpd")) %>% 
  left_join(bind_rows(heat_wave_p975_mean_4), by = c("state", "date", "tmpd"))



## HeatWave, 1 , 2, 3 and 4 days long MAX


heat_wave_p975_max_1 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_max_state$p975[threshold_max_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, max_tmp),
    numDays   = 1)
  heat_wave_p975_max_1[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw1_p975_max = hw, hw1_number_p975_max = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_max_2 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_max_state$p975[threshold_max_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, max_tmp),
    numDays   = 2)
  heat_wave_p975_max_2[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw2_p975_max = hw, hw2_number_p975_max = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_max_3 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_max_state$p975[threshold_max_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, max_tmp),
    numDays   = 3)
  heat_wave_p975_max_3[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw3_p975_max = hw, hw3_number_p975_max = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_max_4 <- list()

for (i in 1:length(states)){
  heat_index <- futureheatwaves::IDHeatwavesR(
    threshold = threshold_max_state$p975[threshold_max_state$state==states[i]],
    datafr    = tmp_inmet_2020_state %>% 
      ungroup() %>% 
      filter(state == states[i]) %>% 
      select(date, max_tmp),
    numDays   = 4)
  heat_wave_p975_max_4[[i]] <- heat_index %>% 
    mutate(state = states[i]) %>% 
    rename(hw4_p975_max = hw, hw4_number_p975_max = hw.number)
  print(states[i])
  print(i)
} 

heat_wave_p975_max <- bind_rows(heat_wave_p975_max_1) %>% 
  left_join(bind_rows(heat_wave_p975_max_2), by = c("state", "date", "tmpd")) %>% 
  left_join(bind_rows(heat_wave_p975_max_3), by = c("state", "date", "tmpd")) %>% 
  left_join(bind_rows(heat_wave_p975_max_4), by = c("state", "date", "tmpd"))