# using terra:
library(tidyverse)
library(terra)
library(slider)
library(paletteer)
nc_file <- rast('./NINO3.4/HadISST/NINO3.4.nc')

  # Extract the SST values and corresponding dates
sst_values <- values(nc_file, mat=FALSE)
dates <- time(nc_file)  # This will give you the time dimension

# Create a tibble with dates and SST values
sst_tibble <- tibble(
  date = as.Date(dates),  # Convert to Date format
  nino34_sst = sst_values
)

# create 15-year running mean time series
nino34_df <- 
sst_tibble %>% 
  mutate(
    runmean_15 = slide_dbl(.x = sst_values, .f = mean, .before = 90, .after = 90, .complete = TRUE), # 15y_mean_SST
    runmean_5m = slide_dbl(.x = sst_values, .f = mean, .before = 2, .after = 2, .complete = TRUE),   # NINO34 SST
    runmean_15y =slide_dbl(.x = runmean_5m, .f = mean, .before = 90, .after = 90, .complete = TRUE), # 15y_mean_NINO34
    nino34_index_15 = nino34_sst - runmean_15,      # SST - 15y_mean
    nino34_index_m_old  = nino34_sst - runmean_5m,  # SST - 5_month_mean  (creates NINO34 index? WRONG)
    nino34_index_m2  = runmean_5m - runmean_15y,     # 5month_mean_SST - 15y_mean (detrended NINO34) USE THIS ONE
    nino34_index_m  = runmean_5m - runmean_15,     # 5month_mean_SST - 15y_mean (detrended NINO34) USE THIS ONE
    nino34_index_my = nino34_sst - runmean_15y,     # SST - 15y_mean_of_5month_SST (detrended SST, not NINO34 index though)
    diff_15 = nino34_index_my - nino34_index_15,
    diff_m = nino34_index_m - nino34_index_m2
  )
  # nino34_df
  # nino34_df %>% ggplot(aes(date, nino34_index_m)) + geom_line(colour = 'blue') + geom_line(aes(date, nino34_index_m_old), colour = 'red', alpha = 0.7)
    # these are quite different!

  # nino34_df %>% ggplot(aes(date, runmean_15)) + geom_line(colour = 'blue') + geom_line(aes(date, runmean_15y), colour = 'red', alpha = 0.7)

# plot of both mean and raw monthly data
  nino34_df %>% ggplot(aes(date, runmean_15)) + geom_line(colour = 'blue') + 
    geom_line(aes(date, nino34_sst), colour = 'red', alpha = 0.7) +
    ggtitle('NINO 3.4 SST with 15-year running mean')
# # plot of anomaly series after removal of 15yr mean
# nino34_df %>% 
#   ggplot(aes(date, nino34_index_15)) + 
#   geom_line(colour = 'blue', alpha = 0.5) + 
#   geom_line(aes(date, nino34_index_my), colour = 'red', alpha = 0.5) + 
#   geom_smooth(method = 'lm')

# plot of SST - 15y_mean_of_5month_SST (detrended SST, not NINO34 index though)
# nino34_df %>% ggplot(aes(date, nino34_index_my)) + geom_line(colour = 'darkgreen') + geom_smooth(method = 'lm')

# plot of 5month_mean_SST - 15y_mean (detrended NINO34) USE THIS ONE
nino34_df %>% ggplot(aes(date, nino34_index_m)) +
  geom_line(colour = 'darkblue') +
  geom_smooth(method = 'lm') +
  ggtitle('NINO34 index', 
          subtitle = '5 month running mean of SST detrended by 15-year running mean of NINO3.4 region SST')

# nino34_df %>% ggplot(aes(date, diff_15)) + geom_line(colour = 'brown')
# nino34_df %>% ggplot(aes(date, diff_m)) + geom_line(colour = 'orange')

# nino34_df$season[which(month(nino34_df$date) %in% c(12, 1, 2))] <- 'DJF' # Summer
# nino34_df$season[which(month(nino34_df$date) %in% c(3, 4, 5))] <- 'MAM' # Autumn
# nino34_df$season[which(month(nino34_df$date) %in% c(6, 7, 8))] <- 'JJA' # Winter
# nino34_df$season[which(month(nino34_df$date) %in% c(9, 10, 11))] <- 'SON' # Spring

# Create a time series of seasonal mean values
seasonal_mean_nino34 <- nino34_df %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "DJF",
    month(date) %in% c(3, 4, 5) ~ "MAM",
    month(date) %in% c(6, 7, 8) ~ "JJA",
    month(date) %in% c(9, 10, 11) ~ "SON"
    ),
    season_year = if_else(
      month(date) == 12, as.integer(year(date) + 1), as.integer(year(date))
  )) %>%
  group_by(season_year, season) %>%
  summarise(
    nino34 = mean(nino34_index_m, na.rm = FALSE))

  annual_mean_nino34 <- nino34_df %>% 
    mutate(season_year = as.integer(year(date))) %>% 
    group_by(season_year) %>% 
    # summarise(nino34 = mean(nino34_index_my, na.rm = FALSE)) %>% 
      summarise(nino34 = mean(nino34_index_m, na.rm = FALSE)) %>% 
    mutate(season = "Annual")
  
seasonal_mean_nino34 %>% ggplot(aes(season_year, nino34, colour = as.factor(season))) + 
  geom_line() + geom_smooth(method = 'lm') + 
  scale_color_paletteer_d(palette = "nbapalettes::thunder_tribute", direction = -1, name = "Season", ) +
  facet_wrap(~ season) + theme(legend.position = "none") +
  ggtitle("Seasonal NINO34 after annual detrending")


  # mean_nino34 <- full_join(seasonal_mean_nino34, annual_mean_nino34) %>% 
  #   rename(year = season_year) %>% 
  #   arrange(year)
  mean_nino34 <- full_join(seasonal_mean_nino34, annual_mean_nino34) %>% 
    rename(Value = nino34) %>% 
    mutate(Year = as.Date(paste0(as.character(season_year),'-01-01')), 
           year = season_year, .before = season) %>% 
    ungroup() %>% select(-season_year) %>% 
    arrange(Year)

save(mean_nino34, file = './mean_seasonal_nino34.Rdata')
