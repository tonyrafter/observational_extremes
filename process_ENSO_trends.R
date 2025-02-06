##################################################################
###################     ENSO TRENDS    ###########################
##################################################################


# load packages
library(tidyverse)
# library(maps)
# library(mapdata)
# library(patchwork)
# library(timeplyr)
# library(ggrepel)
# library(broom)
# library(gt)
# library(knitr)
# library(scales)
# library(paletteer)
library(zoo)
# library(sf)
# sf_use_s2(FALSE)
library(slider)
# library(ggrepel)
library(broom)
library(Kendall)



# self-created NINO34 index:
# library(tidync)
# nino34_sst <- tidync('./NINO3.4/HadISST/NINO3.4.nc', what = 'sst')
#   %>% 
#   activate('D0', 'sst') %>% 
#   hyper_tbl_cube()
#   hyper_tibble(select_var = c("sst", "time"))
  
# nc_sst_file <- open.nc('./NINO3.4/HadISST/NINO3.4.nc')
# nino34_sst_NC <- read.nc(nc_sst_file, recursive = TRUE)
#

# nino34_sst_NC


# # using terra:
# library(terra)
# nc_file <- rast('./NINO3.4/HadISST/NINO3.4.nc')

#   # Extract the SST values and corresponding dates
# sst_values <- values(nc_file, mat=FALSE)
# dates <- time(nc_file)  # This will give you the time dimension

# # Create a tibble with dates and SST values
# sst_tibble <- tibble(
#   date = as.Date(dates),  # Convert to Date format
#   nino34_sst = sst_values
# )

#   # create 15-year running mean time series
#   nino34_df <- 
#   sst_tibble %>% 
#     mutate(
#       runmean_15 = slide_dbl(.x = sst_values, .f = mean, .before = 90, .after = 90, .complete = TRUE),
#       runmean_5m = slide_dbl(.x = sst_values, .f = mean, .before = 2, .after = 2, .complete = TRUE),
#       runmean_15y =slide_dbl(.x = runmean_5m, .f = mean, .before = 90, .after = 90, .complete = TRUE),
#       nino34_index_15 = nino34_sst - runmean_15,
#       nino34_index_m  = nino34_sst - runmean_5m,
#       nino34_index_my = nino34_sst - runmean_15y,
#       diff_15 = nino34_index_my - nino34_index_15
#     )
#     # nino34_df
#   # plot of both mean and raw monthly data
#     # nino34_df %>% ggplot(aes(date, runmean_15)) + geom_line(colour = 'blue') + geom_line(aes(date, nino34_sst), colour = 'red', alpha = 0.7)
#   # plot of anomaly series
#   # nino34_df %>% 
#   #   ggplot(aes(date, nino34_index_15)) + 
#   #   geom_line(colour = 'blue', alpha = 0.5) + 
#   #   geom_line(aes(date, nino34_index_my), colour = 'red', alpha = 0.5) + 
#   #   geom_smooth(method = 'lm')

#   # nino34_df %>% ggplot(aes(date, nino34_index_my)) + geom_line(colour = 'darkgreen') + geom_smooth(method = 'lm')
  
#   # nino34_df$season[which(month(nino34_df$date) %in% c(12, 1, 2))] <- 'DJF' # Summer
#   # nino34_df$season[which(month(nino34_df$date) %in% c(3, 4, 5))] <- 'MAM' # Autumn
#   # nino34_df$season[which(month(nino34_df$date) %in% c(6, 7, 8))] <- 'JJA' # Winter
#   # nino34_df$season[which(month(nino34_df$date) %in% c(9, 10, 11))] <- 'SON' # Spring

# # Create a time series of seasonal mean values
# seasonal_mean_nino34 <- nino34_df %>%
#   mutate(season = case_when(
#     month(date) %in% c(12, 1, 2) ~ "DJF",
#     month(date) %in% c(3, 4, 5) ~ "MAM",
#     month(date) %in% c(6, 7, 8) ~ "JJA",
#     month(date) %in% c(9, 10, 11) ~ "SON"
#     ),
#     season_year = if_else(
#       month(date) == 12, as.integer(year(date) + 1), as.integer(year(date))
#   )) %>%
#   group_by(season_year, season) %>%
#   summarise(
#     nino34 = mean(nino34_index_m, na.rm = FALSE))

#   annual_mean_nino34 <- nino34_df %>% 
#     mutate(season_year = as.integer(year(date))) %>% 
#     group_by(season_year) %>% 
#     summarise(nino34 = mean(nino34_index_m, na.rm = FALSE)) %>% 
#     mutate(season = "Annual")
  

#   # mean_nino34 <- full_join(seasonal_mean_nino34, annual_mean_nino34) %>% 
#   #   rename(year = season_year) %>% 
#   #   arrange(year)
#   mean_nino34 <- full_join(seasonal_mean_nino34, annual_mean_nino34) %>% 
#     rename(Value = nino34) %>% 
#     mutate(Year = as.Date(paste0(as.character(season_year),'-01-01')), 
#            year = season_year, .before = season) %>% 
#     ungroup() %>% select(-season_year) %>% 
#     arrange(Year)




# created the time series of mean nino34 index in R script 'create_NINO3.4.R',
# and saved df to file in 'mean_seasonal_nino34.Rdata'
load('./mean_seasonal_nino34.Rdata') # loads into variable 'mean_nino34'
basepath <- "GSDR_analysis/NINO34/"
season_list <- c("DJF", "MAM", "JJA", "SON", "Annual")

# # Plot of each seasonal and annual time series:
# # seasonal_mean_nino34 %>% 
# mean_nino34 %>% 
#   group_by(season) %>% 
#   # ggplot(aes(Year, seasonal_mean_nino34, colour = factor(season))) + 
#   ggplot(aes(year, Value, colour = factor(season))) + 
#   geom_line(linewidth = 1.1) +
#   # scale_colour_viridis_d(name = 'Season', option = 'magma', end = 0.8, 
#   #                        breaks = season_list, labels = season_list)
#   # scale_color_paletteer_d(palette = "palettetown::teamrocket", name = 'Season', 
#   scale_color_paletteer_d(palette = "ochRe::lorikeet", name = 'Season', 
#                           breaks = season_list, labels = season_list)


# Now need to process these seasonal (and annual) time series of nino34 through the trend analysis,
# creating a mann-kendall trend line for nino34 in each 10+ year period from 1960(?) onwards.

test_estimates_nino <- function(df,
  # periodtype = period_type,
  oldest = first_year,
  newest = last_year,
  trend_type = "year") {

  if (trend_type == "year") {
  trend_var <- sym("year")
  # trend_var <- sym("Year")
  }
  # if (trend_type == "temperature") {
  # trend_var <- sym("temperature")
  # }
  df %>% ungroup() %>% filter(between(year, as.integer(oldest), as.integer(newest))) %>% 
  # {if(trend_type == "year") mutate(., Year = as.Date(paste(as.character(!!trend_var),01,01,sep='-'))) else .} %>%
  # mutate(Year = as.Date(paste(as.character(!!trend_var),01,01,sep='-'))) %>%
  group_by(., season, .add = TRUE) %>%
  do(tidy(lm(Value ~ !!trend_var, .))) %>%
  filter(term == trend_var) %>%
  select(-term) %>%
  mutate(p.value = round(p.value, digits = 5)) %>%
  mutate(year_start = oldest, year_end = newest, .before = season) 

}

modified_mk_test <- function(x, ...) {
  # require(zoo)
  ts <- read.zoo(mutate(x, Year = year(Year)) %>% ungroup() %>% select(Year, Value))
  result <- MannKendall(ts, ...)

  tibble(
    p.value_mk = round(result$sl[1], digits = 5)
  )

}

test_estimates_mk_nino <- function(df,
  # periodtype = period_type,
  oldest = first_year, newest = last_year) {

  df %>%
  group_by(season, .add = TRUE) %>%
  group_modify(~ modified_mk_test(.x)) %>%
  mutate(year_start = oldest, year_end = newest, .before = season)

}

first_year <- 1960
last_year <- 2015
min_length <- 10
max_length <- 56

t1 <- now()

for (period_length in seq(min_length, max_length)) {
  t1_loop <- now()
  last_start <- last_year - period_length + 1
  print(paste("Last start year:",last_start))
  for (start in seq(first_year, last_start)) {
    end <- start + period_length - 1

    linear_tests_nino <- test_estimates_nino(mean_nino34, oldest = start, newest = end, trend_type = "year")
    mk_tests_nino <- test_estimates_mk_nino(mean_nino34, oldest = start, newest = end)
    test_results_nino <- left_join(linear_tests_nino, mk_tests_nino, by = c("year_start", "year_end", "season")) %>% 
      rename(Season = season, trend = estimate)
    readr::write_csv(test_results_nino,
      file = paste0(basepath,"tables/nino34_trends-mk_",start,"-",end,"_seasonal.csv"))
    print(paste(test_results_nino$year_start[1], test_results_nino$year_end[1]))

  }
  t2_loop <- now()
  elapsed_time_loop <- t2_loop - t1_loop
  print(paste("Loop time", elapsed_time_loop)
}

t2 <- now()
# print time taken to run through the above loops
elapsed_time <- t2 - t1
print(paste("  Total time:  ", elapsed_time)

# linear_tests_nino <-
#   test_estimates_nino(mean_nino34, oldest = start, newest = end, trend_type = "year")

# mk_tests_nino <- test_estimates_mk_nino(mean_nino34, oldest = start, newest = end)

# test_results_nino <- 
#   left_join(linear_tests_nino, mk_tests_nino, by = c("year_start", "year_end", "season")) %>% 
#   rename(Season = season, trend = estimate)

# readr::write_csv(test_results_nino,
#   file = paste0(basepath,"tables/nino34_trends-mk_",first_year,"-",last_year,"_seasonal.csv"))


