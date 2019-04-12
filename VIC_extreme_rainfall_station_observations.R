# Script to process station rainfall observations.
# Want to show:
# - 95th, 99th percentiles
# - various ARI return values
# at each station with appropriate length and completeness of record.
# Q: What is "appropriate"?

library(tidyverse)
library(lubridate)
# library(ggmap)
# library(maps)
# library(tmaptools)
# library(mapdata)
# library(ggmapstyles)

# Google maps API key: AIzaSyDJeNzaBarybVQ7G4-eZmgD02xAtyOiuBc
# api_key <- 'AIzaSyDJeNzaBarybVQ7G4-eZmgD02xAtyOiuBc'
# register_google(api_key)

##### FUNCTIONS #####
extremes_analysis <- function(station_data_df) {
  
  # Which percentiles to compute
  percentiles <- c(0.5, 0.75, 0.9, 0.95, 0.99)
  # List the return levelw we want to extract, 
  # and their respective annual probabilties
  RLs <- c(2, 5, 10, 20, 50, 100)
  RL_p <- 1 - (1/RLs)
  
  ###### PERCENTILES #######
  # Now to look at more extreme rainfall:
  percentile_rainfall <- quantile(station_data_df$PRCP_P, probs = percentiles)
  max_precip <- max(station_data_df$PRCP_P)
  
  ###### MONMAX #######
  # Okay, now look at the monthly maxima:
  monmax <- station_data_df %>%
    group_by(YEAR, MO) %>%
    summarise(monmax = max(PRCP_P, na.rm = TRUE))
  head(monmax)
  
  monmax <- monmax %>% 
    mutate(Date = ymd(paste(YEAR,MO,'01',sep=' '))) %>% ungroup(year) %>% 
    select(Date, monmax)

  ###### ANNMAX #######
  # Now look at annual maxima:
  annmax <- station_data_df %>%
    group_by(YEAR) %>%
    summarise(annmax = max(PRCP_P, na.rm = TRUE))
  head(annmax)
  
  annmax <- annmax %>% mutate(Date = ymd(paste(YEAR,'01','01',sep=' '))) %>% ungroup(year) %>% 
    select(Date, annmax)
  
  ###### ARI - GEV #######
  
  # GEV on annual maxima
  # Calculate the parameters of a GEV distribution based on our annmax data, and derive return levels based on these parameters for each package.
  library(lmom)
  Lmoments <- samlmu(annmax$annmax)
  gev_params <- pelgev(Lmoments)
  RL_lmom <- quagev(RL_p, gev_params)

  ### Summary
  # Not much difference between the GEV methods (at least for Melb Reg Office)
  # Keep lmom as it's been used in past and is able to provide better estimates
  # of GEV parameters for short periods
  
  # Join percentile table and ARI table in df entry for this station
  # Annoyingly, having to do this one value at a time. Might be a better way??
  results <- station %>% mutate("95%" = percentile_rainfall[length(percentiles)-1], 
                                "99%" = percentile_rainfall[length(percentiles)],
                                "ARI-2" = RL_lmom[1],
                                "ARI-5" = RL_lmom[2],
                                "ARI-10" = RL_lmom[3],
                                "ARI-20" = RL_lmom[4],
                                "ARI-50" = RL_lmom[5],
                                "ARI-100" = RL_lmom[6])
  
  return(results)
}





### Read in station metadata

# Water regulations stations:
col_names_metadata_waterregs <- 
  c("Station Number", "Agency", "Latitude", "Longitude", "Elevation", "First Date", "First Year", 
    "Last Date", "Last Year", "Number of Years", "Station Name")
metadata_waterregs <- 
  read.csv('BoMdata/Station_Metadata_FinalIFD_Water Regulations_Continuous_reimported.csv', 
           col.names = col_names_metadata_waterregs)

# BoM Continuous read stations:
col_names_metadata_bom_cont <- 
  c("Station Number","Latitude","Longitude","Elevation","First Year","Last Year","Number of Years","Station Name")
metadata_bom_cont <- 
  read.csv('BoMdata/Station_Metadata_FinalIFD_Bureau_continuous_allcols.csv',
           col.names = col_names_metadata_bom_cont)

# BoM daily read stations:
col_names_metadata_bom_daily <- 
  c("Station Number","Latitude","Longitude","Elevation","First Year","Last Year","Number of Years","Station Name")
metadata_bom_daily <- 
  read.csv('BoMdata/Station_Metadata_FinalIFD_Daily Read Stations.csv',
           col.names = col_names_metadata_bom_daily)


### Restrict data to VIC domain:
  
# VIC domain bounds:
lon_limits <- c(140,151.25)
lat_limits <- c(-39.5,-32.8)

# Limit our metadata to stations within this VIC domain:
vic_metadata_waterregs <- metadata_waterregs %>% 
  filter(Latitude >= lat_limits[1], Latitude <= lat_limits[2],
         Longitude >= lon_limits[1], Longitude <= lon_limits[2])
vic_metadata_bom_cont <- metadata_bom_cont %>% 
  filter(Latitude >= lat_limits[1], Latitude <= lat_limits[2],
         Longitude >= lon_limits[1], Longitude <= lon_limits[2])
vic_metadata_bom_daily <- metadata_bom_daily %>% 
  filter(Latitude >= lat_limits[1], Latitude <= lat_limits[2],
         Longitude >= lon_limits[1], Longitude <= lon_limits[2])

### Plot up each data type's locations within the domain:

# vic_metadata_bom_cont %>%
#   # filter(Number.of.Years >= 25) %>%
#   ggplot(aes(x = Longitude, y = Latitude)) +
#   annotation_map(map_data("worldHires"), fill = "NA", col = "grey25") +
#   geom_point(alpha = 0.6, col = "blue") +
#   coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   ggtitle("VIC-5 domain: BoM continuous read stations", 
#           subtitle = "No filtering of stations")
# 
# vic_metadata_bom_daily %>%
#   # filter(Number.of.Years >= 25) %>%
#   ggplot(aes(x = Longitude, y = Latitude)) +
#   annotation_map(map_data("worldHires"), fill = "NA", col = "grey25") +
#   geom_point(alpha = 0.6, col = "red") +
#   coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   ggtitle("VIC-5 domain: BoM daily read stations", 
#           subtitle = "No filtering of stations")
# 
# vic_metadata_waterregs %>%
#   # filter(Number.of.Years >= 25) %>%
#   ggplot(aes(x = Longitude, y = Latitude)) +
#   annotation_map(map_data("worldHires"), fill = "NA", col = "grey25") +
#   geom_point(alpha = 0.6, col = "orange") +
#   coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   ggtitle("VIC-5 domain: Water Regulations stations", 
#           subtitle = "No filtering of stations")


### Look for stations that have at least 16 years (80%) of data between 
### 1986 and 2005 (i.e. 80% of years), starting with BoM daily as they have the best coverage:
min_fraction <- 0.8
year_start <- 1986
year_end <- 2005
years_in_period <- (year_end - year_start) + 1
min_years <- min_fraction * years_in_period
max_years_missing <- years_in_period - min_years

vic_metadata_bom_daily_hist <- vic_metadata_bom_daily %>% 
  filter(Number.of.Years >= min_years, First.Year <= year_start + 4, Last.Year >= year_end - 4)

# Do we have any stations that start and end within this period?
num_stations_within_hist_period <- vic_metadata_bom_daily_hist %>% 
  filter(Last.Year <= year_end - max_years_missing, First.Year >= year_start + max_years_missing) %>% 
  summarise(n())
print(paste0("Stations with start and end years within our period ", year_start,
            " to ", year_end, ": ",num_stations_within_hist_period), sep = " ")

num_stations_start_hist_period <- vic_metadata_bom_daily_hist %>% 
  filter(First.Year >= year_start + max_years_missing) %>% 
  summarise(n())
print(paste0("Stations with start years within our period ", year_start,
             " to ", year_end, ": ",num_stations_within_hist_period), sep = " ")

num_stations_end_hist_period <- vic_metadata_bom_daily_hist %>% 
  filter(Last.Year <= year_end - max_years_missing) %>% 
  summarise(n())
print(paste0("Stations with end years within our period ", year_start,
             " to ", year_end, ": ",num_stations_end_hist_period), sep = " ")


### Get station list
vic_bom_daily_hist_stations <- vic_metadata_bom_daily_hist %>% 
  select(Station.Number, Latitude, Longitude, Elevation, Station.Name) 
# %>% 
#   arrange(Station.Name)
# vic_bom_daily_hist_stations %>%
#   ggplot(aes(x = Longitude, y = Latitude, color = Elevation)) +
#   annotation_map(map_data("worldHires"), fill = "NA", col = "grey25") +
#   geom_point(alpha = 0.6) +
#   coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   ggtitle("VIC-5 domain: BoM daily read stations",
#           subtitle = "Stations with 16+ years in historical period (1986-2005)")

# Analysis of daily rainfall extremes at observational stations

# Read in station data

# Can I read in directly from ruby as a mapped drive?
daily_station_dir <- 'Z:/PhD/data/obs/BoMdata/tmp_Bom_Daily/'

# Testing on the Melbourne Regional Office station:
# station <- vic_bom_daily_hist_stations %>% 
#   filter(Station.Name == "MELBOURNE REGIONAL OFFICE")

# Loop though station numbers?
station_numbers <- unique(vic_bom_daily_hist_stations$Station.Number)
results <- data.frame()

for (n in seq(length(station_numbers))) {
# for (n in seq(10)) {
  
  station_number <- station_numbers[n]
  station <- vic_bom_daily_hist_stations %>%
    filter(Station.Number == station_number)
  print(station)

  # Create file name from station ID:
  station_file <- paste0(daily_station_dir,'dr_',sprintf("%06d",station$Station.Number),'QC4.txt')
  
  # Read in data for station
  station_df <- 
    read_csv(station_file)
  
  # Add the date in proper format, and filter for our historical period (1986-2005). 
  # Filter our data frame to only contain years we're interested in
  station_df <- station_df %>% 
    filter(YEAR >= year_start, YEAR <= year_end)
  # Add datetime column:
  station_df <- station_df %>% 
    mutate(Date = ymd(paste(sep = '-', YEAR, MO, DA))) 
  # station_hist %>% ggplot(aes(Date, PRCP_P)) +
  #   geom_point(col = "blue", alpha = 0.2) +
  #   geom_smooth(method = "lm", col = "red") +
  #   # scale_y_log10() +
  #   ggtitle("Rainfall time series at Melbourne Regional Office")
  
  
  ###########################
  ##### DATA EVALUATION #####
  ###########################
  # Put in some evaluation metrics:
  # e.g. 80% of data present in each year, or exclude the year
  # 80% of years with enough data
  # How to treat unflagged accumulations?
  
  
  ###########################
  #### EXTREMES ANALYSIS ####
  ###########################
  
  extremes <- extremes_analysis(station_df)
  
  # Add these results to the results_all data frame
  results <- rbind(results, extremes)
  
}

# Write data frame of results to output file:
outfile <- paste0('rainfall_extremes_BoM-daily_VIC-5_',year_start,'-',year_end,'_no-QC.csv')
write_csv(results, outfile)
