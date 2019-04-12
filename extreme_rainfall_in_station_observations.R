# Script to process station rainfall observations.
# Want to show:
# - 95th, 99th percentiles
# - various ARI return values
# at each station with appropriate length and completeness of record.
# Q: What is "appropriate"?

library(tidyverse)
library(lubridate)
library(stringi)

# Google maps API key: AIzaSyDJeNzaBarybVQ7G4-eZmgD02xAtyOiuBc
# api_key <- 'AIzaSyDJeNzaBarybVQ7G4-eZmgD02xAtyOiuBc'
# register_google(api_key)

plotting <- FALSE
source("C:/Users/raf018/Documents/Programming and Computing/R/gmtColorPalettes.R")

##### SETUP VARIABLES FOR THIS ANALYSIS
# Set period, domain and which dataset we are looking at
year_start <- 1986
year_end <- 2005
domain <- "VIC-5"
# provider <- "BoM-daily"
# provider <- "BoM-continuous"
provider <- "BoM-continuous"

# Quality control settings:
# Minimum fraction of years in the period with available data:
min_fraction <- 0.8


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
  percentile_rainfall <- quantile(station_data_df$Value, probs = percentiles, na.rm = TRUE)
  max_precip <- max(station_data_df$Value)
  
  ###### MONMAX #######
  # Okay, now look at the monthly maxima:
  monmax <- station_data_df %>%
    group_by(YEAR, MO) %>%
    summarise(monmax = max(Value, na.rm = TRUE))
  head(monmax)
  
  monmax <- monmax %>% 
    mutate(Date = ymd(paste(YEAR,MO,'01',sep=' '))) %>% ungroup(year) %>% 
    select(Date, monmax)
  
  ###### ANNMAX #######
  # Now look at annual maxima:
  annmax <- station_data_df %>%
    group_by(YEAR) %>%
    summarise(annmax = max(Value, na.rm = TRUE))
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
  # evplot(y = annmax$annmax)
  # evdistq(quagev, gev_params)
  
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
  
  return(list(results,gev_params))
}

get_domain_bounds <- function(domain = c("VIC-5", "SYD-2")) {
  if (domain == "VIC-5") {
    lon_limits <- c(140,151.25)
    lat_limits <- c(-39.5,-32.8)
  }
  if (domain == "SYD-2") {
    lon_limits <- c(148.7,153.2)
    lat_limits <- c(-35.9,-31.9)
  }
  limits <- list(lon_limits, lat_limits)
  return(limits)
}

extract_metadata <- function(provider = c("BoM-daily", "BoM-continuous", "Water-Regulations"), limits) {
  # Depending on the provider, the required colnames is different:
  if (provider == "BoM-daily") {
    provider_label <- 'BoM daily read'
    print(paste0("Reading ",provider_label," station metadata"))
    column_names <- 
      c("Station Number","Latitude","Longitude","Elevation","First Year","Last Year",
        "Number of Years","Station Name")
    file_name <- 'BoMdata/Station_Metadata_FinalIFD_Daily Read Stations.csv'
    datadir <- 'tmp_Bom_Daily'
  }
  if (provider == "BoM-continuous") {
    provider_label <- 'BoM continuous pluviograph'
    print(paste0("Reading ",provider_label," station metadata"))
    column_names <- 
      c("Station Number","Latitude","Longitude","Elevation","First Year","Last Year",
        "Number of Years","Station Name")
    file_name <- 'BoMdata/Station_Metadata_FinalIFD_Bureau_continuous_allcols.csv'
    datadir <- 'tmp_BoM-pluvi'
  }
  if (provider == "Water-Regulations") {
    provider_label <- 'Water Regulations pluviograph'
    print(paste0("Reading ",provider_label," station metadata"))
    column_names <- 
      c("Station Number","Agency","Latitude","Longitude","Elevation","First Date",
        "First Year","Last Date","Last Year","Number of Years","Station Name")
    file_name <- 'BoMdata/Station_Metadata_FinalIFD_Water Regulations_Continuous_reimported.csv'
    datadir <- 'tmp_Regs-pluvi'
  }
  
  # Open file and extract metadata
  metadata <- read.csv(file_name, col.names = column_names)
  
  # Limit our metadata to stations within our domain:
  metadata <- metadata %>% 
    filter(Longitude >= limits[[1]][1], Longitude <= limits[[1]][2],
           Latitude >=  limits[[2]][1], Latitude <=  limits[[2]][2])
  
  metadata_list <- list(metadata, datadir, provider_label)
  
  return(metadata_list)
}




### Obtain the limits for our domain
limits <- get_domain_bounds(domain)

### Read in station metadata
metadata_list <- extract_metadata(provider = provider, limits = limits)

metadata <- metadata_list[[1]]
datadir <- metadata_list[[2]]
provider_labels <- metadata_list[[3]]

### Initial QC - length of record
years_in_period <- (year_end - year_start) + 1
min_years <- min_fraction * years_in_period
max_years_missing <- years_in_period - min_years

# Have to have a minimum number of years (usually 80%) within the period,
# so start by ensuring the station has at least that number of years,
# and also has not started or ended its record too far from the start/end of the period

# Step 1 - ensure stations satisfy minimum years, and don't start/end too far within period
metadata <- metadata %>% 
  filter(Number.of.Years >= min_years, 
         First.Year <= year_start + max_years_missing, 
         Last.Year >= year_end - max_years_missing)



# Do we have any stations that start and end within this period?
num_stations_within_period <- metadata %>% 
  filter(Last.Year <= year_end - max_years_missing, First.Year >= year_start + max_years_missing) %>% 
  summarise("start_end_within" = n())

num_stations_start_period <- metadata %>% 
  filter(First.Year >= year_start + max_years_missing) %>% 
  summarise("start_within" = n())

num_stations_end_period <- metadata %>% 
  filter(Last.Year <= year_end - max_years_missing) %>% 
  summarise("end_within" = n())

station_period_summary <- 
  cbind(num_stations_within_period, num_stations_start_period, num_stations_end_period)


### Get station list
if (provider == 'Water-Regulations') {
  stations <- metadata %>% 
    select(Station.Number, Agency, Latitude, Longitude, Elevation, Station.Name) 
} else {
    stations <- metadata %>% 
      select(Station.Number, Latitude, Longitude, Elevation, Station.Name) 
}
# pal <- gmtColors(pal.name = "haxby")
# station_plot <- stations %>%
#   arrange(Station.Name) %>%
#   ggplot(aes(x = Longitude, y = Latitude, color = Elevation)) +
#   scale_color_gradientn(colours = pal) +  
#   annotation_map(map_data("worldHires"), fill = "grey75", col = "grey25") +
#   geom_point(size = 2) +
#   coord_quickmap(xlim = limits[[1]], ylim = limits[[2]], expand = FALSE) +
#   # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
#   ggtitle(paste0(domain," domain: ",provider_labels," stations"),
#           subtitle = paste0("Stations with a minimum of ",min_years,
#                             " years in period ",year_start,"-",year_end))
# station_plot
# Analysis of daily rainfall extremes at observational stations

# Read in station data

# Can I read in directly from ruby as a mapped drive?
# Look up which machine we are running on:
info <- Sys.info()
machine <- info[['nodename']]
if (machine == 'ALEMBIC-AS') {
  station_datadir <- paste0('Z:/PhD/data/obs/BoMdata/',datadir,'/')
}
if (machine == 'ruby') {
  station_datadir <- paste0('/datastore/raf018/PhD/data/obs/BoMdata/',datadir,'/')
}
# Testing on the Melbourne Regional Office station:
# station <- vic_bom_daily_hist_stations %>% 
#   filter(Station.Name == "MELBOURNE REGIONAL OFFICE")

# Loop though station numbers?
station_numbers <- unique(stations$Station.Number)
results <- data.frame()




for (n in seq(length(station_numbers))) {
  # for (n in seq(1)) {
  
  station_number <- station_numbers[n]
  station <- stations %>%
    filter(Station.Number == station_number)
  print(station)
  
  # Create file name from station ID, and 
  # read in data for station:
  if (provider == "BoM-daily") {
    station_file <- paste0(station_datadir,'dr_',sprintf("%06d",station$Station.Number),'QC4.txt')
    station_df <- read_csv(station_file)
    station_df <- station_df %>% 
      mutate(Date = ymd(paste(sep = '-', YEAR, MO, DA)),
             Value = PRCP_P, Flag = QCFLAG1) %>% 
      select(Date, YEAR, MO, DA, Value, Flag)
    station_df <- station_df %>% 
      filter(YEAR >= year_start, YEAR <= year_end)
  }
  if (provider == "BoM-continuous") {
    station_file <- paste0(station_datadir,'qcd_',sprintf("%06d",station$Station.Number),'.txt')
    station_df <- read_table(station_file, col_names = FALSE)
    # Convert date column:
    station_df <- station_df %>% mutate(DateTime = ymd_hm(X2), 
                                        Date = date(DateTime),
                                        YEAR = year(DateTime),
                                        MO = month(DateTime),
                                        DA = day(DateTime),
                                        HO = hour(DateTime),
                                        MI = minute(DateTime),
                                        Value = X3,
                                        Flag = X4) %>% 
      select(DateTime, Date, YEAR, MO, DA, HO, MI, Value, Flag) %>% 
      filter(YEAR >= year_start, YEAR <= year_end) %>% 
      group_by(YEAR, MO, DA, Flag) %>% 
      summarise(Value = sum(Value)) %>% 
      mutate(Date = ymd(paste(YEAR, MO, DA, sep = "-"))) %>% 
      mutate(Value = ifelse(Flag != '0000', NA, Value))
      # filter(Flag == '0000')
  }
  if (provider == "Water-Regulations") {
    # Setting station file is more complicated here - mixture of integer and character names
    if (station$Agency == 58) {
      if (stri_length(as.character(station_number)) == 13 & 
          stri_sub(as.character(station_number),-1,-1) == 5) {
      station_file <- paste0(station_datadir,'qcd_',tolower(station$Station.Number),'8.txt')
      } else {
        station_file <- paste0(station_datadir,'qcd_',tolower(station$Station.Number),'.txt')
      }
    } else {
      station_file <- paste0(station_datadir,'qcd_',station$Station.Number,'.txt')
    }
    station_df <- read_table(station_file, col_names = FALSE)
    # station_df <- read_csv(station_file)
    station_df <- station_df %>% mutate(DateTime = ymd_hm(X2), 
                                        Date = date(DateTime),
                                        YEAR = year(DateTime),
                                        MO = month(DateTime),
                                        DA = day(DateTime),
                                        HO = hour(DateTime),
                                        MI = minute(DateTime),
                                        Value = X3,
                                        Flag = X4) %>% 
      select(DateTime, Date, YEAR, MO, DA, HO, MI, Value, Flag) %>% 
      filter(YEAR >= year_start, YEAR <= year_end) %>%
      mutate(Value = ifelse(Flag < 0, NA, Value))
    ###
    ### Do I want to replace flagged values with NA, rather than filtering them out?
    ###
    # station_df <- station_df %>% mutate(Value = ifelse(Flag < 0, NA, Value))

    # station_df22b <- station_df22 %>% 
    #   filter(Flag == '0')
    # # 118080 values
    # station_df22c <- station_df22 %>% 
    #   filter(Flag == '0' | is.na(Flag))
    # # 123840 values
    # station_df22 %>% 
    #   filter(Flag != '0') %>% summarise(n())
    # # 1980000 values!!!
    # 
    # Convert to daily
    station_df <- station_df %>%
      select(Date, YEAR, MO, DA, Value, Flag) %>%
      group_by(YEAR, MO, DA) %>%
      summarise(Value = sum(Value)) %>%
      mutate(Date = ymd(paste(YEAR, MO, DA, sep = "-")))
    
  }
  
  # Add the date in proper format, and filter for our historical period (1986-2005). 
  # Filter our data frame to only contain years we're interested in
  

    # Add datetime column:
  # station_df <- station_df %>% 
  #   mutate(Date = ymd(paste(sep = '-', YEAR, MO, DA))) 
  # station_df %>% ggplot(aes(Date, Value)) +
  #   geom_point(col = "blue", alpha = 0.2) +
  #   geom_smooth(method = "lm", col = "red") +
  #   scale_y_log10() +
  #   ggtitle("Rainfall time series at first station")
  # 
  # station_df %>% ggplot(aes(x=Date, y=Value)) + geom_point(size = 0.2, alpha = 0.4)
  
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
  
  extremes_list <- extremes_analysis(station_df)
  extremes <- extremes_list[[1]]
  gev_params <- extremes_list[[2]]
  
  # Add these results to the results_all data frame
  results <- rbind(results, extremes)
  
}


# outfile <- paste0('rainfall_extremes_',provider,'_',domain,'_',year_start,'-',year_end,'_no-QC.csv')
outfile <- paste0('testing_rainfall_extremes_',provider,'_',domain,'_',year_start,'-',year_end,'_no-QC.csv')
write_csv(results, outfile)






if (plotting == TRUE) {

  # library(ggmap)
  library(maps)
  # library(tmaptools)
  library(mapdata)
  # library(ggmapstyles)
  library(viridis)
  
  # # Testing out some plotting stuff for these results:
  # extreme_results <- read_csv('rainfall_extremes_daily-read_stations_VIC-5_domain_no-QC.csv')
  extreme_results <- results
  # 
  # pp <- extreme_results %>% ggplot(aes(x = Longitude, y = Latitude, col = `ARI-20`)) +
  #   annotation_map(map_data("worldHires"), fill = "grey85", col = "black") +
  #   geom_point(alpha = 0.85, size = 2) +
  #   scale_color_distiller(palette = "YlGnBu") +
  #   coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
  #   # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
  #   ggtitle("VIC-5 domain: ARI-20 of daily read stations",
  #           subtitle = "No filtering of stations")
  # pp
  # 
  # # pal <- wes_palette("Zissou1", 100, type = "continuous")
  # pal <- scale_fill_distiller(palette = "Spectral")
  num_breaks <- c(50, 100, 150, 200, 250, 300, 350)
  log_breaks <- log10(num_breaks)
  p_log <- extreme_results %>% ggplot(aes(x = Longitude, y = Latitude, col = log10(`ARI-20`))) +
    annotation_map(map_data("worldHires"), fill = "grey85", col = "black") +
    geom_point(alpha = 0.95, size = 2) +
    # scale_color_gradientn(colours = pal, breaks = log_breaks, labels = num_breaks,
    #                       limits = log10(c(35, 350))) +
    scale_colour_viridis_c(breaks = log_breaks, labels = num_breaks,
                           limits = log10(c(35, 350))) +
    coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
    # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
    ggtitle("VIC-5 domain: ARI-20 of daily read stations",
            subtitle = "No filtering of stations")
  p_log + labs(col = "mm")
  
  
  p_nolog <- extreme_results %>% ggplot(aes(x = Longitude, y = Latitude, col = `ARI-20`)) +
    annotation_map(map_data("worldHires"), fill = "grey85", col = "black") +
    geom_point(alpha = 0.95, size = 2) +
    # scale_color_gradientn(colours = pal, breaks = log_breaks, labels = num_breaks,
    #                       limits = log10(c(35, 350))) +
    scale_color_gradientn(colours = viridis(7), breaks = num_breaks, labels = num_breaks,
                          limits = c(35, 350)) +
    # scale_colour_viridis_c(breaks = num_breaks, labels = num_breaks,
    #                        limits = c(35, 350)) +
    coord_quickmap(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
    # coord_cartesian(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
    ggtitle("VIC-5 domain: ARI-20 of daily read stations",
            subtitle = "No filtering of stations")
  p_nolog + labs(col = "mm")
  
}
