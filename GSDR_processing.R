# R script to produce plots and analysis for provided inputs of duration
drivers <- FALSE
library(optparse)

option_list <- list(
  # required
  make_option(c("-p", "--period_type"), type = "character", default = "Annual",
              help = "Season aggregation (Annual / Seasonal / Monthly)", metavar = "character"),
  make_option(c("-d", "--duration"), type = "integer", default = 1,
              help = "Time length of precipitation duration in hours (1, 3, 6, 12, 24)", metavar = "character"),
  make_option(c("-r", "--recordlength"), type = "integer", default = NULL,
              help = "Minimum record length", metavar = "character"),
  make_option(c("-f", "--firstyear"), type = "integer", default = 1981,
              help = "First year of analysis window", metavar = "character"),
  make_option(c("-l", "--lastyear"), type = "integer", default = 2015,
              help = "Last year of analysis window", metavar = "character"),
  make_option(c("-s", "--sig_level"), type = "double", default = 0.05,
              help = "Significance level", metavar = "character"),
  make_option(c("-x", "--proportion"), type = "integer", default = NULL,
              help = "Minimum required proportion (%) of years available in analysis window", metavar = "character"),
  make_option(c("-o", "--output"), type = "character", default = "both",
              help = "Option to specify whether to produce figures ('figs'), tables ('tabs'), or 'both' (default) - or climate drivers ('drivers')", metavar = "character")
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$period_type)){
  print_help(opt_parser)
  stop("Input 'period_type' argument must be supplied", call.=FALSE)
}
if (is.null(opt$duration)){
  print_help(opt_parser)
  stop("Source 'duration' argument must be supplied", call.=FALSE)
}
if (is.null(opt$recordlength)){
  # if (is.null(opt$proportion)){
    print_help(opt_parser)
  stop("Record length argument must be supplied", call.=FALSE)
  # stop("Record length or proportion argument must be supplied", call.=FALSE)
  # } else {
  #   length_dir_label <- paste0("min",as.character(opt$proportion),"pc")
  # }
# } else {
#   length_dir_label <- paste0("minlength_",as.character(opt$record_length))
}
if (is.null(opt$firstyear)){
  print_help(opt_parser)
  stop("Input 'firstyear' argument must be supplied", call.=FALSE)
}
if (is.null(opt$lastyear)){
  print_help(opt_parser)
  stop("Input 'lastyear' argument must be supplied", call.=FALSE)
}
if (is.null(opt$sig_level)){
  print_help(opt_parser)
  stop("Input 'sig_level' argument must be supplied", call.=FALSE)
}
if (is.null(opt$proportion)){
  # minimum required proportion not set
  proportion <- "0"
  paste("No minimum requirement set for proportion of period with complete data.")
} else {
  proportion <- as.character(opt$proportion)
  paste0("Minimum proportion of years required in analysis window: ",proportion,"%")
}
if (is.null(opt$output)){
  plotting <- TRUE
  tables <- TRUE
  choropleths <- FALSE
  drivers <- FALSE
} else if (opt$output == "both") {
  plotting <- TRUE
  tables <- TRUE
  choropleths <- FALSE
  drivers <- FALSE
} else if (opt$output == "figs") {
  plotting <- TRUE
  tables <- FALSE
  choropleths <- FALSE
  drivers <- FALSE
} else if (opt$output == "tabs") {
  plotting <- FALSE
  tables <- TRUE
  choropleths <- FALSE
  drivers <- FALSE
} else if (opt$output == "chor") {
  plotting <- TRUE
  tables <- FALSE
  choropleths <- TRUE
  drivers <- FALSE
} else if (opt$output == "drivers") {
  plotting <- FALSE
  tables <- FALSE
  choropleths <- FALSE
  drivers <- TRUE
}


period_type <- opt$period_type
dur <- as.character(opt$duration)
record_length <- as.character(opt$recordlength)
first_year <- as.character(opt$firstyear)
last_year <- as.character(opt$lastyear)
sig_level <- as.character(opt$sig_level)

if (drivers == TRUE) {
  basepath <- "GSDR_analysis/NINO34/"
} else {
  basepath <- paste0("GSDR_analysis/station_trends/minlength_",record_length,
                    "yr/",first_year,"-",last_year,"/",dur,"hr/")
  # basepath <- paste0("GSDR_analysis/station_trends/",length_dir_label,
  #                    "/",first_year,"-",last_year,"/",dur,"hr/")
}
dir.create(paste0(basepath,"tables"), recursive = TRUE)
dir.create(paste0(basepath,"figures"), recursive = TRUE)

#for testing:
#period_type defined later
# dur <- "1"
# record_length <- 20
# record_length <- 0
# proportion <- 67
# proportion <- 0
# sig_level <- 0.05
# first_year <- 1981
# last_year <- 2015

# load packages
library(tidyverse)
library(maps)
library(mapdata)
library(patchwork)
library(timeplyr)
library(ggrepel)
library(broom)
library(gt)
library(knitr)
library(scales)
library(paletteer)
library(zoo)
library(sf)
sf_use_s2(FALSE)
library(slider)
library(ggrepel)

# setting up some domains:
domain_list <- c("aus", "vic", "syd", "seq")

# domain bounds for all Australia:
aus_lon_limits <- c(112, 156)
aus_lat_limits <- c(-45, -10)

# VIC domain bounds:
vic_lon_limits <- c(140,151.25)
vic_lat_limits <- c(-39.5,-32.8)

# SYD domain bounds:
syd_lon_limits <- c(148,153)
syd_lat_limits <- c(-36,-31.8)

# seq domain bounds:
seq_lon_limits <- c(150,154)
seq_lat_limits <- c(-29,-25)

shapefile_list <- list(
  ncra = "shapefiles/NCRA_regions/NCRA_regions.shp",
  nrm_clust = "shapefiles/NRM_clusters/NRM_clusters.shp",
  nrm_clust_sub = "shapefiles/NRM_sub_clusters/NRM_sub_clusters.shp",
  nrm_clust_super = "shapefiles/NRM_super_clusters/NRM_super_clusters.shp",
  state = "shapefiles/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp"
)

# data processing flow chart:
# - read in time series -> e.g. TimeSeries_Annual_Rx1hr_1min
# - find number of annual max by station -> ann_max_count
# - find stations meeting min length criteria -> annmax_1hr_gt_20
# - time series stations with minimum years -> ts_gt20
# - plot completeness histogram
# - filter out years with low completeness -> ts_20y_80pc
# - eliminate individual years that have poor completeness -> annmax_count_gt20y_80pc
# - get time series for stations meeting minimum years and completeness thresholds -> e.g. ts_gt20y_80pc
# - filter time series for set epoch -> e.g. ts_gt20y_80pc_1981_2015
# - summary stats per station -> e.g. rx1hr_summary_gt20y_1981_2015
# - plot time series
# - test estimates (linear trend, mann-kendall significance) -> e.g.
# - tables of significance



#####################################################
###   FUNCTIONS
#####################################################

# Create plotting function for station locations:
plot_stations <- function(df, nmins = NULL,
                          lon_limits, lat_limits,
                          region = "Australia",
                          period_type = c("Annual", "Seasonal", "Monthly"),
                          filtering = NULL,
                          facet = TRUE,
                          title = TRUE) {
  if (is.null(filtering)) {
    filter_label <- "No filtering of stations"
  } else {
    if (filtering == "failed") {
      filter_label <- "Stations that failed indices creation"
    }
    if (filtering == "succeeded") {
      filter_label <- "Stations that successfully created indices"
    }
  }
  if (! is.null(nmins)) {
    # if (nmins == 1 | nmins == 5) {
    df <- df %>% filter(DataSource == paste0(nmins,"min"))
    # highlight <- "PercentMissing"
    title_label <- paste0(nmins,"-minute observation stations from GSDR over ",region)
    subtitle_label <- paste0(filter_label,", coloured by percent of missing data")
    p <- df %>% ggplot(aes(x = Longitude, y = Latitude, colour = DataSource))
  } else {
    # highlight <- "DataSource"
    title_label <- paste0("All observation stations from GSDR over ",region)
    if (is.null(filtering)) {
      subtitle_label <- NULL
    } else {
      subtitle_label <- filter_label
    }
    p <- df %>% ggplot(aes(x = Longitude, y = Latitude, colour = DataSource))
  }

  p <- p +
    # "fill" is land colour; "col" is border colour.
    # Ocean colour needs to be set as background of panel below.
    annotation_map(map_data("worldHires"), fill = "antiquewhite", col = "grey25") +
    geom_point(alpha = 0.15) +
    # scale_color_gradient(low = "red", high = "blue") +
    coord_sf(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
    # ggtitle(title_label,
    #         subtitle = subtitle_label) +
    theme(panel.background = element_rect(fill = "lightsteelblue1"))

  if (title) {
    p <- p + ggtitle(title_label, subtitle = subtitle_label)
  }
  if (! is.null(nmins)) {
  # if (nmins == 1 | nmins == 5) {
    p + labs(colour = "Station\ntype")
  } else {
    if (period_type == "Annual") {p <- p + facet_grid(~ DataSource) + labs(colour = "Station\ntype")}
    if (period_type == "Seasonal") {p <- p + facet_wrap(~ Season)}
    if (period_type == "Monthly") {p <- p + facet_wrap(~ Month)}
    # if (facet) {p <- p + facet_grid(~ DataSource)}
    # p + labs(colour = "Station\ntype")
    p
  }
}


plot_station_stats <- function(df, stat, dur,
                               lon_limits, lat_limits,
                               region = "Australia",
                               facet = FALSE) {
  title_label <- paste0(str_to_sentence(stat), " of Rx",dur,"hr at observation stations from GSDR over ",region)
  subtitle_label <- NULL

  p <- ungroup(df) %>%
    ggplot(aes_string(x = "Longitude", y = "Latitude", colour = stat)) +
    # "fill" is land colour; "col" is border colour.
    # Ocean colour needs to be set as background of panel below.
    annotation_map(map_data("worldHires"), fill = "antiquewhite", col = "grey25") +
    geom_point(alpha = 0.7, size = 2.5) +
    scale_colour_viridis_c(option = "H") +
    coord_sf(xlim = lon_limits, ylim = lat_limits, expand = FALSE) +
    ggtitle(title_label,
            subtitle = subtitle_label) +
    theme(panel.background = element_rect(fill = "lightsteelblue1"))

  p + labs(colour = "mm/hour")

}


filter_stations <- function(df = ts_gtN_80pc, recordlength = record_length,
                            period_type = c("Annual", "Seasonal", "Monthly")) {

  period_type <- match.arg(period_type)
  recordlength <- as.numeric(recordlength)

  df %>% ungroup() %>%
    {if (period_type == "Annual") group_by(., Station) else . } %>%
    {if (period_type == "Seasonal") group_by(., Season, Station) else . } %>%
    {if (period_type == "Monthly") group_by(., Month, Station) else . } %>%
    arrange(Station, Year) %>%
    unique() %>%
    summarise(n_years = n()) %>%
    # arrange(Station, Season) %>%
    arrange(Station) %>%
    relocate(Station) %>%
    filter(n_years >= recordlength)
}


read_ts <- function(period_type = c("Annual", "Seasonal", "Monthly"),
                              duration = c("1", "3", "6", "12", "24")) {
  require(forcats)
  period_type <- match.arg(period_type)
  duration <- match.arg(duration)
  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  # read in 1min data
  infile_1min <- paste0("GSDR/Indices_1min-postQC/TimeSeries/",period_type,
                        "/TimeSeries_",period_type,"_Rx",duration,"hr.csv")
  ts_1min <- read_csv(infile_1min, col_types = cols(Folder = col_skip()))

  # read in 5min data
  infile_5min <- paste0("GSDR/Indices_5min-postQC/TimeSeries/",period_type,
                        "/TimeSeries_",period_type,"_Rx",duration,"hr.csv")
  ts_5min <- read_csv(infile_5min, col_types = cols(Folder = col_skip()))

  # combine into one ts
  ts <- bind_rows(ts_1min, ts_5min, .id = "DataSource") %>%
    mutate(DataSource = as.factor(ifelse(DataSource == 2,"5min","1min")))

  ts
  # %>%
  #   {if (period_type == "Seasonal")
  #     mutate(., Season = lvls_revalue(as.factor(Season), season_list)) else . } %>%
  #   {if (period_type == "Monthly")
  #     mutate(., Month = lvls_revalue(as.factor(Month), month_list)) else . }

}

# ts2 <- read_ts(period_type, dur)

relabel_aggregations <- function(df, period_type) {
  # Changes integer representations of seasons and months to label form
  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  df %>% ungroup() %>%
    {if (period_type == "Annual")
      mutate(., Annual = "Annual") else . } %>%
    {if (period_type == "Seasonal")
      mutate(., Season = lvls_revalue(as.factor(Season), season_list))
      # %>% select(., -Season)
      else . } %>%
    {if (period_type == "Monthly")
      mutate(., Month = lvls_revalue(as.factor(Month), month_list))
      # %>% select(., -Month)
      else . }
  # %>%
  #   relocate(time_block)

}

# TODO: fix order for seasonal / monthly
valid_years_in_period <- function(df = ts_gtN_80pc,
                                  Nyears = record_length,
                                  valid_prop = proportion,
                                  periodtype = period_type,
                                  oldest = first_year,
                                  newest = last_year) {
  df <- df %>%
    # filter out records before $oldest and after $newest
    filter(Year >= oldest, Year <= newest)

  # check that we have an adequate number of records (Nyears+):
  n_per_station <- df %>%
    ungroup() %>%
    {if (periodtype == "Annual") group_by(., Station) else . } %>%
    {if (periodtype == "Seasonal") group_by(., Station, Season) else . } %>%
    {if (periodtype == "Monthly") group_by(., Station, Month) else . } %>%
    summarise(n_years = n()) %>%
    filter(n_years >= Nyears)

  # how many year observations in the period?
  # newest
  # oldest
  period_length <- (as.integer(newest) - as.integer(oldest)) + 1
  # what is the minimum years to meet proportion target?
  if (! is.null(valid_prop)) {
    prop_years <- ceiling((as.numeric(valid_prop) / 100) * period_length)
    n_per_station <- n_per_station %>%
      filter(n_years >= prop_years)
  }

  if (nrow(n_per_station) > 0) {
    # extract stations with adequate records:
    test_df <- n_per_station %>%

      {if (periodtype == "Annual")
        # keep the stations with adequate records
        select(., Station) %>%
          # select these stations from time series
          left_join(df, by = "Station") %>%
          # groupings by station to allow group labels
          group_by(Station) %>%
          # sort by station number
          arrange(Station) %>%
          # add label for maximum value within each group (Station)
          mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
                 # convert Year to a date
                 Year = as_date(paste0(Year,"-01-01"))) %>%
          # add index to each group
          mutate(group_id = cur_group_id(),
                 station_index = cur_group_id()) %>%
          ungroup() %>%
          # fill in any missing years
          time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
          group_by(Station)

        else . } %>%
      # {if (periodtype == "Seasonal")
      #   select(., Station, Season) %>%
      #     left_join(df, by = c("Station", "Season")) %>%
      #     group_by(Station) %>%
      #     mutate(station_index = cur_group_id()) %>%
      #     group_by(Station, Season) %>%
      #     arrange(Station, Season, Year) %>%
      #     mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
      #            Year = as_date(paste(Year,"01","01",sep="-"))) %>%
      #     time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
      #     # group_by(., Station, Month) %>%
      #     mutate(group_id = cur_group_id()) %>%
      #     # arrange(Station, Year, Season) %>%
      #     relocate(Year, .after = Station)
      {if (periodtype == "Seasonal")
        select(., Station, Season) %>%
          left_join(df, by = c("Station", "Season")) %>%
          group_by(Station) %>%
          mutate(station_index = cur_group_id()) %>%
          group_by(Station, Season) %>%
          arrange(Station, Season, Year) %>%
          mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
                 Year = as_date(paste(Year,"01","01",sep="-"))) %>%
          mutate(group_id = cur_group_id()) %>%
          ungroup() %>%
          time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
          group_by(., Station, Season) %>%
          arrange(Station, Season, Year) %>%
          relocate(Year, .after = Station)
        else . } %>%
      {if (periodtype == "Monthly")
        select(., Station, Month) %>%
          left_join(df, by = c("Station", "Month")) %>%
          group_by(Station) %>%
          mutate(station_index = cur_group_id()) %>%
          group_by(Station, Month) %>%
          arrange(Station, Month, Year) %>%
          mutate(ts_max_index = if_else(Value == max(Value, na.rm = T), "max", NA_character_),
                 Year = as_date(paste(Year,Month,"01",sep="-"))) %>%
          mutate(group_id = cur_group_id()) %>%
          ungroup() %>%
          time_complete(Year, time_by = "year", fill = list(Value = NA)) %>%
          group_by(., Station, Month) %>%
          arrange(Station, Month, Year) %>%
          relocate(Year, .after = Station)
        else . }

    # return modified df
    # test_df %>%
    #   group_by(Station) %>%
    #   mutate(station_index = cur_group_id()) %>%
    #   ungroup()
    test_df

  } else {
    stop("No stations meet the criteria")
  }

}


# function to create summary stats table of time series
summarise_filtered_ts <- function(df = ts_gtN_80pc_period,
                                  oldest = first_year,
                                  newest = last_year,
                                  periodtype = period_type) {

  if (periodtype == "Annual") {groupings <- c("Station")}
  if (periodtype == "Seasonal") {groupings <- c("Station", "Season")}
  if (periodtype == "Monthly") {groupings <- c("Station", "Month")}

  summary_table <- df %>%
    group_by(across(groupings)) %>%
    summarise(minimum = round(min(Value, na.rm = T), digits = 1),
              median = round(median(Value, na.rm = T), digits = 1),
              mean = round(mean(Value, na.rm = T), digits = 1),
              maximum = round(max(Value, na.rm = T), digits = 1)) %>%
    mutate(year_start = oldest, year_end = newest, .after = Station) %>%
    # add the lats/lons back to each station
    left_join(df %>% summarise(Latitude = max(Latitude, na.rm = TRUE),
                               Longitude = max(Longitude, na.rm = TRUE)),
              by = groupings) %>%
    relocate(c(Latitude, Longitude), .after = Station)

}

# plot time series by station
# if seasonal facet each row into 4 seasons;
# if monthly, facet all 12 months for that station
plot_ts_station <- function(df = ts_gtN_80pc_period,
                            periodtype = period_type,
                            station1 = 1, stationN = 12,
                            station_num = NULL,
                            index_num = NULL,
                            duration = dur,
                            valid_prop = proportion,
                            oldest = first_year, newest = last_year) {

  label_suffix <- ""
  season_list <- c("DJF", "MAM", "JJA", "SON")
  month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df <- df %>% ungroup() %>%
    {if (period_type == "Seasonal")
      mutate(., Season = lvls_revalue(as.factor(Season), season_list)) else . } %>%
    {if (period_type == "Monthly")
      mutate(., Month = lvls_revalue(as.factor(Month), month_list)) else . }

  if (!is.null(station_num)) {
    # plot up one station
    df <- df %>% filter(Station == station_num)
    label_suffix <- paste0(" ",station_num)
  } else {
    if (!is.null(index_num)) {
      # plot up one series index
      # df <- df %>% filter(group_id == index_num) # this removes NA values!
      station_num <- df %>% ungroup() %>%
        # filter(group_id == index_num) %>%
        filter(station_index == index_num) %>%
        select(Station) %>% head(1)[[1]]
      df <- df %>% filter(Station == station_num)
      label_suffix <- paste0(" ",station_num)
    } else {
      # filter these stations
      df <- df %>% filter(station_index >= station1,
                          station_index <= stationN)
      label_suffix <- paste0("s ",station1,"-",stationN)
    }
  }

  if (periodtype == "Annual") {
    # groupings <- c("Station")
    period_type_plotting <- sym("Year")
  }
  if (periodtype == "Seasonal") {
    # groupings <- c("Station", "Season")
    # facetvar <- "Season"
    period_type_plotting <- sym("Season")
  }
  if (periodtype == "Monthly") {
    # groupings <- c("Station", "Month")
    # facetvar <- "Month"
    period_type_plotting <- sym("Month")
  }
  # facetvar <- enquo(facetvar)

  p3 <- df %>%
    # {if (periodtype == "Seasonal")
    #   mutate(month_or_season = month(!!period_type_plotting, label = TRUE))
    #   else
    #   mutate(month_or_season = month(!!period_type_plotting, label = TRUE))} %>%
    ggplot(aes(x = Year, y = Value)) +
    geom_point(aes(colour = ts_max_index), alpha = 0.7, show.legend = FALSE) +
    geom_path(colour = "darkgrey") +
    geom_smooth(method = "lm", se = T) +
    theme_bw() +
    labs(x = NULL, y = "mm/hour") +
    ggtitle(paste0("Time series (",oldest,"-",newest,") of ",
                   periodtype," maximum Rx",duration,"hr for Station",
                   label_suffix))
  if (periodtype == "Annual") {
    p3 <- p3 +
      facet_wrap(~ Station, scales = "free_y", nrow = 4)
  } else {
    p3 <- p3 +
      # facet_wrap(~ month_or_season, scales = "free_y", nrow = 4)
      facet_wrap(period_type_plotting, scales = "free_y", nrow = 4)
  }

  if (is.null(valid_prop)) {
    p3
  } else {
    p3 +
      labs(caption = paste0("Minimum proportion of years with valid data: ",
                            valid_prop,"%"))
  }

  # p3
}

add_temp_covariate <- function(df,
                               covariate = "global",
                               smoothing_period = 11) {
  require(zoo)
  # function to read in temperature data to use instead of year for rainfall covariate
  # and add it to the input dataframe
  if (covariate == "global") {
    annual_temp <- readr::read_delim(
      file = "./HadCRUT/HadCRUT5_1850-2022.global_t.global.0112.19811.raw.txt",
      col_names = c("daterange", "temperature"))
  }
  if (covariate == "SH") {
    annual_temp <- readr::read_delim(
      file = "./HadCRUT/HadCRUT5_1850-2022.global_t.sh.0112.35323.raw.txt",
      col_names = c("daterange", "temperature"))
  }

  annual_temp_mod <- annual_temp %>%
    mutate(year_only = as.numeric(substr(x = daterange, start = 1, stop = 4)),
           # Year = ymd(paste0(year_only,"-01-01")),
           .before = daterange,
           temperature = round(
             zoo::rollmean(x = temperature, k = smoothing_period, fill = NA),
             digits = 4)) %>%
    select(-daterange)

  # add the annual temperature values to the DF
  df_temp <- df %>%
    mutate(year_only = year(Year)) %>%
    left_join(annual_temp_mod) %>%
    select(-year_only)

  df_temp
}

add_domain_ownership <- function(df, shapefile_list) {
  # function to add information about which domains each location belongs to,
  # e.g. NCRA region, NRM clusters/sub-/super-clusters, state/territory
  require(sf)
  sf_use_s2(FALSE)

  # shapefile_list <- list(
  #   ncra = "shapefiles/NCRA_regions/NCRA_regions.shp",
  #   nrm_clust = "shapefiles/NRM_clusters/NRM_clusters.shp",
  #   nrm_clust_sub = "shapefiles/NRM_sub_clusters/NRM_sub_clusters.shp",
  #   nrm_clust_super = "shapefiles/NRM_super_clusters/NRM_super_clusters.shp",
  #   state = "shapefiles/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp"
  # )
  state_list <- list(
    "Australian Capital Territory" = "ACT",
    "New South Wales" = "NSW",
    "Northern Territory" = "NT",
    "Queensland" = "QLD",
    "South Australia" = "SA",
    "Tasmania" = "TAS",
    "Victoria" = "VIC",
    "Western Australia" = "WA",
    "unknown" = "other",
    "Other Territories" = "other"
  )

  # df <- test_summaries_temperature
  # add information from each shapefile in list to the supplied df (suggest summary tables)
  for (shape in names(shapefile_list)) {
    print(shape)
    s <- read_sf(shapefile_list[shape])
    # select lats and lons from df
    points_df <- tibble(id = df$Station, Latitude = df$Latitude, Longitude = df$Longitude)
    # convert to sf geometry
    points_sf <- st_as_sf(points_df, coords = c("Longitude", "Latitude"), crs = 4326)
    # ensure they're on the same projection:
    points_sf <- st_transform(points_sf, crs = st_crs(s))
    points_with_domains <- st_join(points_sf, s) %>%
      {if (shape == "ncra")
        transmute(., Station = id, NCRA_name = regionname, NCRA_label = label) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "nrm_clust")
        transmute(., Station = id, NRM_cluster_name = label, NRM_cluster_label = code) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "nrm_clust_sub")
        transmute(., Station = id, NRM_subcluster_name = label, NRM_subcluster_label = code) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "nrm_clust_super")
        transmute(., Station = id, NRM_supercluster_name = label, NRM_supercluster_label = code) %>%
          st_drop_geometry() else . } %>%
      {if (shape == "state")
        transmute(., Station = id, State_name = STE_NAME21) %>%
          mutate(State_name = replace_na(State_name, "unknown")) %>%
          mutate(State_label = map_chr(State_name, ~ state_list[[.x]])) %>%
          st_drop_geometry() else . }

    # join with df:
    df <- df %>% left_join(points_with_domains, by = "Station", multiple = "first")

  }

  # return the df with added location columns for each station
  df

}

test_estimates_linear <- function(df,
                                  periodtype = period_type,
                                  oldest = first_year,
                                  newest = last_year,
                                  trend_type = "year") {

  if (trend_type == "year") {
    trend_var <- sym("Year")
  }
  if (trend_type == "temperature") {
    trend_var <- sym("temperature")
  }
  require(broom)
  df %>%
    {if(trend_type == "year") mutate(., Year = year(Year)) else .} %>%
    # mutate(Year = year(Year)) %>%
    group_by(Station) %>%
    {if(periodtype == "Seasonal") group_by(., Season, .add = TRUE) else .} %>%
    {if(periodtype == "Monthly") group_by(., Month, .add = TRUE) else .} %>%
    do(tidy(lm(Value ~ !!trend_var, .))) %>%
    filter(term == trend_var) %>%
    select(-term) %>%
    mutate(p.value = round(p.value, digits = 5)) %>%
    mutate(year_start = oldest, year_end = newest, .after = Station) %>%
    arrange(Station)

}

# test_temps <- test_estimates_linear(ts_gtN_80pc_period_temperature, trend_type = "temperature")
# test_estimates_linear(ts_gtN_80pc_period, trend_type = "year") %>%
#   ggplot(aes(x = p.value, y = estimate)) +
#   geom_point() +
#   geom_vline(xintercept = 0.05) +
#   geom_point(data = linear_tests_temperature,
#              aes(x = p.value, y = estimate),
#              colour = "red")


# modified_mk_test_temp <- function(x, trend_type = "year", ...) {
#   require(zoo)
#   if (trend_type == "year") {
#     trend_var <- sym("Year")
#   }
#   if (trend_type == "temperature") {
#     trend_var <- sym("temperature")
#   }
#   ts <- read.zoo(mutate(x, Year = year(Year)) %>% ungroup() %>% select(Year, Value))
#   result <- MannKendall(ts, ...)
#
#   tibble(
#     p.value_mk = round(result$sl[1], digits = 5)
#   )
#
# }

modified_mk_test <- function(x, ...) {
  require(zoo)
  ts <- read.zoo(mutate(x, Year = year(Year)) %>% ungroup() %>% select(Year, Value))
  result <- MannKendall(ts, ...)

  tibble(
    p.value_mk = round(result$sl[1], digits = 5)
  )

}


test_estimates_mk <- function(df,
                              periodtype = period_type,
                              oldest = first_year, newest = last_year) {

  require(zoo)
  require(Kendall)

  # mkt_result <- df %>%
  df %>%
    group_by(Station) %>% # might have to change this part
    {if(periodtype == "Seasonal") group_by(., Season, .add = TRUE) else .} %>%
    {if(periodtype == "Monthly") group_by(., Month, .add = TRUE) else .} %>%
    group_modify(~ modified_mk_test(.x)) %>%
    mutate(year_start = oldest, year_end = newest, .after = Station) %>%
    arrange(Station)

}

# nrm_table <- test_summaries_temperature %>%
#   filter(NRM_subcluster_name == NRM_subcluster_name) %>% # removes NA values for subcluster
#   group_by(NRM_subcluster_name, NRM_subcluster_label) %>%
#   mutate(time_period = paste(first_year,last_year,sep="-"),
#          aggregation = period_type) %>%
#   {if (period_type == "Seasonal")
#     group_by(., Season) else . } %>%
#   {if (period_type == "Monthly")
#     group_by(., Month) else . } %>%
#   {if (covariate == "year") mutate(., trend = trend * 10,
#                                    trend_pc_mean = trend_pc_mean * 10,
#                                    trend_pc_median = trend_pc_median * 10)
#     else . } %>%
#   summarise(n_total = n(),
#             n_pos = sum(trend > 0),
#             n_pos_sig = sum(trend > 0 & p.value_mk <= siglevel),
#             n_pos_sig_pc = round(100 * n_pos_sig / n_total, digits = 1),
#             n_neg = sum(trend < 0),
#             n_neg_sig = sum(trend < 0 & p.value_mk <= siglevel),
#             n_neg_sig_pc = round(100 * n_neg_sig / n_total, digits = 1),
#             ratio_pos_neg_sig = round(n_pos_sig / n_neg_sig, digits = 2),
#             # some percentage values are +/- infinity due to div-by-0
#             mean_pc_mean = round(mean(
#               if_else(trend_pc_mean == Inf, NA,
#                       if_else(trend_pc_mean == -Inf, NA, trend_pc_mean)),
#               na.rm=TRUE), digits = 2),
#             mean_pc_median = round(mean(
#               if_else(trend_pc_median == Inf, NA,
#                       if_else(trend_pc_median == -Inf, NA, trend_pc_median)),
#               na.rm=TRUE), digits = 2),
#             median_pc_mean = round(median(trend_pc_mean, na.rm=TRUE), digits = 2),
#             median_pc_median = round(median(trend_pc_median, na.rm=TRUE), digits = 2)) %>%
#   relabel_aggregations(periodtype)



make_test_table <- function(df, covariate = c("year", "temperature"),
                            ys = first_year, ye = last_year,
                            periodtype = period_type, siglevel = sig_level,
                            domain = NULL, #c("aus", "vic", "syd", "seq"),
                            lon_lims = NULL, lat_lims = NULL,
                            cluster_type = NULL) {

  # takes test_summaries (or test_summaries_temperature) df and
  # processes into statistics of all stations within specified region,
  # e.g. test_table for annual AUS:
  # Annual,n_total,n_pos,n_pos_sig,n_pos_sig_pc,n_neg,n_neg_sig,n_neg_sig_pc,
  # ratio_pos_neg_sig,mean_pc_mean,mean_pc_median,median_pc_mean,median_pc_median
  # Annual,251,144,15,6,107,6,2.4,2.5,8.64,9.49,6.72,6.98


  if (! is.null(domain)) {
    domains <- c("aus", "vic", "syd", "seq")
    domain <- toupper(match.arg(domain, domains))
    if (is.null(lon_lims) | is.null(lat_lims)) {
      stop("Need to specify lon_lims and lat_lims for this domain.")
    }
    print(paste("Domain: ",domain))
  } else {
    if (! is.null(cluster_type)) {
      cluster_list <- list("nrm_clust" = "NRM_cluster",
                           "nrm_clust_sub" = "NRM_subcluster",
                           "nrm_clust_super" = "NRM_supercluster",
                           "state" = "State",
                           "ncra" = "NCRA")
      cluster_types <- names(cluster_list)
      cluster_type <- match.arg(cluster_type, cluster_types)
      print(paste("Cluster type: ",cluster_type))
      cluster_label <- paste0(cluster_list[[cluster_type]],"_label")
      # cluster_label <- quo(cluster_label)
      cluster_name <- paste0(cluster_list[[cluster_type]],"_name")
      # cluster_name <- quo(cluster_name)
    } else {
      stop("Need to specify either 'domain' or 'cluster_type'.")
    }
  }
  covariate <- match.arg(covariate)

  df %>%
    {if (! is.null(domain))
      filter(., between(Longitude, lon_lims[1], lon_lims[2]),
             between(Latitude, lat_lims[1], lat_lims[2])) %>%
        mutate(domain = domain) %>%
        group_by(domain) else . } %>%
    {if (! is.null(cluster_type))
      group_by(., !! sym(cluster_label), !! sym(cluster_name))
      else .} %>%
    mutate(time_period = paste(ys,ye,sep="-"),
           aggregation = periodtype) %>%
    group_by(time_period, .add = TRUE) %>%
    {if (period_type == "Seasonal")
      group_by(., Season, .add = TRUE) else . } %>%
    {if (period_type == "Monthly")
      group_by(., Month, .add = TRUE) else . } %>%
    {if (covariate == "year")
      mutate(., trend = trend * 10,
             trend_pc_mean = trend_pc_mean * 10,
             trend_pc_median = trend_pc_median * 10)
      else . } %>%
    summarise(n_total = n(),
              n_pos = sum(trend > 0),
              n_pos_sig = sum(trend > 0 & p.value_mk <= siglevel),
              n_pos_sig_pc = round(100 * n_pos_sig / n_total, digits = 1),
              n_neg = sum(trend < 0),
              n_neg_sig = sum(trend < 0 & p.value_mk <= siglevel),
              n_neg_sig_pc = round(100 * n_neg_sig / n_total, digits = 1),
              ratio_pos_neg_sig = round(n_pos_sig / n_neg_sig, digits = 2),
              # some percentage values are +/- infinity due to div-by-0
              mean_pc_mean = round(mean(
                if_else(trend_pc_mean == Inf, NA,
                        if_else(trend_pc_mean == -Inf, NA, trend_pc_mean)),
                na.rm=TRUE), digits = 2),
              mean_pc_median = round(mean(
                if_else(trend_pc_median == Inf, NA,
                        if_else(trend_pc_median == -Inf, NA, trend_pc_median)),
                na.rm=TRUE), digits = 2),
              median_pc_mean = round(median(trend_pc_mean, na.rm=TRUE), digits = 2),
              median_pc_median = round(median(trend_pc_median, na.rm=TRUE), digits = 2)) %>%
    relabel_aggregations(periodtype) %>%
    {if (! is.null(cluster_type)) filter(., ! is.na(!! sym(cluster_label))) else . }

}

# test_table_nrmclust <- make_test_table(df_test, covariate = cov, cluster_type = "nrm_clust")
# test_table_nrmclust2 <- make_test_table(df_test, covariate = cov, cluster_type = "nrm_clust") %>% filter(!is.na(NRM_cluster_label))
# test_table_aus <- make_test_table(df_test, covariate = cov, lon_lims = aus_lon_limits, lat_lims = aus_lat_limits, domain = "aus")

source('./plot_rxNday_trends.R')



#####################################################


# period_type <- "Annual"
# period_type <- "Seasonal"
# period_type <- "Monthly"


if (drivers == FALSE) {


  ts <- read_ts(period_type, dur)

  max_count <- ts %>%
    {if (period_type == "Annual") group_by(., Station, DataSource) else . } %>%
    {if (period_type == "Seasonal") group_by(., Station, DataSource, Season) else . } %>%
    {if (period_type == "Monthly") group_by(., Station, DataSource, Month) else . } %>%
    summarise(n_maxima = n())

  ts_gtN <- max_count %>%
    ungroup() %>%
    filter(n_maxima >= as.numeric(record_length)) %>%
    select(-DataSource,-n_maxima) %>%
    {if (period_type == "Annual") left_join(., ts, join_by(Station)) else . } %>%
    {if (period_type == "Seasonal") left_join(., ts, join_by(Station, Season)) %>%
        arrange(Station, Year, Season) %>%
        relocate(Year, .after = Station) else . } %>%
    {if (period_type == "Monthly") left_join(., ts, join_by(Station, Month)) %>%
        arrange(Station, Year, Month) %>%
        relocate(Year, .after = Station) else . }

  # - filter out years with low completeness -> ts_20y_80pc
  ts_gtN_80pc <- ts_gtN %>% filter(Completeness >= 80)

  # p1 <- plot_stations(ts_gtN_80pc, period_type = period_type,
  #                     lon_limits = aus_lon_limits,
  #                     lat_limits = aus_lat_limits, title = FALSE)
  # p1
  # ggsave(paste0("station_plot_",dur,"hr_",period_type,"_",first_year,"-",last_year,".png"), p1)


  # - eliminate individual years that have poor completeness -> annmax_count_gt20y_80pc
  annmax_count_gtN_80pc <- filter_stations(period_type = period_type)

  # how many stations do we have for each block (year/season/month)?
  n_annmax_count_gtN_80pc <- annmax_count_gtN_80pc %>%
    {if (period_type == "Seasonal") group_by(., Season) else . } %>%
    {if (period_type == "Monthly") group_by(., Month) else . } %>%
    summarise(n_stations = n())


  # - get time series for stations meeting minimum years and completeness thresholds -> e.g. ts_gt20y_80pc
  # annmax_count_gtN_80pc has stations (and month/season) with enough data
  # Join with time series from ts_gtN_80pc
  ts_gtN_80pc_stations <- annmax_count_gtN_80pc %>%
    {if (period_type == "Annual") select(., Station) %>%
        left_join(ts_gtN_80pc, by = "Station") else . } %>%
    {if (period_type == "Seasonal") select(., Station, Season) %>%
        left_join(ts_gtN_80pc, by = c("Station", "Season")) %>%
        arrange(Station, Year, Season) %>%
        relocate(Year, .after = Station) else . } %>%
    {if (period_type == "Monthly") select(., Station, Month) %>%
        left_join(ts_gtN_80pc, by = c("Station", "Month")) %>%
        arrange(Station, Year, Month) %>%
        relocate(Year, .after = Station) else . }


  # code to plot the number of stations per season/month
  if (period_type == "Annual") {
    # do nothing
    period_type_plotting <- NULL
  } else {
    if (period_type == "Seasonal") {
      period_type_plotting <- sym("Season")
    } else {
      if (period_type == "Monthly") {
        period_type_plotting <- sym("Month")
      }
    }

    p2 <-
      n_annmax_count_gtN_80pc %>%
      # convert months to character labels
      mutate(month_or_season = month(!!period_type_plotting, label = TRUE)) %>%
      ggplot(aes(x = month_or_season, y = n_stations, fill = factor(month_or_season))) +
      geom_col(show.legend = FALSE) +
      scale_fill_viridis_d() +
      scale_y_continuous(expand = c(0.01,0)) +
      labs(x = period_type_plotting, y = "Number of Stations",
          title = paste0(period_type," number of valid stations"))
    # p2
    # ggsave(paste0("station_histogram_",dur,"hr_",period_type,"_",first_year,"-",last_year,".png"), p2)
  }

  # histogram of number of years available
  # ts_gtN_80pc %>% ggplot(aes(x = Year)) +
  #   geom_histogram(binwidth = 1, fill = "darkgreen", alpha = 0.6, colour = "darkgrey") +
  #   scale_x_continuous(breaks = seq(1900,2020,10)) +
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  # ts_gtN_80pc_stations %>% ggplot(aes(x = Year)) +
  #   geom_histogram(binwidth = 1, fill = "darkorange", alpha = 0.6, colour = "darkgrey") +
  #   scale_x_continuous(breaks = seq(1900,2020,10)) +
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

  p_hist <-
    ts_gtN_80pc %>% relabel_aggregations(period_type) %>%
    ggplot(aes(x = Year)) +
    geom_histogram(binwidth = 1, fill = "darkgreen", alpha = 0.6) +
    # ggplot(ts_gtN_80pc_stations, aes(x = Year)) +
    geom_histogram(data = relabel_aggregations(ts_gtN_80pc_stations,period_type),
                  binwidth = 1,fill = "darkorange", alpha = 0.75) +
    scale_x_continuous(breaks = seq(1900,2020,20)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(period_type_plotting)
  # p_hist

  # ggsave(paste0("station-years_histogram_1900-2020_",
  #               period_type,".png"),
  #        p_hist)

  # - filter time series for set epoch -> e.g. ts_gt20y_80pc_1981_2015
  ts_gtN_80pc_period <- valid_years_in_period()
  ts_gtN_80pc_period_temperature <- ts_gtN_80pc_period %>% add_temp_covariate()
    # valid_years_in_period(df = ts_gtN_80pc)

  # - summary stats per station/series -> e.g. rx1hr_summary_gt20y_1981_2015
  # Create summary stats tables for each station:
  rx1hr_summary_gtN_period <- summarise_filtered_ts(ts_gtN_80pc_period)
  # rx1hr_summary_gtN_period_temperature not required, gives same result


  # - plot time series

  # plot_ts_station(df = ts_gtN_80pc_period, station_num = "AU_3003")
  # plot_ts_station(df = ts_gtN_80pc_period, index_num = 4)
  # plot_ts_station(df = ts_gtN_80pc_period, station1 = 1, stationN = 12) # for seasonal,
  # this plots all 12 time series on each season rather than 4 seasons for each station...
  # Does this matter?? Does it happen for monthly data too? (probably)



  # - test estimates (linear trend, mann-kendall significance) -> e.g.

  linear_tests <-
    test_estimates_linear(ts_gtN_80pc_period, trend_type = "year")

  linear_tests_temperature <-
    test_estimates_linear(ts_gtN_80pc_period_temperature,
                          trend_type = "temperature")

  mk_tests <- test_estimates_mk(ts_gtN_80pc_period)
  mk_tests_temp <- test_estimates_mk(ts_gtN_80pc_period_temperature)

  test_results <- linear_tests %>%
    {if (period_type == "Annual")
      left_join(., mk_tests, by = c("Station", "year_start", "year_end")) else . } %>%
    {if (period_type == "Seasonal")
      left_join(., mk_tests, by = c("Station", "year_start", "year_end", "Season")) else . } %>%
    {if (period_type == "Monthly")
      left_join(., mk_tests, by = c("Station", "year_start", "year_end", "Month")) else . }

  test_results_temp <- linear_tests_temperature %>%
    {if (period_type == "Annual")
      left_join(., mk_tests_temp, by = c("Station", "year_start", "year_end")) else . } %>%
    {if (period_type == "Seasonal")
      left_join(., mk_tests_temp, by = c("Station", "year_start", "year_end", "Season")) else . } %>%
    {if (period_type == "Monthly")
      left_join(., mk_tests_temp, by = c("Station", "year_start", "year_end", "Month")) else . }

  # join these results with the statistical summary df:
  test_summaries <- test_results %>%
    {if (period_type == "Annual")
      arrange(., Station, year_start, year_end)
      else . } %>%
    {if (period_type == "Seasonal")
      arrange(., Station, year_start, year_end, Season)
      else . } %>%
    {if (period_type == "Monthly")
      arrange(., Station, year_start, year_end, Month)
      else . } %>%
    full_join(x = rx1hr_summary_gtN_period) %>%
    rename(trend = estimate) %>%
    # add percentage change in trend per year
    mutate(trend_pc_mean = trend * 100 / mean,
          trend_pc_median = trend * 100 / median,
          .after = trend) %>%
    add_domain_ownership(shapefile_list) %>%
    relabel_aggregations(period_type)

  test_summaries_temperature <- test_results_temp %>%
    {if (period_type == "Annual")
      arrange(., Station, year_start, year_end) else . } %>%
    {if (period_type == "Seasonal")
      arrange(., Station, year_start, year_end, Season) else . } %>%
    {if (period_type == "Monthly")
      arrange(., Station, year_start, year_end, Month) else . } %>%
    full_join(x = rx1hr_summary_gtN_period) %>%
    rename(trend = estimate) %>%
    # add percentage change in trend per degree
    mutate(trend_pc_mean = trend * 100 / mean,
          trend_pc_median = trend * 100 / median,
          .after = trend) %>%
            add_domain_ownership(shapefile_list) %>%
    relabel_aggregations(period_type)


}


#########################################################
##################      TABLES      #####################
#########################################################



if (tables == TRUE) {

# - tables of significance

  # loop through various combinations of required options:
  # domains; covariate
  for (cov in c("year", "temperature")) {

    if (cov == "year") {
      df_test <- test_summaries
      cov_label <- ""
    } else {
      df_test <- test_summaries_temperature
      cov_label <- "-temperature"
    }

    for (d in domain_list) {

      print(paste("Creating tables for domain",toupper(d),"w/ covariate",cov))

      lonlims <- get(eval(paste0(d,"_lon_limits")))
      latlims <- get(eval(paste0(d,"_lat_limits")))

      test_table <- make_test_table(df_test, covariate = cov, domain = d,
                                    lon_lims = lonlims, lat_lims = latlims)
      readr::write_csv(test_table,
                       file = paste0(
                         basepath,"tables/station_trends-mk",sig_level,cov_label,
                         "_",dur,"hr_",first_year,"-",last_year,"_",toupper(d),
                         "_",period_type,"_min",proportion,"pc.csv"))

      # # Using gt:
      # gt_table <- test_table %>% ungroup() %>% gt(auto_align = F) %>%
      #   cols_label(
      #     n_total = "Stations",
      #     n_pos = "Positive Trend",
      #     n_pos_sig = "Significant\nPositive Trend",
      #     n_pos_sig_pc = "Proportion Significant\nPositive Trend (%)",
      #     n_neg = "Negative Trend",
      #     n_neg_sig = "Significant\nNegative Trend",
      #     n_neg_sig_pc = "Proportion Significant\nNegative Trend (%)",
      #     ratio_pos_neg_sig = "Ratio +ve Significant Trend /\n-ve Significant Trend",
      #     mean_pc_mean = "Mean percentage/\nchange vs mean (%)",
      #     mean_pc_median = "Mean percentage/\nchange vs median (%)",
      #     median_pc_mean = "Median percentage/\nchange vs mean (%)",
      #     median_pc_median = "Median percentage/\nchange vs median (%)")

      # # # # html
      # # filename = paste0("table-gt_station_trends-mk",
      # #                   sig_level,cov_label,"_",dur,"hr_",
      # #                   first_year,"-",last_year,"_",toupper(d),
      # #                   "_",period_type,"_min",proportion,"pc.html"))

      # # # png ### causing crashes when running on WSL2 ###
      # gtsave(gt_table, path = paste0(basepath,"tables/"),
      #        filename = paste0("table-gt_station_trends-mk",
      #                          sig_level,cov_label,"_",dur,"hr_",
      #                          first_year,"-",last_year,"_",toupper(d),
      #                          "_",period_type,"_min",proportion,"pc.png"))

      # # # rtf (word doc compatible)
      # gtsave(gt_table, path = paste0(basepath,"tables/"),
      #        filename = paste0("table-gt_station_trends-mk",
      #                          sig_level,cov_label,"_",dur,"hr_",
      #                          first_year,"-",last_year,"_",toupper(d),
      #                          "_",period_type,"_min",proportion,"pc.rtf"))

    }
    # }

    # Loop through various clusters as defined by shapefiles:
    for (clustering in names(shapefile_list)) {

      # for (cov in c("year", "temperature")) {
      #   if (cov == "year") {
      #     df_test <- test_summaries
      #     cov_label <- ""
      #   } else {
      #     df_test <- test_summaries_temperature
      #     cov_label <- "-temperature"
      #   }

      test_table_cluster <-
        make_test_table(df_test, covariate = cov, cluster_type = clustering)


      # > names(shapefile_list)
      # [1] "ncra"            "nrm_clust"       "nrm_clust_sub"   "nrm_clust_super" "state"
      if (clustering == "nrm_clust") {
        label_col <- sym("NRM_cluster_label")
        category_col <- sym("NRM_cluster_name")
      }
      if (clustering == "nrm_clust_sub") {
        label_col <- sym("NRM_subcluster_label")
        category_col <- sym("NRM_subcluster_name")
      }
      if (clustering == "nrm_clust_super") {
        label_col <- sym("NRM_supercluster_label")
        category_col <- sym("NRM_supercluster_name")
      }
      if (clustering == "state") {
        label_col <- sym("State_label")
        category_col <- sym("State_name")
        s_col <- sym("label")
      }
      if (clustering == "ncra") {
        label_col <- sym("NCRA_label")
        category_col <- sym("NCRA_name")
      }


      readr::write_csv(test_table_cluster,
                       file = paste0(
                         basepath,"tables/station_trends-mk",sig_level,cov_label,
                         "_",dur,"hr_",first_year,"-",last_year,"_",toupper(clustering),
                         "_",period_type,"_min",proportion,"pc.csv"))

      # # Using gt:
      # gt_table_cluster <- test_table_cluster %>% ungroup() %>% gt(auto_align = F) %>%
      #   cols_label(
      #     !!category_col ~ "Location",
      #     !!label_col ~ "Label",
      #     # NRM_cluster_name = "Location",
      #     # NRM_cluster_label = "Label",
      #     time_period = "Period",
      #     n_total = "Stations",
      #     n_pos = "Positive Trend",
      #     n_pos_sig = "Significant\nPositive Trend",
      #     n_pos_sig_pc = "Proportion Significant\nPositive Trend (%)",
      #     n_neg = "Negative Trend",
      #     n_neg_sig = "Significant\nNegative Trend",
      #     n_neg_sig_pc = "Proportion Significant\nNegative Trend (%)",
      #     ratio_pos_neg_sig = "Ratio +ve Significant Trend /\n-ve Significant Trend",
      #     mean_pc_mean = "Mean percentage/\nchange vs mean (%)",
      #     mean_pc_median = "Mean percentage/\nchange vs median (%)",
      #     median_pc_mean = "Median percentage/\nchange vs mean (%)",
      #     median_pc_median = "Median percentage/\nchange vs median (%)")
      #
      # # # png ### causing crashes when running on WSL2 ###
      # gtsave(gt_table_cluster, path = paste0(basepath,"tables/"),
      #        filename = paste0("table-gt_station_trends-mk",
      #                          sig_level,cov_label,"_",dur,"hr_",
      #                          first_year,"-",last_year,"_",toupper(clustering),
      #                          "_",period_type,"_min",proportion,"pc.png"))

      # # # rtf (word doc compatible)
      # gtsave(gt_table_cluster, path = paste0(basepath,"tables/"),
      #        filename = paste0("table-gt_station_trends-mk",
      #                          sig_level,cov_label,"_",dur,"hr_",
      #                          first_year,"-",last_year,"_",toupper(clustering),
      #                          "_",period_type,"_min",proportion,"pc.rtf"))

    }
  }



  # ## using the MK test for significance:
  # te_table_mk <-
  #   make_test_table(test_summaries, lon_lims = aus_lon_limits, lat_lims = aus_lat_limits)
  #
  # te_table_vic <-
  #   make_test_table(test_summaries, domain = "vic",
  #                   lon_lims = vic_lon_limits, lat_lims = vic_lat_limits)
  #
  # te_table_temp_vic <-
  #   make_test_table(test_summaries_temperature, covariate = "temperature", domain = "vic",
  #                   lon_lims = vic_lon_limits, lat_lims = vic_lat_limits)
  #
  # te_table_temp <- test_summaries_temperature %>%
  #   make_test_table(covariate = "temperature", domain = "aus",
  #                   lon_lims = aus_lon_limits, lat_lims = aus_lat_limits)
  #
  # ### Writing tables to file:
  # # basic csv:
  # readr::write_csv(te_table_mk,
  #                  file = paste0(basepath,"tables/station_trends-mk",sig_level,"_",dur,"hr_",
  #                                first_year,"-",last_year,"_AUS_",period_type,
  #                                "_min",proportion,"pc.csv"))
  # readr::write_csv(te_table_temp,
  #                  file = paste0(basepath,"tables/station_trends-t",sig_level,"-temperature_",
  #                                dur,"hr_",first_year,"-",last_year,"_AUS_",period_type,
  #                                "_min",proportion,"pc.csv"))

  # # Using kable:
  # kable_table <-
  #   te_table_mk %>%
  #    kable(caption = paste0("Trends and significant trends (via M-K test, ",
  #                           sig_level," level) in Rx",dur,"hr data over Australia;",
  #                           " minimum ",proportion,"% of years in period with valid data"),
  #         col.names = c("Season", "Stations",
  #                       "Positive Trend", "Significant\nPositive Trend",
  #                       "Proportion Significant\nPositive Trend (%)",
  #                       "Negative Trend", "Significant\nNegative Trend",
  #                       "Proportion Significant\nNegative Trend (%)",
  #                       "Ratio +ve Significant Trend /\n-ve Significant Trend",
  #                       "Mean percentage change/\nvs mean (%)",
  #                       "Mean percentage change/\nvs median (%)",
  #                       "Median percentage change/\nvs mean (%)",
  #                       "Median percentage change/\nvs median (%)"),
  #         format = "html", align = "c")
  #   # %>%
  #   # kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
  #
  #   readr::write_file(kable_table,
  #                     file = paste0(basepath,
  #                                   "tables/table-kable_station_trends-mk",
  #                                   sig_level,"_",dur,"hr_",
  #                                   first_year,"-",last_year,"_AUS_",
  #                                   period_type,"_min",proportion,"pc.html"))

  # # Using gt:
  # gt_table <-
  #   te_table_mk %>% ungroup() %>%
  #   gt(auto_align = F) %>%
  #   cols_label(
  #     n_total = "Stations",
  #     n_pos = "Positive Trend",
  #     n_pos_sig = "Significant\nPositive Trend",
  #     n_pos_sig_pc = "Proportion Significant\nPositive Trend (%)",
  #     n_neg = "Negative Trend",
  #     n_neg_sig = "Significant\nNegative Trend",
  #     n_neg_sig_pc = "Proportion Significant\nNegative Trend (%)",
  #     ratio_pos_neg_sig = "Ratio +ve Significant Trend /\n-ve Significant Trend",
  #     mean_pc_mean = "Mean percentage/\nchange vs mean (%)",
  #     mean_pc_median = "Mean percentage/\nchange vs median (%)",
  #     median_pc_mean = "Median percentage/\nchange vs mean (%)",
  #     median_pc_median = "Median percentage/\nchange vs median (%)")
  #
  # # gtsave(gt_table, "test_gt_table.png")
  # # gtsave(gt_table, "test_gt_table.pdf")
  # # gtsave(gt_table, "test_gt_table.html")
  #
  # # # # html
  # # gtsave(gt_table, path = paste0(basepath,"tables/"),
  # #        filename = paste0("table-gt_station_trends-mk",
  # #                          sig_level,"_",dur,"hr_",
  # #                          first_year,"-",last_year,"_AUS_",
  # #                          period_type,"_min",proportion,"pc.html"))
  #
  # # # png ### causing crashes when running on WSL2 ###
  # gtsave(gt_table, path = paste0(basepath,"tables/"),
  #        filename = paste0("table-gt_station_trends-mk",
  #                          sig_level,"_",dur,"hr_",
  #                          first_year,"-",last_year,"_AUS_",
  #                          period_type,"_min",proportion,"pc.png"))
  #
  # # # rtf (word doc compatible)
  # gtsave(gt_table, path = paste0(basepath,"tables/"),
  #        filename = paste0("table-gt_station_trends-mk",
  #                          sig_level,"_",dur,"hr_",
  #                          first_year,"-",last_year,"_AUS_",
  #                          period_type,"_min",proportion,"pc.rtf"))
  #
  # gt_table_temp <-
  #   te_table_temp %>% ungroup() %>%
  #   gt(auto_align = F) %>%
  #   cols_label(
  #     n_total = "Stations",
  #     n_pos = "Positive Trend",
  #     n_pos_sig = "Significant\nPositive Trend",
  #     n_pos_sig_pc = "Proportion Significant\nPositive Trend (%)",
  #     n_neg = "Negative Trend",
  #     n_neg_sig = "Significant\nNegative Trend",
  #     n_neg_sig_pc = "Proportion Significant\nNegative Trend (%)",
  #     ratio_pos_neg_sig = "Ratio +ve Significant Trend /\n-ve Significant Trend",
  #     mean_pc_mean = "Mean percentage/\nchange vs mean (%)",
  #     mean_pc_median = "Mean percentage/\nchange vs median (%)",
  #     median_pc_mean = "Median percentage/\nchange vs mean (%)",
  #     median_pc_median = "Median percentage/\nchange vs median (%)")
  #
  # # # # html
  # # gtsave(gt_table_temp, path = paste0(basepath,"tables/"),
  # #        filename = paste0("table-gt_station_trends-t",
  # #                          sig_level,"-temperature_",dur,"hr_",
  # #                          first_year,"-",last_year,"_AUS_",
  # #                          period_type,"_min",proportion,"pc.html"))
  #
  # # # png ### causing crashes when running on WSL2 ###
  # gtsave(gt_table_temp, path = paste0(basepath,"tables/"),
  #        filename = paste0("table-gt_station_trends-t",
  #                          sig_level,"-temperature_",dur,"hr_",
  #                          first_year,"-",last_year,"_AUS_",
  #                          period_type,"_min",proportion,"pc.png"))
  #
  # # rtf (word doc compatible)
  # gtsave(gt_table_temp, path = paste0(basepath,"tables/"),
  #        filename = paste0("table-gt_station_trends-t",
  #                          sig_level,"-temperature_",dur,"hr_",
  #                          first_year,"-",last_year,"_AUS_",
  #                          period_type,"_min",proportion,".rtf"))



}
# end tables section





#########################################################
##################     FIGURES      #####################
#########################################################




if (plotting == TRUE) {

  # dom_list <- c("AUS", "VIC", "SYD", "SEQ")
  dom_list <- c("AUS")
  sig_list <- c(TRUE, FALSE)
  # sig_list <- c(TRUE)
  type_list <- c("magnitude") # c("location", "magnitude")
  trend_type_list <- c("percent_mean") # c("mm", "percent_median", "percent_mean")
  trend_source_list <- c("temperature")
  # trend_source_list <- c("year", "temperature")

  # plots of station trends and significance
  sigtype <- "MK"

  for (dom in dom_list) {

    if (dom == "AUS") {
      lons <- aus_lon_limits
      lats <- aus_lat_limits
    }
    if (dom == "VIC") {
      lons <- vic_lon_limits
      lats <- vic_lat_limits
    }
    if (dom == "SYD") {
      lons <- syd_lon_limits
      lats <- syd_lat_limits
    }
    if (dom == "SEQ") {
      lons <- seq_lon_limits
      lats <- seq_lat_limits
    }

      for (sig in sig_list) {

        if (sig) { sig_label <- "sig-only" } else { sig_label <- "all" }

        for (type in type_list) {

          for (trend_type in trend_type_list) {

            if (trend_type == "mm") {
              type_label <- trend_type
            } else {
              type_label <- gsub("_", "-", trend_type)
            }

            for (trend_source in trend_source_list) {

              if (trend_source == "year") {
                input_df <- test_summaries
              }
              if (trend_source == "temperature") {
                input_df <- test_summaries_temperature
              }

              fig <- plot_rxNday_trends(df = input_df,
                  periodtype = period_type,
                  duration = dur,
                  sig_only = sig,
                  sig_type = "MK",
                  domain = tolower(dom),
                  type = type,
                  covariate = trend_source,
                  trend_measure = trend_type,
                  lon_lims = lons,
                  lat_lims = lats)

              # set up output file name:
              if (sigtype == "MK") { mk_label <- "-mk" } else { mk_label <- "" }

              ggsave(plot = fig,
                path = paste0(basepath,"figures/"),
                filename = paste0("map_station_trends",mk_label,"_",trend_source,
                                  "_",type_label,"_",dur,"hr_",
                                  first_year,"-",last_year,"_",dom,"_",
                                  period_type,"_min",proportion,"pc_",sig_label,".png"))

              rm(fig)

              # factor in:
              # dom sig type trend_type (mm/percent_median/percent_mean) trend_source (year/temp)
              # sample file names
              # map_station_trends-pc_magnitude-mk_1hr_1973-2009_AUS_Annual_min67pc_sig-only.png
              # map_station_trends_magnitude-mk_1hr_1973-2009_AUS_Annual_min67pc_sig-only.png
              # map_station_trends-mk_1hr_1973-2009_AUS_Annual_min67pc_sig-only.png

            }
          }
        }
      }
  }

  # plot_rxNday_trends <- function(df,
  #                                oldest = first_year, newest = last_year,
  #                                periodtype = period_type,
  #                                duration = dur,
  #                                valid_prop = proportion,
  #                                type = "location",
  #                                sig_only = FALSE,
  #                                siglevel = sig_level,
  #                                sig_type = c("t", "MK"),
  #                                trend_period = c("decade", "year"),
  #                                trend_measure = c("mm", "percent_median", "percent_mean"),
  #                                covariate = c("year", "temperature"),
  #                                domain = c("aus", "vic", "syd", "seq"),
  #                                lon_lims, lat_lims)

  if (choropleths) {
    # plot_choropleth <- function(df = test_summaries_temperature,
    #                             cluster_type, shapefile_list) {
    #   # Get
    #   #
    # }


    for (shape in names(shapefile_list)) {
      s <- read_sf(shapefile_list[shape])
      print(names(s))
    }
      # > names(shapefile_list)
      # [1] "ncra"            "nrm_clust"       "nrm_clust_sub"   "nrm_clust_super" "state"
      # shape <- "nrm_clust_sub"
      # s_col <- sym("label")
      if (clustering == "nrm_clust") {
        label_col <- sym("NRM_cluster_label")
        category_col <- sym("NRM_cluster_name")
      }
      if (clustering == "nrm_clust_sub") {
        label_col <- sym("NRM_subcluster_label")
        category_col <- sym("NRM_subcluster_name")
      }
      if (clustering == "nrm_clust_super") {
        label_col <- sym("NRM_supercluster_label")
        category_col <- sym("NRM_supercluster_name")
      }
      if (clustering == "state") {
        label_col <- sym("State_label")
        category_col <- sym("State_name")
        s_col <- sym("label")
      }
      if (clustering == "ncra") {
        label_col <- sym("NCRA_label")
        category_col <- sym("NCRA_name")
      }


      print(shape)
      s <- read_sf(shapefile_list[shape])
      # create summary spatial statistics
      stats_spatial <- test_summaries_temperature %>%
        # filter(Season == "MAM") %>%
        # filter(Season == "DJF") %>%
        group_by(!!label_col, !!category_col) %>%
        {if (period_type == "Seasonal")
          group_by(., Season, .add = TRUE) else . } %>%
        {if (period_type == "Monthly")
          group_by(., Month, .add = TRUE) else . } %>%
        summarise(n = n(),
                  mean_trend = round(mean(trend_pc_mean, na.rm = TRUE), digits = 1)) %>%
        drop_na()

    # # join with s for spatial plotting
    # stats_spatial %>%
    #     left_join(s, by = c(label_col = "label")) %>%
    #     st_as_sf()

      # stats_spatial %>% ggplot(aes(x = NCRA_label, y = mean_trend)) +
      #   geom_col(aes(fill = mean_trend)) +
      #   # coord_flip() +
      #   ggtitle("Average of station-based trends within NCRA regions")

  #       # t <- st_as_sf(t)
    range_lim <- max(abs(min(stats_spatial$mean_trend)), abs(max(stats_spatial$mean_trend)))
  # stats_spatial %>%
  #   left_join(s, by = c(!!label_col = "label"))
  #   # %>%
  #   # left_join(s, by = c("NRM_cluster_name" = "label")) %>%

  # library(mapdata)
  # choropleth_plot <-
    stats_spatial %>%
    left_join(s, by = join_by(!!label_col == "code")) %>%
    st_as_sf() %>%
    ggplot() +
    # colour in land masses outside Aus
    annotation_map(map_data("worldHires"), col = "grey25", alpha = 0.20) +
    geom_sf(aes(fill = mean_trend)) +
    # scale_fill_distiller(palette = "RdBu", direction = -1,
    # scale_fill_paletteer_c(palette = "ggthemes::Orange-Blue Diverging", direction = 1,
      scale_fill_paletteer_c(palette = "ggthemes::Red-Blue-White Diverging", direction = 1,
    # scale_fill_scico(palette = 'roma', alpha = 0.85,
    # scale_fill_scico(palette = 'vik', alpha = 0.85, direction = -1,
    # scale_fill_gradient2(low = "brown", high = "blue", midpoint = 0,
                        name = "Mean trend\n(% of mean\nper decade)",
                        limits = c(-1*range_lim, range_lim)) +
    coord_sf(xlim = aus_lon_limits, ylim = aus_lat_limits, crs = sf::st_crs(4326)) +
    # add coastline for Aus over the top of fill
    annotation_map(map_data("worldHires"), col = "grey25", alpha = 0) +
    geom_sf_label(aes(label = paste(!!label_col, mean_trend, "\nn =", n)), size = 3
                  # ,
                  # fun.geometry = sf::st_centroid
                  ) +
      # geom_sf_label(aes(label = !!label_col)) +
    theme_bw() +
      labs(y = "Latitude", x = "Longitude") +
    facet_wrap(~ Season)

  ########################################################################################
  ### NRM cluster choropleth plot:  ######################################################
  ########################################################################################
    stats_spatial %>%
    left_join(s, by = join_by(!!label_col == "code")) %>%
    st_as_sf() %>%
    ggplot() +
      # colour in land masses outside Aus
      annotation_map(map_data("worldHires"), col = "grey25", alpha = 0.20) +
      geom_sf(aes(fill = !!label_col))   +
      # scale_fill_distiller(palette = "RdBu", direction = -1,
      # scale_fill_paletteer_c(palette = "ggthemes::Orange-Blue Diverging", direction = 1,
        scale_fill_paletteer_d(palette = "tidyquant::tq_dark", 
      # scale_fill_scico(palette = 'roma', alpha = 0.85,
      # scale_fill_scico(palette = 'vik', alpha = 0.85, direction = -1,
      # scale_fill_gradient2(low = "brown", high = "blue", midpoint = 0,
                          name = "NRM\ncluster"
                          #  limits = c(-1*range_lim, range_lim)
                          ) +
      coord_sf(xlim = aus_lon_limits, ylim = aus_lat_limits, crs = sf::st_crs(4326)) +
      # add coastline for Aus over the top of fill
      annotation_map(map_data("worldHires"), col = "grey25", alpha = 0) +
      # geom_sf_label(aes(label = paste(!!label_col)), size = 3
      geom_sf_label(aes(label = paste(!!category_col)), size = 3
                    ,fun.geometry = sf::st_centroid
                    ) +
        # geom_sf_label(aes(label = !!label_col)) +
      theme_bw() +
        labs(y = "Latitude", x = "Longitude") 
    

  # theme(panel.background = element_rect(fill = "lightsteelblue1"))
      ##### working!

      # select lats and lons from df
      # points_df <- tibble(id = df$Station, Latitude = df$Latitude, Longitude = df$Longitude)
      # # convert to sf geometry
      # points_sf <- st_as_sf(points_df, coords = c("Longitude", "Latitude"), crs = 4326)
      # # ensure they're on the same projection:
      # points_sf <- st_transform(points_sf, crs = st_crs(s))
      # points_with_domains <- st_join(points_sf, s) %>%
      #   {if (shape == "ncra")
      #     transmute(., Station = id, NCRA_name = regionname, NCRA_label = label) %>% st_drop_geometry() else . } %>%
      #   {if (shape == "nrm_clust")
      #     transmute(., Station = id, NRM_cluster_name = label, NRM_cluster_label = code) %>% st_drop_geometry() else . } %>%
      #   {if (shape == "nrm_clust_sub")
      #     transmute(., Station = id, NRM_subcluster_name = label, NRM_subcluster_label = code) %>% st_drop_geometry() else . } %>%
      #   {if (shape == "nrm_clust_super")
      #     transmute(., Station = id, NRM_supercluster_name = label, NRM_supercluster_label = code) %>% st_drop_geometry() else . } %>%
      #   {if (shape == "state")
      #     transmute(., Station = id, State_name = STE_NAME21) %>%
      #     mutate(State_name = replace_na(State_name, "unknown")) %>%
      #     mutate(State_label = map_chr(State_name, ~ state_list[[.x]])) %>%
      #       st_drop_geometry() else . }

      # # join with df:
      # df <- df %>% left_join(points_with_domains, by = "Station", multiple = "first")

    # } # end shape for loop

  }

# end plotting section
}



#########
# works to here for monthly, seasonal, annual
#########



#### Add GEV analysis?
# run gev on all maxima
# retain output values in .rdata file for comparison with other time periods?







#### NOW TEST ON OTHER DURATIONS...  ####

# diurnal cycle
# gridded data

# what about relationships with climate drivers?
# - ENSO/SOI/NINO3.4
# - IOD
# - SAM

if (exists("nino_bom_noaa")) {
  # read in NINO3.4 index 

  ## from BoM:
  nino34_bom <- read_csv(file = 'Climate Indices/nino34-monthly_190001-202403.txt', col_names = F) %>% 
    mutate(year = as.integer(str_sub(X1, start = 1, end = 4)),
          month = as.integer(str_sub(X1, start = 5, end = 6)),
          date = ym(paste(year, month, sep = "-")),
          anomaly = X2, .before = X1) %>% 
    select(-X1, -X2)

  nino34_bom_1900to2023 <- nino34_bom %>% filter(between(year, 1900, 2023))


  ## from NOAA
  # Nino 3.4 anomalies (1981-2010 mean removed!)
  nino34_noaa_fwf <- read_fwf(file = 'NINO3.4/nino34.long.anom.data.txt', skip = 1, 
                    col_positions = fwf_widths(c(6,rep(8,12))), n_max = 155,
                    na = '-99.99', col_types = 'idddddddddddd')
  colnames(nino34_noaa_fwf) <- 
    # c('year', as.character(sprintf("%02d", seq(1,12))))
    c('year', as.character(seq(1,12)))
  nino34_noaa_long <- nino34_noaa_fwf %>% 
    pivot_longer(cols = !year, names_to = 'month', values_to = 'anomaly') %>% 
    mutate(date = ym(paste(year,month,sep='-')), 
          year = as.integer(year), 
          month = as.integer(month),
          .before = anomaly) 

  nino34_noaa_long_1900to2023 <- nino34_noaa_long %>% filter(between(year, 1900, 2023))



  # plots
    
    
  # ggplot(nino34_long_1900to2023, aes(date, anomaly)) + 
  #   geom_point(colour = 'red', alpha = 0.6) + 
  #   geom_smooth(method = "lm", colour = 'black') +
  #   ggtitle('NINO 3.4 index (anomalies from 1981-2010)', subtitle = "Source: NOAA")

    
  # ggplot(nino34_1900to2023, aes(date, anomaly)) + 
  #   geom_line(colour = 'black', alpha = 0.8) + 
  #   geom_line(data = nino34_long_1900to2023, aes(date, anomaly), colour = 'red', alpha = 0.8) + 
  #   # geom_point(colour = 'orange', alpha = 0.6) + 
  #     # geom_smooth(method = "lm", colour = 'blue') +
  #       ggtitle('NINO 3.4 index', subtitle = "Source: BoM")


    
    

  nino34_noaa_annmean <- nino34_noaa_long %>% group_by(year) %>% 
    summarise(mean_anomaly = mean(anomaly)) %>% 
    mutate(date = ym(paste(year,"7",sep = '-')), .after = year,
          decadal_mean = slide_dbl(.x = mean_anomaly, .f = mean, 
            .before = 5, .after = 5, .complete = TRUE)) %>% 
              filter(year >= 1960, year <= 2015)

  nino34_noaa_indices <- nino34_noaa_long %>% 
    mutate(
      NINO34_index = slide_dbl(.x = anomaly, .f = mean, .before = 2, .after = 2, .complete = TRUE),
      ONI_index = slide_dbl(.x = anomaly, .f = mean, .before = 1, .after = 1, .complete = TRUE),
      decadal_mean_monthly = slide_dbl(.x = anomaly, .f = mean, .before = 60, .after = 60, .complete = TRUE),
      elevenyear_mean_monthly = slide_dbl(.x = anomaly, .f = mean, .before = 66, .after = 66, .complete = TRUE)
    ) %>% 
      filter(year >= 1960, year <= 2015)


  nino34_noaa_indices %>% 
    ggplot(aes(date, NINO34_index)) + 
    geom_line(colour = 'red', linewidth = 1.1, alpha = 0.6) +
    geom_line(data = nino34_noaa_indices, aes(date, decadal_mean_monthly), colour = "black", alpha = 0.8)

  nino34_noaa_annmean %>% 
    ggplot(aes(date, decadal_mean)) + 
    geom_line(colour = 'blue', linewidth = 1.1, alpha = 0.6) +
    geom_line(data = nino34_noaa_indices, aes(date, decadal_mean_monthly), 
              colour = "orange", linewidth = 1.1, alpha = 0.8) +
    geom_line(data = nino34_noaa_indices, aes(date, elevenyear_mean_monthly), 
              colour = "red", linewidth = 1.1, alpha = 0.8) +
    geom_line(data = nino34_noaa_indices, aes(date, NINO34_index), 
              colour = "red", linewidth = 1.1, alpha = 0.8) 
    


  nino34_combined_1900to2023 <- 
    full_join(nino34_bom_1900to2023, nino34_noaa_long_1900to2023, 
              by = c('year', 'month', 'date'),
              suffix = c('_bom', '_noaa')) %>% 
    # mutate(diff = anomaly_bom - anomaly_noaa) %>% 
    pivot_longer(cols = c("anomaly_bom", "anomaly_noaa"), 
                #  values_to = "anomaly",
                names_to = c(".value", "source"),
                  # "source",
                names_sep = "_")

  nino34_combined_1900to2023 %>% 
    filter(date >= ymd("1960-01-01")) %>% 
    ggplot(aes(date, anomaly, colour = str_to_upper(source))) +
    geom_line(alpha = 0.7) +
    geom_smooth(method = "lm") +
    scale_colour_manual(values = c("red", "blue"), name = "Source")

}
  

##################################################################
###################     ENSO TRENDS    ###########################
##################################################################


if (drivers == TRUE) {

  
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
  load('mean_seasonal_nino34.Rdata')  # contains 'mean_nino34'
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
    require(broom)
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
  
  test_estimates_mk_nino <- function(df,
    periodtype = period_type,
    oldest = first_year, newest = last_year) {

    require(zoo)
    require(Kendall)

    df %>%
    group_by(season, .add = TRUE) %>%
    group_modify(~ modified_mk_test(.x)) %>%
    mutate(year_start = oldest, year_end = newest, .before = season)

  }

  linear_tests_nino <-
    test_estimates_nino(mean_nino34, trend_type = "year")

  mk_tests_nino <- test_estimates_mk_nino(mean_nino34)

  test_results_nino <- 
    left_join(linear_tests_nino, mk_tests_nino, by = c("year_start", "year_end", "season")) %>% 
    rename(Season = season, trend = estimate)
  
  readr::write_csv(test_results_nino,
    file = paste0(basepath,"tables/nino34_trends-mk_",first_year,"-",last_year,"_seasonal.csv"))


  # calc_seasonal_mean <- function(df = nino34_df) {
  #   seasonal_means <- aggregate(nino34_index ~ season, data = df, FUN = mean)
  #   return(seasonal_means)
  # }
    
  # seasonal_means <- calc_seasonal_mean(nino34_df)

  # # library(zoo)
  # ym <- as.yearmon(paste(year(nino34_df$date), month(nino34_df$date)), "%B %y")
  # yq <- as.yearqtr(ym + 1/12)

  # Ag <- aggregate(value ~ yq, DF, mean)

  # season.name <- c("DJF", "MAM", "JJA", "SON")
  # with(Ag, data.frame(year = as.integer(yq), season = season.name[cycle(yq)], value))


}





if (exists(extend_data)) {


    clex_colnames = c('station_ID', 'station_name', 'latitude', 'longitude', 'month_start', 'month_end', 
                      'years', 'percent_complete', 'obs', 'AWS')

    clex_stations_30min <- read_fwf(file = 'CLEX_sub-daily_data/alphaAUS_269_half-hourly.txt', skip = 4, 
      fwf_widths(c(8, 41, 10, 9, 9, 10, 5, 5, 8, 4), col_names = clex_colnames),
      col_types = 'icddccdddc') %>% 
      # , col_names = clex_colnames) 
      slice(1:(n()-6))

    clex_stations_01min <- read_fwf(file = 'CLEX_sub-daily_data/alphaAUS_304_one-minute.txt', skip = 4, 
                                    fwf_widths(c(8, 41, 10, 9, 9, 10, 5, 5, 8, 4), col_names = clex_colnames),
                                    col_types = 'ccddccdddc') %>% 
                                    slice(1:(n()-6)) %>% 
      mutate(station_ID = paste0('AU_', as.character(station_ID)))
                                      

    # Which stations are in this CLEX data that were in the previous years?
    in_both <- intersect(
      union(clex_stations_01min$station_ID, clex_stations_30min$station_ID),
      unique(test_summaries$Station))
    # > length(in_both)
    #   [1] 71
    # > length(test_summaries$Station)
    # [1] 930
    # > length(rbind(clex_stations_01min$station_ID, clex_stations_30min$station_ID))
    # Warning message:
    # In rbind(clex_stations_01min$station_ID, clex_stations_30min$station_ID) :
    #   number of columns of result is not a multiple of vector length (arg 1)
    # [1] 1254
    # > length(rbind(clex_stations_01min$station_ID, clex_stations_30min$station_ID) %>% sort() %>% unique())
    # Warning message:
    # In rbind(clex_stations_01min$station_ID, clex_stations_30min$station_ID) :
    #   number of columns of result is not a multiple of vector length (arg 1)
    # [1] 1225

}




# Met with Todd 2/5/24.
# Discussed the need for sanity check on some of the trends.
# Need to understand the "WHY" for some of the regional clustering of trends.
# Think about how to prove these trends aren't noise.
## -> A proportion(?) of stations will be excluded because of incomplete/short
## time series perhaps? This may increase the effective number of sig trends?
## -> probably well over 5% of stations in some geographic areas/times of year.
## Do some aggregations/filtering by state/region?
# Plot SON stations with sig trends in SEQ domain
# Try to match trends (start/end years) with Hooman's paper over Syd
# Think about how to synthesise information from these plots...?

###
# Make sure we are plotting stations with adequate data coverage for the whole
# period specified...
# ATM, we might be seeing stations with trends only in the last ~20 years of the
# period (but with complete data for that part of the time slice).
###

# produce TS plots of all stations with sig trends
# sig_plots <- test_summaries %>%
#   filter(p.value_mk <= siglevel) %>%
#   select(Station) %>%
#   # head(3) %>%
#   pull() %>%
#   purrr::map(~ plot_ts_station(station_num = .x))
#
# # purrr::map(sig_stations[[1]][1:3],
# #            ~ plot_ts_station(station_num = .x),
# #            .progress = T)
# #
# # sig_stations[[1]] %>%
# #   purrr::map(\(x) plot_ts_station(station_num = x),
# #              .progress = T)
#
# n_sig_stations <-
#   length(test_summaries %>%
#            filter(p.value_mk <= siglevel) %>%
#            select(Station) %>% pull())
#
# index_multiple <- 1
#
# sig_plots[[1]] / sig_plots[[2]] / sig_plots[[3]] / sig_plots[[4]]
# sig_plots[[index_multiple]] / sig_plots[[index_multiple+1]] /
#   sig_plots[[index_multiple+2]] / sig_plots[[index_multiple+3]]
#
# purrr::map(~ plot_ts_station(station_num = .x) +
#              expand_limits(x = c(as.Date(paste0(first_year,"-01-01")),
#                                  as.Date(paste0(last_year,"-01-01")))))
