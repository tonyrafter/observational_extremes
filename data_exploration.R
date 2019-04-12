library(tidyverse)


col_names_metadata_waterregs <- 
  c("Station number", "Agency", "Latitude", "Longitude", "Elevation", "First Date", "First Year", 
    "Last Date", "Last Year", "Number of Years", "Station Name")
metadata_waterregs <- 
  read.csv('BoMdata/Station_Metadata_FinalIFD_Water Regulations_Continuous_reimported.csv', 
           col.names = col_names_metadata_waterregs)

plot1 <- metadata_waterregs %>% ggplot(aes(x = Longitude, y = Latitude)) + geom_point()
plot1 + geom_point(aes(col = Number.of.Years))
plot1 + geom_point(aes(col = First.Year))

metadata_waterregs$Agency <- as.factor(metadata_waterregs$Agency)

metadata_waterregs %>% group_by(Agency) %>%
  ggplot(aes(x=Longitude, y=Latitude, col=Agency)) +
  geom_point()

metadata_waterregs %>% filter(Number.of.Years > 25) %>% group_by(Agency) %>%
  ggplot(aes(x=Longitude, y=Latitude, col=Agency)) +
  geom_point()


# metadata_waterregs %>% group_by(Agency) %>%
#   ggplot(aes(x=First.Year)) +
#   geom_histogram() +
#   facet_grid(~ Agency)

metadata_waterregs %>% group_by(Agency) %>%
  ggplot(aes(x=First.Year, col=Agency, fill=Agency)) +
  geom_histogram(bins=15, alpha = 0.2) 

metadata_waterregs %>% group_by(Agency) %>%
 ggplot(aes(x=First.Year, y=Last.Year, col=Agency)) +
 geom_jitter()


# metadata_waterregs %>% group_by(Agency) %>%
#   summarise(meanNumberOfYears = mean(Number.of.Years)) %>%
#   ggplot(aes(x=First.Year, y=meanNumberOfYears, col=Agency)) +
#   geom_jitter()



# Load in station lookup table from README page of water regs .xls file
metadata_waterregs_stations <- read_csv('BoMdata/Station_Metadata_FinalIFD_Water Regulations_Continuous_README_page.csv', skip = 18, col_names = c("Agency", "OrganisationName", "State"))
# Remove blank rows and values
metadata_waterregs_stations <- metadata_waterregs_stations %>% drop_na()
# Lok at the DF:
metadata_waterregs_stations
# # A tibble: 18 x 3
# Agency OrganisationName                                                      State
# <chr>  <chr>                                                                 <chr>
#   1 2      ACTEW Corporation Limited                                             ACT  
# 2 13     Barwon Region Water Corporation (Barwon Water)                        VIC  
# 3 32     Central Highlands Water                                               VIC  
# 4 58     Department of Commerce                                                NSW  
# 5 66     Department of Natural Resources and Water                             QLD  
# 6 67     Department of Natural Resources Environment and the Arts              NT   
# 7 74     Department of Sustainability and Environment                          VIC  
# 8 75     Department of Territory and Municipal Services                        ACT  
# 9 76     Department of Water                                                   WA   
# 10 77     Department of Water and Energy                                        NSW  
# 11 78     Department of Water Land and Biodiversity Conservation                SA   
# 12 102    Gippsland and Southern Rural Water Corporation (Southern Rural Water) VIC  
# 13 129    Hydro-electric Corporation (Hydro Tasmania)                           TAS  
# 14 151    Melbourne Water Corporation (Melbourne Water)                         VIC  
# 15 166    National Capital Authority                                            ACT  
# 16 205    Snowy Hydro Limited                                                   OTHER
# 17 231    Sydney Catchment Authority                                            NSW  
# 18 233    Sydney Water Corporation (Sydney Water)                               NSW  



# Make a copy for now...
metacopy <- metadata_waterregs
# Add Org name and State to our DF
metacopy <- merge(metacopy, metadata_waterregs_stations[,c("Agency", "OrganisationName", "State")], by = 'Agency')

# Now we can show all stations from certain juristictions...
# Show stations in NSW/ACT/Snowy Hydro ("OTHER") colored by org, on a plot with fixed 1:1 aspect ratio:
metacopy %>% filter(State == 'NSW' | State == "ACT" | State == 'OTHER') %>%
  ggplot(aes(x=Longitude, y=Latitude, col=OrganisationName)) +
  geom_point(alpha = 0.4) +
  coord_fixed(1) +
  theme(legend.position="right",legend.direction="vertical")

# VIC only, with legend at bottom (to accommodate for VIC being wider than it is tall)
metacopy %>% filter(State == 'VIC') %>%
  ggplot(aes(x=Longitude, y=Latitude, col=OrganisationName)) +
  geom_point(alpha = 0.4) +
  coord_fixed(1) +
  theme(legend.position="bottom",legend.direction="vertical")

# TAS only, with legend at bottom 
metacopy %>% filter(State == 'TAS') %>%
  ggplot(aes(x=Longitude, y=Latitude, col=OrganisationName)) +
  geom_point() +
  coord_fixed(1) +
  theme(legend.position="bottom",legend.direction="vertical")

# SA only, with legend at bottom 
metacopy %>% filter(State == 'SA') %>%
  ggplot(aes(x=Longitude, y=Latitude, col=OrganisationName)) +
  geom_point() +
  coord_fixed(1) +
  theme(legend.position="bottom",legend.direction="vertical")



# Google maps API key: AIzaSyDJeNzaBarybVQ7G4-eZmgD02xAtyOiuBc
api_key <- 'AIzaSyDJeNzaBarybVQ7G4-eZmgD02xAtyOiuBc'



rbind(as.numeric(paste(geocode_OSM("Sydney")$bbox)))
register_google(api_key)
nsw <- c(left = 140, right = 154, bottom = -38, top = -28)

# nsw_map <- ggmap(get_map(nsw, maptype = "terrain-background", zoom = 6, col = "bw"))
nsw_map <- ggmap(get_map(nsw, maptype = "toner-lite", zoom = 6))

nsw_map +
geom_point(data = metacopy, aes(x = Longitude, y = Latitude, col = Agency), alpha = 0.6)

nsw_data <- metacopy %>% filter(State == "NSW" | State == "ACT" | State == "OTHER")

nsw_map +
geom_point(data = nsw_data, aes(x = Longitude, y = Latitude, col = OrganisationName), alpha = 0.6)

nsw_map <- ggmap(get_map(nsw, maptype = "toner-lite", zoom = 6), base_layer = ggplot(data = nsw_data, aes(Longitude, Latitude)))

nsw_map +
  geom_point(aes(col = OrganisationName), alpha = 0.6)


qmplot(Longitude, Latitude, color = OrganisationName, data = nsw_data, geom = "point", alpha = I(0.6)) +
  facet_wrap(~ Agency)




# Where are my domain bounds?
syd_domain <- c(left = 148.7, right = 153.2, top = -31.9, bottom = -35.9)
syd_metacopy <- metacopy %>% 
  filter(Longitude > syd_domain['left'] & Longitude < syd_domain['right'] & 
           Latitude < syd_domain['top'] & Latitude > syd_domain['bottom'])

syd_map <- ggmap(get_map(syd_domain, maptype = "toner-lite", zoom = 8), 
                 base_layer = ggplot(data = syd_metacopy, aes(Longitude, Latitude)))
syd_map +
  geom_point(aes(col = OrganisationName), alpha = 0.6)


# Points with station record >= 30 years
syd_metacopy %>%
  filter((Last.Year - First.Year + 1) >= 30) %>%
  ggplot(aes(x = Longitude, y = Latitude, col = OrganisationName)) +
  annotation_map(map_data("world"), fill = "NA", col = "grey25") +
  geom_point(alpha = 0.6) +
  coord_quickmap() +
  labs(col = "Organisation")

# Points with station record >= 20 years
syd_metacopy %>%
  filter((Last.Year - First.Year + 1) >= 20) %>%
  ggplot(aes(x = Longitude, y = Latitude, col = OrganisationName)) +
  annotation_map(map_data("world"), fill = "NA", col = "grey25") +
  geom_point(alpha = 0.6) +
  coord_quickmap() +
  labs(col = "Organisation")

# Points with station record >= 25 years ### USE THIS ###
syd_metacopy %>%
  filter((Last.Year - First.Year + 1) >= 25) %>%
  ggplot(aes(x = Longitude, y = Latitude, col = OrganisationName)) +
  annotation_map(map_data("world"), fill = "NA", col = "grey25") +
  geom_point(alpha = 0.6) +
  coord_quickmap() +
  labs(col = "Organisation")

# Histogram of station durations (from first year and last year)
syd_metacopy %>%
  ggplot(aes(x = Number.of.Years, fill = OrganisationName)) +
  geom_histogram(binwidth = 1) +
  guides(fill = guide_legend(ncol = 2)) +
  #theme_classic() +
  theme(legend.position="bottom",legend.direction="vertical") +
  labs(fill = "Organisation", x = "Length of Station Record (years)")



# Some new plotting backgrounds:
#syd_map <- ggmap(get_map(syd_domain, maptype = "toner-lite", zoom = 8), 
#                 base_layer = ggplot(data = syd_metacopy, aes(Longitude, Latitude)))
syd_centred <- c(lon = (syd_domain['left'] + syd_domain['right'])/2, lon = (syd_domain['top'] + syd_domain['bottom'])/2)

white_map <- ggmap(get_snazzymap(syd_centred, mapRef = "https://snazzymaps.com/style/115/white-landscape", zoom = 7),
                   base_layer = ggplot(data = syd_metacopy, aes(Longitude, Latitude)))

white_map +
  geom_point(aes(col = OrganisationName), alpha = 0.6)


# Try using the worldHires dataset from the mapdata package:
library(maps)
library(mapdata)
map <- fortify(map("worldHires", fill = TRUE, plot = FALSE))

syd_hires_basemap <- ggplot(data = subset(map, region == "Australia"), aes(x=long, y=lat, group = group)) +
  geom_polygon(fill = "ivory2") +
  geom_path(colour = "black") +
  coord_quickmap(xlim = c(syd_domain['left'], syd_domain['right']), ylim = c(syd_domain['bottom'], syd_domain['top'])) +
  theme(panel.background = element_rect(fill = "#F3FFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

syd_hires_basemap +
  geom_point(inherit.aes = FALSE, data = syd_metacopy, aes(Longitude, Latitude, color = OrganisationName), alpha = 0.6)




# Create a table / tibble of how many stations have records longer than X years:
syd_metacopy %>%
  filter(Number.of.Years >= 25) %>%
  group_by(OrganisationName) %>%
  summarise(NumberOfStations = n()) %>%
  arrange(desc(NumberOfStations))
# # A tibble: 7 x 2
# OrganisationName                               NumberOfStations
# <chr>                                                     <int>
# 1 Sydney Catchment Authority                                   66
# 2 Sydney Water Corporation (Sydney Water)                      20
# 3 ACTEW Corporation Limited                                    16
# 4 Department of Territory and Municipal Services               14
# 5 Department of Commerce                                       10
# 6 Department of Water and Energy                                6
# 7 National Capital Authority                                    3





# Now, import metadata for BoM continuous stations:
# Header row:
# Stn Number,Latitude,Longitude,Elevation,First Year,Last Year,Number of Years
metadata_bom_cont <- 
  read.csv('BoMdata/Station_Metadata_FinalIFD_Bureau_continuous.csv', 
           header = TRUE)

# Get station information for our Sydney domain:
syd_bom_cont <- metadata_bom_cont %>% 
  filter(Longitude > syd_domain['left'] & Longitude < syd_domain['right'] & 
           Latitude < syd_domain['top'] & Latitude > syd_domain['bottom'])


# Points with station record >= 25 years ### USE THIS ###
syd_bom_cont %>%
  filter(Number.of.Years >= 25) %>%
  ggplot(aes(x = Longitude, y = Latitude)) +
  annotation_map(map_data("world"), fill = "NA", col = "grey25") +
  geom_point(alpha = 0.6, col = "red") +
  coord_cartesian(xlim = c(syd_domain['left'], syd_domain['right']),
                  ylim = c(syd_domain['top'], syd_domain['bottom'])) +
  coord_quickmap() 



#######################################
########## DATA ANALYSIS ##############
#######################################

# Testing reading in a BoM continuous station:
station_file <- 'BoMdata/qcd_061211.txt'
station_df <- 
  read_table2(station_file, 
              col_names = c("Station", "Datestamp", "Value", "Flag", "Decision"), 
              col_types = c("ccddc"))
# Set datestamp to R datetime format
station_df <- station_df %>% 
  mutate(Datestamp = ymd_hm(Datestamp))

# How many values are flagged, and with which flag?
station_df %>% group_by(as.factor(Flag)) %>% summarise(n())
# # A tibble: 3 x 2
# `as.factor(Flag)`   `n()`
# <fct>               <int>
#   1 -999               459648
# 2 -666                 2304
# 3 0                 4595904

# Plot up proportion of data that is flagged vs unflagged
station_df %>% 
  group_by(year = year(Datestamp), flag = as.factor(Flag)) %>% 
  summarise(nObs = n()) %>% 
  ggplot(aes(x = year, y = nObs, fill = flag)) + geom_col()
# Shows that a fair proportion of some years are flagged. Most years > 3/4 complete.)

station_df %>% filter(Value < 0) %>% summarise(n())
# # A tibble: 1 x 1
# `n()`
# <int>
#   1 575441
station_df %>% filter(Value >= 0) %>% summarise(n())
# # A tibble: 1 x 1
# `n()`
# <int>
#   1 4482415
# Percentage of data flagged:
# > 57544100 / 4482415
# [1] 12.83774 (percent)


# How about how many Values are flagged (negative)?
station_df_noflags <- station_df %>% filter(!(Flag < 0))
station_df_noflags %>% 
  group_by(year = year(Datestamp), flag = as.factor(Value < 0)) %>% 
  summarise(nObs = n()) %>% 
  ggplot(aes(x = year, y = nObs, fill = flag)) + 
  geom_col()
# Shows most years early in the period have significant (~5-10%) values that are negative (missing?)

# Plot positive values as True:
# As a proportion of entire data set:
station_df %>% 
  group_by(year = year(Datestamp), PositiveValues = as.factor(Value >= 0)) %>% 
  summarise(nObs = n()) %>% 
  ggplot(aes(x = year, y = nObs, fill = PositiveValues)) + 
  geom_col()

# As proportion of unflagged data:
station_df_noflags %>% 
  group_by(year = year(Datestamp), NegativeValues = as.factor(Value < 0)) %>% 
  summarise(nObs = n()) %>% 
  ggplot(aes(x = year, y = nObs, fill = NegativeValues)) + 
  geom_col()


# Now do a histogram with negative values removed:
station_df_noflags %>% 
  filter(Value >= 0) %>%
  group_by(year = year(Datestamp)) %>% 
  summarise(nObs = n()) %>% 
  ggplot(aes(x = year, y = nObs)) + 
  geom_col()
# This doesn't show the proportion of values removed...

# Try to show the number of positive values instead:
station_df %>%
group_by(year = year(Datestamp), positive = as.factor(Value >= 0)) %>%
summarise(nObs = n()) %>%
ggplot(aes(x = year, y = nObs, fill = positive)) +
geom_col()


# Look at those rows that have flag -666:
station_df %>% filter(Flag == -666) %>% group_by(Decision) %>% summarise(n())
# # A tibble: 1 x 2
# Decision `n()`
# <chr>    <int>
#   1 ACCEPT    2304

# Remove missing values (flag = -999, Value < 0)
station_df[station_df == -999] <- NA
station_df <- station_df %>% filter(Value >= 0)
station_df_noflags <- station_df %>% filter(!(Flag < 0))


# To enable grouping by month/season/time of day, need to add some specific fields
station_df <- station_df %>% 
  mutate(year = year(Datestamp), 
         month = month(Datestamp), 
         day = day(Datestamp), 
         hour = hour(Datestamp))


# Look at dataset completeness by year:
station_df %>% group_by(year) %>% summarise(nObs = n()) %>% ggplot(aes(x = year, y = nObs)) + geom_col()
# Pretty good completeness there! Just the first and last years missing some data.
# Remove first, last years:
station_df <- station_df %>% filter(year != 1962, year != 2010)

# Now, do some initial examinations of the data:
maxRainByMonth <- station_df %>% group_by(month) %>% summarise(maxRainfall = max(Value))
# # A tibble: 12 x 2
# month maxRainfall
# <dbl>       <dbl>
#   1     1        14.2
# 2     2        15.2
# 3     3         8.2
# 4     4         6.7
# 5     5         5.4
# 6     6        32  
# 7     7         9.6
# 8     8         8.8
# 9     9         6  
# 10    10        12.5
# 11    11         9.4
# 12    12        24.9

maxRainByYear <- station_df %>% group_by(year) %>% summarise(maxRainfall = max(Value))
print(maxRainByYear, n = Inf)
# # A tibble: 49 x 2
# year maxRainfall
# <dbl>       <dbl>
#   1  1962         9.4
# 2  1963         8.3
# 3  1964         9.1
# 4  1965         3  
# 5  1966         8.2
# 6  1967        10.2
# 7  1968         7.9
# 8  1969         8.8
# 9  1970         8.4
# 10  1971         8.8
# 11  1972        12.3
# 12  1973        11.9
# 13  1974        12.5
# 14  1975         6  
# 15  1976         7.1
# 16  1977         5.7
# 17  1978        32  
# 18  1979         3  
# 19  1980         6.7
# 20  1981        10.4
# 21  1982        15  
# 22  1983         7.1
# 23  1984         4.5
# 24  1985         4.2
# 25  1986         5.3
# 26  1987         7.4
# 27  1988        10.3
# 28  1989        10.5
# 29  1990         9.6
# 30  1991        24.9
# 31  1992         4.3
# 32  1993         5.2
# 33  1994         8.9
# 34  1995         5.3
# 35  1996         4.1
# 36  1997         6.8
# 37  1998         7.9
# 38  1999         8.4
# 39  2000         7.4
# 40  2001         3.8
# 41  2002         3.8
# 42  2003         6.6
# 43  2004        19.6
# 44  2005         8.8
# 45  2006        13.8
# 46  2007        15.2
# 47  2008         7  
# 48  2009         9.2
# 49  2010        14.2


maxRainByHour <- station_df %>% group_by(hour) %>% summarise(maxRainfall = max(Value))
print(maxRainByHour, n = Inf)
# # A tibble: 24 x 2
# hour maxRainfall
# <int>       <dbl>
#   1     0        32  
# 2     1         6.4
# 3     2         2.8
# 4     3         5.4
# 5     4         5.1
# 6     5         4.4
# 7     6         6  
# 8     7        10.5
# 9     8         4.8
# 10     9         6.4
# 11    10         5.5
# 12    11         6.6
# 13    12         7.3
# 14    13        19.6
# 15    14        13.8
# 16    15        24.9
# 17    16        15  
# 18    17        11.9
# 19    18        15.2
# 20    19         8.4
# 21    20        12.3
# 22    21         9.6
# 23    22         7.2
# 24    23         6.9

# This shows that there was s usupiciously high value for the 0th hour (midnight) in 1978... Check this:
station_df %>% filter(Value == 32.)
# # A tibble: 1 x 9
# Station Datestamp           Value  Flag Decision  year month   day  hour
# <chr>   <dttm>              <dbl> <dbl> <chr>    <dbl> <dbl> <int> <int>
#   1 061211  1978-06-01 00:05:00    32     0 NA        1978     6     1     0

# Take a look at this day and the day before:
station_df %>% filter(year == 1978, month == 06, day == 01)
# okay, that's odd... Lots of -8888 values in the data around this date! Looks like removing flagged data isn't enough...

station_df %>% filter(Value > 5) %>% group_by(hour) %>% summarise(n())


# Remove this 32mm 5-min event:
station_df[station_df$Value == 32] <- NA


# End of the day before:
station_df %>% filter(year == 1978, month == 05, day == 31) %>% tail()

# How many -8888s? Or other negative values?
station_df %>% filter(Value < 0) %>% group_by(Value) %>% summarise(n())
# # A tibble: 98 x 2
# Value  `n()`
# <dbl>  <int>
#   1 -9999   394885
# 2 -8888   179919
# 3  -170        1
# 4   -82        1
# 5   -62.2      1
# 6   -54.6      1
# 7   -53.8      1
# 8   -52.6      1
# 9   -51.5      1
# 10   -48.2      1
# 11   -46        1
# 12   -45.2      1
# 13   -38        1
# 14   -37.4      1
# 15   -37        1
# 16   -35.5      1
# 17   -35.2      1
# 18   -31.7      1
# 19   -29.4      1
# 20   -28.7      1
# 21   -28        1
# 22   -26.5      1
# 23   -24        1
# 24   -23        1
# 25   -22.8      1
# 26   -21        1
# 27   -20.5      1
# 28   -20        2
# 29   -19.4      1
# 30   -18.9      1
# 31   -17.6      1
# 32   -17.5      2
# 33   -14.6      1
# 34   -13.4      1
# 35   -13        1
# 36   -11.6      2
# 37   -11.4      2
# 38   -10.4      1
# 39    -9.8      1
# 40    -9.6      2
# 41    -9.4      1
# 42    -9        1
# 43    -8.8      2
# 44    -8.3      1
# 45    -8.2      1
# 46    -8        1
# 47    -7.8      2
# 48    -7.7      1
# 49    -7.3      2
# 50    -7        1
# 51    -6.8      1
# 52    -6.6      2
# 53    -6.4      1
# 54    -6.3      1
# 55    -6.2      2
# 56    -6.1      3
# 57    -6        2
# 58    -5.5      1
# 59    -5.3      1
# 60    -5.2      1
# 61    -5.1      1
# 62    -5        2
# 63    -4.8      2
# 64    -4.6      1
# 65    -4.5      2
# 66    -4.1      1
# 67    -4        7
# 68    -3.9      1
# 69    -3.8      1
# 70    -3.5      3
# 71    -3.4      2
# 72    -3.3      1
# 73    -3.2      2
# 74    -3        3
# 75    -2.7      4
# 76    -2.6      1
# 77    -2.5      4
# 78    -2.4      1
# 79    -2.2      5
# 80    -2        7
# 81    -1.8      5
# 82    -1.7     12
# 83    -1.6      3
# 84    -1.5     13
# 85    -1.4     10
# 86    -1.3      2
# 87    -1.2     22
# 88    -1.1      1
# 89    -1       55
# 90    -0.9      2
# 91    -0.8     18
# 92    -0.7     46
# 93    -0.6     33
# 94    -0.5     96
# 95    -0.4     57
# 96    -0.3     28
# 97    -0.2    100
# 98    -0.1     12

# Okay! That's strange. Let's remove all the negative values from our data set:
station_df_clean <- station_df
station_df_clean[station_df_clean < 0] <- NA

# Now try the histogram again:
station_df_clean %>% filter(Value > 0) %>% 
  ggplot(aes(x = Value)) + geom_histogram(binwidth = 0.1, na.rm = TRUE)
# This has a veeeeery long tail!

# Plot up only points up to 5mm:
station_df_clean %>% filter(Value > 0) %>% 
  ggplot(aes(x = Value)) + geom_histogram(binwidth = 0.1, na.rm = TRUE) +
  coord_cartesian(xlim = c(0, 5))
# Interesting result - dwfinitely a bias towards values ending with even numbers. 
# Perhaps a change in the collection mechanism / gauge? (Tipping buckets use 0.2 mm units)

# Remove values above 5mm, retaining them in their own df first:
over_5mm <- station_df %>% filter(Value > 5) 
over_5mm %>% ggplot(aes(x = Value)) + geom_histogram(binwidth = 1)

over_15mm <- station_df %>% filter(Value > 15) 
over_15mm %>% ggplot(aes(x = Value)) + geom_histogram(binwidth = 1)

# When are the extreme values?
station_df %>% filter(Value > 12) %>% arrange(desc(Value))
# # A tibble: 10 x 9
# Station Datestamp           Value  Flag Decision  year month   day  hour
# <chr>   <dttm>              <dbl> <dbl> <chr>    <dbl> <dbl> <int> <int>
# 1 061211  1978-06-01 00:05:00  32       0 NA        1978     6     1     0 # REMOVE
# 2 061211  1991-12-22 15:10:00  24.9     0 NA        1991    12    22    15 # REMOVE
# 3 061211  2004-12-24 13:40:00  19.6     0 NA        2004    12    24    13 # OK
# 4 061211  2007-02-24 18:20:00  15.2     0 NA        2007     2    24    18 # OK
# 5 061211  1982-02-26 16:05:00  15       0 NA        1982     2    26    16 # OK
# 6 061211  2010-01-28 16:30:00  14.2     0 NA        2010     1    28    16 # OK
# 7 061211  2006-02-16 14:40:00  13.8     0 NA        2006     2    16    14 # OK
# 8 061211  2006-02-16 14:45:00  12.6     0 NA        2006     2    16    14 # OK
# 9 061211  1974-10-31 16:20:00  12.5    NA NA        1974    10    31    16 # OK
#10 061211  1972-02-14 20:45:00  12.3     0 NA        1972     2    14    20 # OK

# Already removed the 32mm value; now remove the 24.9 as well
station_df_clean[station_df_clean == 32] <- NA
station_df_clean[station_df_clean == 24.9] <- NA
station_df_clean %>% filter(Value > 12) %>% arrange(desc(Value))
# # A tibble: 7 x 9
# Station Datestamp           Value  Flag Decision  year month   day  hour
# <chr>   <dttm>              <dbl> <dbl> <chr>    <dbl> <dbl> <int> <int>
#   1 061211  2004-12-24 13:40:00  19.6     0 NA        2004    12    24    13
# 2 061211  2007-02-24 18:20:00  15.2     0 NA        2007     2    24    18
# 3 061211  1982-02-26 16:05:00  15       0 NA        1982     2    26    16
# 4 061211  2006-02-16 14:40:00  13.8     0 NA        2006     2    16    14
# 5 061211  2006-02-16 14:45:00  12.6     0 NA        2006     2    16    14
# 6 061211  1974-10-31 16:20:00  12.5    NA NA        1974    10    31    16
# 7 061211  1972-02-14 20:45:00  12.3     0 NA        1972     2    14    20



# Remove first and last year as they are incomplete
#(inserted this up above)

# Okay! Now the QC has been done, get some stats:
# station_df_clean %>% 
#   group_by(year, month) %>% 
#   mutate(monmax = max(Value, na.rm = TRUE)) %>%
#   ggplot(aes(x = Datestamp, y = monmax)) +
#   geom_point()
# THIS TAKES WAY TOO LONG - BREAK IT DOWN
monmax <- station_df_clean %>%
  group_by(year, month) %>%
  summarise(monmax = max(Value, na.rm = TRUE))


monmax <- mutate(Date = floor_date(ymd(paste)))
