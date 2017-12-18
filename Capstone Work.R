# Capstone Project
## Katie Wilson

#### Load libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(rgdal)
library(maptools)
library(ggmap)
library(maps)
library(jsonlite)
library(lubridate)

# Load the two json files from the NYC DOT website
json_uri <- "http://www.nyc.gov/html/dot/downloads/misc/fatality_all_monthly.json"
ny_fatalities_js <- fromJSON(txt = json_uri, flatten = TRUE)
ny_fatalities_df <- ny_fatalities_js$features

json_uri2 <- "http://www.nyc.gov/html/dot/downloads/misc/injury_all_monthly.json"
ny_injuries_js <- fromJSON(txt = json_uri2, flatten = TRUE)
ny_injuries_df <- ny_injuries_js$features

## ny_fatalities_df now contains a column that is itself a list. This doesn't
## work for data frames, so we have to extract the list as columns in the data frame.
## Unfortunately, the list works like a column for every observation of fatalities, and each
## column has two rows, the latitude and longitude. We need to extract the first row
## of every list element to a latitude vector, and the second row of every element to
## a longitude vector.
x <- rep(NA, times = length(ny_fatalities_df$geometry.coordinates))
y <- rep(NA, times = length(ny_fatalities_df$geometry.coordinates))
for(i in 1:length(ny_fatalities_df$geometry.coordinates)) {
  x[i] <- ny_fatalities_df$geometry.coordinates[[i]][1]
  y[i] <- ny_fatalities_df$geometry.coordinates[[i]][2]
}

z <- rep(NA, times = length(ny_injuries_df$geometry.coordinates))
m <- rep(NA, times = length(ny_injuries_df$geometry.coordinates))
for(i in 1:length(ny_injuries_df$geometry.coordinates)) {
  z[i] <- ny_injuries_df$geometry.coordinates[[i]][1]
  m[i] <- ny_injuries_df$geometry.coordinates[[i]][2]
}

## Now we save the coordinates back to the data frame, delete the list from the data
## frame since ggplot2 is likely to choke on it, and wrangle some other columns.
ny_fatalities_df <- ny_fatalities_df %>% 
  mutate(latitude = x,
         longitude = y,
         geometry.coordinates = NULL,
         properties.YR = as.integer(properties.YR),
         properties.MN = as.integer(properties.MN))

ny_injuries_df <- ny_injuries_df %>% 
  mutate(latitude = z,
         longitude = m,
         geometry.coordinates = NULL,
         properties.YR = as.integer(properties.YR),
         properties.MN = as.integer(properties.MN))

# add in a day column as a string
ny_fatalities_df$properties.Day <- 1L
ny_injuries_df$properties.Day <- 1L

# unite the columns, assign to "FullDate" column header, and write into the dataframe
ny_fatalities_df <- unite(ny_fatalities_df, "FullDate",
                               c("properties.YR", "properties.MN", "properties.Day"), sep = "-")
ny_injuries_df <- unite(ny_injuries_df, "FullDate",
                             c("properties.YR", "properties.MN", "properties.Day"), sep = "-")

# Assign this column as a date just using the base package
ny_fatalities_df$FullDate <- as.Date(ny_fatalities_df$FullDate, format = "%Y-%m-%d")
ny_injuries_df$FullDate <- as.Date(ny_injuries_df$FullDate, format = "%Y-%m-%d")

# Check for and remove any columns that do not have a date associated with them
sum(is.na(ny_fatalities_df))
ny_fatalities_df <- ny_fatalities_df[complete.cases(ny_fatalities_df$FullDate), ]
sum(is.na(ny_injuries_df))

# plot the data
ggplot(ny_fatalities_df, aes(x = FullDate, y = Fatalities)) +
  geom_point()
ggplot(ny_injuries_df, aes(x = FullDate, y = Injuries)) +
  geom_point()

# Create a unique identifier for each data point including the date and the location
# But are there any duplicates between the location and the date?
# There could have been more than one accident at a location within a month...
# Create the potentially unique identifier and check for any duplicates in both data sources
ny_fatalities_df$Identifier <- 
  paste(ny_fatalities_df$FullDate,
        ny_fatalities_df$latitude + ny_fatalities_df$longitude,
        sep = "")
length(unique(ny_fatalities_df$Identifier)) == nrow(ny_fatalities_df)
ny_injuries_df$Identifier <- 
  paste(ny_injuries_df$FullDate,
        ny_injuries_df$latitude + ny_injuries_df$longitude,
        sep = "")
length(unique(ny_injuries_df$Identifier)) == nrow(ny_injuries_df)
# Because both of those are true, we know there are no duplicates in the identifier
# confirming it is in fact unique

# Now we can do a full outer join on the two data frames on the Identifier
# Could give it a list of the three columns I want to join on
# 
nrow(ny_fatalities_df) + nrow(ny_injuries_df)
accident_monthly <- full_join(ny_fatalities_df, ny_injuries_df,
                              by = c("Identifier" = "Identifier"))
# To see the number of rows that crossed over between the two dataframes:
(nrow(ny_fatalities_df) + nrow(edit_injury_monthly)) - nrow(accident_monthly)

# Create 4 total columns including the totals of people affected within each Identifier
edit_accident_monthly <- accident_monthly
edit_accident_monthly$PeopleAffected <- rowSums(edit_accident_monthly
                                                [,c("properties.Fatalities",
                                                    "properties.Injuries")], na.rm = TRUE)
edit_accident_monthly$TotalPedestrian <- rowSums(edit_accident_monthly
                                                [,c("properties.PedFatalit",
                                                    "properties.PedInjurie")], na.rm = TRUE)
edit_accident_monthly$TotalBike <- rowSums(edit_accident_monthly
                                                [,c("properties.BikeFatali",
                                                    "properties.BikeInjuri")], na.rm = TRUE)
edit_accident_monthly$TotalMVO <- rowSums(edit_accident_monthly
                                                [,c("properties.MVOFatalit",
                                                    "properties.MVOInjurie")], na.rm = TRUE)

## Map the data
nyc_map <- get_map("new york", zoom = 10)
## scale factors that can be used in this get_map to make it smaller
ggmap(nyc_map) +
  geom_point(data = ny_fatalities_df %>% filter(properties.PedFatalit > 0, !is.na(properties.YR),
                                                !is.na(properties.MN)), 
             aes(x = latitude, y = longitude, colour = properties.PedFatalit),
             alpha = 0.5, size = 0.5)


## Work with the speed limit data
## Input all of the files from the DOT website
json_uri3 <- "http://www.nyc.gov/html/dot/downloads/misc/speed_limit_bronx.json"
speedlimits_Bronx_js <- fromJSON(txt = json_uri3, flatten = TRUE)
speedlimits_Bronx_df <- speedlimits_Bronx_js$features

json_uri4 <- "http://www.nyc.gov/html/dot/downloads/misc/speed_limit_brooklyn.json"
speedlimits_Brooklyn_js <- fromJSON(txt = json_uri4, flatten = TRUE)
speedlimits_Brooklyn_df <- speedlimits_Brooklyn_js$features

json_uri5 <- "http://www.nyc.gov/html/dot/downloads/misc/speed_limit_manhattan.json"
speedlimits_Manhattan_js <- fromJSON(txt = json_uri5, flatten = TRUE)
speedlimits_Manhattan_df <- speedlimits_Manhattan_js$features

json_uri6 <- "http://www.nyc.gov/html/dot/downloads/misc/speed_limit_queens.json"
speedlimits_Queens_js <- fromJSON(txt = json_uri6, flatten = TRUE)
speedlimits_Queens_df <- speedlimits_Queens_js$features

json_uri7 <- "http://www.nyc.gov/html/dot/downloads/misc/speed_limit_statenisland.json"
speedlimits_StatenIsland_js <- fromJSON(txt = json_uri7, flatten = TRUE)
speedlimits_StatenIsland_df <- speedlimits_StatenIsland_js$features

## write a function that will split the geometry.coordinates into four separate columns
geo_columns <- function(df) {
  a <- rep(NA, times = length(df$geometry.coordinates))
  b <- rep(NA, times = length(df$geometry.coordinates))
  c <- rep(NA, times = length(df$geometry.coordinates))
  d <- rep(NA, times = length(df$geometry.coordinates))
  for(i in 1:length(df$geometry.coordinates)) {
    a[i] <- df$geometry.coordinates[[i]][1]
    b[i] <- df$geometry.coordinates[[i]][2]
    c[i] <- df$geometry.coordinates[[i]][3]
    d[i] <- df$geometry.coordinates[[i]][4]
  }
  
  df <- df %>% 
    mutate(start_latitude = a,
           start_longitude = c,
           end_latitude = b,
           end_longitude = d,
           geometry.coordinates = NULL)
}

geo_columns(speedlimits_Bronx_df)

## Bind all five together for a complete NY Speed Limit df
speedlimits_NY <- bind_rows(speedlimits_Bronx_df, speedlimits_Brooklyn_df,
                            speedlimits_Manhattan_df, speedlimits_Queens_df,
                            speedlimits_StatenIsland_df)

a <- rep(NA, times = length(speedlimits_Bronx_df$geometry.coordinates))
b <- rep(NA, times = length(speedlimits_Bronx_df$geometry.coordinates))
c <- rep(NA, times = length(speedlimits_Bronx_df$geometry.coordinates))
d <- rep(NA, times = length(speedlimits_Bronx_df$geometry.coordinates))
for(i in 1:length(speedlimits_NY$geometry.coordinates)) {
  a[i] <- speedlimits_Bronx_df$geometry.coordinates[[i]][1]
  b[i] <- speedlimits_Bronx_df$geometry.coordinates[[i]][2]
  c[i] <- speedlimits_Bronx_df$geometry.coordinates[[i]][3]
  d[i] <- speedlimits_Bronx_df$geometry.coordinates[[i]][4]
}

speedlimits_NY <- speedlimits_NY %>% 
  mutate(start_latitude = a,
         start_longitude = c,
         end_latitude = b,
         end_longitude = d,
         geometry.coordinates = NULL)

#realized not all of the geometry.coordinates columns are two rows. some are up to 14

#read in the precipitation file from the NOAA
#https://www.ncdc.noaa.gov/cdo-web/
CentralPark <- read.csv('E:/Springboard/Datasets/Capstone/Central Park Precipitation.csv')

#read in the date column as an actual date using the lubridate package
CentralPark$Date <- parse_date_time(CentralPark$DATE, orders = "ymd HM")

#group total rainfall by month
total_precipitation <- CentralPark %>%
  mutate(Month = month(Date),
         Year = year(Date)) %>%
  group_by(Month, Year) %>%
  summarise(TotalRainfall = sum(HPCP))

#group each of the totals from accident monthly into the same table
accident_totals <- edit_accident_monthly %>% 
  mutate(Month = month(FullDate.x), Year = year(FullDate.x)) %>% 
  group_by(Month, Year) %>% 
  summarise(PeopleAffected = sum(PeopleAffected, na.rm = TRUE),
            TotalPedestrian = sum(TotalPedestrian, na.rm = TRUE),
            TotalBike = sum(TotalBike, na.rm = TRUE),
            TotalMVO = sum(TotalMVO, na.rm = TRUE),
            TotalFatalities = sum(properties.Fatalities, na.rm = TRUE),
            TotalInjuries = sum(properties.Injuries, na.rm = TRUE))

#create Month_Year column in order to be able to join the two tables
total_precipitation$MonthYear <- paste(total_precipitation$Month,
                                        total_precipitation$Year,
                                        sep = "")
accident_totals$MonthYear <- paste(accident_totals$Month,
                                    accident_totals$Year,
                                    sep = "")

#Join total_precipitation and accident_totals with an inner join
totals_by_month <- inner_join(total_precipitation, accident_totals, by = "MonthYear")

#create an actual date column in order to plot time series data
totals_by_month$Day <- 1L
totals_by_month$Date <- as.Date(paste(totals_by_month$Year.x,
                                      totals_by_month$Month.x,
                                      totals_by_month$Day,
                                      sep = "-"), format = "%Y-%m-%d")

ggplot(totals_by_month, aes(x = Date, y = TotalRainfall)) +
  geom_line()
ggplot(totals_by_month, aes(x = TotalRainfall, y = PeopleAffected)) +
  geom_point()
ggplot(CentralPark, aes(x = Date, y = HPCP)) +
  geom_line()
