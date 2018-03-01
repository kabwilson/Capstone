accident_detail <- read.csv('accident_detail.csv')
library(dplyr)
library(RSocrata)
library(lubridate)
library(ggplot2)
library(ggmap)
library(scales)
ny_uri <- "https://data.cityofnewyork.us/resource/qiz3-axqb.csv"
### API KEY ###
ny_api <- validateUrl(ny_uri, "N2gcj5lI3SeIYCzuSMIGrZldx") ### API KEY ###
ny_df <- read.socrata(ny_api) %>% 
  as_data_frame()
saveRDS(object = ny_df, file = "data-raw/nypd_motor_vehicle_collisions.rds")
ny_df <- readRDS("data-raw/nypd_motor_vehicle_collisions.rds")

# Use lubridate to combine the date/time field
# Fix any of the zeros in the latitude/longitude fields to just read N/A

ny_df$date_time <- paste(ny_df$date, ny_df$time, sep = " ")
ny_df$date_time <- as.Date(ny_df$date_time, format = "%Y-%m-%d %H:%M")

ny_df$latitude[ny_df$latitude == 0] <- NA
ny_df$longitude[ny_df$longitude == 0] <- NA
ny_df$month <- month(ny_df$date_time)
summary(month(ny_df$date_time))
# also remove all blanks in the boroughs column
ny_df$borough[ny_df$borough == ""] <- NA

ny_df$date <- as.Date(ny_df$date, format = "%Y-%m-%d")
ggplot(data = ny_df, aes(x = date, y = number_of_persons_injured)) +
  stat_summary(fun.y = sum, 
               geom = "bar") + 
  scale_x_date(labels = date_format("%Y-%m"),
    date_breaks = "1 year")

ggplot(data = ny_df, aes(x = date, y = number_of_persons_killed)) +
  stat_summary(fun.y = sum, 
               geom = "bar") + 
  scale_x_date(labels = date_format("%Y-%m"),
    date_breaks = "1 year")

# Is this variation due to random chance or is there something else going on in the background?
# Does this data violate any of the assumptions of a homogeneous process
# Should see something like a binomial distrubution
# Could use control charts using the QCC package
# Would want to take out the seasonality ts and stl functions in the stats package to eliminate the seasonality
# You would be left with the residual or remainder data
# can test it directly for the seasonality
# the ts and stl will show it graphically, but the acf function will check for autocorrelation
# it will return significant peaks if there are any forms of lags in the data

ny_df %>% filter(date < as.Date('2017-10-01'), borough == "BRONX") %>% 
  ggplot(aes(month(date, label = TRUE, abbr = TRUE), number_of_persons_injured,
               group = factor(year(date)), colour = factor(year(date)))) +
  stat_summary(fun.y = sum, geom = "line")

# plot the average of accidents in a bar graph with lines over top showing each year
# Specifically for the Bronx because of the lack of change in February's data compared to the other boroughs
ny_df %>% filter(date < as.Date('2017-10-01')) %>% 
  ggplot(aes(month(date, label = TRUE, abbr = TRUE), number_of_persons_injured,
             group = factor(year(date)), colour = factor(year(date)))) +
  stat_summary(fun.y = sum, geom = "line")

ny_df %>% filter(date < as.Date('2017-10-01'), number_of_persons_injured > 0) %>% 
  ggplot(aes(month(date, label = TRUE, abbr = TRUE), number_of_persons_injured,
             group = factor(year(date)), colour = factor(year(date)))) +
  stat_summary(fun.y = mean, geom = "line")
# this shows the average injury per incident each month

# would probably want to create a summary data set that would have the summary statistics in it
# specifically for the average by borough or average by year

# could also use multiple lines on top of each other with the average in a different color so it stands out
# Find average monthly temperature for each year and see if it tracks with the differences in accidents

# Plot number of persons injured by year and facet it by borough
ggplot(ny_df, aes(month(date, label = TRUE, abbr = TRUE), number_of_persons_injured,
                  group = factor(year(date)), fill = borough))+
         stat_summary(fun.y = sum, geom = "bar") +
  facet_grid(borough ~ ., scales = "free_y")

# SUMMARY DATASETS
# Group by borough and group by month
# Create a field for year, month, then borough
# groupby does a group by order and then summarize sort of peels off layers of the group
# summarize does last group first
# need to do some research groupby(borough, year, month) or groupby(borough) %>% groupby(year), etc.
# groupby and summarize work like cross tabs in excel
# think of this like a pivot table
# in the summarize function, you can grab the first of a column

# look at the time of day as well - see if there's a high time for these accidents by borough
# use lubridate to pull out the hour and plot it by hour and by borough
# Convert the time column to a time and pull out the hour
ny_df$time <- hm(ny_df$time)
ny_df$hour <- hour(ny_df$time)

ny_df %>% filter(date < as.Date('2017-10-01')) %>% 
  ggplot(aes(hour, number_of_persons_injured)) +
  stat_summary(fun.y = sum, geom = "bar") +
  facet_grid(. ~ borough)

# this peaks before and after work for every other borough except Manhattan
# this could be due to the fact that the other boroughs have commuters going to and from work
# whereas everyone in Manhattan is out and about at all times of the day

ny_df %>% filter(date < as.Date('2017-10-01')) %>% 
  ggplot(aes(hour, number_of_persons_killed)) +
  stat_summary(fun.y = sum, geom = "line") +
  geom_smooth(method = "loess") +
  facet_grid(. ~ borough)
# Not working right now - need to rerun this using the summary data

sum(ny_df$number_of_persons_killed)
sum(ny_df$number_of_persons_injured)
# an option with facet.grid - scales = free
# the default in facet_grid is scales = "fixed" uses the underlying belief
# that when you plot data on one graph, you want to compare it apples to apples
# by changing it to scales = "free", you're allowing it to plot the same data along different axes parameters

ggplot(ny_df, aes(month(date, label = TRUE, abbr = TRUE), number_of_persons_killed,
                  group = factor(year(date)), fill = borough))+
  stat_summary(fun.y = sum, geom = "bar") +
  facet_grid(. ~ borough)

table(ny_df$borough)
table(ny_df$zip_code)
plot(table(ny_df$contributing_factor_vehicle_1))
# create a table that includes this data - use one column with the count and one column with the description
# Use coord_flip to plot it with the description on the y-axis and the count on the x
# This will help identify the top types of contributing factors to narrow the observations
table(ny_df$vehicle_type_code1)
# do the same thing as above with the vehicle types

# There's about 58 different types of contributing factors
## Map the data
nyc_map <- get_map("new york", zoom = 10, scale = 1)
## scale factors that can be used in this get_map to make it smaller
ny_summary <- ny_df %>% filter(number_of_pedestrians_killed > 0, 
                               !is.na(longitude),
                               !is.na(latitude)) %>% 
  group_by(latitude, longitude) %>% 
  summarize(number_of_pedestrians_killed = sum(number_of_pedestrians_killed))


ggmap(nyc_map) + 
  geom_point(data = ny_summary, 
             aes(x = longitude, y = latitude,
                 colour = number_of_pedestrians_killed),
             alpha = 0.5, size = 0.8)

ggplot(ny_summary, aes(x = longitude, y = latitude,
                       colour = number_of_pedestrians_killed)) +
  geom_point(alpha = 0.5, size = 0.5)


# .exists - use an if/else statement to see if the file exists and choose where to read it from






stl(ny_df, s.window = "periodic", na.action = na.exclude)
#takes a time series object (ts) have to pass the part of the dataframe you want to use
#and pass it through ts() and then take those results and put them into the stl() function
injury_ts <- ts(ny_df$number_of_persons_injured, frequency = 365, start = c(2012,7))
injury_stl <- stl(injury_ts, s.window = "periodic")
plot(injury_stl)
#if local data file exists (file.exists()) then load the data file with readRDS() else run
#the code to get the data from the api and once you get the data and do the wrangling
#you'll save it through saveRDS unless you use the readr package and then it's writeRDS
#the readr function is a little faster
fatality_ts <- ts(ny_df$number_of_persons_killed, frequency = 365, start = c(2012,7))
fatality_stl <- stl(fatality_ts, s.window = "periodic")
plot(fatality_stl)
