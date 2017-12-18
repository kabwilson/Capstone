# Averaging over every incident including the zeros
# If i wanted to make sure I was including all of the relavent data, I would need to include
# the filter twice and then combine the datasets after summarising each of the factors
NPI_summary <- ny_df %>% 
  filter(borough != "", number_of_persons_injured > 0) %>% 
  group_by(borough) %>% 
  summarise(avgInjury = mean(number_of_persons_injured, is.na = TRUE))

NPK_summary <- ny_df %>% 
  filter(borough != "", number_of_persons_killed > 0) %>% 
  group_by(borough) %>% 
  summarise(avgFatality = mean(number_of_persons_killed, is.na = TRUE))

Borough_summary <- full_join(NPI_summary, NPK_summary, by = "borough")

# General summary by month and year
# Do I need to do the same thing as the above summary table? If so, how do I join them?
NPI_bydate <- ny_df %>% 
  filter(borough != "", number_of_persons_injured > 0) %>% 
  mutate(year = year(date_time)) %>% 
  group_by(year, month, borough) %>% 
  summarise(avgTotalInjury = mean(number_of_persons_injured, is.na = TRUE))

NPK_bydate <- ny_df %>% 
  filter(borough != "", number_of_persons_killed > 0) %>% 
  mutate(year = year(date_time)) %>% 
  group_by(year, month, borough) %>% 
  summarise(avgTotalKilled = mean(number_of_persons_killed, is.na = TRUE))

# Create Unique Identifiers for each summary table
NPI_bydate$Unique <- paste(NPI_bydate$year, NPI_bydate$month,
                           NPI_bydate$borough, sep = "")
NPK_bydate$Unique <- paste(NPK_bydate$year, NPK_bydate$month,
                           NPK_bydate$borough, sep = "")
# Join them into a total month summary table
mmyyyy_summary <- full_join(NPI_bydate, NPK_bydate, by = "Unique")

# Want to do the same sort of summary table with the contributing factors - can exclude the Unspecified category
# Could also do one with the vehicle type
# Do similar ones using day of week and time of day for all these also in place of the year/month
# Correlations between vehicles - first and second vehicle types
# using a chi-squared test chisq.test

# Would just need to note that the "Unspecified" contributing factor was not included
ny_df %>% 
  filter(borough != "", contributing_factor_vehicle_1 != "Unspecified") %>% 
  group_by(borough, contributing_factor_vehicle_1) %>% 
  summarise(number = n(),
            avgTotalInjury = sum(number_of_persons_injured, is.na = TRUE),
            avgTotalFatality = sum(number_of_persons_killed, is.na = TRUE)) %>% 
  arrange(desc(number))
# Paredo chart - basically a summary of this table in graphical form
# Heinrick's pyramid - accidents in steel founders; there's a pyramid shape to potential injuries
# This would be interesting to compare to his theory to see if they increase in that sort of a format
# Does the number of injuries predict the potential number of fatalities?
# Interesting from a city planner point of view becuase it's either two separate problems
# or one problem (if they correlate)
# If you sort by Fatalities, there's a different highest contributing factor than if you sort
# by just the number of incidents or the average injuries
# ggplot - geompoint x=injury, y=fatality and facet by borough


# Move this into an R Markdown file to give it some structure
# can also use comments with 4 or more # signs in order to use the level separators in R