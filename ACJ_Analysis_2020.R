library(tidyr)
library(stats)
library(dplyr)
library(lubridate)

### This file will sort through Allegheny County Jail (ACJ) population data for 2020 and aggregate demographic variables for analysis over time

# Read in the data from the .csv file
# The .csv was compiled from Allegheny County Jail daily census data available from the Western Pennsylvania Regional Data Center
ACJ_pop <- read.csv("ACJ_pop.csv")

# Confirm that there are no NAs among the race variable, and remove any if present
# sum(is.na(ACJ_pop$age_at_census))
# ACJ_pop <- ACJ_pop %>% drop_na("race")

# create "Other" race category for inmates whose race is not listed as black (B) or white (W)
ACJ_pop$race[ACJ_pop$race != "B" & ACJ_pop$race !="W"] <- "Other"

# Also create an age category column for 50+
# Initiate a column for age category
ACJ_pop$ageCategory <- NA
ACJ_pop$ageCategory[ACJ_pop$age_at_census >= 40] <- "40+ years old"
ACJ_pop$ageCategory[ACJ_pop$age_at_census < 40] <- "younger than 40 years old"

# Aggregate data to daily totals by intersected variables of interest: gender, race, and age
population_daily <- ACJ_pop[,c(3,4,6,7)] %>%
  drop_na("ageCategory") %>%
  group_by(gender, race, census_date, ageCategory) %>%
  # create a new column called daily_count and sum the number of people by gender & race for each day
  summarise(daily_count = length(race))

# change the date format
population_daily$census_date <- as.Date(population_daily$census_date)

# create a new data frame for monthly averages with a similar structure
monthly_average <- population_daily
# Initiate month column
monthly_average$month <- month(monthly_average$census_date)
monthly_average <- monthly_average %>%
  group_by(gender, race, month, ageCategory) %>%
  summarise(average_daily_count = mean(daily_count))

# # Export data if desired
# write.csv(population_daily,"dailyACJ_RaceAndAge.csv", row.names = FALSE)
# write.csv(monthly_average,"monthlyACJ_RaceAndAge.csv", row.names = FALSE)
