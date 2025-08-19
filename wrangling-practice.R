# clearing environment
rm(list=ls())

# attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) # help us work with dates

# data wrangling refresher
# 1. only include penguins at Briscoe and Dream Islands
# 2 remove the year and sex variables
# 3 add a new column called body_mass_kg with penguin mass converted from grams to kg
# 4 rename island variable to location

penguins |>
  filter(island %in% c("Briscoe", "Dream")) |>
  select(!c(year, sex)) |>
  mutate("body_mass_kg" = body_mass_g / 1000) |>
  rename(location = island)

# limit to Adelie penguins
# remove any observations where flipper_length_mm is NA
# group the data by sex
#find the mean, sd, and sample size n() of flipper lengths for male and females
penguins |>
  filter(species == "Adelie") |>
  filter(!is.na(flipper_length_mm),
         !is.na(sex)) |> # remove rows that are not na
  group_by(sex) |>
  summarise(mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n())

# practice with joins
animals <- data.frame(
  stringsAsFactors = FALSE,
  location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
  species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
  maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

# tables can only be joined one at a time
# practice with full_join
# keeps all rows and adds all columns
full_join(animals, sites)

#left_join()
left_join(animals, sites)

#right_join()
right_join(animals, sites)

# inner_join()
inner_join(animals, sites)

#filtering joins
semi_join(animals, sites)

animals |>
  filter(location %in% sites$location)

anti_join(animals, sites)

animals |>
  filter(!location %in% sites$location)

anti_join(sites, animals)

# practice with lubridate
# lubridate
my_date <- "03-13-1998"
lubridate::mdy(my_date) #fixed date to ISO 1806

# new format for date
my_date <- "08-Jun-1974"
lubridate::dmy(my_date)

# another format
my_date <- "19160518"
lubridate::ymd(my_date)


# what if we give lubridate a data that doesnt make sense?
lubridate:mdy("1942-08-30")

lubridate::dmy("09/12/84")

# working with date-times

time <- "2020-08-12 11:18"
time <- ymd_hm(time, tz = "America/Los_Angeles") # to add time zone

# convert to PDT
with_tz(time, "America/Los_Angeles")

# extract info from dates
week(time)
year(time)
day(time)

# can get time of the computer
start_time <- Sys.time()

end_time <- Sys.time()

# tells how long the script takes to run
end_time - start_time

# practice lubridate within a data frame
urchin_counts <- tribble(
  ~date, ~speceies, ~size_mm,
  "10/3/2020", "purple", 55,
  "10/4/2020", "red", 48,
  "11/17/2020", "red", 67
)

urchin_counts |>
  mutate(date = lubridate::mdy(date)) |>
  mutate(year = year(date),
         month = month(date),
         day = day(date))

day_1 <- lubridate::ymd("2020-01-06")
day_2 <- ymd("2020-05-18")
day_3 <- ymd("2020-05-19")

#create time interval
time_interval <- interval(day_1, day_2)
time_length(time_interval, "week")
time_length(time_interval, "year")

# practice with stringr

# str_detect() to detect string patterns
# returns TRUE/FALSE depending on whether the pattern is detected
my_string <- "Teddy loves eating salmon and socks."

# does the pattern "love" exist within the string?
my_string |>
  str_detect("love")

my_string <- c("burrito", "fish taco", "taco salad")
my_string |>
  str_detect("fish")

# powerful when combined with dplyr functions
starwars |>
  filter(str_detect(name, "Skywalker"))

firewalkers <- starwars |>
  mutate(name = str_replace(name, pattern = "Sky", replacement = "Fire"))

# cleaning up white space
feedback <- c("I ate  some  nachos", "Wednesday morning.    ")

# remove the leading, trailing, and duplicate spaces
str_squish(feedback)

# remove just leading and trailing spaces
str_trim(feedback)

# convert cases
str_to_lower(feedback)
str_to_upper(feedback)
str_to_sentence(feedback)
str_to_title(feedback)

#count mateches in a string
str_count(feedback, pattern = "nachos")
