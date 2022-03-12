#A3
#load the library
library(lintr)
library("tidyverse", warn.conflicts = FALSE)
library("plotly", warn.conflicts = FALSE)
library("leaflet", warn.conflicts = FALSE)
library("ggplot2", warn.conflicts = FALSE)
# load the dataset
dataset <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", show_col_types = FALSE)
setwd("/Users/matty-so/Desktop/Info201code/a3-mattschcs/docs")
lint("../source/analysis.R")
name <- c("Ratio of black prision to prison population in 2010",
         "Jail average population 1970- 2018"
         , "Ratio of white prision to prison population in 2010",
         "Ratio of prison population to population in 2010",
         "The range in prison population from 1980 to 2016")

# 5 variables from the dataset

#1 The proportion of Black prison populations and
# the total prison population in 2010
black_prision_in_2010 <- dataset %>%
  select(year, black_prison_pop) %>%
  filter(year == 2010) %>%
  summarise(
    Black_prison_population = sum(black_prison_pop, na.rm = TRUE)
  ) %>%
  pull(Black_prison_population)


total_prision_in_2010 <- dataset %>%
  select(year, total_prison_pop, black_prison_pop) %>%
  filter(year == 2010) %>%
  summarise(
    total_sum = sum(total_prison_pop, na.rm = TRUE)
  ) %>%
  pull(total_sum)

black_prision_prop_in_2010 <- black_prision_in_2010 / total_prision_in_2010


#2 The average of total jail population 1970 - 2018
total_jail <- dataset %>%
  select(year, total_jail_pop, state) %>%
  group_by(year) %>%
  summarize(
    all_jail = sum(total_jail_pop, na.rm = TRUE)
  ) %>%
  summarize(
    all_jail_pop  = sum(all_jail, na.rm = TRUE)
  ) %>%
  pull(all_jail_pop)

  average <- total_jail / (2018 - 1970)
  
#3 The proportion of white prison populations and the total prison population in 2010
  
  white_pris_population_in_2010 <- dataset %>%
    select(year, white_prison_pop) %>%
    filter(year == 2010) %>%
    summarise(
      White_prison_population = sum(white_prison_pop, na.rm = TRUE)) %>%
    pull(White_prison_population)
  
  proportion_of_white <-
    white_pris_population_in_2010 /
    total_prision_in_2010

#4 The proportion of the total prison and the total population in 2010
  
  
  
  total_population_in2010 <- dataset %>%
    select(year, total_prison_pop, total_pop_15to64) %>%
    filter(year == 2010) %>%
    group_by(year) %>%
    summarize(
      total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE)
    ) %>%
    summarize(
      all_population = sum(total_pop_15to64, na.rm = TRUE)
    ) %>%
    pull(all_population)
  
  proportion_pop_pri <- total_prision_in_2010 / total_population_in2010
  
#5 Variable change the 1980 year and the 2016 year

  
  data_in_1980 <- dataset %>%
  select(year, state, total_prison_pop, state) %>%
    filter(year == 1980) %>%
  group_by(year) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
  pull(total_prison_pop)
  
  data_in_2016 <- dataset %>%
    select(year, state, total_prison_pop, state) %>%
    filter(year == 2016) %>%
    group_by(year) %>%
    summarize(
      total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
    pull(total_prison_pop)
  
  range <- data_in_2016 - data_in_1980
  
  
  
  variable <- c(black_prision_prop_in_2010,
                average, proportion_of_white,
                proportion_pop_pri, range)
  
  summary_info <- data.frame(name, variable)