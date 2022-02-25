#A3
#load the library 
library("tidyverse",warn.conflicts = FALSE)
library("plotly",warn.conflicts = FALSE)
library("leaflet",warn.conflicts = FALSE)  
library("ggplot2",warn.conflicts = FALSE)
#library(rjson)
# load the dataset
dataset <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",show_col_types = FALSE)
setwd("/Users/matty-so/Desktop/Info201code/a3-mattschcs/docs")
#Two variable graph

black_prison_pop <- dataset %>%
  select(year,black_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1989) %>%
  filter(year <2017)

Black_population <- dataset %>%
  select(year,black_pop_15to64 ) %>%
  group_by(year) %>%
  summarize(
    Black_population = sum(black_pop_15to64, na.rm = TRUE)
  ) %>%
  filter(year > 1989) %>%
  filter(year <2017)


Two_variable <- left_join(Black_population, black_prison_pop, by = "year")


Two_chart_data <- Two_variable %>% 
  gather(key = compare, value = population,  Black_population, black_prison_pop)

Two_chart <- plot_ly(
  data = Two_chart_data,     
  x = ~year, 
  y = ~population, 
  color = ~compare, 
  type = "scatter",
  mode   = 'markers'
) %>% layout(
  title = "African American population and African American population",
  xaxis = list(title = 'Year'), 
  yaxis = list(title = 'population') 
)

Two_chart 
