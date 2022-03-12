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
lint("../source/trend_data.R")

# Trend over time
 black_prison <- dataset %>%
  select(year, black_prison_pop) %>%
   filter(year > 1985) %>%
   filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    Black = sum(black_prison_pop, na.rm = TRUE)
  )
 
asian_prison <- dataset %>%
  select(year, aapi_prison_pop) %>%
  filter(year > 1985) %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    Asian = sum(aapi_prison_pop, na.rm = TRUE)
  )

latinx_prison <- dataset %>%
  select(year, latinx_prison_pop) %>%
  filter(year > 1985) %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    Latinx = sum(latinx_prison_pop, na.rm = TRUE)
  )

native_prison <- dataset %>%
  select(year, native_prison_pop) %>%
  filter(year > 1985) %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    Native = sum(native_prison_pop, na.rm = TRUE)
  )

white_prison <- dataset %>%
  select(year, white_prison_pop) %>%
  filter(year > 1985) %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    White = sum(white_prison_pop, na.rm = TRUE)
  )

other_prison <- dataset %>%
  select(year, other_race_prison_pop) %>%
  group_by(year) %>%
  filter(year > 1985) %>%
  filter(year < 2017) %>%
  summarize(
    `Other Race` = sum(other_race_prison_pop, na.rm = TRUE)
  )

combined_data <- left_join(black_prison, asian_prison, by = "year")
combined_data <- left_join(combined_data, latinx_prison, by = "year")
combined_data <- left_join(combined_data, native_prison, by = "year")
combined_data <- left_join(combined_data, white_prison, by = "year")
combined_data <- left_join(combined_data, other_prison, by = "year")


chart_data <- combined_data %>%
  gather(key = compare,
         value = population,
         Black,
         Asian,
         Latinx,
         Native,
         White,
         `Other Race`)

trend_data <- plot_ly(
  data = chart_data,
  x = ~year,
  y = ~population,
  color = ~compare,
  type = "scatter",
  mode = "lines"
) %>% layout(
  title = "Prison Population by race from 1986 to 2016",
  xaxis = list(title = "Year"),
  yaxis = list(title = "Prison population"),
  legend = list(title = list(text = "<b> Demographic </b>"))
)
trend_data
