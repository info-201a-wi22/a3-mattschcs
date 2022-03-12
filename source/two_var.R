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
#Two variable graph
lint("../source/two_var.R")

black_prison_prop <- dataset %>%
  select(year, black_prison_pop_rate) %>%
  filter(year > 1989) %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    Black = sum(black_prison_pop_rate, na.rm = TRUE)
  )

white_prison_prop <- dataset %>%
  select(year, white_prison_pop_rate) %>%
  filter(year > 1989) %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarize(
    White = sum(white_prison_pop_rate, na.rm = TRUE)
  )

two_variable <- left_join(white_prison_prop, black_prison_prop, by = "year")

two_chart_data <- two_variable %>%
  gather(key = compare, value = population,  Black, White)

two_chart <- plot_ly(
  data = two_chart_data,
  x = ~year,
  y = ~population,
  color = ~compare,
  type = "scatter",
  mode   = "markers"
) %>% layout(
  title = "Prison Rates between White American and African American",
  xaxis = list(title = "Year"),
  yaxis = list(title = "population"),
  legend = list(title = list(text = "<b> Type of Population </b>"))
)
two_chart
