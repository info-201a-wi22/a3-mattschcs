#A3
#load the library 
library("tidyverse",warn.conflicts = FALSE)
library("plotly",warn.conflicts = FALSE)
library("leaflet",warn.conflicts = FALSE)  
library("ggplot2",warn.conflicts = FALSE)
#library(rjson)
# load the dataset
dataset <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",show_col_types = FALSE)

  
# Trend over time
  
 black_prison <- dataset %>%
  select(year,black_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

Asian_prison <- dataset %>%
  select(year,aapi_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    Asian_prison = sum(aapi_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

latinx_prison <- dataset %>%
  select(year,latinx_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    latinx_prison = sum(latinx_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

native_prison <- dataset %>%
  select(year,native_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    Native_prison = sum(native_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

White_prison <- dataset %>%
  select(year,white_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    White_prison = sum(white_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

Other_prison <- dataset %>%
  select(year,other_race_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    other_race_prison_pop = sum(other_race_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

total <- dataset %>%
  select(year,total_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)


combined_data <- left_join(total, black_prison, by = "year")
combined_data <- left_join(combined_data, Asian_prison, by = "year")
combined_data <- left_join(combined_data, latinx_prison, by = "year")
combined_data <- left_join(combined_data, native_prison, by = "year")
combined_data <- left_join(combined_data, White_prison, by = "year")
combined_data <- left_join(combined_data, Other_prison, by = "year")

chart_data <- combined_data %>% 
  gather(key = compare, value = population, total_prison_pop,black_prison_pop, Asian_prison, latinx_prison, Native_prison,White_prison, other_race_prison_pop)

Trend_data <- plot_ly(
  data = chart_data,     
  x = ~year, 
  y = ~population, 
  color = ~compare, 
  type = "scatter",
  mode = "lines"
) %>% layout(
  title = "Prison Population by race",
  xaxis = list(title = 'Year'), 
  yaxis = list(title = 'Prison population') 
)
Trend_data

