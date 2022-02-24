#A3
#load the library 
library("tidyverse")
library("plotly")
library("leaflet")  
library(ggplot2)
# load the dataset
dataset <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Trend over time
afician_American_in_prision <- dataset %>%
  select(year,black_jail_pop ) %>%
  group_by(year) %>%
  summarize(
    African_prision = sum(black_jail_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

Asian <- dataset %>%
  select(year,aapi_jail_pop ) %>%
  group_by(year) %>%
  summarize(
    Asian = sum(aapi_jail_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

Latinx <- dataset %>%
  select(year,latinx_jail_pop ) %>%
  group_by(year) %>%
  summarize(
    Latinx = sum(latinx_jail_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

Native <- dataset %>%
  select(year,native_jail_pop ) %>%
  group_by(year) %>%
  summarize(
    Native = sum(native_jail_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

White <- dataset %>%
  select(year,white_jail_pop ) %>%
  group_by(year) %>%
  summarize(
    White = sum(white_jail_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

Other <- dataset %>%
  select(year,other_race_jail_pop ) %>%
  group_by(year) %>%
  summarize(
    other = sum(other_race_jail_pop, na.rm = TRUE)
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


combined_data <- left_join(total, afician_American_in_prision, by = "year")
combined_data <- left_join(combined_data, Asian, by = "year")
combined_data <- left_join(combined_data, Latinx, by = "year")
combined_data <- left_join(combined_data, Native, by = "year")
combined_data <- left_join(combined_data, White, by = "year")
combined_data <- left_join(combined_data, Other, by = "year")

chart_data <- combined_data %>% 
  gather(key = compare, value = population, total_prison_pop,African_prision, Asian, Latinx, Native,White, other)

plot_ly(
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

#Two variable graph

White_population <- dataset %>%
  select(year,white_pop_15to64 ) %>%
  group_by(year) %>%
  summarize(
    White_population = sum(white_pop_15to64, na.rm = TRUE)
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

year <- dataset %>% 
  group_by(year) %>%
  summarize(
    prison_population = sum(total_jail_pop, na.rm = TRUE)
  )
Two_variable <- left_join(Black_population, White_population, by = "year")

Two_chart_data <- Two_variable %>% 
  gather(key = compare, value = population, Black_population,  White_population)

plot_ly(
  data = Two_chart_data,     
  x = ~year, 
  y = ~population, 
  color = ~compare, 
  type = "scatter"
) %>% layout(
  title = "African American and White American population",
  xaxis = list(title = 'Year'), 
  yaxis = list(title = 'population') 
)
#map char that compare the ratio of total prison to that state 

# state_prison <- dataset %>%
#   select(year, state, black_jail_pop,aapi_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop,other_race_jail_pop, total_jail_pop ) %>%
#   filter(year > 2000) %>%
#   group_by(state) %>%
#   summarize(
#     black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
#     aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
#     latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
#     native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
#     white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
#     other_race_jail_pop = sum(other_race_jail_pop, na.rm = TRUE),
#     total_jail_pop = sum(total_jail_pop, na.rm = TRUE)
#   )
# 
# state_prison_data <- state_prison %>% 
#   gather(key = compare, value = population, black_jail_pop, aapi_jail_pop,latinx_jail_pop,native_jail_pop,white_jail_pop,other_race_jail_pop,total_jail_pop )
# 
# US_map <- map_data("state")
# ggplot(US_map) +
#   geom_polygon(
#     mapping = aes(x = long, y = lat, group = group),
#     color = "white", 
#     size = .1        
#   ) +
#   coord_map()

total <- dataset %>%
  select(year,total_prison_pop ) %>%
  group_by(year) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
  ) %>%
  filter(year > 1985) %>%
  filter(year <2017)

US_map <- map_data("state")
 ggplot(US_map) +
  geom_polygon(
     mapping = aes(x = long, y = lat, group = group,  fill = total),
     color = "white", 
    size = .1        
  ) +
  coord_map()





