#A3
#load the library
library(lintr)
library("maps", warn.conflicts = FALSE)
library("tidyverse", warn.conflicts = FALSE)
library("plotly", warn.conflicts = FALSE)
library("leaflet", warn.conflicts = FALSE)
library("ggplot2", warn.conflicts = FALSE)
library("mapproj", warn.conflicts = FALSE)
suppressPackageStartupMessages(library("maps"))
suppressPackageStartupMessages(library("mapproj"))
# load the dataset
dataset <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", show_col_types = FALSE)
setwd("/Users/matty-so/Desktop/Info201code/a3-mattschcs/docs")
lint("../source/map.R")
total_prision_2010 <- dataset %>%
  select(year, total_prison_pop) %>%
  filter(year == 2010) %>%
  group_by(year) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
  ) %>%
  pull(total_prison_pop)



proportion_prison_in_county <- dataset %>%
  select(year, fips, total_prison_pop) %>%
  filter(year == 2010)  %>%
  group_by(fips) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
  ) %>%
  mutate(proportion = total_prison_pop / total_prision_2010)

 us_counties <- map_data("county") %>%
   unite(polyname, region, subregion, sep = ",") %>%
   left_join(county.fips, by = "polyname")

 map_data <- us_counties %>%
   left_join(proportion_prison_in_county, by = "fips")

 blank_theme <- theme_bw() +
   theme(
     axis.line = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank(),
     axis.title = element_blank(),
     plot.background = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank()
   )
 
 
map_chart <- ggplot(map_data) +
   geom_polygon(
     mapping = aes(x = long, y = lat, group = group, fill = proportion),
      color = "black",
     size = .1
   ) +
  coord_map() +
   scale_fill_continuous(limit = c(0, max(map_data$proportion)), na.value = "white", low = "yellow", high = "red") +
   labs(title = "Proportion of total prison population in county to the entire nation"
        , fill = "Proportion") +
   blank_theme

map_chart
