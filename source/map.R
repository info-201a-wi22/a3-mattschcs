#A3
#load the library 

library("tidyverse",warn.conflicts = FALSE)
library("plotly",warn.conflicts = FALSE)
library("leaflet",warn.conflicts = FALSE)  
library("ggplot2",warn.conflicts = FALSE)
library("mapproj",warn.conflicts = FALSE)
library("maps",warn.conflicts = FALSE)
#library(rjson)
# load the dataset
dataset <- read_csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",show_col_types = FALSE)

dataset_map <- dataset %>%
  mutate(county_name = gsub(" County","",dataset$county_name))%>%
  rename(subregion = county_name) %>%
  mutate(subregion = tolower(subregion))


total_prision_2010 <- dataset_map %>%
  select(year,state, total_prison_pop,state) %>%
  filter(year == 2010) %>%
  group_by(year) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
  ) %>%
  pull(total_prison_pop)



proportion_Prison_in_County <- dataset_map %>%
  select(year,fips,subregion, total_prison_pop,state,) %>%
  filter(year == 2010)  %>%
  group_by(subregion) %>%
  summarize(
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
  ) %>%
  mutate(proportion = total_prison_pop/ total_prision_2010)

 us_counties <- map_data("county") 
 
 map_data <- us_counties %>%
   group_by(subregion) %>%
   summarise(
     lat = mean(lat , na.rm = TRUE),
     long = mean(long , na.rm = TRUE),
     group = mean(group , na.rm = TRUE)
     )%>%
   left_join(proportion_Prison_in_County, by="subregion")



 ggplot(us_counties) +
   geom_polygon(
     mapping = aes(x = long, y = lat, group = group, fill = map_data),
      color = "white",
     size = .1
   ) +
  coord_map()

 
  
#  ggplot(US_map) +
# geom_polygon( data= proportion_Prison_in_state, mapping = aes(fill = state.name), show.legend = FALSE) 
#  + coord_map()
#              
 # ggplot() + 
 #   geom_sf(data = proportion_Prison_in_state, mapping = aes(fill = NAME), show.legend = FALSE) +
 #   geom_sf(data = US_map, fill = NA) + 
 #   coord_sf()
# url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
# counties <- rjson::fromJSON(file=url)
# 
# map <- plot_ly()
# map <- fig %>% add_trace(
#   type="choropleth",
#   geojson=counties,
#   locations=dataset$fips,
#   z=proportion_Prison_in_state,
#   colorscale="Viridis",
#   zmin=0,
#   zmax=12,
#   marker=list(line=list(
#     width=0)
#   )
# )
# 
# map

# total_prision_2010 <- dataset %>%
#   select(year,state, total_prison_pop,state) %>%
#   filter(year == 2010) %>%
#   group_by(year) %>%
#   summarize(
#     total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
#   ) %>%
#   pull(total_prison_pop)
# 
# 
# 
# proportion_Prison_in_County <- dataset %>%
#   select(year,fips, total_prison_pop) %>%
#   filter(year == 2010)  %>%
#   group_by(fips) %>%
#   summarize(
#     total_prison_pop = sum(total_prison_pop, na.rm = TRUE)
#   ) %>%
#   mutate(proportion = total_prison_pop/ total_prision_2010)
# 
# blank_theme <- theme_bw() +
#   theme(
#     axis.line = element_blank(),       
#     axis.text = element_blank(),       
#     axis.ticks = element_blank(),      
#     axis.title = element_blank(),      
#     plot.background = element_blank(),  
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(), 
#     panel.border = element_blank()      
#   )
# 
# 
# 
# state_shape <- map_data("county")%>%
#   group_by(subregion) %>%
#   summarise(
#     lat = mean(lat , na.rm = TRUE),
#     long = mean(long , na.rm = TRUE)
#   )
#   
# 
# ggplot(state_shape) +
#   geom_polygon(
#     mapping = aes(x = long, y = lat, group = group),
#     color = "white", 
#     size = .1        
#   ) +
#   coord_map() + # use a map-based coordinate system
#   scale_fill_continuous(low = "#132B43", high = "Red") +
#   labs(fill = "Eviction Rate") +
#   blank_theme
#   
