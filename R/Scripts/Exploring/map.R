## map



library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)
library(ggplot2)
library(rnaturalearth)
library(ggmap)

location_list <- read_csv("R/Data_outputs/full_database/location_list.csv")
source_list <- read_csv("R/Data_outputs/full_database/source_list.csv") 

map_plot <- bodysize_data %>% 
  filter(
    form == "individual",
    !is.na(mass),
    !is.na(genus),
    life.stage %in% c("adult", "active")
  ) %>% 
  
  select(
    -location.code.2:-location.code.17
  ) %>% 
  
  rename(
    location.code = location.code.1
  ) %>% 
  
  left_join(
    select(
      location_list, location.code, longitude, latitude, country
    ), by = "location.code"
  ) %>% 
  
  select(
    genus, latitude, longitude, group, location.code
  ) %>% 
  
  filter(
    !is.na(latitude)
  ) %>% 
  
  group_by(
    location.code, group
  ) %>% 
  
  summarise(
    diversity = n()
  ) %>% 
  left_join(
    select(
      location_list, location.code, longitude, latitude
    ), by = "location.code"
  )


world <- ne_countries(scale = "small", returnclass = "sf")
ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = map_plot, aes(x = longitude, y = latitude, , colour = group, size = diversity, alpha = 5))


old_map_plot <- bodysize_data %>% 
  filter(
    source.code %in% c("1", "109", "217", "313", "415", "519", "573", "618"),
    form == "individual",
    !is.na(mass),
    !is.na(genus),
    life.stage %in% c("adult", "active")
  )%>% 
  
  select(
    -location.code.2:-location.code.17
  ) %>% 
  
  rename(
    location.code = location.code.1
  ) %>% 
  
  left_join(
    select(
      location_list, location.code, longitude, latitude
    ), by = "location.code"
  )

world <- ne_countries(scale = "small", returnclass = "sf")
ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = old_map_plot, aes(x = longitude, y = latitude, colour = group))



# Define your countries
countries <- map_plot$country

# Get world data with country coordinates
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter the data to include only the specified countries
country_data <- world %>% filter(admin %in% countries) 

full_data <- map_plot %>% 
  left_join(country_data, by = c("country" = "admin"))

ggplot() +
  geom_sf(data = world, fill = "lightgrey", color = "white") +
  geom_sf(data = country_data, aes(geometry = geometry))





