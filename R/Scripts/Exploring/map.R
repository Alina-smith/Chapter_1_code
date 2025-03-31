## map

library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)
library(ggplot2)
library(rnaturalearth)
library(ggmap)

location_list <- read_rds("R/Data_outputs/database_products/final_products/location.rds")
source_list <- read_rds("R/Data_outputs/database_products/final_products/sources.rds")
phyto_all <- readRDS("R/Data_outputs/database_products/final_products/phyto_all.rds") %>% 
  # select just individuals for now
  filter(
    nu == "individual"
  )

map_plot <- phyto_all %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";") %>% 
  
  select(
    `1`,
    species
  ) %>% 
  
  rename(
    location.code = `1`
  ) %>% 
  
  left_join(
    select(
      location_list, location.code, longitude, latitude, country
    ), by = "location.code"
  ) %>% 
  
  filter(
    !is.na(latitude),
    !is.na(species)
  ) %>% 
  
  group_by(
    location.code, species
  ) %>% 
  
  summarise(
    diversity = n()
  ) %>% 
  
  left_join(
    select(
      location_list, location.code, longitude, latitude, country
    ), by = "location.code"
  )


world <- ne_countries(scale = "small", returnclass = "sf")
ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = map_plot, aes(x = longitude, y = latitude, size = diversity, alpha = 5))


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





