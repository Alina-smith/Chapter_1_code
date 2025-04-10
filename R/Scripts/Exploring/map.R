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
bs_data <- readRDS("R/Data_outputs/database_products/final_products/phyto_subset.rds")
tax_list <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")


x <- bs_data %>% 
  select(
    genus,
    r.group
  ) %>% 
  group_by(
    genus, r.group
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() 

x2 <- x %>% 
  group_by(
    genus
  ) %>% 
  summarise(
    n2 = n()
  ) %>% 
  ungroup() %>% 
  filter(
    n2 > 1
  )
  
  

# Species ----

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
  ) %>% 
  
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

class(map_plot$longitude)

world <- ne_countries(scale = "small", returnclass = "sf")
world_map <- ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = map_plot, aes(x = longitude, y = latitude, size = diversity, alpha = 0.5), colour = "#FF8FA3")

ggsave("R/data_outputs/database_products/plots/world_map.png", plot = world_map, width = 10, height = 6, dpi = 600)


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
  geom_sf(data = country_data, aes(geometry = geometry))+
  geom_point(data = map_plot, aes(x = longitude, y = latitude, size = diversity, alpha = 0.5), colour = "#FF8FA3")

# Genus ----

map_plot_genus <- phyto_all %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";") %>% 
  
  select(
    `1`,
    genus
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
    !is.na(genus)
  ) %>%
  
  group_by(
    location.code, genus
  ) %>% 
  
  summarise(
    diversity = n()
  ) %>% 
  
  left_join(
    select(
      location_list, location.code, longitude, latitude, country
    ), by = "location.code"
  ) %>% 
  
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

class(map_plot_genus$longitude)

world <- ne_countries(scale = "small", returnclass = "sf")
world_map_genus <- ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = map_plot_genus, aes(x = longitude, y = latitude, size = diversity, alpha = 0.5), colour = "#FF8FA3")

ggsave("R/data_outputs/database_products/plots/world_map_genus.png", plot = world_map_genus, width = 10, height = 6, dpi = 600)


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
  geom_sf(data = country_data, aes(geometry = geometry))+
  geom_point(data = map_plot, aes(x = longitude, y = latitude, size = diversity, alpha = 0.5), colour = "#FF8FA3")





