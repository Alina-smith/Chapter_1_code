## map

library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)
library(ggplot2)
library(rnaturalearth)
library(ggmap)
library(scatterpie)

location_list <- read_rds("R/Data_outputs/database_products/final_products/location.rds")
source_list <- read_rds("R/Data_outputs/database_products/final_products/sources.rds")
bs_data <- readRDS("R/Data_outputs/database_products/final_products/phyto_subset.rds")

# Genus ----

map_plot_r_group <- bs_data %>% 
  
  mutate(
    # when the r.group for the species is missing that use the next highest one
    r.group = case_when(
      !is.na(r.group.genus) ~ r.group.genus,
      is.na(r.group.genus) & !is.na(r.group.family) ~ r.group.family,
      is.na(r.group.genus) & is.na(r.group.family) & !is.na(r.group.order) ~ r.group.order,
      is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & !is.na(r.group.class) ~ r.group.class,
      is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & is.na(r.group.class) & !is.na(r.group.phylum) ~ r.group.phylum,
      
      TRUE ~ "unasigned"
    )
  ) %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";") %>% 
  
  select(
    `1`,
    latitude,
    genus,
    r.group
  ) %>% 
  
  rename(
    location.code = `1`
  ) %>% 
  
  filter(
    !is.na(latitude)
  ) %>% 
  
  group_by(
    location.code, r.group
  ) %>% 
  
  summarise(
    value = n()
  ) %>% 
  
  ungroup() %>% 
  
  left_join(
    select(
      location_list, location.code, longitude, latitude, country
    ), by = "location.code"
  ) %>% 
  
  mutate(
    latitude = if_else(
      stri_detect_regex(latitude, ":"),
      stri_extract_first_regex(latitude, "\\S+(?=:)"),
      latitude
    ),
    latitude = as.numeric(latitude),

    longitude = if_else(
      stri_detect_regex(longitude, ":"),
      stri_extract_first_regex(longitude, "\\S+(?=:)"),
      longitude
    ),
    longitude = as.numeric(longitude)
  ) %>% 
  
  filter(
    !is.na(latitude),
    r.group != "unasigned"
  ) %>% 
  
  group_by(
    location.code
  )


  
  pivot_wider(
    names_from = r.group, 
    values_from = diversity, 
    values_fill = 0) %>% 
  
  select(
    -unasigned
  ) %>% 
  
  filter(
    !is.na(latitude)
  )
  



world <- ne_countries(scale = "small", returnclass = "sf")

world_map_r_groups <- ggplot(world) +
  geom_sf(fill = "grey90", colour = "black") +
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_scatterpie(
    data = map_plot_r_group,
    aes(x = longitude, y = latitude),
    cols = "r.group",
    long_format = TRUE,
    pie_scale = 0.4  # adjust size of pies
  ) 

world_map_r_groups

ggsave("R/data_outputs/world_map_r_groups.png", plot = world_map_genus, width = 10, height = 6, dpi = 600)


world_map_genus <- ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = map_plot_genus, aes(x = longitude, y = latitude, size = diversity, alpha = 0.5), colour = "#FF8FA3")

world_map_genus



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

