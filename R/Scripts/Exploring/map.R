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
library(ggrepel)

location_list <- read_rds("R/Data_outputs/database_products/final_products/location.rds")
source_list <- read_rds("R/Data_outputs/database_products/final_products/sources.rds")
bs_data <- readRDS("R/Data_outputs/database_products/final_products/phyto_subset.rds")

# Genus ----

map_plot_locations_raw_genus <- bs_data %>% 
  
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
    latitude,
    `1`,
    genus,
    r.group
  ) %>% 
  
  filter(
    !is.na(latitude)
  ) %>% 
  
  rename(
    location.code = `1`
  ) %>% 
  
  select(
    -latitude
  )


diversity_genus <- map_plot_locations_raw_genus %>% 
  
  group_by(location.code) %>% 
  
  summarise(
    diversity = n_distinct(genus),
    .groups = "drop"
  ) %>% 
  
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
    !is.na(latitude)
  )

r_group_genus <- map_plot_locations_raw_genus %>% 
  
  group_by(location.code, r.group) %>% 
  
  summarise(
    value = n(),
    .groups = "drop"
  ) %>% 
  
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
    !is.na(latitude)
  )
  
world <- ne_countries(scale = "small", returnclass = "sf")

world_map_genus <- ggplot(world) +
  geom_sf(fill = "grey90", colour = "black") +
  coord_sf(ylim = c(-90, 90), xlim = c(-185, 185), expand = FALSE) +
  geom_scatterpie(
    data = map_plot_data_genus,
    aes(
      x = longitude,
      y = latitude
      ),
    pie_scale = 0.5,
    cols = "r.group",
    long_format = TRUE
  ) +

  geom_text_repel(
    data = diversity_genus,
    aes(x = longitude, y = latitude, label = diversity),
    box.padding = 0.5,
    max.overlaps = Inf,
    nudge_x = 1.5
  )

world_map_genus


ggsave("R/data_outputs/plots/world_map_genus.png", plot = world_map_genus, width = 10, height = 6, dpi = 600)






# species ----

map_plot_locations_raw_species <- bs_data %>% 
  
  filter(
    !is.na(species)
  ) %>% 
  
  mutate(
    # when the r.group for the species is missing that use the next highest one
    r.group = case_when(
      !is.na(r.group.species) ~ r.group.species,
      is.na(r.group.species) & !is.na(r.group.genus) ~ r.group.genus,
      is.na(r.group.species) & is.na(r.group.genus) & !is.na(r.group.family) ~ r.group.family,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & !is.na(r.group.order) ~ r.group.order,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & !is.na(r.group.class) ~ r.group.class,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & is.na(r.group.class) & !is.na(r.group.phylum) ~ r.group.phylum,
      
      TRUE ~ "unasigned"
    )
  ) %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";") %>% 
  
  select(
    latitude,
    `1`,
    species,
    r.group
  ) %>% 
  
  filter(
    !is.na(latitude)
  ) %>% 
  
  rename(
    location.code = `1`
  ) %>% 
  
  select(
    -latitude
  )


diversity_species <- map_plot_locations_raw_species %>% 
  
  group_by(location.code) %>% 
  
  summarise(
    diversity = n_distinct(species),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.diversity = log(diversity)
  ) %>% 
  
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
    !is.na(latitude)
  )

map_plot_data_species <- map_plot_locations_raw_species %>% 
  
  group_by(location.code, r.group) %>% 
  
  summarise(
    value = n(),
    .groups = "drop"
    ) %>% 
  
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
    !is.na(latitude)
  )

world <- ne_countries(scale = "small", returnclass = "sf")

world_map_species <- ggplot(world) +
  geom_sf(fill = "grey90", colour = "black") +
  coord_sf(ylim = c(-90, 90), xlim = c(-185, 185), expand = FALSE) +
  geom_scatterpie(
    data = map_plot_data_species,
    aes(
      x = longitude,
      y = latitude
    ),
    pie_scale = 0.5,
    cols = "r.group",
    long_format = TRUE
  ) +
  
  geom_text_repel(
    data = diversity_species,
    aes(x = longitude, y = latitude, label = diversity),
    box.padding = 0.5,
    max.overlaps = Inf,
    nudge_x = 5
  )

world_map_species

ggsave("R/data_outputs/plots/world_map_species.png", plot = world_map_species, width = 10, height = 6, dpi = 600)









  