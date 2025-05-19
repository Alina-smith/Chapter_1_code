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
bs_data <- readRDS("R/Data_outputs/database_products/final_products/bodysize.rds")

# format data ----

map_plot_locations_raw <- bs_data %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";") %>% 
  
  select(
    latitude,
    `1`,
    taxa.name,
    fg
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


diversity <- map_plot_locations_raw %>% 
  
  group_by(location.code) %>% 
  
  summarise(
    diversity = n_distinct(taxa.name),
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

fg <- map_plot_locations_raw %>% 
  
  group_by(location.code, fg) %>% 
  
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

world_map <- ggplot(world) +
  geom_sf(fill = "grey90", colour = "black") +
  coord_sf(ylim = c(-90, 90), xlim = c(-185, 185), expand = FALSE) +
  geom_scatterpie(
    data = fg,
    aes(
      x = longitude,
      y = latitude
      ),
    pie_scale = 0.5,
    cols = "fg",
    long_format = TRUE
  ) +

  geom_text_repel(
    data = diversity,
    aes(x = longitude, y = latitude, label = diversity),
    box.padding = 0.5,
    max.overlaps = Inf,
    nudge_x = 1.5
  )

world_map


ggsave("R/data_outputs/plots/world_map.png", plot = world_map, width = 10, height = 6, dpi = 600)
