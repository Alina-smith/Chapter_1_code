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
library(patchwork)
library(paletteer)

location_list <- read_rds("R/Data_outputs/database_products/final_products/locations_list.rds")
source_list <- read_rds("R/Data_outputs/database_products/final_products/sources_list.rds")
bs_data <- readRDS("R/Data_outputs/database_products/final_products/plankton_database.rds")

# Map ----
## format data ----
### phyto ----

phyto_groups <- bs_data %>% 
  
  filter(
    type == "Phytoplankton"
  ) %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";")%>%
  separate(functional.group, into = c("f1", "f2", "f3", "f4"), sep = "/") %>% 
  
  select(
    latitude,
    `1`,
    f1,
    taxa.name
  ) %>% 
  
  filter(
    !is.na(latitude)
  ) %>% 
  
  rename(
    location.code = `1`,
    functional.group = f1
  ) %>% 
  
  select(
    -latitude
  )


diversity_phyto <- phyto_groups %>% 
  
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

fg_phyto <- phyto_groups %>% 
  
  group_by(location.code, functional.group) %>% 
  
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

world_map_phyto <- ggplot(world) +
  geom_sf(fill = "grey90", colour = "black") +
  coord_sf(ylim = c(-90, 90), xlim = c(-185, 185), expand = FALSE) +
  
  # geom_point(
  #     data = diversity_phyto,
  #     aes(
  #       x = longitude,
  #       y = latitude,
  #       alpha = 0.5,
  #       color = diversity
  #     )
  # ) +
   #scale_color_viridis_c()
   geom_scatterpie(
     data = fg_phyto,
     aes(
       x = longitude,
       y = latitude
       ),
     pie_scale = 0.5,
     cols = "functional.group",
     long_format = TRUE
   ) +

   geom_text_repel(
     data = diversity_phyto,
     aes(x = longitude, y = latitude, label = diversity),
     box.padding = 0.5,
     max.overlaps = Inf,
     nudge_x = 1.5
   )+
  
  theme(
    legend.key.size = unit(0.1, "cm"),
    legend.spacing.x = unit(-0.3, 'cm'),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4)
  )+
  
  ggtitle("Phytoplankton")+
  
  labs(fill = "Reynolds group")

world_map_phyto


# zoo ----

zoo_groups <- bs_data %>% 
  
  filter(
    type == "Zooplankton"
  ) %>% 
  
  separate(location.code, into = c("1", "2", "3", "4", "5", "6"), sep = ";") %>% 
  separate(functional.group, into = c("f1", "f2", "f3"), sep = "/") %>% 
  
  select(
    latitude,
    `1`,
    f1,
    taxa.name
  ) %>% 
  
  filter(
    !is.na(latitude)
  ) %>% 
  
  rename(
    location.code = `1`,
    functional.group = f1
  ) %>% 
  
  select(
    -latitude
  )

diversity_zoo <- zoo_groups %>% 
  
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

fg_zoo <- zoo_groups %>% 
  
  group_by(location.code, functional.group) %>% 
  
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

world_map_zoo <- ggplot(world) +
  geom_sf(fill = "grey90", colour = "black") +
  coord_sf(ylim = c(-90, 90), xlim = c(-185, 185), expand = FALSE)+
  geom_scatterpie(
    data = fg_zoo,
    aes(
      x = longitude,
      y = latitude
    ),
    pie_scale = 0.5,
    cols = "functional.group",
    long_format = TRUE
  ) +
  
  geom_text_repel(
    data = diversity_zoo,
    aes(x = longitude, y = latitude, label = diversity),
    box.padding = 0.5,
    max.overlaps = Inf,
    nudge_x = 0.000001
  ) +
  
  theme(
    legend.key.size = unit(0.1, "cm"),
    legend.spacing.x = unit(-0.3, 'cm'),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4)
  )+
  
  ggtitle("Zooplankton")+
  
  labs(fill = "Functional group")

world_map_zoo

# join together ----
map <- world_map_phyto / world_map_zoo
map

ggsave("R/data_outputs/plots/world_map_phyto.png", plot = world_map_phyto, width = 15, height = 10, dpi = 600)
ggsave("R/data_outputs/plots/world_map_zoo.png", plot = world_map_zoo, width = 15, height = 10, dpi = 600)
ggsave("R/data_outputs/plots/map.png", plot = map, width = 10, height = 10)

# Continent spread ----
## format data ----
### phyto ----

country_spread_data <- bs_data %>% 
  #filter(type == "Zooplankton") %>% 
  select(location.code, type) %>% 
  separate_rows(location.code, sep = ";") %>%
  left_join(location_list, by = "location.code") %>% 
  distinct(location.code, .keep_all = TRUE) %>% 
  group_by(habitat) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    per = (count/sum(count))*100
  )
  

country_spread_data$country <- factor(country_spread_data$country, levels = c("Netherlands", "Ireland", "Austria", "Spain",  "Germany", "France", "Finland", "Norway", "UK", "Sweeden", "Belgium", "Czechia","Denmark","Russia", "Hungary", "Poland", "Greece", "Italy", 
                                                                                "Canada", "USA", "Mexico",
                                                                                "Uruguay", "Brazil", "Argentina", "Chile", "Ecuador", 
                                                                                "China", "Japan", "Turkey", "South Korea", "Vietnam", "India", "Israel",  "Thailand",
                                                                                "Australia", "New Zealand",
                                                                                "Ethiopia", "South Africa", "Uganda", "Kenya", "Morocco"))

country_spread_data$continent <- factor(country_spread_data$continent, levels = c("Africa", "Antarctica", "Australasia", "Asia", "South America", "North America", "Europe"))

country_spread <- ggplot(country_spread_data, aes(x = country, fill = continent))+
  geom_bar()+
  facet_wrap(~ type, ncol = 1)+
  scale_fill_manual(values = c("Africa" = "#A3C4DC", "Antarctica" = "#76B7B2", "Asia" = "#97B498", "Australasia" = "#D6A77A", "Europe" = "#BFA0C8", "North America" = "#F1D4AF", "South America" = "#C4D6A4"))+
  scale_y_log10()+
  theme(
    axis.text.x = element_text(angle = 90)
  )+
  labs(
    y = "Count (log10)",
    x = "Country"
  )

country_spread

ggsave("R/data_outputs/plots/country_spread.png", plot = country_spread, width = 10, height = 7)
