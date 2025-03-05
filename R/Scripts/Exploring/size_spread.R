# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)

# Data ----
genus_traits <- readRDS("R/Data_outputs/final_products/phyto_traits_genus.rds")
species_traits <- readRDS("R/Data_outputs/final_products/phyto_traits_species.rds")

traits <- species_traits %>% 
  select(
    -original.sources, -source.code, individual.uid, -nu, -mass, -cells.per.nu, -biovolume, -mld, -sample.month, -sample.year, -location.code, -longitude, -latitude, -location, -habitat, -country, - continent, - individual.uid
  ) %>% 
  distinct(
    species, .keep_all = TRUE
  )

# species plot
species_plot <- species_traits %>% 
  
  group_by(species) %>% 
  
  summarise(
    mass.mean = mean(mass),
    .groups = "drop"
  ) %>% 
  
  left_join(
    traits, by = "species"
  ) %>% 
  
  mutate(
    reynolds.group = case_when(
      !is.na(reynolds.group) ~ reynolds.group,
      is.na(reynolds.group) & !is.na(padisak.group) ~ padisak.group,
      TRUE ~ "Unclassified"
    )
  )

x <- species_plot %>% 
  filter(
    !(reynolds.group == "Unclassified")
  )

ggplot(species_plot, aes(x = log(mass.mean))) +
  geom_histogram(binwidth = 2)+
  facet_wrap(~reynolds.group, scales = "free_y")

+
  theme(strip.text.x = element_text(size=0))

# Genus plot

genus_plot <- genus_traits %>% 
  
  group_by(genus) %>% 
  
  summarise(
    mass.mean = mean(mass),
    .groups = "drop"
  ) %>% 
  
  left_join(
    traits, by = "genus"
  ) %>% 
  
  mutate(
    reynolds.group = case_when(
      !is.na(reynolds.group) ~ reynolds.group,
      is.na(reynolds.group) & !is.na(padisak.group) ~ padisak.group,
      TRUE ~ "Unclassified"
    )
  )

ggplot(genus_plot, aes(x = log(mass.mean))) +
  geom_histogram(binwidth = 3)+
  facet_wrap(~reynolds.group, scales = "free_y")


