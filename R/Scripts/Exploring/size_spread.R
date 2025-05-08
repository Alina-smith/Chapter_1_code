# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)

# Import data ----
phyto_subset <- readRDS("R/data_outputs/database_products/final_products/phyto_subset.rds")

# Genus ----
## Format data ----

phyto_format_genus <- phyto_subset %>% 
  
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
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    genus, phylum, kingdom, family, order, class, r.group, group, mass.d, mass.c
  )

# Get a taxonomy list to add in in later steps
extra_info_genus <- phyto_format_genus %>% 
  
  select(
    -mass.d,
    -mass.c
  ) %>% 
  
  distinct(
    genus, .keep_all = TRUE
  )

# Get mean masses
mass_genus <- phyto_format_genus %>% 
  
  group_by(
    genus
  ) %>% 
  
  summarise(
    avg.mass.c = mean(mass.c),
    avg.mass.d = mean(mass.d)
  ) %>% 
  
  # add back in extra info
  left_join(
    extra_info_genus, by = "genus"
  )

# Plot
# Facet by R group
size_spread_r_genus <- ggplot(mass_genus, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 2) +
  # facet by r.group
  facet_wrap(~r.group, scales = "free_y", ncol = 5)

size_spread_r_genus

ggsave("R/Data_outputs/plots/ssize_spread_r_genus.png", plot = size_spread_r_genus, width = 10, height = 6, dpi = 600)

# Facet by group
size_spread_group_genus <- ggplot(mass_genus, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 2) +
  # facet by group
  facet_wrap(~group, scales = "free_y", ncol = 5)

size_spread_group_genus

ggsave("R/Data_outputs/plots/size_spread_group_genus.png", plot = size_spread_group_genus, width = 10, height = 6, dpi = 600)


# Species ----
## Format data ----

phyto_format_species <- phyto_subset %>% 
  
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
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    species, genus, phylum, kingdom, family, order, class, r.group, group, mass.d, mass.c
  )

# Get a taxonomy list to add in in later steps
extra_info_species <- phyto_format_species %>% 
  
  select(
    -mass.d,
    -mass.c
  ) %>% 
  
  distinct(
    species, .keep_all = TRUE
  )

# Get mean masses
mass_species <- phyto_format_species %>% 
  
  group_by(
    species
  ) %>% 
  
  summarise(
    avg.mass.c = mean(mass.c),
    avg.mass.d = mean(mass.d)
  ) %>% 
  
  # add back in extra info
  left_join(
    extra_info_species, by = "species"
  )

# Plot
# Facet by R group
size_spread_r_species <- ggplot(mass_species, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.3) +
  # facet by r.group
  facet_wrap(~r.group, ncol = 5)

size_spread_r_species

ggsave("R/Data_outputs/plots/size_spread_r_species.png", plot = size_spread_r_species, width = 10, height = 6, dpi = 600)

# Facet by group
size_spread_group_species <- ggplot(mass_species, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.5) +
  # facet by group
  facet_wrap(~group, scales = "free_y", ncol = 5)

size_spread_group_species

ggsave("R/Data_outputs/plots/size_spread_group_species.png", plot = size_spread_group_species, width = 10, height = 6, dpi = 600)




