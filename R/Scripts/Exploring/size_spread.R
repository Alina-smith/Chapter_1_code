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
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    taxa.name, family, order, class, phylum, kingdom, r.group, group, mass.d, mass.c
  )

# Get a taxonomy list to add in in later steps
extra_info_genus <- phyto_format_genus %>% 
  
  select(
    -mass.d,
    -mass.c
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  )

# Get mean masses
mass_genus <- phyto_format_genus %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    avg.mass.c = mean(mass.c),
    avg.mass.d = mean(mass.d)
  ) %>% 
  
  # add back in extra info
  left_join(
    extra_info_genus, by = "taxa.name"
  )

# Plot
# Facet by R group
size_spread_r_genus <- ggplot(mass_genus, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.3) +
  # facet by r.group
  facet_wrap(~r.group, ncol = 5)

size_spread_r_genus

ggsave("R/Data_outputs/plots/ssize_spread_r_genus.png", plot = size_spread_r_genus, width = 10, height = 6, dpi = 600)

# Facet by group
size_spread_group_genus <- ggplot(mass_genus, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.5) +
  # facet by group
  facet_wrap(~group, scales = "free_y", ncol = 5)

size_spread_group_genus

ggsave("R/Data_outputs/plots/size_spread_group_genus.png", plot = size_spread_group_genus, width = 10, height = 6, dpi = 600)
