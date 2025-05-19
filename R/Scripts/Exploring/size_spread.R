# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)

# Import data ----
bodysize <- readRDS("R/data_outputs/database_products/final_products/bodysize.rds")

# Format data ----

phyto_format <- bodysize %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    taxa.name, type, family, order, class, phylum, kingdom, fg, group, mass.all.d, mass.all.c
  )

# Get a taxonomy list to add in in later steps
extra_info <- phyto_format %>% 
  
  select(
    -mass.all.d,
    -mass.all.c
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  )

# Get mean masses
mass <- phyto_format %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    avg.mass.c = mean(mass.all.c),
    avg.mass.d = mean(mass.all.d)
  ) %>% 
  
  # add back in extra info
  left_join(
    extra_info, by = "taxa.name"
  ) %>% 
  filter(
    !is.na(group)
  )

# Plot
# Facet by R group
size_spread_r <- ggplot(mass, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.3) +
  # facet by r.group
  facet_wrap(~fg, ncol = 5)

size_spread_r

ggsave("R/Data_outputs/plots/size_spread_r.png", plot = size_spread_r, width = 10, height = 6, dpi = 600)

# Facet by group
size_spread_group <- ggplot(mass, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.5) +
  # facet by group scales = "free_y"
  facet_wrap(~group, ncol = 5)

size_spread_group

ggsave("R/Data_outputs/plots/size_spread_group.png", plot = size_spread_group, width = 10, height = 6, dpi = 600)

# Facet by type
size_spread_type <- ggplot(mass, aes(x = log(avg.mass.d))) +
  geom_histogram(binwidth = 0.5) +
  # facet by group scales = "free_y"
  facet_wrap(~type, scales = "free_y", ncol = 1)

size_spread_type

ggsave("R/Data_outputs/plots/size_spread_type.png", plot = size_spread_type, width = 10, height = 6, dpi = 600)
