# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(patchwork)

# Import data ----
bs <- readRDS("R/data_outputs/database_products/final_products/bodysize_traits.rds")
taxonomy_list <- readRDS("R/data_outputs/database_products/final_products/taxonomy_list.rds")


# Get mean masses
avg_mass <- bs %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    avg.mass = mean(mass)
  ) %>% 
  
  # add back in extra info
  left_join(
    taxonomy_list, by = "taxa.name"
  )

# Plot
# facet by type
spread_type <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.4)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()

spread_type # View

# save
ggsave("R/Data_outputs/plots/spread_type.png", plot = spread_type)

# Facet by phyla
spread_phylum <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.8)+
  facet_wrap(~phylum) +
  scale_y_log10()

spread_phylum

# save
ggsave("R/Data_outputs/plots/spread_phylum.png", plot = spread_phylum)

# Facet by group
spread_group <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.8)+
  facet_wrap(~group) +
  scale_y_log10()

spread_group

# save
ggsave("R/Data_outputs/plots/spread_group.png", plot = spread_group)

# Facet by fg
spread_fg <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.8)+
  facet_wrap(~fg) +
  scale_y_log10()

spread_fg

# save
ggsave("R/Data_outputs/plots/spread_fg.png", plot = spread_fg)




