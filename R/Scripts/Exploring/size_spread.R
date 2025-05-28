# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(patchwork)

# Import data ----
bs <- readRDS("R/data_outputs/database_products/final_products/bodysize.rds")
taxonomy_list <- readRDS("R/data_outputs/database_products/final_products/taxonomy_list.rds")


# Get mean masses
avg_mass <- bs %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    avg.mass = mean(mass)
  ) %>% 
  
  mutate(
    log.mass = log10(avg.mass)
  ) %>% 
  
  # add back in extra info
  left_join(
    taxonomy_list, by = "taxa.name"
  ) %>% 
  filter(
    !is.na(group)
  )

# Plot
ggplot(avg_mass, aes(x = log.mass, fill = type))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, scales = "free_y", ncol = 1)

ggplot(avg_mass, aes(x = log.mass, fill = type))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~group, scales = "free_y")

ggplot(avg_mass, aes(x = log.mass, fill = type))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~fg, scales = "free_y")
