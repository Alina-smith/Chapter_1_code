# body size spread

# packages

library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)
library(ggplot2)

# Data ----
species_traits <- readRDS("R/Data_outputs/species_traits.rds")
taxonomy <- readRDS("R/Data_outputs/taxonomy.rds")
traits <- readRDS("R/Data_outputs/traits.rds")

x <- species_traits %>% 
  
  filter(
    nu == "cell",
    !is.na(reynolds.group)
    ) %>%
  
  group_by(reynolds.group) %>% 
  
  summarise(
    mass.mean = mean(mass)
  )
%>% 
  
  left_join(
    traits, by = "tax.uid"
  ) %>% 
  
  mutate(
    blue.green = case_when(
      group == "Blue/green" ~ "blue/green",
      TRUE ~ "other"
    )
  )
  


ggplot(x, aes(x = log(mass.mean))) +
  geom_histogram(binwidth = 2)
+
  facet_wrap(~blue.green, scales = "free_y")
+
  theme(strip.text.x = element_text(size=0))



ncol = 1, scales = "free_y"





x <- traits %>% 
  
  mutate(
    x = case_when(
      !is.na(reynolds.group)~"yes",
      !is.na(padisak.group)~"yes",
      TRUE ~ "no"
    )
  ) %>% 
  filter(
    x == "yes"
  )
































genus_taxonomy <- taxonomy_list %>% 
  distinct(genus, .keep_all = TRUE) %>% 
  select(
    -accepted.taxa.name,
    -tax.uid,
    -tol.id,
    -species
  ) %>% 
  filter(
    !is.na(genus)
  )

bodysize_spread <- bodysize_data %>% 
  # select ones with data
  filter(
    form == "individual",
    !is.na(mass),
    !is.na(genus),
    life.stage %in% c("adult", "active")
  ) %>% 
  
  # get mean mass for each taxa
  group_by(genus) %>% 
  
  dplyr::summarise(
    mean.mass = mean(mass)
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    log.mean.mass = log(mean.mass)
  ) %>% 
  
  # add in taxonomy info
  left_join(
    select(
      genus_taxonomy, genus, group
    ),
    by = "genus"
  )

ggplot(bodysize_spread, aes(x = log.mean.mass, fill = group)) +
  geom_histogram(alpha = 0.7, binwidth = 1)+
  facet_wrap(~group, ncol = 1, scales = "free_y")+
  theme(strip.text.x = element_text(size=0))




