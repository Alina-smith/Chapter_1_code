# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)

# Data ----
genus_traits <- readRDS("R/Data_outputs/final_products/phyto_traits_genus.rds")

y <- genus_traits %>% 
  select(
    -original.sources, -source.code, individual.uid, -nu, -mass, -cells.per.nu, -biovolume, -mld, -sample.month, -sample.year, -location.code, -longitude, -latitude, -location, -habitat, -country, - continent, - individual.uid
  ) %>% 
  distinct(
    genus, .keep_all = TRUE
  )

x <- genus_traits %>% 
  
  mutate(
    reynolds.group = case_when(
      !is.na(reynolds.group) ~ reynolds.group,
      is.na(reynolds.group) & !is.na(padisak.group) ~ padisak.group,
      TRUE ~ "Unclassified"
    )
  ) %>% 
  
  group_by(reynolds.group) %>% 
  
  summarise(
    mass.mean = mean(mass),
    .groups = "drop"
  ) 
%>% 
  
  left_join(
    y , by = "genus"
  )
  


ggplot(x, aes(x = log(mass.mean))) +
  geom_histogram(binwidth = 3)
+
  facet_wrap(~reynolds.group, scales = "free_y")

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