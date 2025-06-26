# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(patchwork)

# Import data ----
bs <- readRDS("R/data_outputs/database_products/final_products/plankton_genus_traits.rds")
taxonomy_list <- readRDS("R/data_outputs/database_products/final_products/taxonomy_list.rds")


# Get mean masses
avg_mass <- bs %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    avg.mass = mean(c.pg)
  ) %>% 
  
  # add back in extra info
  left_join(
    taxonomy_list, by = "taxa.name"
  ) %>% 
  
  mutate(
    functional.group = case_when(
      is.na(functional.group) ~ NA,
      stri_detect_regex(functional.group, "\\/") ~ stri_extract_first_regex(functional.group, "//S+(?=\\/)"),
      !(stri_detect_regex(functional.group, "\\/")) ~ functional.group,
      TRUE ~ NA
    )
  )

# Plot
# facet by type
spread_type <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()

spread_type # View

# save
ggsave("R/Data_outputs/plots/spread_type.png", plot = spread_type)

# Facet by group
spread_group <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(type~taxonomic.group) +
  scale_y_log10()+
  guides(fill = "none")

spread_group

# save
ggsave("R/Data_outputs/plots/spread_group.png", plot = spread_group)

# Facet by fg
spread_fg <- ggplot(avg_mass, aes(x = log10(avg.mass), fill = type))+
  geom_histogram(binwidth = 0.8)+
  facet_wrap(type~functional.group) +
  scale_y_log10()+
  guides(fill = "none")

spread_fg

# save
ggsave("R/Data_outputs/plots/spread_fg.png", plot = spread_fg)

# other spreads ----
p <- bs %>% 
  
  filter(
    type == "Phytoplankton"
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  group_by(taxonomic.group) %>% 
  summarise(
    n = n()
  )

z <- bs %>% 
  
  filter(
    type == "Zooplankton"
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  )  %>% 
  group_by(taxonomic.group) %>% 
  summarise(
    n = n()
  )


group_hist_p <- ggplot(p, aes(x = taxonomic.group, fill = type))+
  geom_bar()+
  theme(
    axis.text.x = element_text(angle = -20)
  )+
  ylim(c(0, 250))

group_hist_z <- ggplot(z, aes(x = taxonomic.group, fill = type))+
  geom_bar()+
  theme(
    axis.text.x = element_text(angle = -20)
  )+
  ylim(c(0, 250))

group_hist <- group_hist_p + group_hist_z

ggsave("R/Data_outputs/plots/group_hist.png", plot = group_hist)

group_hist

