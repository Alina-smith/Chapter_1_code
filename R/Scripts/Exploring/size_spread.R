# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)

# Data ----
bodysize <- readRDS("R/data_outputs/database_products/final_products/bodysize.rds")
tax_list <- readRDS("R/data_outputs/database_products/taxonomy/tax_list_raw.rds")

# Specis ----

bodysize_format <- bodysize %>% 
  
  mutate(
    mass.all = case_when(
      !is.na(mass.d) ~ mass.d,
      !is.na(mass) ~ mass,
      
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    !is.na(mass.all),
    !is.na(species)
  ) %>% 
  
  # get mean mass for each species
  group_by(ott.id) %>% 
  
  summarise(
    mean.mass = mean(mass.all),
    .groups = "drop"
  ) %>% 
  
  left_join(
    ., tax_list, by = "ott.id"
  )


size_spread <- ggplot(bodysize_format, aes(x = log(mean.mass), fill = type)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Phytoplankton" = "#FF8FA3", "Zooplankton" = "#B19CD9"))

size_spread

ggsave("R/data_outputs/database_products/plots/size_spread.png", plot = size_spread, width = 10, height = 6, dpi = 600)

# Genus ---

tax_genus <- tax_list %>% 
  
  select(
    genus,
    type
    ) %>% 
  
  distinct(genus, .keep_all = TRUE)

bodysize_format_genus <- bodysize %>% 
  
  filter(
    !(taxa.name %in% c("Calanus", "Moina", "Neodiaptomus", "Asplanchna", "Paracyclopina")),
    !(individual.uid %in% c("10.1007/s11356-022-23696-0-436", "10.1007/s11356-022-23696-0-172", "10.1007/s11356-022-23696-0-84", "10.1007/s11356-022-23696-0-524", "10.1007/s11356-022-23696-0-348", "10.1007/s11356-022-23696-0-260"))
  ) %>% 
  
  mutate(
    mass.all = case_when(
      !is.na(mass.d) ~ mass.d,
      !is.na(mass) ~ mass,
      
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    nu == "individual",
    !is.na(mass.all),
    !is.na(genus)
  ) %>% 
  
  # get mean mass for each species
  group_by(genus) %>% 
  
  summarise(
    mean.mass = mean(mass.all),
    .groups = "drop"
  ) %>% 
  
  left_join(
    ., tax_genus, by = "genus"
  )


size_spread_genus <- ggplot(bodysize_format_genus, aes(x = log10(mean.mass), fill = type)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Phytoplankton" = "#FF8FA3", "Zooplankton" = "#B19CD9"))

size_spread_genus

ggsave("R/data_outputs/database_products/plots/size_spread_genus.png", plot = size_spread, width = 10, height = 6, dpi = 600)





