## using length-weight regression to get biomass values

# Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)

# Read in data
all_raw_tax <- readRDS("./Data/all_raw_tax.rds")
vol_M <- read_xlsx("./Data/length-weight master.xlsx", sheet = "McCauley rotifers")
b_pooled <- read_xlsx("./Raw data/length-weight master.xlsx", sheet = "Bottrell pooled")

### Phytoplankton
# convert biovolume to mass
all_biomass <- all_raw_tax %>% 
  filter(type == "Phytoplankton" | type == "Holoplankton" | type == "Meroplankton") %>% 
  # assign groups for length weight
  mutate(
    pooled.taxa = case_when(
      genus == "Daphnia" ~ "daphnia",
      genus == "Bosmina" ~ "bosmina",
      family == "Daphniidae" ~ "daphniidae",
      order == "Diplostraca" ~ "cladocera",
      class == "Copepoda" ~ "copepoda",
      TRUE ~ NA
    )
  ) %>% 
  left_join(b_pooled %>% select(pooled.taxa, lna, b), by = "pooled.taxa") %>% 
  # 1) convert to ml and muliply by denisty / calculate length weight
  mutate(
    indv.biomass = case_when(
      type == "Phytoplankton" ~ (cell.biovol*10^-12)*1,
      type == "Holoplankton" | type == "Meroplankton" ~ lna + (b * log(avg.length)),
      TRUE ~ NA
    ),
    nu.biomass = case_when(
      type == "Phytoplankton" ~ (nu.biovol*10^-12)*1,
      TRUE ~ NA
    ),
    nu.biomass.mucilage = case_when(
      type == "Phytoplankton" ~ (nu.biovol.mucilage*10^-12)*1,
      TRUE ~ NA
    ),
    # 2) convert to micrograms / delog
    indv.biomass = case_when(
      type == "Phytoplankton" ~ indv.biomass*10^6,
      type == "Holoplankton" | type == "Meroplankton" ~ exp(indv.biomass),
      TRUE ~ NA
    ),
    nu.biomass = case_when(
      type == "Phytoplankton" ~ nu.biomass*10^6,
      TRUE ~ NA
    ),
    nu.biomass.mucilage = case_when(
      type == "Phytoplankton" ~ nu.biomass.mucilage*10^6,
      TRUE ~ NA
    )
  ) %>% 
  select(- lna, - b, - pooled.taxa) 
# Save
saveRDS(all_biomass, file = "Data/biomass/all_biomass.rds")


## avergae biomass
avg_biomass <- all_biomass %>% 
  filter(!is.na(indv.biomass)) %>% 
  group_by(taxa.name) %>% 
  summarise(
    mean.biomass = mean(indv.biomass),
    across()
  ) %>% 
  #put the taxonomy info back in
  left_join(select(tax_list, taxa.name, rank, form, variety, species, genus, family, order, class, phylum, kingdom))

# Save
saveRDS(avg_biomass, file = "Data/biomass/avg_biomass.rds")