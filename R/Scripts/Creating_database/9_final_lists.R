# make final sources and tax list now all the ones i dont want have been taken out


# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ----
plankton_database <- readRDS("R/data_outputs/database_products/final_products/plankton_database.rds")
imputed_tax <- readRDS("R/data_outputs/database_products/imputed_tax.rds")
sources_list_old <- readRDS("R/Data_outputs/database_products/source_list_wt.rds")
location_list_old <- readRDS("R/Data_outputs/database_products/locations_list_update.rds")

# location list ----

# get a list of locations used in the final data
location_codes <- plankton_database %>% 
  
  # select column
  select(
    location.code
  ) %>% 
  
  # separate out into one column per code
  separate(location.code, into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), sep = ";") %>% 
  
  # pivot onto their own rows
  pivot_longer(
    cols = 1:14,
    values_to = "location.code"
  ) %>% 
  
  # get all distinct non NAs
  distinct(
    location.code
  ) %>% 
  
  filter(
    !is.na(location.code)
  ) %>% 
  
  # pull out into a vector
  pull(location.code)
  
# select locations in location_codes
locations_list <- location_list_old %>% 
  filter(
    location.code %in% location_codes
  )

# save
saveRDS(locations_list, "R/data_outputs/database_products/final_products/locations_list.rds")

# Sources ----

# get a list of sources used in final data
source_codes <- plankton_database %>% 
  
  # select columns
  select(
    source.code,
    original.sources
    ) %>% 
  
  # separate out into their own columns
  separate(original.sources, into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"), sep = ";") %>% 
  
  # put onto their own rows 
  pivot_longer(
    cols = 1:17,
    values_to = "source.code"
  ) %>% 
  
  # get all distinct non NAs
  distinct(
    source.code, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(
      source.code
      )
    )%>% 
  
  # pull out into a vector
  pull(
    source.code
  )

# Select sources that are in source_codes
sources_list <- sources_list_old %>% 
  
  # select ones in source_codes
  filter(
    source.code %in% source_codes
  ) %>% 
  
  distinct(source.code, .keep_all = TRUE) %>% 
  
  # select and reorder
  select(
    source.code, authors, year, title, publication, volume, issue, start.page, end.page, doi, ISSN, ISBN
  ) 

  
# save
saveRDS(sources_list, "R/data_outputs/database_products/final_products/sources_list.rds")

# taxonomy ----
taxonomy_groups_list <- plankton_database %>% 
  select(
    ott.id, taxa.name, type, genus, family, order, class, phylum, kingdom, taxonomic.group, functional.group
  ) %>% 
  distinct(taxa.name, .keep_all = TRUE) %>% 
  left_join(
    select(
      imputed_tax, taxa.name, tip.label
    ), by = "taxa.name"
  ) %>% 
  relocate(
    ott.id, tip.label, taxa.name, type, genus, family, order, class, phylum, kingdom, taxonomic.group, functional.group
  )
# some wont have a tip label because they arebn't in the tree

# save
saveRDS(taxonomy_groups_list, "R/data_outputs/database_products/final_products/taxonomy_groups_list.rds")

