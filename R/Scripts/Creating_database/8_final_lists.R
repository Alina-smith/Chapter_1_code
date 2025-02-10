# make final sources and tax list now all the ones i dont want have been taken out


# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)

# Data ----
species_raw_cell_size <- readRDS("R/Data_outputs/full_database/species_raw_cell_size.rds")
tax_list_distinct <- readRDS("R/Data_outputs/taxonomy/gbif/tax_list_distinct.rds")
source_list <- readRDS("R/Data_outputs/locations_sources/source_list.rds")
location_list <- readRDS("R/Data_outputs/locations_sources/location_list.rds")

# taxonomy list ----

# get a list of all the distinct tax.uids still in the data
tax_uids <- species_raw_cell_size %>% 
  distinct(
    tax.uid
  ) %>% 
  
  # convert to string
  pull(tax.uid)

# select just ones in the tax_uids list
taxonomy <- tax_list_distinct %>% 
  
  filter(
    tax.uid %in% tax_uids
  ) %>% 
  
  # select and reorder - don't need type or rank as all are species and phyto, don't need resolved.taxa.name anymore
  relocate(
    tax.uid, taxa.name, species, genus, family, order, class, phylum, kingdom
  ) %>% 
  
  ungroup()

# save
saveRDS(taxonomy, "R/data_outputs/taxonomy.rds")


# location list ----

# get a list of locations used in the final data
location_codes <- species_raw_cell_size %>% 
  
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
location <- location_list %>% 
  filter(
    location.code %in% location_codes
  )

# save
saveRDS(location, "R/data_outputs/location.rds")

# Sources ----

# get a list of sources used in final data
source_codes <- species_raw_cell_size %>% 
  
  # select columns
  select(
    source.code,
    original.sources
    ) %>% 
  
  # separate out into their own columns
  separate(original.sources, into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"), sep = ";") %>% 
  
  # make same type
  mutate(
    across(
      everything(), ~ as.integer(.)
      )
    ) %>% 
  
  # put onto their own rows 
  pivot_longer(
    cols = 1:17,
    values_to = "source.code"
  ) %>% 
  
  # get all distinct non NAs
  distinct(
    source.code
  ) %>% 
  
  filter(
    !is.na(
      source.code
    )
  ) %>% 
  
  # pull out into a vector
  pull(
    source.code
  )
  
# Select sources that are in source_codes
sources <- source_list %>% 
  
  # rename new.source.code and source.code and remove old source.code
  select(
   - source.code
  ) %>% 
  
  rename(
    source.code = new.source.code
  ) %>% 
  
  # select ones in source_codes
  filter(
    source.code %in% source_codes
  ) %>% 
  
  # select and reorder
  select(
    source.code, authors, year, title, publication, volume, issue, start.page, end.page, doi, ISSN, ISBN
  )
  
# save
saveRDS(sources, "R/data_outputs/sources.rds")

