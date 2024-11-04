# Aim of this script: Join location codes to main data and make a location list
## have manually checked through the location spreadsheet for any duplicates and have set as the same location code and long and lat

# Packages ----
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(here)

# Data ----
location_raw <- read_xlsx(here("Raw_data","location_data_full.xlsx"), sheet = "location_raw")
bodysize_joined <- readRDS("R/Data_outputs/full_database/bodysize_joined.rds")

# Edit location list ----
# need to add source.code to join.location to make it easier to left join as there are duplicate join.locations between sources
location_join <- location_raw %>% 
  mutate(
    join.location = paste(source.code, join.location, sep = "")
  )

# Add codes to raw data ----
# left join the proper location.code for each location to the raw data by the join.location columns
bodysize_location <- bodysize_joined %>% 
  
  mutate(
    ## Merge source.code and location.code ----
    # join source.code and join.location to avoid duplicates when there is the same join.location between sources
    join.location.1 = paste(source.code, join.location.1, sep = ""),
    join.location.2 = paste(source.code, join.location.2, sep = ""),
    join.location.3 = paste(source.code, join.location.3, sep = ""),
    join.location.4 = paste(source.code, join.location.4, sep = ""),
    join.location.5 = paste(source.code, join.location.5, sep = ""),
    join.location.6 = paste(source.code, join.location.6, sep = ""),
    join.location.7 = paste(source.code, join.location.7, sep = ""),
    join.location.8 = paste(source.code, join.location.8, sep = ""),
    join.location.9 = paste(source.code, join.location.9, sep = ""),
    join.location.10 = paste(source.code, join.location.10, sep = ""),
    join.location.11 = paste(source.code, join.location.11, sep = ""),
    join.location.12 = paste(source.code, join.location.12, sep = ""),
    join.location.13 = paste(source.code, join.location.13, sep = ""),
    join.location.14 = paste(source.code, join.location.14, sep = ""),
    join.location.15 = paste(source.code, join.location.15, sep = ""),
    join.location.16 = paste(source.code, join.location.16, sep = ""),
    join.location.17 = paste(source.code, join.location.17, sep = "")
  ) %>% 
  
  ## Pivot longer ----
  # want to get each join.location on it's own line so that can left join the location codes to it
  pivot_longer(
    cols = join.location.1:join.location.17,
    values_to = "join.location"
  ) %>% 
  
  ## Location.codes ----
  # left join location codes
  left_join(
    select(
      location_join, join.location, location.code
    ), by = "join.location"
  ) %>% 
  
  ## Pivot wider ----
  # pivot back to get seperate columns for each location.code
  pivot_wider(
    id_cols = uid,
    names_from = name,
    values_from = location.code
  ) %>% 
  
  # Add back in the rest of the data
  left_join(
    ., bodysize_joined, by = "uid",
    suffix = c(".new", ".old")
  ) %>% 
  
  # remove old join.location columns 
  select(
    -join.location.1.old:-join.location.17.old
  ) %>% 
  
  # rename new join.location columns to location.code
  rename_with(~ gsub("join.location", "location.code", .)) %>% 
  rename_with(~ gsub(".new", "", .))

# save
saveRDS(bodysize_location, file = "R/Data_outputs/full_database/bodysize_location.rds")

# Check for unused ----
# check for any locations that are in the location_raw sheet but not used in the main data and if so remove them
# no locations in the location sheet that aren't used in the data so don't need to remove any
location_check <- bodysize_location %>% 
  
  # Select columns
  select(
    location.code.1:location.code.17
  ) %>% 
  
  # Pivot to get location.codes on seperate lines 
  pivot_longer(., cols = 1:17, values_to = "location.code")%>% 
  
  filter(
    !is.na(location.code)
  ) %>% 
  
  # get distinct ones
  distinct(location.code) %>% 
  
  # anti_join to leave any that are in locations raw but not bodysize_location
  anti_join(location_raw, ., by = "location.code")

# Final location list ----
# get final sheet by getting distinct location.codes - might need to be changed after later steps incase they remove any more data points
location_list <- location_raw %>% 
  distinct(location.code, .keep_all = TRUE) %>% 
  select(
    - join.location,
    - source.code
  )

# save - list of all distinct locations used in the data
saveRDS(location_list, file = "R/Data_outputs/full_database/location_list.rds")

