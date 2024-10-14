## Location Information

## Aim of this script:
  # have manuallt checked through the location spreadsheet for any duplicates and have made the same location code and long and lat
  # Change the join.location columns in the bodysize_joined data to location codes and then merge into one column so there isn't 17 columns for because of datapoints where the sources are not species specific just given multiple ones for all of them
  # Check to see if there are any locations in the location_raw list that aren't used in the main data and remove
  # Get lit of distinct location codes for final location list

# Packages ----
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

# Data ----
location_raw <- read_xlsx(here("Raw_data","location_data_for_wos_db.xlsx"), sheet = "location_raw")
bodysize_joined <- readRDS("R/Data_outputs/databases/bodysize_joined.rds") ## CHANGE WHRN SOURCE STUFF DONE

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
  
  ## Merge columns together ----
  mutate(
    location.code = paste(join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9,
                          join.location.10, join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
                          sep = ","),
    location.code = stri_replace_all_regex(location.code, ",NA|NA,|NA", ""),
    location.code = na_if(location.code, "") 
  ) %>% 
  
  ## Redundant columns
  select(
    - join.location.1, - join.location.2, - join.location.3, - join.location.4, - join.location.5, - join.location.6, - join.location.7, - join.location.8, - join.location.9, - join.location.10,
    - join.location.11, - join.location.12, - join.location.13, - join.location.14, - join.location.15, - join.location.16, - join.location.17
  ) %>% 
  
  ## Add back in the rest of the data ----
  left_join(
    ., bodysize_joined, by = "uid"
  )
  

## save ----
saveRDS(bodysize_location, file = "R/Data_outputs/databases/bodysize_location.rds")

# Check for unused ----
# check for any locations that are in the location_raw sheet but not used in the main data and if so remove them
# no locations in the location sheet that aren't used in the data so don't need to remove any
location_check <- bodysize_location %>% 
  
  # Select columns
  select(
    location.code
  ) %>% 
  
  # Separate out
  separate(
    ., location.code, into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17"),
    sep = ","
  ) %>% 
  
  # Pivot to get location.codes on seperate lines 
  pivot_longer(., cols = 1:17, values_to = "location.code") %>% 
  
  filter(
    !is.na(location.code)
  ) %>% 
  
  # get distinct ones
  distinct(location.code) %>% 
  
  # anti_join to leave any that are in locations raw but not bodysize_location
  anti_join(location_raw, ., by = "location.code")

# Final location list ----
# get final sheet by getting distinct location.codes
location_list <- location_raw %>% 
  distinct(location.code, .keep_all = TRUE) %>% 
  select(
    - join.location,
    - source.code
  )

# save - list of all distinct locations used in the data
saveRDS(location_list, file = "R/Data_outputs/databases/location_list.rds")

