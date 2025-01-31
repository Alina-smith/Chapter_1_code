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
bodysize_sources <- readRDS("R/Data_outputs/full_database/bodysize_sources.rds")
new_sources <- readRDS("R/Data_outputs/full_database/source_list.rds")

# Edit location list ----
# need to add source.code to join.location to make it easier to left join as some sources have the same join.locationeven though they are different locations
location_join <- location_raw %>% 
  
  # update with new source codes
  left_join(., select(
    new_sources, source.code, new.source.code
    ), by = "source.code"
  ) %>% 
  
  select(
    -source.code
  ) %>% 
  
  rename(
    source.code = new.source.code
  ) %>% 
  
  mutate(
    join.location = paste(source.code, join.location, sep = "-")
  )

# Add codes to raw data ----
# left join the new location.code for each location to the raw data by the join.location columns
bodysize_location <- bodysize_sources %>% 
  
  mutate(
    ## Merge source.code and location.code ----
    # join source.code and join.location to avoid duplicates when there is the same join.location between sources
    across(
      c(join.location.1:join.location.17),
      ~ paste(source.code, ., sep = "-")
    )
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
      location_join, join.location, location.code, longitude, latitude
    ), by = "join.location"
  ) %>% 
  
  ## Pivot wider ----
  # pivot back to get seperate columns for each location.code
  pivot_wider(
    id_cols = uid,
    names_from = name,
    values_from = c(location.code, longitude, latitude)
  ) %>% 
  rename_with(~ gsub("location.code_join.location", "location.code", .)) %>% 
  rename_with(~ gsub("longitude_join.location", "longitude", .)) %>% 
  rename_with(~ gsub("latitude_join.location", "latitude", .)) %>% 
  
  # Add back in the rest of the data
  left_join(
    ., bodysize_sources, by = "uid",
    suffix = c(".new", ".old")
  ) %>% 
  
  # remove old join.location columns 
  select(
    - ends_with(".old"),
    - starts_with("join.location")
  ) 

x <- bodysize_location %>%   
  # merge columns together
  mutate(
    location.code = paste(location.code.1, location.code.2, location.code.3, location.code.4, location.code.5, location.code.6, location.code.7, location.code.8, location.code.9, location.code.10,
                          location.code.11, location.code.12, location.code.13, location.code.14, location.code.15, location.code.16, location.code.17,
                             sep = ";"),
    location.code = stri_replace_all_regex(location.code, "NA;|NA|;NA", ""),
    location.code = na_if(location.code, "")
  ) %>% 

  pivot_longer(
    cols = starts_with("latitude"),  # Select all latitude columns
    names_to = "latitude_type",      # Temporary column name
    values_to = "latitude_value"     # Temporary values column
  ) %>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(latitude_value, na.rm = TRUE)
  ) %>% 
  
  ungroup() %>% 

  group_by(uid) %>%  
  mutate(
    latitude_range = 
      if_else(
        n >1,
        paste(range(latitude_value, na.rm = TRUE), collapse = ":"),
        latitude_value
      )
  )


y <- x %>% 
  pivot_wider(
    id_cols = uid,
    names_from = latitude_type,
    values_from = latitude_range
  )


# save
saveRDS(bodysize_location, file = "R/Data_outputs/full_database/bodysize_location.rds")
