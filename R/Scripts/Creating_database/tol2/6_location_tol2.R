# Aim of this script: Join location codes to main data and make a location list
## have manually checked through the location spreadsheet for any duplicates and have set as the same location code and long and lat

# Packages ----
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)

# Data ----
location_raw <- read_xlsx("raw_data/location_data_full.xlsx", sheet = "location_raw")
bodysize_sources <- readRDS("R/Data_outputs/full_database/tol/bodysize_sources_tol2.rds")
new_sources <- readRDS("R/Data_outputs/full_database/tol/sources_list_update_tol2.rds")

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

# Update location codes in raw data ----
# left join the new location.code for each location to the raw data by the join.location columns
bodysize_location_codes <- bodysize_sources %>% 
  
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
      location_join, join.location, location.code, longitude, latitude, country, continent, location, habitat
    ), by = "join.location"
  ) %>% 
  
  ## Pivot wider ----
  # pivot back to get seperate columns for each location.code
  pivot_wider(
    id_cols = uid,
    #names_from = name,
    values_from = c(location.code, longitude, latitude, country, continent, location, habitat)
  ) %>% 
  rename_with(~ gsub("location.code_join.location", "location.code", .)) %>% 
  rename_with(~ gsub("longitude_join.location", "longitude", .)) %>% 
  rename_with(~ gsub("latitude_join.location", "latitude", .)) %>% 
  rename_with(~ gsub("country_join.location", "country", .)) %>% 
  rename_with(~ gsub("continent_join.location", "continent", .)) %>% 
  rename_with(~ gsub("location_join.location", "location", .)) %>% 
  rename_with(~ gsub("habitat_join.location", "habitat", .)) %>% 
  
  # Add back in the rest of the data
  left_join(
    ., bodysize_sources, by = "uid",
    suffix = c(".new", ".old")
  ) %>% 
  
  # remove old join.location columns 
  select(
    - ends_with(".old"),
    - starts_with("join.location")
  ) %>% 
  
  # merge columns together
  mutate(
    location.code = paste(location.code.1, location.code.2, location.code.3, location.code.4, location.code.5, location.code.6, location.code.7, location.code.8, location.code.9, location.code.10,
                          location.code.11, location.code.12, location.code.13, location.code.14, location.code.15, location.code.16, location.code.17,
                          sep = ";"),
    location.code = stri_replace_all_regex(location.code, "NA;|NA|;NA", ""),
    location.code = na_if(location.code, "")
  ) %>% 
  
  select(
    - starts_with("location.code.")
  )

# Latitude ranges ----

latitudes <- bodysize_location_codes %>%   

  pivot_longer(
    cols = starts_with("latitude"),  # Select all latitude columns
    names_to = "latitude.type",      # Temporary column name
    values_to = "latitude.value"     # Temporary values column
  ) %>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(latitude.value, na.rm = TRUE)
  ) %>% 
  
  ungroup() %>% 

  group_by(uid) %>%  
  mutate(
    latitude.range = 
      if_else(
        n >1,
        paste(range(latitude.value, na.rm = TRUE), collapse = ":"),
        latitude.value
      )
  ) %>%
  
  ungroup() %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = latitude.type,
    values_from = latitude.range
  ) %>% 
  
  select(
    uid, latitude.1
  ) %>% 
  
  rename(
    latitude = latitude.1
  )

# Longitude ranges ----
longitudes <- bodysize_location_codes%>%   
  
  pivot_longer(
    cols = starts_with("longitude"),  # Select all latitude columns
    names_to = "longitude.type",      # Temporary column name
    values_to = "longitude.value"     # Temporary values column
  ) %>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(longitude.value, na.rm = TRUE)
  ) %>% 
  
  ungroup() %>% 
  
  group_by(uid) %>%  
  mutate(
    longitude.range = 
      if_else(
        n >1,
        paste(range(longitude.value, na.rm = TRUE), collapse = ":"),
        longitude.value
      )
  ) %>%
  
  ungroup() %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = longitude.type,
    values_from = longitude.range
  ) %>% 
  
  select(
    uid, longitude.1
  ) %>% 
  
  rename(
    longitude = longitude.1
  )

# country ranges ----
countries <- bodysize_location_codes %>%
  
  pivot_longer(
    cols = starts_with("country"),  # Select all latitude columns
    names_to = "country.type",      # Temporary column name
    values_to = "country"     # Temporary values column
  ) %>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(country, na.rm = TRUE)
  ) %>% 
  
  ungroup() %>% 
  
  group_by(uid) %>%  
  mutate(
    country = 
      if_else(
        n >1,
        paste(unique(country), collapse = ","),
        country
      ),
    country = stri_replace_all_regex(country, ",NA|NA,", ""),
    country = stri_replace_all_regex(country, ",", ", ")
  ) %>%
  
  ungroup() %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = country.type,
    values_from = country
  ) %>% 
  
  select(
    uid, country.1
  ) %>% 
  
  rename(
    country = country.1
  )

# continent ranges ----
continents <- bodysize_location_codes %>%   
  
  pivot_longer(
    cols = starts_with("continent"),  # Select all latitude columns
    names_to = "continent.type",      # Temporary column name
    values_to = "continent"     # Temporary values column
  ) %>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(continent, na.rm = TRUE)
  ) %>% 
  
  ungroup() %>% 
  
  group_by(uid) %>%  
  mutate(
    continent = 
      if_else(
        n >1,
        paste(unique(continent), collapse = ","),
        continent
      ),
    continent = stri_replace_all_regex(continent, ",NA|NA,", ""),
    continent = stri_replace_all_regex(continent, ",", ", ")
  ) %>%
  
  ungroup() %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = continent.type,
    values_from = continent
  ) %>% 
  
  select(
    uid, continent.1
  ) %>% 
  
  rename(
    continent = continent.1
  )

# location ranges ----
locations <- bodysize_location_codes %>%   
  
  pivot_longer(
    cols = location.1:location.17,  # Select all latitude columns
    names_to = "location.type",      # Temporary column name
    values_to = "location"     # Temporary values column
  )%>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(location, na.rm = TRUE)
  ) %>% 
  
  ungroup()%>% 
  
  group_by(uid) %>%  
  mutate(
    location = 
      if_else(
        n >1,
        paste(unique(location), collapse = ","),
        location
      ),
    location = stri_replace_all_regex(location, ",NA|NA,", ""),
    location = stri_replace_all_regex(location, ",", ", ")
  ) %>%
  
  ungroup() %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = location.type,
    values_from = location
  ) %>% 
  
  select(
    uid, location.1
  ) %>% 
  
  rename(
    location = location.1
  )

# Habitat ranges ----
habitats <- bodysize_location_codes %>%   
  
  pivot_longer(
    cols = starts_with("habitat"),  # Select all latitude columns
    names_to = "habitat.type",      # Temporary column name
    values_to = "habitat"     # Temporary values column
  ) %>%
  group_by(location.code) %>% 
  mutate(
    n = n_distinct(habitat, na.rm = TRUE)
  ) %>% 
  
  ungroup() %>% 
  
  group_by(uid) %>%  
  mutate(
    habitat = 
      if_else(
        n >1,
        paste(unique(habitat), collapse = ","),
        habitat
      ),
    habitat = stri_replace_all_regex(habitat, ",NA|NA,", ""),
    habitat = stri_replace_all_regex(habitat, ",", ", ")
  ) %>%
  
  ungroup() %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = habitat.type,
    values_from = habitat
  ) %>% 
  
  select(
    uid, habitat.1
  ) %>% 
  
  rename(
    habitat = habitat.1
  )


# Adding all to main data ----
bodysize_location <- bodysize_location_codes %>% 
  left_join(latitudes, by = "uid") %>% 
  left_join(longitudes, by = "uid") %>% 
  left_join(countries, by = "uid") %>% 
  left_join(continents, by = "uid") %>% 
  left_join(locations, by = "uid") %>%
  left_join(habitats, by = "uid") %>%
  select(
    - location.1, - location.2, - location.3, - location.4, - location.5, - location.6, - location.7, - location.8, - location.9, - location.10, - location.11, - location.12, - location.13, - location.14, - location.15, - location.16, - location.17,
   - starts_with("longitude."),
   - starts_with("latitude."),
   - starts_with("country."),
   - starts_with("continent."),
   - starts_with("habitat.")
  ) %>% 
  
  mutate(
    habitat = na_if(habitat, "unknown"),
    location = na_if(location, "unknown")
  )

# save
saveRDS(bodysize_location, file = "R/Data_outputs/full_database/tol/bodysize_location_tol2.rds")

# update location list
updated_locations <- bodysize_location %>% 
  
  select(
    location.code
  ) %>% 
  
  separate(
    location.code, sep = ";", into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17")
    ) %>%
  
  pivot_longer(., cols = 1:17, values_to = "location.code") %>% 
  
  distinct(
    location.code
  ) %>% 
  
  left_join(., location_join, by = "location.code") %>% 
  
  distinct(
    location.code, .keep_all = TRUE
  ) %>% 
  
  select(
    -join.location,
    - source.code) %>% 
  
  filter(
    !is.na(location.code)
  )
  
# save
saveRDS(updated_locations, file = "R/Data_outputs/full_database/tol/updated_locations_tol2.rds")
  
  
  
  
  
  
  
  
