# Aim of script: Join the DB and WOS data together and format anything to make the same

# Flow of script:
  # 1) Join the formatted db and wos together
  # 2) Replace any invisible characters with normal characters
  # 3) Make any final edits that weren't picked up in the previous formatting steps

# Last updated: 18/03/2025

# Packages ----
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ----
wos_formatted <- readRDS("R/Data_outputs/database_products/wos_formatted.rds")
db_formatted <- readRDS("R/Data_outputs/database_products/db_formatted.rds")


# Joining db and wos ----
joined_raw <- bind_rows(db_formatted, wos_formatted) 


# Invisible characters ----
# Remove any invisible characters from the original.taxa.name

## Replace with regex ----
spec_char_to_fix <- joined_raw %>% 
  
  mutate(
    
    # Replace any invisible characters with an easily identifiable regex (*SpecChar* stands for special character) to help when doing taxonomy next (don't want to do a space or remove because that could change it to a different name)
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*")
  ) %>% 
  
  # Remove duplicates of names
  distinct(
    original.taxa.name
  ) %>% 
  
  # Select just ones with the regex
  filter(
    stri_detect_regex(original.taxa.name, "\\*SpecChar\\*")
  )

# save
write_csv(spec_char_to_fix, "R/data_outputs/database_products/spec_char_to_fix.csv")

## Swap old for new names ----

# Read in updated names
updated_spec_char <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "special_characters")

# Replace *SpecChar* with updated name
bodysize_spec_char <- joined_raw %>% 
  
  mutate(
    
    # Add in *SpecChar* again so can left join with updated_spec_char
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*"),
  ) %>% 
  
  # Join updated names
  left_join(
    updated_spec_char, by = "original.taxa.name"
  ) %>% 
  
  mutate(
    
    # Replace old with new
    original.taxa.name = if_else(
      !is.na(new.taxa.name), new.taxa.name, original.taxa.name
    ),
    
    # Remove any white spaces if there are any
    original.taxa.name = trimws(original.taxa.name)
  ) %>% 
  
  # remove redundant columns
  select(
    - new.taxa.name
  )

# Final edits ----

bodysize_raw_no_loc <- bodysize_spec_char %>% 
  
  mutate(
    
    ## UID ----
    # Make uid column so each data point has it's own uid
    uid = row_number()
  ) %>% 
  
  ## Reorder ----
  # Just to make it easier for mutating across in later steps
  relocate(
    uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
    join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
    join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
    individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
    min.body.size, max.body.size, body.size,
    bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type
  ) %>% 
  
  mutate(
    
    ## Original.source.codes NAs ----
    # When the original.source.code.1 column is NA set to "unknown" instead - if this column is NA then it means the original source is unknown
    original.source.code.1 = if_else(
      is.na(original.source.code.1),
      "unknown",
      original.source.code.1
    ),
    
    # When the other original.source.code columns are NA set to "no.source" - just makes later steps in the sources script easier
    across(
      .cols = original.source.code.2:original.source.code.18,
      ~ if_else(
        is.na(.),
        "no.source",
        .
        )
      ),
    
    ## Join.location NAs ----
    # When join.location is NA then set to "unkown" - just makes later steps in the location script easier
    across(
      .cols = join.location.2:join.location.17, # all join.location.1 column should have a value as the unknowns have their own location code
      ~ if_else(
        is.na(.),
        "unkown",
        .)
      ),
    
    ## General edits ----
    # Make general edits that got missed in previous steps
    
    # Paper code 10.1038/s41598-022-14301-y is actually body length not body size so change that
    bodysize.measurement = case_when(
      bodysize.measurement == "body size" ~ "length",
      TRUE ~ bodysize.measurement
    ),
    
    # Change units that are unicode to be the same
    units = stri_replace_all_regex(units, "[^\\x20-\\x7E]", "µ"),
    
    # Remove any random capitals - Can't use toupper because gna_verify doesn't work when the second word is capitalized
    original.taxa.name = tolower(original.taxa.name), # set all to lower case
    original.taxa.name = paste(
      toupper(
        str_sub(
          original.taxa.name, 1,1 # Select first letter and set to upper case
          )
        ),
      str_sub(original.taxa.name, 2), # Paste remaining word
      sep = ""
    ),
    
    ## Calculate min max ----
    # For any where body size is NA but there is a min and max value then use this to calculat body size
    body.size = case_when(
      is.na(body.size) & !is.na(min.body.size) & !is.na(max.body.size) ~ (min.body.size + max.body.size)/2,
      is.na(body.size) & is.na(min.body.size) & !is.na(max.body.size) ~ max.body.size,
      is.na(body.size) & !is.na(min.body.size) & is.na(max.body.size) ~ min.body.size,
      TRUE ~ body.size
    )
  )



# Adding in location info ----
# Data ----
location_raw <- read_xlsx("raw_data/location_data_full.xlsx", sheet = "location_raw")
#sources_list_nd <- readRDS("R/Data_outputs/database_products/sources_list_nd.rds")

# Edit location list ----
# need to add source.code to join.location to make it easier to left join as some sources have the same join.locationeven though they are different locations
location_join <- location_raw %>% 
  
  mutate(
    join.location = paste(source.code, join.location, sep = "-")
  )

# Update location info in raw data ----
# left join the new location.code for each location to the raw data by the join.location columns
bodysize_raw <- bodysize_raw_no_loc %>% 
  
  mutate(
    ## Merge source.code and location.code ----
    # join source.code and join.location to avoid duplicates when there is the same join.location between sources
    across(
      c(join.location.1:join.location.17),
      ~ paste(source.code, ., sep = "-")
    )
  )%>% 
  
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
      location_join, join.location, location.code, habitat, longitude, latitude, water.body, place, country, continent, area
    ), by = "join.location"
  )%>% 
  
  ## Pivot wider ----
  # pivot back to get seperate columns for each location.code
  pivot_wider(
    id_cols = uid,
    values_from = c(location.code, habitat, longitude, latitude, water.body, place, country, continent, area)
  ) %>% 
  rename_with(~ gsub("location.code_join.location", "location.code", .)) %>% 
  rename_with(~ gsub("longitude_join.location", "longitude", .)) %>% 
  rename_with(~ gsub("latitude_join.location", "latitude", .)) %>% 
  rename_with(~ gsub("country_join.location", "country", .)) %>% 
  rename_with(~ gsub("continent_join.location", "continent", .)) %>% 
  rename_with(~ gsub("water.body_join.location", "water.body", .)) %>% 
  rename_with(~ gsub("area_join.location", "area", .)) %>% 
  rename_with(~ gsub("place_join.location", "place", .)) %>% 
  rename_with(~ gsub("habitat_join.location", "habitat", .))%>% 
  
  # merge columns together
  mutate(
    location.code = pmap_chr(select(., starts_with("location.code.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")         # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
    
    latitude = pmap_chr(select(., starts_with("latitude.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      if (length(vals) == 0) {
        return(NA_character_)                          # Return NA if empty
      } else {
        paste(range(vals), collapse = ":")             # Return "min;max"
      }
    }),
    
    longitude = pmap_chr(select(., starts_with("longitude")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      if (length(vals) == 0) {
        return(NA_character_)                          # Return NA if empty
      } else {
        paste(range(vals), collapse = ":")             # Return "min;max"
      }
    }),
    
    
    habitat = pmap_chr(select(., starts_with("habitat.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")                   # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
    
    water.body = pmap_chr(select(., starts_with("water.body.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")                   # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
    
    place = pmap_chr(select(., starts_with("place.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")                   # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
    
    country = pmap_chr(select(., starts_with("country.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")                   # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
    
    continent = pmap_chr(select(., starts_with("continent.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")                   # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
    
    area = pmap_chr(select(., starts_with("area.")), function(...) {
      vals <- unlist(list(...))                     # Combine all columns into a vector
      vals <- vals[!is.na(vals) & vals != ""]       # Remove NA and empty strings
      vals <- unique(vals)                          # Keep only unique values
      merged <- paste(vals, collapse = ";")                   # Collapse to single string
      if_else(merged == "", NA, merged)
    }),
  ) %>% 
  
  select(
    - starts_with("location.code."),
    - starts_with("habitat."),
    - starts_with("water.body."),
    - starts_with("place."),
    - starts_with("country."),
    - starts_with("continent."),
    - starts_with("area."),
    - starts_with("longitude."),
    - starts_with("latitude."),
  ) %>% 
  
  # Add back in the rest of the data
  left_join(bodysize_raw_no_loc,., by = "uid") %>% 
  
  select(
    - starts_with("join.location.")
  ) %>%  
  
  # Remove data from bromeliads
  filter(
    !(habitat == "bromeliads")
  )

# Save ----
saveRDS(bodysize_raw, file = "R/data_outputs/database_products/final_products/bodysize_raw.rds")

# update location list
locations_list_update <- bodysize_raw %>% 
  
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
saveRDS(locations_list_update, file = "R/Data_outputs/database_products/locations_list_update.rds")

































