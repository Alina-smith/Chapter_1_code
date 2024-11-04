# Aim of script:
# Join the DB and WOS data together and format anything to make the same
## This will then have the taxonomy, location and source info added and filtered for duplicate sources 

# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ----
wos_raw <- readRDS("R/Data_outputs/full_database/wos_formatted.rds")
db_raw <- readRDS("R/Data_outputs/full_database/db_formatted.rds")


# Joining db and wos ----
bodysize_joined <- bind_rows(db_raw, wos_raw) %>% 
  
  mutate(
    # make uid column so each data point has it's own uid
    uid = row_number(),
    
    ## Invisible characters ----
    # Remove any invisible characters from the original.taxa.name - replace with an easily identifiable sequence (*SpecChar* stands for special character) to help when doing taxonomy next (don't want to do a space or remove because that could change it to a different name)
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*")
  ) %>% 
  
  ## reorder ----
  # make easier for mutating across
  relocate(
    uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
    join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
    join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
    individual.uid, original.taxa.name, life.stage, sex, form, form.no,
    min.body.size, max.body.size, body.size,
    bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type
  ) %>% 
    
  mutate(
    ## Original.source.codes NAs
    # set NA original.source.code.1 to "unknown" - because if original.source.code.1 is NA then it means there is no original source code and so it is unknown
    original.source.code.1 = if_else(
      is.na(original.source.code.1),
      "unknown",
      original.source.code.1
    ),
    
    # set NA in the original.source.columns to no.source - because if these are NA it is because it just didn't have that many original source codes
    # set to "no.source" instead of NA to make later steps for the sources easier
    across(
      .cols = original.source.code.2:original.source.code.18,
      ~ if_else(
        is.na(.),
        "no.source",
        .
        )
      ),
    
    ## join.source NAs ----
    # set join.source to no.source for NAs - because if these are NA it is because it just didn't have that many locations
    # set to "no.source" instead of NA to make later steps for the location codes easier
    across(
      .cols = join.location.2:join.location.17, # all join.location.1 column should have a value as the unknonws have their own location code
      ~ if_else(
        is.na(.),
        "no.source",
        .)
      ),
    
    # General edits ----
    # make general edits that got missed in previous steps
    
    # paper code 10.1038/s41598-022-14301-y is actually body length not body size so change that
    bodysize.measurement = case_when(
      bodysize.measurement == "body size" ~ "length",
      TRUE ~ bodysize.measurement
    ),
    
    # change units that are unicode to be the same
    units = case_when(
      units %in% c("µm", "μm") ~ "µm",
      units %in% c("µg", "μg") ~ "µg",
      units %in% c("µm^3", "μm^3") ~ "µm^3",
      TRUE ~ units
    ),
    
    # calculate min max
    # for nay that still have only min and max and not body size calculate this
    body.size = case_when(
      is.na(body.size) & !is.na(min.body.size) & !is.na(max.body.size) ~ (min.body.size + max.body.size)/2,
      is.na(body.size) & is.na(min.body.size) & !is.na(max.body.size) ~ max.body.size,
      is.na(body.size) & !is.na(min.body.size) & is.na(max.body.size) ~ min.body.size,
      TRUE ~ body.size
    )
  )

# Save ----
saveRDS(bodysize_joined, file = "R/Data_outputs/full_database/bodysize_joined.rds")
