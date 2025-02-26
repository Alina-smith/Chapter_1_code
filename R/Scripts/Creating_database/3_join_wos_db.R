# Aim of script:
# Join the DB and WOS data together and format anything to make the same
## This will then have the taxonomy, location and source info added and filtered for duplicate sources 

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ----
wos_raw <- readRDS("R/Data_outputs/full_database/wos_formatted.rds")
db_raw <- readRDS("R/Data_outputs/full_database/db_formatted.rds")


# Joining db and wos ----
joined_raw <- bind_rows(db_raw, wos_raw) 


# Invisible characters ----
# Remove any invisible characters from the original.taxa.name

## Replace with regex ----
spec_char_to_fix <- joined_raw %>% 
  mutate(
    # Replace with an easily identifiable sequence (*SpecChar* stands for special character) to help when doing taxonomy next (don't want to do a space or remove because that could change it to a different name)
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*")
  ) %>% 
  distinct(
    original.taxa.name
  ) %>% 
  filter(
    stri_detect_regex(original.taxa.name, "\\*SpecChar\\*")
  )

# save
write_csv(spec_char_to_fix, "R/data_outputs/taxonomy/spec_char_to_fix.csv")

## Swap old for new names ----
# read in updated names
updated_spec_char <- read_xlsx(here("raw_data","manual_taxonomy.xlsx"), sheet = "special_characters")

# replace ones with *SpecChar* with updated name
bodysize_spec_char <- joined_raw %>% 
  
  mutate(
    # add in *SpecChar* again
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*"),
  ) %>% 
  
  # Join updated names
  left_join(
    updated_spec_char, by = "original.taxa.name"
  ) %>% 
  
  mutate(
    # replace old with new
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

bodysize_raw <- bodysize_spec_char %>% 
  
  mutate(
    ## UID ----
    # make uid column so each data point has it's own uid
    uid = row_number()
  ) %>% 
  
  ## reorder ----
  # make easier for mutating across
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
    units = stri_replace_all_regex(units, "[^\\x20-\\x7E]", "µ"),
    
    # calculate min max
    # for any that still have only min and max and not body size calculate this
    body.size = case_when(
      is.na(body.size) & !is.na(min.body.size) & !is.na(max.body.size) ~ (min.body.size + max.body.size)/2,
      is.na(body.size) & is.na(min.body.size) & !is.na(max.body.size) ~ max.body.size,
      is.na(body.size) & !is.na(min.body.size) & is.na(max.body.size) ~ min.body.size,
      TRUE ~ body.size
    ),
    
    # remove any random capitals - set just first letter to upper case, gna_verify doesn't work with anything else
    original.taxa.name = tolower(original.taxa.name), # set all to lower case
    original.taxa.name = paste(
      toupper(
        str_sub(
          original.taxa.name, 1,1 # select first letter and set to upper case
          )
        ),
      str_sub(original.taxa.name, 2), # paste remaining word
      sep = ""
    )
  )

# Save ----
saveRDS(bodysize_raw, file = "R/data_outputs/final_products/bodysize_raw.rds")

