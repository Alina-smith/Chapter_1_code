# Aim of script: Standardizing the WOS data

# Flow of script:
  # 1) Body size edits:             Make any minor edits and when body.size is NA use min and max to calculate it
  # 2) Body size measurment edits:  Extract the extra info in the bodysize.measurements column and put it into a new notes column
  # 3) Units:                       Standadize the units
  # 4) Life stage:                  Standadize the life stages
  # 5) Dates:                       Put the dates into one column as a range
  # 6) Ind.per.nu:                  When NA but form is individual set to 1
  # 7) Edit tyes:                   Change type of columns to help with merging later on

# Last updated: 18/03/2025

# Packages ----
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ---- 
wos_raw_body <- read_xlsx("raw_data/master_wos_data.xlsx", sheet = "bodysize", guess_max = 100000)
wos_source_list <- read_xlsx("raw_data/master_wos_data.xlsx", sheet = "source_list")


# Formatting ----

wos_formatted <- wos_raw_body %>%
  
  ## Filter ----
  filter(
    # Remove "unknown" taxa names
    !(original.taxa.name == "unknown")
  ) %>% 
  
  ## Select columns ----
  # Remove columns I don't need as were not present in all data
  select(
    - experimental.design,
    - treatment.1.name,
    - treatment.1.value,
    - treatment.2.name,
    - treatment.2.value,
  ) %>% 
  
  mutate(
    
    ## Body size edits ----
    # Make edits to the size columns
    
    ### Body.size ----
    # Replace , with .
    body.size = stri_replace_all_regex(body.size, ",", "."),
    body.size = case_when(
      
      # Set any variations of na to NA
      body.size %in% c("NA", "na", "-", "0") ~ NA,
      
      # change one with two decimal places to one
      individual.uid == "10.1002/etc.5034-2" & body.size == "0.000.16" ~ "0.00016",
      TRUE ~ body.size
    ),
    
    ### Min.body.size ----
    # Set any variations of na to NA
    min.body.size = if_else(
      min.body.size == "NA",
      NA, # make NA
      min.body.size
    ),
    
    ### Max.body.size ----
    # Set any variations of na to NA
    max.body.size = if_else(
      max.body.size == "NA",
      NA, # make NA
      max.body.size
    ),
    
    ### Calculations ----
    # Make the same type
    total.bs = as.numeric(total.bs),
    abundance = as.numeric(abundance),
    body.size = as.numeric(body.size),
    min.body.size = as.numeric(min.body.size),
    max.body.size = as.numeric(max.body.size),
    
    # Calculate
    body.size = case_when(
      
      # Divide total population biovolume by abundance
      is.na(body.size) & !is.na(total.bs) ~ total.bs/abundance,
      
      # Take average of range values when the average isn't given already
      measurement.type == "range" & is.na(body.size) ~ (min.body.size+max.body.size)/2,
      
      # Select min values
      measurement.type == "min" & is.na(body.size) ~ min.body.size,
      
      # Select max values
      measurement.type == "max" & is.na(body.size) ~ max.body.size,
      
      # Keep the rest the same
      TRUE ~ body.size
    )
  ) %>% 
  
  ## Filter ----
  # Filter out body sizes with NA only for all min,max and body.size
  mutate(
    keep = if_else(
      is.na(body.size) & is.na(min.body.size) & is.na(max.body.size),
      "NA",
      "keep"
    )
  ) %>% 
  
  # Keep only ones with "keep"
  filter(
    keep == "keep"
  ) %>% 
  
  # Remove redundant columns
  select(
    - keep,
    - total.bs,
    - abundance
  ) %>% 
  
  mutate(
    
    ## Bodysize.measurement.notes ----
    # Remove the extra info from the bodysize.measurement column and put it into a new column called bodysize.measurement.notes
    
    bodysize.measurement.notes = stri_extract_first_regex(bodysize.measurement, "(?<=\\- ).*"), # Make notes column by taking the extra info from measurment column
    bodysize.measurement = stri_replace_all_regex(bodysize.measurement, " \\- .*", ""), # Remove extra info from measurment column
    bodysize.measurement = tolower(bodysize.measurement), # set everything to lower case to make easier
    
    # Group all the different measurement types into a standardized few
    bodysize.measurement = case_when(
      stri_detect_regex(bodysize.measurement, "length") ~ "length",
      stri_detect_regex(bodysize.measurement, "width") ~ "width",
      stri_detect_regex(bodysize.measurement, "diameter") ~ "diameter",
      stri_detect_regex(bodysize.measurement, "depth") ~ "depth",
      stri_detect_regex(bodysize.measurement, "height") ~ "height",
      
      TRUE ~ bodysize.measurement
    ),
    
    ## Units ----
    # Fix any mistakes in units and standadize them to all be the same
    
    # Change any mistakes or synonyms to the same
    units = case_when(
      units == "μg ind^-1" ~ "μg",
      units %in% c("fl/cell", "fl cell^-1") ~ "μm^3", # One femtoliter is the same as one micrometer cubed
      source.code == "16" ~ "mm", # The units were written wrong in the supplementary data, changed to units used in main paper
      
      TRUE ~ units
    ),
    
    # Convert all to μ so they are all the same
    body.size = case_when(
      units == "mg" ~ body.size*1000,
      units == "cm" ~ body.size*10,
      units == "nm" ~ body.size/1000,
      
      TRUE ~ body.size
    ),
    
    units = case_when(
      units == "mg" ~ "μg",
      units == "cm" ~ "mm",
      units == "nm" ~ "μm",
      
      TRUE ~ units
    ),
    
    ## life stage ----
    # Fix any mistakes in life.stage column and standardize to all be the same
    
    # Sort out capitals
    life.stage = tolower(life.stage),
    
    # Change all the different words for the life stages to either adult or juvenile
    life.stage = case_when(
      
      # Adults
      stri_detect_fixed(life.stage, "adult") ~ "adult",
      source.code %in% c("61", "263") & stri_detect_regex(life.stage, "C") ~ "adult",
      life.stage == "7 days" ~ "adult",
      is.na(life.stage) ~ "adult", # assume they are adults unless specified otherwise
      
      # Juvenile
      stri_detect_regex(life.stage, "(?i)copepodite|(?i)neonate|(?i)copepodid|(?i)juvenile|(?i)metamorphasis|(?i)nauplii|(?i)instar") ~ "juvenile",
      source.code %in% c("61", "263") & stri_detect_regex(life.stage, "(?i)n|(?i)c") ~ "juvenile",
      source.code == "5" & life.stage != "adult" ~ "juvenile",
      source.code == "66" ~ "juvenile",
      
      TRUE ~ life.stage
    ),
    
    ## Sample dates ----
    # When it is inputted as the full date format extract the year and month
    
    ### Extract full date ----
    #### Year ----
    
    # make the same type
    sample.start.year = as.character(sample.start.year),
    sample.end.year = as.character(sample.end.year),
    
    # Extract the year from the full date column
    sample.start.year = case_when(
      !is.na(sample.start.date.full) ~ stri_extract_first_regex(sample.start.date.full, "\\d{4}"),
      TRUE ~ sample.start.year
    ),
    
    sample.end.year = case_when(
      !is.na(sample.end.date.full) ~ stri_extract_first_regex(sample.end.date.full, "\\d{4}"),
      TRUE ~ sample.end.year
    ),
    
    #### Month ----
    # Extract the month from the full date column
    sample.start.month = case_when(
      
      # American style date
      !is.na(sample.start.date.full) & source.code == "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<!\\/)\\d+(?=\\/)"), # take the two digits that don't have a / infront but have on following it - dd/
      
      # Non-american style dates
      stri_detect_regex(sample.start.date.full, "\\/") & source.code != "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\/)\\d+(?=\\/)"), # take the two digits inbetween an / - /dd/
      stri_detect_regex(sample.start.date.full, "\\.") & source.code != "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\.)\\d+(?=\\.)"), # take the two digits inbetween an . - .dd.
      TRUE ~ sample.start.month
    ),
    
    sample.end.month = case_when(
      # American style date
      !is.na(sample.end.date.full) & source.code == "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<!\\/)\\d+(?=\\/)"), # take the two digits that don't have a / infront but have on following it - dd/
      
      # Non-american style dates
      stri_detect_regex(sample.end.date.full, "\\/") & source.code != "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\/)\\d+(?=\\/)"), # take the two digits inbetween an / - /dd/
      stri_detect_regex(sample.end.date.full, "\\.") & source.code != "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\.)\\d+(?=\\.)"), # take the two digits inbetween an . - .dd.
      TRUE ~ sample.end.month
    ),
    
    ### Ranges ----
    # Put the date into one column as a range
    
    ##### Year ----
    sample.year = case_when(
      is.na(sample.end.year) ~ sample.start.year, # when there is only one sample year use that one
      sample.start.year == sample.end.year ~ sample.start.year, # when the start and end year are the same just keep one
      sample.start.year != sample.end.year ~ paste(sample.start.year, sample.end.year, sep = "-"), # when it is over multiple years make a range
      TRUE ~ NA),
    
    #### Month ----
    sample.month = case_when(
      is.na(sample.end.month) ~ sample.start.month, # when there is only one sample year use that one
      sample.start.month == sample.end.month ~ sample.start.month, # when the start and end year are the same just keep one
      sample.start.month != sample.end.month ~ paste(sample.start.month, sample.end.month, sep = "-"), # when it is over multiple years make a range
      TRUE ~ NA),
  ) %>% 
  
  # Remove redundant columns
  select(
    -sample.start.date.full,
    -sample.end.date.full,
    -sample.start.month,
    -sample.start.year,
    -sample.end.month,
    -sample.end.year
  ) %>% 
  
  mutate(
    # change error type 
    error = as.numeric(error),
    
    ## nu ----
    # Edit weird form ones
    
    nu = if_else(
      nu == "coenobium",
      "colony",
      nu
    ),
    
    ## Form no ----
    # When the ind.per,nu is missing but the form is individual then set to 1
    
    ind.per.nu = case_when(
      !is.na(ind.per.nu) ~ ind.per.nu,
      nu == "individual" ~ 1,
      TRUE ~ NA
    ),
    
    ## Sample.size and reps ----
    # Change unknown to NA in reps and sample.size
    
    sample.size = if_else(
      sample.size == "unknown",
      NA,
      sample.size
    ),
    
    reps = if_else(
      reps == "unknown",
      NA,
      reps
  ), 
  
  ## Original.source.codes: ----
  # Change types to merge with db
  
  source.code = as.character(source.code),
  original.source.code.1 = as.character(original.source.code.1),
  original.source.code.2 = as.character(original.source.code.2),
  original.source.code.3 = as.character(original.source.code.3),
  original.source.code.4 = as.character(original.source.code.4),
  original.source.code.5 = as.character(original.source.code.5),
  original.source.code.6 = as.character(original.source.code.6),
  original.source.code.7 = as.character(original.source.code.7),
  original.source.code.8 = as.character(original.source.code.8),
  original.source.code.9 = as.character(original.source.code.9),
  original.source.code.10 = as.character(original.source.code.10),
  original.source.code.11 = as.character(original.source.code.11),
  original.source.code.12 = as.character(original.source.code.12),
  original.source.code.13 = as.character(original.source.code.13),
  original.source.code.14 = as.character(original.source.code.14),
  original.source.code.15 = as.character(original.source.code.15),
  original.source.code.16 = as.character(original.source.code.16)
  ) %>% 
  
  # Reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7,
           original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14,
           original.source.code.15, original.source.code.16, 
           sample.year, sample.month,
           join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
           join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type)

# Save
saveRDS(wos_formatted, file = "R/data_outputs/database_products/wos_formatted.rds")

