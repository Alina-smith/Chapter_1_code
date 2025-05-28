## Calculating the body sizes using length-weight and biovolume conversions
# Have decided to go to genus level as this provides the most data points and locations compared to species level

# all volume = um3
# all length, width and height = um,
# all mass = ug

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)

# Formatting all data ----
# Final list of all data including phyto and zooplankton with groups and mass

## Import data ----
bodysize_location <- readRDS("R/Data_outputs/database_products/bodysize_location.rds")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")
sources_list_update <- readRDS("R/Data_outputs/database_products/sources_list_update.rds")

## Initial edits ----

format_all_bs <- bodysize_location %>% 
  
  mutate(
    
    # Small misc edits
    
    # change types for later merging
    uid = as.character(uid),
    
    # change measurement type to single when its raw but only has one value because going to get averages of raw data later on so don't want these ones in there
    measurement.type = if_else(
      uid %in% c("340977", "341039", "343115", "338767", "338672", "338649"),
      "single",
      measurement.type
    ),
    
    # assume all na life stage are adults
    life.stage = if_else(
      is.na(life.stage),
      "adult",
      life.stage
    ),
    
    # Convert all units to be the same to make it easier
    body.size = case_when(
      units %in% c("mg", "mm") ~ body.size*1000,
      TRUE ~ body.size
    ),
    
    units = case_when(
      units == "mg" ~ "µg",
      units == "mm" ~ "µm",
      TRUE ~ units
    ),
    
    # set nu for missing one
    nu = if_else(
      uid == "321726",
      "individual",
      nu
    ), 
    
    # change life stage so all phyto are either active or dormant
    life.stage = case_when(
      type == "Phytoplankton" & life.stage == "adult" ~ "active",
      type == "Phytoplankton" & life.stage == "juvenile" ~ "dormant",
      type == "Zooplankton" & life.stage == "active" ~ "adult",
      type == "Zooplankton" & life.stage == "dormant" ~ "juvenile",
      TRUE ~ life.stage
    )
  ) %>%
  
  # don't need units because each measurement type is the same now
  select(
    - units
  ) %>% 
  
  # nu column
  
  mutate(
    nu = case_when(
      ind.per.nu > 1 ~ "multi-cellular",
      ind.per.nu == 1 ~ "individual",
      
      nu %in% c("multi-cell", "multi-cellular", "colony", "filament") ~ "multi-cellular",
      nu == "individual" ~ "individual",
      TRUE ~ NA
    )
  ) %>% 
  
  # select and relocate columns
  
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    ott.id, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes
  )





## Calculate averages for each source ----
# Have error values for some but not the majority so will be taking an average of averages

# get location info to left join to bs_raw
location_info <- format_all_bs %>% 
  select(
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  distinct(
    location.code, .keep_all = TRUE
  )

# Calculate average
bs_raw <- format_all_bs %>% 
  
  # Select raw data
  filter(
    measurement.type == "raw"
  ) %>% 
  
  # Group by all the below factors so that it gives average for different locations and dates
  group_by(
    ott.id, source.code, original.sources, location.code, bodysize.measurement, life.stage, sex, sample.year, sample.month, nu
  ) %>% 
  
  # Calculate mean
  summarise(
    body.size.mean = mean(body.size),
    sample.size = n(),
    sd = sd(body.size),
    error = sd/sqrt(sample.size),
    .groups = "drop"
  ) %>% 
  
  rename(
    body.size = body.size.mean
  ) %>% 
  
  ungroup() %>% 
  
  # Join back in extra info
  left_join(
    location_info, by = "location.code"
  )%>% 
  
  left_join(
    tax_list_raw, by = "ott.id"
  ) %>% 
  
  left_join(
    select(
      sources_list_update, doi, source.code, new.source.code
    ), by = c("source.code" = "new.source.code")
  ) %>% 
  
  # Make new individual.uid
  group_by(
    ott.id, source.code, original.sources, location.code, life.stage, sex, sample.year, sample.month, nu
  ) %>% 
  
  mutate(
    individual.uid = paste(doi, cur_group_id(), sep = "-")
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    
    # Add in misc extra info
    uid = paste("r", row_number(), sep = "-"),
    ind.per.nu = 1, # all individuals
    reps = NA,
    error.type = "se",
    measurement.type = "average",
    bodysize.measurement.notes = NA,
    
    # change tyes for later merging
    sample.size = as.character(sample.size),
    error = as.character(error)
  ) %>% 
  
  ungroup() %>% 
  
  # Select and relocate
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    ott.id, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes
  )

## Combine all together ----
bodysize_groups <- format_all_bs %>% 
  
  # remove raw data
  filter(
    !(measurement.type == "raw")
  ) %>% 
  
  # add in the new averages of raw data
  bind_rows(
    bs_raw
  ) %>% 
  
  # remove columns that aren't needed
  select(
    - measurement.type # all average now
  ) %>% 
  
  # Calculate average for ones that are the same individual in the same location and time
  group_by(
    individual.uid, bodysize.measurement,life.stage
  ) %>% 
  
  mutate(
    body.size = mean(body.size)
  ) %>% 
  
  distinct(
    body.size, .keep_all = TRUE
  ) %>% 
  
  ungroup() %>% 
  
  # Add in traditional fg info
  
  mutate(
    group = case_when(
      # Phyto
      phylum == "Cyanobacteria" ~ "Blue/green",
      phylum == "Ochrophyta" ~ "Stramenopile",
      phylum == "Glaucophyta" ~ "Glaucophyte",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Green",
      phylum == "Bacillariophyta" ~ "Diatom",
      phylum == "Euglenozoa" ~ "Euglenoid",
      phylum == "Cryptophyta" ~ "Cryptomonad",
      phylum == "Haptophyta" ~ "Haptophyte",
      phylum == "Myzozoa" ~ "Dinoflagellate",
      
      # Zoo
      class == "Branchiopoda" ~ "Cladoceran",
      class == "Hexanauplia" ~ "Copepod",
      phylum == "Rotifera" ~ "Rotifer",
      phylum == "Ciliophora (phylum in subkingdom SAR)" ~ "Ciliate",
      class == "Ostracoda" ~ "Ostracod",
      
      TRUE ~ NA
      
    ),
    
    # assign zooplankton to groups for length-weight
    lw.group = case_when(
      
      taxa.name == "Bosmina" ~ "bosmina",
      taxa.name == "Daphnia" ~ "daphnia",
      family == "Daphniidae" ~ "daphniidae",
      order == "Diplostraca" ~ "cladocera",
      phylum == "Rotifera" ~ "rotifer",
      phylum == "Arthropoda" ~ "copepoda",
      phylum == "Ciliophora (phylum in subkingdom SAR)" ~ "ciliate",
      
      TRUE ~ NA
    ) 
  ) %>% 
  
  # Filter to genus level - chose genus as this gives the most data compared to species
  filter(
    !is.na(genus)
  ) %>% 
  
  select(
    - species, -taxa.name
  ) %>% 
  
  # replace taxa name with the genera
  rename(
    taxa.name = genus
  )