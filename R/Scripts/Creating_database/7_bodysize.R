## Calculating the body size for ones I don't have it for
# all volum = um3
# all length, width and height = um,
# all mass = ug

# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)

# Data ----
bodysize_location <- readRDS("R/Data_outputs/full_database/bodysize_location.rds")
bodysize_taxonomy <- readRDS("R/Data_outputs/full_database/bodysize_taxonomy.rds")
tax_list_distinct <- readRDS("R/Data_outputs/taxonomy/gbif/tax_list_distinct.rds")
source_list <- readRDS("R/Data_outputs/locations_sources/source_list.rds")

# All data ----
# Final list of all data including phyto and zooplankton

format_all_bs <- bodysize_location %>% 
  
  mutate(
    
    ## Small misc edits ----
    
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
    
    # set form number for missing one
    form = if_else(
      uid == "321726",
      "individual",
      form
      )
    ) %>%
  
  # don't need units because each measurement type is the same now
  select(
    - units
  ) %>% 
  
  ## nu column ----
  # rename form and form.no columns to nu and set so either individual or multi-cellular
  rename(
    ind.per.nu = form.no,
    nu = form
  ) %>% 
  
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
    tax.uid, rank, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes
  )

# Convert raw to averages ----

## get location info to left join to bs_raw ----
location_info <- format_all_bs %>% 
  select(
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  distinct(
    location.code, .keep_all = TRUE
  )

## Calculate average ----
bs_raw <- format_all_bs %>% 
  
  # Select raw data
  filter(
    measurement.type == "raw"
  ) %>% 
  
  # Group by all the below factors so that it gives average for different locations and dates
  group_by(
    tax.uid, source.code, original.sources, location.code, bodysize.measurement, life.stage, sex, sample.year, sample.month, nu
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
  ) %>% 
  
  left_join(
    tax_list_distinct, by = "tax.uid"
  ) %>% 
  
  left_join(
    select(
      source_list, doi, new.source.code
    ), by = c("source.code" = "new.source.code")
  ) %>% 
  
  # Make new individual.uid
  group_by(
    tax.uid, source.code, original.sources, location.code, life.stage, sex, sample.year, sample.month, nu
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
    form = nu,
    
    # change tyes for later merging
    sample.size = as.character(sample.size),
    error = as.character(error)
  ) %>% 
  
  ungroup() %>% 
  
  # Select and relocate
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    tax.uid, rank, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes
  )

## Combine all together ----
all_bodysize <- format_all_bs %>% 
  
  # remove raw data
  filter(
    !(measurement.type == "raw")
  ) %>% 
  
  # add in the new averages of raw data
  bind_rows(
    bs_raw
  )

## save ----
saveRDS(all_bodysize, file = "R/Data_outputs/full_database/all_bodysize.rds")

# Phyto ----
# select just phytoplankton and do minor edits

phyto <- all_bodysize %>% 
  
  mutate(
    # set depth in bodysize.measurment as height as there is no overlap between these
    bodysize.measurement = if_else(
      bodysize.measurement == "depth",
      "height",
      bodysize.measurement
    )
  ) %>% 
  
  # Select data I want
  filter(
    type == "Phytoplankton",
    life.stage %in% c("adult", "active") # select just active and not dormant
  ) %>% 
  
  # Rename from body to cell
  rename(
    cell.size = body.size,
    cells.per.nu = ind.per.nu
  ) %>% 
  
  mutate(
    nu = if_else(
      nu == "individual",
      "cell",
      nu
    )
  ) %>% 
  
  select(
    # don't need life.stage and sex for phyto as all active and don't have sex info
    - life.stage,
    - sex,
    
    # don't have error data for majority so leaving it out
    - error,
    - error.type,
    - reps,
    - sample.size,
    
    - measurement.type, # all average now
    - bodysize.measurement.notes, # not needed
    - type # all phyto
  )

# Cell mass ----
# Calculate cell mass

## Separate volumes ----
# find the mld for each individual and calculate body.mass

species_raw_cell_size <- phyto %>% 
  
  # Get all the different bodysize.measurements on one row per individual
  pivot_wider(
    id_cols = individual.uid,
    names_from = bodysize.measurement,
    values_from = cell.size
  ) %>%
  
  rename(
    dry.mass = `dry mass`
  ) %>% 
  
  # join all extra data back 
  left_join(
    select(
      phyto, - cell.size, bodysize.measurement, - uid
      ), by = "individual.uid"
  ) %>% 
  
  distinct(
    individual.uid, .keep_all = TRUE
  ) %>% 
  
  # calculate mass from biovolume and dry mass
  mutate(
    mass = case_when(
      !is.na(biovolume) ~ biovolume*(1*10^-6),
      is.na(biovolume) & !is.na(dry.mass) ~ dry.mass/0.2,
      TRUE ~ NA
    ),
    
    # make a mld column
    mld = pmax(length, width, height, diameter, na.rm = TRUE)
  ) %>% 
  
  # select columns and reorder
  select(
    individual.uid, source.code, original.sources, taxa.name,
    nu, cells.per.nu, mass, biovolume, mld,
    tax.uid, rank, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  filter(
    rank == "Species"
  )

# save
saveRDS(species_raw_cell_size, file = "R/Data_outputs/full_database/species_raw_cell_size.rds")


