## Calculating the body size for ones I don't have it for

# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)

# Data ----
bodysize_location <- readRDS("R/Data_outputs/full_database/bodysize_location.rds")
b_pooled <- read_xlsx("Raw_data/master_length_weight.xlsx", sheet = "Bottrell pooled")

# Removing multiple measurements ----
# Some individuals have biovolume and dimension or length and mass measurements so want o keep just the biovolume or mass ones in these cases

# make a list of all individual uids that has more than one measurement type to use in next step to remove datapoints I don't want
multiple_measurements <- bind_rows(db_raw, wos_raw) %>% 
  
  # merge the measurment types into groups to help with working out when theres a biovolumne and a dimension measurment
  mutate(
    bs.check = case_when(
      bodysize.measurement == "biovolume" ~ "biovolume",
      bodysize.measurement %in% c("length", "height", "width", "depth", "diameter") ~ "dimension",
      bodysize.measurement %in% c("dry mass", "wet mass") ~ "mass",
      TRUE ~ "other"
    )
  ) %>% 
  
  # group by individual.uid to check for multiple measurement types within each individual
  group_by(
    individual.uid
  ) %>% 
  
  # get distinct measurement types so that only looking for multiple measurement types not multiple measurements
  distinct(
    bs.check
  ) %>% 
  
  # count how many measurement types per individual
  mutate(
    bs.check.2 = n()
  ) %>% 
  
  # select ones that are more than one
  filter(
    bs.check.2 > 1
  )

# Format data ----
bodysize_data <- bodysize_sources %>% 
  
  ## Remove multiple measurements ----
  # remove dimension measurements when there is already a biovolume or mass

  # merge measurement.type together like in removing multiple measurements step above to make easier
  mutate(
    bs.type = case_when(
      bodysize.measurement == "biovolume" ~ "biovolume",
      bodysize.measurement %in% c("length", "height", "width", "depth", "diameter") ~ "dimension",
      bodysize.measurement %in% c("dry mass", "wet mass") ~ "mass",
      TRUE ~ "other"
    ),
  
  # When the individual.uid is in the multiple_measurements individual.uid then assign each datapoint either yes to keep if it is biovolume or mass and no to remove if a dimension
  keep = case_when(
    individual.uid %in% multiple_measurements$individual.uid & bs.type == "dimension" ~ "no",
    individual.uid %in% multiple_measurements$individual.uid & bs.type == "biovolume" ~ "yes",
    individual.uid %in% multiple_measurements$individual.uid & bs.type == "mass" ~ "yes",
    TRUE ~ "yes" # ones that aren't multiple measurements to keep
    )
  ) %>% 
  
  # filter out ones I don't want
  filter(
    keep == "yes"
  ) %>% 
  
  select(
    -keep,
    -bs.type
  ) %>% 
  
  ## Final changes to format ----
  # making any final adjustments that were missed in the past steps
  mutate(
    
    # Convert units
    # all dimentions to mm and mass to ug NEED TO CHANGE WHEN CLACLATING THE BIOVOLUME FOR ONES WITH DIMENTSIONA
    body.size = 
      case_when(
        units == "µm" ~ body.size/1000,
        units == "mg" ~ body.size*1000,
        TRUE ~ body.size
      ),
    
    units = case_when(
      units == "µm" ~ "mm",
      units == "mg" ~ "µg",
      TRUE ~ units
      )
    ) %>% 
  
  ## convert to mass 
  left_join(
    select(
      taxonomy_list, genus, family, order, class, phylum, kingdom, tax.uid
      ), by = "tax.uid"
  ) %>% 
  
  mutate(
    # convert from dry to wet mass
    body.size = case_when(
      bodysize.measurement == "dry mass" ~ body.size/(1-0.90),
      TRUE ~ body.size
    ),
    
    bodysize.measurement = case_when(
      bodysize.measurement == "dry mass" ~ "wet mass",
      TRUE ~ bodysize.measurement
    ),
    
    # log the zooplankton data for the equations
    body.size.eq = case_when(
      bodysize.measurement == "wet mass" ~ body.size,
      bodysize.measurement == "biovolume" ~ body.size,
      genus == "Bosmina" & bodysize.measurement == "length" ~ log(body.size),
      genus == "Daphnia" & bodysize.measurement == "length" ~ log(body.size),
      family == "Daphniidae" & bodysize.measurement == "length" ~ log(body.size),
      order == "Diplostraca" & bodysize.measurement == "length" ~ log(body.size),
      class == "Copepoda" & bodysize.measurement == "length" ~ log(body.size),
      TRUE ~ body.size
    ),
    
    mass = case_when(
      bodysize.measurement == "wet mass" ~ body.size.eq,
      bodysize.measurement == "biovolume" ~ body.size.eq*(1*10^-6),
      genus == "Bosmina" & bodysize.measurement == "length" ~ 3.0896 + (3.0395 * body.size.eq),
      genus == "Daphnia" & bodysize.measurement == "length" ~ 1.4681 + (2.8292 * body.size.eq),
      family == "Daphniidae" & bodysize.measurement == "length" ~ 1.5072 + (2.7610 * body.size.eq),
      order == "Diplostraca" & bodysize.measurement == "length" ~ 1.7512 + (2.6530 * body.size.eq),
      class == "Copepoda" & bodysize.measurement == "length" ~ 1.9526 + (2.3990 * body.size.eq),
      TRUE ~ NA
    ),
    
    # delog
    mass = case_when(
      genus == "Bosmina" & bodysize.measurement == "length" ~ exp(mass),
      genus == "Daphnia" & bodysize.measurement == "length" ~ exp(mass),
      family == "Daphniidae" & bodysize.measurement == "length" ~ exp(mass),
      order == "Diplostraca" & bodysize.measurement == "length" ~ exp(mass),
      class == "Copepoda" & bodysize.measurement == "length" ~ exp(mass),
      TRUE ~ mass
    ),
    
    # convert mass from length weight to wet mass 
    mass = case_when(
      genus == "Bosmina" & bodysize.measurement == "length" ~ mass/(1-0.90),
      genus == "Daphnia" & bodysize.measurement == "length" ~ mass/(1-0.90),
      family == "Daphniidae" & bodysize.measurement == "length" ~ mass/(1-0.90),
      order == "Diplostraca" & bodysize.measurement == "length" ~ mass/(1-0.90),
      class == "Copepoda" & bodysize.measurement == "length" ~ mass/(1-0.90),
      TRUE ~ mass
    )
  ) %>% 
  
  select(
    - body.size.eq
  ) %>% 
  
  ## Averages for multiple indivdual.uid measurments
  # some papers gave averages for each individual and some gave raw so get averages for ones that are raw
  group_by(
    individual.uid
  ) %>% 
  
  mutate(
    mass = mean(mass)
  ) %>% 
  
  distinct(individual.uid, .keep_all = TRUE) %>% 
  
  ungroup() %>% 
  
  mutate(
    group = case_when(
      bodysize.measurement == "biovolume" ~ "phytoplankton",
      TRUE ~ "zooplankton"
    )
  ) %>% 
  
  relocate(
    uid, individual.uid, tax.uid, source.code, original.sources, accepted.taxa.name, form, form.no, life.stage, sex, mass, min.body.size, max.body.size, body.size, units, bodysize.measurement, sample.size, error, error.type, sample.year, sample.month
  )

# save
saveRDS(bodysize_data, file = "R/Data_outputs/full_database/bodysize_data.rds")
