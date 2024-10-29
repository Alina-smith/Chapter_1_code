## Calculating the body size for ones I don't have it for

# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)

# Data ----
bodysize_sources <- readRDS("R/Data_outputs/full_database/bodysize_sources.rds")
taxonomy_list <- read.csv("R/Data_outputs/full_database/taxonomy_list.rds")
rotifer <- read_xlsx("Raw_data/master_length_weight.xlsx", sheet = "McCauley rotifers")
b_pooled <- read_xlsx("Raw_data/master_length_weight.xlsx", sheet = "Bottrell pooled")

# List of multiple measurment types ----
# make a list of all individual uids that has more than one measurement type

multiple_measurements <- bodysize_sources %>% 
  
  # merge the measurment types into groups to help with working out when theres a biovolumne and a dimension measurment
  mutate(
    bs.check = case_when(
      bodysize.measurement == "biovolume" ~ "biovolume",
      bodysize.measurement %in% c("length", "height", "width", "depth", "diameter") ~ "dimension",
      bodysize.measurement %in% c("dry mass", "wet mass") ~ "mass",
      TRUE ~ "other"
    )
  ) %>% 
  
  # group by individual.uid to check for multiple measurment types within each indicidual
  group_by(
    individual.uid
  ) %>% 
  
  # get distinct measurment types 
  distinct(
    bs.check
  ) %>% 
  
  # count how many measurment types per individual
  mutate(
    bs.check.2 = n()
  ) %>% 
  
  # select ones that are more than one
  filter(
    bs.check.2 > 1
  )


# Format data ----
bodysize_data <- bodysize_sources %>% 
  
  ## Final changes to format ----
  # making any final adjustments that were missed in the past steps
  mutate(
    
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
    ),
    
    # calculate min max
    # for nay that still have only min and max and not body size calculate this
    body.size = case_when(
      is.na(body.size) & !is.na(min.body.size) & !is.na(max.body.size) ~ (min.body.size + max.body.size)/2,
      is.na(body.size) & is.na(min.body.size) & !is.na(max.body.size) ~ max.body.size,
      is.na(body.size) & !is.na(min.body.size) & is.na(max.body.size) ~ min.body.size,
      TRUE ~ body.size
      )
    ) %>% 
  
  ## Remove multple measurments
  # remove dimension measurements when there is already a biovolume
  # merge them together again to make easier
  mutate(
    bs.type = case_when(
      bodysize.measurement == "biovolume" ~ "biovolume",
      bodysize.measurement %in% c("length", "height", "width", "depth", "diameter") ~ "dimension",
      bodysize.measurement %in% c("dry mass", "wet mass") ~ "mass",
      TRUE ~ "other"
    ),
    
    # make a column and assign yes if there is only one measurement or multiple measurements but it is biovolume or mass and no if it is multiple measurments and a dimension
    keep = case_when(
      individual.uid %in% multiple_measurements$individual.uid & bs.type == "dimension" ~ "no",
      individual.uid %in% multiple_measurements$individual.uid & bs.type == "biovolume" ~ "yes",
      individual.uid %in% multiple_measurements$individual.uid & bs.type == "mass" ~ "yes",
      TRUE ~ "yes"
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
