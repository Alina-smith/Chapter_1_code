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


# Format data ----
bodysize_data <- bodysize_sources %>% 
  
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
