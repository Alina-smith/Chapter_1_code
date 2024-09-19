## standadising the WOS data dump

# Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## Data 
wos_raw_body <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "bodysize_data")

wos_data <- wos_raw_body %>%
  mutate(
    ## body size measurments
    # seperate out the body size measurement method into the method and then a column with additional notes on the method like what is measured etc
    body.size.method.notes = stri_extract_first_regex(body.size.method, "(?<=\\- ).*"),
    body.size.method = stri_replace_all_regex(body.size.method, " \\- .*", ""),
    
    # fix spelling mistakes
    body.size.method = stri_replace_all_regex(body.size.method, "B", "b",),
    
    ## units
    # change any mistakes or synonyms to the same
    units = case_when(
      units == "μg ind^-1" ~ "μg",
      units %in% c("fl/cell", "fl cell^-1") ~ "μm^3", # one femtoliter is the same as one micrometer cubed
      source.code == "16" ~ "mm", # the units were written wrong in the supplementary data, changed to units used in main paper
      TRUE ~ units
    ),
    
    # standardize to the same units
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
    )
  )

early_graph <- wos_data %>% 
  mutate(
    filter = case_when(
      body.size.method == "biovolume" ~ "phyto",
      body.size %in% c()
    )
  )
  
  filter(
    body.size.method == "biovolume" ~ "phyto",
    body.size
  )






sixteen <- wos_data %>% 
  filter(source.code == "16")

units <- wos_data %>% 
  distinct(units)

nm <- wos_data %>% 
  filter(units == "nm")

mg <- wos_data %>% 
  filter(units == "mg")

cm <- wos_data %>% 
  filter(units == "cm")

mm <- wos_data %>% 
  filter(units == "mm")

mm_type <- mm %>% 
  distinct(body.size.method)

    # edit the names
    body.size.method = case_when(
      body.size.method %in% c("body diameter", "cell diameter") ~ "diameter",
      body.size.method %in% c("body length", "cell length") ~ "diameter",
      TRUE ~ body.size.method
    )
  )

method <- wos_data %>% 
  distinct(body.size.method)

