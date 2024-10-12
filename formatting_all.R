## Formatting all data

## remove measurments when they already have biovolume
## calculate biovolume when only have dimensions 

# Packages ----
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

# Data ----
#set relative file paths
all_raw <- readRDS("R/Data_outputs/databases/raw_body_size.rds")

body_size_filtered <- all_raw %>% 
  
  ## filter out dimension measurments when biovolume is given within the same source
  group_by(source.code, original.taxa.name, bodysize.measurement) %>% 
  summarise(
    count = n(), .groups = 'drop'
  )
