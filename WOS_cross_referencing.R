## Cross referencing the sources from the the WOS secondary data list with the primary data list

## packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## data
original_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "secondary_data_original")

## Check if any of the sources in the secondary data list have referenced other in that list
within_referenced <- original_sources %>% 
  group_by(source) %>% 
  mutate(
    referenced = ifelse(
      n()>1, "yes", "no"
    )
  )
