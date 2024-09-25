## joining full source info to original sources

#### Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)
library(data.table)

#### Data ---- 
sources_shortlist <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "sources_shortlist")
full_info <- read_xlsx(here("Raw_data","original_source_full_info.xlsx"), sheet = "original_source_full")

together <- left_join(sources_shortlist, full_info, by = "doi")

write_csv(together, "Raw_data/together.csv")
