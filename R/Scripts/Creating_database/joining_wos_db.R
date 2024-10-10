## Combining DB and WOS

# Aim of this script
  # Combine WOS and DB dataframes into one
  # cross reference sources and remove any duplicates with the same taxa.name
  # add in location info

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
master_db_path <- here("Raw_data", "Master_db_traits.xlsx")

wos_raw <- readRDS("R/Data_outputs/databases/wos_formatted.rds")
db_raw <- readRDS("R/Data_outputs/databases/db_formatted.rds")
db_sources_raw <- read_xlsx(here("Raw_data","master_db_traits.xlsx"), sheet = "source_list")
wos_sources_raw <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "source_list")

# Join data
all_raw <- bind_rows(db_raw, wos_raw)

# Sources - Finding duplicates ----
## Join sources together ----

# Format so they are the same to join:
wos_sources <- wos_sources_raw %>% 
  # Select columns
  select(
    - list.name,
    - book.doi,
    - wos.uid,
    - authors.full.name
  ) %>% 
  
  # change paper.code name to match db_sources
  rename(
    citing.source = paper.code
  ) %>% 
  
  # change types to make easier to merge
  mutate(
    source.code = as.character(source.code),
    citing.source = as.character(citing.source)
  ) %>% 
  
  # set "NA" to NA
  mutate_all(
    ., ~na_if(., "NA")
  ) 

db_sources <- db_sources_raw %>% 
  # select columns
  select(
    - join.source,
    - book.doi
  ) %>% 
  
  # change paper.code name to match wos_sources
  rename(
    citing.source = db.code
  ) %>% 
  
  # change types to make mergeing easier
  mutate(
    volume = as.character(volume),
    issue = as.character(issue),
    start.page = as.character(start.page),
    end.page = as.character(end.page)
  )
  
# join together
all_source_raw <- bind_rows(db_sources, wos_sources) %>% 
  filter(
    !is.na(title) ### JUST FOR NOW WHILE DON'T HAVE RIMET_2013 SOURCES
  )

## DOI duplicates ----
# first check for duplicates among ones with dois 

# Make a dataframe of sources with dois
doi <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(doi)
  )

# make frequency table
doi_freq <- data.frame(table(doi$doi)) %>% 
  filter(
    Freq >1 # filter any that appear more than once
  )

# find the sources with multiple doi's in the all_source_raw dataframe
doi_dupes <- all_source_raw %>% 
  filter(
    doi %in% doi_freq$Var1 # select any that match the doi's in the doi_freq dataframe
  ) %>% 
  
  # make individual columns for each source.code for each duplicate doi
  group_by(doi) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2", "source.code.3"), sep = ",") %>% 
  
  # rename so can merge with others later
  rename(
    source.info = doi,
  ) %>% 
  
  # add in type so knows what was used to find the duplicates
  mutate(
    duplcate.type = "doi"
  )

## isbn duplicates ----
### DECIDE WHAT TO DO WITH ONES WITH ISBN AND DOIS ###
# check for duplicate isbn for ones without dois

# select ones with isbns
isbn <- all_source_raw %>%
  filter(
    !is.na(ISBN)
  )

# make frequency table
isbn_freq <- data.frame(table(isbn$ISBN)) %>% 
  filter(
    Freq > 1 # filter any that appear more than once
  )
  
# find the sources with multiple doi's in the all_source_raw dataframe
isbn_dupes <- all_source_raw %>% 
  filter(
    ISBN %in% isbn_freq$Var1 # select any that match the doi's in the doi_freq dataframe
  ) %>% 
  
  # make individual columns for each source.code for each duplicate doi
  group_by(ISBN) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2", "source.code.3"), sep = ",") %>% 
  
  # rename so can merge with others later
  rename(
    source.info = ISBN,
  ) %>% 
  
  # add in type so knows what was used to find the duplicates
  mutate(
    duplcate.type = "isbn"
  )

## Title duplicates ----
# for any that don't have isbn or doi check for duplicate titles

# select ones without isbn or doi
title <- all_source_raw %>% 
  filter(
    is.na(ISBN) & is.na(doi)
  )

# make frequency table
title_freq <- data.frame(table(title$title)) %>% 
  filter(
    Freq > 1
  )

# find sources with those titles in the all all_source_raw dataframe
title_dupes <- all_source_raw %>% 
  filter(
    title %in% title_freq$Var1 # select any that match the doi's in the doi_freq dataframe
  ) %>% 
  
  # make individual columns for each source.code for each duplicate doi
  group_by(title) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2"), sep = ",") %>% 
  
  # rename so can merge with others later
  rename(
    source.info = title,
  ) %>% 
  
  # add in type so knows what was used to find the duplicates
  mutate(
    duplcate.type = "title"
  )

## Combine all ----
all_dupes <-  bind_rows(doi_dupes, isbn_dupes, title_dupes)

# Source - removing duplicates ----
# remove any duplicated data from the raw data












