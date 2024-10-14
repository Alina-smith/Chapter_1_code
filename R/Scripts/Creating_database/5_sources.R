## Fitering out duplicate sources from raw data

# Aim of this script
  # Combine the WOS and DB source lists
  # Find any sources that are duplicates but have different source codes by finding ones with matching, DOI, ISBN or titles
  # Remove any data points from the bodysize_joined data that is in a secondary data paper but has been taken from a primary source
    # to do this I need to only remove matching original.taxa.names otherwise if i just remove all data.points with matching 

# Packages ----
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

# Data ----
bodysize_joined <- readRDS("R/Data_outputs/databases/bodysize_joined.rds") 
db_sources_raw <- read_xlsx(here("Raw_data","master_db_traits.xlsx"), sheet = "source_list")
wos_sources_raw <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "source_list")

# Join sources together ----

## Format so they are the same to join ----
# WOS:
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

# DB:
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
  
## Join together ----
all_source_raw <- bind_rows(db_sources, wos_sources) %>% 
  mutate_all(
    # set NA to na
    ~ na_if(., "NA")
    ) %>% 
  mutate(
    # set title to lower case to help with finding duplicates
    title = tolower(title)
  )

# Finding duplicates: ----
# want to find any sources that are either duplicated within source.codes or are duplicated between source.codes - one of the secondary data papers has used data from one of the primary data sources that I already have

## Within source dupes - DOI ----
within_dupe_doi <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(doi)
  ) %>%
  group_by(citing.source, doi) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  ) %>% 
  # rename doi for merging later
  rename(
    dupe.type = doi
  )

## Within source dupes - ISBN ----
within_dupe_isbn <- all_source_raw %>% 
  # select ones with isbn
  filter(
    !is.na(ISBN)
  ) %>%
  group_by(citing.source, ISBN) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  ) %>%   # rename doi for merging later
  rename(
    dupe.type = ISBN
  )
  
## Within source dupes - title ----
within_dupe_title <- all_source_raw %>% 
  # select ones with no doi or isbn
  filter(
    is.na(doi) & is.na(ISBN)
  ) %>%
  # make all lower case incase some are caps
  mutate(
    title = tolower(title)
  ) %>% 
  group_by(citing.source, title) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  ) %>% 
  # rename doi for merging later
  rename(
    dupe.type = title
  )

## Within source dupes - Merge ----
within_dupes <- bind_rows(within_dupe_doi, within_dupe_isbn, within_dupe_title)
# gave two duplicates but one is the title of the same lake monitoring program but different years so already has different source.codes and the other had different join.codes for joining the original.source code onto the db data but has the same original.source code so when the final source list is made by getting all the unique source codes this will be fine

## Between source dupes - DOI ----
# find any DOIS that appear more than once by making a frequency table
between_dupe_doi_freq <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(doi)
  ) %>%
  group_by(doi) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  ) %>% 
  # add in a type to make it easier for later steps
  mutate(
    source.type = "doi"
  )
  
## Between source dupes - ISBN ----
# find any ISBNs that appear more than once by making a frequency table
between_dupe_isbn_freq <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(ISBN)
  ) %>%
  group_by(ISBN) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  ) %>% 
  # add in a type to make it easier for later steps
  mutate(
    source.type = "isbn"
  )

## Between source dupes - title ----
# find any titles that appear more than once by making a frequency table
between_dupe_title_freq <- all_source_raw %>% 
  # select ones with doi
  filter(
    is.na(doi),
    is.na(ISBN)
  ) %>%
  # make all lower case incase some are caps
  mutate(
    title = tolower(title)
  ) %>% 
  group_by(title) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1,
    
    # remove the süsswasserflora as this appears twice but with the same source code so it's fine 
    title != "süsswasserflora von mitteleuropa"
  ) %>% 
  # add in a type to make it easier for later steps
  mutate(
    source.type = "title"
  )

## Between source dupes - Merge together ----
# 1) Merge ISBN, DOI and title together
# 2) Get source codes from the main source list that match the dois, isbns or titles
# 3) make into a table so that each source code is on it's own line with two columns 1) the source code I want to find and replace, 2) the source code I want to replace it with
# this will allow me to then change all the duplicate sources with different source codes in the bodysize_raw dataframe to have the same source code
between_dupe_all <- bind_rows(between_dupe_doi_freq, between_dupe_isbn_freq, between_dupe_title_freq) %>% 
  mutate(
    # put the source identifier into one column
    duplicate.source = case_when(
      source.type == "doi" ~ doi,
      source.type == "isbn" ~ ISBN,
      source.type == "title" ~ title
    )
  ) %>% 
  
  # remove redundant columns
  select(
    -doi,
    -ISBN,
    -title
  ) %>% 
  
  # add in the source codes and merge into one column
  left_join(
    select(all_source_raw, source.code, doi), by = c('duplicate.source' = "doi")
  ) %>% 
  left_join(
    select(all_source_raw, source.code, ISBN), by = c('duplicate.source' = "ISBN")
  ) %>% 
  left_join(
    select(all_source_raw, source.code, title), by = c('duplicate.source' = "title")
  ) %>% 

  mutate(
    source.code = case_when(
      !is.na(source.code) ~ source.code,
      !is.na(source.code.x) ~ source.code.x,
      !is.na(source.code.y) ~ source.code.y,
    )
  ) %>% 
  
  select(
    - source.code.x,
    - source.code.y
  ) %>% 
  
  # make individual columns for each source.code for each duplicate title
  group_by(duplicate.source) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2", "source.code.3"), sep = ",") %>% 
  
  # remove redundant columns 
  select(
    -duplicate.source
  ) %>% 
  
  # pivot longer so that can have a column for the source code i want to keep and then the source codes i want to chnage to the source.code i want to keep becaus there is more than one duplicate needs to be done this way
  pivot_longer(
    cols = source.code.2:source.code.3, values_to = "source.code.change"
  ) %>% 
  
  # remove redundant columns
  select(
    -name
  ) %>% 
  
  # rename to make it easier to understand
  rename(
    source.code.keep = source.code.1
  ) %>% 
  
  # remove NA
  filter(
    !is.na(source.code.change)
  )












# filter out duplicate sources from bodysize_raw ----
bodysize_raw <- bodysize_raw %>% 
  # change duplicate sources with different codes to the same code
  # original.source.code.1
  left_join(
    between_dupe_all, by = c("original.source.code.1" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.1 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.1
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.2
  left_join(
    between_dupe_all, by = c("original.source.code.2" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.2 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.2
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.3
  left_join(
    between_dupe_all, by = c("original.source.code.3" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.3 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.3
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.4
  left_join(
    between_dupe_all, by = c("original.source.code.4" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.4 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.4
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.5
  left_join(
    between_dupe_all, by = c("original.source.code.5" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.5 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.5
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.6
  left_join(
    between_dupe_all, by = c("original.source.code.6" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.6 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.6
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.7
  left_join(
    between_dupe_all, by = c("original.source.code.7" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.7 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.7
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.8
  left_join(
    between_dupe_all, by = c("original.source.code.8" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.8 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.8
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.9
  left_join(
    between_dupe_all, by = c("original.source.code.9" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.9 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.9
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.1
  left_join(
    between_dupe_all, by = c("original.source.code.10" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.10 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.10
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.1
  left_join(
    between_dupe_all, by = c("original.source.code.11" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.11 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.11
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.12
  left_join(
    between_dupe_all, by = c("original.source.code.12" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.12 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.12
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.13
  left_join(
    between_dupe_all, by = c("original.source.code.13" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.13 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.13
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.14
  left_join(
    between_dupe_all, by = c("original.source.code.14" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.14 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.14
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.15
  left_join(
    between_dupe_all, by = c("original.source.code.15" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.15 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.15
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.16
  left_join(
    between_dupe_all, by = c("original.source.code.16" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.16 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.16
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.17
  left_join(
    between_dupe_all, by = c("original.source.code.17" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.17 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.17
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) %>% 
  
  # original.source.code.18
  left_join(
    between_dupe_all, by = c("original.source.code.18" = "source.code.change")
  ) %>% 
  
  mutate(
    original.source.code.18 = if_else(
      !is.na(source.code.keep),
      source.code.keep,
      original.source.code.18
    )
  ) %>% 
  
  select(
    - source.code.keep
  ) 
  

y <- bodysize_filtered %>% 
  
  # select columns I want
  select(
    original.source.code.1:original.source.code.18,
    individual.uid,
    original.taxa.name,
    source.code
  ) %>% 
  
  # pivot longer to get each original.source.code on it's own row
  pivot_longer(
    cols = original.source.code.1:original.source.code.18, values_to = "original.source.code"
  ) %>% 
  
  # remove NA
  filter(
    !is.na(original.source.code)
  ) %>% 
  
  # make a new column with the original.source.code and taxa.name pasted together
  mutate(
    duplicate.check = paste(original.source.code, original.taxa.name, sep = "-")
  ) %>% 
  
  # get distinct taxa names for each source so that when finding duplicates it doesn't give duplicates within the same sources
  group_by(source.code) %>% 
  distinct(original.taxa.name, .keep_all = TRUE) %>% 
  ungroup() %>%  # ungroup to group again later
  
  # Make a frequency table of all measurements that have come from duplicate sources of the same taxa name
  group_by(duplicate.check) %>% 
  
  summarise(
    count = n(),
    .groups = "drop"
  ) %>% 
  
  filter(
    count > 1
  )
  
  
  
  


x <- bodysize_filtered %>% 
  
  # select columns I want
  select(
    original.source.code.1:original.source.code.18,
    individual.uid,
    original.taxa.name,
    source.code
  ) %>% 
  
  # pivot longer to get each source on it's own row
  pivot_longer(
    cols = original.source.code.1:original.source.code.18, values_to = "original.source.code"
  ) %>% 
  
  select(
    - name
  ) %>% 
  
  # remove NA
  filter(
    !is.na(original.source.code)
  ) %>% 
  
  # make a new column with the original.source.code and taxa.name pasted together
  mutate(
    duplicate.check = paste(original.source.code, original.taxa.name, sep = "-")
  ) %>% 
  
  group_by(duplicate.check) %>% 
  
  summarise(
    count = n(),
    .groups = "drop"
  ) %>% 
  
  filter(
    count > 1
  )


















  













