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
locations_raw <- read_xlsx(here("Raw_data","location_data_for_wos_db.xlsx"), sheet = "location_raw")

# Sources - wthin source duplicates ----
# Want to check if any of the original sources have been used multiple times within the same source first
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
  # set NA to na
  mutate_all(
    ~ na_if(., "NA")
    )

## DOI: ----
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

## ISBN ----
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
  
## title ----
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

## Merge all together ----
within_dupes <- bind_rows(within_dupe_doi, within_dupe_isbn, within_dupe_title)
# gave two duplicates but one is the title of the same lake monitoring program but different years and the other had different join.codes for joining the original.source
# code onto the db data but has the same original.source code so when the final source list is made by getting all the unique source codes this will be fine

# Sources - between source duplicates ----
# want to find if there are any sources that have used the same original data and if so then remove them if they are also for the same species

## DOI ----
between_dupe_doi_freq <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(doi)
  ) %>%
  group_by(doi) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  )

# make list of duplicate sources with their source code
between_dupe_doi <- all_source_raw %>% 
  filter(
    doi %in% between_dupe_doi_freq$doi # select any that match the doi's in the doi_freq dataframe
  ) %>% 
  
  # make individual columns for each source.code for each duplicate title
  group_by(doi) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2", "source.code.3"), sep = ",") %>% 
  
  # rename so can merge with others later
  rename(
    source.info = doi,
  ) %>% 
  
  # add in type so knows what was used to find the duplicates
  mutate(
    duplicate.type = "doi"
  )

## ISBN ----
between_dupe_isbn_freq <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(ISBN)
  ) %>%
  group_by(ISBN) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  )

# make list of duplicate sources with their source code
between_dupe_isbn <- all_source_raw %>% 
  filter(
    ISBN %in% between_dupe_isbn_freq$ISBN # select any that match the doi's in the doi_freq dataframe
  ) %>% 
  
  # make individual columns for each source.code for each duplicate title
  group_by(ISBN) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2"), sep = ",") %>% 
  
  # rename so can merge with others later
  rename(
    source.info = ISBN,
  ) %>% 
  
  # add in type so knows what was used to find the duplicates
  mutate(
    duplicate.type = "isbn"
  )

## title ----
between_dupe_title_freq <- all_source_raw %>% 
  # select ones with doi
  filter(
    !is.na(title)
  ) %>%
  # make all lower case incase some are caps
  mutate(
    title = tolower(title)
  ) %>% 
  group_by(title) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  filter(
    count > 1
  ) 

# make list of duplicate sources with their source code
between_dupe_title <- all_source_raw %>% 
  # make lower case to match between_dupe_title
  mutate(
    title = tolower(title)
  ) %>% 
  filter(
    title %in% between_dupe_title_freq$title # select any that match the doi's in the doi_freq dataframe
  ) %>% 
  
  # make individual columns for each source.code for each duplicate title
  group_by(title) %>% 
  summarise(source.codes = paste(source.code, collapse = ",")) %>% 
  separate(source.codes, into = c("source.code.1", "source.code.2", "source.code.3", "source.code.4"), sep = ",") %>% 
  
  # rename so can merge with others later
  rename(
    source.info = title,
  ) %>% 
  
  # add in type so knows what was used to find the duplicates
  mutate(
    duplicate.type = "title"
  )

## Merge all together ----
between_dupes <- bind_rows(between_dupe_doi, between_dupe_isbn, between_dupe_title) %>% 
  relocate(source.info, source.code.1, source.code.2, source.code.3, source.code.4, duplicate.type)

x <- all_raw %>% 
  filter(
    bodysize.measurement == "biovolume"
  )%>% 
  distinct(original.taxa.name)

# Joining data ----
all_raw <- bind_rows(db_raw, wos_raw)%>% 
  # reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
           join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
           join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type)

# Locations ----

## Edit location list ----
# need to add source.code o join.location to make it easier to left join as there are duplicate join.locations between sources
locations_join <- locations_raw %>% 
  mutate(
    join.location = paste(source.code, join.location, sep = "")
  )



## Add in location.code ----
all_raw_locations <- all_raw %>% 
  mutate(
    join.location.1 = paste(source.code, join.location.1, sep = ""),
    join.location.2 = paste(source.code, join.location.2, sep = ""),
    join.location.3 = paste(source.code, join.location.3, sep = ""),
    join.location.4 = paste(source.code, join.location.4, sep = ""),
    join.location.5 = paste(source.code, join.location.5, sep = ""),
    join.location.6 = paste(source.code, join.location.6, sep = ""),
    join.location.7 = paste(source.code, join.location.7, sep = ""),
    join.location.8 = paste(source.code, join.location.8, sep = ""),
    join.location.9 = paste(source.code, join.location.9, sep = ""),
    join.location.10 = paste(source.code, join.location.10, sep = ""),
    join.location.11 = paste(source.code, join.location.11, sep = ""),
    join.location.12 = paste(source.code, join.location.12, sep = ""),
    join.location.13 = paste(source.code, join.location.13, sep = ""),
    join.location.14 = paste(source.code, join.location.14, sep = ""),
    join.location.15 = paste(source.code, join.location.15, sep = ""),
    join.location.16 = paste(source.code, join.location.16, sep = ""),
    join.location.17 = paste(source.code, join.location.17, sep = "")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.1" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.2" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.3" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.4" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.5" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.6" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.7" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.8" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.9" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.10" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.11" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.12" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.13" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.13" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.14" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.15" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.16" = "join.location")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.17" = "join.location")
  ) %>% 
  
  mutate(
    location.code = paste(location.code.x, location.code.x.x, location.code.x.x.x, location.code.x.x.x.x, location.code.x.x.x.x.x, location.code.x.x.x.x.x.x, location.code.x.x.x.x.x.x.x, location.code.x.x.x.x.x.x.x.x, location.code.x.x.x.x.x.x.x.x.x,
                          location.code.y, location.code.y.y, location.code.y.y.y, location.code.y.y.y.y, location.code.y.y.y.y.y, location.code.y.y.y.y.y.y, location.code.y.y.y.y.y.y.y, location.code.y.y.y.y.y.y.y.y, location.code.y.y.y.y.y.y.y.y.y,
                          sep = ","),
    location.code = stri_replace_all_regex(location.code, ",NA|NA,|NA", ""),
    location.code = na_if(location.code, "") 
    )%>% 

  select(
    location.code,
    source.code
  ) %>% 
  filter(
    is.na(location.code)
  ) %>% 
  distinct(source.code)

x <- all_raw %>% 
  filter(
    is.na(join.location.1)
  ) %>% 
  distinct(
    source.code
  )



x_locations <- all_raw %>% 
  mutate(
    join.location.1 = paste(source.code, join.location.1, sep = ""),
    join.location.2 = paste(source.code, join.location.2, sep = ""),
    join.location.3 = paste(source.code, join.location.3, sep = ""),
    join.location.4 = paste(source.code, join.location.4, sep = ""),
    join.location.5 = paste(source.code, join.location.5, sep = ""),
    join.location.6 = paste(source.code, join.location.6, sep = ""),
    join.location.7 = paste(source.code, join.location.7, sep = ""),
    join.location.8 = paste(source.code, join.location.8, sep = ""),
    join.location.9 = paste(source.code, join.location.9, sep = ""),
    join.location.10 = paste(source.code, join.location.10, sep = ""),
    join.location.11 = paste(source.code, join.location.11, sep = ""),
    join.location.12 = paste(source.code, join.location.12, sep = ""),
    join.location.13 = paste(source.code, join.location.13, sep = ""),
    join.location.14 = paste(source.code, join.location.14, sep = ""),
    join.location.15 = paste(source.code, join.location.15, sep = ""),
    join.location.16 = paste(source.code, join.location.16, sep = ""),
    join.location.17 = paste(source.code, join.location.17, sep = "")
  ) %>% 
  
  left_join(
    select(
      locations_join, join.location, location.code
    ), by = c("join.location.1" = "join.location")
  ) %>% 
  filter(
    is.na(location.code)
  ) %>% 
  distinct(join.location.1)

  