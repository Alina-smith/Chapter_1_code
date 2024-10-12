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
location_raw <- read_xlsx(here("Raw_data","location_data_for_wos_db.xlsx"), sheet = "location_raw")

# Joining db and wos ----
# Make a dataframe of all the db and wos data
  # Will filter this for duplicate sources and then add location data to

all_raw <- bind_rows(db_raw, wos_raw)%>% 
  # set NA join.location columns to no.source to help with left joining later
  mutate(
    join.location.1 = if_else(
      is.na(join.location.1),
      "no.source",
      join.location.1
    ),
    
    join.location.2 = if_else(
      is.na(join.location.2),
      "no.source",
      join.location.2
    ),
    
    join.location.3 = if_else(
      is.na(join.location.3),
      "no.source",
      join.location.3
    ),
    
    join.location.4 = if_else(
      is.na(join.location.4),
      "no.source",
      join.location.4
    ),
    
    join.location.5 = if_else(
      is.na(join.location.5),
      "no.source",
      join.location.5
    ),
    
    join.location.6 = if_else(
      is.na(join.location.6),
      "no.source",
      join.location.6
    ),
    
    join.location.7 = if_else(
      is.na(join.location.7),
      "no.source",
      join.location.7
    ),
    
    join.location.8 = if_else(
      is.na(join.location.8),
      "no.source",
      join.location.8
    ),
    
    join.location.9 = if_else(
      is.na(join.location.9),
      "no.source",
      join.location.9
    ),
    
    join.location.10 = if_else(
      is.na(join.location.10),
      "no.source",
      join.location.10
    ),
    
    join.location.11 = if_else(
      is.na(join.location.11),
      "no.source",
      join.location.11
    ),
    
    join.location.12 = if_else(
      is.na(join.location.12),
      "no.source",
      join.location.12
    ),
    
    join.location.13 = if_else(
      is.na(join.location.13),
      "no.source",
      join.location.13
    ),
    
    join.location.14 = if_else(
      is.na(join.location.14),
      "no.source",
      join.location.14
    ),
    
    join.location.15 = if_else(
      is.na(join.location.15),
      "no.source",
      join.location.15
    ),
    
    join.location.16 = if_else(
      is.na(join.location.16),
      "no.source",
      join.location.16
    ),
    
    join.location.17 = if_else(
      is.na(join.location.17),
      "no.source",
      join.location.17
    )
  ) %>% 
  # reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
           join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
           join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type)


# Sources - join together ----
## Join sources together and format so they are the same to join

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
  
# Join together:
all_source_raw <- bind_rows(db_sources, wos_sources) %>% 
  # set NA to na
  mutate_all(
    ~ na_if(., "NA")
    )

# Sources - within source dupes ----
# Want to check if any of the original sources have been used multiple times within the same source first

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

## within source - ISBN ----
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
  
## within source - title ----
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

## within source - Merge ----
within_dupes <- bind_rows(within_dupe_doi, within_dupe_isbn, within_dupe_title)
# gave two duplicates but one is the title of the same lake monitoring program but different years and the other had different join.codes for joining the original.source
# code onto the db data but has the same original.source code so when the final source list is made by getting all the unique source codes this will be fine

# Sources - between source dupes ----
# want to find if there are any sources that have used the same original data and if so then remove them if they are also for the same species

## DOI ----
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
  )

# match dois in frequency table to ones in all_source_raw to get source code
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
# find any DOIS that appear more than once by making a frequency table
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

# match dois in frequency table to ones in all_source_raw to get source code
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
# find any DOIS that appear more than once by making a frequency table
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

# find any DOIS that appear more than once by making a frequency table
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

# save - list of doi, isbn, titles that appear more than once and their source codes
saveRDS(between_dupes, file = "R/Data_outputs/databases/between_dupes.rds")

# Locations ----
# Want to add in the proper location codes to the raw data and format the location sheet 

## Edit location list ----
# need to add source.code to join.location to make it easier to left join as there are duplicate join.locations between sources
location_join <- location_raw %>% 
  mutate(
    join.location = paste(source.code, join.location, sep = "")
  )

## Add codes to raw data ----
# left join the proper location.code for each location to the raw data by the join.location columns
all_raw_location <- all_raw %>% 
  mutate(
    # join source.code and join.location to avoid duplicates when there is the same join.location between sources
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
      location_join, join.location, location.code
    ), by = c("join.location.1" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.1 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.2" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.2 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.3" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.3 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.4" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.4 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.5" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.5 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.6" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.6 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.7" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.7 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.8" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.8 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.9" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.9 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.10" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.10 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.11" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.11 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.12" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.12 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.13" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.13 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.14" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.14 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.15" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.15 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.16" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.16 = location.code)%>% 
  select(-location.code) %>% 
  
  left_join(
    select(
      location_join, join.location, location.code
    ), by = c("join.location.17" = "join.location")
  ) %>% 
  
  # rename and remove columns to make it easier to keep track
  mutate(join.location.17 = location.code)%>% 
  select(-location.code) %>% 
  
  # merge columns together
  mutate(
    location.code = paste(join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9,
                          join.location.10, join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
                          sep = ","),
    location.code = stri_replace_all_regex(location.code, ",NA|NA,|NA", ""),
    location.code = na_if(location.code, "") 
    ) %>% 
  
  # remove seperate join.location columns and keep just the merged one
  select(
    - join.location.1, - join.location.2, - join.location.3, - join.location.4, - join.location.5, - join.location.6, - join.location.7, - join.location.8, - join.location.9, - join.location.10,
    - join.location.11, - join.location.12, - join.location.13, - join.location.14, - join.location.15, - join.location.16, - join.location.17
  )

# save - db and wos data filtered for duplicate sources and with location codes
saveRDS(all_raw_location, file = "R/Data_outputs/databases/raw_body_size.rds")

## Format location sheet ----
# Check if there are any in the locations sheet but not used in the raw data 
location_check_1 <- all_raw_location %>% 
  select(
    location.code
  ) %>% 
  separate(
    ., location.code, into = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17"),
    sep = ","
  ) %>% 
  pivot_longer(., cols = 1:17, values_to = "location.code") %>% 
  distinct(location.code) %>% 
  filter(
    !is.na(location.code)
  )

location_check_2 <- anti_join(location_raw, location_check_1, by = "location.code")
# no locations in the location sheet that aren't used in the data so don't need to remove any

# get final sheet by getting distinct location.codes
location_list <- location_raw %>% 
  distinct(location.code, .keep_all = TRUE) %>% 
  select(
    - join.location,
    - source.code
  )

# save - list of all distinct locations used in the data
saveRDS(location_list, file = "R/Data_outputs/databases/location_list.rds")

