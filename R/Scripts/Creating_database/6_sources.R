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
bodysize_taxonomy <- readRDS("R/Data_outputs/full_database/bodysize_taxonomy.rds")
db_sources_raw <- read_xlsx(here("Raw_data","master_db_traits.xlsx"), sheet = "source_list")
wos_sources_raw <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "source_list")

# Join sources together ----

## Format WOS ----
wos_sources <- wos_sources_raw %>% 
  # Select columns
  select(
    - book.doi,
    - wos.uid,
    - authors.full.name
  ) %>% 
  
  # change paper.code name to match db_sources
  rename(
    citing.source = paper.code
  ) %>% 
  
  mutate(
    # change source.type info
    source.type = if_else(
      source.type == "original source",
      "secondary data",
      source.type
    ),
    
    # change types to make easier to merge
    source.code = as.character(source.code),
    citing.source = as.character(citing.source)
  ) %>% 
  
  # set "NA" to NA
  mutate_all(
    ., ~na_if(., "NA")
  ) 

## Format DB ----
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
  
  mutate(
    # add in a source.type column to match WOS
    source.type = case_when(
      source.code %in% c("odume et al", "db-7") ~ "primary data",
      TRUE ~ "secondary data"
    ),
    
    # change types to make mergeing easier
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


# Duplicate sources - within ----
## Finding any sources that are inputted twice under different source codes within the same citing source
within_dupe_check <- all_source_raw %>% 
  
  mutate(
    ## Formatting
    # make titles lower case incase there are any duplicates but have different caps in their title
    title = tolower(title),
    
    # make a new column take the doi if it has it and if not second choice is ISBN and then final choice is title
    source.info = case_when(
      !is.na(doi) ~ doi,
      is.na(doi) & !is.na(ISBN) ~ ISBN,
      is.na(doi) & is.na(ISBN) ~ title
    )
  ) %>% 
  
  ## Filter
  # remove any that are the same but have the same source.code already as only looking for duplicates under different source.codes
  distinct(
    source.code, .keep_all = TRUE
    ) %>% 
  
  ## Frequency table
  # group by citing.source and source.info to check for duplicates within citing sources only
  group_by(
    citing.source, source.info
    ) %>%
  
  # Make a column called freq which counts the number of occurances
  summarise(
    freq = n(), .groups = 'drop'
    ) %>% 
  
  # select any that appear more than once
  filter(
    freq > 1
  )
  
## No duplicates within sources under different source.codes

# Duplicate sources - between ----
## Checking if there are any sources that are inputted twice under different source codes between citing sources by making a frequency table

## Finding duplicates ----
between_dupe_check <- all_source_raw %>% 
  
  mutate(
    ## Formatting
    # make titles lower case incase there are any duplicates but have different caps in their title
    title = tolower(title),
    
    # make a new column take the doi if it has it and if not second choice is ISBN and then final choice is title
    source.info = case_when(
      !is.na(doi) ~ doi,
      is.na(doi) & !is.na(ISBN) ~ ISBN,
      is.na(doi) & is.na(ISBN) ~ title
    )
  ) %>% 
  
  ## Filter
  # remove any that are the same but have the same source.code already as only looking for duplicates under different source.codes
  distinct(
    source.code, .keep_all = TRUE
  ) %>% 
  
  ## Frequency table
  # group by source.info to check for duplicates between
  group_by(
    source.info
    ) %>%
  
  # Make a column called freq which counts the number of occurances
  summarise(
    freq = n(), .groups = 'drop'
  ) %>% 
  
  # select any that appear more than once
  filter(
    freq > 1
  )

## Duplicate source.code list ----
# Getting a list of source codes for each duplicate source found in the between_dupe_check list
# assigning them a new source code
duplicate_source_list <- all_source_raw %>% 
  
  mutate(
    ## Formatting ----
    # make titles lower case incase there are any duplicates but have different caps in their title
    title = tolower(title),
    
    # make a new column take the doi if it has it and if not second choice is ISBN and then final choice is title
    source.info = case_when(
      !is.na(doi) ~ doi,
      is.na(doi) & !is.na(ISBN) ~ ISBN,
      is.na(doi) & is.na(ISBN) ~ title
    )
  ) %>% 
  
  select(
    source.code,
    source.info,
    source.type
    ) %>% 
  
  ## Filter for duplicates ----
  # left join the frequency info from between_source_dupes and select ones that have a frequency value as there are the ones the appear more than once
  left_join(
    between_dupe_check,
    by = "source.info"
  ) %>% 
  
  filter(
    !is.na(freq)
  ) %>% 
  
  ## Make into wider format ----
  # Put each duplicate onto one row with separate columns for each different source.code so that new source codes can be assigned
  
  # group by the source info so each action is carried out within each duplicate source
  group_by(
    source.info
    ) %>%
  
  # make a column with the source codes for each duplicate merged together
  summarise(
    source.codes = paste(source.code, collapse = ",")
    ) %>% 
  
  # separate out into separate columns
  separate(
    source.codes,
    into = c("source.code.1", "source.code.2", "source.code.3"),
    sep = ","
    ) %>% 
  
  ## New source codes ----
  # Make a column called new.original.source.code for each group of duplicates
  mutate(
    new.original.source.code = paste("dupe", row_number(), sep = "-")
  ) %>% 
  
  # Put all the sources into one column so that each is on its own line but has the same new.original.source.ocde as the other duplicates of that source
  pivot_longer(
    cols = source.code.1:source.code.3,
    values_to = "original.source.code",
  ) %>% 
  
  filter(
    !is.na(
      original.source.code
    )
  ) %>% 
  
  # remove redundant columns
  select(
    -name
    ) %>% 
  
  # add if it is a primary or secondary sources
  left_join(
    select(
      all_source_raw, source.code, source.type
    ), by = c("original.source.code" = "source.code")
  )

# Duplicate data points ----
# Want to remove data points if they are from duplicate sources but also have the same taxa code because some secondary data papers might have gotten data from the same source but focused on different species so want to keep those


# pivot bodysize data to get all original.source.code columns into one column
# change the original.source.codes of duplicates to be the same using duplicate_new_code
# make a column called duplicate.check which is the new.source.code and the original.taxa.name merged together to check for duplicates of the same source and taxa.name



# Remove duplicates ----
# get a list of all the individual.uids that have duplicated data
duplicate_uids <- bodysize_taxonomy %>% 
  
  select(
    original.source.code.1:original.source.code.18,
    individual.uid,
    accepted.taxa.name,
    source.code
  ) %>% 
  
  # Pivot to get original.source.codes columns in one column
  pivot_longer(
    cols = original.source.code.1:original.source.code.18,
    values_to = "original.source.code",
    names_to = "source.code.column.no"
  ) %>% 
  
  filter(
    original.source.code %in% duplicate_source_list$original.source.code
  ) %>% 
  
  # Left_join the new source codes for the duplicates from duplicate_new_code
  left_join(
    duplicate_source_list, by = "original.source.code" # when a source code matches one of the ones to change in the duplicate_sources_list (source.code.change) then left join the one i want to change it to (source.code.keep)
  ) %>% 
  
  # Find data points that use the same original.source for the same taxa
  # merge the new.original.source.code and taxa name
  mutate(
    duplicate.check = paste(new.original.source.code, accepted.taxa.name, sep = "-")
  ) %>%
  
  group_by(duplicate.check) %>% 
  
  mutate(
    count = n_distinct(source.code)
    ) %>% 
  
  filter(
    count > 1,
    source.type != "primary data"
  ) %>% 
  
  mutate(
    count = n_distinct(source.code)
  ) %>% 
  filter(
    !(count > 1 & duplicated(duplicate.check))
  ) 

## Remove duplicates ----
# remove duplicates when they have only one original source code
bodysize_duplicates <- bodysize_taxonomy %>% 
  
  mutate(
    duplicate = if_else(
      individual.uid %in% duplicate_uids$individual.uid & original.source.code.2 == "no.source",
      "yes",
      "no"
    )
  ) %>% 
  
  filter(
    duplicate == "no"
  )

# make new source list ----
source_list_with_dupes <- all_source_raw %>% 
  
  # remove duplicate source codes as these have already been done
  distinct(
    source.code, .keep_all = TRUE
  ) %>% 
  
  left_join(
    select(
      duplicate_source_list, original.source.code, new.original.source.code
      ), by = c("source.code" = "original.source.code")
    ) %>% 
  
  mutate(
    row = row_number(),
    row = as.character(row),
    code.filler = if_else(
      !is.na(new.original.source.code),
      new.original.source.code,
      row
    )
  ) %>% 
  group_by(code.filler) %>% 
  mutate(new.code = cur_group_id()) %>% 
  ungroup()

## assing the new source codes to the data ----
bodysize_sources <- bodysize_duplicates %>% 
  
  select(
    original.source.code.1:original.source.code.18,
    uid
  ) %>% 
  
  # Pivot to get original.source.codes columns in one column
  pivot_longer(
    cols = original.source.code.1:original.source.code.18,
    values_to = "old.source.code",
    names_to = "source.code.column.no"
  ) %>% 

  left_join(
    select(
      source_list_with_dupes, source.code, new.code
  ), by = c("old.source.code" = "source.code")
  ) %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = source.code.column.no,
    values_from = new.code
  ) %>% 
  
  left_join(
    bodysize_taxonomy, by = "uid", suffix = c(".new", ".old")
  ) %>% 
  select(
    -original.source.code.1.old:-original.source.code.18.old
  ) %>% 
  
  # same thing but for source codes
  left_join(
    select(
      source_list_with_dupes, source.code, new.code
    ), by = "source.code"
  ) %>% 
  
  mutate(
    source.code = new.code
  ) %>% 
  
  select(
    - new.code
  ) %>% 
  
  rename_with(~ gsub(".new", "", .)) %>% 
  
  # merge together
  mutate(
    original.sources = paste(original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10,
                             original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
                             sep = ";"),
    original.sources = stri_replace_all_regex(original.sources, "NA;|NA|;NA", ""),
    original.sources = na_if(original.sources, "")
  ) %>% 
  
  select(
    -original.source.code.1:-original.source.code.18
  ) %>% 
  
  relocate(
    uid, individual.uid, tax.uid, source.code, original.sources, accepted.taxa.name, form, form.no, life.stage, sex, min.body.size, max.body.size, body.size, units, bodysize.measurement, sample.size, error, error.type, sample.year, sample.month
  )

## Save
saveRDS(bodysize_sources, file = "R/Data_outputs/full_database/bodysize_sources.rds")

source_list <- source_list_with_dupes %>% 
  mutate(
    source.code = new.code
  ) %>% 
  
  select(
    - code.filler,
    - new.code,
    - citing.source,
    - row,
    - new.original.source.code,
    - source.type
  ) %>% 
  distinct(
    source.code, .keep_all = TRUE
  )

## Save
write_csv(source_list, file = "R/Data_outputs/full_database/source_list.cvs")



