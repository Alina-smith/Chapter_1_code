# Aim of script: Filtering out duplicate sources from raw data

# Flow of script:
# 1) Join:                  Join the WOS and DB source lists together
# 2) Within source dupes:   Check for any duplicates within the same citing source (e.g. a paper that has gotten it's data from other sources has cited the same source twice)
# 3) Between source dupes:  Check for any duplicates between sources (e.g. I have taken data from a paper that produced its own data and then I have also taken data from a paper that has used other people data and some of this data has been taken from a paper I have already used)
# 4) Remove dupes:          Make a list of all data points that have come from a duplicated source that is also for the same taxa and remove it from the main data
# 5) Update source codes:   Make a new source list with updated source codes and then use this to update source codes in main data

# Script last updated on 18/03/2025

# Packages ----
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)

# Data ----
bodysize_taxonomy <- readRDS("R/Data_outputs/full_database/tol/bodysize_taxonomy_tol2.rds")
db_sources_raw <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "source_list")
wos_sources_raw <- read_xlsx("raw_data/master_wos_data.xlsx", sheet = "source_list")

# Raw source list ----
# Join the WOS and DB source lists together into one main source list

## Format sources ----
# Need to format wos and db sources so they can be joined together

### WOS ----
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

### DB ----
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


# Within source duplicates ----
## Finding any sources that are inputted twice under different source codes within the same citing source

within_dupe_check <- all_source_raw %>% 
  
  mutate(
    ## Formatting ----
    # Make titles lower case incase there are any duplicates but have different caps in their title
    title = tolower(title),
    
    # Make a new column take the doi if it has it and if not second choice is ISBN and then final choice is title
    source.info = case_when(
      !is.na(doi) ~ doi,
      is.na(doi) & !is.na(ISBN) ~ ISBN,
      is.na(doi) & is.na(ISBN) ~ title
    )
  ) %>% 
  
  ## Filter ----
  # Remove any that are the same but have the same source.code already as only looking for duplicates under different source.codes
  distinct(
    source.code, .keep_all = TRUE
    ) %>% 
  
  ## Frequency table ----
  # Group by citing.source and source.info to check for duplicates within citing sources only
  group_by(
    citing.source, source.info
    ) %>%
  
  # Make a column called freq which counts the number of occurrences
  summarise(
    freq = n(), .groups = 'drop'
    ) %>% 
  
  # select any that appear more than once
  filter(
    freq > 1
  )
  
## No duplicates within sources under different source.codes

# Between source duplicates ----
# Checking if there are any sources that are inputted twice under different source codes between citing sources

## Duplicates source info list ----
between_dupe_check <- all_source_raw %>% 
  
  mutate(
    ### Formatting ----
    # Make titles lower case incase there are any duplicates but have different caps in their title
    title = tolower(title),
    
    # Make a new column take the doi if it has it and if not second choice is ISBN and then final choice is title
    source.info = case_when(
      !is.na(doi) ~ doi,
      is.na(doi) & !is.na(ISBN) ~ ISBN,
      is.na(doi) & is.na(ISBN) ~ title
    )
  ) %>% 
  
  ### Filter ----
  # Remove any that are the same but have the same source.code already as only looking for duplicates under different source.codes
  distinct(
    source.code, .keep_all = TRUE
  ) %>% 
  
  ### Frequency table ----
  # Group by source.info to check for duplicates - already checked for ones within sources
  group_by(
    source.info
    ) %>%
  
  # Make a column called freq which counts the number of occurrences
  summarise(
    freq = n(), .groups = 'drop'
  ) %>% 
  
  # select any that appear more than once
  filter(
    freq > 1
  )

# 23 duplicate sources

## Duplicate source.codes list ----
# Using the doi list to etting a list of source codes for each duplicate source found

duplicate_source_list <- all_source_raw %>% 
  
  mutate(
    ### Formatting ----
    # make the same format as the between dupes list
    title = tolower(title), # make titles lower case incase there are any duplicates but have different caps in their title
    
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
  
  ### Filter for duplicates ----
  # Get a list of all duplicates

  # left join the frequency info from between_source_dupes by the new source.info column
  left_join(
    between_dupe_check,
    by = "source.info"
  ) %>% 
  
  # Select ones that have a frequency value as there are the ones the appear more than once
  filter(
    !is.na(freq)
  ) %>% 
  
  ### Make into wider format ----
  # Put each duplicate onto one row with separate columns for each different source.code so that new source codes can be assigned
  
  # Group by the source info so each action is carried out within each duplicate source
  group_by(
    source.info
    ) %>%
  
  # Make a column with the source codes for each duplicate merged together
  summarise(
    source.codes = paste(source.code, collapse = ",")
    ) %>% 
  
  # separate out into separate columns
  separate(
    source.codes,
    into = c("source.code.1", "source.code.2", "source.code.3"),
    sep = ","
    ) %>% 
  
  ### New source codes
  # Assign new source codes to the duplicates to make them identifiable as a duplicate 
  
  # Make a column called new.original.source.code
  mutate(
    new.original.source.code = paste("dupe", row_number(), sep = "-")
  ) %>% 
  
  # Pivot back into a longer format so that each source has it's own row but will still have the same dupe-(rownumber) new.original.source.code
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
  
  # Add in if it is a primary or secondary sources as this will be needed later
  left_join(
    select(
      all_source_raw, source.code, source.type
    ), by = c("original.source.code" = "source.code")
  )

## Duplicate uid list ----
# Get a list of all uids that come from a duplicate source
# Now have the sources that appear more than once in the source list want to locate the data points in the raw data that use these sources and keep just one of them
# However want there may instances where they are from the same source but for different species so want to keep all that are for different species 

## Duplicate uid list ----
# Get a list of all the individual.uids that have duplicated data - probably a long winded way of doing it but only way I could visulise in my head
# There may instances where they are from the same source but for different species so want to keep all that are for different species 

duplicate_uids <- bodysize_taxonomy %>% 
  
  select(
    original.source.code.1:original.source.code.18,
    individual.uid,
    taxa.name,
    source.code
  )%>% 
  
  ### Pivot ----
  # Puts each original.source.code on their own row
  pivot_longer(
    cols = original.source.code.1:original.source.code.18,
    values_to = "original.source.code",
    names_to = "source.code.column.no"
  ) %>% 
  
  ### Filter ----
  # Select the data points that have sources that appear in the duplicate source list
  filter(
    original.source.code %in% duplicate_source_list$original.source.code
  ) %>% 
  
  ### New source codes ----
  # Left_join the new source codes for the duplicates from duplicate_new_code
  left_join(
    duplicate_source_list, by = "original.source.code" # when a source code matches one of the ones to change in the duplicate_sources_list (source.code.change) then left join the one i want to change it to (source.code.keep)
  ) %>% 
  
  ### Duplicate + taxa ----
  # Find data points that use the same original.source for the same taxa
  
  # Merge the new.original.source.code and taxa name
  mutate(
    duplicate.check = paste(new.original.source.code, taxa.name, sep = "-")
  ) %>%
  
  ### Frequency table ----
  # make a frequency table for all the points and select ones that occur more than once

  # Group by duplicate check
  group_by(duplicate.check) %>% 
  
  # Count how many sources there are for each one - will show if there are more than once source for each one
  mutate(
    count = n_distinct(source.code)
    ) %>% 
  
  filter(
    # Select only ones that occur more than once
    count > 1, # This now gives a list of all data points where the same source for the same individual has been used
    
    # Prioritize primary data points - When there are duplicates and one is primary want to keep the primary one so remove these from the list so they are not removed from main data
    source.type != "primary data" 
  ) %>% 
  
  # Redo count column
  mutate(
    count.primary = n_distinct(source.code) # This now gives a list of all the duplicates of the primary data points (count = 1) and any data points where the same source for the same individual has been used and they both came from secndary sources (count = 2)
  ) %>% 
  
  ungroup() %>% 
  
  # Remove secondary duplicates
  group_by(duplicate.check, source.code) %>% 
  
  # Want to keep the source that has the most data points so make a column which count the number of data points for each source code
  mutate(
    point.number = n()
  ) %>% 
  
  ungroup() %>% 
  
  group_by(duplicate.check) %>% 
  
  mutate(
    
    # When there are just one data point in each source point.no will be the same for both so when removing the highest point.no is will remove both of these so want to mark out these ones
    dupe.no = n(),

    remove = if_else(
      point.number == max(point.number) & count.primary == "2" & dupe.no > 2, # Remove the one with the max point number when it is a secondary duplicate and has more than 2 data points
      "yes",
      "no"
    )
  ) %>% 
  
  ungroup() %>%
  
  # keep the ones we don't want to remove
  filter(
    remove == "no"
  ) %>% 
  
  # Finally all the ones that are left are duplicates with just one in each so just delete any one
  
  group_by(duplicate.check) %>%
  
  # make a row number column so can just select row 1 to remove
  mutate(
    row.no = row_number()
  ) %>% 
  
  mutate(
    remove = if_else(
      dupe.no == "2" & row.no == "1" & count.primary == "2", # remove the first row of all that have only have two duplicates and are secondary duplicates
      "yes",
      "no"
    )
  ) %>% 
  
  ungroup() %>% 
  
  filter(
    remove == "no"
  )%>% 
  
  select(
    individual.uid
  )
# This now gives a list of all uids that need to be removed from the main data

## Remove duplicates ----
# remove duplicates when they have only one original source code
bodysize_duplicates <- bodysize_taxonomy %>% 
  
  mutate(
    duplicate = if_else(
      individual.uid %in% duplicate_uids$individual.uid & original.source.code.2== "no.source",
      "yes",
      "no"
    )
  ) %>% 
  
  filter(
    duplicate == "no"
  )

# Update source.codes ----
# Make new source codes

## Make a new source list ----
# Make a new source list with new codes

new_source_codes <- bodysize_duplicates %>% 
  
  # Select columns
  select(source.code:original.source.code.18) %>% 
  
  # Get each source code on its own line
  pivot_longer(
    cols = source.code:original.source.code.18,
    values_to = "source.code"
    ) %>% 
  
  # Get all the distinct not unkown or no.source source codes
  distinct(
    source.code
  ) %>% 
  
  filter(
    !(source.code %in% c("unknown", "no.source"))
  ) %>% 
  
  # Add in source info
  left_join(., all_source_raw, by = "source.code") %>%
  
  distinct(
    source.code
  ) %>% 
  
  # Make new source codes
  mutate(
    new.source.code = row_number()
  )

## Update source codes in data ----
bodysize_sources <- bodysize_duplicates %>% 
  
  select(
    source.code:original.source.code.18,
    uid
  ) %>% 
  
  # Pivot to get original.source.codes columns in one column
  pivot_longer(
    cols = source.code:original.source.code.18,
    values_to = "old.source.code",
    names_to = "source.code.column.no"
  ) %>% 

  # Join in new source codes
  left_join(
    select(
      new_source_codes, source.code, new.source.code
  ), by = c("old.source.code" = "source.code")
  ) %>% 
  
  # Put back into original format
  pivot_wider(
    id_cols = uid,
    names_from = source.code.column.no,
    values_from = new.source.code
  ) %>% 
  
  # Join back in data that was lost in the pivots
  left_join(
    bodysize_taxonomy, by = "uid", suffix = c(".new", ".old")
  ) %>% 
  select(
    -source.code.old:-original.source.code.18.old
  ) %>% 
  
  rename_with(~ gsub(".new", "", .))%>% 
  
  # Merge the original source columns together into one
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
  
  # Relocate
  relocate(
    uid, individual.uid,
    source.code, original.sources,
    original.taxa.name, taxa.name.full, taxa.name, tax.uid, type, species, genus, family, order, class, phylum, kingdom,
    nu, ind.per.nu, life.stage, sex,
    min.body.size, max.body.size, body.size, units, bodysize.measurement, bodysize.measurement.notes,
    reps, measurement.type, sample.size, error, error.type,
    sample.year, sample.month
  )

# Save
saveRDS(bodysize_sources, file = "R/Data_outputs/full_database/tol/bodysize_sources_tol2.rds")

# updated source list

sources_list_update <- left_join(new_source_codes, all_source_raw, by = "source.code") %>% 
  
  distinct(
    source.code, .keep_all = TRUE
  ) %>% 
  
  select(
    - citing.source,
    - source.type
  )

## Save
saveRDS(sources_list_update, file = "R/Data_outputs/full_database/tol/sources_list_update_tol2.rds")

