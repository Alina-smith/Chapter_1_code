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

## Save ----
saveRDS(all_source_raw, file = "R/Data_outputs/databases/all_source_raw.rds")

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

## Save ----
saveRDS(duplicate_source_list, file = "R/Data_outputs/databases/duplicate_source_list.rds")

# Duplicate data points ----
# Want to remove data points if they are from duplicate sources but also have the same taxa code because some secondary data papers might have gotten data from the same source but focused on different species so want to keep those


# pivot bodysize data to get all original.source.code columns into one column
# change the original.source.codes of duplicates to be the same using duplicate_new_code
# make a column called duplicate.check which is the new.source.code and the original.taxa.name merged together to check for duplicates of the same source and taxa.name



# Duplicate individual.UIDs ----
# get a list of all the individual.uids that have duplicated data
duplicate_uids <- bodysize_joined %>% 
  
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
    duplicate.check = paste(new.original.source.code, original.taxa.name, sep = "-")
  ) %>% 
  
  ## Remove replicates
  # don't want data points of the same data from within the same study
  group_by(source.code) %>% 
  distinct(duplicate.check,.keep_all = TRUE) %>% 
  ungroup() %>% 
  
  ## Find duplicates
  group_by(duplicate.check) %>%  
  
  mutate(
    duplicate = n()
  ) %>% 
  
  ungroup() %>% 
  
  filter(duplicate > 1) %>% 
  
  select(
    -duplicate
  ) %>% 
  
  filter(
    source.type != "primary data"
  ) %>% 
  
  ## Find duplicates
  group_by(duplicate.check) %>%  
  
  mutate(
    duplicate = n(),
    dupe.no = row_number()
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    remove = case_when(
      duplicate == "1" ~ "yes",
      duplicate == "2" & dupe.no == "1" ~ "yes",
      duplicate == "2" & dupe.no == "2" ~ "no",
    )
  ) %>% 
  
  filter(
    remove == "yes"
  ) %>% 
  
  select(
    individual.uid
  ) %>% 
  
  mutate(
    duplicate = "yes"
  )
  

x <- bodysize_joined %>% 
  left_join(
    duplicate_uids, by = "individual.uid"
  ) %>% 
  
  filter(
    duplicate == "yes"
  )















x <- bodysize_joined %>% 
  
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
    duplicate.check = paste(new.original.source.code, original.taxa.name, sep = "-")
  ) %>% 
  
  ## Remove replicates
  # don't want data points of the same data from within the same study
  group_by(source.code) %>% 
  distinct(duplicate.check,.keep_all = TRUE) %>% 
  ungroup() %>% 
  
  ## Find duplicates
  group_by(duplicate.check) %>%  
  
  mutate(
    duplicate = n()
  ) %>% 
  
  ungroup() %>% 
  
  filter(duplicate > 1) 
%>% 
  
  group_by(duplicate.check) %>%  
  
  mutate(
    row = row_number()
  )




















mutate(
    remove = if_else(
      duplicate == "1",
      "yes",
      NA
    )
  )


## make a list of all uids that are duplicate data points
duplicate_datapoints <- duplicate_uids %>% 
  
  ## Remove replicates ----
  # don't want data points of the same data from within the same study
  group_by(source.code) %>% 
  distinct(duplicate.check,.keep_all = TRUE) %>% 
  ungroup() %>% 
  
  ## Find duplicates ----
  group_by(duplicate.check) %>%  

  mutate(
    duplicate = n(),
    ) 
%>% 
  filter(duplicate > 1) %>% 
  
  # Join the dupicate info onto main data
  left_join(duplicate_pivot, ., by = "duplicate.check")%>% 
  filter(
    !is.na(duplicate)
  ) %>% 
  select(
    - duplicate
  ) %>% 
  
  mutate(
    remove = if_else(
      source.type == "primary data",
      "no",
      NA
    )
  )

duplicate_primarys <- duplicate_datapoints %>% 
  # don't want data points of the same data from within the same study
  group_by(source.code) %>% 
  distinct(duplicate.check,.keep_all = TRUE) %>% 
  ungroup() %>%
  
  filter(
    source.type != "primary data"
  ) %>% 
  
  ## Find duplicates
  group_by(duplicate.check) %>% 
  summarise(freq = n(), .groups = "drop")
%>% 
  mutate(
    remove = if_else(
      freq == "1",
      "yes",
      "no"
    )
  ) %>% 
  filter(
    remove == "yes"
  ) %>% 
  left_join(duplicate_datapoints, ., by = "duplicate.check")
  
  group_by()




%>% 
  filter(duplicate > 1)
  
  

# group by assign 1 2 and delet 2s
%>%
  
  select(
    source.code,
    uid,
    original.source.code,
    new.original.source.code,
    source.type,
    duplicate.check
  )
  
  ## Make into wider format
  # Put each duplicate onto one row with separate columns for each different source.code
  
  # group by the source info so each acton is carried out within each duplicate source
  group_by(
    new.original.source.code, source.type
  ) %>%
  
  # make a column with the source codes for each duplicate merged together
  summarise(
    source.codes = paste(original.source.code, collapse = ",")
  ) %>% 
  
  # separate out into separate columns
  separate(
    source.codes,
    into = c("source.code.1", "source.code.2", "source.code.3", "source.code.4", "source.code.5", "source.code.6"),
    sep = ","
  ) 









  
  # remove duplicates within sources
  group_by(source.code) %>% 
  distinct(duplicate.check,.keep_all = TRUE) %>% 
  ungroup() %>% 
  
  group_by(duplicate.check) %>% 
  summarise(remove = n()) %>% 
  
  mutate(
    remove = if_else(
      
    )
  )

%>% 
  
  # remove all that have a value of 1 because now the primary sources have been removed this means these were the ines citing them
  filter(
    remove > 1
  )
  )

%>% 
  left_join(
    duplicate_pivot, ., by = "duplicate.check"
  )






bodysize_pivot <- bodysize_joined %>% 
  
  # pivot to get original.source.codes columns in one column
  pivot_longer(
    cols = original.source.code.1:original.source.code.18,
    values_to = "original.source.code",
    names_to = "source.code.column.no"
  ) %>% 
  
  # make new original.source.code column with the source codes changed to the same for duplicates
  left_join(
    duplicate_new_code, by = c("original.source.code" = "duplicate.source.code") # when a source code matches one of the ones to change in the duplicate_sources_list (source.code.change) then left join the one i want to change it to (source.code.keep)
  ) %>% 
  
  # keep the original.source.code the same for non-duplicates
  mutate(
    new.original.source.code = if_else(
      is.na(new.original.source.code),
      original.source.code,
      new.original.source.code
    )
  ) %>% 
  
  # make a duplicate.check column to use later on for left_joining
  mutate(
    duplicate.check = paste(new.original.source.code, original.taxa.name, sep = "-")
  )

# Duplicate data points - Finding data points ----
# find all data points that are from the same original.source.code and the same taxa

bodysize_duplicates <- bodysize_pivot %>% 
  
  # remove any that don't have an original.source.code as these will show up as duplicates but they aren't
  filter(
    new.original.source.code != "no.source",
    new.original.source.code != "unknown"
  ) %>% 
  
  ## Remove within source duplicates ----
  # group_by source.code and then get distinct duplicate.codes to get rid of dupicates within sources
  group_by(source.code) %>%
  
  # get only distinct values for duplicate.check for each source.code
  distinct(
    duplicate.check, 
    .keep_all = TRUE
  ) %>% 
  
  # remove grouping so can check for duplicates between source.codes next
  ungroup() %>% 
  
  ## Find duplicate data points ----
  # group by duplicate.check and make a frequency table with column named duplicate to find all duplicate.check values that appear more than once
  group_by(
    duplicate.check
  ) %>% 
  
  summarise(
    duplicate = n()
  ) %>% 
  
  # select ones that appear more than once
  filter(
    duplicate > 1
  ) %>% 
  
  ## Join back all data ----
  # left join bodysize_pivot by duplicate.check to get all the data back again with the frequency info
  left_join(
    bodysize_pivot, ., by = "duplicate.check"
  ) %>% 
  
  # update duplicate column for extra data
  mutate(
    duplicate = if_else(
      is.na(duplicate),
      "no",
      "yes"
    )
  ) %>% 
  
  filter(
    duplicate == "yes"
  )


x <- bodysize_duplicates %>% 
  pivot_wider(
    id_cols = uid,
    names_from = source.code.column.no,
    values_from = new.original.source.code,
    .keep_all = TRUE
  ) %>% 
  left_join(
    bodysize_joined, by = "uid",
    suffix = c(".old", ".new")
  )

x <- bodysize_duplicates %>% 
  filter(
    duplicate == "yes"
  ) 
%>% 
  distinct(
    individual.uid,
    .keep_all = TRUE
  ) 

y <- bodysize_joined %>% 
  left_join(
    select(
      x, individual.uid, duplicate
    ), by = "individual.uid"
  ) %>% 
  filter(
    duplicate == "yes"
  )
  
  
  
  left_join(bodysize_joined, select(
  x, individual.uid, duplicate
), by = "individual.uid"
) %>% 
  filter(
    duplicate == "yes"
  )

bodysize_source.3 <- bodysize_sources.2 %>% 
  # remove duplicates where a secondary paper has cited a primary paper - want to keep the primary paper
  filter(
    duplicate == "yes", 
  )
  
  
  
  
  
  
  left_join(
    select(
      bodysize_sources.2, duplicate.check, duplicate
      ),by = "duplicate.check"
  ) %>% 
  
  mutate(
    duplicate = if_else(
      is.na(duplicate),
      "no",
      "yes"
    ),
    
    remove = case_when(
      duplicate == "no" ~ "no",
      duplicate == "yes" & paper.type == "primary" ~ "no",
      
    )
  )
  

  
  


x <- bodysize_source.3 %>% 
  filter(
    duplicate == "yes"
  )
  
x <- bodysize_joined %>% 
  filter(
    original.taxa.name == "Desmodesmus subspicatus"
  )







