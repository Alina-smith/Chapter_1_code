## WOS cross referencing dump

## packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## data
original_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "secondary_data_original_raw")
primary_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "source_list_raw")

## DUMP
## Check if any of the sources in the secondary data list have referenced other in that list
within_referenced_exp <- original_sources %>% 
  mutate(
    duplicate = duplicated(source)
  ) %>% 
  filter(duplicate == TRUE)

within_referenced_exp2 <- original_sources %>% 
  mutate(
    duplicate = duplicated(source)
  ) %>% 
  filter(duplicate == TRUE)%>% 
  group_by(source.code.of.first.paper) %>% 
  mutate(
    same.source = duplicated(source.code.of.first.paper)
  )

# find overall duplicates
overall_duplicate_table <- as.data.frame(table(original_sources$source)) %>% 
  rename(
    source = Var1
  )
# find duplicates within sources
within_source_duplicate <- original_sources %>% 
  group_by(source.code.of.first.paper) %>% 
  as.data.frame(table(within_source_duplicate$source))


%>% 
  rename(
    source = Var1
  )

within_referenced_exp6 <- left_join()

within_referenced_exp5 <- as.data.frame(table(original_sources$source)) %>%
  # rename column to source for left_join later
  rename(
    source = Var1
  ) %>% 
  # make new column for true or false if its a replicate
  mutate(
    duplicate = ifelse(
      Freq > 1, TRUE, FALSE
    )
  ) %>% 
  # remove unnecessary columns
  select(-Freq) %>% 
  # join duplcate column onto full original_sources list
  left_join(original_sources, ., by = "source") %>% 
  # filter duplicates
  filter(duplicate == TRUE) %>% 
  # within source duplicate
  group_by(source.code.of.first.paper)






within_referenced_exp8 <- as.data.frame(table(within_referenced_exp7$source)) %>% 
  # rename column to source for left_join later
  rename(
    source = Var1
  ) %>% 
  # make new column for true or false if its a replicate
  mutate(
    duplicate = ifelse(
      Freq > 1, TRUE, FALSE
    )
  ) 
%>% 
  # remove unnecessary columns
  select(-Freq) %>% 
  # join duplcate column onto full original_sources list
  left_join(original_sources, ., by = "source") %>% 
  filter(duplicate == TRUE)

%>% 
  as.data.frame(table(group_by(original_sources, source.code.of.first.paper)$source))


%>% 
  group_by(source.code.of.first.paper) 





## Check for any papers in this list that reference other papers within the list
## Make a list of all distinct sources in each paper
full_source_list <- original_sources %>% 
  # select just ones with data in the paper
  filter(type == "data") %>% 
  # group by primary.source.code and then get each distinct paper within that paper. This means i can check which papers are citing the same papers and remove them
  group_by(secondary.source.code) %>% 
  distinct(primary.source, .keep_all = TRUE) %>% 
  select(- citation, - type)

## find which papers are 
duplicates_within_secondary <- as.data.frame(table(full_source_list$primary.source)) %>% 
  # rename column to source for left_join later
  rename(
    primary.source = Var1
  ) %>% 
  # make new column for true or false if its a replicate
  mutate(
    duplicate = ifelse(
      Freq > 1, TRUE, FALSE
    )
  ) %>% 
  # remove unnecessary columns
  select(-Freq) %>% 
  # join duplcate column onto full original_sources list
  #left_join(original_sources, ., by = "primary.source") %>% 
  filter(duplicate == TRUE)



## Make a list of all distinct sources in each paper
full_source_list <- original_sources %>% 
  # select just ones with data in the paper
  filter(type == "data") %>% 
  # group by primary.source.code and then get each distinct paper within that paper. This means i can check which papers are citing the same papers and remove them
  group_by(secondary.source.code) %>% 
  distinct(primary.source, .keep_all = TRUE) %>% 
  select(- citation, - type)

# give a list of all sources that are duplicates and the source code they are duplicated in
duplicated_sources <- original_sources %>% 
  filter(type == "data") %>% 
  group_by(secondary.source.code) %>% 
  distinct(primary.source) %>% 
  left_join(., as.data.frame(table(.$primary.source)), by = c("primary.source" = "Var1")) %>% 
  mutate(
    duplicate = ifelse(
      Freq > 1, TRUE, FALSE
    )
  ) %>% 
  filter(duplicate == TRUE) %>% 
  # remove unnecessary columns
  select(-Freq, -duplicate)








#### cross reference sources from secondary data with primary data list ----
## 1) add in a duplicate column to the primary sources list to left_join any duplicates to
primary_sources_duplicate_check <- primary_sources %>% 
  mutate(
    duplicate = "yes"
  )

## create a list of sources that are duplicated within the secondary data list and what sources they are duplicated in
# give a list of all sources that are duplicates and the source code they are duplicated in
secondary_sources_duplicate_check <- original_sources %>% 
  filter(type == "data") %>% 
  group_by(secondary.source.code) %>% 
  distinct(primary.source) %>% 
  ungroup() %>% 
  left_join(., as.data.frame(table(.$primary.source)), by = c("primary.source" = "Var1")) %>% 
  mutate(
    duplicate = ifelse(
      Freq > 1, TRUE, FALSE
    )
  ) %>% 
  filter(duplicate == TRUE) %>% 
  # remove unnecessary columns
  select(-Freq, -duplicate) %>% 
  group_by(primary.source) %>% 
  summarise(secondary.duplicate.source.code = paste(secondary.source.code, collapse = ", "))

## 2) make a list of all distinct sources in the secondary source list, only need DOI, title, or author ones as primary data list doesn't have lmp or databases and then left join the duplicate column in primary source list when any match
secondary_source_list <- original_sources %>% 
  rename(
    source = primary.source
  ) %>% 
  filter(type == "data") %>% 
  distinct(source, .keep_all = TRUE) %>% 
  select(source, source.type) %>% 
  left_join(., select(primary_sources_duplicate_check, doi, source.code), by = c("source" = "doi")) %>% 
  left_join(., select(primary_sources_duplicate_check, Article.Title, source.code), by = c("source" = "Article.Title")) %>% 
  mutate(
    primary.duplicate.source.code = case_when(
      !is.na(source.code.x) ~ source.code.x,
      !is.na(source.code.y) ~ source.code.y,
      TRUE ~ NA
    )
  ) %>% 
  select(
    -source.code.x,
    -source.code.y
  ) %>% 
  left_join(., secondary_sources_duplicate_check, by = c("source" = "primary.source")) %>% 
  filter(
    !is.na(secondary.duplicate.source.code) | !is.na(primary.duplicate.source.code)
  )












