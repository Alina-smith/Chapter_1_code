## Standardising wos data

## packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## getting reference list from secondar sources into same format as WOS
## Data
zotero_source_list <- read_xlsx(here("Raw_data", "zotero original source list.xlsx"), sheet = "source_list")

# creatng columns for each part of the reference
original_source_list <- zotero_source_list %>% 



















## WOS cross referencing:
# cross referencing the original sources taken from secondary data papers with the data I already have
## Data
secondary_data_original_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "secondary_data_original_raw")
primary_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "sources_with_data")

## 1) List of sources in the original source list that are duplicated within that list and the source codes they are duplicated in
within_secondary_sources_duplicates <- secondary_data_original_sources %>% 
  # select only papers that have the data listed in them to cut down of retrieval time
  filter(type == "data") %>% 
  
  # group_by the source that cites the paper because and then get the distinct papers for each group - some papers have repeats within the same paper so want to remove these but keep the repeats netween papers so that I can see whats repeated and what paper it's repeated in
  group_by(citing.source.code) %>% 
  distinct(original.source, .keep_all = TRUE) %>%
  select(original.source, citing.source.code) %>% 
  ungroup() %>% 
  
  # Make a frequency table to see which sources are done more than once and left join it into the main data frame
  left_join(., as.data.frame(table(.$original.source)), by = c("original.source" = "Var1")) %>% 
  mutate(
    duplicate = ifelse(
      Freq > 1, TRUE, FALSE
    )
  ) %>% 
  
  # select duplicated papers
  filter(duplicate == TRUE) %>%
  
  # remove unnecessary columns
  select(-Freq, -duplicate) %>% 
  
  # get each distinct source and which sources they are duplicated in
  group_by(original.source) %>% 
  summarise(citing.source.codes = paste(citing.source.code, collapse = ", "))

## 2) List of any sources from the secondary_data_original_source list that I already have in the primary data or secondary data list and the source codes of them
primary_source_duplicates <- secondary_data_original_sources %>% 
  #rename(
  #source = primary.source
  #) %>% 
  # select only papers that have the data listed in them to cut down of retrieval time
  filter(type == "data") %>% 
  
  # group_by the source that cites the paper because and then get the distinct papers for each group - some papers have repeats within the same paper so want to remove these but keep the repeats netween papers so that I can see whats repeated and what paper it's repeated in
  group_by(citing.source.code) %>% 
  distinct(original.source, .keep_all = TRUE) %>%
  select(original.source, citing.source.code) %>% 
  ungroup() %>% 
  
  # left join the sources code of any matching sources from the primary and secondary sources list
  left_join(., select(primary_sources, doi, source.code), by = c("original.source" = "doi")) %>% 
  left_join(., select(primary_sources, Article.Title, source.code), by = c("original.source" = "Article.Title")) %>% 
  
  # join the two columns into one
  mutate(
    primary.source.codes = case_when(
      !is.na(source.code.x) ~ source.code.x,
      !is.na(source.code.y) ~ source.code.y,
      TRUE ~ NA
    )
  ) %>% 
  select(
    -source.code.x,
    -source.code.y
  ) %>% 
  filter(
    !is.na(primary.source.codes)
  )

## left joining source code onto original sources
original_edits <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "original_edits")
secondary_data_original_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "secondary_data_original_raw")

secondary_original_sources_filter <- secondary_data_original_sources %>% 
  filter(
    citing.source.code == "202"
  ) %>% 
  mutate(
    citing.source.code = as.character(citing.source.code)
  )

left_joining_sources <- original_edits %>% 
  mutate(
    citation = as.character(citation)
  ) %>% 
  left_join(select(secondary_original_sources_filter, citation, source.code), by = "citation")

write_csv(left_joining_sources, "Raw_data/left_joining_sources.csv")
  