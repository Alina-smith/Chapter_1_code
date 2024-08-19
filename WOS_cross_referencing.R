## Check through the list of original sources cited by the papers who didn't collect their own data and see if any of them cite papers within the secondary paper list or papers
#  that I already have

## packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## Data
original_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "secondary_data_original_raw")
primary_sources <- read_xlsx(here("Raw_data", "Master_WOS_data.xlsx"), sheet = "source_list_raw")

## 1) create a list of duplicated sources within the secondary source list and what sources they are duplicated in
secondary_sources_duplicate_check <- original_sources %>% 
  # select only papers that have the data listed in them to cut down of retrieval time
  #filter(type == "data") %>% 
  
  # group_by the source that cites the paper because and then get the distinct papers for each group - some papers have repeats within the same paper so want to remove these but keep the repeats netween papers so that I can see whats repeated and what paper it's repeated in
  group_by(secondary.source.code) %>% 
  distinct(primary.source) %>% 
  ungroup() %>% 
  
  # Make a frequency table to see which sources are done more than once and left join it into the main data frame
  left_join(., as.data.frame(table(.$primary.source)), by = c("primary.source" = "Var1")) %>% 
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
  group_by(primary.source) %>% 
  summarise(secondary.duplicate.source.code = paste(secondary.source.code, collapse = ", "))

## 2) Find any source that I already have in my primary source list and list the source code of the one I already have
primary_source_duplicate_check <- original_sources %>% 
  rename(
    source = primary.source
  ) %>% 
  filter(type == "data") %>% 
  distinct(source, .keep_all = TRUE) %>% 
  select(source, source.type, secondary.source.code) %>% 
  left_join(., select(primary_sources, doi, source.code), by = c("source" = "doi")) %>% 
  left_join(., select(primary_sources, Article.Title, source.code), by = c("source" = "Article.Title")) %>% 
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
  ) 

duplicate_check <- left_join(primary_source_duplicate_check, secondary_sources_duplicate_check, by = c("source" = "primary.source")) %>% 
  filter(
    !is.na(secondary.duplicate.source.code) | !is.na(primary.duplicate.source.code)
  )
%>% 
  mutate(
    duplicated = 
  )

















