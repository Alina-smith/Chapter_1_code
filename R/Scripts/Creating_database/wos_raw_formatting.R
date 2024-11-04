## Standardising wos data

## packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## Getting data from secondary sources
# my list data
original_sources <- read_xlsx(here("Raw_data","master_wos_data.xlsx"), sheet = "secondary_data_original_raw")
## 204
# left join the source.code from my list to the correspinding source in their list
# data
raw_204 <- read.csv(here("Raw_data/wos_formatting", "204.csv"))

# left join source code
edit_204 <- raw_204 %>% 
  left_join(., select(original_sources, citation, source.code), by = c("Source.1" = "citation")) %>% 
  rename(source.code.1 = source.code) %>% 
  left_join(., select(original_sources, citation, source.code), by = c("Source.2" = "citation")) %>% 
  rename(source.code.2 = source.code) %>% 
  left_join(., select(original_sources, citation, source.code), by = c("Source.3" = "citation")) %>% 
  rename(source.code.3 = source.code) %>% 
  left_join(., select(original_sources, citation, source.code), by = c("Source.4" = "citation")) %>% 
  rename(source.code.4 = source.code)

write_csv(edit_204, "R/Data_outputs/wos_formatting/204_edit.csv")

## 208
raw_208 <- read_xlsx(here("Raw_data/wos_formatting","208.xlsx"), sheet = "Table S2")

edit_208 <- raw_208 %>% 
  mutate(
    min_size = case_when(
      stri_detect_regex(`Individual cell volumes (μm³)`, ">") ~ `Individual cell volumes (μm³)`,
      stri_detect_regex(`Individual cell volumes (μm³)`, "~") ~ stri_extract_first_regex(`Individual cell volumes (μm³)`, "\\S*(?=~)"),
      TRUE ~ NA
    ),
    min_size = stri_replace_all_regex(min_size, ">", ""),
    
    max_size = case_when(
      stri_detect_regex(`Individual cell volumes (μm³)`, "<") ~ `Individual cell volumes (μm³)`,
      stri_detect_regex(`Individual cell volumes (μm³)`, "~") ~ stri_extract_first_regex(`Individual cell volumes (μm³)`, "(?<=~)\\S*"),
      TRUE ~ NA
    ),
    max_size = stri_replace_all_regex(max_size, "<", ""),
    
    min_length = case_when(
      stri_detect_regex(`Maximum linear size (μm)`, ">") ~ `Maximum linear size (μm)`,
      stri_detect_regex(`Maximum linear size (μm)`, "~") ~ stri_extract_first_regex(`Maximum linear size (μm)`, "\\S*(?=~)"),
      TRUE ~ NA
    ),
    min_length = stri_replace_all_regex(min_length, ">", ""),
    
    max_length = case_when(
      stri_detect_regex(`Maximum linear size (μm)`, "<") ~ `Maximum linear size (μm)`,
      stri_detect_regex(`Maximum linear size (μm)`, "~") ~ stri_extract_first_regex(`Maximum linear size (μm)`, "(?<=~)\\S*"),
      TRUE ~ NA
    ),
    max_length = stri_replace_all_regex(max_length, "<", "")
  )

write_csv(edit_208, "R/Data_outputs/wos_formatting/208_edit.csv")

## 223
edit_223 <- read_xlsx(here("Raw_data/wos_formatting", "223.xlsx"), sheet = "223") %>% 
  mutate(
    min.body.size = case_when(
      BioVol_C1 == "1" ~ "5",
      BioVol_C2 == "1" ~ "100",
      BioVol_C3 == "1" ~ "300",
      BioVol_C4 == "1" ~ "600",
      BioVol_C5 == "1" ~ "1500",
    ),
    
    max.body.size = case_when(
      BioVol_C1 == "1" ~ "100",
      BioVol_C2 == "1" ~ "300",
      BioVol_C3 == "1" ~ "600",
      BioVol_C4 == "1" ~ "1500",
      TRUE ~ NA
    ),
    
    type = case_when(
      is.na(max.body.size) ~ "min",
      TRUE ~ "range"
    )
  )
write_csv(edit_223, "R/Data_outputs/wos_formatting/223_edit.csv")


## getting reference list from secondary sources into same format as WOS
## Data
zotero_source_list_raw <- read_xlsx(here("Raw_data/wos_formatting", "zotero_original_source_list.xlsx"), sheet = "source_list")

# creating columns for each part of the reference
zotero_source_list_edit <- zotero_source_list_raw %>% 
  mutate(
    source.code = row_number(),
    
    # author
    authors = case_when(
      source.code == "24" ~ "Armitage, P.D., Cranston, P.S. and Pinder, L.C.V.",
      source.code == "45" ~ "Bellinger, E.G. and Sigee, D.C.",
      source.code == "56" ~ "Bledzki, L.A. and Rybak, J.I.",
      source.code == "80" ~ "Brodie, J. and Lewis, J.",
      source.code == "156" ~ "Duff, K.E., Zeeb, B.A. and Smol, J.P.",
      source.code == "293" ~ "Jones, E.B.G. and Pang, K.-L.",
      source.code == "331" ~ "Kofoid, C.A. and Swezy, O.",
      source.code == "406" ~ "Lynn, D.H.",
      source.code == "436" ~ "Mehlhorn, H.",
      source.code == "527" ~ "Reynolds, C.S.",
      TRUE ~  stri_extract_first_regex(source, ".*(?=\\(\\d{4}\\) ‘|\\(\\d{4}\\w\\) ‘)")
    ),
    
    # year
    year = stri_extract_first_regex(source, "\\(\\d{4}\\)|\\(\\d{4}\\w*\\)"),# select the date in the bracket
    year = stri_replace_all_regex(year, "\\(|\\)", ""), # remove bracket
    
    # title
    title = case_when(
      source.code == "484" ~ "On the bicoecidae: a family of colourless flagellates",
      source.code == "563" ~ "SEASONAL VARIATION IN LENGTH OF COPEPODIDS AND ADULTS OF DIACYCLOPS THOMASI (FORBES) IN TWO COLORADO MONTANE RESERVOIRS (COPEPODA)",
      source.code == "393" ~ "Time series of phytoplankton biovolume at the depth of the vertical chlorophyll maximum in Falling Creek Reservoir, Vinton, VA, USA 2016-2019",
      source.code == "707" ~ "Marine Protists: Diversity and Dynamics",
      TRUE ~ stri_extract_first_regex(source, "(?<=\\(\\d{4}\\) ‘|\\(\\d{4}\\w\\) ‘).*(?=’,)")
    ),
    
    #doi
    doi = stri_extract_first_regex(source, "https://doi.org/(?<=https://doi.org/).*"),
    doi = stri_replace_last_regex(doi, "\\.", ""), # get rid of full stop at the end
    doi = stri_replace_last_regex(doi, "https://doi.org/", ""),
    
    # page
    page = stri_extract_first_regex(source, "(?<= pp. | p. ).*(?=\\. Available)"),
    
    # volume and issue
    volume = stri_extract_first_regex(source, ", \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))*"), # easier to extract the volume and issue together with the regex and then separate them
    issue = case_when(
      source.code == "508" ~ "308",
      source.code == "531" ~ "308",
      TRUE ~ stri_extract_first_regex(volume, "\\(\\d+(–\\d+)*\\)")
    ),
    volume = stri_replace_all_regex(volume, "\\(\\d+(–\\d+)*\\)", ""),
    volume = stri_replace_all_regex(volume, "\\, ", "")
    ) %>% 
  select(-source.code) %>% 
  left_join(., original_sources_citations_list, by = c("doi" = "original.source"))


## left join sources onto doi list with original citations
# data
original_sources_citations_list <- read_xlsx(here("Raw_data/wos_formatting", "zotero_original_source_list.xlsx"), sheet = "in_text_citations")

zotero_source_list <- original_sources_citations_list %>% 
  left_join(., zotero_source_list_edit, by = )



## 1312
# left join the source.code from my list to the correspinding source in their list
# data
raw_1312 <- read.csv(here("Raw_data/wos_formatting", "1312.csv"))

edit_1312 <- raw_1312 %>% 
  filter(
    MarksInOcularMicrometer_No. != ""
  ) %>% 
  
  mutate(
    body.size = case_when(
      
    )
  )
