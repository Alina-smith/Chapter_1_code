## standadising the WOS data dump

#### Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)
library(data.table)

#### Data ---- 
wos_raw_body <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "bodysize_data", guess_max = 40000)
sources_shortlist <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "sources_shortlist")

#### Standardising ----
wos_data <- wos_raw_body %>%
  mutate(
    ## body.size 
    # replacing , with .
    body.size = stri_replace_all_regex(body.size, ",", "."),
    
    # calculating values when average isn't given
    # make the same type
    total.bs = as.numeric(total.bs),
    abundance = as.numeric(abundance),
    body.size = as.numeric(body.size),
    min.body.size = as.numeric(min.body.size),
    max.body.size = as.numeric(max.body.size),
    
    body.size = case_when(
      # divide total population biovolume by abundance
      !is.na(total.bs) ~ total.bs/abundance,
      # take average of range values when the average isn't given already
      measurement.type == "range" & is.na(body.size) ~ (min.body.size*max.body.size)/2,
      # select min values
      measurement.type == "min" & is.na(body.size) ~ min.body.size,
      # select max values
      measurement.type == "max" & is.na(body.size) ~ max.body.size,
      # keep the rest the same
      TRUE ~ body.size
      )
    ) %>% 
  # remove NAs and 0 in source 71
  filter(
    !is.na(body.size) & body.size != "0"
  ) %>% 
  select(
    -total.bs,
    -abundance
  ) %>% 
  mutate(
    ## body size measurments
    # seperate out the body size measurement method into the method and then a column with additional notes on the method like what is measured etc
    body.size.method.notes = stri_extract_first_regex(body.size.method, "(?<=\\- ).*"),
    body.size.method = stri_replace_all_regex(body.size.method, " \\- .*", ""),
    
    # fix spelling mistakes
    body.size.method = stri_replace_all_regex(body.size.method, "B", "b",),
    
    ## units
    # change any mistakes or synonyms to the same
    units = case_when(
      units == "μg ind^-1" ~ "μg",
      units %in% c("fl/cell", "fl cell^-1") ~ "μm^3", # one femtoliter is the same as one micrometer cubed
      source.code == "16" ~ "mm", # the units were written wrong in the supplementary data, changed to units used in main paper
      TRUE ~ units
    ),
    
    # standardize to the same units
    body.size = case_when(
      units == "mg" ~ body.size*1000,
      units == "cm" ~ body.size*10,
      units == "nm" ~ body.size/1000,
      TRUE ~ body.size
    ),
    
    units = case_when(
      units == "mg" ~ "μg",
      units == "cm" ~ "mm",
      units == "nm" ~ "μm",
      TRUE ~ units
      )
    )
,
    
    ## life stage
    # sort out captals
    life.stage = stri_replace_all_regex(life.stage, "A", "a"),
    
    # change to either adult or juvenile
    life.stage = case_when(
      stri_detect_regex(life.stage, "Copepodite|neonate|copepodid|juvenile") ~ "juvenile",
      stri_detect_fixed(life.stage, "adult") ~ "adult",
      source.code == "263" & stri_detect_regex(life.stage, "Instar") ~ "juvenile",
      source.code == "263" & stri_detect_regex(life.stage, "Instar|C|N") ~ "juvenile",
      TRUE ~ life.stage
    )
    )

    
    ## format dates
    # make the same type
    sample.start.year = as.character(sample.start.year),
    sample.end.year = as.character(sample.end.year),
    sample.end.date.full = as.character(sample.end.date.full),
    
    # take year and month from sample.start.date.full
    sample.start.year = case_when(
      !is.na(sample.start.date.full) ~ stri_extract_first_regex(sample.start.date.full, "\\d{4}"),
      TRUE ~ sample.start.year
    ),
    
    sample.start.month = case_when(
      !is.na(sample.start.date.full) ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\d{4}-)\\d{2}"),
      TRUE ~ sample.start.year
    ),
    
    # take year from sample.end.year.full
    sample.end.year = case_when(
      !is.na(sample.end.date.full) ~ stri_extract_first_regex(sample.end.date.full, "\\d{4}"),
      TRUE ~ sample.end.year
    ),
    
    sample.end.month = case_when(
      !is.na(sample.end.date.full) ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\d{4}-)\\d{2}"),
      TRUE ~ sample.end.year
    ),
  ) %>% 
  select(-sample.end.date.full, -sample.start.date.full) %>% 
  mutate(
    ## life stage
    # sort out captals
    life.stage = stri_replace_all_regex(life.stage, "A", "a"),
    
    # change to either adult or juvenile
    life.stage = case_when(
      stri_detect_regex(life.stage, "Copepodite|neonate|copepodid|juvenile") ~ "juvenile",
      stri_detect_fixed(life.stage, "adult") ~ "adult",
      source.code == "263" & stri_detect_regex(life.stage, "Instar") ~ "juvenile",
      source.code == "263" & stri_detect_regex(life.stage, "Instar|C|N") ~ "juvenile",
      TRUE ~ life.stage
    )
  )



start_date <- wos_raw_body %>% 
  filter(
    !is.na(sample.start.date.full)
  ) %>% 
  mutate(
    keep = case_when(
      stri_detect_regex(sample.start.date.full, "/") ~ "no",
      TRUE ~ "yes"
    )
  ) %>% 
  filter(
    keep == "yes"
  ) %>% 
  select(
    source.code, sample.start.date.full, keep
  )

start_date_sources <- start_date %>% 
  distinct(source.code)

true <- wos_raw_body %>% 
  filter(
    sample.end.date.full == "TRUE"
  ) 
%>% 
  distinct(source.code)

eighty_four <- wos_raw_body %>% 
  filter(
    source.code == "84"
  )


twenty_five <- wos_data %>% 
  filter(
    source.code == "25"
  )

two_6_3 <- wos_data %>% 
  filter(
    source.code == "263"
  )


early_graph <- wos_data %>% 
  mutate(
    filter = case_when(
      body.size.method == "biovolume" ~ "phyto",
      body.size %in% c()
    )
  )
  
  filter(
    body.size.method == "biovolume" ~ "phyto",
    body.size
  )

two_0_3 <- wos_data %>% 
  filter(
    source.code == "203"
  )

life_stage <- wos_data %>% 
  filter(
    life.stage != "adult" & life.stage != "juvenile"
  )

full_date <- wos_data %>% 
  filter(
    !is.na(sample.start.date.full)
  )

sixteen <- wos_data %>% 
  filter(source.code == "16")

units <- wos_data %>% 
  distinct(units)

nm <- wos_data %>% 
  filter(units == "nm")

mg <- wos_data %>% 
  filter(units == "mg")

cm <- wos_data %>% 
  filter(units == "cm")

mm <- wos_data %>% 
  filter(units == "mm")

mm_type <- mm %>% 
  distinct(body.size.method)

    # edit the names
    body.size.method = case_when(
      body.size.method %in% c("body diameter", "cell diameter") ~ "diameter",
      body.size.method %in% c("body length", "cell length") ~ "diameter",
      TRUE ~ body.size.method
    )
  )

method <- wos_data %>% 
  distinct(body.size.method)


sources_code <- sources %>% 
  select(original.source.code)

source_check <- sources_code[duplicated(sources), ]

lake_names <- wos_raw_body %>% 
  filter(source.code == "82") %>% 
  distinct(paper.location.1)
  

#### cross referencing -----
## get a list of the source codes in the sources.shortlist sheet
sources_shortlist_codes <- sources_shortlist %>% 
  select(source.code)

## Get a list of source codes using in body.size sheet
# Source codes of primary data
primary_source_codes <- wos_data %>% 
  select(
    source.code) %>% 
  distinct(source.code) %>% 
  mutate(
    source.code = as.character(source.code),
    # make a column to say it is primary for later use
    source.type = "primary"
  )

# Source codes from secondary data
    # when it is a primary source then it will have the primary source as the original source as well and I want to remove these to make a list of just original sources cited in secondary source list
secondary_source_codes <- wos_data %>%
  
  # Select original.source.code columns
  select(
    original.source.code.1,
    original.source.code.2,
    original.source.code.3,
    original.source.code.4,
    original.source.code.5,
    original.source.code.6
  ) %>% 
  
  # make all the same type
  mutate(
    original.source.code.1 = as.character(original.source.code.1),
    original.source.code.2 = as.character(original.source.code.2),
    original.source.code.3 = as.character(original.source.code.3),
    original.source.code.4 = as.character(original.source.code.4),
    original.source.code.5 = as.character(original.source.code.5),
    original.source.code.6 = as.character(original.source.code.6)
  ) %>% 
  
  # put all sources codes into one column
  pivot_longer(
    cols = 1:6,
    values_to = "source.code"
    ) %>% 
  
  # remove duplicates
  distinct(source.code) %>% 
  
  # remove NAs 
  filter(!is.na(source.code)) %>% 
  
  ## Remove primary sources from original sources
  left_join(., primary_source_codes, by = "source.code") %>% 
  
  # add in secondary to source.type column
  mutate(
    source.type = if_else(
      is.na(source.type),"secondary", source.type
    )
  ) %>% 
  
  # select just secondary sources
  filter(
    source.type == "secondary"
  )

# combine secondary and primary source code
used_source_codes <- bind_rows(primary_source_codes, secondary_source_codes)

### Formatting the full source list
## Checking for sources I didn't end up using
delete_sources <- anti_join(sources_shortlist_codes, used_source_codes, by = "source.code") %>% 
  mutate(used = "not used")

## removing unused sources
source_list_used <- left_join(sources_shortlist, delete_sources, by = "source.code") %>% 
  mutate(
    used = if_else(
      is.na(used), "used", used
    )
  ) %>% 
  filter(
    used == "used"
  ) %>% 
  select(-used)
  
write.csv(source_list_used, )


  
  
  
  
  
  
  


  












