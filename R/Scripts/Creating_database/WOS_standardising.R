## Standadizing the WOS data

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
wos_raw_body <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "bodysize", guess_max = 40000)
wos_source_list <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "source_list")

#### Standardising ----
wos_standardised <- wos_raw_body %>%
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
  
  # remove redundent columns
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
    ),
    
    ## life stage
    # sort out captals
    life.stage = stri_replace_all_regex(life.stage, "A", "a"),
    
    # change to either adult or juvenile
    life.stage = case_when(
      # adults
      stri_detect_fixed(life.stage, "adult") ~ "adult",
      source.code %in% c("61", "263") & stri_detect_regex(life.stage, "C") ~ "adult",
      life.stage == "7 days" ~ "adult",
      is.na(life.stage) ~ "adult", # assume they are adults unless specified otherwise
      # juvenile
      stri_detect_regex(life.stage, "Copepodite|neonate|copepodid|juvenile|metamorphasis") ~ "juvenile",
      source.code %in% c("61", "263") & stri_detect_regex(life.stage, "N|Instar") ~ "juvenile",
      source.code == "5" & life.stage != "adult" ~ "juvenile",
      source.code == "66" ~ "juvenile",
      
      TRUE ~ life.stage
    ),
    
    ## format dates
    # make the same type
    ## american format = 8, 
    sample.start.year = as.character(sample.start.year),
    sample.end.year = as.character(sample.end.year),
    
    # Year
    sample.start.year = case_when(
      !is.na(sample.start.date.full) ~ stri_extract_first_regex(sample.start.date.full, "\\d{4}"),
      TRUE ~ sample.start.year
    ),
    
    sample.end.year = case_when(
      !is.na(sample.end.date.full) ~ stri_extract_first_regex(sample.end.date.full, "\\d{4}"),
      TRUE ~ sample.end.year
    ),
    
    # month
    sample.start.month = case_when(
      # american style date
      !is.na(sample.start.date.full) & source.code == "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<!\\/)\\d+(?=\\/)"), # take the two digits that don't have a / infront but have on following it - dd/
      
      # non-american style dates
      stri_detect_regex(sample.start.date.full, "\\/") & source.code != "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\/)\\d+(?=\\/)"), # take the two digits inbetween an / - /dd/
      stri_detect_regex(sample.start.date.full, "\\.") & source.code != "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\.)\\d+(?=\\.)"), # take the two digits inbetween an . - .dd.
      TRUE ~ sample.start.month
    ),
    
    sample.end.month = case_when(
      # american style date
      !is.na(sample.end.date.full) & source.code == "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<!\\/)\\d+(?=\\/)"), # take the two digits that don't have a / infront but have on following it - dd/
      
      # non-american style dates
      stri_detect_regex(sample.end.date.full, "\\/") & source.code != "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\/)\\d+(?=\\/)"), # take the two digits inbetween an / - /dd/
      stri_detect_regex(sample.end.date.full, "\\.") & source.code != "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\.)\\d+(?=\\.)"), # take the two digits inbetween an . - .dd.
      TRUE ~ sample.end.month
    ),
    
    # make it into ranges in one column
    # year
    sample.year = case_when(
      is.na(sample.end.year) ~ sample.start.year, # when there is only one sample year use that one
      sample.start.year == sample.end.year ~ sample.start.year, # when the start and end year are the same just keep one
      sample.start.year != sample.end.year ~ paste(sample.start.year, sample.end.year, sep = "-"), # when it is over multiple years make a range
      TRUE ~ NA),
    
    # month
    sample.month = case_when(
      is.na(sample.end.month) ~ sample.start.month, # when there is only one sample year use that one
      sample.start.month == sample.end.month ~ sample.start.month, # when the start and end year are the same just keep one
      sample.start.month != sample.end.month ~ paste(sample.start.month, sample.end.month, sep = "-"), # when it is over multiple years make a range
      TRUE ~ NA),
  ) %>% 
  # remove full date columns
  select(
    -sample.start.date.full,
    -sample.end.date.full,
    -sample.start.month,
    -sample.start.year,
    -sample.end.month,
    -sample.end.year
  )











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
  select(- used,- in.text.citation, - paper.code, - list.name)

write_csv(source_list_used, "R/Data_outputs/WOS/source_list_used.csv")


exp <- wos_raw_body %>% 
  filter(original.taxa.name == "Chlorella vulgaris")
