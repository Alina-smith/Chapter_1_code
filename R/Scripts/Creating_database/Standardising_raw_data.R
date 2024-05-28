# 2/11/2023
# Organizing and standardizing the data in each individual database:
# Aim:
# standadize all the databases into the same units
# this is just a list of the measurements, location data and references for all the species, a list of the functional traits will be created in another script

## description of databases 
# Rimmet - average biovolume from literature, functional group classifications
# Kremer - raw biovolume measurements, no functional group classifications

# Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

########################################## data ====
#set relative file paths
database_path <- here("Raw_data", "Master biovolume_30 Oct 23.xlsx")


rimet <- read_xlsx(database_path, sheet = "Rimmet")
kremer <- read_xlsx(database_path, sheet = "Kremer")
odume <- read_xlsx(database_path, sheet = "Odume")
hebert <- read_xlsx(database_path, sheet = "Hebert")
Gavrilko <- read_xlsx(database_path, sheet = "Gavrilko")
LT <- read_xlsx(database_path, sheet = "Laplace-Treyture")
NO <- read_xlsx(database_path, sheet = "Neury-Ormanni")
rimet2012 <- read_xlsx(database_path, sheet = "Rimet 2012")
original_sources_raw <- read_xlsx(database_path, sheet = "original_source")
sources <- read_xlsx(database_path, sheet = "source")
location <- read_xlsx(database_path, sheet = "location")

##### Original source dataframe ----
# rimmet has been put first to make it easier with the ref.code numbering
original_source <- original_sources_raw %>% 
  rename(
    authors = Authors,
    year = Year,
    title = Title,
    publisher = Journal.Book,
    pub.type = Pub.type,
    ref.code = Ref.code
  ) %>% 
  mutate(
    original.source.code = row_number()
  ) %>% 
  relocate(original.source.code, ref.code, database, authors, year, title, publisher, pub.type)

saveRDS(original_source, file = "R/Data_outputs/Standardised_data/original_source.rds")

############################################################### Rimmet ====
# Reorganise and mutate
rimmet_subset <- rimet %>% 
  # Select columns I need and rename
  select(
    `Genus + species name`,
    `Cell biovolume µm3`,
    `Notes on biovolumes`,
    `Cumulated biovolume of cells in a colony µm3`,
    `Colony biovolume µm3 (without mucilage)`,
    `Number of cells per colony`,
  ) %>% 
  rename(
    original.taxa.name = `Genus + species name`,
    cell.biovol = `Cell biovolume µm3`,
    biovol.notes = `Notes on biovolumes`,
    nu.biovol = `Cumulated biovolume of cells in a colony µm3`,
    nu.biovol.mucilage = `Colony biovolume µm3 (without mucilage)`,
    cells.per.nu = `Number of cells per colony`
  ) %>%
  
  mutate(
    ## Make a life stage column
    # set TRUE to adult for now and then can change this to active for phyto when taxonomy info is added
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult"),
    
    ### Extracting info from biovol.notes
    ## Year (make all into in 0000 format)
    sample.start.year = case_when(
      stri_detect_regex(biovol.notes, "\\b\\d{4}[-\u2013]\\d{4}\\b|\\b\\d{4}\\b") ~ stri_extract_first_regex(biovol.notes, "\\b\\d{4}"),
      stri_detect_regex(biovol.notes, "\\b\\d{2}/\\d{2}/\\d{2}\\b|\\b\\d{2}-\\d{2}-\\d{2}\\b") ~ paste0("20",str_sub(stri_extract_all_regex(biovol.notes, "\\b\\d{2}/\\d{2}/\\d{2}\\b|\\b\\d{2}-\\d{2}-\\d{2}\\b"), start = -2)),
      stri_detect_regex(biovol.notes, "\\b\\d{1}/\\d{1}/\\d{1}\\b|\\b\\d{1}-\\d{1}-\\d{1}\\b") ~ paste0("200",str_sub(stri_extract_all_regex(biovol.notes, "\\b\\d{1}/\\d{1}/\\d{1}\\b|\\b\\d{1}-\\d{1}-\\d{1}\\b"), start = -1)),
      TRUE ~ NA
    ),
    sample.end.year = case_when(
      stri_detect_regex(biovol.notes, "\\b\\d{4}[-\u2013]\\d{4}\\b|\\b\\d{4}\\b") ~ stri_extract_last_regex(biovol.notes, "\\d{4}\\b"),
      stri_detect_regex(biovol.notes, "\\b\\d{2}/\\d{2}/\\d{2}\\b|\\b\\d{2}-\\d{2}-\\d{2}\\b") ~ paste0("20",str_sub(stri_extract_all_regex(biovol.notes, "\\b\\d{2}/\\d{2}/\\d{2}\\b|\\b\\d{2}-\\d{2}-\\d{2}\\b"), start = -2)),
      stri_detect_regex(biovol.notes, "\\b\\d{1}/\\d{1}/\\d{1}\\b|\\b\\d{1}-\\d{1}-\\d{1}\\b") ~ paste0("200",str_sub(stri_extract_all_regex(biovol.notes, "\\b\\d{1}/\\d{1}/\\d{1}\\b|\\b\\d{1}-\\d{1}-\\d{1}\\b"), start = -1)),
      TRUE ~ NA
    ),
    
    ## Season:
    sample.season = case_when(
      stri_detect_regex(biovol.notes, "(?i)hiver") ~ "winter",
      stri_detect_regex(biovol.notes, "(?i)été") ~ "summer"
    ),
    
    ## Location:
    # make location code column to use with left join by extract any location names that match the ones listed in the location.code column in location
    location.code = sapply(stri_extract_all_regex(biovol.notes, regex(paste0(location$location.code, collapse = "|"))), paste0, collapse = " "),
    location.code = str_remove_all(location.code, " SHL2"), # remove ones with the lake and monitoring program (" " before to keep the ones with just the monitoring program)
    location.code = na_if(location.code, "NA"),
    location.code = case_when(location.code == "Bourget Léman" ~ "Auvergne-Rhône-Alpes", TRUE ~ location.code) #when both lakes are put then change to the region
  ) %>%
  
  # left_join the location info in location for each location name in location.code
  left_join(., location, by = "location.code") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)
  ) %>% 
  select(- database) %>% 
  
  # Add in biovol.ref info - match any of the location codes with the ref.code in original source
  left_join(select(original_source, original.source.code, ref.code), by = c("location.code" = "ref.code")) %>% 
  rename(biovol.ref = original.source.code) %>% 
  mutate(biovol.ref = as.character(biovol.ref)) %>% 
  
  # remove redundant columns
  select(-location.code, - biovol.notes) %>% 
  
  # Add in extra info and reorder
  mutate(
    source.code = '1',
    sample.month = NA_character_,
  ) %>%
  relocate(source.code, original.taxa.name, life.stage, cell.biovol, cells.per.nu, nu.biovol, nu.biovol.mucilage, biovol.ref, sample.start.year, sample.end.year, sample.month, sample.season, location, country, continent, latitude, longitude)

saveRDS(rimmet_subset, file = "R/Data_outputs/Standardised_data/rimmet_subset.rds")

################################################################################### Kremer ====
# organize kremer raw data
# Reorganize and mutate
kremer_subset <- kremer %>% 
  
  # Select columns I need and rename
  select(location,
         original.taxa.name,
         nu,
         cell.biovol,
         data.source,
         sample.date,
         cells.per.nu,
         nu.biovol) %>% 
  rename(location.code = location,
         original.source = data.source) %>%
  
  # replace "NA" with NA
  mutate(
    nu = na_if(nu, "NA")) %>% 
  
  # remove any with no taxa.name
  filter(!is.na(original.taxa.name)) %>% 
  
  mutate(
    # make life.stage column
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      stri_detect_regex(nu, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(nu, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(nu,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(nu,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(nu,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult"),
    
    # reverse transfrom biovolume from log10
    cell.biovol = cell.biovol^10,
    nu.biovol = nu.biovol^10,
    
    # # sort out the missing cell.biovol from ansp, set as same as nu.biovol and it is assumed that is what it is
    cell.biovol = case_when(
      original.source == "ANSP" ~ nu.biovol,
      TRUE ~ cell.biovol
    ),
    
    # change cells.per.nu from NA to 1 when nu.biovol is the same as cell.biovol
    cells.per.nu = case_when(
      is.na(cells.per.nu) ~ ifelse(cell.biovol == nu.biovol, 1, NA),
      TRUE ~ cells.per.nu
    )
  ) %>% 
  
  # remove data points with no cell biovol or with nu.biovol but no information about cells.per.nu
  filter(!is.na(cells.per.nu)) %>% 
  
  # Add in location info and remove location.code
  # rename the ANSP names to make more manageable!!!!!!!!!!!!!!!!!!!!!!!
  left_join(., location, by = "location.code") %>%
  select(- location.code, - database) %>%
  mutate(
    latitude = as.character(latitude),
    longitude = as.character(longitude))%>% 
  
  # Extract sample dates from the original.source and add that to the column with sample date
  mutate(
    # make a column for year and month taken from sample.date and the dates in the original.source
    sample.start.year = case_when(stri_detect_regex(sample.date, "\\b\\d{4}\\b") ~ stri_extract_first_regex(sample.date, "\\b\\d{4}\\b"),
                                  stri_detect_regex(original.source, "\\b\\d{4}-\\d{4}\\b") ~ stri_extract_first_regex(original.source, "\\b\\d{4}\\b"),
                                  TRUE ~ NA),
    sample.end.year = case_when(stri_detect_regex(sample.date, "\\b\\d{4}\\b") ~ stri_extract_first_regex(sample.date, "\\b\\d{4}\\b"),
                                stri_detect_regex(original.source, "\\b\\d{4}-\\d{4}\\b") ~ stri_extract_last_regex(original.source, "\\b\\d{4}\\b"),
                                TRUE ~ NA),
    sample.month = case_when(stri_detect_regex(sample.date, "\\b\\d{4}-\\d{2}-\\d{2}\\b") ~ stri_extract_first_regex(sample.date, "-\\d{2}-"),
                             TRUE ~ NA),
    sample.month = stri_replace_all_regex(sample.month, "-", ""),
    # remove dates from the original.source column
    original.source = case_when(
      stri_detect_regex(original.source, "\\b.\\d{4}-\\d{4}\\b") ~ stri_replace_all_regex(original.source, "\\b.\\d{4}-\\d{4}\\b", ""),
      TRUE ~ original.source
    )
  ) %>% 
  # remove redundant columns
  select(- sample.date, -nu) %>% 
  
  # Add in biovol.ref info
  left_join(select(original_source, original.source.code, ref.code), by = c("original.source" = "ref.code")) %>% 
  select(- original.source) %>% 
  rename(biovol.ref = original.source.code) %>% 
  mutate(biovol.ref = as.character(biovol.ref)) %>% 
  
  # Add in extra info
  mutate(
    source.code = '2'
  ) %>%
  
  # Reorder
  relocate(source.code, original.taxa.name, life.stage, cell.biovol, nu.biovol, cells.per.nu, biovol.ref, sample.start.year, sample.end.year, sample.month, location, country, continent, latitude, longitude)

saveRDS(kremer_subset, file = "R/Data_outputs/Standardised_data/kremer_subset.rds")

############################################################### Odume ====

# Reorganise and mutate all
odume_subset_raw <- odume %>% 
  
  # Select columns I need and rename
  select(`Taxon`,
         `Measured body size (mm)`,
         `Maximum body size (mm) - comment`
  )%>% 
  rename(
    original.taxa.name = Taxon,
    body.size = `Measured body size (mm)`,
    body.size.comment = `Maximum body size (mm) - comment`
  ) %>%
  
  # Remove the first rows with extra column headers
  slice(-(1:11)) %>% 
  
  mutate(  
    # change the cm to mm so it's the same as all the others
    body.size = stri_replace_all_fixed(body.size, "7.0 cm", "70.0 mm"),
    
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      
      stri_detect_regex(body.size, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(body.size, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(body.size,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(body.size,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(body.size,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      
      stri_detect_regex(body.size.comment, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(body.size.comment, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(body.size.comment,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(body.size.comment,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(body.size.comment,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult")
  ) %>%
  
  # add in original.source infor for all the measurements
  mutate(ref.code = "odume et al") %>% 
  left_join(select(original_source, ref.code, original.source.code), by = "ref.code") %>% 
  mutate(
    length.ref = as.character(original.source.code),
    width.ref = as.character(original.source.code),
    height.ref = as.character(original.source.code)
  ) %>% 
  select(-original.source.code, -ref.code) %>% 
  
  # Add extra info and reorder
  mutate(
    location = "Southern Africa",
    country = "NA",
    continent = "Africa",
    latitude = "-24.175147",
    longitude = "19.227253",
    source.code = '3',
    sample.start.year = NA_character_,
    sample.end.year = NA_character_,
    sample.month = NA_character_
  )

# Extact measurments
odume_exact <- odume_subset_raw %>% 
  filter(
    !is.na(body.size) &
      !(stri_detect_regex(body.size, "(?i)length|(?i)height|(?i)width|(?i)\\bor\\b|(?i)height|longer|(?i)Approximately|(?i)Approx|-|\u2013")) &
      body.size != "Suhling et al. 2014"
  ) %>% 
  mutate(
    body.size = stri_replace_all_regex(body.size, "(Wiederholm 1983)", ""),
    body.size = ifelse(
      body.size == "Varies; one species 6.4 mm and the other 4.0 (de Moor et al. 2003).", 
      (6.4+4)/2,
      body.size
    ),
    avg.length = as.numeric(stri_extract_all_regex(body.size, "\\d+\\.?\\d*")),
    data.method = "exact", #this is what the data was at the start e.g. ranges, approx etc
  ) %>% 
  filter(!is.na(avg.length)) 


# Ranges
odume_ranges <- odume_subset_raw %>% 
  # get ranges
  filter(stri_detect_regex(body.size, "-|\u2013") |
           stri_detect_regex(body.size.comment, "-|\u2013") 
  ) %>% 
  
  # format ranges into min, max and average
  # extract just the ranges and remove all the other words and letters, there are some that have ranges in the comments but not the size column so extract those as well
  mutate(
    body.size = as.character(case_when(
      !is.na(body.size) ~ stri_extract_all_regex(body.size,"\\d+\\.?\\d*[-\u2013]\\d+\\.?\\d*|
                                                 \\d+\\.?\\d* [-\u2013] \\d+\\.?\\d*"),
      is.na(body.size) ~ ifelse(stri_detect_regex(body.size.comment, "mm"), stri_extract_all_regex(body.size.comment,"\\d+\\.?\\d*[-\u2013]\\d+\\.?\\d*|\\d+\\.?\\d*\\ [-\u2013] \\d+\\.?\\d*"), NA_character_),
      TRUE ~ NA
    )
    )
  ) %>% 
  # seperate them into min and max
  separate(body.size, into = c("min.length", "max.length"), sep = "-|-|\u2013", convert = TRUE) %>% 
  # create average column
  mutate(avg.length = ((min.length+max.length)/2)) %>% 
  
  # Add in extra info and reorder
  mutate(
    data.method = "Range", #this is what the data was at the start e.g. ranges, approx etc
  )


# Approx
odume_approx <- odume_subset_raw %>% 
  # select approx and remove any ranges that have been done in previous step
  filter(
    stri_detect_regex(body.size, "(?i)Approximately|(?i)Approx"),
    (!stri_detect_regex(body.size, "[-\u2013]"))
  ) %>% 
  
  mutate(
    # extract just the numbers and remove any words and letters
    avg.length = as.numeric(stri_extract_all_regex(body.size, "\\d+\\.\\d+")),
    
    # Add in extra info
    data.method = "Approximately"
  ) %>% 
  select(-body.size)

# Or
odume_or <- odume_subset_raw %>% 
  # Select or values and remove any that say or longer
  filter(
    stri_detect_regex(body.size, "\\bor\\b"),
    (!stri_detect_regex(body.size, "longer"))
  ) %>% 
  
  # Separate into min, max and make an average column
  separate(body.size, into = c("min.length", "max.length"), sep = "or", convert = TRUE) %>% 
  mutate(avg.length = (min.length+max.length)/2) %>% 
  
  # Add in extra info and reorder
  mutate(
    data.method = "Or"
  )

# Longer
odume_longer <- odume_subset_raw %>% 
  # Select or values and remove any that say or longer
  filter(
    stri_detect_regex(body.size, "longer")
  ) %>% 
  mutate(
    avg.length = as.numeric(stri_extract_all_regex(body.size, "\\d+\\.\\d+")),
    
    # Add extra info
    data.method = "LWH"
  ) %>% 
  
  # remove the Bl column
  select(- body.size)

# Length, width and height
odume_lwh <- odume_subset_raw %>% 
  # get just the values with any combination of length, width and height but without the ranges or <> values
  filter(
    stri_detect_regex(body.size, "(?i)length|(?i)height|(?i)width")|stri_detect_regex(body.size.comment, "(?i)length|(?i)height|(?i)width"),
    (!stri_detect_regex(body.size, "[<>]"))|is.na(body.size),
    (!stri_detect_regex(body.size, "[-\u2013]"))|is.na(body.size),
    (!stri_detect_regex(body.size, "\\bor\\b"))|is.na(body.size),
    (!stri_detect_regex(body.size.comment, "[<>]"))|is.na(body.size.comment),
    (!stri_detect_regex(body.size.comment, "[-\u2013]"))|is.na(body.size.comment),
    (!stri_detect_regex(body.size.comment, "\\bor\\b"))|is.na(body.size.comment),
    (!stri_detect_regex(body.size.comment, "\\bgreater\\b"))|is.na(body.size.comment),
    (!stri_detect_regex(body.size.comment, "\\bless\\b"))|is.na(body.size.comment),
  ) %>% 
  
  mutate(
    # remove mm and double spaces
    body.size = str_remove_all(body.size, "mm"),
    body.size = str_replace_all(body.size, "\\b\\s{2,}\\b|\\b\\s{3,}\\b", " "),
    body.size.comment = str_remove_all(body.size.comment, "mm"),
    body.size.comment = str_replace_all(body.size.comment, "\\b\\s{2,}\\b|\\b\\s{3,}\\b", " "),
    
    # separate out the values into columns
    avg.height = case_when(
      !is.na(body.size) ~ as.numeric(str_extract(body.size, "(?i)\\d+\\.?\\d* (?=height)")),
      is.na(body.size) ~ ifelse(
        stri_detect_regex(body.size.comment, "\\bheight\\b"), 
        as.numeric(str_extract(body.size.comment, "(?i)\\d+\\.?\\d* (?=height)")),
        NA),
      TRUE ~ NA
    ),
    avg.length = case_when(
      !is.na(body.size) ~ as.numeric(str_extract(body.size, "(?i)\\d+\\.?\\d* (?=length)")),
      is.na(body.size) ~ ifelse(
        stri_detect_regex(body.size.comment, "\\blength\\b"), 
        as.numeric(str_extract(body.size.comment, "(?i)\\d+\\.?\\d* (?=length)")),
        NA),
      TRUE ~ NA
    ),
    avg.width = case_when(
      !is.na(body.size) ~ as.numeric(str_extract(body.size, "(?i)\\d+\\.?\\d* (?=width)")),
      is.na(body.size) ~ ifelse(
        stri_detect_regex(body.size.comment, "\\bwidth\\b"), 
        as.numeric(str_extract(body.size.comment, "(?i)\\d+\\.?\\d* (?=width)")),
        NA),
      TRUE ~ NA
    ),
    
    # Add extra info
    data.method = "LWH"
  ) %>% 
  
  # remove the Bl column
  select(- body.size)

# Combine them all 
odume_subset <- bind_rows(odume_exact,odume_approx, odume_lwh, odume_or, odume_ranges, odume_longer) %>% 
  select(-body.size.comment) %>% 
  mutate(
    min.height = NA,
    max.height = NA,
    min.width = NA,
    max.width = NA
  ) %>% 
  relocate(source.code, original.taxa.name, life.stage, min.length, max.length, avg.length, length.ref, min.width, max.width, avg.width, width.ref, min.height, max.height, avg.height, height.ref, sample.start.year, sample.end.year, sample.month, location, country, continent, latitude, longitude)

saveRDS(odume_subset, file = "R/Data_outputs/Standardised_data/odume_subset.rds")

############################################################### Hebert ====
# Reorganise and mutate
hebert_subset <- hebert %>%
  
  # Select columns I need
  select(Column1, Column2, Column5, Column8, Column9, Column10, Column11, Column12, Column13, Column14, Column15) %>% 
  rename(
    genus = Column1,
    species = Column2,
    habitat = Column5,
    avg.length = Column8,
    min.length = Column9,
    max.length = Column10,
    length.ref = Column11,
    avg.biomass = Column12,
    min.biomass = Column13,
    max.biomass = Column14,
    biomass.ref = Column15
  ) %>%
  
  # Remove the first line with extra headings
  slice(-1) %>% 
  
  # filter out marine species and remove habitat column
  filter(habitat == "Freshwater") %>% 
  select(-habitat) %>% 
  
  # combine genus and species column to get a full taxa name and remove extra columns
  mutate(original.taxa.name = stri_c(genus, species, sep = " ")) %>% 
  select(-genus, -species) %>% 
  
  # Change "NA" to NA
  mutate_all(., ~na_if(., "NA"))%>% 
  
  # Change the commas to decimals
  mutate(  
    avg.length = as.numeric(stri_replace_all_regex(avg.length, ",", ".")),
    min.length = as.numeric(stri_replace_all_regex(min.length, ",", ".")),
    max.length = as.numeric(stri_replace_all_regex(max.length, ",", ".")),
    avg.biomass = as.numeric(stri_replace_all_regex(avg.biomass, ",", ".")),
    min.biomass = (stri_replace_all_regex(min.biomass, " ", "")), # get rid of unecessary spaces
    min.biomass = as.numeric(stri_replace_all_regex(min.biomass, ",", ".")),
    max.biomass = as.numeric(stri_replace_all_regex(max.biomass, ",", "."))
  ) %>% 
  
  #make a column to calculate the average length and average dm when only min and max are given and add it into avg.length 
  mutate(
    calc.avg.length = (min.length+max.length)/2,
    avg.length = coalesce(avg.length, calc.avg.length),
    calc.avg.biomass = (min.biomass+max.biomass)/2,
    avg.biomass = coalesce(avg.biomass, calc.avg.biomass)
  ) %>% 
  select(-calc.avg.length, - calc.avg.biomass) %>% 
  
  # when there is not value for avg.biomass but something in min or max take that as average
  mutate(
    avg.biomass = case_when(
      is.na(avg.biomass) & min.biomass > 0 ~ min.biomass,
      is.na(avg.biomass) & max.biomass > 0 ~ max.biomass,
      .default = avg.biomass
    )
  ) %>% 
  
  # convert avg.biomass to ug
  mutate(
    min.biomass = min.biomass*1000,
    max.biomass = max.biomass*1000,
    avg.biomass = avg.biomass*1000
  ) %>% 
  
  #Add in extra info
  mutate(
    source.code = '4',
    life.stage = "adult",
    min.width = NA,
    max.width = NA,
    avg.width = NA,
    min.height = NA,
    max.height = NA,
    avg.height = NA,
    location = NA_character_,
    country = NA_character_,
    continent = NA_character_,
    latitude = NA_character_,
    longitude = NA_character_,
    sample.start.year = NA_character_,
    sample.end.year = NA_character_,
    sample.month = NA_character_,
    data.method = NA_character_
  ) %>%
  relocate(source.code, original.taxa.name, biomass.ref, length.ref, sample.start.year, sample.end.year, sample.month, location, country, continent, latitude, longitude, min.length, max.length, avg.length, min.width, max.width, avg.width, min.height, max.height, avg.height, min.biomass, max.biomass, avg.biomass, data.method)

saveRDS(hebert_subset, file = "R/Data_outputs/Standardised_data/hebert_subset.rds")

# get all the refernces
hebert_ref_biomass <-  hebert_subset %>% 
  select(biomass.ref) %>% 
  separate(biomass.ref, into = c("1", "2", "3"), sep = ",", convert = TRUE) %>% 
  pivot_longer(cols = c("1","2","3"), values_to = "value") %>% 
  distinct(value) %>% 
  filter(!is.na(value)) %>% 
  rename(
    ref.code = value
  )%>% 
  mutate(
    ref.code = as.character(ref.code)
  ) %>% 
  left_join(., original_source, by = "ref.code")

hebert_ref_length <-  hebert_subset %>% 
  select(length.ref) %>% 
  separate(length.ref, into = c("1", "2", "3"), sep = ",", convert = TRUE) %>% 
  pivot_longer(cols = c("1","2","3"), values_to = "value") %>% 
  distinct(value)%>% 
  filter(!is.na(value)) %>% 
  rename(
    ref.code = value
  )%>% 
  mutate(
    ref.code = as.character(ref.code)
  ) %>% 
  left_join(., original_source, by = "ref.code")

############################################################### Gavrilko ----
gavrilko_subset <- Gavrilko %>% 
  ## Select columns I want
  select(
    species,
    max.body.size
  ) %>% 
  # rename to match other dataframes
  rename(
    original.taxa.name = species,
    avg.length = max.body.size
  ) %>% 
  # change "," to "." in avg.length
  mutate(
    avg.length = as.numeric(stri_replace_all_regex(avg.length, ",", "."))
  ) %>% 
  
  mutate(
    # make life stage column
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult"),
    
    # add in extra info
    source.code = "5",
    location = "European Russia",
    continent = "Europe"
  )

## Save
saveRDS(gavrilko_subset, file = "R/Data_outputs/Standardised_data/gavrilko_subset.rds")

###############################################################  Laplace-Treyture ----
lt_subset <- LT %>% 
  # Select columns I want
  select(
    Taxa_Name,
    Cell_Biovolume,
    Ind_Biovolume
  ) %>% 
  # rename to match other databases
  rename(
    original.taxa.name = Taxa_Name,
    cell.biovol = Cell_Biovolume,
    nu.biovol = Ind_Biovolume
  ) %>% 
  
  mutate(
    # change any NA strings to NA and change classes
    nu.biovol = as.numeric(na_if(nu.biovol, "#NA")),
    
    # calculate cells per nu
    cells.per.nu = nu.biovol/cell.biovol,
    
    # add life stage column
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult"),
  ) %>% 
  
  # Add in source info
  mutate(source.code = "6") %>% 
  # left_join(select(original_source, source.code, ref.code), by = c("source" = "ref.code")) %>% 
  # rename(biovol.ref = source.code) %>% 
  
  # Add in extra info
  mutate(
    sample.start.year = "2005",
    sample.end.year = "2016",
    sample.month = NA_character_,
    sample.season = NA_character_,
    location = NA_character_,
    country = "France",
    continent = "Europe",
    latitude = NA_character_,
    longitude = NA_character_,
    biovol.ref = NA_character_
  ) %>% 
  relocate(source.code, original.taxa.name, life.stage, cell.biovol, cells.per.nu, nu.biovol, biovol.ref, sample.start.year, sample.end.year, sample.month, sample.season, location, country, continent, latitude, longitude)

## Save
saveRDS(lt_subset, file = "R/Data_outputs/Standardised_data/lt_subset.rds")

###############################################################  Neury-Ormanni ----
no_subset <- NO %>% 
  # select columns i want
  select(
    Species,
    LengthMax,
    LengthMin,
    WidthMax,
    WidthMin,
  ) %>% 
  # rename to match other databases
  rename(
    original.taxa.name = Species,
    min.length = LengthMin,
    max.length = LengthMax,
    min.width = WidthMin,
    max.width = WidthMax
  ) %>% 
  mutate(
    # replace _ to " " in original.taxa.name
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "_", " "),
    
    # make numeric
    min.length = as.numeric(min.length),
    min.width = as.numeric(min.width),
    max.width = as.numeric(max.width),
    
    # make avg column
    avg.length = case_when(
      !is.na(min.length) & !is.na(max.length) ~ (min.length+max.length)/2,
      !is.na(min.length) & is.na(max.length) ~ min.length,
      is.na(min.length) & !is.na(max.length) ~ max.length
    ),
    avg.width = case_when(
      !is.na(min.width) & !is.na(max.width) ~ (min.width+max.width)/2,
      !is.na(min.width) & is.na(max.width) ~ min.width,
      is.na(min.width) & !is.na(max.width) ~ max.width
    )
  ) %>% 
  # remove any with no avg measurment
  filter(!is.na(avg.length)|!is.na(avg.width)) %>% 
  
  # add in extra info
  mutate(
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult"
    ),
    # add source
    source.code = "7"
  )

saveRDS(no_subset, file = "R/Data_outputs/Standardised_data/no_subset.rds")

###############################################################  Rimet 2012 ----   
rimet2012_subset <- rimet2012 %>% 
  select(
    `genus + species + var`,
    `Biovolume (µm3)`
  ) %>% 
  rename(
    original.taxa.name = `genus + species + var`,
    cell.biovol = `Biovolume (µm3)`
  ) %>% 
  # add life.stage column
  mutate(
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "stomatocyst",
      stri_detect_regex(original.taxa.name, "\\b(?i)palmeloid\\b") ~ "palmeloid",
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "nauplii",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b") ~ "larvae",
      stri_detect_regex(original.taxa.name,  "\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "nymph",
      TRUE ~ "adult"
    ),
    
    # add source
    source.code = "8"
  )

saveRDS(rimet2012_subset, file = "R/Data_outputs/Standardised_data/rimet2012_subset.rds")


############################################# Combine together ====
# combine all into one big database and then can separate out into types etc later
standardised_raw <- bind_rows(kremer_subset, rimmet_subset, hebert_subset, odume_subset, gavrilko_subset, lt_subset, no_subset, rimet2012_subset) %>% 
  # add in raw.uid
  mutate(
    raw.uid = row_number()
  )%>% 
  # remove data with a 0 cell.biovol (for some reason can't filter this so have to maunally removed based on uid)
  filter(!(raw.uid %in% c("3046", "14402", "36324", "43875", "117691", "120016", "124937", "132283", "133473", "203570"))) %>% 
  relocate(raw.uid, source.code, original.taxa.name, life.stage, cell.biovol, nu.biovol, nu.biovol.mucilage, cells.per.nu, biovol.ref, min.length, max.length, avg.length, length.ref, min.width, max.width, avg.width, width.ref, min.height, max.height, avg.height, height.ref, min.biomass, max.biomass, avg.biomass, biomass.ref, sample.start.year, sample.end.year, sample.month, sample.season, location, country, continent, latitude, longitude, data.method)

saveRDS(standardised_raw, file = "R/Data_outputs/Standardised_data/standardised_raw.rds")
