# 2/11/2023
# Organizing and standardizing the data in each individual database:
# Aim:
# standadize all the databases into the same units
# this is just a list of the measurements, location data and references for all the species, a list of the functional traits will be created in another script

## description of databases 
# Rimmet - average biovolume from literature, functional group classifications
# Kremer - raw biovolume measurements, no functional group classifications

## descriptions of columns
  # individual.uid = a uid for each individual so if there are mor than one measurment for an individual it will have the same individual.uid
  # source.code = the source I got the data from
  # original.source = the originalsource that th emeasurment was taken from - if measurments were taken by the authors of the database then it will be the same as the source code otherwise it will be different
  # original.taxa.name = taxa name as described in databse
  # life.stage = for zooplankton will either be adult or juvenile and for phyto will either be active or dormant
  # sample.year = the year the sample was taken, this will either be an exact year or range
  # sample month = the month the sample was taken, this will either be an exact month or range
  # join.location = a code to identify a location that is present in the location sheet, this will be used to left join location data once it has been merged with the WOS data
  # min/max/body.size = size measurement (separate column for min, max and body.size)
  # form = the form of body.size measurment indiidual/colony/filament/multi-celluar
  # no.individuals = no of individuals that have been measured - will be 1 for individuals
  # bodysize.measurement = what the body measurement is e.g length/biovolume
  # units = units for measurment
  # measurement.tpe = if it is an average /raw/range etc

# Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

# data ----
#set relative file paths
master_db_path <- here("Raw_data", "Master_db_traits.xlsx")


rimet <- read_xlsx(master_db_path, sheet = "Rimmet")
kremer <- read_xlsx(master_db_path, sheet = "Kremer")
odume <- read_xlsx(master_db_path, sheet = "Odume")
hebert <- read_xlsx(master_db_path, sheet = "Hebert")
Gavrilko <- read_xlsx(master_db_path, sheet = "Gavrilko")
LT <- read_xlsx(master_db_path, sheet = "Laplace-Treyture")
NO <- read_xlsx(master_db_path, sheet = "Neury-Ormanni")
rimet2012 <- read_xlsx(master_db_path, sheet = "Rimet_2012")
db_source_list <- read_xlsx(master_db_path, sheet = "source_list")
db_location <- read_xlsx(master_db_path, sheet = "location")

# Rimmet ----
### Separate it into two dataframes for individual measurements and nu measurments so that I can then join them back together and have have separate indivudal.uids for individual and colonial measurements

## Individual measurments ----
rimet_ind <- rimet %>% 
  # Select columns I need and rename
  select(
    `Genus + species name`,
    `Cell biovolume µm3`,
    `Notes on biovolumes`
  ) %>% 
  rename(
    original.taxa.name = `Genus + species name`,
    body.size = `Cell biovolume µm3`,
    biovol.notes = `Notes on biovolumes`
  ) %>% 
  
  mutate(
    ## Life stage
    # set TRUE to adult for now and then can change this to active for phyto when taxonomy info is added
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "dormant",
      TRUE ~ "adult"),
    
    ## Year
    sample.year = case_when(
      # 1) get all dates in 0000 format
      stri_detect_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})") ~ stri_extract_first_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})"),
      stri_detect_regex(biovol.notes, "\\d{4}-\\d{4}") ~ stri_extract_first_regex(biovol.notes, "\\d{4}-\\d{4}"),
      
      # 2) weird ones
      biovol.notes %in% c("Moyenne Annecy GL - 23/09/09", "Aiguebelette 28-09-09") ~ "2009",
      biovol.notes == "Moyenne sur Annecy 20087" ~ "2008",
      biovol.notes == "Mesures effectuées sur Annecy GL le 9/5/7, pas de correspondance taxo, forme crée pour la circonstance" ~ "2007",
      TRUE ~ NA),
    
    ## Month - do seperately because theres not many of them and they're all different
    sample.month = case_when(
      biovol.notes == "Mesures sur Bayssou - 06-2008" ~ "06",
      biovol.notes == "1 individu sur Annecy n 3-2007, de longeur 40 um" ~ "03",
      biovol.notes == "Moyenne Annecy GL - 23/09/09" ~ "09",
      biovol.notes == "Aiguebelette 15-10-2012" ~ "10",
      biovol.notes == "Aiguebelette 28-09-09" ~ "09",
      biovol.notes == "valeurs moyennes selon KLB 2/3 1991" ~ "03",
      biovol.notes == "Annecy GL - cloche 0-18 - 17/01/2007" ~ "01",
      biovol.notes == "Mesures effectuées sur Annecy GL le 9/5/7, pas de correspondance taxo, forme crée pour la circonstance" ~ "05",
      biovol.notes == "Mesure Aiguebelette 20-12-2011" ~ "12",
      TRUE ~ NA
    ),
    
    ## Location:
    # Make join.location column which will be used to left join location info later on - extract any location info from the biovol.notes that match the locations in the join.location column in db_location
    join.location.1 = sapply(
      stri_extract_all_regex(
        biovol.notes, regex(
          paste0("(?i)", paste0(filter(db_location, db.code == "1")$join.location, collapse = "|")) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
        )
      ),
      paste0, collapse = " "),
    
    # When two lakes are stated split into two columns
    join.location.2 = case_when(
      join.location.1 == "Bourget Léman" ~ "Léman",
      TRUE ~ NA
    ),
    
    join.location.1 = case_when(
      join.location.1 == "Bourget Léman" ~ "Bourget",
      TRUE ~ join.location.1
    ),
    
    # When its the lake monitoring program and the lake listed remove the lake monitoring program to just leave the lake
    join.location.1 = str_remove_all(join.location.1, " SHL2"),
    # set "NA" in location.code.1 to NA
    join.location.1 = na_if(join.location.1, "NA")
  ) %>% 
  
  ## Sources
  # Add in source.code column for any measurements that have a source in the biovol.notes
  mutate(
    # make join.source column to left join source codes to
    join.source = sapply(
      stri_extract_all_regex(
        biovol.notes, regex(
          paste0("(?i)", paste0(filter(db_source_list, db.code == "1")$join.source, collapse = "|")) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
        )
      ),
      paste0, collapse = " "),
    
    # Make "NA" to NA
    join.source = case_when(
      join.source %in% c("NA", "na") ~ NA,
      TRUE ~ join.source
    )
  ) %>% 
  
  left_join(select(db_source_list, join.source, source.code), by = "join.source") %>% 
  rename(
    original.source.code = source.code
  ) %>% 
  
  ## Add in extra info and reorder
  mutate(
    source.code = '1',
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    form = "individual",
    no.individuals = 1,
    
    # make uid
    uid.db = "RI", #stands for rimet individual
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>%
  
  ## remove redundant columns
  select(
    - biovol.notes,
    - join.source,
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(individual.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, join.location.2, min.body.size, max.body.size, body.size, form, no.individuals, bodysize.measurement, units, measurement.type)
  
## nu measurments ----
rimet_nu <- rimet %>% 
  # Select columns I need and rename
  select(
    `Genus + species name`,
    `Cell biovolume µm3`,
    `Notes on biovolumes`,
    `Cumulated biovolume of cells in a colony µm3`,
    `Number of cells per colony`,
    `Colonial`,
    `Filament`
  ) %>% 
  rename(
    original.taxa.name = `Genus + species name`,
    ind.bodysize = `Cell biovolume µm3`,
    biovol.notes = `Notes on biovolumes`,
    nu.bodysize = `Cumulated biovolume of cells in a colony µm3`,
    no.individuals = `Number of cells per colony`
  ) %>%
  
  ## Filter out ones that a cellular measurements
  filter(
    !(nu.bodysize == ind.bodysize)
  ) %>% 
  
  ## Rename nu.bodysize and remove ind.bodysize
  rename(
    body.size = nu.bodysize
  ) %>% 
  select(
    -ind.bodysize
  ) %>% 
  
  mutate(
    # Form
    # make form column using the colonial and filament columns
    form = case_when(
      Colonial == "1" & Filament == "1" ~ "multi-cell",
      Colonial == "1" ~ "colony",
      Filament == "1" ~ "filament"
    ),
    ## Life stage
    # set TRUE to adult for now and then can change this to active for phyto when taxonomy info is added
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "dormant",
      TRUE ~ "adult"),
    
    ## Year
    sample.year = case_when(
      # 1) get all dates in 0000 format
      stri_detect_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})") ~ stri_extract_first_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})"),
      stri_detect_regex(biovol.notes, "\\d{4}-\\d{4}") ~ stri_extract_first_regex(biovol.notes, "\\d{4}-\\d{4}"),
      
      # 2) weird ones
      biovol.notes %in% c("Moyenne Annecy GL - 23/09/09", "Aiguebelette 28-09-09") ~ "2009",
      TRUE ~ NA),
    
    ## Month - do seperately because theres not many of them and they're all different
    sample.month = case_when(
      biovol.notes == "Mesures sur Bayssou - 06-2008" ~ "06",
      biovol.notes == "1 individu sur Annecy n 3-2007, de longeur 40 um" ~ "03",
      biovol.notes == "Moyenne Annecy GL - 23/09/09" ~ "09",
      biovol.notes == "Aiguebelette 15-10-2012" ~ "10",
      biovol.notes == "Aiguebelette 28-09-09" ~ "09",
      TRUE ~ NA
    ),
    
    ## Location:
    # Make join.location column which will be used to left join location info later on - extract any location info from the biovol.notes that match the locations in the join.location column in db_location
    join.location.1 = sapply(
      stri_extract_all_regex(
        biovol.notes, regex(
          paste0("(?i)", paste0(filter(db_location, db.code == "1")$join.location, collapse = "|")) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
        )
      ),
      paste0, collapse = " "),
    
    # When its the lake monitoring program and the lake listed remove the lake monitoring program to just leave the lake
    join.location.1 = str_remove_all(join.location.1, " SHL2"),
    # set "NA" in location.code.1 to NA
    join.location.1 = na_if(join.location.1, "NA")
  ) %>% 
  
  ## Sources
  # Add in source.code column for any measurements that have a source in the biovol.notes
  mutate(
    # make join.source column to left join source codes to
    join.source = sapply(
      stri_extract_all_regex(
        biovol.notes, regex(
          paste0("(?i)", paste0(filter(db_source_list, db.code == "1")$join.source, collapse = "|")) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
          )
        ),
      paste0, collapse = " "),
    
    # Make "NA" to NA
    join.source = case_when(
      join.source %in% c("NA", "na") ~ NA,
      TRUE ~ join.source
      )
  ) %>% 
  
  left_join(select(db_source_list, join.source, source.code), by = "join.source") %>% 
  rename(
    original.source.code = source.code
  ) %>% 
  
  ## Add in extra info and reorder
  mutate(
    source.code = '1',
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    
    # make uid
    uid.db = "Rnu", # stands for rimet NU
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>%
  
  ## remove redundant columns
  select(
    - biovol.notes,
    - join.source,
    - Colonial,
    - Filament,
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(individual.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, no.individuals, bodysize.measurement, units, measurement.type)

## join together ----
rimet_formatted <- bind_rows(rimet_ind, rimet_nu)

## Save
saveRDS(rimet_formatted, file = "R/Data_outputs/databases/rimet_formatted.rds")

#Kremer ----
### Separate it into two dataframes for individual measurements and nu measurments so that I can then join them back together and have have separate indivudal.uids for individual and colonial measurements

## Individual measurments ----
kremer_ind <- kremer %>% 
  
  ## Select columns I need and rename
  select(
    location,
    original.taxa.name,
    cell.biovol,
    data.source,
    sample.date
    ) %>% 
  rename(
    join.location.1 = location,
    join.source = data.source,
    body.size = cell.biovol
  ) %>% 
  
  ## Remove any with no taxa.name or body.size measurement
  filter(
    !is.na(original.taxa.name) & !is.na(body.size)
    ) %>% 
  
  mutate(
    ## Bodysize
    # reverse transform bodysize from log10
    body.size = body.size^10,
    
    ## Sample Dates
    sample.year = case_when(
      stri_detect_regex(join.source, "\\b\\d{4}-\\d{4}\\b") ~ stri_extract_first_regex(join.source, "\\b\\d{4}-\\d{4}\\b"),
      stri_detect_regex(sample.date, "\\b\\d{4}\\b") ~ stri_extract_first_regex(sample.date, "\\b\\d{4}\\b")
    ),
    
    sample.month = stri_extract_first_regex(sample.date, "(?<=-)\\d{2}(?=-)"),
    
    # remove dates from the original.source column
    join.source = stri_replace_all_regex(join.source, "\\b.\\d{4}-\\d{4}\\b", "")
  ) %>% 
  
  # Add in biovol.ref info
  left_join(select(db_source_list, source.code, join.source), by = "join.source") %>% 
  rename(
    original.source.code = source.code
  ) %>% 
  
  ## Add in extra info
  mutate(
    source.code = '2',
    life.stage = "active",
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    no.individuals = 1,
    form = "individual",
    
    # make uid
    uid.db = "Kind",
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  ## Remove redundant columns
  select(
    - sample.date,
    - join.source,
    - uid.db,
    - uid.no
  )%>% 
  
  # reorder
  relocate(individual.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, no.individuals, bodysize.measurement, units, measurement.type)
  
## nu measurments ----
kremer_nu <- kremer %>% 
  
  ## Select columns I need and rename
  select(location,
         original.taxa.name,
         nu,
         cell.biovol,
         data.source,
         sample.date,
         cells.per.nu,
         nu.biovol) %>% 
  rename(join.location.1 = location,
         join.source = data.source,
         ind.bodysize = cell.biovol,
         no.individuals = cells.per.nu,
         nu.bodysize = nu.biovol) %>%
  
  ## filter to get only multi cell measurments 
  filter(
    !(ind.bodysize == nu.bodysize)
  ) %>% 
  
  ## remove ind.bodysize and rename nu.bodysize
  rename(
    body.size = nu.bodysize
  ) %>% 
  select(
    -ind.bodysize
  ) %>% 
  
  ## replace "NA" with NA
  mutate(
    nu = na_if(nu, "NA")
    ) %>% 
  
  mutate(
    ## Bodysize
    # reverse transform bodysize from log10
    body.size = body.size^10,
    
    ## Form
    # Set form to either colony or filament
    form = case_when(
      stri_detect_regex(nu, "Colonial|palmeloid") ~ "colony",
      stri_detect_regex(nu, "Filament") ~ "filament",
      TRUE ~ "multi-cellular"
      ),
    
    ## Sample Dates
    sample.year = case_when(
      stri_detect_regex(join.source, "\\b\\d{4}-\\d{4}\\b") ~ stri_extract_first_regex(join.source, "\\b\\d{4}-\\d{4}\\b"),
      stri_detect_regex(sample.date, "\\b\\d{4}\\b") ~ stri_extract_first_regex(sample.date, "\\b\\d{4}\\b")
      ),
    
    sample.month = stri_extract_first_regex(sample.date, "(?<=-)\\d{2}(?=-)"),
    
    # remove dates from the original.source column
    join.source = stri_replace_all_regex(join.source, "\\b.\\d{4}-\\d{4}\\b", "")
  ) %>% 
  
  # Add in biovol.ref info
  left_join(select(db_source_list, source.code, join.source), by = "join.source") %>% 
  rename(
    original.source.code = source.code
  ) %>% 
  
  ## Add in extra info
  mutate(
    source.code = '2',
    life.stage = "active",
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    
    # make uid
    uid.db = "Knu",
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  ## Remove redundant columns
  select(
    - sample.date,
    - join.source,
    - uid.db,
    - uid.no,
    - nu
    ) %>% 
  
  # reorder
  relocate(individual.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, no.individuals, bodysize.measurement, units, measurement.type)

## Kremer - join together
kremer_formatted <- bind_rows(kremer_ind, kremer_nu)

## Save
saveRDS(kremer_formatted, file = "R/Data_outputs/databases/kremer_formatted.rds")

# Odume ----

## Reorganise and mutate all ----
odume_formatted_raw <- odume %>% 
  
  # Select columns I need and rename
  select(
    `Taxon`,
    `Measured body size (mm)`,
    `Maximum body size (mm) - comment`
  )%>% 
  rename(
    original.taxa.name = Taxon,
    body.size = `Measured body size (mm)`,
    body.size.comment = `Maximum body size (mm) - comment`
  ) %>%
  
  # Remove the first rows with extra column headers
  slice(
    -(1:11)
    ) %>% 
  
  mutate( 
    ## Give temporary uids to make the extra formatted needed in this database easier
    odume.uid = row_number(),
    
    ## Chnages to body.size column change the one cm to mm so it's the same as all the others
    body.size = case_when(
      odume.uid == "1989" ~ stri_replace_all_regex(body.size, "7.0 cm", "70.0 mm"), # replace cm with mm
      is.na(body.size) & stri_detect_regex(body.size.comment, "\\bmm\\b") ~ body.size.comment, # When theres size info in body.size.coment but not body.size paste body.size.comment into bodysize
      TRUE ~ body.size
    ), 
    
    ## Life.stage
    life.stage = case_when(
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      stri_detect_regex(body.size,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      stri_detect_regex(body.size.comment,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      TRUE ~ "adult")
  ) %>%
  
  ## Add in extra info
  mutate(
    source.code = "3",
    original.source.code = "3",
    sample.year = NA,
    sample.month = NA,
    join.location.1 = "southern Africa",
    nu.bodysize = NA,
    min.nu.bodysize = NA,
    max.nu.bodysize = NA,
    individuals.per.nu = NA,
    nu = NA,
    units = "mm"
  ) %>% 
  
  ## filter out ones with no body.size measurement
  filter(
    !is.na(body.size)
  ) %>% 
  
  ## Reorder
  relocate(odume.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.nu.bodysize, max.nu.bodysize, nu.bodysize, individuals.per.nu, nu, units, body.size, body.size.comment)

## Exact measurements ----
odume_exact <- odume_formatted_raw %>% 
  ## Get only extact measuements 
  filter(
    !is.na(body.size) &
      !(stri_detect_regex(body.size, "(?i)\\bor\\b|(?i)Approximately|about|-|\u2013|(?i)up to|(?i)Less than |and|<|>|width")) &
      !(odume.uid %in% c("1657","2098", "2690", "97", "146", "1529", "2175", "2176", "2181", "2182", "1883")) # random odd ones
  ) %>% 
  mutate(
    ## Extract body sizes
    body.size = stri_replace_all_regex(body.size, "\\b\\d{4}\\)", ""), # remove date to make it easier to extract measurement numbers
    ind.bodysize = as.numeric(stri_extract_all_regex(body.size, "\\d+(\\.\\d)*"))
    ) %>% 
  
  ## add in extra info
  mutate(
    bodysize.measurement = "body length",
    measurement.type = "average",
    min.ind.bodysize = NA,
    max.ind.bodysize = NA,
    
    # make individual.uid
    uid.db = "OE", # stands for odume extact
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Remove redundent columns
  select(
    - body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.ind.bodysize, max.ind.bodysize, ind.bodysize, min.nu.bodysize, max.nu.bodysize, nu.bodysize, individuals.per.nu, nu, bodysize.measurement, units, measurement.type)


## Ranges ----
odume_ranges <- odume_formatted_raw %>% 
  ## Get ranges
  filter(
    stri_detect_regex(body.size, "-|\u2013")
    ) %>% 
  mutate(
    ## Format ranges into min, max and average
    # edit two odd ones to make extraxting easier
    body.size = case_when(
      odume.uid == "1440" ~ "11-14",
      odume.uid == "1989" ~ "20–70",
      TRUE ~ body.size
    ),
    
    # Extract just measurements
    body.size.range = stri_extract_first_regex(body.size, "\\d+(\\.\\d)*[-\u2013]\\d+(\\.\\d)*"), # can use first as there are not multiple ranges in any of the ones i want to extract
    
    # Separate into min and max
    min.ind.bodysize = as.numeric(stri_extract_first_regex(body.size.range, "\\d+(\\.\\d)*(?=[-\u2013])")),
    max.ind.bodysize = as.numeric(stri_extract_first_regex(body.size.range, "(?<=[-\u2013])\\d+(\\.\\d)*")),
    
    ## Make ind.bodysize column
    ind.bodysize = (min.ind.bodysize+max.ind.bodysize)/2,
  
    ## Add in extra info
    measurement.type = "range",
    bodysize.measurement = "body length",
    
    # make individual.uid
    uid.db = "OR", # stands for odume range
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## remove redundant columns
  select(
    - body.size,
    - body.size.range,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.ind.bodysize, max.ind.bodysize, ind.bodysize, min.nu.bodysize, max.nu.bodysize, nu.bodysize, individuals.per.nu, nu, bodysize.measurement, units, measurement.type)


## Approx ----
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

## Or ----
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

## Longer ----
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

## Length, width and height ----
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

## Combine them all ----
odume_formatted <- bind_rows(odume_exact, odume_ranges)

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
standardised_databases <- bind_rows(kremer_subset, rimmet_subset, hebert_subset, odume_subset, gavrilko_subset, lt_subset, no_subset, rimet2012_subset) %>% 
  # add in raw.uid
  mutate(
    raw.uid = row_number()
  )%>% 
  # remove data with a 0 cell.biovol (for some reason can't filter this so have to maunally removed based on uid)
  filter(!(raw.uid %in% c("3046", "14402", "36324", "43875", "117691", "120016", "124937", "132283", "133473", "203570"))) %>% 
  relocate(raw.uid, source.code, original.taxa.name, life.stage, cell.biovol, nu.biovol, nu.biovol.mucilage, cells.per.nu, biovol.ref, min.length, max.length, avg.length, length.ref, min.width, max.width, avg.width, width.ref, min.height, max.height, avg.height, height.ref, min.biomass, max.biomass, avg.biomass, biomass.ref, sample.start.year, sample.end.year, sample.month, sample.season, location, country, continent, latitude, longitude, data.method)

saveRDS(standardised_raw, file = "R/Data_outputs/Standardised_data/standardised_raw.rds")



db_formatted <- bind_rows(rimet_formatted, kremer_formatted)







