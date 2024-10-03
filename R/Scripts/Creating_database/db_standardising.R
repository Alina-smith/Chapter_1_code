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
  # form.no = no of individuals that have been measured - will be 1 for individuals
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
          paste0(
            "(?i)", paste0(
              filter(
                db_location, db.code == "1")$join.location, collapse = "|"
              )
            ) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
        )
      ),
      paste0, collapse = " "
      ),
    
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
          paste0(
            "(?i)", paste0(
              filter(
                db_source_list, db.code == "1")$join.source, collapse = "|"
              )
            ) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
        )
      ),
      paste0, collapse = " "
      ),
    
    # Make "NA" to NA
    join.source = case_when(
      join.source %in% c("NA", "na") ~ NA,
      TRUE ~ join.source
    )
  ) %>% 
  
  left_join(
    select(
      db_source_list, join.source, source.code
      ), by = "join.source"
    ) %>% 
  
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  mutate(
    original.source.code.1 = as.character(original.source.code.1) # make numerica for merging with other dataframes later on
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
    form.no = 1,
    
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
  relocate(individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, join.location.2, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)
  
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
    form.no = `Number of cells per colony`
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
          paste0(
            "(?i)", paste0(
              filter(
                db_location, db.code == "1")$join.location, collapse = "|"
              )
            ) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
        )
      ), paste0, collapse = " "
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
          paste0(
            "(?i)", paste0(
              filter(
                db_source_list, db.code == "1")$join.source, collapse = "|")
            ) #take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
          )
        ),
      paste0, collapse = " "
      ),
    
    # Make "NA" to NA
    join.source = case_when(
      join.source %in% c("NA", "na") ~ NA,
      TRUE ~ join.source
      )
  ) %>% 
  
  left_join(
    select(
      db_source_list, join.source, source.code
      ), by = "join.source"
    ) %>% 
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  mutate(
    original.source.code.1 = as.character(original.source.code.1) # make numerica for merging with other dataframes later on
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
  relocate(individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)

## join together ----
rimet_formatted <- bind_rows(rimet_ind, rimet_nu)

## Save ----
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
  
  # Sources
  left_join(
    select(
      filter(
        db_source_list, db.code == "2"
        ), source.code, join.source
      ), by = "join.source"
    ) %>% 
  
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  mutate(
    original.source.code.1 = as.character(original.source.code.1) # make numerica for merging with other dataframes later on
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
    form.no = 1,
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
  relocate(individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)
  
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
         form.no = cells.per.nu,
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
  
  # Sources
  left_join(
    select(
      filter(
        db_source_list, db.code == "2"
        ), source.code, join.source
      ), by = "join.source"
    ) %>% 
  
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  mutate(
    original.source.code.1 = as.character(original.source.code.1) # make numerica for merging with other dataframes later on
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
  relocate(individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)

## join together ----
kremer_formatted <- bind_rows(kremer_ind, kremer_nu)

## Save ----
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
    original.body.size = `Measured body size (mm)`,
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
    original.body.size = case_when(
      odume.uid == "1989" ~ stri_replace_all_regex(original.body.size, "7.0 cm", "70.0 mm"), # replace cm with mm
      is.na(original.body.size) & stri_detect_regex(body.size.comment, "\\bmm\\b") ~ body.size.comment, # When theres size info in body.size.coment but not body.size paste body.size.comment into bodysize
      TRUE ~ original.body.size
    ), 
    
    ## Life.stage
    life.stage = case_when(
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      stri_detect_regex(original.body.size,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      stri_detect_regex(body.size.comment,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      TRUE ~ "adult")
  ) %>%
  
  ## Add in extra info
  mutate(
    source.code = "3",
    original.source.code.1 = "3",
    sample.year = NA,
    sample.month = NA,
    join.location.1 = "southern Africa",
    form.no = 1,
    form = "individual",
    units = "mm"
  ) %>% 
  
  ## filter out ones with no body.size measurement
  filter(
    !is.na(original.body.size)
  ) %>% 
  
  ## Reorder
  relocate(odume.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, form, form.no, units)

## Exact measurements ----
odume_exact <- odume_formatted_raw %>% 
  ## Get only extact measuements 
  filter(
      !(stri_detect_regex(original.body.size, "(?i)\\bor\\b|(?i)Approximately|about|-|\u2013|(?i)up to|(?i)Less than |and|<|>|width")) &
      !(odume.uid %in% c("1657","2098", "2690", "97", "146", "1529", "2175", "2176", "2181", "2182", "1883", "2255")) # random odd ones
  ) %>% 
  mutate(
    ## Extract body sizes
    original.body.size = stri_replace_all_regex(original.body.size, "\\b\\d{4}\\)", ""), # remove date to make it easier to extract measurement numbers
    body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*"))
    ) %>% 
  
  ## add in extra info
  mutate(
    bodysize.measurement = case_when(
      stri_detect_regex(original.body.size, "(?i)high|(?i)height") ~ "body height",
      TRUE ~ "body length"
    ),
    measurement.type = "average",
    min.body.size = NA,
    max.body.size = NA,
    
    # make individual.uid
    uid.db = "OE", # stands for odume extact
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Remove redundent columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)


## Ranges ----
odume_ranges <- odume_formatted_raw %>% 
  ## Get ranges
  filter(
    stri_detect_regex(original.body.size, "-|\u2013|\\bor\\b") | odume.uid %in% c("1864", "1529"),
    # remove any that will be used in later steps
    !(stri_detect_regex(original.body.size, "longer")),
    !(stri_detect_regex(original.body.size, "diameter"))
    ) %>% 
  mutate(
    ## Format ranges into min, max and average
    original.body.size = case_when(
      # edit odd ones to make extracting easier
      odume.uid == "1440" ~ "11-14",
      odume.uid == "1989" ~ "20–70",
      odume.uid == "1529" ~ "1-10",
      odume.uid == "1864" ~ "4-6.4",
      odume.uid == "2628" ~ "60-61",
      odume.uid == "2624" ~ "60-62",
      odume.uid == "1529" ~ "1-10",
      # change "or" to "-"
      stri_detect_regex(original.body.size, " or ") ~ stri_replace_all_regex(original.body.size, " or ", "-"),
      TRUE ~ original.body.size
    ),
    
    # Extract just measurements
    body.size.range = stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*[-\u2013]\\d+(\\.\\d+)*"), # can use first as there are not multiple ranges in any of the ones i want to extract
    
    # Separate into min and max
    min.body.size = as.numeric(stri_extract_first_regex(body.size.range, "\\d+(\\.\\d+)*(?=[-\u2013])")),
    max.body.size = as.numeric(stri_extract_first_regex(body.size.range, "(?<=[-\u2013])\\d+(\\.\\d+)*")),
    
    ## Make ind.bodysize column
    body.size = (min.body.size+max.body.size)/2,
  
    ## Add in extra info
    measurement.type = "range",
    bodysize.measurement = "body length", # all are length so don't need to do a word search
    
    # make individual.uid
    uid.db = "OR", # stands for odume range
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## remove redundant columns
  select(
    - original.body.size,
    - body.size.range,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)

## Approx ----
odume_approx <- odume_formatted_raw %>% 
  ## select approx and remove any ranges that have been done in previous step or ones with max that will be used in next steps
  filter(
    stri_detect_regex(original.body.size, "(?i)Approximately|(?i)about"),
    !(stri_detect_regex(original.body.size, "[-\u2013]")),
    !(stri_detect_regex(original.body.size, "max"))
  ) %>% 
  
  mutate(
    ## Extract size
    body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")),
    
    ## Add in extra info
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "approximately",
    bodysize.measurement = "body length", # all are length so don't need to do a word search
    
    # make individual.uid
    uid.db = "OA", # stands for odume approx
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
    ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)

## Min ----
odume_min <- odume_formatted_raw %>% 
  
  ## select ones I want
  filter(
    stri_detect_regex(original.body.size, "(?i)\\blonger\\b|>|(?i)\\bover\\b|(?i)greater than"),
    !(odume.uid == "1529")
  ) %>% 
  
  mutate(
    ## Extract sizes
    min.body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")),
    
    ## Add in extra info
    body.size = NA,
    max.body.size = NA,
    measurement.type = "min",
    bodysize.measurement = "body length", # all are length so don't need to do a word search
    
    # make individual.uid
    uid.db = "OMIN", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)
  
## max ----
odume_max <- odume_formatted_raw %>% 
  
  ## select ones I want
  filter(
    stri_detect_regex(original.body.size, "(?i)up to|(?i)less than|<|(?i)\\brarely\\b|(?i)\\bmax\\b|(?i)grow up\\b"),
    !(stri_detect_regex(original.body.size, "(?i)\\bwidth\\b")),
    !(odume.uid == "170")
  ) %>% 
  
  mutate(
    ## Extract sizes
    max.body.size = as.numeric(stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")),
    
    ## add in extra info
    body.size = NA,
    min.body.size = NA,
    measurement.type = "max",
    bodysize.measurement = case_when(
      stri_detect_regex(original.body.size, "(?i)length|(?i)long") ~ "body length",
      stri_detect_regex(original.body.size, "(?i)height|(?i)hight|(?i)high") ~ "body height",
      stri_detect_regex(original.body.size, "(?i)diameter") ~ "body diameter",
      TRUE ~ "body length"
    ),
    
    # make individual.uid
    uid.db = "OMAX", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)

## duplication check ----
# check if any of the ones in the above odume formatting have been used twice by checking the odume.uid
odume_check <- bind_rows(odume_approx, odume_exact, odume_max, odume_min, odume_ranges) %>% 
  select(
    odume.uid
  ) %>% 
  # add in a duplicated column to use to check for duplicate in the lwh step
  mutate(
    duplicated = "yes"
  )

duplicate_check <- data.frame(table(odume_check$odume.uid)) %>% 
  filter(
    Freq > 1
  )

## Length, width and height ----
odume_lwh <- odume_formatted_raw %>% 
  # select ones I want
  filter(
    stri_detect_regex(original.body.size, "(?i)\\bwidth\\b|(?i)\\bheight\\b|(?i)\\bhight\\b|(?i)\\bhigh\\b|(?i)\\bheigh\\b|(?i)\\blong\\b")
  ) %>% 
  
  ## Check if any from the filter I have already used in the previous steps
  left_join(
    ., odume_check, by = "odume.uid"
    ) %>% 
  
  filter(
    is.na(duplicated) # remove duplicates
  ) %>% 
  
  mutate(
    # edit weird one - assume same format as the others of height length
    original.body.size = if_else(
      odume.uid == "169",
      "49 height 33 length 23 width",
      original.body.size
    ),
    
    # extract sizes
    `body height` = as.numeric(stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*(?= (?i)height| (?i)heigh)|\\d+(\\.\\d+)*(?= mm (?i)height| mm (?i)\\(height\\)| mm  (?i)height| mm  (?i)hight|mm (?i)height| mm (?i)heigh)")), #  can use first instead of all beacause theres only one value per row
    `body width` = as.numeric(stri_extract_first_regex(original.body.size, "\\b\\d+(\\.\\d+)*(?= (?i)width|(?i)width)|\\b\\d+(\\.\\d+)*(?= mm (?i)width| mm (?i)\\(width\\)|mm (?i)width)")),
    `body length` = as.numeric(stri_extract_first_regex(original.body.size, "\\b\\d+(\\.\\d+)*(?= mm (?i)length|mm (?i)length)|\\b\\d+(\\.\\d+)*(?= (?i)length)")),
    
    ## Add extra info
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    # make individual.uid
    uid.db = "OLWH", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## put length, width and height onto separate rows but with the same individual.uid
  pivot_longer(
    ., cols = `body height`:`body length`, values_to = "body.size", names_to = "bodysize.measurement"
    ) %>% 
  
  ## remove NA values in body.size
  filter(
    !is.na(body.size) 
  ) %>% 
  
  ## remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no,
    - duplicated
  ) %>% 
  
  # Reorder
  relocate(odume.uid, individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)

## Combine them all ----
odume_formatted <- bind_rows(odume_exact, odume_ranges, odume_lwh, odume_approx, odume_max, odume_min) %>% 
  # remove odume.uid now I don't need
  select(
    - odume.uid
  ) %>% 
  # Reorder
  relocate(individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form.no, form, bodysize.measurement, units, measurement.type)

## save ----
saveRDS(odume_formatted, file = "R/Data_outputs/databases/odume_formatted.rds")

# Hebert ----
## length ----
hebert_length <- hebert %>%
  
  # Select columns I need
  select(Column1, Column2, Column5, Column8, Column9, Column10, Column11) %>% 
  rename(
    genus = Column1,
    species = Column2,
    habitat = Column5,
    body.size = Column8,
    min.body.size = Column9,
    max.body.size = Column10,
    original.source.code.1 = Column11
  ) %>%
  
  # Remove the first line with extra headings
  slice(-1) %>% 
  
  # filter out marine species and remove habitat column
  filter(
    habitat == "Freshwater"
    ) %>% 
  
  select(
    -habitat
    ) %>% 
  
  # combine genus and species column to get a full taxa name and remove extra columns
  mutate(
    original.taxa.name = stri_c(genus, species, sep = " ")
    ) %>% 
  select(
    -genus, -species
    ) %>% 
  
  # Change "NA" to NA
  mutate_all(
    ., ~na_if(., "NA")
    ) %>% 
  
  mutate(
    ## Change the commas to decimals
    body.size = as.numeric(stri_replace_all_regex(body.size, ",", ".")),
    min.body.size = as.numeric(stri_replace_all_regex(min.body.size, ",", ".")),
    max.body.size = as.numeric(stri_replace_all_regex(max.body.size, ",", ".")),
    
    ## measurment type
    measurement.type = case_when(
      is.na(body.size) ~ "range",
      TRUE ~ "average"
    ),
    
    ## body.size
    # when body.size is Na but there is a range calculate an average
    body.size = case_when(
      is.na(body.size) ~ (min.body.size+max.body.size)/2,
      TRUE ~ body.size
    )
  ) %>% 
  
  ## original.source
  # separate original sources into separate columns
  separate(
    original.source.code.1, into = c("original.source.code.1", "original.source.code.2"), sep = ","
  ) %>% 
  
  ## remove NA body.size
  filter(
    !is.na(body.size)
  ) %>% 
  
  ## Add in extra info
  mutate(
    source.code = '4',
    life.stage = "adult",
    bodysize.measurement = "body length",
    units = "mm",
    sample.month = NA,
    sample.year = NA,
    form = "individual",
    form.no = 1,
    join.location.1 = NA,
    
    # make uid
    uid.db = "Hbl", # stands for hebert body length
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  ## Remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(individual.uid, source.code, original.source.code.1, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)

## dry mass ----
hebert_dm <- hebert %>%
  
  # Select columns I need
  select(Column1, Column2, Column5, Column12, Column13, Column14, Column15) %>% 
  rename(
    genus = Column1,
    species = Column2,
    habitat = Column5,
    body.size = Column12,
    min.body.size = Column13,
    max.body.size = Column14,
    original.source.code.1 = Column15
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
    body.size = as.numeric(stri_replace_all_regex(body.size, ",", ".")),
    min.body.size = stri_replace_all_regex(min.body.size, " ", ""), # get rid of unecessary spaces
    min.body.size = as.numeric(stri_replace_all_regex(min.body.size, ",", ".")),
    max.body.size = as.numeric(stri_replace_all_regex(max.body.size, ",", ".")),
    
    ## measurment type
    measurement.type = case_when(
      is.na(body.size) ~ "range",
      TRUE ~ "average"
    ),
    
    ## body.size
    # when body.size is Na but there is a range calculate an average
    body.size = case_when(
      is.na(body.size) ~ (min.body.size+max.body.size)/2,
      TRUE ~ body.size
    )
  ) %>% 
  
  ## original.source
  # separate original sources into separate columns
  separate(
    original.source.code.1, into = c("original.source.code.1", "original.source.code.2", "original.source.code.3", "original.source.code.4"), sep = ","
  ) %>% 
  
  ## remove NA body.size
  filter(
    !is.na(body.size)
  ) %>% 
  
  ## Add in extra info
  mutate(
    source.code = '4',
    life.stage = "adult",
    bodysize.measurement = "dry mass",
    units = "mg",
    sample.month = NA,
    sample.year = NA,
    form = "individual",
    form.no = 1,
    join.location.1 = NA,
    
    # make uid
    uid.db = "Hdm", # stands for hebert dry mass
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  ## Remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(individual.uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)

## Join together ----
hebert_formatted <- bind_rows(hebert_length, hebert_dm) %>% 
  # reorder
  relocate(individual.uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)

## Save ----
saveRDS(hebert_formatted, file = "R/Data_outputs/databases/hebert_formatted.rds")


# Gavrilko ----
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



db_formatted<- bind_rows(rimet_formatted, kremer_formatted, odume_formatted, hebert_formatted) %>% 
  # reorder
  relocate(individual.uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.taxa.name, life.stage, sample.year, sample.month, join.location.1, join.location.2, min.body.size, max.body.size, body.size, form, form.no, bodysize.measurement, units, measurement.type)








