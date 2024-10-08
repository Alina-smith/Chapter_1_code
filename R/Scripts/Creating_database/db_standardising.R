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

# Packages ----
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
db_extra <- read_xlsx(master_db_path, sheet = "extra_info")

# Rimmet ----
rimet_formatted <- rimet %>% 
  ## Select columns ----
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
    cell.bs = `Cell biovolume µm3`,
    biovol.notes = `Notes on biovolumes`,
    nu.bs = `Cumulated biovolume of cells in a colony µm3`,
    form.no = `Number of cells per colony`
  ) %>% 
  
  mutate(
    ## Filter ----
    # set nu.bs to NA when it is the same as cell.bs to filter out later on
    nu.bs = if_else(
      nu.bs == cell.bs,
      NA,
      nu.bs
    ),
    
    ## Life stage ----
    # set TRUE to adult for now and then can change this to active for phyto when taxonomy info is added
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "dormant",
      TRUE ~ "adult"),
    
    ## Sample dates ----
    # Year
    sample.year = case_when(
      # 1) get all dates in 0000 format
      stri_detect_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})") ~ stri_extract_first_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})"),
      stri_detect_regex(biovol.notes, "\\d{4}-\\d{4}") ~ stri_extract_first_regex(biovol.notes, "\\d{4}-\\d{4}"),
      
      # 2) weird ones
      biovol.notes %in% c("Moyenne Annecy GL - 23/09/09", "Aiguebelette 28-09-09") ~ "2009",
      biovol.notes == "Moyenne sur Annecy 20087" ~ "2008",
      biovol.notes == "Mesures effectuées sur Annecy GL le 9/5/7, pas de correspondance taxo, forme crée pour la circonstance" ~ "2007",
      TRUE ~ NA),
    
    # Month - do seperately because theres not many of them and they're all different
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
    
    ## Location ----
    # Make join.location column which will be used to left join location info later on - extract any location info from the biovol.notes that match the locations in the join.location column in db_location
    join.location.1 = sapply(
      stri_extract_all_regex(
        biovol.notes, regex(
          paste0(
            "(?i)", paste0(
              filter(
                db_location, db.code == "1")$join.location, collapse = "|"
            )
          ) # take all the the values in the db_source_list$join.source column that are from the rimmet db (1) and make them into a string seperated by | to turn it into a regex of place names seperated by the OR (|) operator
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
    join.location.1 = na_if(join.location.1, "NA"),
    
    ## Sources ----
    # Add in source.code column for any measurements that have a source in the biovol.notes
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
  
  ## Pivot ----
  # put nu and cell biovolumes on seperate rows - don't need the same individual uid because they are different individuals
  pivot_longer(
    cols = c(cell.bs, nu.bs), names_to = "cell.nu", values_to = "body.size"
  ) %>% 
  
  # Remove NA body.size using column made in previous filter step
  filter(
    !is.na(body.size)
  ) %>% 
  
  mutate(
    ## Form ----
    form = case_when(
      cell.nu == "cell.bs" ~ "individual",
      cell.nu == "nu.bs"& Colonial == "1" & Filament == "1" ~ "multi-cell",
      cell.nu == "nu.bs"& Colonial == "0" & Filament == "0" ~ "multi-cell",
      cell.nu == "nu.bs" & Colonial == "1" ~ "colony",
      cell.nu == "nu.bs" & Filament == "1" ~ "filament"
    ),
    
    ## Form.no ----
    # Change form.no to 1 for indivduals
    form.no = if_else(
      form == "individual",
      1,
      form.no
    ),
    
    ## Extra info ----
    source.code = '1',
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    sample.size = "unknown",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "ex-situ",
      
    ## Individual.uid ----
    uid.db = "rimetF", # stands for rimet formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # Remove redundant columns
  select(
    - biovol.notes,
    - Colonial,
    - Filament,
    - cell.nu,
    - uid.db,
    - uid.no,
    - join.source
  ) %>% 
  
  # Reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1, join.location.2,
            individual.uid, original.taxa.name, life.stage, sex, form, form.no,
            min.body.size, max.body.size, body.size,
            bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(rimet_formatted, file = "R/Data_outputs/databases/rimet_formatted.rds")

# Kremer ----
kremer_formatted <- kremer %>% 
  ## Select columns ----
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
         cell.bs = cell.biovol,
         form.no = cells.per.nu,
         nu.bs = nu.biovol) %>%
  
  ## Filter ----
  # Remove any with no taxa.name
  filter(
    !is.na(original.taxa.name)
    ) %>% 
  
  # Set nu.bs to NA when it is the same as cell.bs to filter out later on
  mutate(
    nu.bs = if_else(
      nu.bs == cell.bs,
      NA,
      nu.bs
    ),
    
    ## Sample Dates ----
    sample.year = case_when(
      stri_detect_regex(join.source, "\\b\\d{4}-\\d{4}\\b") ~ stri_extract_first_regex(join.source, "\\b\\d{4}-\\d{4}\\b"),
      stri_detect_regex(sample.date, "\\b\\d{4}\\b") ~ stri_extract_first_regex(sample.date, "\\b\\d{4}\\b")
    ),
    
    sample.month = stri_extract_first_regex(sample.date, "(?<=-)\\d{2}(?=-)"),
    
    # remove dates from the original.source column
    join.source = stri_replace_all_regex(join.source, "\\b.\\d{4}-\\d{4}\\b", "")
  ) %>% 
  
  ## Sources ----
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
    original.source.code.1 = as.character(original.source.code.1) # make character for merging with other dataframes later on
  ) %>% 
  
  ## Pivot ----
  # put nu and cell biovolumes on seperate rows - don't need the same individual uid because they are different individuals
  pivot_longer(
    cols = c(cell.bs, nu.bs), names_to = "cell.nu", values_to = "body.size"
  ) %>% 
  
  # remove NA body.size using column made in previous filter step
  filter(
    !is.na(body.size)
  ) %>% 
  
  mutate(
    ## body.size ----
    # reverse transform body.size measurments from log10
    body.size = body.size^10,
    
    ## Form ----
    form = case_when(
      cell.nu == "cell.bs" ~ "individual",
      cell.nu == "nu.bs" & stri_detect_regex(nu, "Colonial|palmeloid") ~ "colony",
      cell.nu == "nu.bs" & stri_detect_regex(nu, "Filament") ~ "filament",
      TRUE ~ "multi-cellular"
    ),
    
    ## Form.no ----
    # change form.no to 1 for individual measurements
    form.no = if_else(
      form == "individual",
      1,
      form.no
      ),
    
    ## Extra info ----
    source.code = '2',
    life.stage = "active",
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    sample.size = "unknown",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "in-situ",
      
    ## Individual uid ----
    uid.db = "kremerF", # stands for kremer formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # Remove redundant columns
    select(
      - sample.date,
      - join.source,
      - uid.db,
      - uid.no,
      - cell.nu,
      - nu
    ) %>% 
      
    # Reorder
    relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
              individual.uid, original.taxa.name, life.stage, sex, form, form.no,
              min.body.size, max.body.size, body.size,
              bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(kremer_formatted, file = "R/Data_outputs/databases/kremer_formatted.rds")

# Odume - Reorganise and mutate all ----
odume_formatted_raw <- odume %>% 
  
  ## Select columns ----
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
  
  ## Filter ----
  # Remove the first rows with extra column headers
  slice(
    -(1:11)
    ) %>% 
  
  mutate( 
    ## Temp uid ----
    # Give temporary uids to make the extra formatted needed in this database easier
    odume.uid = row_number(),
    
    ## Body.size ----
    # Chnages to body.size column change the one cm to mm so it's the same as all the others
    original.body.size = case_when(
      odume.uid == "1989" ~ stri_replace_all_regex(original.body.size, "7.0 cm", "70.0 mm"), # replace cm with mm
      is.na(original.body.size) & stri_detect_regex(body.size.comment, "\\bmm\\b") ~ body.size.comment, # When theres size info in body.size.coment but not body.size paste body.size.comment into bodysize
      TRUE ~ original.body.size
    )
  ) %>% 
  
  # filter out ones with no body.size measurement
  filter(
    !is.na(original.body.size)
  ) %>% 
  
  mutate(  
    ## Life.stage ----
    life.stage = case_when(
      stri_detect_regex(original.taxa.name,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      stri_detect_regex(original.body.size,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      stri_detect_regex(body.size.comment,  "\\b(?i)Larvae\\b|\\b(?i)Nymph\\b|\\b(?i)Nymphs\\b") ~ "juvenile",
      TRUE ~ "adult")
  ) %>%
  
  ## Extra info ----
  mutate(
    source.code = "3",
    original.source.code.1 = "3",
    sample.year = NA,
    sample.month = NA,
    join.location.1 = "southern Africa",
    form.no = 1,
    form = "individual",
    units = "mm",
    sample.size = "unknown",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "in-situ",
  ) %>% 
  
  ## Order columns
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           original.taxa.name, life.stage, sex, form, form.no,
           original.body.size, units, sample.size, reps, error, error.type)

# Odume - Exact measurements ----
odume_exact <- odume_formatted_raw %>% 
  
  ## Filter ----
  # Get only extact measuements 
  filter(
      !(stri_detect_regex(original.body.size, "(?i)\\bor\\b|(?i)Approximately|about|-|\u2013|(?i)up to|(?i)Less than |and|<|>|width")) &
      !(odume.uid %in% c("1657","2098", "2690", "97", "146", "1529", "2175", "2176", "2181", "2182", "1883", "2255")) # random odd ones
  ) %>% 
  
  mutate(
    ## Body sizes ----
    # Extract measurments
    original.body.size = stri_replace_all_regex(original.body.size, "\\b\\d{4}\\)", ""), # remove date to make it easier to extract measurement numbers
    body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*"))
    ) %>% 
  
  ## bodysize.measurement ----
  mutate(
    bodysize.measurement = case_when(
      stri_detect_regex(original.body.size, "(?i)high|(?i)height") ~ "body height",
      TRUE ~ "body length"
    ),
    
    ## Extra info ----
    measurement.type = "average",
    min.body.size = NA,
    max.body.size = NA,
    
    ## Individual.uid ----
    uid.db = "odumeE", # stands for odume extact
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # Remove redundent columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Ranges ----
odume_ranges <- odume_formatted_raw %>% 
  
  ## Filter ----
  # Get ranges
  filter(
    stri_detect_regex(original.body.size, "-|\u2013|\\bor\\b") | odume.uid %in% c("1864", "1529"),
    
    # remove any that will be used in later steps
    !(stri_detect_regex(original.body.size, "longer")),
    !(stri_detect_regex(original.body.size, "diameter"))
    ) %>% 
  
  mutate(
    ## Body.size ----
    # Format ranges into min, max and average
    
    # edit original.body.size to make extractng easier
    original.body.size = case_when(
      # edit odd ones
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
    
    # calculate average body size from min and max
    body.size = (min.body.size+max.body.size)/2,
  
    ## Extra info ----
    measurement.type = "range",
    bodysize.measurement = "body length", # all are length so don't need to do a word search
    
    ## Individual.uid
    uid.db = "odumeR", # stands for odume range
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.range,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Approx ----
odume_approx <- odume_formatted_raw %>% 
  
  ## Filter ----
  # select approx and remove any ranges that have been done in previous step or ones with max that will be used in next steps
  filter(
    stri_detect_regex(original.body.size, "(?i)Approximately|(?i)about"),
    !(stri_detect_regex(original.body.size, "[-\u2013]")),
    !(stri_detect_regex(original.body.size, "max"))
  ) %>% 
  
  mutate(
    ## Body.size ----
    # extract measurments from origiona.body.size column
    body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")),
    
    ## Extra info ----
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "approximately",
    bodysize.measurement = "body length", # all are length so don't need to do a word search
    
    ## Individual.uid ----
    uid.db = "odumeA", # stands for odume approx
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
    ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Min ----
odume_min <- odume_formatted_raw %>% 
  
  ## Filter ----
  filter(
    stri_detect_regex(original.body.size, "(?i)\\blonger\\b|>|(?i)\\bover\\b|(?i)greater than"),
    !(odume.uid == "1529")
  ) %>% 
  
  mutate(
    ## Body.size ----
    # Extract measurements from original.body.size
    min.body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")),
    
    ## Extra info ----
    body.size = NA,
    max.body.size = NA,
    measurement.type = "min",
    bodysize.measurement = "body length", # all are length so don't need to do a word search
    
    ## Individual.uid ----
    uid.db = "odumeMIN", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - max ----
odume_max <- odume_formatted_raw %>% 
  
  ## Filter ----
  filter(
    stri_detect_regex(original.body.size, "(?i)up to|(?i)less than|<|(?i)\\brarely\\b|(?i)\\bmax\\b|(?i)grow up\\b"),
    !(stri_detect_regex(original.body.size, "(?i)\\bwidth\\b")),
    !(odume.uid == "170")
  ) %>% 
  
  mutate(
    ## body.size ----
    # extract measurements from original.body.size
    max.body.size = as.numeric(stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")),
    
    ## bodysize.measurement ----
    bodysize.measurement = case_when(
      stri_detect_regex(original.body.size, "(?i)length|(?i)long") ~ "body length",
      stri_detect_regex(original.body.size, "(?i)height|(?i)hight|(?i)high") ~ "body height",
      stri_detect_regex(original.body.size, "(?i)diameter") ~ "body diameter",
      TRUE ~ "body length"
    ),
    
    ## Extra info ----
    body.size = NA,
    min.body.size = NA,
    measurement.type = "max",
    
    ## Individual.uid ----
    uid.db = "odumeMAX", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - duplication check ----
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

# Odume - Length, width and height ----
odume_lwh <- odume_formatted_raw %>% 
  
  ## Filter ----
  filter(
    stri_detect_regex(original.body.size, "(?i)\\bwidth\\b|(?i)\\bheight\\b|(?i)\\bhight\\b|(?i)\\bhigh\\b|(?i)\\bheigh\\b|(?i)\\blong\\b")
  ) %>% 
  
  # Check if any from the filter I have already used in the previous steps
  left_join(
    ., odume_check, by = "odume.uid"
    ) %>% 

  filter(
    is.na(duplicated) # remove duplicates
  ) %>% 
  
  mutate(
    ## body.size
    # edit weird one to make extracting easier - assume same format as the others of height length
    original.body.size = if_else(
      odume.uid == "169",
      "49 height 33 length 23 width",
      original.body.size
    ),
    
    # extract measurements from original.body.size
    `body height` = as.numeric(stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*(?= (?i)height| (?i)heigh)|\\d+(\\.\\d+)*(?= mm (?i)height| mm (?i)\\(height\\)| mm  (?i)height| mm  (?i)hight|mm (?i)height| mm (?i)heigh)")), #  can use first instead of all beacause theres only one value per row
    `body width` = as.numeric(stri_extract_first_regex(original.body.size, "\\b\\d+(\\.\\d+)*(?= (?i)width|(?i)width)|\\b\\d+(\\.\\d+)*(?= mm (?i)width| mm (?i)\\(width\\)|mm (?i)width)")),
    `body length` = as.numeric(stri_extract_first_regex(original.body.size, "\\b\\d+(\\.\\d+)*(?= mm (?i)length|mm (?i)length)|\\b\\d+(\\.\\d+)*(?= (?i)length)")),
    
    ## Extra info ----
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    
    ## Individual.uid
    uid.db = "odumeLWH", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Pivot ----
  # put length, width and height onto separate rows but with the same individual.uid as they are measurements of the same individual
  pivot_longer(
    ., cols = `body height`:`body length`, values_to = "body.size", names_to = "bodysize.measurement"
    ) %>% 
  
  # remove NA values in body.size
  filter(
    !is.na(body.size) 
  ) %>% 
  
  ## Order columns ----
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
    - uid.db,
    - uid.no,
    - duplicated
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Combine them all ----
odume_formatted <- bind_rows(odume_exact, odume_ranges, odume_lwh, odume_approx, odume_max, odume_min) %>% 
  # remove odume.uid now I don't need
  select(
    - odume.uid
  ) %>% 
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## save ----
saveRDS(odume_formatted, file = "R/Data_outputs/databases/odume_formatted.rds")

# Hebert ----
hebert_formatted <- hebert %>%
  
  ## Select columns ----
  select(Column1, Column2, Column5, Column8, Column9, Column10, Column11, Column12, Column13, Column14, Column15) %>% 
  rename(
    genus = Column1,
    species = Column2,
    habitat = Column5,
    `body length` = Column8, # have it with a sapce as then can just assign that column as the bodysize.measurement and don't need to change anything
    min.bl = Column9,
    max.bl = Column10,
    bl.original.source.code = Column11,
    `dry mass` = Column12, # have it with a sapce as then can just assign that column as the bodysize.measurement and don't need to change anything
    min.dm = Column13,
    max.dm = Column14,
    dm.original.source.code = Column15
  ) %>%
  
  ## Filter ----
  # Remove the first line with extra headings
  slice(-1) %>% 
  
  # select freshwater species
  filter(
    habitat == "Freshwater"
  ) %>% 
  
   # remove habitat column
  select(
    - habitat
  ) %>% 
  
  ## General changes ----
  # Combine genus and species column to get a full taxa name and remove extra columns
  mutate(
    original.taxa.name = stri_c(genus, species, sep = " ")
  ) %>% 
  
  # remove redundant columns
  select(
    -genus, -species
  ) %>% 
  
  # Change "NA" to NA
  mutate_all(
    ., ~na_if(., "NA")
  ) %>% 
  
  # change the commas to decimals
  mutate(
    `body length` = as.numeric(stri_replace_all_regex(`body length`, ",", ".")),
    min.bl = as.numeric(stri_replace_all_regex(min.bl, ",", ".")),
    max.bl = as.numeric(stri_replace_all_regex(max.bl, ",", ".")),
    `dry mass` = as.numeric(stri_replace_all_regex(`dry mass`, ",", ".")),
    min.dm = as.numeric(stri_replace_all_regex(min.dm, ",", ".")),
    max.dm = as.numeric(stri_replace_all_regex(max.dm, ",", "."))
  ) %>% 
  
  ## Pivot ----
  # Make it so dry mass and body length are n seperate lines - don't need same individual.uid as they are from different references
  pivot_longer(
    cols = c(`body length`, `dry mass`), names_to = "bodysize.measurement", values_to = "body.size"
  ) %>% 
  
  mutate(
    # assign correct min, max and references for each one as they will have been duplicated on multiple rows from the pivot
    min.body.size = case_when(
      bodysize.measurement == "body length" ~ min.bl,
      bodysize.measurement == "dry mass" ~ min.dm,
      TRUE ~ NA
    ),
    
    max.body.size = case_when(
      bodysize.measurement == "body length" ~ max.bl,
      bodysize.measurement == "dry mass" ~ max.dm,
      TRUE ~ NA
    ),
    
    original.source.code = case_when(
      bodysize.measurement == "body length" ~ bl.original.source.code,
      bodysize.measurement == "dry mass" ~ dm.original.source.code,
      TRUE ~ NA
    )
  ) %>% 
  
  # remove redundant columns
  select(
    - min.bl,
    - max.bl,
    - bl.original.source.code,
    - min.dm,
    - max.dm,
    - dm.original.source.code,
  ) %>% 
  
  mutate(
    ## Measurement.type ----
    measurement.type = if_else(
      is.na(body.size),
      "range",
      "average"
    ),
    ## Units ----
    units = if_else(
      bodysize.measurement == "body length",
      "mm",
      "mg"
    ),
    
    ## body.size ----
    # calculate average body size when only range is given
    body.size = if_else(
      is.na(body.size),
      (min.body.size + max.body.size)/2,
      body.size
    )
  ) %>% 
  
  # filter only ones that have a body size measurement using column made in previous step
  filter(
    !is.na(body.size)
  ) %>% 
  
  ## Original.source.code ----
  # separate original sources into separate columns
  separate(
    original.source.code, into = c("original.source.code.1", "original.source.code.2", "original.source.code.3", "original.source.code.4"), sep = ","
  ) %>% 
  
  ## sample.dates, sample.size, sex, experiemntal.design ----
  # left join info in the location sheet
  left_join(
    filter(db_extra, db.code == "4"),
  by = c("original.source.code.1" = "join.extra")
  ) %>% 
  
  ## Extra info ----
  mutate(
    ## join.location
    # the join location if the same as the source code in db_location except for some specific sepcies
    join.location.1 = original.source.code.1, 
    join.location.2 = original.source.code.2, 
    join.location.3 = original.source.code.3, 
    join.location.4 = original.source.code.4, 
    source.code = '4',
    life.stage = "adult",
    form = "individual",
    form.no = 1,
    sample.size = "unknown",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    
    ## Indivudal.uid ----
    uid.db = "hebertF", # stands for hebert formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no,
    - db.code
  ) %>% 
  
  # Reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, experimental.design, sample.year, sample.month, join.location.1, join.location.2, join.location.3, join.location.4,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(hebert_formatted, file = "R/Data_outputs/databases/hebert_formatted.rds")

# Gavrilko ----
gavrilko_formatted <- Gavrilko %>% 
  
  ## Select columns ----
  select(
    species,
    max.body.size
  ) %>% 
  
  rename(
    original.taxa.name = species,
    body.size = max.body.size
  ) %>% 
  
  ## General changes ----
  # change "," to "." in avg.length
  mutate(
    body.size = as.numeric(stri_replace_all_regex(body.size, ",", "."))
  ) %>% 
  
  mutate(
    ## Life.stage ----
    life.stage = case_when(
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b") ~ "juvenile",
      TRUE ~ "adult"),
    
    ## Extra info ----
    source.code = '5',
    original.source.code.1 = "216",
    original.source.code.2 = "217",
    original.source.code.3 = "218",
    original.source.code.4 = "219",
    original.source.code.5 = "220",
    original.source.code.6 = "221",
    original.source.code.7 = "222",
    original.source.code.8 = "223",
    original.source.code.9 = "224",
    original.source.code.10 = "225",
    original.source.code.11 = "226",
    original.source.code.12 = "227",
    original.source.code.13 = "228",
    original.source.code.14 = "229",
    original.source.code.15 = "230",
    original.source.code.16 = "231",
    original.source.code.17 = "232",
    original.source.code.18 = "233",
    original.source.code.19 = "234",
    bodysize.measurement = "body length",
    units = "mm",
    sample.month = NA,
    sample.year = NA,
    form = "individual",
    form.no = 1,
    join.location.1 = "Gavrilko et al., 2021",
    max.body.size = NA,
    min.body.size = NA,
    measurement.type = "average",
    sample.size = "unknown",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "ex-situ",
      
    ## Individual.uid ----
    uid.db = "gavrilkoF", # stands for gavrilko formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Order columns ----
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18, original.source.code.19,
           experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save
saveRDS(gavrilko_formatted, file = "R/Data_outputs/databases/gavrilko_formatted.rds")

#  Laplace-Treyture ----

lt_formatted <- LT %>% 
  ## Select columns ----
  select(
    Taxa_Name,
    Cell_Biovolume,
    Ind_Biovolume,
    Life_Form
  ) %>% 
  
  rename(
    original.taxa.name = Taxa_Name,
    cell.bs = Cell_Biovolume,
    nu.bs = Ind_Biovolume,
    form = Life_Form
  ) %>% 
  
  mutate(
    ## Filter ----
    # set form.bs to NA when it is the same as cell.bs to filter out later on
    nu.bs = if_else(
      nu.bs == cell.bs,
      NA,
      nu.bs
    ),
    
    ## Body.size ----
    # change "#NA" to NA
    nu.bs = as.numeric(na_if(nu.bs, "#NA"))
  ) %>% 
  
  ## Pivot ----
  # put nu and cell biovolumes on seperate rows - don't need the same individual uid because they are different individuals
  pivot_longer(
    cols = cell.bs:nu.bs, values_to = "body.size", names_to = "cell.nu"
  ) %>% 
  
  # remove NA body.sizes using column from filter step
  filter(
    !is.na(body.size)
  ) %>% 
  
  mutate(
    ## Form ----
    form = case_when(
      cell.nu == "cell.bs" ~ "individual",
      cell.nu == "nu.bs" & form == "Col." ~ "colony",
      cell.nu == "nu.bs" & form == "Fil." ~ "filament",
    ),
    
    ## Extra info ----
    sample.size = "30",
    source.code = '6',
    original.source.code.1 = "6",
    bodysize.measurement = "biovolume",
    units = "µm^3",
    sample.month = NA,
    sample.year = "2005-2016",
    form.no = NA,
    join.location.1 = "Laplace-Treyture et al., 2021",
    max.body.size = NA,
    min.body.size = NA,
    measurement.type = "average",
    life.stage = "adult",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "in-situ",
    
    ## Individual.uid ----
    uid.db = "ltF", # stands for LT formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
    
  ) %>% 
  
  ## Order columns ----
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no,
    - cell.nu
  ) %>% 
  
  # Reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(lt_formatted, file = "R/Data_outputs/databases/lt_formatted.rds")

#  Neury-Ormanni ----
no_formatted <- NO %>% 
  
  ## Select columns ----
  select(
    Species,
    LengthMax,
    References...10,
    LengthMin,
    References...12,
    WidthMax,
    References...14,
    WidthMin,
    References...16
  ) %>% 
  
  rename(
    original.taxa.name = Species,
    min.length = LengthMin,
    minl.ref = References...10,
    max.length = LengthMax,
    maxl.ref = References...12,
    min.width = WidthMin,
    minw.ref = References...14,
    max.width = WidthMax,
    maxw.ref = References...16
  ) %>% 
  
  mutate(
    ## taxa name ----
    # replace _ to " " in original.taxa.name
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "_", " "),
    
    ## body.size ----
    # make numeric
    min.length = as.numeric(min.length),
    max.length = as.numeric(max.length),
    min.width = as.numeric(min.width),
    max.width = as.numeric(max.width),
    
    # calculate average from min and max
    `body length` = (min.length+max.length)/2,
    `body width` = (min.width+max.width)/2,
    
    ## Original.source ----
    # merge min and max references into one column, all were the same for min and max so didn'e need to do anythng else
    original.source.length = if_else(
      minl.ref == maxl.ref,
      minl.ref,
      stri_c(minl.ref, maxl.ref, sep = ", ")
    ),
    
    original.source.width = if_else(
      minw.ref == maxw.ref,
      minw.ref,
      stri_c(minw.ref, maxw.ref, sep = ", ")
    ),
    
    ## Individual.uid ----
    # needs to be done before pivot so that the length and width measurements on seperate lines have the same uid
    uid.db = "noF", # stands for neury-ormanni formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Pivot ----
  # make separate lines for length and width
  pivot_longer(
    cols = `body length`:`body width`, names_to = "bodysize.measurement", values_to = "body.size"
  ) %>% 
  
  # assign min and max body.size depending on if it is length or width
  mutate(
    min.body.size = case_when(
      bodysize.measurement == "body length" ~ min.length,
      bodysize.measurement == "body width" ~ min.width,
      TRUE ~ NA
    ),
    max.body.size = case_when(
      bodysize.measurement == "body length" ~ max.length,
      bodysize.measurement == "body width" ~ max.width,
      TRUE ~ NA
    ),
    
    # assign the correct ref as duplicated have been made for each row now
    original.source.code.1 = if_else(
      bodysize.measurement == "body length",
      original.source.length,
      original.source.width
    ),
    
    ## Filter ----
    # remove ones with all NA in size columns but keep any if they have at least one value in it
    remove = if_else(
      is.na(body.size) & is.na(min.body.size) & is.na(max.body.size),
      "remove",
      "keep"
    )
  ) %>% 
  
  filter(
    remove == "keep"
  ) %>% 
  
  mutate(
    ## Measurement.type ----
    measurement.type = case_when(
      !is.na(body.size) ~ "range",
      is.na(body.size) & !is.na(min.body.size) ~ "min",
      is.na(body.size) & !is.na(max.body.size) ~ "max"
    ),
    
    ## Extra info ----
    sample.size = "30",
    source.code = '7',
    units = "µm^3",
    sample.month = NA,
    sample.year = NA,
    form = "individual",
    form.no = 1,
    join.location.1 = NA,
    life.stage = "adult",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "ex-situ",
  ) %>% 
  
  ## Order columns
  # remove redundant columns 
  select(
    - remove,
    - min.length,
    - max.length,
    - min.width,
    - max.width,
    - original.source.length,
    - original.source.width,
    - minl.ref,
    - maxl.ref,
    - minw.ref,
    - maxw.ref
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## save ----
saveRDS(no_formatted, file = "R/Data_outputs/databases/no_formatted.rds")

#  Rimet 2012 ----  
rimet2012_formatted <- rimet2012 %>% 
  
  ## Select columns ----
  select(
    `genus + species + var`,
    `Biovolume (µm3)`,
    `length (µm)`,
    `width (µm)`
  ) %>% 
  
  rename(
    original.taxa.name = `genus + species + var`,
    biovolume = `Biovolume (µm3)`,
    `cell length` = `length (µm)`, # have as two words so when it is pivotted then names column can be changed to bodysize.measurement and nothing needs to edited
    `cell width` = `width (µm)` # have as two words so when it is pivotted then names column can be changed to bodysize.measurement and nothing needs to edited
  ) %>% 
  
  mutate(
    ## Individual.uid ----
    # needs to be done before pivot so that the length and width measurements have the same individual uid 
    uid.db = "r12F", # stands for rimet 2012 formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Pivot ----
  # make each length, width and biovol measurement it's own line with the same uid
  pivot_longer(cols = biovolume:`cell width`, names_to = "bodysize.measurement", values_to = "body.size") %>% 
  
  mutate(
    ## Units ----
    units = case_when(
      bodysize.measurement %in% c("cell length", "cell width") ~ "µm",
      bodysize.measurement == "biovolume" ~ "µm^3"
    ),
    
    ## Extra info ----
    min.body.size = NA,
    max.body.size = NA,
    sample.size = NA,
    source.code = '8',
    original.source.code.1 = NA,
    sample.month = NA,
    sample.year = NA,
    form = "individual",
    form.no = 1,
    join.location.1 = NA,
    life.stage = "adult",
    reps = "unknown",
    error = NA,
    error.type = NA,
    sex = NA,
    experiemntal.design = "ex-situ",
    measurement.type = "average"
  ) %>% 
  
  ## Order columns ----
  # reorder
  relocate(source.code, original.source.code.1, experiemntal.design, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## save ----
saveRDS(rimet2012_formatted, file = "R/Data_outputs/databases/rimet2012_formatted.rds")


# Combine together ----
db_formatted<- bind_rows(rimet_formatted, kremer_formatted, odume_formatted, hebert_formatted, gavrilko_formatted, lt_formatted, no_formatted, rimet2012_formatted) %>% 
  # remove data with a 0 cell.biovol (for some reason can't filter this so have to maunally removed based on uid)
  filter(
    !(individual.uid %in% c("kremerF4326", "kremerF19189", "kremerF41111", "kremerF47749", "kremerF48939", "kremerF119036"))
  ) %>% 
  # reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18, original.source.code.19,
           experiemntal.design, sample.year, sample.month, join.location.1, join.location.2,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)



## hebert source list
h_source <- hebert_formatted %>% 
  select(
    original.source.code.1,
    original.source.code.2,
    original.source.code.3,
    original.source.code.4
  ) %>% 
  pivot_longer(
    cols = original.source.code.1:original.source.code.4, values_to = "original.source.code"
  ) %>% 
  mutate(
  original.source.code = stri_replace_all_regex(original.source.code, " ", "")
  ) %>% 
  filter(
    !is.na(original.source.code)
  ) %>% 
  select(
    original.source.code
  ) %>% 
  distinct(
    original.source.code
  )

## no source list
n_source <- no_formatted %>% 
  distinct(
    original.source.code.1
  )

## rimet2012 sources
r2_source <- rimet2012 %>% 
  distinct(
    `Reference for sizes`
  )

write.csv(r2_source, "rimet2012_source.csv")
