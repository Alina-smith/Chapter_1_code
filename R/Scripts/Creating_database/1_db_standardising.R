# Aim of script: Standardising the databases and merging into one

# Description of databases ----
  # Rimet - all phyto
  # Kremer - all phyto
  # Odume - all zoo or insects 
  # Hebert - all zoo
  # Gavrilko - all zoo
  # Laplace-Treyture - all phyto
  # Neury-Ormanni - all phyto

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# data ----
rimet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet")
kremer <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Kremer")
odume <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Odume")
hebert <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Hebert")
Gavrilko <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Gavrilko")
LT <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture")
NO <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Neury-Ormanni")
rimet2012 <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimet_2012")
db_source_list <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "source_list")
db_location <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "location_join") # this is different to th full location list this is just for left joining join.location stuff to so that the location.codes can be joined from the full list later on
db_extra <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "extra_info")
rimet_2012_sources <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimet_2012_sources")

# Rimmet ----
# All phyto
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
    ind.per.nu = `Number of cells per colony`
  ) %>% 
  
  mutate(
    ## Filter ----
    # set nu.bs to NA when it is the same as cell.bs to filter out later on - if it is the same then implying that it is the same measurement so don't need them
    nu.bs = if_else(
      nu.bs == cell.bs,
      NA,
      nu.bs
    ),
    
    ## Life stage ----
    # all phyto so it is active and dormant instead of adult and juvenile
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)Stomatocyst\\b|\\b(?i)Cyst\\b|\\b(?i)Cysts\\b") ~ "dormant",
      TRUE ~ "active"),
    
    ## Sample dates ----
    # Year
    sample.year = case_when(
      # extract out the dates that are in a normal format with regex
      stri_detect_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})") ~ stri_extract_first_regex(biovol.notes, "(?<!\\d{4}-)\\b\\d{4}\\b(?!-\\d{4})"),
      stri_detect_regex(biovol.notes, "\\d{4}-\\d{4}") ~ stri_extract_first_regex(biovol.notes, "\\d{4}-\\d{4}"),
      
      # extract weird ones manually
      biovol.notes %in% c("Moyenne Annecy GL - 23/09/09", "Aiguebelette 28-09-09") ~ "2009",
      biovol.notes == "Moyenne sur Annecy 20087" ~ "2008",
      biovol.notes == "Mesures effectuées sur Annecy GL le 9/5/7, pas de correspondance taxo, forme crée pour la circonstance" ~ "2007",
      TRUE ~ NA),
    
    # Month - do separately because theres not many of them and they're all different
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
    # Make join.all column which will be used to left join location info and source info later on
    # extract any location info from the biovol.notes that match the locations in the join.location column in db_location
    
    join.all.list = sapply(
      biovol.notes, function(x) {
        
        # make a regex to use in extract_all
        regex_pattern <- paste0( # paste (?i) to make case insensitive
          "(?i)", paste0( # paste all the values in the db_source_list$join.source column that are from db-1 into a string separated by | to make an OR regex
            filter(
              db_location, db.code == "db-1")$join.location, collapse = "|"
            )
          )
        
        # extract any strings that match the regex
        extracted_locations <- stri_extract_all_regex(x, regex_pattern)
      }
    ),
    
    # unlist the join.location.list column - some rows had multiple matches to the regex to makes it into a list column with multiple values per cell
    join.all.unlist = sapply(
      join.all.list, function(x){
        # unlist
        unlist <- paste0(x, collapse = " ")
        
        # set to lower case - just to make it easier with formating and extracting in next steps
        lower_case <- tolower(unlist)
        
        # set "na" to NA
        na_if(lower_case, "na")
      }
    ),
    
    # Make join.location columns from join.all
    
    join.location.1 = case_when(
      # when the lake monitoring program and lake is mentioned then keep just the lake
      join.all.unlist == "léman shl2" ~ "léman",
      
      # When two lakes are stated split into two columns
      join.all.unlist == "bourget léman" ~ "bourget",
      
      # set NA to unknown - only need for this column as don't need unknown for all location columns
      is.na(join.all.unlist) ~ "unknown",
      TRUE ~ join.all.unlist
    ),
    
    join.location.2 = case_when(
      # when the lake monitoring program and lake is mentioned then keep just the lake
      join.all.unlist == "léman shl2" ~ "léman",
      
      # When two lakes are stated split into two columns
      join.all.unlist == "bourget léman" ~ "léman",
      
      TRUE ~ NA
    )
  ) %>% 
  
  # remove redundant columns - join.all.unlist will be used for sources so keep that
  select(
    - join.all.list,
    - biovol.notes
  ) %>% 
  
  ## Sources ----
  # Add in original.source.code column for any measurements that have a source in the biovol.notes
  # use join.all.unlist in above step for this
    
  # left join original.source.code
  left_join(
    select(
      filter(db_source_list, db.code == "db-1"), # only codes from db-1
      join.source, source.code
    ), by = c("join.all.unlist" = "join.source")
  ) %>% 
  
  # rename to fit with other databases that have multiple sourecs
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  # remove redundant columns
  select(
    - join.all.unlist
  ) %>% 
  
  ## Pivot ----
  # put nu and cell biovolumes on seperate rows - don't need the same individual uid because they are different individuals
  pivot_longer(
    cols = c(cell.bs, nu.bs), names_to = "cell.nu", values_to = "body.size"
  ) %>% 
  
  # Remove NA body.size made in begining
  filter(
    !is.na(body.size)
  ) %>% 
  
  ## Nu -----
  # change 1s and 0s to words
  mutate(
    nu = case_when(
      cell.nu == "cell.bs" ~ "individual",
      cell.nu == "nu.bs"& Colonial == "1" & Filament == "1" ~ "multi-cell",
      cell.nu == "nu.bs"& Colonial == "0" & Filament == "0" ~ "multi-cell",
      cell.nu == "nu.bs" & Colonial == "1" ~ "colony",
      cell.nu == "nu.bs" & Filament == "1" ~ "filament"
    )
  ) %>% 
  
  # remove redundant columns
  select(
    - Colonial,
    - Filament,
    - cell.nu,
  ) %>% 
  
  mutate(
    ## ind.per.nu ----
    # Change ind.per.nu to 1 for indivduals
    ind.per.nu = if_else(
      nu == "individual",
      1,
      ind.per.nu
    ),
    
    ## Extra info ----
    source.code = 'db-1',
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    sample.size = NA,
    reps = NA,
    error = NA,
    error.type = NA,
    sex = NA, # all phyto so no sex info
      
    ## Individual.uid
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "rimetF", # stands for rimet formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1, join.location.2,
            individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
            min.body.size, max.body.size, body.size,
            bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(rimet_formatted, file = "R/data_outputs/databases/rimet_formatted.rds")



# Kremer ----

kremer_formatted <- kremer %>% 
  
  ## Select columns ----
  select(location,
         original.taxa.name,
         cell.biovol,
         data.source,
         sample.date,
         cells.per.nu,
         nu.biovol) %>% 
  
  rename(join.location.1 = location,
         join.source = data.source,
         cell.bs = cell.biovol,
         ind.per.nu = cells.per.nu,
         nu.bs = nu.biovol) %>%
  
  ## Filter
  filter(
    #  Remove any with no taxa.name
    !is.na(original.taxa.name),
    
    # remove onces from REBECCA due to inconsitenties and reported bias
    !(join.location.1 == "REBECCA"),
    
    # remove ones without a cell.bs as cannot determine if these are multi-cellular or single cell measuresuement 
    !is.na(cell.bs)
    ) %>% 
  
  mutate(
    # Set nu.bs to NA when it is the same as cell.bs to filter out later on as if they are both the same then it implies that it is the same measurement so don't need it
    nu.bs = case_when(
      is.na(cell.bs) ~ nu.bs,
      nu.bs == cell.bs ~ NA,
      TRUE ~ nu.bs
    ),
    
    ## Location ----
    # change join.location to match db_location_sheet for ANSP as too many codes to go through each one and find location
    join.location.1 = if_else(
      join.source == "ANSP",
      "North America",
      join.location.1
    ),

    ## Sample Dates ----
    # split the sample dates into year and month
    sample.year = case_when(
      stri_detect_regex(join.source, "\\b\\d{4}-\\d{4}\\b") ~ stri_extract_first_regex(join.source, "\\b\\d{4}-\\d{4}\\b"), # when the date is in the source and not the date column take it from source column
      stri_detect_regex(sample.date, "\\b\\d{4}\\b") ~ stri_extract_first_regex(sample.date, "\\b\\d{4}\\b"), # take years from the sample.date column
      join.source == "ANSP" ~ "1994-2013" # use the meta data for ANSP
    ),
    
    sample.month = stri_extract_first_regex(sample.date, "(?<=-)\\d{2}(?=-)"), # take months from the sample.date column
    
    # remove dates from the join.source column now they are in the date columns
    join.source = stri_replace_all_regex(join.source, "\\b.\\d{4}-\\d{4}\\b", "")
  ) %>% 
  
  # remove redundant columns
  select(
    - sample.date
  ) %>% 
  
  ## Original sources ----
  # left join source.code from db_source_list by matching to location (now join.source) column in the original database
  left_join(
    select(
      filter(
        db_source_list, db.code == "db-2" # select ones in the db_source_list that are just for this database
      ), source.code, join.source # select just source.code and join.source columns from db_source_list
    ), by = "join.source"
  ) %>% 
  
  # rename to match other databases that have multiple sources
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  # remove redundant columns
  select(
    - join.source
  ) %>% 
  
  ## Pivot ----
  # put nu and cell biovolumes on seperate rows - don't need the same individual uid because they are different individuals
  pivot_longer(
    cols = c(cell.bs, nu.bs), names_to = "cell.nu", values_to = "body.size"
  ) %>% 
  
  # remove NA body.size using column made in begining
  filter(
    !is.na(body.size)
  ) %>% 
  
  mutate(
    ## body.size ----
    # reverse transform body.size measurments from log10
    body.size = 10^body.size,
    
    ## Nu ----
    # make an nu column to say if it is multicell or individual measurement
    nu = case_when(
      cell.nu == "cell.bs" ~ "individual",
      cell.nu == "nu.bs" ~ "multi-cellular",
      TRUE ~ "multi-cellular"
    ),
    
    ## ind.per.nu ----
    # change ind.per.nu to 1 for individual measurements
    ind.per.nu = if_else(
      nu == "individual",
      1,
      ind.per.nu
      )
  ) %>% 
  
  # remove redundant columns
  select(
    - cell.nu,
  ) %>% 
    
  mutate(
    ## Extra info ----
    source.code = 'db-2',
    life.stage = "active", # checked for any dormant names and there are none
    bodysize.measurement = "biovolume",
    units = "µm^3",
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    sample.size = NA,
    reps = NA,
    error = NA,
    error.type = NA,
    sex = NA, # all phyto so no sex needed
      
    ## Individual uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "kremerF", # stands for kremer formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # Remove redundant columns
    select(
      - uid.db,
      - uid.no
    ) %>% 
      
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
            individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
            min.body.size, max.body.size, body.size,
            bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)


## Save ----
saveRDS(kremer_formatted, file = "R/data_outputs/databases/kremer_formatted.rds")



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
    # Change the datapoint in cm to mm so it's the same as all the others
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
    source.code = "db-3",
    original.source.code.1 = "db-3",
    sample.year = NA,
    sample.month = NA,
    join.location.1 = "Odume et al., 2023",
    ind.per.nu = 1,
    nu = "individual",
    units = "mm",
    sample.size = NA,
    reps = NA,
    error = NA,
    error.type = NA,
    sex = NA, # no sex column mentioned
  ) %>% 
  
  ## Order columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           original.taxa.name, life.stage, sex, nu, ind.per.nu,
           original.body.size, units, sample.size, reps, error, error.type)

# Odume - Exact measurements ----
# extract all the data points that are just one measurement 

odume_exact <- odume_formatted_raw %>% 
  
  ## Filter ----
  # Get only extact measuements 
  filter(
      !(stri_detect_regex(original.body.size, "(?i)\\bor\\b|(?i)Approximately|about|-|\u2013|(?i)up to|(?i)Less than |and|<|>|width")) & # ones that don't contain any of the phrases I use to get the data points in later steps
      !(odume.uid %in% c("1657","2098", "2690", "97", "146", "1529", "2175", "2176", "2181", "2182", "1883", "2255")) # random odd ones that I don't want but easier to just remove by odume.uid
  ) %>% 
  
  mutate(
    ## Body sizes ----
    # Extract measurments
    original.body.size = stri_replace_all_regex(original.body.size, "\\b\\d{4}\\)", ""), # remove date to make it easier to extract measurement numbers
    body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*")) # any numbers that are or aren't followed by a decimal
    ) %>% 
  
  ## bodysize.measurement ----
  # set bodysize.measurement to height or length
  mutate(
    bodysize.measurement = case_when(
      stri_detect_regex(original.body.size, "(?i)high|(?i)height") ~ "height",
      TRUE ~ "length"
    )
  ) %>% 
  
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
  ) %>% 
  
  mutate(
    ## Extra info ----
    measurement.type = "average",
    min.body.size = NA,
    max.body.size = NA,
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "odumeE", # stands for odume extact
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # Remove redundent columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  # Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Ranges ----
# data points that are a range

odume_ranges <- odume_formatted_raw %>% 
  
  ## Filter ----
  # Get ranges
  filter(
    # get ones that contain a short or long dash or "or" or the two that are ranges but easier to select by odume.uid
    stri_detect_regex(original.body.size, "-|\u2013|\\bor\\b") | odume.uid %in% c("1864", "1529"), 
    
    # remove any that will be used in later steps
    !(stri_detect_regex(original.body.size, "longer")),
    !(stri_detect_regex(original.body.size, "diameter"))
    ) %>% 
  
  mutate(
    ## Body.size ----
    # Format ranges into min, max and average
    
    # edit original.body.size to make extracting easier
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
    min.body.size = as.numeric(stri_extract_first_regex(body.size.range, "\\d+(\\.\\d+)*(?=[-\u2013])")), # ones that preceed "-"
    max.body.size = as.numeric(stri_extract_first_regex(body.size.range, "(?<=[-\u2013])\\d+(\\.\\d+)*")), # ones that follow "-"
    
    # calculate average body size from min and max
    body.size = (min.body.size+max.body.size)/2
  ) %>% 
  
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.range,
    - body.size.comment,
  ) %>% 
  
  mutate(
    ## Extra info ----
    measurement.type = "range",
    bodysize.measurement = "length", # all are length so don't need to do a word search
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "odumeR", # stands for odume range
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Approx ----
# ones that have approximately in the measurment

odume_approx <- odume_formatted_raw %>% 
  
  ## Filter ----
  # Select ones I want
  filter(
    # Any that have approximately or about in
    stri_detect_regex(original.body.size, "(?i)Approximately|(?i)about"),
    
    # remove ones that will be done in other steps
    !(stri_detect_regex(original.body.size, "[-\u2013]")),
    !(stri_detect_regex(original.body.size, "max"))
  ) %>% 
  
  mutate(
    ## Body.size ----
    # Extract measurements from original.body.size column
    body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")) # any numbers that may or may not have a decimal that preceed "mm"
  ) %>% 
  
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
  ) %>% 
  
  mutate(
    ## Extra info ----
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "approximately",
    bodysize.measurement = "length", # all are length so don't need to do a word search
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "odumeA", # stands for odume approx
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # remove redundant columns
  select(
    - uid.db,
    - uid.no
    ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Min ----
# Any measurements that imply it is the bottom end of the size

odume_min <- odume_formatted_raw %>% 
  
  ## Filter ----
  filter(
    # Ones that contain a phrase that indicates it is a minumum measurement
    stri_detect_regex(original.body.size, "(?i)\\blonger\\b|>|(?i)\\bover\\b|(?i)greater than"),
    
    # remove ones that I don't want that are easier to do by odume.uid
    !(odume.uid == "1529")
  ) %>% 
  
  mutate(
    ## Body.size ----
    # Extract measurements from original.body.size
    min.body.size = as.numeric(stri_extract_all_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")) # any numbers that may or may not have a decimal that preceed "mm"
  ) %>% 
  
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment
  ) %>% 
  
  mutate(
    ## Extra info ----
    body.size = NA,
    max.body.size = NA,
    measurement.type = "min",
    bodysize.measurement = "length", # all are length so don't need to do a word search
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "odumeMIN", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - max ----
# Any measurements that indicate it is the max end of the measurment 

odume_max <- odume_formatted_raw %>% 
  
  ## Filter ----
  filter(
    # select any that conatin a pgrase that indicates it is the max end of the measurment 
    stri_detect_regex(original.body.size, "(?i)up to|(?i)less than|<|(?i)\\brarely\\b|(?i)\\bmax\\b|(?i)grow up\\b"),
    
    # remove ones i don't want
    !(stri_detect_regex(original.body.size, "(?i)\\bwidth\\b")),
    !(odume.uid == "170")
  ) %>% 
  
  mutate(
    ## body.size ----
    # extract measurements from original.body.size
    max.body.size = as.numeric(stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*(?= mm)")), # any numbers that may or may not have a decimal that preceed "mm"
    
    ## Bodysize.measurement ----
    # set measurment to length, width or height
    bodysize.measurement = case_when(
      stri_detect_regex(original.body.size, "(?i)length|(?i)long") ~ "length",
      stri_detect_regex(original.body.size, "(?i)height|(?i)hight|(?i)high") ~ "height",
      stri_detect_regex(original.body.size, "(?i)diameter") ~ "diameter",
      TRUE ~ "length"
    )
  ) %>% 
  
  # remove redundant columns 
  select(
    - original.body.size,
    - body.size.comment,
  ) %>% 
  
  mutate(
    ## Extra info ----
    body.size = NA,
    min.body.size = NA,
    measurement.type = "max",
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "odumeMAX", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - duplication check ----
# Check if any of the ones in the above odume formatting have been used twice by checking the odume.uid
# Doing this before the length, width height step because that one is a but more complicated so want to get all odume.uids extracted so far and then check if any of these come up in the lwh step so I can exclude them

odume_check <- bind_rows(odume_approx, odume_exact, odume_max, odume_min, odume_ranges) %>% 
  select(
    odume.uid
  )

duplicate_check <- data.frame(table(odume_check$odume.uid)) %>% 
  filter(
    Freq > 1
  )

# No duplicates found

# Odume - Length, width and height ----
# Any measurements that have multiple types of measurements in them e.g. length and width and height all together
odume_lwh <- odume_formatted_raw %>% 
  
  ## Filter ----
  filter(
    # select ones that contain phrases that indicate it could be multiple measurements
    stri_detect_regex(original.body.size, "(?i)\\bwidth\\b|(?i)\\bheight\\b|(?i)\\bhight\\b|(?i)\\bhigh\\b|(?i)\\bheigh\\b|(?i)\\blong\\b"),
    
    # remove any that I have alread included in previous steps by checking odume.uid from odume_check
    !(odume.uid %in% odume_check$odume.uid)
  ) %>% 
  
  mutate(
    ## body.size ----
    # edit weird one to make extracting easier - assume same format as the others of height length
    original.body.size = if_else(
      odume.uid == "169",
      "49 height 33 length 23 width",
      original.body.size
    ),
    
    # extract measurements from original.body.size into separate columns to use when pivoting onto separate rows later
    height = as.numeric(stri_extract_first_regex(original.body.size, "\\d+(\\.\\d+)*(?= (?i)height| (?i)heigh)|\\d+(\\.\\d+)*(?= mm (?i)height| mm (?i)\\(height\\)| mm  (?i)height| mm  (?i)hight|mm (?i)height| mm (?i)heigh)")), #  can use first instead of all beacause theres only one value per row
    width = as.numeric(stri_extract_first_regex(original.body.size, "\\b\\d+(\\.\\d+)*(?= (?i)width|(?i)width)|\\b\\d+(\\.\\d+)*(?= mm (?i)width| mm (?i)\\(width\\)|mm (?i)width)")),
    length = as.numeric(stri_extract_first_regex(original.body.size, "\\b\\d+(\\.\\d+)*(?= mm (?i)length|mm (?i)length)|\\b\\d+(\\.\\d+)*(?= (?i)length)"))
  ) %>% 
  
  # remove redundant columns
  select(
    - original.body.size,
    - body.size.comment,
  ) %>% 
  
  mutate(
    ## Extra info ----
    min.body.size = NA,
    max.body.size = NA,
    measurement.type = "average",
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "odumeLWH", # stands for odume min
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  ## Pivot ----
  # put length, width and height onto separate rows - need to be done after individual.uid because they are measurements of the same individual so need the same individual.uid but with the same individual.uid
  pivot_longer(
    ., cols = height:length, values_to = "body.size", names_to = "bodysize.measurement"
    ) %>% 
  
  # remove NA values in body.size
  filter(
    !is.na(body.size) 
  ) %>% 
  
  # remove redundant columns
  select(
    - uid.db,
    - uid.no,
  ) %>% 
  
  ## reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - Combine them all ----
odume_formatted <- bind_rows(odume_exact, odume_ranges, odume_lwh, odume_approx, odume_max, odume_min) %>% 
  # remove odume.uid now I don't need
  select(
    - odume.uid
  ) %>% 
  # reorder
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Odume - save ----
saveRDS(odume_formatted, file = "R/data_outputs/databases/odume_formatted.rds")


# Hebert ----

hebert_formatted <- hebert %>%
  
  ## Select columns ----
  select(Column1, Column2, Column5, Column8, Column9, Column10, Column11, Column12, Column13, Column14, Column15) %>% 
  rename(
    genus = Column1,
    species = Column2,
    habitat = Column5,
    length = Column8,
    min.bl = Column9,
    max.bl = Column10,
    bl.join.all = Column11, # this is the same code for the sources, locations and extra so names join.all
    `dry mass` = Column12, # have it with a space as then can just assign that column as the bodysize.measurement and don't need to change anything
    min.dm = Column13,
    max.dm = Column14,
    dm.join.all = Column15 # this is the same code for the sources, locations and extra so names join.all
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
    -genus,
    -species
  ) %>% 
  
  # Change "NA" to NA
  mutate_all(
    ., ~na_if(., "NA")
  ) %>% 
  
  # change the commas to decimals and get rid of space in min.dm
  mutate(
    length = as.numeric(stri_replace_all_regex(length, ",", ".")),
    min.bl = as.numeric(stri_replace_all_regex(min.bl, ",", ".")),
    max.bl = as.numeric(stri_replace_all_regex(max.bl, ",", ".")),
    `dry mass` = as.numeric(stri_replace_all_regex(`dry mass`, ",", ".")),
    min.dm = stri_replace_all_regex(min.dm, ",", ".") %>% 
      stri_replace_all_regex(., " ", "") %>%
      as.numeric(.),
    max.dm = as.numeric(stri_replace_all_regex(max.dm, ",", "."))
  ) %>% 
  
  ## Pivot ----
  # Make it so dry mass and length are n seperate lines - don't need same individual.uid as they are from different references
  pivot_longer(
    cols = c(length, `dry mass`), names_to = "bodysize.measurement", values_to = "body.size"
  ) %>% 
  
  mutate(
  # assign correct min, max and references for each one as they will have been duplicated on multiple rows from the pivot
  # min and max
    min.body.size = case_when(
      bodysize.measurement == "length" ~ min.bl,
      bodysize.measurement == "dry mass" ~ min.dm,
      TRUE ~ NA
      ),
    
    max.body.size = case_when(
      bodysize.measurement == "length" ~ max.bl,
      bodysize.measurement == "dry mass" ~ max.dm,
      TRUE ~ NA
    ),
    
    # join.all (refernces)
    join.all = case_when(
      bodysize.measurement == "length" ~ bl.join.all,
      bodysize.measurement == "dry mass" ~ dm.join.all,
      TRUE ~ NA
    ),
    
    join.all = stri_replace_all_regex(join.all, " ", "")
  ) %>% 
  
  # remove redundant columns
  select(
    - min.bl,
    - max.bl,
    - bl.join.all,
    - min.dm,
    - max.dm,
    - dm.join.all,
  ) %>% 
  
  mutate(
    ## Measurement.type ----
    # when body.size = NA means it was a range so assign type to range
    measurement.type = if_else(
      is.na(body.size),
      "range",
      "average"
    ),
    
    ## Units ----
    # assign length mm and dry mass mg
    units = if_else(
      bodysize.measurement == "length",
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
  
  # Remove any tha don't have a bodysize measurement - had to do after calulating average other would of removed ranges
  filter(
    !is.na(body.size)
  ) %>% 
  
  ## Original.source.code ----
  # need to left join the source codes from db_source_list so need to make them into separate columns and then left join one column at a time

  # separate original sources into separate columns
  separate(
    join.all, into = c("join.all.1", "join.all.2", "join.all.3", "join.all.4"), sep = ","
  ) %>% 
  
  # left_join db_source_list info
  # original.source.code.1
  left_join(
    select(
      filter(
        db_source_list, db.code == "db-4" # filter for just the references in hebert (db 4)
        ), source.code, join.source), # select just source.code and join.source columns
    by = c("join.all.1" = "join.source")
    ) %>% 
  
  # original.source.code.2
  left_join(
    select(
      filter(
        db_source_list, db.code == "db-4" # filter for just the references in hebert (db 4)
        ), source.code, join.source # select just source.code and join.source columns
      ), by = c("join.all.2" = "join.source"),
    suffix = c(".1", ".2")
    ) %>% 
  
  # original.source.code.3
  left_join(
    select(
      filter(
        db_source_list, db.code == "db-4" # filter for just the references in hebert (db 4)
        ), 
      source.code, join.source # select just source.code and join.source columns
      ),
    by = c("join.all.3" = "join.source")
  ) %>% 
  
  # original.source.code.4
  left_join(
    select(
      filter(
        db_source_list, db.code == "db-4" # filter for just the references in hebert (db 4)
        ), source.code, join.source # select just source.code and join.source columns
      ), by = c("join.all.4" = "join.source"),
    suffix = c(".3", ".4")
    ) %>% 
  
  # rename
  rename(
    original.source.code.1 = source.code.1,
    original.source.code.2 = source.code.2,
    original.source.code.3 = source.code.3,
    original.source.code.4 = source.code.4
  ) %>% 
  
  ## Sample.dates, sample.size ----
  # left join info in the extra.info sheet - will need to do it for each source code and then merge all into one column for each
  left_join(
    select(
      filter(
        db_extra, db.code == "db-4"
      ), - experimental.design, - db.code
    ), by = c("join.all.1" = "join.extra") # source.code.1
    ) %>% 
  
  left_join(
    select(
      filter(
        db_extra, db.code == "db-4"
      ), - experimental.design, - db.code
    ), by = c("join.all.2" = "join.extra"), # source.code.2
    suffix = c(".1", ".2")
  ) %>%  
  
  left_join(
    select(
      filter(
        db_extra, db.code == "db-4"
      ), - experimental.design, - db.code
    ), by = c("join.all.3" = "join.extra") # source.code.3
  ) %>% 
  
  left_join(
    select(
      filter(
        db_extra, db.code == "db-4"
      ), - experimental.design, - db.code
    ), by = c("join.all.4" = "join.extra"), # source.code.4
    suffix = c(".3", ".4")
  ) %>%  
  
  # merge into one column for each
  mutate(
    error = paste(error.1, error.2, error.3, error.4, sep = ",") %>% 
      stri_replace_all_regex(., "unknown,|NA,|,NA|NA", "") %>% 
      na_if(., ""),
    error.type = paste(error.type.1, error.type.2, error.type.3, error.type.4, sep = ",") %>% 
      stri_replace_all_regex(., "unknown,|NA,|,NA|NA", "") %>% 
      na_if(., ""),
    sample.size = paste(sample.size.1, sample.size.2, sample.size.3, sample.size.4, sep = ",") %>% 
      stri_replace_all_regex(., "unknown,|NA,|,NA|NA", "")%>%
      na_if(., ""),
    reps = paste(reps.1, reps.2, reps.3, reps.4, sep = ",") %>% 
      stri_replace_all_regex(., "unknown,|NA,|,NA|NA", "")%>% 
      na_if(., ""),
    sample.year = paste(sample.year.1, sample.year.2, sample.year.3, sample.year.4, sep = ",") %>% 
      stri_replace_all_regex(., "unknown,|NA,|,NA|NA", "") %>% 
      na_if(., ""),
    sample.month = paste(sample.month.1, sample.month.2, sample.month.3, sample.month.4, sep = ",") %>% 
      stri_replace_all_regex(., "unknown,|NA,|,NA|NA", "") %>% 
      na_if(., ""),
    
    ## when more than one value is in the column decide what to do with each
    # make multiple years into a range
    sample.year = case_when(
      sample.year == "2003,1986-1990,1774-1976" ~ "1974-2003",
      TRUE ~ sample.year
      )
    ) %>% 
  
  # remove redundant columns
  select(
    - sample.size.1:-error.type.4) %>% 
  
  mutate(
    ## Location ----
    # some will be the same as the join.all but some of the sources have multiple locations so will need to make multiple join.location columns with a case_when
    
    # join.all.1:
    join.location.1 = case_when(
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia affinis" ~ "10a",
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia megalops" ~ "10c",
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia laticaudata" ~ "10e",
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia pulchella" ~ "10g",
      join.all.1 == "10" & original.taxa.name == "Daphnia thomsoni" ~ "10i",
      join.all.1 == "10" & original.taxa.name == "Simocephalus obtusatus" ~ "10j",
      join.all.1 == "10" & original.taxa.name == "Simocephalus exspinosus" ~ "10k",
      join.all.1 == "10" & original.taxa.name == "Simocephalus acutifrons" ~ "10m",
      join.all.1 == "10" & original.taxa.name == "Scapholeberis aurita" ~ "10n",
      join.all.1 == "10" & original.taxa.name == "Eurycercus glacialis" ~ "10o",
      join.all.1 == "13" & stri_detect_regex(original.taxa.name,"Bythotrephes longimanus") ~ "13a",
      join.all.1 == "13" & original.taxa.name == "Leptodora kindtii" ~ "13b",
      join.all.1 == "18" ~ "18a",
      join.all.1 == "23" ~ "23a",
      join.all.1 == "26" ~ "26a",
      join.all.1 %in% c("2", "3", "5", "6", "8", "9", "20", "22", "25", "30", "32", "34", "37", "41", "45", "46", "49", "64", "65", "67") ~ "unknown",
      TRUE ~ join.all.1
    ),
    
    join.location.2 = case_when(
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia affinis" ~ "10b",
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia megalops" ~ "10d",
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia laticaudata" ~ "10f",
      join.all.1 == "10" & original.taxa.name == "Ceriodaphnia pulchella" ~ "10h",
      join.all.1 == "10" & original.taxa.name == "Simocephalus exspinosus" ~ "10l",
      join.all.1 == "10" & original.taxa.name == "Eurycercus glacialis" ~ "10p",
      join.all.1 == "18" ~ "18b",
      join.all.1 == "23" ~ "23b",
      join.all.1 == "26" ~ "26b",
      TRUE ~ "unknown"
    ),
    
    # join.all.2:
    join.location.3 = case_when(
      join.all.2 == "10" & original.taxa.name == "Eurycercus glacialis" ~ "10o",
      join.all.2 == "18" ~ "18a",
      join.all.2 == "23" ~ "23a",
      join.all.2 == "26" ~ "26a",
      join.all.2 %in% c("2", "3", "5", "6", "8", "9", "20", "22", "25", "30", "32", "34", "37", "41", "45", "46", "49", "64", "65", "67") ~ "unknown",
      TRUE ~ join.all.2
    ),
    
    join.location.4 = case_when(
      join.all.2 == "10" & original.taxa.name == "Eurycercus glacialis" ~ "10p",
      join.all.2 == "18" ~ "18b",
      join.all.2 == "23" ~ "23b",
      join.all.2 == "26" ~ "26b",
      TRUE ~ "unknown"
      ),
    
    # join.all.3 - no ones with more than one location code
    join.location.5 = case_when(
      join.all.3 %in% c("2", "3", "5", "6", "8", "9", "20", "22", "25", "30", "32", "34", "37", "41", "45", "46", "49", "64", "65", "67") ~ "unknown",
      TRUE ~ join.all.3
    ),
    
    # join.all.4 - only one which is 18
    join.location.6 = case_when(
      join.all.4 %in% c("2", "3", "5", "6", "8", "9", "20", "22", "25", "30", "32", "34", "37", "41", "45", "46", "49", "64", "65", "67") ~ "unknown",
      join.all.4 == "18" ~ "18a",
      TRUE ~ join.all.4
    ),
    
    join.location.7 = case_when(
      join.all.4 == "18" ~ "18b",
      TRUE ~ join.all.4
    ),
    
    # Neaten up
    # join them all into one column and then separate out again to remove unecessary NAs and unkowns 
    join.full = paste(join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, sep = ",") %>% # have to use paste instead iof stri_c because of NA values
      stri_replace_all_regex(., ",NA", "") %>% # remove NAs
      stri_replace_all_regex(., ",unknown|unknown,|unknown", "") # remove unknowns
  ) %>%
  
  # remove columns so don't get mixes up when separating back out again
  select(
    - join.location.1:-join.location.7,
    - join.all.1:-join.all.4
  ) %>%
  
  # separate back out into one column per source
  separate(
    join.full, into = c("join.location.1", "join.location.2", "join.location.3", "join.location.4", "join.location.5"), sep = ","
    ) %>% 
  
  # Assign empty in join.location.1 to unknown
  mutate(
    join.location.1 = if_else(
      join.location.1 == "",
      "unknown",
      join.location.1
    )
  ) %>% 
  
  ## Extra info ----
  mutate(
    source.code = 'db-4',
    sex = "female", # paper says it only looked at females in the metadata
    life.stage = "adult", # paper says it only looked at adults in the metadata
    nu = "individual", # all zoo so will be individual
    ind.per.nu = 1,
    
    ## Indivudal.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "hebertF", # stands for hebert formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, sample.year, sample.month, join.location.1, join.location.2, join.location.3, join.location.4, join.location.5,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(hebert_formatted, file = "R/data_outputs/databases/hebert_formatted.rds")



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
      stri_detect_regex(original.taxa.name,  "\\b(?i)nauplii\\b|\\b(?i)Copepodit\\b") ~ "juvenile", # assuming Copepodit means Copepodite
      TRUE ~ "adult"),
    
    ## Extra info ----
    source.code = 'db-5',
    original.source.code.1 = "db-66",
    original.source.code.2 = "db-67",
    original.source.code.3 = "db-68",
    original.source.code.4 = "db-70",
    original.source.code.5 = "db-71",
    original.source.code.6 = "db-72",
    original.source.code.7 = "db-73",
    original.source.code.8 = "db-74",
    original.source.code.9 = "db-75",
    original.source.code.10 = "db-76",
    original.source.code.11 = "db-77",
    original.source.code.12 = "db-78",
    original.source.code.13 = "db-79",
    original.source.code.14 = "db-80",
    original.source.code.15 = "db-81",
    original.source.code.16 = "db-82",
    original.source.code.17 = "db-83",
    original.source.code.18 = "db-84",
    bodysize.measurement = "length",
    units = "mm",
    sample.month = NA, # cannot access orginal papers so set to NA
    sample.year = NA, # cannot access orginal papers so set to NA
    nu = "individual", # all zoo so will be individual
    ind.per.nu = 1,
    join.location.1 = "Gavrilko et al., 2021",
    max.body.size = NA,
    min.body.size = NA,
    measurement.type = "average",
    sample.size = NA, # cannot acces orginal papers so set to NA
    reps = NA, # cannot acces orginal papers so set to NA
    error = NA,
    error.type = NA,
    sex = NA, # cannot acces orginal papers so set to NA
      
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "gavrilkoF", # stands for gavrilko formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
           sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save
saveRDS(gavrilko_formatted, file = "R/data_outputs/databases/gavrilko_formatted.rds")


# Laplace-Treyture ----

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
    nu = Life_Form
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
    nu.bs = as.numeric(na_if(nu.bs, "#NA")),
    
    ## ind.per.nu ----
    # no number given in database so when it is multi-cells then divide nu.cell by cell
    ind.per.nu = case_when(
      is.na(nu.bs) ~ 1,
      !is.na(nu.bs) ~ nu.bs/cell.bs
    )
  ) %>% 
  
  ## Pivot ----
  # put nu and cell biovolumes on seperate rows - don't need the same individual uid because they are different individuals
  pivot_longer(
    cols = cell.bs:nu.bs, values_to = "body.size", names_to = "cell.nu"
  ) %>% 
  
  # remove NA body.sizes
  filter(
    !is.na(body.size)
  ) %>% 
  
  mutate(
    ## Nu ----
    # make an nu column and assign either individual, colony or filament
    nu = case_when(
      cell.nu == "cell.bs" ~ "individual",
      cell.nu == "nu.bs" & nu == "Col." ~ "colony",
      cell.nu == "nu.bs" & nu == "Fil." ~ "filament",
    ),
    
    # change ind.per.nu for individuals
    ind.per.nu = if_else(
      nu == "individual",
      1,
      ind.per.nu
    )
  ) %>% 
  
  # remove redundant columns 
  select(
    - cell.nu
  ) %>% 
  
  mutate(
    ## Extra info ----
    sample.size = "30", # stated in meta data
    source.code = 'db-6',
    original.source.code.1 = "db-6", # data was primary data from this paper
    bodysize.measurement = "biovolume",
    units = "µm^3",
    sample.month = NA,
    sample.year = "2005-2016", # stated n meta data
    join.location.1 = "Laplace-Treyture et al., 2021",
    max.body.size = NA,
    min.body.size = NA,
    measurement.type = "average",
    life.stage = "active", # checked for dormant names and non so set to active
    reps = NA,
    error = NA,
    error.type = NA,
    sex = NA, # all phyto so not needed
    
    ## Individual.uid ----
    # UID for each individual for when there are multiple measurements (e.g. length and width) for one individual so they have the same UID
    uid.db = "ltF", # stands for LT formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # Remove redundant columns
  select(
    - uid.db,
    - uid.no,
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## Save ----
saveRDS(lt_formatted, file = "R/data_outputs/databases/lt_formatted.rds")



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
    max.length = LengthMax,
    maxl.ref = References...10,
    min.length = LengthMin,
    minl.ref = References...12,
    max.width = WidthMax,
    maxw.ref = References...14,
    min.width = WidthMin,
    minw.ref = References...16
  ) %>% 
  
  mutate(
    ## taxa name ----
    # replace _ to " " in original.taxa.name
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "_", " "),
    
    ## body.size ----
    # change ND and OBS to NA and make numeric
    min.length = na_if(min.length, "ND") %>% 
      as.numeric(.),
    
    max.length = as.numeric(max.length),
    
    min.width = na_if(min.width, "ND") %>% 
      as.numeric(.),
    
    max.width = na_if(max.width, "ND") %>% 
      as.numeric(.),
    
    # calculate average from min and max
    length = (min.length+max.length)/2,
    width = (min.width+max.width)/2,
    
    ## Original.source (1) ----
    # merge min and max references into one column - all ended up being the same for min and max anyway so didnt need to do anything extra
    
    join.all.length = case_when( # join.all because will be used for location and extra info later on
      minl.ref == maxl.ref ~ minl.ref,
      minl.ref != maxl.ref ~ paste(minl.ref, maxl.ref, sep = ","),
      is.na(minl.ref) ~ maxl.ref,
      is.na(maxl.ref) ~ minl.ref
      ),
    
    join.all.width = case_when( # join.all because will be used for location and extra info later on
      minw.ref == maxw.ref ~ minw.ref,
      minw.ref != maxw.ref ~ paste(minw.ref, maxw.ref, sep = ","),
      is.na(minw.ref) ~ maxw.ref,
      is.na(maxw.ref) ~ minw.ref
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
    cols = length:width, names_to = "bodysize.measurement", values_to = "body.size"
  ) %>% 
  
  # assign correct info because some will have been duplicated by the pivot step
  
  # min and max body.size depending on if it is length or width
  mutate(
    min.body.size = case_when(
      bodysize.measurement == "length" ~ min.length,
      bodysize.measurement == "width" ~ min.width,
      TRUE ~ NA
    ),
    max.body.size = case_when(
      bodysize.measurement == "length" ~ max.length,
      bodysize.measurement == "width" ~ max.width,
      TRUE ~ NA
    ),
    
    # assign the correct ref
    join.all = if_else(
      bodysize.measurement == "length",
      join.all.length,
      join.all.width
      ),
    
    ## Original.source.code (2) ----
    # change OBS to db-7 as this is code for it came from this paper
    join.all = if_else(
      join.all == "OBS",
      "db-7",
      join.all
      )
    ) %>% 
  
  # left join original.source.codes from db_source_list by join.source column
  left_join(
    select(
      filter(
        db_source_list, db.code == "db-7"
      ), source.code, join.source
    ), by = c("join.all" = "join.source")
  ) %>% 
  
  # rename
  rename(
    original.source.code.1 = source.code
  ) %>% 
  
  mutate(
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
  
  # remove redundant columns 
  select(
    - remove,
    - min.length,
    - max.length,
    - min.width,
    - max.width,
    - minl.ref,
    - maxl.ref,
    - minw.ref,
    - maxw.ref,
    - join.all.length,
    - join.all.width
  ) %>% 
  
  ## Sample.size etc ----
  # left join extra info by join.all column
  left_join(
    select(
      filter(
        db_extra, db.code == "db-7"
        ),- db.code, - experimental.design
      ), by = c("join.all" = "join.extra")
    ) %>% 
  
  mutate(
    # change types for merging later on
    sample.month = as.character(sample.month),
    reps = as.character(reps),
    
    ## Measurement.type ----
    measurement.type = case_when(
      !is.na(body.size) ~ "range",
      is.na(body.size) & !is.na(min.body.size) ~ "min",
      is.na(body.size) & !is.na(max.body.size) ~ "max"
    ),
    
    ## Locations ----
    # there are multiple locations for the same source so need to make two columns
    join.location.1 = case_when(
      join.all == "1" ~ "1a",
      join.all == "13" ~ "13a",
      !(join.all %in% c("1", "13", "14", "21", "31", "43", "70", "76", "77", "db-7")) ~ "unknown",
      TRUE ~ join.all
    ),
    
    join.location.2 = case_when(
      join.all == "1" ~ "1b",
      join.all == "13" ~ "13b",
      TRUE ~ NA
    )
  ) %>% 
  
  # remove redundant columns
  select(
    - join.all
  ) %>% 
  
  mutate(
    ## Extra info ----
    source.code = 'db-7',
    units = "µm",
    nu = "individual",
    ind.per.nu = 1,
    life.stage = "active", # change from extra info to this as checked for any dormant names but none so set to active as all are phyto
  ) %>% 
  
  ## Reorder columns ----
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## save ----
saveRDS(no_formatted, file = "R/data_outputs/databases/no_formatted.rds")


#  Rimet 2012 ----  
# All phyto
rimet2012_formatted <- rimet2012 %>% 
  
  ## Select columns ----
  select(
    `genus + species + var`,
    `Biovolume (µm3)`,
    `length (µm)`,
    `width (µm)`,
    `Reference for sizes`
  ) %>% 
  
  rename(
    original.taxa.name = `genus + species + var`,
    biovolume = `Biovolume (µm3)`,
    length = `length (µm)`,
    width = `width (µm)`,
    join.all = `Reference for sizes` # will be used for join extra info as well
  ) %>% 
  
  mutate(
    ## Individual.uid ----
    # needs to be done before pivot so that the length and width measurements have the same individual uid 
    uid.db = "r12F", # stands for rimet 2012 formatted
    uid.no = row_number(),
    individual.uid = paste(uid.db, uid.no, sep = "")
  ) %>% 
  
  # remove redundnt columns
  select(
    - uid.db,
    - uid.no
  )%>% 
  
  ## Original source ----
  mutate(
    # Remove invisible characters - have already removed all invisible characters in the rimet_2012_sources so just making it match that
    join.all = stri_replace_all_regex(join.all, "[^\\x20-\\x7E]", "")
  ) %>% 
  
  # left join original.source.codes from rimet_2012_sources
  left_join(
    select(
      rimet_2012_sources, source.code, join.source
      ), by = c("join.all" = "join.source")) %>% 
  
  # rename
  rename(
    original.source.code.1 = source.code
  ) %>%
  
  ## Sample.size etc ----
  # left join extra info by join.all column
  left_join(
    select(
      filter(
        db_extra, db.code == "db-8"
        ),- db.code, - experimental.design,
      ), by = c("join.all" = "join.extra")
    ) %>% 
  
  # change types to make mergeing easier late
  mutate(
    reps = as.character(reps),
    sample.month = as.character(sample.month)
  ) %>% 
  
  # remove redundant columns
  select(
    - join.all
  ) %>% 
  
  ## Pivot ----
  # make each length, width and biovol measurement it's own line with the same uid
  pivot_longer(cols = biovolume:width, names_to = "bodysize.measurement", values_to = "body.size") %>% 
  
  mutate(
    ## Units ----
    units = case_when(
      bodysize.measurement %in% c("length", "width") ~ "µm",
      bodysize.measurement == "biovolume" ~ "µm^3"
    ),
    
    ## Extra info ----
    min.body.size = NA,
    max.body.size = NA,
    source.code = 'db-8',
    nu = "individual", # meta data says cells were mesureed so assumed not colonies or filaments
    ind.per.nu = 1,
    join.location.1 = "Rimet et al., 2012", # says all sources are from european sits in meta data so don't need to go through original sources
    life.stage = "active", # checked for dormant names but non so set as active as all phyto so can change from joined extra info
    sex = NA, # all phyto so no sex needed
    measurement.type = "average"
  ) %>% 
  
  ## Order columns ----
  # reorder
  relocate(source.code, original.source.code.1, sample.year, sample.month, join.location.1,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

## save ----
saveRDS(rimet2012_formatted, file = "R/data_outputs/databases/rimet2012_formatted.rds")


# Combine together ----
db_formatted<- bind_rows(rimet_formatted, kremer_formatted, odume_formatted, hebert_formatted, gavrilko_formatted, lt_formatted, no_formatted, rimet2012_formatted) %>% 
  # remove data with a 0 cell.biovol
  filter(
    body.size != 0
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
           sample.year, sample.month, join.location.1, join.location.2, join.location.3, join.location.4, join.location.5,
           individual.uid, original.taxa.name, life.stage, sex, nu, ind.per.nu,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, units, measurement.type, sample.size, reps, error, error.type)

# Save
saveRDS(db_formatted, file = "R/data_outputs/full_database/db_formatted.rds")

