# adding in available functional group info

# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)

# Data ----
bodysize_taxonomy <- readRDS("R/Data_outputs/full_database/bodysize_taxonomy.rds")
species_raw_cell_size <- readRDS("R/Data_outputs/full_database/species_raw_cell_size.rds")

rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet")
lt <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture")
kruk <- read_xlsx("raw_data/kruk_groups.xlsx")

updated_spec_char <- read_xlsx(here("raw_data","manual_taxonomy.xlsx"), sheet = "special_characters")
taxonomy <- readRDS("R/Data_outputs/taxonomy.rds")


# Kruk ----
# add in the data from the kruk paper

## Update names in the kruk data ----
# need to join the resolved version of the names that i have used to the older names in the kruk paper

kruk_names_list <- bodysize_taxonomy %>% # used bodysize_taxonomy because it has the original.taxa.names in
  
  # select just kruk data
  filter(
    source.code == "1313"
  ) %>% 
  
  # select name columns
  select(
    original.taxa.name,
    taxa.name
  ) %>%
  
  # get distinct names
  distinct(
    original.taxa.name, .keep_all = TRUE
  ) 

## Kruk traits ----
# get list of updated names with the reynolds groups and other relevent info
kruk_traits <- kruk %>% 
  
  # select columns
  select(
    Species_name,
    `Classification by Experts`,
    Life_form,
    Aerotopes,
    Flagella,
    Mucilage,
    Akinete,
    Heterocite,
    Wall_of_Si,
    Wall_not_of_Si
  ) %>% 
  
  rename(
    original.taxa.name = Species_name,
    reynolds.group = `Classification by Experts`,
    life.form = Life_form,
    aerotopes = Aerotopes,
    flagella = Flagella,
    mucilage = Mucilage,
    akinete = Akinete,
    heterocite = Heterocite,
    wall_of_Si = Wall_of_Si,
    wall_not_of_Si = Wall_not_of_Si
  ) %>% 
  
  # Need to edit the names to remove species characters as done in join_db script so that the original.taxa.names are the same for left joining
  mutate(
    # remove any random capitals - set just first letter to upper case, gna_verify doesn't work with anything else
    original.taxa.name = tolower(original.taxa.name), # set all to lower case
    original.taxa.name = paste(
      toupper(
        str_sub(
          original.taxa.name, 1,1 # select first letter and set to upper case
        )
      ),
      str_sub(original.taxa.name, 2), # paste remaining word
      sep = ""
    ),
    
    # add in *SpecChar* to replace special characters
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*"),
  ) %>% 
  
  # Join updated names
  left_join(
    updated_spec_char, by = "original.taxa.name"
  ) %>% 
  
  mutate(
    # replace old with new
    original.taxa.name = if_else(
      !is.na(new.taxa.name), new.taxa.name, original.taxa.name
    ),
    
    # Remove any white spaces if there are any
    original.taxa.name = trimws(original.taxa.name)
  ) %>% 
  
  # remove redundant columns
  select(
    - new.taxa.name
  ) %>% 
  
  # add in the new names
  left_join(kruk_names_list, by = "original.taxa.name") %>% 
  
  # remove original.taxa.name
  select(-original.taxa.name) %>% 
  
  # get only distinct non NA names
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  # edit format to make it fir twith everything else
  mutate(
    
    reynolds.group = toupper(reynolds.group), 
    
    life.form = case_when(
      life.form == 1 ~ "Cell",
      life.form == 2 ~ "Colonial",
      life.form == 3 ~ "Filamentous",
      TRUE ~ NA
    ),
    
    mobility.apparatus = if_else(
      flagella == 1,
      "Flagella",
      NA
    ),
    
    siliceous.wall = case_when(
      wall_of_Si == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    mucilage = case_when(
      mucilage == 1 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  select(
    taxa.name, reynolds.group, life.form, mobility.apparatus, siliceous.wall, mucilage
    )

# Rimmet ----
# get all the functional trait data from rimmet

## Update names in the rimmet data ----
# need to join the resolved version of the names that i have used to the older names in the rimmet paper

rimmet_names_list <- bodysize_taxonomy %>% # used bodysize_taxonomy because it has the original.taxa.names in
  
  # select just kruk data
  filter(
    source.code == "db-1"
  ) %>% 
  
  # select name columns
  select(
    original.taxa.name,
    taxa.name
  ) %>%
  
  # get distinct names
  distinct(
    original.taxa.name, .keep_all = TRUE
  ) 

## get list of traits ----

rimmet_traits <- rimmet %>% 
  
  select(
    `Genus + species name`,
    `Functional groups (Reynolds 2002)`,
    `Functional groups (Padisak 2009)`,
    `Morpho-classification (Kruk 2010)`,
    `Mobility apparatus`,
    `Mobility apparatus: Flagella`,
    `Mobility apparatus: Raphe`,
    Colonial,
    Filament,
    Heterotrophic,
    Mixotrophic,
    Autotrophic
  ) %>% 
  
  rename(
    original.taxa.name = `Genus + species name`,
    reynolds.group = `Functional groups (Reynolds 2002)`,
    padisak.group = `Functional groups (Padisak 2009)`,
    morpho.classification = `Morpho-classification (Kruk 2010)`,
    mobility.apparatus = `Mobility apparatus`,
    flagella = `Mobility apparatus: Flagella`,
    raphe = `Mobility apparatus: Raphe`,
    colonial = Colonial,
    filament = Filament,
    heterotrophic = Heterotrophic,
    mixotrophic = Mixotrophic,
    autotrophic = Autotrophic
  ) %>% 
  
  # Need to edit the names to remove species characters as done in join_db script so that the original.taxa.names are the same for left joining
  mutate(
    # remove any random capitals - set just first letter to upper case, gna_verify doesn't work with anything else
    original.taxa.name = tolower(original.taxa.name), # set all to lower case
    original.taxa.name = paste(
      toupper(
        str_sub(
          original.taxa.name, 1,1 # select first letter and set to upper case
        )
      ),
      str_sub(original.taxa.name, 2), # paste remaining word
      sep = ""
    ),
    
    # add in *SpecChar* to replace special characters
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*"),
  ) %>% 
  
  # Join updated names
  left_join(
    updated_spec_char, by = "original.taxa.name"
  ) %>% 
  
  mutate(
    # replace old with new
    original.taxa.name = if_else(
      !is.na(new.taxa.name), new.taxa.name, original.taxa.name
    ),
    
    # Remove any white spaces if there are any
    original.taxa.name = trimws(original.taxa.name)
  ) %>% 
  
  # remove redundant columns
  select(
    - new.taxa.name
  ) %>% 
  
  left_join(
    rimmet_names_list, by = "original.taxa.name"
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  select(-original.taxa.name) %>% 
  
  # edit format to fit with everything else
  mutate(
    
    reynolds.group = toupper(reynolds.group),
    
    padisak.group = toupper(padisak.group),
    
    life.form = case_when(
      colonial == 0 & filament == 0 ~ "Cell",
      colonial == 1 & filament == 0 ~ "Colonial",
      colonial == 0 & filament == 1 ~ "Filamentous",
      colonial == 1 & filament == 1 ~ "Filamentous/Colonial",
      TRUE ~ NA
      ),
    
    motile = case_when(
      mobility.apparatus == 0 ~ "No",
      mobility.apparatus == 1 ~ "Yes"
    ),
    
    mobility.apparatus = case_when(
      flagella == 1 ~ "Flagella",
      raphe == 1 ~ "Raphe",
      TRUE ~ NA
    ),
    
    feeding.guild = case_when(
      heterotrophic == 1 ~ "Heterotroph",
      autotrophic == 1 ~ "Autotroph",
      mixotrophic == 1 ~ "Mixotroph",
      TRUE ~ NA
    )
  ) %>% 
  select(
    taxa.name, life.form, feeding.guild, motile, mobility.apparatus, reynolds.group, padisak.group, morpho.classification, 
  )

# LT ----

lt_names_list <- bodysize_taxonomy %>% # used bodysize_taxonomy because it has the original.taxa.names in
  
  # select just kruk data
  filter(
    source.code == "db-6"
  ) %>% 
  
  # select name columns
  select(
    original.taxa.name,
    taxa.name
  ) %>%
  
  # get distinct names
  distinct(
    original.taxa.name, .keep_all = TRUE
  ) 

## get list of traits ----

lt_traits <- lt %>% 
  
  select(
    Taxa_Name,
    Nutrition,
    Reynolds_Group,
    Siciliceous_Skeleton,
    Mucilage,
    Motility,
    Flagellum,
    Life_Form
  ) %>% 
  
  rename(
    original.taxa.name = Taxa_Name,
    reynolds.group = Reynolds_Group,
    mobility = Motility,
    flagella = Flagellum,
    feeding.guild = Nutrition,
    mucilage = Mucilage,
    siliceous.wall = Siciliceous_Skeleton,
    life.form = Life_Form
  ) %>% 
  
  # Need to edit the names to remove species characters as done in join_db script so that the original.taxa.names are the same for left joining
  mutate(
    # remove any random capitals - set just first letter to upper case, gna_verify doesn't work with anything else
    original.taxa.name = tolower(original.taxa.name), # set all to lower case
    original.taxa.name = paste(
      toupper(
        str_sub(
          original.taxa.name, 1,1 # select first letter and set to upper case
        )
      ),
      str_sub(original.taxa.name, 2), # paste remaining word
      sep = ""
    ),
    
    # add in *SpecChar* to replace special characters
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*"),
  ) %>% 
  
  # Join updated names
  left_join(
    updated_spec_char, by = "original.taxa.name"
  ) %>% 
  
  mutate(
    # replace old with new
    original.taxa.name = if_else(
      !is.na(new.taxa.name), new.taxa.name, original.taxa.name
    ),
    
    # Remove any white spaces if there are any
    original.taxa.name = trimws(original.taxa.name)
  ) %>% 
  
  # remove redundant columns
  select(
    - new.taxa.name
  ) %>% 
  
  left_join(
    lt_names_list, by = "original.taxa.name"
  ) %>% 
  
  # get distinct non NAs
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  select(-original.taxa.name) %>% 
  
  # make format changes to fit others
  mutate(
    
    reynolds.group = toupper(reynolds.group),
    
    motile = case_when(
      mobility == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    reynolds.group = na_if(reynolds.group, "#NA"),
    
    mobility.apparatus = case_when(
      flagella == 1 ~ "Flagella",
      TRUE ~ NA
    ),
    
    siliceous.wall = case_when(
      siliceous.wall == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    mucilage = case_when(
      mucilage == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    life.form = case_when(
      life.form == "Cel." ~ "Cell",
      life.form == "Col." ~ "Colonial",
      life.form == "Fil." ~ "Filamentous"
    )
  ) %>% 
  
  select(
    taxa.name, life.form, feeding.guild, motile, mobility.apparatus, reynolds.group, siliceous.wall, mucilage
  )

# Join together ----
# chose the most recent paper as preferece when multiple

functional_traits <- full_join(kruk_traits, rimmet_traits, by = "taxa.name", suffix = c(".kruk", ".rimmet")) %>% 
  
  mutate(
    mobility.apparatus = case_when(
      !is.na(mobility.apparatus.rimmet) ~ mobility.apparatus.rimmet,
      TRUE ~ mobility.apparatus.kruk
    ),
    
    life.form = case_when(
      life.form.rimmet == "Filamentous/Colonial" & !is.na(life.form.kruk) ~ life.form.kruk,
      !is.na(life.form.rimmet) ~ life.form.rimmet,
      TRUE ~ life.form.kruk
    ),
    
    reynolds.group = case_when(
      !is.na(reynolds.group.rimmet) ~ reynolds.group.rimmet,
      TRUE ~ reynolds.group.kruk
    )
  ) %>% 
  
  select(
    - mobility.apparatus.kruk,
    - mobility.apparatus.rimmet,
    - life.form.kruk,
    - life.form.rimmet,
    - reynolds.group.kruk,
    - reynolds.group.rimmet
  ) %>% 
  full_join(., lt_traits, by = "taxa.name", suffix = c(".old", ".lt")) %>% 
  
  mutate(
    life.form = case_when(
      life.form.old == "Filamentous/Colonial" & life.form.lt %in% c("Colonial", "Filamentous") ~ life.form.lt,
      !is.na(life.form.old) ~ life.form.old,
      TRUE ~ life.form.lt
    ),
    
    reynolds.group = case_when(
      !is.na(reynolds.group.old) ~ reynolds.group.old,
      TRUE ~ reynolds.group.lt
    ),
    
    siliceous.wall = case_when(
      !is.na(siliceous.wall.old) ~ siliceous.wall.old,
      TRUE ~ siliceous.wall.lt
    ),
    
    mucilage = case_when(
      !is.na(mucilage.old) ~ mucilage.old,
      TRUE ~ mucilage.lt
    ),
    
    feeding.guild = case_when(
      !is.na(feeding.guild.old) ~ feeding.guild.old,
      TRUE ~ feeding.guild.lt
    ),
    
    motile = case_when(
      !is.na(motile.old) ~ motile.old,
      TRUE ~ motile.lt
    ),
    
    mobility.apparatus = case_when(
      !is.na(mobility.apparatus.old) ~ mobility.apparatus.old,
      TRUE ~ mobility.apparatus.lt
    )
  ) %>% 
  
  select(
    taxa.name, life.form, reynolds.group, padisak.group, morpho.classification, feeding.guild, motile, mobility.apparatus, mucilage, siliceous.wall
  )

# Add to taxonomy ----

traits <- taxonomy %>% 
  
  left_join(
    ., functional_traits, by = "taxa.name"
  ) %>% 
  
  mutate(
    group = case_when(
      phylum %in% c("Cyanobacteria", "Glaucophyta") ~ "Blue/green",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Green",
      phylum == "Rhodophyta" ~ "Red",
      class == "Bacillariophyceae" ~ "Diatom",
      class %in% c("Chrysophyceae", "Dictyochophyceae") ~ "Golden-brown",
      class == "Dinophyceae" ~ "Dinoflagellate",
      phylum == "Euglenozoa" ~ "Euglenoid",
      class == "Raphidophyceae" ~ "Raphidophytes",
      class == "Xanthophyceae" ~ "Yellow-green",
      class == "Phaeophyceae" ~ "Brown",
      class == "Eustigmatophyceae" ~ "Eustigmatophytes",
      phylum == "Cryptophyta" ~ "Cryptomonads",
      phylum == "Haptophyta" ~ "Haptophytes",
      
      TRUE ~ NA
    )
  )

# save
saveRDS(traits, file = "R/Data_outputs/traits.rds")

# Add to main data ----

species_traits <- species_raw_cell_size %>% 
  left_join(select(
    traits, tax.uid, life.form, reynolds.group, padisak.group, morpho.classification, feeding.guild, motile, mobility.apparatus, mucilage, siliceous.wall, group
  ), by = "tax.uid") %>% 
  
  filter(
    !is.na(mass)
  )

# save
saveRDS(species_traits, file = "R/Data_outputs/species_traits.rds")






