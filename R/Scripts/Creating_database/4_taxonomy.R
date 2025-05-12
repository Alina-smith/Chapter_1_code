# Aim of script: Getting taxonomy info for all the species and adding it to the raw data

# Flow of script:
# 1) Clean names:     Run the names through taxize::gna_verifier to fix spelling mistakes and then manually fix any that weren't picked up
# 2) Resolved names:  Run the cleaned names through rotl::tnrs_match_names to get most up to date names and then manually fix any that weren't picked up
# 3) Taxonomy:        Run cleaned names through taxize::classification with tol to get taxonomy and then manuallu fill in any gaps

# Last updated: 18/03/2025
# Data run through veryfier on 19/01/2025
# Data run through tnrs_match_names on 19/03/2025 
# Data ran through classification on 19/03/2025

# Packages ----

# Installing
library(devtools)
install_github("ropensci/bold")
install_github("ropensci/taxize")

# Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
  BiocManager::install(version = "3.20")

BiocManager::install(c("ggtree","treeio","ggreeExtra"), force = TRUE)
BiocManager::install("ggtreeExtra")
BiocManager::install("treeio", force = TRUE)
BiocManager::install("ggtree", force = TRUE)

# library
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)
library(ape)
library(ggnewscale)

library(ggtree)
library(treeio)
library(ggtreeExtra)

# Clean names ----

## gna_verifier ----

### Import data ----
bodysize_raw <- readRDS("R/data_outputs/database_products/final_products/bodysize_raw.rds")

### Distinct names ----

# Get a list of all distinct names 
names_list <- select(bodysize_raw, original.taxa.name) %>% 
  
  distinct(original.taxa.name) %>% 
  
  # Convert to a string of names
  pull(original.taxa.name)

# View names
glimpse(names_list)

### run through gna_verifier ----
# Run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
cleaned_gna <- do.call(rbind, lapply(names_list, function(name) {
  tryCatch(
    {
      # Process each name and return the result
      gna_verifier(name) %>%
        as.data.frame() %>%
        select(submittedName, matchedCanonicalFull, dataSourceTitleShort)
    },
    error = function(e) {
      # Fallback for errors - fill columns with NAs
      data.frame(
        submittedName = name,
        matchedCanonicalFull = NA,
        dataSourceTitleShort = NA)
      }
    ) 
  }
  )
  ) %>% 
  rename(
    original.taxa.name = submittedName,
    cleaned.taxa.name.gna = matchedCanonicalFull,
    cleaned.source.gna = dataSourceTitleShort
  )

### Save ----
saveRDS(cleaned_gna, file = "R/data_outputs/database_products/taxonomy/cleaned_gna.rds")





## Manual ----

### Import data ----
cleaned_gna <- readRDS("R/data_outputs/database_products/taxonomy/cleaned_gna.rds")

### List to clean ----
# Find taxa.names from clean_gna to manually resolved

to_clean_manually <- cleaned_gna %>% 
  
  mutate(
    
    manual = case_when(
      # Ones that weren't picked up by gna_veryier at all - gave NA for the cleaned.taxa.name
      is.na(cleaned.taxa.name.gna) ~ "na",
      
      # Ones that were bumped up a taxonomic rank by the cleaner - had two words in original.species.name (contain a space) but one word in the cleaned name (no space)
      stri_detect_regex(original.taxa.name, " ") & 
        !(stri_detect_regex(cleaned.taxa.name.gna, " ")) & 
        !(stri_detect_regex(original.taxa.name, "\\b(?i)sp\\.|\\b(?i)spp\\.|\\b(?i)sp\\b|\\b(?i)spp\\b|\\b(?i)sp(\\d+)|\\b(?i)ssp\\b")) ~ "bumped",
      
      # Ones where the cleaned.taxa.name was set to the name of the juvenile form or a common name instead of the taxa.name (e.g. nauplii or cyclops instead of copepoda)
      stri_detect_regex(cleaned.taxa.name.gna, "\\b(?i)cyst\\b|\\b(?i)stomatocyst\\b|\\b(?i)nauplius\\b|\\b(?i)centric\\b|\\b(?i)volvocales\\b|\\b(?i)cyclops\\b|\\b(?i)mite\\b") ~ "juvenile",
      
      # Unknown
      stri_detect_regex(cleaned.taxa.name.gna, "\\b(?i)unknown\\b") ~ "unknown",
      
      TRUE ~ "keep"
    )
  ) %>% 
  
  filter(
    !(manual == "keep")
  )

# Save
write_csv(to_clean_manually, "R/data_outputs/database_products/taxonomy/to_clean_manually.csv") # Will manually resolve in excel sheet and then import back in

### Update names ----
# Update cleaned.taxa.name with the manually cleaned names

# Import the manually cleaned names
manually_cleaned <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "cleaned")

# Replace cleaned.taxa.name with manually cleaned names when ones is present
cleaned <- cleaned_gna %>% 
  
  # left join all the manually cleaned ones from manualy_cleaned spreadsheet
  left_join(
    manually_cleaned,
    by = "original.taxa.name"
  ) %>% 
  
  # when a name has been cleaned manually (cleaned.source = "manually") then select that name else keep the current one
  mutate(
    cleaned.taxa.name = if_else(
      !is.na(cleaned.source.manual),
      cleaned.taxa.name.manual,
      cleaned.taxa.name.gna
    ),
    
    # When there is a variety or form that still has var. or f. remove this so it goes through tnrs_match_name easier
    cleaned.taxa.name = case_when(
      stri_detect_regex(cleaned.taxa.name, " var\\.| f\\.") ~ stri_replace_first_regex(cleaned.taxa.name, " var\\.| f\\.", ""),
      
      TRUE ~ cleaned.taxa.name
    )
  ) %>% 
  
  select(
    original.taxa.name,
    cleaned.taxa.name
  ) %>% 
  
  # remove any that couldn't be cleaned
  filter(
    !(is.na(cleaned.taxa.name))
  )
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

### Save ----
saveRDS(cleaned, file = "R/data_outputs/database_products/taxonomy/cleaned.rds")





# Resolve names ----

## tnrs_match_names ----

### Import data ----
cleaned <- readRDS("R/data_outputs/database_products/taxonomy/cleaned.rds")

### Run through tnrs_match_names ----
# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)

resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name)

### Save ----
saveRDS(resolved_tol, file = "R/data_outputs/database_products/taxonomy/resolved_tol.rds")



## Manual ----

### List to resolve ----
# Find ones that need to be manually resolved

# Import resolved_tol
resolved_tol <- readRDS("R/data_outputs/database_products/taxonomy/resolved_tol.rds")

# Select ones that weren't picked up by tol
to_resolve_manually <- resolved_tol %>% 
  
  filter(
    is.na(unique_name)
  )

# Save
write_csv(to_resolve_manually, "R/data_outputs/database_products/taxonomy/to_resolve_manually.csv") # Will manually resolve in excel sheet and then import back in

### Update names ----
# Add in the manually resolved names to the full name list

# Import the manually resolved names
manually_resolved_subset <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "resolve")

# Add to main names list
resolved_manual <- left_join(resolved_tol, manually_resolved_subset, by = "search_string") %>% 
  
  # When a manually resolved name is present use this instead use the original resolved name
  mutate(
    resolved.taxa.name = if_else(
      !is.na(unique_name_manual),
      unique_name_manual,
      unique_name
      )
    ) %>%
  
  # Remove any that couldn't be resolved
  filter(
    !is.na(resolved.taxa.name)
  ) %>% 
  
  # Select relevant columns
  select(
    search_string, unique_name, resolved.taxa.name
  )

### Save ----
saveRDS(resolved_manual, "R/data_outputs/database_products/taxonomy/resolved_manual.rds")



## tnrs_match_names rerun ----
# Rerun resolved manual through tnrs_match_name to get missing otts

### Import data ----
resolved_manual <- readRDS("R/data_outputs/database_products/taxonomy/resolved_manual.rds")

### tnrs_match_names ----
# Run through tnrs_match_names
# pmc - pre_mult_check
resolved_pmc <- tnrs_match_names(resolved_manual$resolved.taxa.name)

# check for any that weren't picked up - if false then fine
unique(is.na(resolved_pmc$ott_id))

### Save ----
saveRDS(resolved_pmc, "R/data_outputs/database_products/taxonomy/resolved_pmc.rds")





# Taxonomy: ----

## tax_lineage ----

### Import data ----
resolved_pmc <- readRDS("R/data_outputs/database_products/taxonomy/resolved_pmc.rds")

### Lineage ----
# Get the taxonomy lineage from tol

taxonomy_raw_pmc <- resolved_pmc %>% 
  
  mutate(
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for that taxa
      tax_lineage(taxonomy_taxon_info(ott_id, include_lineage = TRUE))# rows = 1 so that is only takes the first one and doesn't require you select options for each one
    )[[1]] # select the first element of the list
  ) %>% 
  
  # take the info out of the list column so now it has multiple of the same search_string
  unnest(
    col = everything(), names_sep = "."
  ) %>% 
  
  filter(
    tax.rank != "no rank"
  ) %>% 
  
  select(
    unique_name,
    ott_id,
    tax.unique_name,
    tax.rank
  ) %>% 
  
  # Pivot so the ranks are now columns
  pivot_wider(
    names_from = tax.rank,
    values_from = tax.unique_name) %>% 
  
  unnest_wider(
    col = everything(), names_sep = "."
  )

### Save ----
saveRDS(taxonomy_raw_pmc, file = "R/data_outputs/database_products/taxonomy/taxonomy_raw_pmc.rds")



## Format taxonomy ----

### Initial edits ----
# Do any edits that can easily be done within mutate

#### Import data ----
taxonomy_raw_pmc <- readRDS("R/data_outputs/database_products/taxonomy/taxonomy_raw_pmc.rds")
not_genus <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "not_genus")

#### Easy edits ----
taxonomy_formatted_pmc <- taxonomy_raw_pmc %>% 
  
  # Just rename to make easier
  rename(
    resolved.taxa.name = unique_name.1,
    ott.id = ott_id.1
  ) %>% 
  
  mutate(
    
    # Filling in gaps - this isn't changing anything to do with the phylogeny it is just filling in gaps in ranks
    # Some have multiple outputs for one rank so for each one they were cone through a checked which output is the correct one
    # Some have had the correct name put in the wrong rank so fixing these ones as well
    
    # Species
    # It doesn't have the species in the lineage info so need to find all that are at species level (has a space) some have a space because they have brackets so need to remove brackets first
    
    name.crop = if_else(
      stri_detect_regex(resolved.taxa.name, "\\("),
      stri_extract_first_regex(resolved.taxa.name, ("\\w+ \\w*")),
      resolved.taxa.name
    ),
    
    name.crop = stri_trim(name.crop),
    
    species = case_when(
      !is.na(species.1) ~ species.1,
      stri_detect_regex(name.crop, " ") ~ name.crop,
      TRUE ~ NA
    ),
    
    # genus
    # First just do ones I have
    genus = case_when(
      stri_detect_regex(name.crop, "Euglena") ~ "Euglena (genus in infrakingdom Excavata)", 
      stri_detect_regex(name.crop, "Cryptaulax") ~ "Cryptaulax",
      stri_detect_regex(name.crop, "Amoeba") ~ "Amoeba", 
      stri_detect_regex(name.crop, "Karlodinium") ~ "Karlodinium",
      stri_detect_regex(name.crop, "Karenia") ~ "Karenia",
      
      !is.na(genus.2) ~ genus.2,
      
      TRUE ~ genus.1,
    ),
    
    # Now select any that have a species and use the genus name
    # When species is filled but genus is missing select just the first name in the species name
    genus = if_else(
      is.na(genus) & !is.na(species),
      stri_extract_first_regex(species, "\\w+"),
      genus
    ),
    
    # Finally just take the resolved.taxa.name and then if it is a higher rank can change it later on
    genus = if_else(
      is.na(genus),
      resolved.taxa.name,
      genus
    ),
    
    genus = if_else(
      genus %in% not_genus$name,
      NA,
      genus
    ),
    
    # family
    family = case_when(
      genus == "Euglena (genus in infrakingdom Excavata)" ~ "Euglenaceae",
      
      resolved.taxa.name %in% c("Empididae", "Ephydridae", "Caenidae", "Aeshnidae", "Gomphidae", "Ecnomidae", "Glossosomatidae", "Hydroptilidae", "Pisuliidae", "Stenopsychidae", "Dixidae", "Tanyderidae", "Thaumaleidae", "Chironomidae", "Culicidae", "Xiphocentronidae", "Calamoceratidae") ~ resolved.taxa.name,
      
      family.1 == "Bicosoecidae" ~ "Bicosoecaceae",
      
      TRUE ~ family.1
    ),
    
    # order
    order = case_when(
      family == "Euglenaceae" ~ "Euglenales",
      phylum.1 == "Euglenida" ~ "Euglenida",
      resolved.taxa.name %in% c("Chrysomeridales", "Euglenida", "Amphipoda", "Plecoptera (order in cohort Polyneoptera)", "Ephemeroptera", "Coleoptera") ~ resolved.taxa.name,
      
      order.1 == "Kinetoplastea" ~ NA,
      order.1 == "Bicosoecida" ~ "Bicosoecales",
      
      TRUE ~ order.1
    ),
    
    # class
    class = case_when(
      order == "Euglenales" ~ "Euglenophyceae",
      order == "Euglenida" ~ "Euglenoidea",
      
      resolved.taxa.name %in% c("Dinophyceae", "Raphidophyceae", "Cryptophyceae", "Chrysophyceae") ~ resolved.taxa.name,
      
      class.1 == "Prymnesiophyceae" ~ "Coccolithophyceae", # updating name
      class.1 %in% c("Glaucophyta", "Haptophyta") ~ NA, # phylum was put into class column
      order.2 == "Kinetoplastea" ~ order.2, # class was put into order column
      phylum.1 == "Cryptophyceae" ~ "Cryptophyceae",
      
      TRUE ~ class.1
    ),
    
    phylum = case_when(
      
      class.1 %in% c("Glaucophyta", "Haptophyta") ~ class.1, # phylum was put into class column
      class.2 == "Haptophyta" ~ class.2, # phylum was put into class column
      
      !is.na(phylum.2) ~ phylum.2,
      
      TRUE ~ phylum.1
    ),
    
    domain = domain.1
  ) %>% 
  
  select(
    resolved.taxa.name, ott.id, species, genus, family, order, class, phylum, domain
  )

### Fill family ----
# want to manually input missing ranks for family as will take too long to do in a case_when

# Make a list of missing family with their genus
missing_family <- taxonomy_formatted_pmc %>% 
  select(
    genus,
    family
  ) %>% 
  
  filter(
    is.na(family)
  ) %>% 
  
  distinct(
    genus
  )

# save
write_csv(missing_family, "R/data_outputs/database_products/taxonomy/missing_family.csv")

# Import the manually filled family
manually_family <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "family")

# Update family
taxonomy_family <- taxonomy_formatted_pmc %>% 
  
  left_join(manually_family, by = "genus", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    family = if_else(
      !is.na(family.manual),
      family.manual,
      family.tol
    ), 
    
    family = case_when(
      resolved.taxa.name %in% c("Potamididae", "Bithyniidae", "Littorinidae", "Bosminidae", "Daphniidae", "Desmidiaceae", "Chlorellaceae", "Stephanodiscaceae", "Characiaceae", "Chlamydomonadaceae",
                       "Palmellaceae","Microcystaceae", "Closteriaceae", "Fragilariaceae", "Aphanizomenonaceae", "Tetrasporaceae", "Nostocaceae", "Oscillatoriaceae", "Gymnodiniaceae",
                       "Thalassiosiraceae", "Chromulinaceae") ~ resolved.taxa.name,
      
      TRUE ~ family
    )
  ) %>% 
  
  select(
    - family.manual,
    - family.tol
  )

### Fill order ----
# Want to manually input missing ranks for order as will take too long to do in a case_when

# Make a list of missing order with their family
missing_order <- taxonomy_family %>% 
  select(
    family,
    order
  ) %>% 
  
  filter(
    is.na(order)
  ) %>% 
  
  distinct(
    family
  )

# Save
write_csv(missing_order, "R/data_outputs/database_products/taxonomy/missing_order.csv")

# Import the manually filled order
manually_order <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "order")

# Update order
taxonomy_order <- taxonomy_family %>% 
  
  left_join(manually_order, by = "family", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    order = if_else(
      !is.na(order.manual),
      order.manual,
      order.tol
    ),
    
    order = case_when(
      resolved.taxa.name %in% c("Chromulinales", "Gymnodiniales", "Ochromonadales", "Oscillatoriales", "Peridiniales", "Chroococcales", "Chlamydomonadales", "Pennales") ~ resolved.taxa.name,
      
      family == "Collodictyonidae" ~ "Diphylleida", # one that got missed
      family == "Chlorellaceae" ~ "Chlorellales",
      TRUE ~ order
    )
  ) %>% 
  
  select(
    - order.manual,
    - order.tol
  ) %>% 
  
  relocate(
    ott.id, resolved.taxa.name, species, genus, family, order, class, phylum
  )


### Final edits ----
# Fill in all the rest of the ranks

taxonomy_pmc <- taxonomy_order %>% 
  
  mutate(
    #### Class ----
    
    class = case_when(
      class == "Insecta" ~ "Hexapoda",
      class == "Prymnesiophyceae" ~ "Coccolithophyceae",
      
      order == "Eustigmatales" ~ "Eustigmatophyceae",
      order == "Pyramimonadales" ~ "Pyramimonadophyceae",
      order %in% c("Fragilariales", "Tabellariales", "Coscinodiscales", "Aulacoseirales", "Melosirales", "Thalassiosirales", "Leptocylindrales", "Paraliales") ~ "Bacillariophyceae",
      order == "Synurales" ~ "Chrysophyceae",
      order == "Bangiales" ~ "Bangiophyceae",
      order == "Chrysomeridales" ~ "Chrysomeridophyceae",
      order %in% c("Chroococcales", "Oscillatoriales", "Nostocales", "Pseudanabaenales", "Pleurocapsales", "Synechococcales", "Nodosilineales", "Spirulinales", "Leptolyngbyales") ~ "Cyanophyceae",
      order %in% c("Noctilucales", "Gymnodiniales") ~ "Dinophyceae",
      order == "Vaginulinida" ~ "Nodosariata",
      order == "Euglenales" ~ "Euglenophyceae",
      order == "Mischococcales" ~ "Xanthophyceae",
      order %in% c("Gomontiellales", "Coleofasciculales", "Pelonematales", "Chroococcidiopsidales", "Gloeobacterales", "Geitlerinematales") ~ "Cyanophyceae",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order == "Picocystales" ~ "Picocystophyceae",
      order == "Bicosoecales" ~ "Bicosoecophyceae", 
      order == "Chaetonotida" ~ "Gastrotricha incertae sedis",
      order == "Craspedida" ~ "Choanoflagellatea",
      order == "Cryptomonadales" ~ "Cryptophyceae",
      order == "Centrohelida" ~ "Centrohelea",
      order == "Micrococcales" ~ 'Actinomycetia',
      order %in% c("Glaucocystales", "Gloeochaetales") ~ "Glaucophyceae",
      order == "Tetrahymenida" ~ "Oligohymenophorea",
      order == "Euamoebida" ~ "Tubulinea",
      order == "Chloroflexales" ~ "Chloroflexi",
      order == "Diphylleida" ~ "Diphyllatea",
      order == "Chlorellales" ~ "Trebouxiophyceae",
      order == "Neobodonida" ~ "Kinetoplastea",
      order == "Goniochloridales" ~ "Eustigmatophyceae",
      
      resolved.taxa.name == "Prymnesiophyceae" ~ "Coccolithophyceae",
      resolved.taxa.name == "Insecta" ~ "Hexapoda",
      resolved.taxa.name %in% c("Chlorophyceae", "Trebouxiophyceae", "Bacillariophyceae", "Zygnemophyceae", "Arachnida", "Bdelloidea (class in Lophotrochozoa)", "Ostracoda") ~ resolved.taxa.name,
      
      TRUE ~ class
    ),
    
    #### Phylum ----
    phylum = case_when(
      phylum == "Streptophyta" ~ "Charophyta",
       
      class %in% c("Dictyochophyceae", "Chrysophyceae", "Xanthophyceae", "Phaeothamniophyceae", "Raphidophyceae", "Actinophryidae", "Eustigmatophyceae", "Chrysomeridophyceae") ~ "Ochrophyta",
      class == "Dinophyceae" ~ "Myzozoa",
      class %in% c("Kinetoplastea", "Euglenophyceae", "Euglenoidea") ~ "Euglenozoa",
      class == "Cryptophyceae" ~ "Cryptophyta",
      class == "Choanoflagellatea" ~ "Choanozoa",
      class == "Cyanophyceae" ~ "Cyanobacteria", 
      class == "Centrohelea" ~ "Heliozoa",
      class == "Oligohymenophorea" ~ "Ciliophora (phylum in subkingdom SAR)",
      class == "Tubulinea" ~ "Amoebozoa",
      class == "Diphyllatea" ~ "Sulcozoa",
      class == "Bicosoecophyceae" ~ "Bigyra",
      class == "Trebouxiophyceae" ~ "Chlorophyta",
      
      resolved.taxa.name == "Dinoflagellata" ~ "Myzozoa",
      resolved.taxa.name %in% c("Cyanobacteria", "Chlorophyta", "Rotifera", "Haptophyta", "Ciliophora (phylum in subkingdom SAR)") ~ resolved.taxa.name,
      
      TRUE ~ phylum
    ),
    
    #### Kingdom ----
    kingdom = case_when(
      phylum %in% c("Cyanobacteria",  "Proteobacteria (phylum silva:A16379/#2)", "Chloroflexi", "Actinobacteria") ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Rhodophyta", "Glaucophyta", "Cryptophyta", "Charophyta") ~ "Plantae",
      phylum %in% c("Ochrophyta", "Bacillariophyta", "Haptophyta", "Bigyra", "Myzozoa", "Ciliophora (phylum in subkingdom SAR)", "Cercozoa", "Foraminifera", "Heliozoa") ~ "Chromista",
      phylum %in% c("Euglenozoa", "Amoebozoa", "Choanozoa", "Sulcozoa") ~ "Protozoa",
      phylum %in% c("Arthropoda", "Mollusca", "Rotifera", "Gastrotricha", "Cnidaria", "Chordata", "Annelida", "Platyhelminthes", "Nematoda") ~ "Animalia",
      phylum %in% c("Ascomycota", "Basidiomycota") ~ "Fungi",
      
      TRUE ~ NA
    ),
    
    #### Type ----
    type = case_when(
      
      # One that aren't plankton - need to look over these incase any have multuples and need a different one selecting
      kingdom == "Fungi" ~ "not plankton",
      phylum %in% c("Proteobacteria (phylum silva:A16379/#2)", "Chloroflexi", "Actinobacteria", "Annelida", "Platyhelminthes", "Nematoda", "Cnidaria", "Mollusca", "Gastrotricha", "Rhodophyta") ~ "not plankton",
      class %in% c("Amphibia", "Anthozoa", "Arachnida", "Malacostraca", "Hexapoda") ~ "not plankton",
      
      # Ones that are plankton
      kingdom == "Plantae" ~ "Phytoplankton",
      phylum == "Cyanobacteria" ~ "Phytoplankton",
      phylum %in% c("Ochrophyta", "Haptophyta", "Myzozoa", "Euglenozoa", "Bacillariophyta", "Cryptophyta") ~ "Phytoplankton",
      
      phylum %in% c("Cercozoa", "Amoebozoa", "Foraminifera", "Ciliophora (phylum in subkingdom SAR)", "Rotifera", "Heliozoa", "Sulcozoa", "Choanozoa", "Bigyra") ~ "Zooplankton",
      class %in% c("Branchiopoda", "Hexanauplia", "Ostracoda") ~ "Zooplankton",
      
      TRUE ~ NA
    )
  ) %>% 
  
  relocate(
    resolved.taxa.name, ott.id, type, species, genus, family, order, class, phylum, kingdom, domain
  ) %>% 
  
  filter(
    !is.na(type)
  )

#### Save ----
saveRDS(taxonomy_pmc, "R/data_outputs/database_products/taxonomy/taxonomy_pmc.rds")




# Multi check 1 ----
# Checking for multiple hits in the ones that were classified into not plankton to see if any have been assigned wrong

## Import data ----
taxonomy_pmc <- readRDS("R/data_outputs/database_products/taxonomy/taxonomy_pmc.rds")
resolved_pmc <- readRDS("R/data_outputs/database_products/taxonomy/resolved_pmc.rds")

## List of multi not plankton ----

not_plankton <- taxonomy_pmc %>% 
  
  filter(
    type == "not plankton"
  )

# Find them in the resolved_pre_multi_check
not_plankton_multi <- resolved_pmc %>% 
  
  filter(
    unique_name %in% not_plankton$resolved.taxa.name,
    
    number_matches > 1
  ) 

## Get info for each multi ----
# Loop through all the names in the multi list to get a list of all the hits

# make an empty list
not_plankton_multi_list <- list()

# Loop through the indices of search_string
for (i in 1:length(not_plankton_multi$search_string)) {
  not_plankton_multi_list[[i]] <- inspect(resolved_pmc, taxon_name = not_plankton_multi$search_string[i])
}

# make into a data frame
not_plankton_multi_all <- bind_rows(not_plankton_multi_list)

## Update info ----
# update the taxonomy info for the ones that need it - only two so just do in mutate as easier
# mct - multi_check_1

taxonomy_mc1 <- taxonomy_pmc %>% 
  
  mutate(
    taxa.name = case_when(
      resolved.taxa.name == "Sphaerellopsis" ~ "Vitreochlamys",
      resolved.taxa.name == "Rhaphidiopsis" ~ "Raphidiopsis",
      
      TRUE ~ resolved.taxa.name
    ),
    
    ott.id = case_when(
      taxa.name == "Vitreochlamys" ~ 28980,
      taxa.name == "Raphidiopsis" ~ 836111,
      
      TRUE ~ ott.id
    ),
    
    species = case_when(
      ott.id %in% c("28980", "836111") ~ NA,
      
      TRUE ~ species
    ),
    
    genus = case_when(
      ott.id %in% c("28980", "836111") ~ taxa.name,
      
      TRUE ~ genus
    ),
    
    family = case_when(
      genus == "Vitreochlamys" ~ "Chlamydomonadaceae",
      genus == "Raphidiopsis" ~ "Aphanizomenonaceae",
      
      TRUE ~ family
    ),
    
    order = case_when(
      family == "Chlamydomonadaceae" ~ "Chlamydomonadales",
      family == "Aphanizomenonaceae" ~ "Nostocales",
      
      TRUE ~ order
    ),
    
    class = case_when(
      order == "Chlamydomonadales" ~ "Chlorophyceae",
      order == "Nostocales" ~ "Cyanophyceae",
      
      TRUE ~ class
    ),
    
    phylum = case_when(
      class == "Chlorophyceae" ~ "Chlorophyta",
      class == "Cyanophyceae" ~ "Cyanobacteria",
      
      TRUE ~ phylum
    ),
    
    kingdom = case_when(
      phylum == "Chlorophyta" ~ "Plantae",
      phylum == "Cyanobacteria" ~ "Bacteria",
      
      TRUE ~ kingdom
    ),
    
    domain = case_when(
      kingdom == "Plantae" ~ "Eukaryota",
      kingdom == "Bacteria" ~ "Bacteria",
      
      TRUE ~ domain
    ),
    
    type = case_when(
      ott.id %in% c("28980", "836111") ~ "Phytoplankton",
      
      TRUE ~ type
    )
  ) %>% 
  
  relocate(
    resolved.taxa.name, taxa.name, ott.id, type, species, genus, family, order, class, phylum, kingdom, domain
  ) %>% 
  
  filter(
    type != "not plankton"
  )

## Save ----
saveRDS(taxonomy_mc1, "R/data_outputs/database_products/taxonomy/taxonomy_mc1.rds")





# Phylogeny plot ----
# plot the phylogeny to see if any have been put in the wrong place - easy to see visually

## Import data ----
taxonomy_mc1 <- readRDS("R/data_outputs/database_products/taxonomy/taxonomy_mc1.rds")

## Format data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data <- taxonomy_mc1 %>% 

  select(
    ott.id,
    taxa.name,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 
  
  distinct(ott.id, .keep_all = TRUE)

## Find only ones in the tree ----

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_tax <- is_in_tree(ott_ids = phylo_plot_data$ott.id)

# Save
saveRDS(in_tree_tax, "R/data_outputs/database_products/taxonomy/in_tree_tax.rds")

# View data
in_tree_tax

# See which ones are in and out
sum(in_tree_tax == TRUE) # 4939
sum(in_tree_tax == FALSE) # 822

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree <- phylo_plot_data[in_tree_tax, ]
taxa_in_tree <- taxa_in_tree %>% 
  filter(
    kingdom == "Protozoa"
    #phylum == "Arthropoda"
  )
# Make tree
tree <- tol_induced_subtree(ott_ids = taxa_in_tree$ott.id)

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

## Plot square tree ----
plot(tree, show.tip.label = FALSE)

## Format data with tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

phylo_plot_data_update <- as.data.frame(tree$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree, by = c("stripped.tip.label" = "taxa.name")
  ) %>% 
  
  left_join(
    taxa_in_tree, by = c("stripped.tip.label" = "genus"),
    suffix = c(".tn", ".g")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label,
    ott.id = if_else(!is.na(ott.id.tn), ott.id.tn, ott.id.g),
    species = if_else(!is.na(species.tn), species.tn, species.g),
    genus = if_else(!is.na(genus), genus, stripped.tip.label),
    family = if_else(!is.na(family.tn), family.tn, family.g),
    order = if_else(!is.na(order.tn), order.tn, order.g),
    class = if_else(!is.na(class.tn), class.tn, class.g),
    phylum = if_else(!is.na(phylum.tn), phylum.tn, phylum.g),
    kingdom = if_else(!is.na(kingdom.tn), kingdom.tn, kingdom.g)
  ) %>% 
  
  select(
    tip.label, stripped.tip.label, taxa.name, ott.id, species, genus, family, order, class, phylum, kingdom
  ) %>% 
    
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    class_label = paste0("Class: ", class),
    phylum_label = paste0("Phylum: ", phylum),
    order_label = paste0("Order: ", order),
    family_label = paste0("Family: ", family),
    genus_label = paste0("Genus: ", genus)
  )

## Plotting circular tree ----

circular_plot <- ggtree(tree, branch.length='none', layout='circular') 
circular_plot

## Plotting circular tree with group info ----

circular_plot <- circular_plot %<+% phylo_plot_data_update +
  #geom_tippoint(aes(x = x + 5, color = class_label), size = 3, show.legend = TRUE)
  geom_tippoint(aes(x = x + 5, color = phylum_label), size = 3, show.legend = TRUE)+
  geom_tippoint(aes(x = x + 7, color = kingdom_label), size = 3, show.legend = TRUE) +
  
  scale_color_manual(values =
                       c("Kingdom: Plantae" = "#006400", "Kingdom: Bacteria" = "#9B59B6", "Kingdom: Protozoa" = "#CC5500", "Kingdom: Chromista" = "#00008B", "Kingdom: Animalia" = "#8B0000",
                         "Phylum: Chlorophyta" = "#e5f5e0", "Phylum: Charophyta" = "#a1d99b", "Phylum: Glaucophyta" = "#74c476", "Phylum: Rhodophyta" = "#238b45", "Phylum: Cryptophyta" = "#41ab5d",
                         "Phylum: Cyanobacteria" = "#B57EDC",
                         "Phylum: Euglenozoa" = "#FFFFE0", "Phylum: Amoebozoa" = "#FFF176", "Phylum: Choanozoa" = "#FBC02D",
                         "Phylum: Myzozoa" = "#6495ED", "Phylum: Bacillariophyta"= "#CFE2F3", "Phylum: Ochrophyta" = "#87CEEB", "Phylum: Haptophyta" = "#ADD8E6", "Phylum: Bigyra"= "#003366", "Phylum: Ciliophora (phylum in subkingdom SAR)" = "#1E3A5F", "Phylum: Cercozoa" = "#4682B4","Phylum: Heliozoa" = "#EBF5FA",
                         "Phylum: Cnidaria" = "#FFCCCB", "Phylum: Mollusca" = "#F08080", "Phylum: Arthropoda" = "#FF6347", "Phylum: Rotifera" = "#D32F2F", "Phylum: Gastrotricha" = "#B71C1C"
                       )
  )
    

circular_plot







# Final changes to taxonomy ----
# Edit ones that got flagged up from the phylogeny graph and also just ones that poped up on the fly
# Quite a lot so going to fill it in in an extra spreadsheet and then import in

## Import data ----
manual_multi <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "multi_updates")

## Update names ----
taxonomy <- taxonomy_mc1 %>% 
  
  # Remove weird ones
  filter(
    !(resolved.taxa.name %in% manual_multi$resolved.taxa.name),
    !(ott.id %in% c("972900"))
  ) %>% 
  
  bind_rows(
    ., manual_multi
  ) %>% 
  
  mutate(
    # minor edits
    genus = if_else(
      genus == "Cryptaulax",
      "Cryptaulax (genus in infrakingdom Excavata)",
      genus
    )
  )

# Save
saveRDS(taxonomy, "R/data_outputs/database_products/taxonomy/taxonomy.rds")






# Phylogeny plot 2 ----
# plot the phylogeny to see if any have been put in the wrong place - easy to see visually

## Import data ----
taxonomy <- readRDS("R/data_outputs/database_products/taxonomy/taxonomy.rds")

## Format data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data <- taxonomy %>% 
  
  select(
    ott.id,
    taxa.name,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 
  
  distinct(ott.id, .keep_all = TRUE)

## Find only ones in the tree ----

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_tax_2 <- is_in_tree(ott_ids = phylo_plot_data$ott.id)

# Save
saveRDS(in_tree_tax_2, "R/data_outputs/database_products/taxonomy/in_tree_tax_2.rds")

# View data
in_tree_tax_2

# See which ones are in and out
sum(in_tree_tax_2 == TRUE) # 4939
sum(in_tree_tax_2 == FALSE) # 806

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree <- phylo_plot_data[in_tree_tax_2, ]

# Make tree
tree <- tol_induced_subtree(ott_ids = taxa_in_tree$ott.id)

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

## Plot square tree ----
plot(tree, show.tip.label = FALSE)

## Format data with tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

phylo_plot_data_update <- as.data.frame(tree$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree, by = c("stripped.tip.label" = "taxa.name")
  ) %>% 
  
  left_join(
    taxa_in_tree, by = c("stripped.tip.label" = "genus"),
    suffix = c(".tn", ".g")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label,
    ott.id = if_else(!is.na(ott.id.tn), ott.id.tn, ott.id.g),
    species = if_else(!is.na(species.tn), species.tn, species.g),
    genus = if_else(!is.na(genus), genus, stripped.tip.label),
    family = if_else(!is.na(family.tn), family.tn, family.g),
    order = if_else(!is.na(order.tn), order.tn, order.g),
    class = if_else(!is.na(class.tn), class.tn, class.g),
    phylum = if_else(!is.na(phylum.tn), phylum.tn, phylum.g),
    kingdom = if_else(!is.na(kingdom.tn), kingdom.tn, kingdom.g)
  ) %>% 
  
  select(
    tip.label, stripped.tip.label, taxa.name, ott.id, species, genus, family, order, class, phylum, kingdom
  ) %>% 
  
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    class_label = paste0("Class: ", class),
    phylum_label = paste0("Phylum: ", phylum),
    order_label = paste0("Order: ", order),
    family_label = paste0("Family: ", family),
    genus_label = paste0("Genus: ", genus)
  )

## Plotting circular tree ----

circular_plot <- ggtree(tree, branch.length='none', layout='circular') 
circular_plot

## Plotting circular tree with group info ----

circular_plot <- circular_plot %<+% phylo_plot_data_update +
  geom_tippoint(aes(x = x + 1, color = phylum_label), size = 1, show.legend = TRUE)+
  geom_tippoint(aes(x = x + 2, color = kingdom_label), size = 1, show.legend = TRUE) +
  
  scale_color_manual(values =
                       c("Kingdom: Plantae" = "#006400", "Kingdom: Bacteria" = "#9B59B6", "Kingdom: Protozoa" = "#CC5500", "Kingdom: Chromista" = "#00008B", "Kingdom: Animalia" = "#8B0000",
                         "Phylum: Chlorophyta" = "#e5f5e0", "Phylum: Charophyta" = "#a1d99b", "Phylum: Glaucophyta" = "#74c476", "Phylum: Rhodophyta" = "#238b45", "Phylum: Cryptophyta" = "#41ab5d",
                         "Phylum: Cyanobacteria" = "#B57EDC",
                         "Phylum: Euglenozoa" = "#FFFFE0", "Phylum: Amoebozoa" = "#FFF176", "Phylum: Choanozoa" = "#FBC02D",
                         "Phylum: Myzozoa" = "#6495ED", "Phylum: Bacillariophyta"= "#CFE2F3", "Phylum: Ochrophyta" = "#87CEEB", "Phylum: Haptophyta" = "#ADD8E6", "Phylum: Bigyra"= "#003366", "Phylum: Ciliophora (phylum in subkingdom SAR)" = "#1E3A5F", "Phylum: Cercozoa" = "#4682B4","Phylum: Heliozoa" = "#EBF5FA",
                         "Phylum: Cnidaria" = "#FFCCCB", "Phylum: Mollusca" = "#F08080", "Phylum: Arthropoda" = "#FF6347", "Phylum: Rotifera" = "#D32F2F", "Phylum: Gastrotricha" = "#B71C1C"
                       )
  )

circular_plot

# Save
ggsave("R/data_outputs/exploring/circular_plot.pdf", width = 7, height = 5, limitsize = FALSE)





# Final tax list ----
# All good so can use classification as tax_list

tax_list_raw <- taxonomy %>% 
  distinct(
    ott.id, .keep_all = TRUE
  ) %>% 
  
  # make a rank column 
  mutate(
    rank = case_when(
      !is.na(species) ~ "Species",
      !is.na(genus) ~ "Genus",
      !is.na(family) ~ "Family",
      !is.na(order) ~ "Order",
      !is.na(class) ~ "Class",
      !is.na(phylum) ~ "Phylum",
      !is.na(kingdom) ~ "Kingdom",
      !is.na(domain) ~ "Domain",
      
      TRUE ~ NA
    )
  )

# Save
saveRDS(tax_list_raw, file = "R/data_outputs/database_products/taxonomy/tax_list_raw.rds")

# Add to main data ----
# need to join things inn sequence because there were a couple of steps to get from the raw names to the formatted names

## Linking new to original names ----

### taxonomy to resolved ----

tax_to_resolved <- left_join(
  select(
    resolved_pmc, search_string, unique_name
    ), select(
    taxonomy, ott.id, taxa.name, resolved.taxa.name
    ), by = c("unique_name" = "resolved.taxa.name")
  ) %>% 
  
  rename(
    resolved.pmc.search.string = search_string
  ) %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  select(
    -unique_name
  )

### Resolved to cleaned ----

# 2) resolved_pmc to resolved.manual
pmc_manual <- resolved_manual %>% 
  
  select(
    resolved.taxa.name,
    search_string
  ) %>% 
  
  # need to make lower case to match
  mutate(
    resolved.taxa.name = tolower(resolved.taxa.name)
  ) %>% 
  
  left_join(., tax_to_resolved, by = c("resolved.taxa.name" = "resolved.pmc.search.string")) %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(ott.id)
  ) %>% 
  
  select(
    - resolved.taxa.name
  )

# 3) resolved.manual to cleaned
resolved_to_cleaned <- cleaned %>% 
  
  # need to make lower case to match
  mutate(
    cleaned.taxa.name = tolower(cleaned.taxa.name)
  ) %>% 
  
  left_join(
    ., select(
      pmc_manual, search_string, ott.id
    ), by = c("cleaned.taxa.name" = "search_string")
  ) %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(ott.id)
  )

## Linking to raw data ----

bodysize_taxonomy <-  bodysize_raw %>% 
  
  # Join ott_id onto raw data
  left_join(
    ., resolved_to_cleaned, by = "original.taxa.name"
  ) %>% 
  
  # add in taxonomy data
  left_join(
    ., tax_list_raw, by = "ott.id"
  ) %>% 
  
  # Select and reorder
  select(
    uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
    join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
    join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
    individual.uid, original.taxa.name, taxa.name, ott.id, type, species, genus, family, order, class, phylum, kingdom, domain,
    life.stage, sex, nu, ind.per.nu,
    min.body.size, max.body.size, body.size,
    bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type,
    sample.year, sample.month
  ) %>% 
  
  # Remove any without a taxa.name
  filter(
    !is.na(taxa.name)
  )

# Save
saveRDS(bodysize_taxonomy, file = "R/data_outputs/database_products/bodysize_taxonomy.rds")
