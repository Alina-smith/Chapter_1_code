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

# Import data ----
bodysize_raw <- readRDS("R/data_outputs/database_products/final_products/bodysize_raw.rds")

# Clean names ----

## gna_verifier ----
# run through gna_verifier to fix any spelling

# Get a list of all distinct names 
names_list <- select(bodysize_raw, original.taxa.name) %>% 
  
  distinct(original.taxa.name) %>% 
  
  # Convert to a string of names
  pull(original.taxa.name)

# View names
glimpse(names_list)

# Run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
cleaned_gna <- do.call(rbind, lapply(names_list, function(name) {
  tryCatch(
    {
      # Process each name and return the result
      gna_verifier(names_list) %>%
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

# Save
saveRDS(cleaned_gna, file = "R/data_outputs/database_products/taxonomy/cleaned_gna.rds")





## Manual ----

# 1) Import cleaned_gna data
cleaned_gna <- readRDS("R/data_outputs/database_products/taxonomy/cleaned_gna.rds")

# 2) Find taxa.names from clean_gna to manually resolved

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

# 3) Update cleaned.taxa.name with the manually cleaned names

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

# Save
saveRDS(cleaned, file = "R/data_outputs/database_products/taxonomy/cleaned.rds")





# Resolve names ----

## tnrs_match_names ----

# 1) Import cleaned data
cleaned <- readRDS("R/data_outputs/database_products/taxonomy/cleaned.rds")

# 2) Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)
resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name)

# 3) Save
saveRDS(resolved_tol, file = "R/data_outputs/database_products/taxonomy/resolved_tol.rds")



## Manual ----

# 1) Find ones that need to be manually resolved

# Import resolved_tol
resolved_tol <- readRDS("R/data_outputs/database_products/taxonomy/resolved_tol.rds")

# Select ones that weren't picked up by tol
to_resolve_manually <- resolved_tol %>% 
  
  filter(
    is.na(unique_name)
  )

# Save
write_csv(to_resolve_manually, "R/data_outputs/database_products/taxonomy/to_resolve_manually.csv") # Will manually resolve in excel sheet and then import back in

# 2) Add in the manually resolved names to the full name list

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

# Save
saveRDS(resolved_manual, "R/data_outputs/database_products/taxonomy/resolved_manual.rds")



## tnrs_match_names rerun ----

# import data
resolved_manual <- readRDS("R/data_outputs/database_products/taxonomy/resolved_manual.rds")

# Rerun resolved manual through tnrs_match_name to get missing otts
resolved_pre_multi_check <- tnrs_match_names(resolved_manual$resolved.taxa.name)

# check for any that weren't picked up - if false then fine
unique(is.na(resolved_pre_multi_check$ott_id))

# Save
saveRDS(resolved_pre_multi_check, "R/data_outputs/database_products/taxonomy/resolved_pre_multi_check.rds")





# Taxonomy: ----

# Import data
resolved_pre_multi_check <- readRDS("R/data_outputs/database_products/taxonomy/resolved_pre_multi_check.rds")

## tax_lineage ----
taxonomy_raw_pmc <- resolved_pre_multi_check %>% 
  
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

# Save
saveRDS(taxonomy_raw_pmc, file = "R/data_outputs/database_products/taxonomy/taxonomy_raw_pmc.rds")



## Format taxonomy ----

# import data
taxonomy_raw_pmc <- readRDS("R/data_outputs/database_products/taxonomy/taxonomy_raw_pmc.rds")
not_genus <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "not_genus")

### Initial edits ----
# Do any edits that can easily be done within mutate

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

### Fill in remaining ranks ----

#### Family ----
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







    
    # Phylum
    phylum = case_when(
      class.1 %in% c("Glaucophyta", "Haptophyta") ~ class.1, # phylum was put into class column
      class.2 == "Haptophyta" ~ class.2, # phylum was put into class column
      
      class %in% c("Dictyochophyceae", "Chrysophyceae", "Xanthophyceae", "Phaeothamniophyceae", "Raphidophyceae", "Actinophryidae") ~ "Ochrophyta",
      class == "Dinophyceae" ~ "Myzozoa",
      class %in% c("Kinetoplastea", "Euglenophyceae", "Euglenoidea") ~ "Euglenozoa",
      class == "Cryptophyceae" ~ "Cryptophyta",
      
      order == "Craspedida" ~ "Choanozoa",
      order %in% c("Eustigmatales", "Synurales", "Chrysomeridales") ~ "Ochrophyta",
      order == "Centrohelida" ~ "Heliozoa",
      order == "Noctilucales" ~ "Myzozoa",
      order == "Bicosoecales" ~ "Bigyra",
      order == "Euglenida" ~ "Euglenozoa",
      
      genus == "Cryptaulax (genus in infrakingdom Excavata)" ~ "Cryptophyta",
      
      tol.taxa.name %in% c("Cyanobacteria", "Ciliophora (phylum in subkingdom SAR)", "Rotifera", "Chlorophyta", "Haptophyta")  ~ tol.taxa.name,
      tol.taxa.name %in% c("Dinoflagellata", "Karenia mikimotoi", "Karlodinium veneficum") ~ "Myzozoa",
      tol.taxa.name == "Amoeba (genus in Opisthokonta)" ~ "Amoebozoa",
      tol.taxa.name %in% c("Anacystis incerta", "Oscillatoria amphibia", "Schizothrix lacustris (species in domain Eukaryota)", "Schizothrix pulvinata (species in domain Eukaryota)") ~ "Cyanobacteria",
      tol.taxa.name == "Colpidium campylum" ~ "Ciliophora (phylum in subkingdom SAR)",
      tol.taxa.name == "Crumenula texta" ~ "Euglenozoa",
      tol.taxa.name %in% c("Acanthosphaera (genus in subkingdom SAR)", "Diphylleia rotans") ~ "Chlorophyta",
      
      !is.na(phylum.2) ~ phylum.2,
      
      TRUE ~ phylum.1
    ),
    
    # Kingdom
    kingdom = case_when(
      phylum %in% c("Cyanobacteria",  "Proteobacteria (phylum silva:A16379/#2)", "Chloroflexi", "Actinobacteria") ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Rhodophyta", "Glaucophyta", "Cryptophyta", "Streptophyta") ~ "Plantae",
      
      phylum %in% c("Ochrophyta", "Bacillariophyta", "Haptophyta", "Bigyra", "Myzozoa", "Ciliophora (phylum in subkingdom SAR)", "Cercozoa", "Foraminifera") ~ "Chromista",
      phylum %in% c("Euglenozoa", "Amoebozoa", "Choanozoa") ~ "Protozoa",
      
      phylum %in% c("Arthropoda", "Mollusca", "Rotifera", "Gastrotricha", "Cnidaria", "Bryozoa", "Chordata", "Annelida", "Platyhelminthes", "Nematoda") ~ "Animalia",
      phylum %in% c("Ascomycota", "Basidiomycota") ~ "Fungi",
      
      TRUE ~ NA
    ),
    
    # Domain
    domain = domain.1,
    
    # type
    type = case_when(
      
      # One that aren't plankton - need to look over these incase any have multuples and need a different one selecting
      class %in% c("Amphibia", "Anthozoa", "Arachnida", "Malacostraca", "Insecta") ~ "not plankton",
      phylum %in% c("Proteobacteria (phylum silva:A16379/#2)", "Chloroflexi", "Actinobacteria", "Annelida", "Platyhelminthes", "Nematoda") ~ "not plankton",
      kingdom == "Fungi" ~ "not plankton",
      
      # Ones that are plankton
      phylum == "Cyanobacteria" ~ "Phytoplankton",
      phylum %in% c("Ochrophyta", "Haptophyta", "Bigyra", "Myzozoa", "Euglenozoa", "Bacillariophyta", "Cryptophyta", "Choanozoa") ~ "Phytoplankton",
      kingdom == "Plantae" ~ "Phytoplankton",
      
      kingdom == "Animalia" ~ "Zooplankton",
      phylum %in% c("Cercozoa", "Amoebozoa", "Foraminifera", "Ciliophora (phylum in subkingdom SAR)") ~ "Zooplankton",
      
      TRUE ~ NA
    )
    
  ) %>% 
  
  select(
    tol.taxa.name, ott.id, type, species, genus, family, order, class, phylum, kingdom, domain
  ) %>% 
  
  filter(
    !is.na(type)
  )

## Checking not plankton ones ----
# Check the ones that were sorted into not plankton to see if they had multiple hits in the trns_match_names and if so use those

not_plankton <- taxonomy_formatted_pmc %>% 
  
  filter(
    type == "not plankton"
  )

# find them in the resolved_pre_multi_check
not_plankton_multi <- resolved_pre_multi_check %>% 
  
  filter(
    unique_name %in% not_plankton$tol.taxa.name,
    
    number_matches > 1
  ) 




x <- tnrs_match_names("Desmodesmus armatus")

tax_lineage(taxonomy_taxon_info(x$ott_id[1], include_lineage = TRUE))




















