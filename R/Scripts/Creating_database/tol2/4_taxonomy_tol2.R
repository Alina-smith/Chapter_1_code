# Aim of script: Getting taxonomy info for all the species and adding it to the raw data

# Flow of script:
  # 1) Clean names:     Run the names through taxize::gna_verifier to fix spelling mistakes and then manually fix any that weren't picked up
  # 2) Resolved names:  Run the cleaned names through rotl::tnrs_match_names to get most up to date names and then manually fix any that weren't picked up
  # 3) Taxonomy:        Run cleaned names through taxize::classification with tol to get taxonomy and then manuallu fill in any gaps

# Last updated: 18/03/2025
# Data run through veryfier on 18/03/2025
# Data run through tnrs_match_names on 18/03/2025 
# Data ran through classification on 18/03/2025


library(devtools)
install_github("ropensci/bold")
install_github("ropensci/taxize")

# Packages
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)

# Import data ----
bodysize_raw <- readRDS("R/data_outputs/final_products/bodysize_raw.rds")

# Clean names ----

## gna ----
# run through gna_verifier to fix any spelling

# get a list of all distinct names 
names_list <- select(bodysize_raw, original.taxa.name) %>% 
  
  distinct(original.taxa.name) %>% 
  
  # Convert to a string of names
  pull(original.taxa.name)

# View names
glimpse(names_list)

# run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
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
saveRDS(cleaned_gna, file = "R/data_outputs/taxonomy/tol2/cleaned_gna.rds")

## Manual ----

# 1) Find all the taxa.names from clean_gna to manually resolve based on the criteria in the comments
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
write_csv(to_clean_manually, "R/data_outputs/taxonomy/tol2/to_clean_manually.csv")

# 2) Update cleaned.taxa.name with the manually cleaned names

# Import the manually cleaned names
manually_cleaned <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "cleaned_tol")

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

# Save
saveRDS(cleaned, file = "R/data_outputs/taxonomy/tol2/cleaned.rds")

# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Resolve names ----
## TOL ----

# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)
resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name) %>% 
  
  # rename columns to make easier
  rename(
    cleaned.taxa.name = search_string,
    resolved.taxa.name = unique_name
  )

# Save
saveRDS(resolved_tol, file = "R/data_outputs/taxonomy/tol2/resolved_tol.rds")

## Manual ----
# 1) Select all the ones that weren't picked up by tol and manually resolve their names

to_resolve_manually <- resolved_tol %>% 
  
  filter(
    is.na(resolved.taxa.name)
  )

# Save
write_csv(to_resolve_manually, "R/data_outputs/taxonomy/tol2/to_resolve_manually.csv")

# 2) Add in the manually resolved names to the full name list

# Import the manually resolved names
manually_resolved_subset <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "resolve_tol")

# Add to main names list
resolved_manual <- left_join(resolved_tol, manually_resolved_subset, by = "cleaned.taxa.name") %>% 
  
  # When a manually resolved name is present use this instead use the original resolved name
  mutate(
    resolved.taxa.name.manual = if_else(
      !is.na(resolved.taxa.name.manual),
      resolved.taxa.name.manual,
      resolved.taxa.name
    )
  ) %>%
  
  # Remove any that couldn't be resolved
  filter(
    !is.na(resolved.taxa.name.manual)
  ) %>% 
  
  # Select relevant columns
  select(
    cleaned.taxa.name, resolved.taxa.name.manual
  )

# 3) Rerun through tnrs_match_names to resolve all again

resolved <- tnrs_match_names(manually_resolved$resolved.taxa.name) %>%
  
  # rename columns to make easier
  rename(
    resolved.taxa.name.manual = search_string,
    resolved.taxa.name = unique_name
  )

# Save
saveRDS(resolved, file = "R/data_outputs/taxonomy/tol2/resolved.rds")


# Taxonomy: ----

## Initial run through classification ----
classification_raw <- resolved %>% 
  
  # Get disinct names
  distinct(resolved.taxa.name) %>% 
  
  # Use rowwise so it looks at each row one at a time
  rowwise() %>%
  
  mutate(
    
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for that taxa
      classification(
        resolved.taxa.name, db = "tol", return_id = FALSE, rows = 1 # rows = 1 so that is only takes the first one and doesn't require you select options for each one
      )[[1]] # select the first element of the list
    ),
    
    # Change ones that didn't get classified from just NA to a dataframe of NAs
    tax = ifelse(
      is.data.frame(tax),
      list(tax),
      list(data.frame(name = NA, rank = "no rank"))
    ),
    
    # Pivot tax so that it makes columns for each rank
    pivot_wider(tax,
                names_from = rank,
                values_from = name)
  ) %>% 
  
  # remove unnecessary columns
  select(
    - tax,
    - `no rank`
  ) %>% 
  
  # Separate columns that have multiple names for a rank into multiple columns
  unnest_wider(
    col = everything(), names_sep = "."
  ) %>% 
  
  rename(
    resolved.taxa.name = resolved.taxa.name.1
  ) %>% 
  
  ungroup() # ungroup to remove rowwise 

# Save
saveRDS(classification_raw, file = "R/data_outputs/taxonomy/tol2/classification_raw.rds")

## Format classification ----

### Initial edits ----
# Do any edits that can easily be done within mutate

classification_formatted <- classification_raw %>% 
  
  mutate(
    
    # Species
    species = case_when(
      # When the species was put into the form/variety column rather than species column
      varietas.1 == "Daphnia sinensis" ~ "Daphnia sinensis",
      forma.1 == "Prymnesium parvum f. patelliferum" ~ "Prymnesium parvum",
      
      # Where the species name was missing but resolved.taxa.name was a species
      resolved.taxa.name %in% c("Chrysastrella furcata", "Cymbopleura cuspidata", "Cystodinium cornifax", "Mytilina mucronata", "Mytilina ventralis", "Parkeria sphaerica", "Praetriceratium inconspicuum", "Pseudopodosira kosugii",
                                "Aulacoseira ambigua", "Navicula menisculus", "Brachysira follis", "Brachysira elliptica", "Cymbella proxima", "Cymbella diversistigmata", "Conticribra weissflogii", "Rossithidium duthiei",
                                "Hippodonta lueneburgensis", "Delphineis surirella", "Adlafia parabryophila", "Hippodonta arkonensis", "Lenticulina muensteri", "Daphnia sinensis", "Geissleria acceptata", "Stephanodiscus carconensis",
                                "Dinobryon cylindricum") ~ resolved.taxa.name,
      
      TRUE ~ species.1
    ),
    
    # Genus
    genus = case_when(
      
      # Any minor edits to names
      resolved.taxa.name == "Dinobryon (in Ochromonas sup.)" ~ "Dinobryon",
      resolved.taxa.name == "Palaeacmea" ~ "Palaeacmaea",
      resolved.taxa.name == "Rhizosolenia (in Bacillariophytina)" ~ "Rhizosolenia",
      
      # Where the genus name was missing but the resolved.taxa.name was a genus
      resolved.taxa.name %in% c("Cryptaulax", "Cryptoglena", "Cystodinium", "Rhaphidiopsis", "Tetralithus", "Lithoperidinium", "Petersophlebia", "Proboscidoplocia", "Chrysastrella", "Gleocapsa") ~ resolved.taxa.name,
      
      # Use the genus.2 column when it has a value in as these were correct
      !is.na(genus.2) ~ genus.2,
      
      # Otherwise just select genus.1
      TRUE ~ genus.1
    ),
    
    # When species is filled but genus is missing select just the first name in the species name
    genus = if_else(
      is.na(genus) & !is.na(species),
      stri_extract_first_regex(species, "\\w+"),
      genus
    ),
    
    # Family
    # Too many missing family rows to do in mutate so will do this in excel and read into R
    family = case_when(
      
      # Use the family.2 column when it has a value in as these were correct
      !is.na(family.2) ~ family.2,
      
      # Otherwise use family.1
      TRUE ~ family.1
    ),
    
    # Order
    # Too many missing order rows to do in mutate so will do this in excel and read into R
    
    order = case_when(
      resolved.taxa.name == "Acanthosphaera (genus in subkingdom SAR)" ~ "Chlorellales",
      
      TRUE ~ order.1
    ),
    
    # Class
    # Will do class manually after order is done
    class = case_when(
      
      # Minor edits
      class.1 == "Haptophyta" ~ "Coccolithophyceae",
      class.1 == "Glaucophyta" ~ "Glaucophyceae",
    
      TRUE ~ class.1
    ),
    
    # Phylum
    # do phylum now because I need to get the types to work out which ones aren't zoo or phyto to remove before later steps
    
    phylum = case_when(
      
      phylum.1 == "Cryptophyceae" ~ "Cryptophyta",
      phylum.1 == "Euglenida" ~ "Euglenozoa",
      
      resolved.taxa.name == "Acanthosphaera (genus in subkingdom SAR)" ~ "Chlorophyta",
      resolved.taxa.name == "Dinoflagellata" ~ "Myzozoa",
      
      genus == "Palaeacmaea" ~ "Mollusca",
      genus == "Amoeba" ~ "Amoebozoa",
      genus == "Colpidium" ~ "Ciliophora",
      genus %in% c("Crumenula", "Euglena") ~ "Euglenozoa",
      genus %in% c("Oscillatoria", "Anacystis", "Schizothrix") ~ "Cyanobacteria",
      genus %in% c("Karlodinium", "Karenia") ~ "Myzozoa",
      
      family == "Collodictyonidae" ~ "Apusozoa",
      
      order == "Bicosoecida" ~ "Bigyra",
      order == "Choanoflagellida" ~ "Sarcomastigophora",
      order %in% c("Eustigmatales", "Synurales", "Chrysomeridales") ~ "Ochrophyta",
      order == "Kinetoplastea" ~ "Euglenozoa",
      order == "Noctilucales" ~ "Myzozoa",
      order == "Centrohelida" ~ "Chlorophyta",
      
      class %in% c("Chrysophyceae", "Dictyochophyceae", "Raphidophyceae", "Xanthophyceae", "Phaeothamniophyceae", "Actinophryidae") ~ "Ochrophyta",
      class == "Coccolithophyceae" ~ "Haptophyta",
      class == "Glaucophyceae" ~ "Glaucophyta",
      class == "Dinophyceae" ~ "Myzozoa",
      class %in% c("Zygnemophyceae", "Coleochaetophyceae", "Klebsormidiophyceae") ~ "Charophyta",
      
      !is.na(phylum.2) ~ phylum.2,
      
      TRUE ~ phylum.1
    ),
    
    # Kingdom 
    
    kingdom = case_when(
      
      phylum %in% c("Cyanobacteria") ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Charophyta", "Rhodophyta", "Glaucophyta") ~ "Plantae",
      phylum %in% c("Ochrophyta", "Bacillariophyta", "Haptophyta", "Cryptophyta", "Bigyra", "Myzozoa", "Ciliophora", "Cercozoa", "Foraminifera", "Bacillariophyta") ~ "Chromista",
      phylum %in% c("Sarcomastigophora", "Euglenozoa", "Amoebozoa", "Apusozoa") ~ "Protozoa",
      phylum %in% c("Arthropoda", "Mollusca", "Rotifera", "Gastrotricha", "Cnidaria", "Bryozoa", "Chordata") ~ "Animalia",
      
      TRUE ~ NA
    ),
    
    # Type
    # Assign either phytoplankton or zooplankton and when it is neither then NA
    # Do have some insect larvae in there but not much so discarding it
    
    type = case_when(
      
      kingdom %in% c("Bacteria", "Plantae") ~ "Phytoplankton",
      kingdom == "Animalia" ~ "Zooplankton",
      
      phylum %in% c("Ochrophyta", "Haptophyta", "Sarcomastigophora", "Bigyra", "Myzozoa", "Euglenozoa", "Bacillariophyta", "Cryptophyta") ~ "Phytoplankton",
      phylum %in% c("Cercozoa", "Amoebozoa", "Foraminifera", "Apusozoa", "Ciliophora", "Sarcomastigophora") ~ "Zooplankton",
      
      TRUE ~ NA
    ),
    
    # Make a tax.uid column
    tax.uid = paste0("tax-", row_number())
  ) %>% 
  
  # Remove non plankton
  filter(
    !(resolved.taxa.name == "Marssoniella (genus in kingdom Archaeplastida)"),
    !is.na(type)
  ) %>% 
  
  # want to make two taxa name columns one with the extra info in brackets and one without
  rename(
    taxa.name.full = resolved.taxa.name
  ) %>% 
  
  mutate(
    taxa.name = case_when(
      stri_detect_regex(taxa.name.full, "\\(") ~ stri_extract_first_regex(taxa.name.full, "\\w+ \\w+|\\w+"),
      
      TRUE ~ taxa.name.full
    )
  ) %>% 
  
  # Select columns
  select(
    tax.uid, taxa.name.full, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  )

### Family ----
# want to manually input missing ranks for family as will take too long to do in a case_when

# Make a list of missing family with thier genus
missing_family <- classification_formatted %>% 
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
write_csv(missing_family, "R/data_outputs/taxonomy/tol2/missing_family.csv")

# Import the manually filled family
manually_family <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "family_tol")

# Update family
classification_family <- classification_formatted %>% 
  
  left_join(manually_family, by = "genus", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    family = if_else(
      !is.na(family.manual),
      family.manual,
      family.tol
    )
  ) %>% 
  
  select(
    - family.manual,
    - family.tol
  )

### Order ----
# Want to manually input missing ranks for order as will take too long to do in a case_when

# Make a list of missing order with their family
missing_order <- classification_family %>% 
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
write_csv(missing_order, "R/data_outputs/taxonomy/tol2/missing_order.csv")

# Import the manually filled order
manually_order <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "order_tol")

# Update order
classification_order <- classification_family %>% 
  
  left_join(manually_order, by = "family", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    order = if_else(
      !is.na(order.manual),
      order.manual,
      order.tol
    ),
    
    # do the ones with a missing family from genus with case_when
    order = case_when(
      genus == "Jaaginema" ~ "Synechococcales",
      genus == "Pseudofallacia" ~ "Naviculales",
      
      TRUE ~ order
    )
  ) %>% 
  
  select(
    - order.manual,
    - order.tol
  )

### Final edits ----
# Final edits can just be done with case_when

classification <- classification_order %>% 
  
  mutate(
    
    # Class
    # Fill in missing class rank
    
    class = case_when(
      family == "Radialiplicataceae" ~ "Coscinodiscophyceae",
      
      order == "Choanoflagellida" ~ "Zoomastigophora",
      order == "Eustigmatales" ~ "Eustigmatophyceae",
      order == "Pyramimonadales" ~ "Pyramimonadophyceae",
      order == "Cryptomonadales" ~ "Cryptophyceae",
      order %in% c("Fragilariales", "Tabellariales", "Orthoseirales", "Coscinodiscales") ~ "Bacillariophyceae",
      order == "Synurales" ~ "Chrysophyceae",
      order == "Bangiales" ~ "Bangiophyceae",
      order == "Centrohelida" ~ "Centrohelea",
      order == "Chrysomeridales" ~ "Chrysomeridophyceae",
      order %in% c("Chroococcales", "Oscillatoriales", "Nostocales", "Pseudanabaenales", "Pleurocapsales", "Synechococcales", "Nodosilineales", "Spirulinales", "Leptolyngbyales") ~ "Cyanobacteria",
      order %in% c("Noctilucales", "Gymnodiniales") ~ "Dinophyceae",
      order == "Bicosoecida" ~ "Bicosoecophyceae",
      order == "Heteronematales" ~ "Euglenida",
      order == "Vaginulinida" ~ "Nodosariata",
      order == "Euglenales" ~ "Euglenophyceae",
      order == "Natomonadida" ~ "Peranemea", 
      order == "Euamoebida" ~ "Tubulinea",
      order == "Mischococcales" ~ "Xanthophyceae",
      
      TRUE ~ class
    )
  ) %>% 
  
  # Reorder
  relocate(
    tax.uid, taxa.name.full, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  )

# Save
saveRDS(classification, file = "R/data_outputs/taxonomy/tol2/classification.rds")

# Add to main data ----
# need to join things inn sequence because there were a couple of steps to get from the raw names to the formatted names

## Linking new to original names ----
### Classification to resolved ----
class_to_resolve <- left_join(
  classification,
  select(
    resolved, resolved.taxa.name, resolved.taxa.name.manual
    ), by = c("taxa.name.full" = "resolved.taxa.name")
  ) 

### Resolved to cleaned ----

# First link above to the manually resolved list
resolved_to_clean_1 <- resolved_manual %>% 
  
  mutate(
    resolved.taxa.name.manual = tolower(resolved.taxa.name.manual)
  ) %>% 
  
  left_join(
    ., class_to_resolve, by = "resolved.taxa.name.manual"
  ) %>% 
  
  select(
    - resolved.taxa.name.manual
  )

# Next link above to the cleaned list
resolved_to_clean_2 <- cleaned %>% 
  
  mutate(
    cleaned.taxa.name = tolower(cleaned.taxa.name)
  ) %>% 
  
  left_join(
    ., resolved_to_clean_1, by = "cleaned.taxa.name"
  ) %>% 
  
  select(
    -cleaned.taxa.name
  )

## Linking to raw data ----

bodysize_taxonomy <-  bodysize_raw %>% 
  
  # Join taxonomy info onto raw data
  left_join(
    ., resolved_to_clean_2, by = "original.taxa.name"
  ) %>% 
  
  # Select and reorder
  select(
    uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
    join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
    join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
    individual.uid, original.taxa.name, taxa.name.full, taxa.name, tax.uid, type, species, genus, family, order, class, phylum, kingdom,
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
saveRDS(bodysize_taxonomy, file = "R/data_outputs/full_database/tol/bodysize_taxonomy_tol2.rds")


# Initial taxonomy list ----

tax_list_raw <- bodysize_taxonomy %>% 
  distinct(
    tax.uid, .keep_all = TRUE
  ) %>% 
  select(
    taxa.name.full, taxa.name, tax.uid, type, species, genus, family, order, class, phylum, kingdom
  )

# Save
saveRDS(tax_list_raw, file = "R/data_outputs/taxonomy/tol2/tax_list_raw.rds")

