# Adding taxonomy data to the species list 
# data run through veryfier on 13/1/2025
# data ran through taxize on 20/1/2025

# Aim of script
# 1) Clean names - Run the names through gna_verifier to fix spelling mistakes and then manually fix any that weren't picked up
# 2) Resolved names - Run the cleaned names through tnrs_match_names to get most up to date names and then manually fix any that weren't picked up

# Resolve names - Run the names through gna_veryfier to fix spellings and get most up to date synonyms and manually resolve any that weren't picked up
# Taxonomy
# 1) Run resolved names through classification with tol, then select all that weren't recognised by tol and update any synonyms and then rerun
# 2) Run the names through classification with gbif 
# 3) Fill in any gaps in the tol data with the gbif data

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
distinct_names <- select(bodysize_raw, original.taxa.name) %>% 
  
  distinct(original.taxa.name) %>% 
  
  # Convert to a string of names
  pull(original.taxa.name)

# View names
glimpse(names_list)

# run string through the resolver and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
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
})
) %>% 
  rename(
    original.taxa.name = submittedName,
    cleaned.taxa.name.gna = matchedCanonicalFull,
    cleaned.source.gna = dataSourceTitleShort
  )

# Save
saveRDS(cleaned_gna, file = "R/data_outputs/taxonomy/tol2/cleaned_gna.rds")

## Manual ----
# How manual resolving was carried out:
# When the species name could be found then use that
# When the species name couldn't be found on a database then keep the original.taxa.name in case it is a newly discovered species not in the databases yet
# When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
# When two species are stated then the closet common higher group is used

# 1) Find all the taxa.names from resolved_gna to manually resolve based on the criteria in the comments
to_clean_manually <- cleaned_gna %>% 
  mutate(
    manual = case_when(
      # Ones that weren't picked up by the resolver at all - gave NA for the resolved.taxa.name
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

# 2) Update resolved.taxa.names with the manually resolved names

# Import the manually cleaned names
manually_cleaned <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "cleaned_tol")

# Replace cleaned.taxa.name with manually cleaned names when ones is present
cleaned <- cleaned_gna %>% 
  
  # left join all the manually resolved ones from manual_resolve spreadsheet
  left_join(
    manually_cleaned,
    by = "original.taxa.name"
  ) %>% 
  
  # when a name has been cleaned manually (resolved source = "manaully") then select that name else keep the current one
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
  
  filter(
    !(is.na(cleaned.taxa.name))
  )

# Save
saveRDS(cleaned, file = "R/data_outputs/taxonomy/tol2/cleaned.rds")

# remove form and variety as majority don't have this and makes the taxonomy steps more complex
#cleaned <- cleaned_manual %>% 
  
#  mutate(
#    cleaned.taxa.name = stri_replace_all_regex(cleaned.taxa.name.all, "cf\\.|f\\.|var\\.", " "),
#    cleaned.taxa.name = stri_replace_all_regex(cleaned.taxa.name.all, "  ", "")
#  ) %>% 
#  
#  separate(cleaned.taxa.name, into = c("genus", "species", "c", "d", "e"), sep = " ") %>% 
#  
#  mutate(
#    cleaned.taxa.name.new = stri_c(genus, species, sep = " "),
#    cleaned.taxa.name.new = if_else(
#      is.na(cleaned.taxa.name.new),
#      genus,
#      cleaned.taxa.name.new
#    )
#  ) %>% 
#  
#  select(
#    original.taxa.name, cleaned.taxa.name.new, cleaned.taxa.name.all
#  ) %>% 
#  
#  rename(
#    cleaned.taxa.name = cleaned.taxa.name.new
#  )

# Save
#saveRDS(cleaned, file = "R/data_outputs/taxonomy/tol2/cleaned.rds")

# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Resolve names ----
## TOL ----
# run the cleaned names through the resolver to get updated versions of names

# run through tnrs_match_names to get the most up to date synonyms
resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name)

# Save
saveRDS(resolved_tol, file = "R/data_outputs/taxonomy/tol2/resolved_tol.rds")

## Manual ----
# Select all the ones that weren't picked up by tol and manually resolve their names

to_resolve_manually <- resolved_tol %>% 
  
  filter(
    is.na(unique_name)
  )

## need to yupdate the manually resolved

# Save
write_csv(to_resolve_manually, "R/data_outputs/taxonomy/tol2/to_resolve_manually.csv")

# Add in the manually resolved names to the full name list

x <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "resolve_tol")


# Import the manually resolved names
manually_resolved_subset <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "resolve_tol_new")

# add to main data
manually_resolved <- left_join(resolved_tol, manually_resolved_subset, by = "search_string") %>% 
  
  mutate(
    resolved.taxa.name = if_else(
      !is.na(new_name),
      new_name,
      unique_name
    )
  ) %>%
  
  filter(
    !is.na(resolved.taxa.name)
  ) %>% 
  
  select(
    resolved.taxa.name, search_string, unique_name
  )

# rerun through tnrs_match_names to resolve all again

resolved <- tnrs_match_names(manually_resolved$resolved.taxa.name)

# Save
saveRDS(resolved, file = "R/data_outputs/taxonomy/tol2/resolved.rds")


# Taxonomy ----

## Initial run through classification ----
classification_raw <- resolved %>% 
  
  distinct(unique_name) %>% 
  
  rowwise() %>% # use rowwise so it looks at each row at a time
  
  mutate(
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for the species
      classification(
        unique_name, db = "tol", return_id = FALSE, rows = 1 # rows = 1 so that is only takes the first one and doesn't require you select options for each one
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
    resolved.taxa.name = unique_name.1
  ) %>% 
  
  ungroup() # ungroup to remove rowwise 

# Save
saveRDS(classification_raw, file = "R/data_outputs/taxonomy/tol2/classification_raw.rds")

## Format classification ----

x <- classification_formatted %>% 
  
  distinct(
    phylum
  )

classification_formatted <- classification_raw %>% 
  
  mutate(
    species = case_when(
      varietas.1 == "Daphnia sinensis" ~ "Daphnia sinensis",
      forma.1 == "Prymnesium parvum f. patelliferum" ~ "Prymnesium parvum",
      
      resolved.taxa.name %in% c("Chrysastrella furcata", "Cymbopleura cuspidata", "Cystodinium cornifax", "Mytilina mucronata", "Mytilina ventralis", "Parkeria sphaerica", "Praetriceratium inconspicuum", "Pseudopodosira kosugii",
                                "Aulacoseira ambigua", "Navicula menisculus", "Brachysira follis", "Brachysira elliptica", "Cymbella proxima", "Cymbella diversistigmata", "Conticribra weissflogii", "Rossithidium duthiei",
                                "Hippodonta lueneburgensis", "Delphineis surirella", "Adlafia parabryophila", "Hippodonta arkonensis", "Lenticulina muensteri", "Daphnia sinensis", "Geissleria acceptata", "Stephanodiscus carconensis",
                                "Dinobryon cylindricum") ~ resolved.taxa.name,
      
      TRUE ~ species.1
    ),
    
    genus = case_when(
      resolved.taxa.name == "Dinobryon (in Ochromonas sup.)" ~ "Dinobryon",
      resolved.taxa.name == "Palaeacmea" ~ "Palaeacmaea",
      resolved.taxa.name == "Rhizosolenia (in Bacillariophytina)" ~ "Rhizosolenia",
      resolved.taxa.name %in% c("Cryptaulax", "Cryptoglena", "Cystodinium", "Rhaphidiopsis", "Tetralithus", "Lithoperidinium", "Petersophlebia", "Proboscidoplocia", "Chrysastrella", "Gleocapsa") ~ resolved.taxa.name,
      
      !is.na(genus.2) ~ genus.2,
      
      TRUE ~ genus.1
    ),
    
    genus = if_else(
      is.na(genus) & !is.na(species),
      stri_extract_first_regex(species, "\\w+"),
      genus
    ),
    
    family = case_when(
      genus == "Ammatoidea" ~ "Microcoleaceae",
      !is.na(family.2) ~ family.2,
      
      TRUE ~ family.1
    ),
    
    order = case_when(
      resolved.taxa.name == "Acanthosphaera (genus in subkingdom SAR)" ~ "Chlorellales",
      
      TRUE ~ order.1
    ),
    
    class = case_when(
      class.1 == "Haptophyta" ~ "Coccolithophyceae",
      class.1 == "Glaucophyta" ~ "Glaucophyceae",
      
      TRUE ~ class.1
    ),
    
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
    
    kingdom = case_when(
      
      phylum %in% c("Cyanobacteria") ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Charophyta", "Rhodophyta", "Glaucophyta") ~ "Plantae",
      phylum %in% c("Ochrophyta", "Bacillariophyta", "Haptophyta", "Cryptophyta", "Bigyra", "Myzozoa", "Ciliophora", "Cercozoa", "Foraminifera", "Bacillariophyta") ~ "Chromista",
      phylum %in% c("Sarcomastigophora", "Euglenozoa", "Amoebozoa", "Apusozoa") ~ "Protozoa",
      phylum %in% c("Arthropoda", "Mollusca", "Rotifera", "Gastrotricha", "Cnidaria", "Bryozoa", "Chordata") ~ "Animalia",
      
      TRUE ~ NA
    ),
    
    type = case_when(
      kingdom %in% c("Bacteria", "Plantae") ~ "Phytoplankton",
      kingdom == "Animalia" ~ "Zooplankton",
      
      phylum %in% c("Ochrophyta", "Haptophyta", "Sarcomastigophora", "Bigyra", "Myzozoa", "Euglenozoa", "Ciliophora", "Bacillariophyta", "Cryptophyta") ~ "Phytoplankton",
      phylum %in% c("Cercozoa", "Amoebozoa", "Foraminifera", "Apusozoa") ~ "Zooplankton",
      
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    !(resolved.taxa.name == "Marssoniella (genus in kingdom Archaeplastida)"),
    !is.na(type)
  ) %>% 
  
  select(
    resolved.taxa.name, type, species, genus, phylum, kingdom
  )

# Save
saveRDS(classification_formatted, file = "R/data_outputs/taxonomy/tol2/classification_formatted.rds")

# Add to main data ----

class_to_resolve <- left_join(
  classification_formatted,select(
    resolved, search_string, unique_name
    ), by = c("resolved.taxa.name" = "unique_name")
  ) %>% 
  
  rename(
    search.string.resolved = search_string
  )

resolve_to_clean_1 <- manually_resolved %>% 
  
  mutate(
    resolved.taxa.name = tolower(resolved.taxa.name)
  ) %>% 
  
  rename(
    search.string.manually.resolved = search_string,
    resolved.taxa.name.manually.resolved = resolved.taxa.name
  ) %>% 
  
  select(
    - unique_name
  ) %>% 
  
  left_join(
    ., class_to_resolve, by = c("resolved.taxa.name.manually.resolved" = "search.string.resolved")
  ) %>% 
  
  select(
    - resolved.taxa.name.manually.resolved
  )

resolved_to_clean_2 <- cleaned %>% 
  
  mutate(
    cleaned.taxa.name.lower = tolower(cleaned.taxa.name)
  ) %>% 
  
  select(
    original.taxa.name,
    cleaned.taxa.name.lower
  ) %>% 
  
  left_join(
    ., resolve_to_clean_1, by = c("cleaned.taxa.name.lower" = "search.string.manually.resolved")
  ) %>% 
  
  select(
    -cleaned.taxa.name.lower
  )








bodysize_taxonomy <-  bodysize_raw %>% 
  
  # join resolved names onto raw data
  left_join(
    ., select(
      resolved_names_2gbif_manual, original.taxa.name, resolved.taxa.name),
    by = "original.taxa.name"
  ) %>% 
  
  # join taxonomy uid
  left_join(
    tax_list_multiples, by = "resolved.taxa.name"
  ) %>% 
  
  # select and reorder
  select(
    uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
    join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
    join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
    individual.uid, original.taxa.name, taxa.name, tax.uid, type, rank, species, genus, family, order, class, phylum, kingdom,
    life.stage, sex, nu, ind.per.nu,
    min.body.size, max.body.size, body.size,
    bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type,
    sample.year, sample.month
  ) %>% 
  
  # Remove any without a taxa.name
  filter(
    !is.na(taxa.name)
  )



