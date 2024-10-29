# Adding taxonomy data to the species list 
# data run through resolver on 17/10/2024
# data ran through taxize on 24/10/2024

# Aim of script
# 1) resolved_gnr - Run the names through the gnr_resolve first to get rid of any spelling mistakes 
# 2) Manually resolve any names that weren't picked up by resolver and also change ones that are form or variety back as they resolver gets rid of the var. f. and I want to keep them
# 3) Run through classification to get taxonomy

# Packages
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)
library(taxize)

# Import data ----
# set file path:
master_db_path <- here("Raw_data", "Master_db_traits.xlsx")

bodysize_location <- readRDS("R/Data_outputs/full_database/bodysize_location.rds")


# Resolve names ----
## gnr ----

# run through gnr_resolve to fix any spellin
resolved_names_raw <- select(bodysize_location, original.taxa.name) %>% 
  
  # Select all distinct original.taxa.names from all_raw
  distinct(original.taxa.name) %>% 
  
  rowwise() %>% # have to set as rowwise otherwise the gnr_resolve assigns the first species as all the species
  mutate(
    # Run through resolver
    resolved = list(gnr_resolve(sci = original.taxa.name, http = "post", canonical = TRUE, best_match_only = TRUE)),
    
    # extract information
    resolved.taxa.name.gnr = ifelse(
      "matched_name2" %in% colnames(resolved),
      resolved$matched_name2,
      NA
    ),
    resolved.source.gnr = ifelse(
      "matched_name2" %in% colnames(resolved),
      resolved$data_source_title,
      NA
    )
  ) %>% 
  # remove excess info
  select(-resolved)

# Save
saveRDS(resolved_names_raw, file = "R/Data_outputs/taxonomy/tol/resolved_names_raw.rds")

## manual ----
# Find all the taxa.names that needed to be resolved manually from the gnr_resolve dataframe and manually resolved them in a separate excel sheet
# How taxa.names were chosen for manual resolving:
# Ones that were bumped up a taxonomic rank by the resolver - if the species is valid keep that otherwise keep the higher rank
# Ones where the resolved.taxa.name was set to the name of the juvenile form or a common name instead of the taxa.name (e.g. nauplii or cyclops instead of copepoda)
# Ones that weren't picked up by the resolver at all - gave NA for the resolved.taxa.name
# Ones with the *SpecChar* regex put in to replcae special characters when joining the data together just to check that the regex didn't cause the resolver to resolve it weirdly

# How manual resolving was carried out:
# When the species name could be found then use that
# When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
# when the species can't be found then the next highest rank is chosen
# When two species are stated then the closet common higher group is used

## To manually resolve list ----
# Finding all the taxa.names from resolved_gnr to manually resolve based on the criteria above

# Weren't resolved:
NAs <- resolved_names_raw %>% 
  filter(
    is.na(resolved.taxa.name.gnr)
  )

# Bumped up a taxonomic level:
## Find all that has a space in the original.taxa.name and not in the resolved.taxa.name as this will indicate that it was two names (species) and now one name (higher rank)
bumped_up <- resolved_names_raw %>% 
  mutate(
    bumped = case_when(
      stri_detect_regex(original.taxa.name, " ") & !(stri_detect_regex(resolved.taxa.name.gnr, " ")) ~ "bumped",
      TRUE ~ "same"
    ),
    
    sp = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)sp\\.|\\b(?i)spp\\.|\\bsp\\b|\\bspp\\b|\\bsp(\\d+)|\\bssp\\b") ~ "sp",
      TRUE ~ "not"
    )
  ) %>% 
  filter(
    bumped == "bumped",
    sp == "not",
  )

# Resolved.taxa.name is juvenile form or common name:
resolved_wrong <- resolved_names_raw %>% 
  mutate(
    wrong = case_when(
      resolved.taxa.name.gnr %in% c("Cyst", "Nauplius", "Centric", "Centric diatom", "Volvocales", "Cyclops") ~ "yes",
      TRUE ~ "no"
    )
  ) %>% 
  filter(
    wrong == "yes"
  )

# Special charaters:
spec_char <- resolved_names_raw %>% 
  filter(
    stri_detect_regex(original.taxa.name, "\\*SpecChar\\*")
  )

# Join together - list of names to resolve manually
to_resolve_manually <- bind_rows(NAs, bumped_up, resolved_wrong, spec_char) %>% 
  # get distinct original.taxa.names in case some got picked up more than once
  distinct(
    original.taxa.name
  )

# Save
write_csv(to_resolve_manually, "R/Data_outputs/Taxonomy/tol/to_resolve_manually.csv")

## Join in manually resolved ones ----
# replace the resolved.taxa.name of the ones in resolved_gnr to the manually resolved ones when a manually resolved name is present

# Import the to_resolve_manual list with the now manually resolved names
manually_resolved <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "resolve")

# Join the manually resolved names from the manually_resolved
resolved_names <- resolved_names_raw %>% 
  # left join all the manually resolved ones from manual_resolve spreadsheet
  left_join(
    manually_resolved,
    by = "original.taxa.name"
  ) %>% 
  
  # select the ones left joined in
  mutate(
    resolved.taxa.name = case_when(
      resolved.taxa.name.manual == "couldn't resolve" ~ NA, # change the ones that couldn't be resolved to NA
      !is.na(resolved.taxa.name.manual) ~ resolved.taxa.name.manual,
      TRUE ~ resolved.taxa.name.gnr
    ),
    
    resolved.source = case_when(
      resolved.taxa.name.manual == "couldn't resolve" ~ NA,
      !is.na(resolved.source.manual) ~ resolved.source.manual,
      TRUE ~ resolved.source.gnr
    )
  ) %>% 
  
  select(
    original.taxa.name,
    resolved.taxa.name,
    resolved.source
  )

# Save
saveRDS(resolved_names, file = "R/Data_outputs/Taxonomy/tol/resolved_names.rds")
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Taxonomy ----

## Classification ----
# Run through classification with tree of life

taxonomy_tol_raw <- resolved_names %>% 
  
  # get list of distinct names
  distinct(
    resolved.taxa.name
  ) %>% 
  
  # run through classification
  mutate(
    taxonomy = list(classification(resolved.taxa.name.spelling, db = "gbif", return_id = TRUE, rows = 1)[[1]])
  )


# Save
saveRDS(taxonomy_tol_raw, file = "R/Data_outputs/Taxonomy/tol/taxonomy_tol_raw.rds") 

## Extract info ----
taxonomy_tol_extracted <- as.data.frame(taxonomy_tol_raw) %>% # make data frame to remove rowwise for the tax.uid
  
  mutate(
    # make a tax.uis column
    tax.uid = row_number()
  ) %>% 
  
  rowwise() %>% 
    
  mutate(
    # Set all taxa that didn't come back with a name to NA
    taxonomy = ifelse(
      "name" %in% colnames(taxonomy),
      list(taxonomy),
      NA
    ),
    
    # Extract information
    form = ifelse(
      "form" %in% taxonomy$rank, # if form is present in the rank column
      taxonomy$name[taxonomy$rank == "form"], # select the row of the name column that matches the row for form
      NA_character_),
    
    variety = ifelse(
      "variety" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "variety"],
      NA_character_),
    
    species = ifelse(
      "species" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "species"],
      NA_character_),
    
    genus = ifelse(
      "genus" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "genus"],
      NA_character_),
    
    family = ifelse(
      "family" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "family"],
      NA_character_),
    
    order = ifelse(
      "order" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "order"],
      NA_character_),
    
    class = ifelse(
      "class" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "class"],
      NA_character_),
    
    phylum = ifelse(
      "phylum" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "phylum"],
      NA_character_),
    
    kingdom = ifelse(
      "kingdom" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "kingdom"],
      NA_character_),
    
    rank = ifelse(
      "rank" %in% colnames(taxonomy),
      as.character(taxonomy$rank[nrow(taxonomy)]),
      NA_character_),
    
    tol.id = ifelse(
      "id" %in% colnames(taxonomy),
      as.character(taxonomy$id[nrow(taxonomy)]),
      NA_character_),
    
    # source
    source = "tol"
  ) %>% 
  
  select(
    -taxonomy
  ) %>% 
  
  mutate(
    
    rank = case_when(
      rank %in% c("variety", "form", "subspecies") ~ "species",
      TRUE ~ rank
    ),
    
    accepted.taxa.name = case_when(
      rank == "species" ~ species,
      rank == "genus" ~ genus,
      rank == "family" ~ family,
      rank == "order" ~ order,
      rank == "class" ~ class,
      rank == "phylum" ~ phylum,
      rank == "kingdom" ~ kingdom,
      TRUE ~ NA
    ),
    
    # Add a group column for whether it is phyto plankton or zooplankton
    group = case_when(
      kingdom == "Animalia" ~ "zooplankton",
      TRUE ~ "phytoplankton"
    )
  ) %>% 
  
  select(
    - form,
    - variety
  )

# Save
saveRDS(taxonomy_tol_extracted, file = "R/Data_outputs/taxonomy/tol/taxonomy_tol_extracted.rds") 

## Final taxonomy list ----
# geti distinct accepted.taxa.names that aren't na

taxonomy_list <- taxonomy_tol_extracted %>% 
  filter(
    !is.na(tol.id)
  ) %>% 
  distinct(
    accepted.taxa.name, .keep_all = TRUE
  ) %>% 
  select(
    -resolved.taxa.name
  ) %>% 
  relocate(
    tax.uid, tol.id, accepted.taxa.name, species, genus, family, order, class, phylum, kingdom
  )

write_csv(taxonomy_list, file = "R/Data_outputs/full_database/taxonomy_list.csv")

## add to main data ----
bodysize_taxonomy <- bodysize_location %>% 
  
  # first add in the resolved names
  left_join(
    select(
      resolved_names, original.taxa.name, resolved.taxa.name
    ), by = "original.taxa.name"
  ) %>% 
  
  # use the resolved names to add in taxonomy info
  left_join(
    select(
      taxonomy_tol_extracted, resolved.taxa.name, tax.uid, accepted.taxa.name
      ), by = "resolved.taxa.name"
  ) %>% 
  
  select(
    -resolved.taxa.name,
    - original.taxa.name
  ) %>% 
  
  relocate(
    uid, individual.uid, tax.uid, accepted.taxa.name, form, form.no, life.stage, sex, min.body.size, max.body.size, body.size, units, sample.size, error, error.type, sample.year, sample.month
  )

# Save
saveRDS(bodysize_taxonomy, file = "R/Data_outputs/full_database/bodysize_taxonomy.rds") 


  
  



