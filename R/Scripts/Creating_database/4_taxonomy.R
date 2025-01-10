# Adding taxonomy data to the species list 
# data run through resolver on 17/10/2024
# data ran through taxize on 24/10/2024

# Aim of script
# 1) gna_verifyer - Run the names through the gna_verifyer first to get rid of any spelling mistakes 
# 2) Manually resolve any names that weren't picked up by resolver and also change ones that are form or variety back as they resolver gets rid of the var. f. and I want to keep them
# 3) Run through classification to get taxonomy

library(devtools)
install_github("ropensci/bold")
install_github("ropensci/taxize")

# Packages
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)

# Import data ----
bodysize_joined <- readRDS("R/data_outputs/full_database/bodysize_joined.rds")

# Resolve names ----

## gna ----
# run through gna_verifier to fix any spelling

# get a list of all distinct names 
distinct_names <- select(bodysize_joined, original.taxa.name) %>% 
  distinct(original.taxa.name)

# convert to a string of names
names_list <- paste0(distinct_names$original.taxa.name)
glimpse(names_list)

# run string through the resolver and select columns I want - run through a catchall because it will throw an error when it doesn't recognise a name
resolved_names_gna <- do.call(rbind, lapply(names_list, function(name) {
  tryCatch(
    {
      # Process each name and return the result
      gna_verifier(name) %>%
        as.data.frame() %>%
        select(submittedName, matchedCanonicalFull, dataSourceTitleShort)
    },
    error = function(e) {
      # Fallback for errors
      data.frame(
        submittedName = name,
        matchedCanonicalFull = NA,
        dataSourceTitleShort = NA)
    }
  )
})) %>% 
  rename(
    original.taxa.name = submittedName,
    resolved.taxa.name.gna = matchedCanonicalFull,
    resolved.source.gnr = dataSourceTitleShort
  )

# Save
saveRDS(resolved_names_gna, file = "R/data_outputs/taxonomy/resolved_names_gna.rds")

## manual ----
# How manual resolving was carried out:
  # When the species name could be found then use that
  # When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
  # when the species can't be found then the next highest rank is chosen
  # When two species are stated then the closet common higher group is used

# Finding all the taxa.names from resolved_gna to manually resolve based on the criteria below
# Weren't resolved:
to_resolve_manually <- resolved_names_gna %>% 
  mutate(
    manual = case_when(
      # Ones that weren't picked up by the resolver at all - gave NA for the resolved.taxa.name
      is.na(resolved.taxa.name.gna) ~ "na",
      # Ones that were bumped up a taxonomic rank by the resolver, ones that are two words in original.species.name (contain a space) but one word in the resolved name (no sapce)
      stri_detect_regex(original.taxa.name, " ") & 
        !(stri_detect_regex(resolved.taxa.name.gna, " ")) & 
        !(stri_detect_regex(original.taxa.name, "\\b(?i)sp\\.|\\b(?i)spp\\.|\\b(?i)sp\\b|\\b(?i)spp\\b|\\b(?i)sp(\\d+)|\\b(?i)ssp\\b")) ~ "bumped",
      # Ones where the resolved.taxa.name was set to the name of the juvenile form or a common name instead of the taxa.name (e.g. nauplii or cyclops instead of copepoda)
      stri_detect_regex(resolved.taxa.name.gna, "\\b(?i)cyst\\b|\\b(?i)stomatocyst\\b|\\b(?i)nauplius\\b|\\b(?i)centric\\b|\\b(?i)volvocales\\b|\\b(?i)cyclops\\b") ~ "juvenile",
      TRUE ~ "keep"
    )
  ) %>% 
  filter(
    !(manual == "keep")
  )

# Save
write_csv(to_resolve_manually, "R/data_outputs/taxonomy/to_resolve_manually.csv")

# Join in manually resolved ones
# replace the resolved.taxa.name of the ones in resolved_names_gna to the manually resolved ones when a manually resolved name is present
# How manual resolving was carried out:
  # When the species name could be found then use that
  # When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
  # when the species can't be found then the next highest rank is chosen
  # When two species are stated then the closet common higher group is used

old_resolved <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "old_manual_resolve")
to_resolve <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "manual_resolve")

x <- left_join(to_resolve, old_resolved, by = "original.taxa.name")


# Import the to_resolve_manual list with the now manually resolved names
manually_resolved <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "resolve")


# Join the manually resolved names from the manually_resolved
resolved_names <- resolved_names_gnr %>% 
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

## Step 1: Classification ----
# Run through classification with tree of life

taxonomy_tol_step1_raw <- select(resolved_names, resolved.taxa.name) %>% 
  
  # Select all distinct resolved.taxa.names from resolved_names
  distinct(resolved.taxa.name) %>% 
  
  # set to rowwise so that tit takes each value individually - otherwise just assigns the first row for all of them
  rowwise() %>% 
  
  # run through classification - doing it in the dataframe and not passing a string like above so the original name is kept incase a new name is given by TOL
  mutate(
    taxonomy = list(classification(resolved.taxa.name, db = "tol", return_id = TRUE, rows = 1)[[1]])
  )

# Save
saveRDS(taxonomy_tol_step1_raw, file = "R/Data_outputs/Taxonomy/tol/taxonomy_tol_step1_raw.rds") 

# Extract info
taxonomy_tol_step1_extracted <- taxonomy_tol_step1_raw %>% # make data frame to remove rowwise for the tax.uid
    
  mutate(
    # Set all taxa that didn't come back with a name to NA
    taxonomy = ifelse("name" %in% colnames(taxonomy), list(taxonomy), NA),
    
    # Extract information
    # If desired rank name is present in the rank column, select the row of the name column that matches the row for that rank
    # Ranks:
    form = ifelse("form" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "form"], NA_character_),
    variety = ifelse("variety" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "variety"], NA_character_),
    species = ifelse("species" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "species"], NA_character_),
    genus = ifelse("genus" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "genus"], NA_character_),
    family = ifelse("family" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "family"], NA_character_),
    order = ifelse("order" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "order"], NA_character_),
    class = ifelse("class" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "class"], NA_character_),
    phylum = ifelse("phylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "phylum"], NA_character_),
    kingdom = ifelse("kingdom" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "kingdom"], NA_character_),
    
    # Extra info:
    rank = ifelse("rank" %in% colnames(taxonomy), as.character(taxonomy$rank[nrow(taxonomy)]), NA_character_),
    tol.id = ifelse("id" %in% colnames(taxonomy), as.character(taxonomy$id[nrow(taxonomy)]), NA_character_),
    
    # source
    source = "tol"
  ) %>% 
  
  select(
    -taxonomy
  )

## Step 2: Variety/form ----
# tol does't pick up the form or variety ones well so select just the first two words when theres three and re-run

taxonomy_tol_step2_raw <- taxonomy_tol_step1_extracted %>% 
  filter(
    is.na(tol.id)
  ) %>% 
  
  select(
    resolved.taxa.name,
    tol.name
  ) %>% 
  
  mutate(
    # select first two words
    tol.name = paste0(stri_extract_all_regex(resolved.taxa.name, "\\w+(-\\w+)? \\w+(-\\w+)?\\b")),
    
    # run through classification
    taxonomy = list(classification(tol.name, db = "tol", return_id = TRUE, rows = 1)[[1]])
  )

# Save
saveRDS(taxonomy_tol_step2_raw, file = "R/Data_outputs/Taxonomy/tol/taxonomy_tol_step2_raw.rds") 

# Extract info
taxonomy_tol_step2_extracted <- taxonomy_tol_step2_raw %>%
  
  mutate(
    # Set all taxa that didn't come back with a name to NA
    taxonomy = ifelse("name" %in% colnames(taxonomy), list(taxonomy), NA),
    
    # Extract information
    # If desired rank name is present in the rank column, select the row of the name column that matches the row for that rank
    # Ranks:
    form = ifelse("form" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "form"], NA_character_),
    variety = ifelse("variety" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "variety"], NA_character_),
    species = ifelse("species" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "species"], NA_character_),
    genus = ifelse("genus" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "genus"], NA_character_),
    family = ifelse("family" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "family"], NA_character_),
    order = ifelse("order" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "order"], NA_character_),
    class = ifelse("class" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "class"], NA_character_),
    phylum = ifelse("phylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "phylum"], NA_character_),
    kingdom = ifelse("kingdom" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "kingdom"], NA_character_),
    
    # Extra info:
    rank = ifelse("rank" %in% colnames(taxonomy), as.character(taxonomy$rank[nrow(taxonomy)]), NA_character_),
    tol.id = ifelse("id" %in% colnames(taxonomy), as.character(taxonomy$id[nrow(taxonomy)]), NA_character_),
    
    # source
    source = "tol"
  ) %>% 
  
  select(
    -taxonomy,
    -tol.name
  )

# Add to main data
taxonomy_tol_step2 <- left_join(
  taxonomy_tol_step1_extracted, taxonomy_tol_step2_extracted,
  by = "resolved.taxa.name",
  suffix = c(".old", ".new")
  ) %>% 
  
  mutate(
    form = if_else(!is.na(tol.id.new), form.new, form.old),
    variety = if_else(!is.na(tol.id.new), variety.new, variety.old),
    species = if_else(!is.na(tol.id.new), species.new, species.old),
    genus = if_else(!is.na(tol.id.new), genus.new, genus.old),
    family = if_else(!is.na(tol.id.new), family.new, family.old),
    order = if_else(!is.na(tol.id.new), order.new, order.old),
    class = if_else(!is.na(tol.id.new), class.new, class.old),
    phylum = if_else(!is.na(tol.id.new), phylum.new, phylum.old),
    kingdom = if_else(!is.na(tol.id.new), kingdom.new, kingdom.old),
    rank = if_else(!is.na(tol.id.new), rank.new, rank.old),
    tol.id = if_else(!is.na(tol.id.new), tol.id.new, tol.id.old),
    source = if_else(!is.na(tol.id.new), source.new, source.old)
  ) %>% 
  select(
    -form.old:-source.new
  )


## Step 3: NAs ----
# find all that weren't picked up by tol and check if there are more up top date names and run with those
tol_na <- taxonomy_tol_step2_extracted %>% 
  filter(
    is.na(tol.id)
  ) %>% 
  select(
    resolved.taxa.name
  )

# save
write_csv(tol_na, "R/Data_outputs/Taxonomy/tol/tol_na.csv")

# Import updated names
tol_na_manual <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "na")

# Run updated names through classification
taxonomy_tol_step3_raw <- tol_na_manual %>% 
  
  # redo rowwsie becasue it's a newly imported data frame
  rowwise() %>% 
  
  mutate(
    # run through classification
    taxonomy = list(classification(new.name, db = "tol", return_id = TRUE, rows = 1)[[1]])
  )
  
# Save
saveRDS(taxonomy_tol_step3_raw, file = "R/Data_outputs/Taxonomy/tol/taxonomy_tol_step3_raw.rds") 

# Extract info
taxonomy_tol_step3_extracted <- taxonomy_tol_step3_raw %>%
  
  mutate(
    # Set all taxa that didn't come back with a name to NA
    taxonomy = ifelse("name" %in% colnames(taxonomy), list(taxonomy), NA),
    
    # Extract information
    # If desired rank name is present in the rank column, select the row of the name column that matches the row for that rank
    # Ranks:
    form = ifelse("form" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "form"], NA_character_),
    variety = ifelse("variety" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "variety"], NA_character_),
    species = ifelse("species" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "species"], NA_character_),
    genus = ifelse("genus" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "genus"], NA_character_),
    family = ifelse("family" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "family"], NA_character_),
    order = ifelse("order" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "order"], NA_character_),
    class = ifelse("class" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "class"], NA_character_),
    phylum = ifelse("phylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "phylum"], NA_character_),
    kingdom = ifelse("kingdom" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "kingdom"], NA_character_),
    
    # Extra info:
    rank = ifelse("rank" %in% colnames(taxonomy), as.character(taxonomy$rank[nrow(taxonomy)]), NA_character_),
    tol.id = ifelse("id" %in% colnames(taxonomy), as.character(taxonomy$id[nrow(taxonomy)]), NA_character_),
    
    # source
    source = "tol"
  ) %>% 
  
  select(
    -taxonomy,
    -new.name
  )

# Add to main data
taxonomy_tol_step3 <- left_join(
  taxonomy_tol_step2, taxonomy_tol_step3_extracted,
  by = "resolved.taxa.name",
  suffix = c(".old", ".new")
  ) %>% 
  
  mutate(
    form = if_else(!is.na(tol.id.new), form.new, form.old),
    variety = if_else(!is.na(tol.id.new), variety.new, variety.old),
    species = if_else(!is.na(tol.id.new), species.new, species.old),
    genus = if_else(!is.na(tol.id.new), genus.new, genus.old),
    family = if_else(!is.na(tol.id.new), family.new, family.old),
    order = if_else(!is.na(tol.id.new), order.new, order.old),
    class = if_else(!is.na(tol.id.new), class.new, class.old),
    phylum = if_else(!is.na(tol.id.new), phylum.new, phylum.old),
    kingdom = if_else(!is.na(tol.id.new), kingdom.new, kingdom.old),
    rank = if_else(!is.na(tol.id.new), rank.new, rank.old),
    tol.id = if_else(!is.na(tol.id.new), tol.id.new, tol.id.old),
    source = if_else(!is.na(tol.id.new), source.new, source.old)
  ) %>% 
  select(
    -form.old:-source.new
  )

# Save and manually edit any missing ranks
write_csv(taxonomy_tol_step3, "R/Data_outputs/Taxonomy/tol/taxonomy_tol_step3.csv")



















## Classification: rows != 1 ----
# find any that were not classified, have missing ranks or were bumped up a taxonmic level and rerun classification with rows = 1 off so i can manually select ones

x <- taxonomy_tol_step1_extracted %>% 
  filter(
    is.na(tol.id)
  )

# Select ones that were not classified or were bumped up
taxonomy_step2_subset <- taxonomy_tol_step1_extracted %>%
  mutate(
    manual = case_when(
      # ones that were not picked up at all
      is.na(tol.id) ~ "yes",
      
      # ones that were bumped up to a higher rank
      stri_detect_regex(resolved.taxa.name, " ") & is.na(species) ~ "yes",
      rank == "kingdom" & resolved.taxa.name != kingdom ~ "yes",
      rank == "phylum" & resolved.taxa.name != phylum ~ "yes",
      rank == "class" & resolved.taxa.name != class ~ "yes",
      rank == "order" & resolved.taxa.name != order ~ "yes",
      rank == "family" & resolved.taxa.name != family ~ "yes",
    )
  ) %>% 
  
  filter(
    manual == "yes"
  )
  
# Run through classification qith rows = 1 turned off
taxonomy_tol_step2_raw <- select(taxonomy_step2_subset, resolved.taxa.name) %>% 
  
  # run through classification - doing it in the dataframe and not passing a string like above so the original name is kept incase a new name is given by TOL
  mutate(
    taxonomy = list(classification(resolved.taxa.name, db = "tol", return_id = TRUE)[[1]])
  )

# Save
saveRDS(taxonomy_tol_step2_raw, file = "R/Data_outputs/Taxonomy/tol/taxonomy_tol_step2_raw.rds") 

# Extract info
taxonomy_tol_step2_extracted <- taxonomy_tol_step2_raw %>%
  
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
  )

# Join new stuff from subset onto main data

taxonomy_tol_step2 <- left_join(
  taxonomy_tol_step1_extracted, taxonomy_tol_step2_extracted,
  by = "resolved.taxa.name",
  suffix = c(".old", ".new")) %>% 
  
  mutate(
    form = if_else(!is.na(tol.id.new), form.new, form.old),
    variety = if_else(!is.na(tol.id.new), variety.new, variety.old),
    species = if_else(!is.na(tol.id.new), species.new, species.old),
    genus = if_else(!is.na(tol.id.new), genus.new, genus.old),
    family = if_else(!is.na(tol.id.new), family.new, family.old),
    order = if_else(!is.na(tol.id.new), order.new, order.old),
    class = if_else(!is.na(tol.id.new), class.new, class.old),
    phylum = if_else(!is.na(tol.id.new), phylum.new, phylum.old),
    kingdom = if_else(!is.na(tol.id.new), kingdom.new, kingdom.old),
    rank = if_else(!is.na(tol.id.new), rank.new, rank.old),
    tol.id = if_else(!is.na(tol.id.new), tol.id.new, tol.id.old),
    source = if_else(!is.na(tol.id.new), source.new, source.old)
  ) %>% 
  select(
    -form.old:-source.new
  )

x <- taxonomy_tol_step2 %>% 
  filter(
    is.na(tol.id)
  )






## Manual edits ----
# manullay fill in any missing info or any that were bumped up a taxonomic level
# NEED TO REDO ACCEPTED TAXA NAME AND ASSIGN PHYTO/ZOO GROUP AND MAKE EVERYTHING BELOW SPECIES A SPECIES, TAX.UID

to_taxonomy_manual <- taxonomy_tol_raw_extracted %>%
  mutate(
    manual = case_when(
      # ones that were not picked up at all
      is.na(tol.id) ~ "yes",
      
      # ones with missing ranks
      is.na(kingdom) ~ "yes",
      is.na(phylum) ~ "yes",
      is.na(class) ~ "yes",
      is.na(order) ~ "yes",
      is.na(family) ~ "yes",
      is.na(genus) ~ "yes",
      
      # ones that were bumped up to a higher rank
      stri_detect_regex(resolved.taxa.name, " ") & is.na(species) ~ "yes",
      rank == "kingdom" & resolved.taxa.name != kingdom ~ "yes",
      rank == "phylum" & resolved.taxa.name != phylum ~ "yes",
      rank == "class" & resolved.taxa.name != class ~ "yes",
      rank == "order" & resolved.taxa.name != order ~ "yes",
      rank == "family" & resolved.taxa.name != family ~ "yes",
    )
  ) %>% 
  
  filter(
    manual == "yes"
  )

x <- taxonomy_tol_extracted %>% 
  filter(
    stri_detect_regex(resolved.taxa.name, " "),
    is.na(species)
  )

# NA - 57

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


  
  



