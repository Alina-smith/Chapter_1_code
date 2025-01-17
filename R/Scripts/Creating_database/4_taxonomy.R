# Adding taxonomy data to the species list 
# data run through veryfier on 13/1/2025
# data ran through taxize on 17/1/2025

# Aim of script
# Resolve names - Run the names through gna_veryfier to fix spellings and get most up to date synonyms and manually resolve any that weren't picked up
# Taxonomy - Run resolved names through classification for tol, worms, itis and gbif to get taxonomic hierarchy for taxa and manually fill in any that weren't picked up

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

# run string through the resolver and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
resolved_names_gna <- do.call(rbind, lapply(names_list, function(name) {
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
    resolved.taxa.name.gna = matchedCanonicalFull,
    resolved.source.gna = dataSourceTitleShort
  )

# Save
saveRDS(resolved_names_gna, file = "R/data_outputs/taxonomy/resolved_names_gna.rds")

## Manual ----
# How manual resolving was carried out:
  # When the species name could be found then use that
  # When the species name couldn't be found on a database then keep the original.taxa.name in case it is a newly discovered species not in the databases yet
  # When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
  # When two species are stated then the closet common higher group is used

# 1) Find all the taxa.names from resolved_gna to manually resolve based on the criteria in the comments
to_resolve_manually <- resolved_names_gna %>% 
  mutate(
    manual = case_when(
      # Ones that weren't picked up by the resolver at all - gave NA for the resolved.taxa.name
      is.na(resolved.taxa.name.gna) ~ "na",
      
      # Ones that were a variety or form as the resolver removed the var. and f. and this is needed for classification steps
      stri_detect_regex(original.taxa.name, " f\\.| var\\.") ~ "var.f",
      
      # Ones that were bumped up a taxonomic rank by the resolver - had two words in original.species.name (contain a space) but one word in the resolved name (no space)
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

# 2) Update resolved.taxa.names with the manually resolved names

# Import the manually resolved names
manually_resolved <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "resolve")

# Replace resolved.taxa.name with manually resolved names when ones is present
resolved_names <- resolved_names_gna %>% 
  # left join all the manually resolved ones from manual_resolve spreadsheet
  left_join(
    manually_resolved,
    by = "original.taxa.name"
  ) %>% 
  # when a name has been resolved manually (resolved source = "manaully") then select that name else keep the current one
  mutate(
    resolved.taxa.name = if_else(
      !is.na(resolved.source.manual),
      resolved.taxa.name.manual,
      resolved.taxa.name.gna
    ),
    
    resolved.source = if_else(
      !is.na(resolved.source.manual),
      resolved.source.manual,
      resolved.source.gna
    )
  ) %>% 
  
  select(
    original.taxa.name,
    resolved.taxa.name,
    resolved.source
  )

# Save
saveRDS(resolved_names, file = "R/data_outputs/taxonomy/resolved_names.rds")
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Taxonomy ----

## Classification ----

# get a list of all distinct names 
distinct_resolved_names <- select(resolved_names, resolved.taxa.name) %>% 
  # get distinct names
  distinct(resolved.taxa.name)

# convert to a string of names
resolved_names_list <- paste0(distinct_resolved_names$resolved.taxa.name)
glimpse(distinct_resolved_names)

# Initial run through classification to get taxonomic hierachy
# Tree of life
tax_tol_raw <- rbind(classification(resolved_names_list, db = "tol", return_id = FALSE, rows = 1)) %>% 
  filter(
    rank %in% c("form", "variety", "species", "genus", "family", "order", "class", "phylum", "kingdom", "domain")
  ) %>% 
  mutate(
    rank = paste(rank, "tol", sep = ".")
  ) %>% 
  pivot_wider(.,
              names_from = rank,
              values_from = name) %>% 
    unnest_wider(col = everything(), names_sep = ".") %>% 
  rename(
    resolved.taxa.name = query.1
  )  %>% 
  left_join(distinct_resolved_names, ., by = "resolved.taxa.name")

# Save
saveRDS(tax_tol_raw, file = "R/data_outputs/taxonomy/tax_tol_raw.rds")

# Worms
tax_worms_raw <- rbind(classification(resolved_names_list, db = "worms", return_id = FALSE, rows = 1)) %>% 
  filter(
    rank %in% c("Form", "Variety", "Species", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom", "Domain")
  ) %>% 
  mutate(
    rank = paste(rank, "worms", sep = ".")
  ) %>% 
  pivot_wider(.,
              names_from = rank,
              values_from = name) %>% 
  unnest_wider(col = everything(), names_sep = ".") %>% 
  rename(
    resolved.taxa.name = query.1
  ) %>% 
  rename_with(
    ., tolower
  ) %>% 
  left_join(distinct_resolved_names, ., by = "resolved.taxa.name")

# Save
saveRDS(tax_worms_raw, file = "R/data_outputs/taxonomy/tax_worms_raw.rds")

# Gbif
tax_gbif_raw <- rbind(classification(resolved_names_list, db = "gbif", return_id = FALSE, rows = 1)) %>% 
  filter(
    rank %in% c("form", "variety", "species", "genus", "family", "order", "class", "phylum", "kingdom", "domain")
  ) %>% 
  mutate(
    rank = paste(rank, "gbif", sep = ".")
  ) %>% 
  pivot_wider(.,
              names_from = rank,
              values_from = name) %>% 
  unnest_wider(col = everything(), names_sep = ".") %>% 
  rename(
    resolved.taxa.name = query.1
  ) %>% 
  left_join(distinct_resolved_names, ., by = "resolved.taxa.name")

# Save
saveRDS(tax_gbif_raw, file = "R/data_outputs/taxonomy/tax_gbif_raw.rds")

# Itis
tax_itis_raw <- rbind(classification(resolved_names_list, db = "itis", return_id = FALSE, rows = 1)) %>% 
  filter(
    rank %in% c("form", "variety", "species", "genus", "family", "order", "class", "phylum", "kingdom", "domain")
  ) %>% 
  mutate(
    rank = paste(rank, "itis", sep = ".")
  ) %>% 
  pivot_wider(.,
              names_from = rank,
              values_from = name) %>% 
  unnest_wider(col = everything(), names_sep = ".") %>% 
  rename(
    resolved.taxa.name = query.1
  ) %>% 
  left_join(distinct_resolved_names, ., by = "resolved.taxa.name")

# Save
saveRDS(tax_itis_raw, file = "R/data_outputs/taxonomy/tax_itis_raw.rds")

# combine all together
tax_all_raw <- left_join(distinct_resolved_names, tax_gbif_raw, by = "resolved.taxa.name") %>% 
  left_join(., tax_tol_raw, by = "resolved.taxa.name") %>% 
  left_join(., tax_worms_raw, by = "resolved.taxa.name") %>% 
  mutate(
    form.tol.1 = NA,
    variety.tol.1 = NA,
    
    domain.gbif.1 = NA,
    
    form.worms.1 = NA,
    domain.worms.1 = NA,
  ) %>% 
  relocate(
    resolved.taxa.name, form.tol.1, variety.tol.1, species.tol.1, genus.tol.1, genus.tol.2, family.tol.1, family.tol.2, order.tol.1, order.tol.2, class.tol.1, class.tol.2, phylum.tol.1, phylum.tol.2, kingdom.tol.1, kingdom.tol.2, domain.tol.1,
    form.gbif.1, variety.gbif.1, species.gbif.1, genus.gbif.1, family.gbif.1, order.gbif.1, class.gbif.1, phylum.gbif.1, kingdom.gbif.1, domain.gbif.1,
    form.worms.1, variety.worms.1, species.worms.1, genus.worms.1, family.worms.1, order.worms.1, class.worms.1, phylum.worms.1, kingdom.worms.1, domain.worms.1,
  )

# Save
saveRDS(tax_all_raw, file = "R/data_outputs/taxonomy/tax_all_raw.rds")



















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


  
  



