# Adding taxonomy data to the species list 
# data run through veryfier on 13/1/2025
# data ran through taxize on 20/1/2025

# Aim of script
# Resolve names - Run the names through gna_veryfier to fix spellings and get most up to date synonyms and manually resolve any that weren't picked up
# Taxonomy
  # 1) Run resolved names through classification with tol, then select all that weren't recognised by tol and update any synonyms and then rerun
  # 2) Run the names through classification with gbif 
  # 3) Fill in any gaps in the tol data with the gbif data

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

# Finished script ----

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
resolved_names_1_gna <- do.call(rbind, lapply(names_list, function(name) {
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
saveRDS(resolved_names_1_gna, file = "R/data_outputs/taxonomy/resolved_names_1_gna.rds")

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
      stri_detect_regex(resolved.taxa.name.gna, "\\b(?i)cyst\\b|\\b(?i)stomatocyst\\b|\\b(?i)nauplius\\b|\\b(?i)centric\\b|\\b(?i)volvocales\\b|\\b(?i)cyclops\\b|\\b(?i)mite\\b") ~ "juvenile",
      
      # Unknown
      stri_detect_regex(resolved.taxa.name.gna, "\\b(?i)unknown\\b") ~ "unknown",
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
resolved_names_2_manual <- resolved_names_gna %>% 
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
  ) %>% 
  
  filter(
    !(is.na(resolved.taxa.name))
  )

# Save
saveRDS(resolved_names_2_manual, file = "R/data_outputs/taxonomy/resolved_names_2_manual.rds")
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Taxonomy ----

## Classification: distinct names ----
# TOL: First run through classification with TOL and try and get as many of them with this database as this is the database that will be used in later steps for taxonomy figure

# Get a list of all distinct names
distinct_resolved_names <- select(resolved_names_2_manual, resolved.taxa.name) %>% 
  
  # get distinct names
  distinct(resolved.taxa.name) %>% 
  
  # TOL doesn't recognise variety and forms well so make a separate column which will be used to run through classification that selects the species name when var or f is present
  mutate(
    classification.name = if_else(
      stri_detect_regex(resolved.taxa.name, "var\\.|f\\."),
      paste0(stri_extract_all_regex(resolved.taxa.name, "\\w+(-\\w+)? \\w+(-\\w+)?\\b")),
      resolved.taxa.name
    )
  )

## Classification: TOL - Initial run ----
tax_tol_1_raw <- distinct_resolved_names %>% 
  
  rowwise() %>% # use rowwise so it looks at each row at a time
  
  mutate(
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for the species
      classification(
        classification.name, db = "tol", return_id = FALSE, rows = 1 # rows = 1 so that is only takes the first one and doesn't require you select options for each one
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
    -`no rank`,
    - tax,
    - classification.name
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
saveRDS(tax_tol_1_raw, file = "R/data_outputs/taxonomy/tax_tol_1_raw.rds")

## Classification: TOL - Clean intial run ----
# When there are multiple options for the same rank select the correct one
tax_tol_1_cleaned <- tax_tol_1_raw %>% 
  
  # Select correct names
  mutate(
    species.1 = case_when(
      is.na(species.1) & !(is.na(subspecies.1)) ~ subspecies.1,
      is.na(species.1) & !(is.na(varietas.1)) ~ varietas.1,
      TRUE ~ species.1
    ),
    
    genus.1 = if_else(
      !(is.na(genus.2)), # checked through them all and all are genus.2
      genus.2,
      genus.1
      ),
    
    order.1 = case_when(
      order.2 %in% c("Craspedida", "Neobodonida", "Parabodonida") ~ order.2,
      resolved.taxa.name %in% c("Bodo", "Bodo ovatus", "Bodo saltans") ~ "Bodonida",
      resolved.taxa.name == "Ochromonas viridis" ~ "Ochromonadales",
      resolved.taxa.name == "Trypanosomatida" ~ NA,
      TRUE ~ order.1
    ),
    
    family.1 = if_else(
      !(is.na(family.2)),
      family.2, # only one family.2 so don't need to specify which one
      family.1
    ),
    
    class.1 = case_when(
      class.2 %in% c("Oligotrichea", "Pavlovophyceae") ~ class.2,
      class.2 == "Prymnesiophyceae" ~ "Coccolithophyceae",
      TRUE ~ class.1
    ),
    
    phylum.1 = if_else(
      !(is.na(phylum.2)),
      phylum.2, # checked through them all and all are phylum.2
      phylum.1
    ),
    
    kingdom.1 = case_when(
      !is.na(kingdom.2) ~ "Plantae",
      TRUE ~ kingdom.1
    )
  ) %>% 
  
  select(
    resolved.taxa.name, varietas.1 , subspecies.1, species.1, subgenus.1, genus.1, subfamily.1, family.1, superfamily.1, suborder.1, order.1, superorder.1, subclass.1, class.1, superclass.1,
    subphylum.1, phylum.1, kingdom.1
  ) %>% 
  
  rename_with(
    ~ stri_replace_all_regex(., "\\.1$", "")
    ) %>% 
  
  rename(
    variety = varietas
  )

# Save
saveRDS(tax_tol_1_cleaned, file = "R/data_outputs/taxonomy/tax_tol_1_cleaned.rds")

## Classification: TOL - Not classified list ----

# Find all that weren't classified and see if they have any synonyms and update them
tol_not_classified <- tax_tol_1_cleaned %>% 
  
  # Make a column to say if the taxa.name was picked up by TOL or not
  mutate(
    classified = if_else(
      apply( # apply to each column
        select(
          .,-resolved.taxa.name # select all columns apart from these two as we only want to check the columns gotten from classification()
          ), 1, function(row) all(
            is.na(row) # if all selected columns in row is NA
            )
        ),
      "not classified",
      "classified"),
  ) %>% 
  
  # Select only ones that haven't been classified
  filter(
    classified == "not classified"
  )

# Save
write_csv(tol_not_classified, "R/data_outputs/taxonomy/tol_not_classified.csv")

# Read in new names
tol_not_classified_fix <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "tol_not_classified_fix")

# Update resolved names list with new names
resolved_names_3_not_classified_fix <- tol_not_classified_fix %>% 
  filter(
    !(is.na(new.name))
  ) %>% 
  left_join(resolved_names_2_manual, ., by = "resolved.taxa.name") %>% 
  mutate(
    resolved.taxa.name = if_else(
      !(is.na(new.name)),
      new.name,
      resolved.taxa.name
    ),
    
    resolved.source = if_else(
      !(is.na(new.name)),
      "manually",
      resolved.source
    )
  ) %>% 
  
  select(-new.name)
  
# Save
saveRDS(resolved_names_3_not_classified_fix, file = "R/data_outputs/taxonomy/resolved_names_3_not_classified_fix.rds")

## Classification: TOL - Second run ----

# Run new names from not_classified_fix through classification
tax_tol_2_not_classified_fix_raw <- tol_not_classified_fix %>% 
  filter(
    !(is.na(new.name))
  ) %>% 

  rowwise() %>% 
  
  mutate(
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for the species
      classification(
        new.name, db = "tol", return_id = FALSE, rows = 1 # rows = 1 so that is only takes the first one and doesn't require you select options for each one
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
    -`no rank`,
    - tax
  ) %>% 
  
  # Separate columns that have multiple names for a rank into multiple columns
  unnest_wider(
    col = everything(), names_sep = "."
  ) %>% 
  
  ungroup() %>%  # ungroup to remove rowwise
  
  rename(
    resolved.taxa.name = resolved.taxa.name.1,
    new.name = new.name.1
  )
  
# Save
saveRDS(tax_tol_2_not_classified_fix_raw, file = "R/data_outputs/taxonomy/tax_tol_2_not_classified_fix_raw.rds")

## Classification: TOL - Second clean ----

# Select columns I want and when there are multiple names for the same rank choose the correct one
tax_tol_2_not_classified_fix_cleaned <- tax_tol_2_not_classified_fix_raw %>% 
  
  mutate(
    kingdom.1 = case_when(
      !is.na(kingdom.2) ~ "Plantae",
      TRUE ~ kingdom.1
      )
  ) %>% 
  
  select(
    resolved.taxa.name, new.name, varietas.1 , species.1, genus.1, subfamily.1, family.1, superfamily.1, suborder.1, order.1, subclass.1, class.1,
    subphylum.1, phylum.1, kingdom.1
  ) %>% 
  
  rename_with(
    ~ stri_replace_all_regex(., "\\.1$", "")
  ) %>% 
  
  rename(
    variety = varietas
  )

# Replace the above names in the full list of names
tax_tol_2_cleaned <- tax_tol_1_cleaned %>% 
  filter(
    !(resolved.taxa.name %in% tax_tol_2_not_classified_fix_cleaned$resolved.taxa.name)
  ) %>% 
  
  bind_rows(., tax_tol_2_not_classified_fix_cleaned) %>% 
  
  mutate(
    resolved.taxa.name = if_else(
      !(is.na(new.name)),
      new.name,
      resolved.taxa.name
    )
  ) %>% 
  
  select(
    - new.name
  )

# Save
saveRDS(tax_tol_2_cleaned, file = "R/data_outputs/taxonomy/tax_tol_2_cleaned.rds")


## Classification: GBIF - Initial run ----
# Have gotten all I can from TOL so now run through gbif and fill in gaps with gbif

# Initial run through classification
tax_gbif_1_raw <- resolved_names_3_not_classified_fix %>% 
  
  distinct(resolved.taxa.name) %>% 
  
  rowwise() %>% # use rowwise so it looks at each row at a time
  
  mutate(
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for the species
      classification(
        resolved.taxa.name, db = "gbif", return_id = FALSE, rows = 1 # rows = 1 so that is only takes the first one and doesn't require you select options for each one
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
    -`no rank`,
    - tax
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
saveRDS(tax_gbif_1_raw, file = "R/data_outputs/taxonomy/tax_gbif_1_raw.rds")

## Classification: GBIF - Initial clean ----
tax_gbif_1_cleaned <- tax_gbif_1_raw %>% 
  rename_with(~ stri_replace_all_regex(., "\\.1$", "")) %>% 
  relocate(
    resolved.taxa.name, variety, form, subspecies, species, genus, family, order, class, phylum, kingdom
  )

# Save
saveRDS(tax_gbif_1_cleaned, file = "R/data_outputs/taxonomy/tax_gbif_1_cleaned.rds")

## Classification: GBIF - bumped up rerun ----

# Find all taxa that were bumped up a taxanomic group or have missing ranks and rerun with rows = 1 off to manaully chose them
tax_gbif_2_bumped <- tax_gbif_1_cleaned %>% 
  mutate(
    
    classified = if_else(
      apply( # apply to each column
        select(
          .,-resolved.taxa.name, # select all columns apart from these two as we only want to check the columns gotten from classification()
        ), 1, function(row) all(
          is.na(row) # if all selected columns in row is NA
        )
      ),
      "not classified",
      "classified"),
    
    rerun = case_when(
      
      # ones that were not picked up at all
      classified == "not classified" ~ "yes",
      
      # ones that were bumped up to a higher rank
      stri_detect_regex(resolved.taxa.name, " ") & is.na(species) ~ "yes", # were a species in resolved.taxa.name (had a space) but the species column is empty
      classified == "classified" & is.na(genus) ~ "yes",
      classified == "classified" & is.na(family) ~ "yes",
      classified == "classified" & is.na(order) ~ "yes",
      classified == "classified" & is.na(class) ~ "yes",
      classified == "classified" & is.na(phylum) ~ "yes",
      classified == "classified" & is.na(kingdom) ~ "yes",
      
      TRUE ~ "no"
    )
  ) %>% 
  
  filter(
    rerun == "yes"
  ) %>% 
  
  select(
    resolved.taxa.name
  ) %>% 
  mutate(
    id = row_number()
  )

classification("Cosmarium punctulatum", db = "gbif")

%>% 
  
  rowwise() %>% # use rowwise so it looks at each row at a time
  
  mutate(
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for the species
      classification(
        resolved.taxa.name, db = "gbif", return_id = FALSE # rows = 1 so that is only takes the first one and doesn't require you select options for each one
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
    -`no rank`,
    - tax
  ) %>% 
  
  # Separate columns that have multiple names for a rank into multiple columns
  unnest_wider(
    col = everything(), names_sep = "."
  ) %>% 
  
  rename(
    resolved.taxa.name = resolved.taxa.name.1
  ) %>% 
  
  ungroup() # ungroup to remove rowwise 
    


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









# Find any that were not classified, have missing ranks or were bumped up a taxonomic level and rerun classification with rows = 1 off so i can manually select ones

classification("Geissleria acceptata", db = "gbif")


x <- tax_tol_2_cleaned %>% 
  filter(
    is.na(phylum)
  )
  filter(
    stri_detect_regex(resolved.taxa.name, " ") & is.na(species)
  )
  distinct(
    genus
  )

# Select ones that were not classified or were bumped up
tax_tol_3_rerun_subset <- tax_tol_2_cleaned %>%
  mutate(
    
    classified = if_else(
      apply( # apply to each column
        select(
          .,-resolved.taxa.name, # select all columns apart from these two as we only want to check the columns gotten from classification()
        ), 1, function(row) all(
          is.na(row) # if all selected columns in row is NA
        )
      ),
      "not classified",
      "classified"),
    
    manual = case_when(
      
      # ones that were not picked up at all
      classified == "not classified" ~ "yes",
      
      # ones that were bumped up to a higher rank
      stri_detect_regex(resolved.taxa.name, " ") & is.na(species) ~ "yes", # were a species in resolved.taxa.name (had a space) but the species column is empty
      classified == "classified" & is.na(genus) ~ "yes",
      classified == "classified" & is.na(family) ~ "yes",
      classified == "classified" & is.na(order) ~ "yes",
      classified == "classified" & is.na(class) ~ "yes",
      classified == "classified" & is.na(phylum) ~ "yes",
      classified == "classified" & is.na(kingdom) ~ "yes",
      classified == "classified" & is.na(domain) ~ "yes",
      
      TRUE ~ "no"
    )
  ) %>% 
  
  filter(
    manual == "yes"
  )




























# Working script ----

# convert to a string of names
resolved_names_list <- paste0(distinct_resolved_names$resolved.taxa.name)
glimpse(distinct_resolved_names)

# 1) Initial run through classification to get taxonomic hierarchy
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

# 2) TOL doesn't recognise ones with form and variety will so select just the species name and rerun
tax_tol_var_f <- tax_tol_raw %>% 
  select(
    resolved.taxa.name
  ) %>% 
  filter(
    stri_detect_regex(resolved.taxa.name, "var\\.|f\\.")
  ) %>% 
  mutate(
    new.name = paste0(stri_extract_all_regex(resolved.taxa.name, "\\w+(-\\w+)? \\w+(-\\w+)?\\b"))
  ) %>% 
  head(5) %>% 
  rbind(classification(resolved_names_list, db = "tol", return_id = FALSE, rows = 1))
  













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
    
    tax.uid = row_number()
  ) %>% 
  relocate(
    tax.uid, resolved.taxa.name,
    form.tol.1, variety.tol.1, species.tol.1, genus.tol.1, genus.tol.2, family.tol.1, family.tol.2, order.tol.1, order.tol.2, class.tol.1, class.tol.2, phylum.tol.1, phylum.tol.2, kingdom.tol.1, kingdom.tol.2, domain.tol.1,
    form.gbif.1, variety.gbif.1, species.gbif.1, genus.gbif.1, family.gbif.1, order.gbif.1, class.gbif.1, phylum.gbif.1, kingdom.gbif.1, domain.gbif.1,
    form.worms.1, variety.worms.1, species.worms.1, genus.worms.1, family.worms.1, order.worms.1, class.worms.1, phylum.worms.1, kingdom.worms.1, domain.worms.1,
  )

# Save
saveRDS(tax_all_raw, file = "R/data_outputs/taxonomy/tax_all_raw.rds")




















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


  
  



