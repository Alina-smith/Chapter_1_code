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
    subphylum.1, phylum.1, kingdom.1, domain.1
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
  
  rowwise() %>% # use rowwise so it looks at each row at a time
  
  
  mutate(
    tax = tryCatch( # run through try catch because it randomly throws errors at different places each time and can't work out why
      {
        # Process each name and return the result
        list( # need to set as list so that it makes it into a list column with each row containing a dataframe for the species
          classification(
            resolved.taxa.name, db = "gbif", return_id = FALSE # rows = 1 so that is only takes the first one and doesn't require you select options for each one
          )[[1]] # select the first element of the list
        )
      },
      error = function(e) {
        # Fallback for errors - fill columns with NAs
        list(data.frame(
          name = NA,
          rank = "no rank"
          ))
      }
    ),
    
    # Change ones that didn't get classified from just NA to a dataframe of NAs
    tax = ifelse(
      is.data.frame(tax),
      list(tax),
      list(data.frame(
        name = NA,
        rank = "no rank"
        ))
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
saveRDS(tax_gbif_2_bumped_raw, file = "R/data_outputs/taxonomy/tax_gbif_2_bumped_raw.rds")

## Classification: GBIF - Clean and add into full list ----

# Clean tax_gbif_2_bumped
tax_gbif_2_bumped_cleaned <- tax_gbif_2_bumped_raw %>% 
  rename_with(~ stri_replace_all_regex(., "\\.1$", "")) %>% 
  relocate(
    resolved.taxa.name, variety, form, subspecies, species, genus, family, order, class, phylum, kingdom
  )

# add in tax_gbif_2_bumped_cleaned to full list
tax_gbif_2_cleaned <- tax_gbif_1_cleaned %>% 
  
  filter(
    !(resolved.taxa.name %in% tax_gbif_2_bumped_cleaned$resolved.taxa.name)
  )%>% 
  
  bind_rows(., tax_gbif_2_bumped_cleaned) 

# Save
saveRDS(tax_gbif_2_cleaned, file = "R/data_outputs/taxonomy/tax_gbif_2_cleaned.rds")

## Combining tol and gbif ----
tax_all <- left_join(
  tax_tol_2_cleaned, tax_gbif_2_cleaned,
  suffix = c(".tol", ".gbif"),
  by = "resolved.taxa.name") %>% 
  
  rename(
    form.gbif = form,
    domain.tol = domain
  ) %>% 
  
  mutate(
    
    # merge tol and gbif with preference for tol
    variety = case_when(
      !(is.na(variety.tol)) ~ variety.tol,
      is.na(variety.tol) & !(is.na(variety.gbif)) ~ variety.gbif,
      TRUE ~ NA
    ),
    
    form = case_when(
      !(is.na(form.gbif)) ~ form.gbif,
      TRUE ~ NA
    ),
    
    species = case_when(
      !(is.na(species.tol)) ~ species.tol,
      is.na(species.tol) & !(is.na(species.gbif)) ~ species.gbif,
      TRUE ~ NA
    ),
    
    genus = case_when(
      !(is.na(genus.tol)) ~ genus.tol,
      is.na(genus.tol) & !(is.na(genus.gbif)) ~ genus.gbif,
      TRUE ~ NA
    ), 
    
    family = case_when(
      !(is.na(family.tol)) ~ family.tol,
      is.na(family.tol) & !(is.na(family.gbif)) ~ family.gbif,
      TRUE ~ NA
    ), 
    
    order = case_when(
      !(is.na(order.tol)) ~ order.tol,
      is.na(order.tol) & !(is.na(order.gbif)) ~ order.gbif,
      TRUE ~ NA
    ), 
    
    class = case_when(
      !(is.na(class.tol)) ~ class.tol,
      is.na(class.tol) & !(is.na(class.gbif)) ~ class.gbif,
      TRUE ~ NA
    ), 
    
    phylum = case_when(
      !(is.na(phylum.tol)) ~ phylum.tol,
      is.na(phylum.tol) & !(is.na(phylum.gbif)) ~ phylum.gbif,
      TRUE ~ NA
    ), 
    
    kingdom = case_when(
      !(is.na(kingdom.tol)) ~ kingdom.tol,
      is.na(kingdom.tol) & !(is.na(kingdom.gbif)) ~ kingdom.gbif,
      TRUE ~ NA
    ), 
    
    domain = case_when(
      !(is.na(domain.tol)) ~ domain.tol,
      TRUE ~ NA
    ),
    
    # Manually fill in gaps
    variety = case_when(
      !(is.na(variety)) ~ variety,
      is.na(variety) & stri_detect_regex(resolved.taxa.name, "var\\.") ~ resolved.taxa.name,
      TRUE ~ variety
    ),
    
    form = case_when(
      !(is.na(form)) ~ variety,
      is.na(form) & stri_detect_regex(resolved.taxa.name, "f\\.") ~ resolved.taxa.name,
      TRUE ~ form
    ),
    
    species = case_when(
      !(is.na(species)) ~ species,
      is.na(species) & stri_detect_regex(resolved.taxa.name, " ") ~ resolved.taxa.name, # select all that have a space in
      TRUE ~ species
    ),
    
    # Genus will be the first word of the two
    genus = if_else(
      is.na(genus),
      stri_extract_first_regex(resolved.taxa.name, "\\w+(?= )"),
      genus
    ),
    
    family = case_when(
      !(is.na(family)) ~ family,
      genus %in% c("Romeria", "Rhabdoderma", "Lemmermannia") ~ "Cymatolegaceae",
      genus == "Fallacia" ~ "Sellaphoraceae",
      genus %in% c("Nitzschia", "Bacillaria") ~ "Bacillariaceae",
      genus == "Trichodina" ~ "Achatinidae",
      genus == "Schroederia" ~ "Schroederiaceae",
      genus == "Carteria" ~ "Chlamydomonadaceae",
      genus %in% c("Coccomyxa", "Microglena") ~ "Coccomyxaceae",
      genus %in%  c("Stokesiella", "Pseudokephyrion") ~ "Dinobryaceae",
      genus == "Anabaena" ~ "Aphanizomenonaceae",
      genus == "Euastrum" ~ "Desmidiaceae",
      genus == "Jaaginema" ~ "Synechococcales familia incertae sedis",
      genus == "Coenocystis" ~ "Radiococcaceae",
      genus == "Lobocystis" ~ "Chlorophyceae familia incertae sedis",
      genus %in% c("Encyonema", "Cymbella") ~ "Cymbellaceae",
      genus == "Oscillatoria" ~ "Oscillatoriaceae",
      genus == "Staurosira" ~ "Staurosiraceae",
      genus == "Stephanodiscus" ~ "Stephanodiscaceae",
      genus %in% c("Synuropsis", "Chrysodendron") ~ "Ochromonadaceae",
      genus == "Heterothrix" ~ "Tribonemataceae",
      genus == "Picochlorum" ~ "Chlorellales incertae sedis",
      genus == "Achnanthidium" ~ "Achnanthidiaceae",
      genus == "Eunotia" ~ "Eunotiaceae",
      genus == "Stauroneis" ~ "Stauroneidaceae",
      genus == "Amoeba" ~ "Amoebidae",
      genus == "Achnanthes" ~ "Achnanthaceae",
      genus == "Gomphonella" ~ "Cymbellales incertae sedis",
      genus == "Syracosphaera" ~ "Syracosphaeraceae",
      genus == "Amphichrysis" ~ "Chromulinaceae",
      genus == "Chroostipes" ~ "Cyanophyceae familia incertae sedis",
      genus %in% c("Chrysoxys", "Saccochrysis") ~ "Chromulinaceae",
      genus == "Dactylosphaerium" ~ "Dictyosphaeriaceae",
      genus == "Glenodinium" ~ "Peridiniales familia incertae sedis",
      genus == "Hortobagyiella" ~ "Hortobagyiella",
      genus == "Kephyrion" ~ "Chrysococcaceae",
      genus == "Monodus" ~ "Pleurochloridaceae",
      genus == "Phaeobotrys" ~ "Phaeothamniaceae",
      genus == "Rhaphidiopsis" ~ "Aphanizomenonaceae",
      genus == "Rhodomonas" ~ "Pyrenomonadaceae",
      genus == "Spirulina" ~ "Spirulinaceae",
      genus == "Euplotes" ~ "Euplotidae",
      genus == "Astasia" ~ "Astasiidae",
      TRUE ~ NA
    ),
    
    order = case_when(
      !(is.na(order)) ~ order,
      family == "Cymatolegaceae" ~ "Nodosilineales",
      family == "Wilmottiaceae" ~ "Coleofasciculales",
      family == "Radiococcaceae" ~ "Sphaeropleales",
      family == "Coccomyxaceae" ~ "Trebouxiophyceae ordo incertae sedis",
      family == "Amphidiniaceae" ~ "Dinophyceae",
      family == "Cymbellaceae" ~ "Cymbellales",
      family == "Bacillariaceae" ~ "Bacillariales",
      family == "Katablepharidaceae" ~ "	Katablepharidales",
      family %in% c("Achnanthidiaceae","Achnanthaceae") ~ "Achnanthales",
      family == "Eunotiaceae" ~ "Eunotiales",
      family %in% c("Stauroneidaceae", "Sellaphoraceae") ~ "Naviculales",
      family == "Stephanodiscaceae" ~ "Stephanodiscales",
      family == "Paramastigaceae" ~ "Spironematellales",
      family %in% c("Potamididae", "Thiaridae", "Paludomidae") ~ "Caenogastropoda incertae sedis",
      family == "Achatinidae" ~ "Stylommatophora",
      family == "Schroederiaceae" ~ "Sphaeropleales",
      family == "Suessiaceae" ~ "Suessiales",
      family == "Bicosoecaceae" ~ "Bicosoecales",
      family == "Chlamydomonadaceae" ~ "Chlamydomonadales",
      family == "Cyanophyceae familia incertae sedis" ~ "Cyanophyceae ordo incertae sedis",
      family == "Chrysosaccaceae" ~ "Chrysosaccales",
      family == "Hortobagyiella" ~ "Chlorophyceae incertae sedis",
      family == "Lepidochromonadaceae" ~ "Paraphysomonadales",
      family == "Schizotrichaceae" ~ "Leptolyngbyales",
      family == "Tovelliaceae" ~ "Tovelliales",
      family == "Spirulinaceae" ~ "Spirulinales",
      family == "Dinobryaceae" ~ "Chromulinales",
      family == "Euplotidae" ~ "Euplotida",
      family == "Amoebidae" ~ "Euamoebida",
      family == "Astasiidae" ~ "Natomonadida",
      family == "Aphanizomenonaceae" ~ "Nostocales",
      family == "Desmidiaceae" ~ "Desmidiales",
      TRUE ~ order
    ),
    
    class = case_when(
      !(is.na(class)) ~ class,
      order %in% c("Nodosilineales", "Coleofasciculales", "Leptolyngbyales", "Spirulinales", "Nostocales", "Cyanophyceae ordo incertae sedis") ~ "Cyanophyceae",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order %in% c("Naviculales", "Bacillariales") ~ "Bacillariophyceae",
      order == "Stylommatophora" ~ "Gastropoda",
      order == "Chrysomeridales" ~ "Chrysomeridophyceae",
      order == "Bicosoecales" ~ "Bicoecidea",
      order == "Eustigmatales" ~ "Eustigmatophyceae",
      order == "Cryptomonadales" ~ "Cryptophyceae",
      order %in% c("Chromulinales", "Synurales") ~ "Chrysophyceae",
      order == "Desmidiales" ~ "Zygnematophyceae",
      order == "Euamoebida" ~ "Tubulinea",
      order == "Natomonadida" ~ "Peranemea",
      order == "Trebouxiophyceae ordo incertae sedis" ~ "Trebouxiophyceae",
      TRUE ~ class
    ),
    
    phylum = case_when(
      !(is.na(phylum)) ~ phylum,
      class %in% c("Chrysophyceae", "Xanthophyceae", "Bacillariophyceae", "Chrysomeridophyceae", "Eustigmatophyceae") ~ "Heterokontophyta",
      class == "Coccolithophyceae" ~ "Haptophyta",
      class == "Gastropoda" ~ "Mollusca",
      class == "Trebouxiophyceae" ~ "Chlorophyta",
      class == "Dinophyceae" ~ "Dinoflagellata",
      class == "Zygnematophyceae" ~ "Charophyta",
      class == "Tubulinea" ~ "Amoebozoa",
      TRUE ~ phylum
    ),
    
    # Make a rank column
    rank = case_when(
      !(is.na(variety)) ~ "variety",
      is.na(variety) & !(is.na(form)) ~ "form",
      is.na(variety) & is.na(form) & !(is.na(species)) ~ "species",
      is.na(variety) & is.na(form) & is.na(species) & !(is.na(genus)) ~ "genus",
      is.na(variety) & is.na(form) & is.na(species) & is.na(genus) & !(is.na(family)) ~ "family",
      is.na(variety) & is.na(form) & is.na(species) & is.na(genus) & is.na(family) & !(is.na(order)) ~ "order",
      is.na(variety) & is.na(form) & is.na(species) & is.na(genus) & is.na(family) & is.na(order) & !(is.na(class)) ~ "class",
      is.na(variety) & is.na(form) & is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class) & !(is.na(phylum)) ~ "phylum",
      is.na(variety) & is.na(form) & is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class) & is.na(phylum) & !(is.na(kingdom)) ~ "kingdom",
      is.na(variety) & is.na(form) & is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class) & is.na(phylum) & is.na(kingdom) & !(is.na(domain)) ~ "domain",
      TRUE ~ NA
    ),
    
    # update resolved.taxa.name with names from classification
    classification.resolved.taxa.name = case_when(
      rank == "variety" ~ variety,
      rank == "form" ~ form,
      rank == "species" ~ species,
      rank == "genus" ~ genus,
      rank == "family" ~ family,
      rank == "order" ~ order,
      rank == "class" ~ class,
      rank == "phylum" ~ phylum,
      rank == "kingdom" ~ kingdom,
      rank == "domain" ~ domain,
      TRUE ~ resolved.taxa.name
    ),
    
    # make columns for the source of each rank
    variety.source = case_when(
      !(is.na(variety.tol)) ~ "tol",
      is.na(variety.tol) & !(is.na(variety.gbif)) ~ "gbif",
      is.na(variety.tol) & is.na(variety.gbif) & !(is.na(variety)) ~ "manually",
      TRUE ~ NA
    ),
    
    form.source = case_when(
      !(is.na(form.gbif)) ~ "gbif",
      is.na(form.gbif) & !(is.na(form)) ~ "manually",
      TRUE ~ NA
    ),
    
    species.source = case_when(
      !(is.na(species.tol)) ~ "tol",
      is.na(species.tol) & !(is.na(species.gbif)) ~ "gbif",
      is.na(species.tol) & is.na(species.gbif) & !(is.na(species)) ~ "manually",
      TRUE ~ NA
    ),
    
    genus.source = case_when(
      !(is.na(genus.tol)) ~ "tol",
      is.na(genus.tol) & !(is.na(genus.gbif)) ~ "gbif",
      is.na(genus.tol) & is.na(genus.gbif) & !(is.na(genus)) ~ "manually",
      TRUE ~ NA
    ),
    
    family.source = case_when(
      !(is.na(family.tol)) ~ "tol",
      is.na(family.tol) & !(is.na(family.gbif)) ~ "gbif",
      is.na(family.tol) & is.na(family.gbif) & !(is.na(family)) ~ "manually",
      TRUE ~ NA
    ),
    
    order.source = case_when(
      !(is.na(order.tol)) ~ "tol",
      is.na(order.tol) & !(is.na(order.gbif)) ~ "gbif",
      is.na(order.tol) & is.na(order.gbif) & !(is.na(order)) ~ "manually",
      TRUE ~ NA
    ), 
    
    class.source = case_when(
      !(is.na(class.tol)) ~ "tol",
      is.na(class.tol) & !(is.na(class.gbif)) ~ "gbif",
      is.na(order.tol) & is.na(order.gbif) & !(is.na(order)) ~ "manually",
      TRUE ~ NA
    ), 
    
    phylum.source = case_when(
      !(is.na(phylum.tol)) ~ "tol",
      is.na(phylum.tol) & !(is.na(phylum.gbif)) ~ "gbif",
      is.na(phylum.tol) & is.na(phylum.gbif) & !(is.na(phylum)) ~ "manually",
      TRUE ~ NA
    ), 
    
    kingdom.source = case_when(
      !(is.na(kingdom.tol)) ~ "tol",
      is.na(kingdom.tol) & !(is.na(kingdom.gbif)) ~ "gbif",
      is.na(kingdom.tol) & is.na(kingdom.gbif) & !(is.na(kingdom)) ~ "manually",
      TRUE ~ NA
    ), 
    
    domain.source = case_when(
      !(is.na(domain.tol)) ~ "tol",
      is.na(domain.tol) & !(is.na(domain)) ~ "manually",
      TRUE ~ NA
    ),
    
  ) %>% 
  
  select(
    resolved.taxa.name, classification.resolved.taxa.name, rank,
    variety, form, species, genus, family, order, class, phylum, kingdom, domain,
    variety.source, form.source, species.source, genus.source, family.source, order.source, class.source, phylum.source, kingdom.source, domain.source
  ) %>% 
  ungroup()

# Final taxonomy list ----
taxonomy <- tax_all %>% 
  distinct(
    classification.resolved.taxa.name, .keep_all = TRUE
  ) %>% 
  
  mutate(
    tax.uid = row_number()
  ) %>% 
  
  select(-resolved.taxa.name) %>% 
  
  rename(
    resolved.taxa.name = classification.resolved.taxa.name
  ) %>% 
  
  relocate(
    tax.uid, resolved.taxa.name, rank,
    variety, form, species, genus, family, order, class, phylum, kingdom, domain,
    variety.source, form.source, species.source, genus.source, family.source, order.source, class.source, phylum.source, kingdom.source, domain.source
  )

# Save
saveRDS(taxonomy, file = "R/Data_outputs/taxonomy/taxonomy.rds") 

# Add to main data ----
bodysize_taxonomy <- bodysize_joined %>% 
  left_join(
    ., select(
      resolved_names_3_not_classified_fix, original.taxa.name, resolved.taxa.name),
      by = "original.taxa.name"
  ) %>% 
  left_join(
    ., select(
      tax_all, resolved.taxa.name, classification.resolved.taxa.name),
      by = "resolved.taxa.name"
    ) %>% 
  left_join(
    ., select(
      taxonomy, resolved.taxa.name, tax.uid
    ),
    by = "resolved.taxa.name"
  ) %>% 
  
  select(- resolved.taxa.name) %>% 
    
  rename(
    resolved.taxa.name = classification.resolved.taxa.name
  )

# Save
saveRDS(bodysize_taxonomy, file = "R/Data_outputs/full_database/bodysize_taxonomy.rds") 


  
  



