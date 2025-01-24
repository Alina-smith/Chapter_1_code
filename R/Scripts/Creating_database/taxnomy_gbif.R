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

## Final tax list ----
# decided to use just gbif data as it covers the most species and means all species will use the same format

x <- tax_final %>% 
  filter(
    is.na(genus)
  ) 
%>% 
  separate(resolved.taxa.name, into = c("a", "b", "c", "d"), sep = " ")

tax_final <- tax_gbif_2_cleaned %>% 
  
  # don't have infor for all of these so only keep species up
  select(
    -form,
    -variety,
    -subspecies
  ) %>% 
  
  mutate(
    
    species = case_when(
      # resolved wrong
      resolved.taxa.name == "Apodochloris simplicissima" ~ "Apodochloris simplicissima",
      resolved.taxa.name == "Epicystis peridinearum" ~ "Epicystis peridinearum",
      
      # gaps
      !(is.na(species)) ~ species,
      resolved.taxa.name == "Microcystis holsatica" ~ "Aphanocapsa holsatica",
      resolved.taxa.name == "Protococcus wimmeri" ~ "Protococcus wimmeri",
      resolved.taxa.name == "Spirodinium glaucum" ~ "Lebouridinium glaucum",
      resolved.taxa.name %in% c("Scenedesmus bicaudatus", "Scenedesmus bicaudatus var. brevicaudatus") ~ "Desmodesmus bicaudatus",
      resolved.taxa.name == "Chlorotetraëdron bitridens" ~ "Chlorotetraedron bitridens",
      resolved.taxa.name == "Delicata alpestris" ~ "Delicata alpestris",
      resolved.taxa.name == "Chrysopora fenestrata" ~ "Chrysopora fenestrata",
      resolved.taxa.name == "Crucigeniella secta" ~ "Crucigeniella secta",
      
      # when it has been resolved right but has the species missing
      is.na(species) & stri_detect_regex(resolved.taxa.name, " ") & !(stri_detect_regex(resolved.taxa.name, "var\\.|f\\.")) ~ resolved.taxa.name,
      is.na(species) & (stri_detect_regex(resolved.taxa.name, "var\\.|f\\.")) ~ paste0(stri_extract_all_regex(resolved.taxa.name, "\\w+ \\w+")), # when there is a variety then take just the first two words
      
      TRUE ~ species
    ),
    
    genus = case_when(
      # ones that were resolved wrong
      resolved.taxa.name == "Apodochloris simplicissima" ~ "Apodochloris",
      resolved.taxa.name == "Diacanthos" ~ "Micractinium",
      resolved.taxa.name == "Epicystis peridinearum" ~ "Epicystis",
      resolved.taxa.name == "Protococcus wimmeri" ~ "Protococcus",
      resolved.taxa.name == "Delicata alpestris" ~ "Delicata",
      resolved.taxa.name == "Chrysopora fenestrata" ~ "Chrysopora",
      resolved.taxa.name == "Crucigeniella secta" ~ "Crucigeniella",
      resolved.taxa.name == "Sphaerastrum" ~ "Sphaerastrum",
      
      !(is.na(genus)) ~ genus,
      
      # missing gaps - they will just be the first word of the species column
      !(is.na(species)) ~ stri_extract_first_regex(species, "\\w+"),

      
      # rest of the missing 
      TRUE ~ genus
      ),
    
    family = case_when(
      # ones that were resolved wrong
      family == "Cyanobiaceae" ~ "Prochlorococcaceae",
      family == "Spirodiniidae" ~ "Gymnodiniaceae",
      resolved.taxa.name == "Epicystis peridinearum" ~ "Chrysosphaeraceae",
      resolved.taxa.name == "Chrysopora fenestrata" ~ "Chrysocapsaceae",
      resolved.taxa.name == "Crucigeniella secta" ~ "Oocystaceae",
      
      !(is.na(family)) ~ family,
      genus %in% c("Coccomyxa", "Microglena", "Paradoxia") ~ "Coccomyxaceae",
      genus == "Crucigenia" ~ "Trebouxiophyceae incertae sedis",
      genus %in% c("Spondylosium", "Cosmarium", "Staurodesmus", "Teilingia", "Xanthidium", "Staurastrum", "Pleurotaenium", "Euastrum", "Desmidium", "Bambusina",
                   "Micrasterias", "Octacanthium", "Onychonema", "Tetmemorus", "Hyalotheca", "Oocardium", "Docidium", "Haplotaenium", "Sphaerozosma") ~ "Desmidiaceae",
      genus %in% c("Monema", "Biblarium", "Microneis", "Discoplea") ~ "Bacillariophyceae familia incertae sedis",
      genus %in% c("Geminella", "Micractinium", "Chlorella", "Aliichlorella") ~ "Chlorellaceae",
      genus == "Polyedriopsis" ~ "Sphaeropleales incertae sedis",
      genus == "Polychaos" ~ "Euamoebida incertae sedis",
      genus %in% c("Limnomonas", "Ettlia") ~ "Chlamydomonadales familia incertae sedis",
      genus == "Apodochloris" ~ "Chlorococcaceae",
      genus == "Ecballocystis" ~ "Oocystaceae",
      genus %in% c("Kephyriopsis", "Stokesiella") ~ "Dinobryaceae",
      genus == "Pseudochlorangium" ~ "Chlorangiellaceae",
      genus == "Baldinia" ~ "Borghiellaceae",
      genus == "Spirotaenia" ~ "Mesotaeniaceae",
      genus == "Mougeotia" ~ "Zygnemataceae",
      genus == "Jaaginema" ~ "Synechococcales familia incertae sedis",
      genus == "Coenocystis" ~ "Radiococcaceae",
      genus == "Lobocystis" ~ "Chlorophyceae familia incertae sedis",
      genus == "Pleurostauron" ~ "Staurosiraceae",
      genus %in% c("Synuropsis", "Chrysodendron") ~ "Ochromonadaceae",
      genus == "Picochlorum" ~ "Chlorellales incertae sedis",
      genus == "Himantidium" ~ "Eunotiaceae",
      genus %in%c("Amoeba", "Vibrio") ~ "Amoebidae",
      genus == "Gomphonella" ~ "Cymbellales incertae sedis",
      genus == "Chroostipes" ~ "Cyanophyceae familia incertae sedis",
      genus %in% c("Chrysoxys", "Saccochrysis", "Amphichrysis") ~ "Chromulinaceae",
      genus == "Dactylosphaerium" ~ "Dictyosphaeriaceae",
      genus == "Hortobagyiella" ~ "Koliellaceae",
      genus == "Phaeobotrys" ~ "Phaeothamniaceae",
      genus == "Rhaphidiopsis" ~ "Aphanizomenonaceae",
      genus == "Euplotes" ~ "Euplotidae",
      genus == "Diaphanosoma" ~ "Sididae",
      genus == "Lebouridinium" ~ "Gymnodiniales incertae sedis",
      genus %in% c("Rhabdoderma", "Romeria") ~ "Cymatolegaceae",
      genus == "Ulothrix" ~ "Ulotrichaceae",
      genus == "Heterothrix" ~ "Tribonemataceae",
      genus == "Fallacia" ~ "Sellaphoraceae",
      genus == "Nitzschia" ~ "Bacillariaceae",
      genus == "Iconella" ~ "Surirellaceae",
      genus %in% c("Carteria", "Sphaerellopsis") ~ "Chlamydomonadaceae",
      genus == "Schroederia" ~ "Schroederiaceae",
      genus == "Haematococcus" ~ "Haematococcaceae",
      genus == "Kirchneriella" ~ "Selenastraceae",
      genus == "Monodus" ~ "Pleurochloridaceae",
      genus == "Radiococcus" ~ "Radiococcaceae",
      genus == "Rhodomonas" ~ "Pyrenomonadaceae",
      genus == "Schizothrix" ~ "Schizotrichaceae",
      genus == "Sphaerochloris" ~ "Xanthophyceae familia incertae sedis",
      genus == "Spirulina" ~ "Spirulinaceae",
      genus == "Anabaena" ~ "Aphanizomenonaceae",
      genus == "Diplopora" ~ "Diploporaceae",
      genus == "Astasia" ~ "Astasiidae",
      
      TRUE ~ family
    ),
    
    order = case_when(
      # resolved wrong
      order == "Euglypha" ~ "Euglyphida",
      resolved.taxa.name == "Epicystis peridinearum" ~ "Chrysosphaerales",
      resolved.taxa.name == "Protococcus wimmeri" ~ "Chlamydomonadales",
      resolved.taxa.name == "Chrysopora fenestrata" ~ "Chromulinales",
      resolved.taxa.name == "Crucigeniella secta" ~ "Chlorellales",
      
      !(is.na(order)) ~ order,
      family == "Amoebidae" ~ "Euamoebida",
      family == "Amphidiniaceae" ~ "Amphidiniales",
      family == "Bicosoecaceae" ~ "Bicosoecales",
      family == "Chlorellaceae" ~ "Chlorellales",
      family == "Chlorococcaceae" ~ "Chlamydomonadales",
      family == "Chrysosaccaceae" ~ "Chrysosaccales",
      family == "Coccomyxaceae" ~ "Trebouxiophyceae ordo incertae sedis",
      family == "Cymatolegaceae" ~ "Nodosilineales",
      family == "Desmidiaceae" ~ "Desmidiales",
      family == "Dinobryaceae" ~ "Chromulinales",
      family == "Ebriaceae" ~ "Ebriales",
      family == "Eunotiaceae" ~ "Eunotiales",
      family == "Katablepharidaceae" ~ "Katablepharidales",
      family == "Koliellaceae" ~ "Koliellaceae",
      family == "Lepidochromonadaceae" ~ "Paraphysomonadales",
      family %in% c("Mesotaeniaceae", "Zygnemataceae") ~ "Zygnematales",
      family == "Ochromonadaceae" ~ "Ochromonadales",
      family == "Paramastigaceae" ~ "Spironematellales",
      family == "Radiococcaceae" ~ "Sphaeropleales",
      family == "Staurosiraceae" ~ "Fragilariales",
      family == "Wilmottiaceae" ~ "Coleofasciculales",
      family %in% c("Potamididae", "Thiaridae", "Paludomidae") ~ "Caenogastropoda incertae sedis",
      family == "Gnesiocerotidae" ~ "Polycladida",
      family == "Trinematidae" ~ "Euglyphida",
      family == "Neogosseidae" ~ "Chaetonotida",
      family == "Euplotidae" ~ "Euplotida",
      family == "Prochlorococcaceae" ~ "Synechococcales",
      family == "Gymnodiniaceae" ~ "Gymnodiniales",
      family == "Ulotrichaceae" ~ "Ulotrichales",
      family == "Tribonemataceae" ~ "Tribonematales",
      family == "Sellaphoraceae" ~ "Naviculales",
      family == "Bacillariaceae" ~ "Bacillariales",
      family == "Surirellaceae" ~ "Surirellales",
      family %in% c("Chlamydomonadaceae", "Haematococcaceae") ~ "Chlamydomonadales",
      family %in% c("Schroederiaceae", "Selenastraceae") ~ "Sphaeropleales",
      family == "Pleurochloridaceae" ~ "Mischococcales",
      family == "Pyrenomonadaceae" ~ "Pyrenomonadales",
      family == "Schizotrichaceae" ~ "Leptolyngbyales",
      family == "Spirulinaceae" ~ "Spirulinales",
      family == "Aphanizomenonaceae" ~ "Nostocales",
      family == "Diploporaceae" ~ "Dasycladales",
      family == "Astasiidae" ~ "Natomonadida",
       
      family == "Trebouxiophyceae incertae sedis" ~ "Trebouxiophyceae ordo incertae sedis",
      family == "Bacillariophyceae familia incertae sedis" ~ "Bacillariophyceae ordo incertae sedis",
      
      genus == "Polychaos" ~ "Euamoebida",
      genus == "Chroostipes" ~ "Cyanophyceae incertae sedis",
      genus == "Lebouridinium" ~ "Gymnodiniales",
      genus == "Sphaerochloris" ~ "Xanthophyceae ordo incertae sedis",
      
      TRUE ~ order
    ),
    
    class = case_when(
      # resolved wrong 
      class == "Filosia" ~ "Imbricatea",
      resolved.taxa.name == "Epicystis peridinearum" ~ "Chrysophyceae",
      resolved.taxa.name == "Chrysopora fenestrata" ~ "Chrysophyceae",
      
      !(is.na(class)) ~ class,
      order == "Ebriales" ~ "Thecofilosea",
      order %in% c("Nodosilineales", "Coleofasciculales", "Leptolyngbyales", "Spirulinales", "Nostocales") ~ "Cyanophyceae",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order == "Bicosoecales" ~ "Bicoecidea",
      order == "Chromulinales" ~ "Chrysophyceae",
      order == "Desmidiales" ~ "Zygnematophyceae",
      order == "Euamoebida" ~ "Tubulinea",
      order == "Chlorellales" ~ "Trebouxiophyceae",
      order == "Euglyphida" ~ "Imbricatea",
      order == "Gymnodiniales" ~ "Dinophyceae",
      order == "Zygnematales" ~ "Zygnematophyceae",
      order == "Euplotida" ~ "Spirotrichea",
      order == "Cyanophyceae incertae sedis" ~ "Cyanophyceae",
      order == "Ulotrichales" ~ "Ulvophyceae",
      order %in% c("Tribonematales", "Mischococcales") ~ "Xanthophyceae",
      order %in% c("Naviculales", "Bacillariales", "Surirellales") ~ "Bacillariophyceae",
      order %in% c("Chlamydomonadales", "Sphaeropleales") ~ "Chlorophyceae",
      order == "Pyrenomonadales" ~ "Cryptophyceae",
      order == "Dasycladales" ~ "Ulvophyceae",
      order == "Natomonadida" ~ "Peranemea",
      
      family == "Coccomyxaceae" ~ "Trebouxiophyceae",
      genus == "Sphaerochloris" ~ "Xanthophyceae",
      TRUE ~ class
    ),
    
    phylum = case_when(
      # resolved wrong
      resolved.taxa.name %in% c("Epicystis peridinearum", "Chrysopora fenestrata") ~ "Ochrophyta",
      
      !(is.na(phylum)) ~ phylum,
      class == "Zygnematophyceae" ~ "Charophyta",
      class == "Polycystina" ~ "Radiozoa",
      class == "Spironematellophyceae" ~ "Spironematellophyta",
      class == "Dinophyceae" ~ "Dinoflagellata",
      class == "Spirotrichea" ~ "Ciliophora",
      class == "Tubulinea" ~ "Amoebozoa",
      class %in% c("Imbricatea", "Thecofilosea") ~ "Cercozoa",
      class == "Cyanophyceae" ~ "Cyanobacteria",
      class %in% c("Trebouxiophyceae", "Ulvophyceae", "Chlorophyceae") ~ "Chlorophyta",
      class %in% c("Chrysophyceae", "Xanthophyceae", "Bacillariophyceae") ~ "Ochrophyta",
      class == "Cryptophyceae" ~ "Cryptista",
      class == "Peranemea" ~ "Euglenophyta",
      TRUE ~ phylum
    ),
    
    kingdom = case_when(
      !(is.na(kingdom)) ~ kingdom,
      
      # resolved wrong
      resolved.taxa.name == "Chrysopora fenestrata" ~ "Chromista",
      
      # missing
      phylum == "Cyanobacteria" ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Plantae",
      phylum %in% c("Ochrophyta", "Cryptista") ~ "Chromista",
      phylum %in% c("Amoebozoa", "Euglenophyta") ~ "Protozoa",
      
      TRUE ~ kingdom
    )
  )
  
  
x <- tax_final %>% 
  filter(
    is.na(kingdom)
  )

%>% 
  distinct(
    phylum
  )
%>% 
  separate(resolved.taxa.name, into = c(".1", ".2"), sep = " ") %>% 
  filter(
    `.1` != genus
  )
  





resolved.taxa.name == "Chrysodendron ramosum" ~ "Chrysodendron",
stri_detect_regex(resolved.taxa.name, "Cosmarium ") ~ "Cosmarium",
stri_detect_regex(resolved.taxa.name, "Euastrum ") ~ "Euastrum",
stri_detect_regex(resolved.taxa.name, "Kephyriopsis ") ~ "Kephyriopsis",
resolved.taxa.name == "Mougeotia thylespora" ~ "Mougeotia",
resolved.taxa.name == "Pseudochlorangium anomalum" ~ "Pseudochlorangium",
resolved.taxa.name == "Spirodinium glaucum" ~ "Lebouridinium",
stri_detect_regex(resolved.taxa.name, "Xanthidium ") ~ "Xanthidium",










