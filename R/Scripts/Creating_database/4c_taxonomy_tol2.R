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

# Import data ----
bodysize_raw <- readRDS("R/data_outputs/final_products/bodysize_raw.rds")

# Clean names ----

## gna ----
# run through gna_verifier to fix any spelling

# get a list of all distinct names 
distinct_names <- select(bodysize_raw, original.taxa.name) %>% 
  distinct(original.taxa.name) 

# convert to a string of names
names_list <- paste0(distinct_names$original.taxa.name)
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
    resolved.taxa.name.gna = matchedCanonicalFull,
    resolved.source.gna = dataSourceTitleShort
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
      is.na(resolved.taxa.name.gna) ~ "na",
      
      # ones with cf. as the resolver removes this and need to keep it
      stri_detect_regex(original.taxa.name, " cf\\.| cf ") ~ "cf",
      
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
write_csv(to_clean_manually, "R/data_outputs/taxonomy/tol2/to_clean_manually.csv")

# 2) Update resolved.taxa.names with the manually resolved names

# Import the manually resolved names
manually_cleaned <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "resolve")

# Replace resolved.taxa.name with manually resolved names when ones is present
cleaned_manual <- cleaned_gna %>% 
  # left join all the manually resolved ones from manual_resolve spreadsheet
  left_join(
    manually_cleaned,
    by = "original.taxa.name"
  ) %>% 
  # when a name has been cleaned manually (resolved source = "manaully") then select that name else keep the current one
  mutate(
    resolved.taxa.name = if_else(
      !is.na(resolved.source.manual),
      resolved.taxa.name.manual,
      resolved.taxa.name.gna
    )
  ) %>% 
  
  select(
    original.taxa.name,
    resolved.taxa.name
  ) %>% 
  
  filter(
    !(is.na(resolved.taxa.name))
  )

# Save
saveRDS(cleaned_manual, file = "R/data_outputs/taxonomy/tol2/cleaned_manual.rds")
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Resolve names ----
## TOL ----
# run the cleaned names through the resolver to get updated versions of names

# remove form and variety as majority don't have this and makes the taxonomy steps more complex
cleaned <- cleaned_manual %>% 
  
  mutate(
    resolved.taxa.name = stri_replace_all_regex(resolved.taxa.name, "cf\\.|f\\.|var\\.", " "),
    resolved.taxa.name = stri_replace_all_regex(resolved.taxa.name, "  ", "")
  ) %>% 
  
  separate(resolved.taxa.name, into = c("genus", "species", "c", "d", "e"), sep = " ") %>% 
  
  mutate(
    resolved.taxa.name.new = stri_c(genus, species, sep = " "),
    resolved.taxa.name.new = if_else(
      is.na(resolved.taxa.name.new),
      genus,
      resolved.taxa.name.new
    )
  )

# Save
saveRDS(cleaned, file = "R/data_outputs/taxonomy/tol2/cleaned.rds")

# run through tnrs_match_names to get the most up to date synonyms
resolved_tol <- tnrs_match_names(unique(cleaned$resolved.taxa.name))

# Save
saveRDS(resolved_tol, file = "R/data_outputs/taxonomy/tol2/resolved_tol.rds")

## Manual ----
# Select all the ones that weren't picked up by tol and manually resolve their names

to_resolve_manually <- resolved_tol %>% 
  
  filter(
    is.na(unique_name)
  )

# Save
write_csv(to_resolve_manually, "R/data_outputs/taxonomy/tol2/to_resolve_manually.csv")

# Add in the manually resolved names to the full name list













