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
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)

# Import data ----
bodysize_raw <- readRDS("R/data_outputs/final_products/bodysize_raw.rds")

# Resolve names ----

## gna ----
# run through gna_verifier to fix any spelling

# get a list of all distinct names 
distinct_names <- select(bodysize_raw, original.taxa.name) %>% 
  distinct(original.taxa.name) 

# convert to a string of names
names_list <- paste0(distinct_names$original.taxa.name)
glimpse(names_list)

# run string through the resolver and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
resolved_names_1gbif_gna <- do.call(rbind, lapply(names_list, function(name) {
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
saveRDS(resolved_names_1gbif_gna, file = "R/data_outputs/taxonomy/gbif/resolved_names_1gbif_gna.rds")

## Manual ----
# How manual resolving was carried out:
# When the species name could be found then use that
# When the species name couldn't be found on a database then keep the original.taxa.name in case it is a newly discovered species not in the databases yet
# When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
# When two species are stated then the closet common higher group is used

# 1) Find all the taxa.names from resolved_gna to manually resolve based on the criteria in the comments
to_resolve_manually <- resolved_names_1gbif_gna %>% 
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
write_csv(to_resolve_manually, "R/data_outputs/taxonomy/to_resolve_manually.csv")

# 2) Update resolved.taxa.names with the manually resolved names

# Import the manually resolved names
manually_resolved <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "resolve")

# Replace resolved.taxa.name with manually resolved names when ones is present
resolved_names_2gbif_manual <- resolved_names_1gbif_gna %>% 
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
saveRDS(resolved_names_2gbif_manual, file = "R/data_outputs/taxonomy/gbif/resolved_names_2gbif_manual.rds")
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Taxonomy ----

## Classification: Initial run ----
# Run through classification() with gbif

# Initial run through classification
tax_1gbif_raw <- resolved_names_2gbif_manual %>% 
  
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
saveRDS(tax_1gbif_raw, file = "R/data_outputs/taxonomy/gbif/tax_1gbif_raw.rds")

## Classification: Initial clean ----
tax_1gbif_cleaned <- tax_1gbif_raw %>% 
  
  # There were no repeated columns so just remove the .1 from all of them
  rename_with(~ stri_replace_all_regex(., "\\.1$", "")) %>% 
  
  # Select columns
  select(
    resolved.taxa.name, variety, form, subspecies, species, genus, family, order, class, phylum, kingdom
  )

# Save
saveRDS(tax_1gbif_cleaned, file = "R/data_outputs/taxonomy/gbif/tax_1gbif_cleaned.rds")

## Classification: rerun ----

# Find all taxa that were bumped up a taxanomic group or have missing ranks and rerun with rows = 1 off to manaully chose them
tax_2gbif_rerun_raw <- tax_gbif_1_cleaned %>% 
  
  mutate(
    
    classified = if_else(
      apply( # apply to each column
        select(
          .,-resolved.taxa.name, # select all columns apart from this as we only want to check the columns gotten from classification()
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
saveRDS(tax_2gbif_rerun_raw, file = "R/data_outputs/taxonomy/gbif/tax_2gbif_rerun_raw.rds")

## Classification: Clean rerun ----

# Clean tax_gbif_2_bumped
tax_2gbif_rerun_cleaned <- tax_2gbif_rerun_raw %>% 
  
  rename_with(~ stri_replace_all_regex(., "\\.1$", "")) %>% 
  
  select(
    resolved.taxa.name, variety, form, subspecies, species, genus, family, order, class, phylum, kingdom
  )

# add in tax_2gbif_rerun_cleaned to full list
tax_2gbif_cleaned <- tax_1gbif_cleaned %>% 
  
  filter(
    !(resolved.taxa.name %in% tax_2gbif_rerun_cleaned$resolved.taxa.name)
  )%>% 
  
  bind_rows(., tax_2gbif_rerun_cleaned) 

# Save
saveRDS(tax_2gbif_cleaned, file = "R/data_outputs/taxonomy/gbif/tax_2gbif_cleaned.rds")

## Final tax list ----
# fill in missing gaps and taxa that have been resolved wrong

tax_manual_changes <- tax_2gbif_cleaned %>% 
  
  ungroup() %>% 
  # don't have info for all of these so only keep species up
  select(
    -form,
    -variety,
    -subspecies
  ) %>% 
  
  mutate(
    
    # Species:
    species = case_when(
      
      # 1) keep blank because couldn't find the species anywhere
      resolved.taxa.name %in% c("Unapertura latens", "Fallacia difficillimoides", "Coccomyxa lunaris", "Microcystis tenuissima", "Diplopora oregonensis", "Pica cyana", "Heterothrix tenuissima") ~ NA,
      
      # 1) Were assigned wrong
      resolved.taxa.name %in% c("Lemmermannia cf. komarekii", "Lemmermannia komarekii") ~ "Lemmermannia komarekii",
      resolved.taxa.name == "Lemmermannia tetrapedia" ~ "Lemmermannia tetrapedia",
      resolved.taxa.name == "Tetrastrum triangulare" ~ "Lemmermannia triangularis",
      resolved.taxa.name == "Echinopus minutus" ~ "Echinopus minutus",
      resolved.taxa.name == "Chlorobium saxatile" ~ "Monoraphidium saxatile",
      resolved.taxa.name == "Chlorolobium braunii" ~ "Chlorolobion braunii",
      resolved.taxa.name == "Monoraphidium arcuatum" ~ "Ankistrodesmus arcuatus",
      resolved.taxa.name == "Euglena rostrata" ~ "Euglena rostrata",
      
      # 2) Gaps:
      # These ones have a more updated name than the resolved.taxa.name so need changing
      resolved.taxa.name == "cf. Katodinium fungiforme" ~ "Speroidium fungiforme",
      resolved.taxa.name == "Haematococcus buetschlii" ~ "Balticola buetschlii",
      resolved.taxa.name == "Rhodomonas ovalis" ~ "Pyrenomonas ovalis",
      resolved.taxa.name == "Coccomyxa lacustris" ~ "Planktococcomyxa lacustris",
      resolved.taxa.name == "Scenedesmus bicaudatus var. brevicaudatus" ~ "Desmodesmus bicaudatus",
      resolved.taxa.name == "Chlorotetraëdron bitridens" ~ "Chlorotetraedron bitridens",
      resolved.taxa.name == "Delicata alpestris" ~ "Delicatophycus alpestris",
      resolved.taxa.name == "Microcystis holsatica" ~ "Aphanocapsa holsatica",
      resolved.taxa.name == "Coenocystis quadriguloides" ~ "Palmococcus quadriguloides",
      resolved.taxa.name == "Spirodinium glaucum" ~ "Lebouridinium glaucum",
      resolved.taxa.name == "Crucigenia rectangularis" ~ "Willea rectangularis",
      resolved.taxa.name %in% c("Crucigenia tetrapedia", "Lemmermannia tetrapedia") ~ "Lemmermannia tetrapedia",
      resolved.taxa.name %in% c("Bacillaria paxillifer", "Bacillaria paradoxa", "Bacillaria paxillifera") ~ "Bacillaria paxillifera",
      
      
      # These have a gap but the species exists with that as the most up to date name so just use resolved.taxa.name
      is.na(species) & stri_detect_regex(resolved.taxa.name, "aff\\.") ~ stri_replace_all_regex(resolved.taxa.name, "aff\\. ", ""), # remove cf. when its present
      is.na(species) & stri_detect_regex(resolved.taxa.name, "cf\\.") ~ stri_replace_all_regex(resolved.taxa.name, "cf\\. ", ""), # remove cf. when its present
      is.na(species) & stri_detect_regex(resolved.taxa.name, " ") & !(stri_detect_regex(resolved.taxa.name, "var\\.|f\\.")) ~ resolved.taxa.name,
      is.na(species) & (stri_detect_regex(resolved.taxa.name, "var\\.|f\\.")) ~ paste0(stri_extract_all_regex(resolved.taxa.name, "\\w+ \\w+")), # when there is a variety then take just the first two words
      
      TRUE ~ species
    ),
    
    # Genus
    genus = case_when(
      # Not a species
      resolved.taxa.name %in% c("Pica cyana", "Pennate", "Unapertura latens") ~ NA,
      
      # 1) Assigned wrong
      resolved.taxa.name %in% c("Sphaerastrum fockii", "Epicystis peridinearum", "Protococcus wimmeri") ~ stri_extract_first_regex(resolved.taxa.name, "\\w+"),
      resolved.taxa.name == "Diacanthos" ~ "Micractinium",
      genus == "Apodocloris" ~ "Apodochloris",
      resolved.taxa.name %in% c("Chrysopora", "Chrysopora fenestrata") ~ "Chrysopora",
      species %in% c("Crucigeniella lunaris", "Crucigeniella secta") ~ "Crucigeniella",
      resolved.taxa.name == "Crucigeniella" ~ "Crucigeniella",
      species %in% c("Lemmermannia punctata", "Lemmermannia triangularis", "Lemmermannia komarekii", "Lemmermannia tetrapedia") ~ "Lemmermannia",
      genus == "Pennellia" ~ "Xanthonema",
      resolved.taxa.name %in% c("Echinopus minutus", "Echinopus minutus", "Echinopus") ~ "Echinopus",
      resolved.taxa.name == "Rhaphidiopsis" ~ "Raphidiopsis",
      species == "Monoraphidium saxatile" ~ "Monoraphidium",
      species == "Chlorolobion braunii" ~ "Chlorolobion",
      species == "Willea rectangularis" ~ "Willea",
      species == "Bacillaria paxillifera" ~ "Bacillaria",
      resolved.taxa.name == "Chrysodendron" ~ "Chrysodendron",
      species == "Euglena rostrata" ~ "Euglena",
      
      # 2) Gaps
      # Genus level
      resolved.taxa.name == "Dinoflagelado/peridinium" ~ "Peridinium",
      resolved.taxa.name == "Heterothrix tenuissima" ~ "Xanthonema",
      
      # species level - Checked for names and species are up to date so just need to take the first word of the species column
      is.na(genus) & !is.na(species) ~ stri_extract_first_regex(species, "\\w+"),
      is.na(genus) & stri_detect_regex(resolved.taxa.name, " ") ~ stri_extract_first_regex(resolved.taxa.name, "\\w+"),

      TRUE ~ genus
    ),
    
    # Family
    family = case_when(
      # Not a sepcies
      resolved.taxa.name == "Unapertura latens" ~ NA,
      
      # Gaps
      genus %in% c("Spondylosium", "Cosmarium", "Staurodesmus", "Teilingia", "Xanthidium", "Staurastrum", "Hyalotheca", "Euastrum", "Pleurotaenium", "Desmidium", "Bambusina",
                   "Micrasterias","Octacanthium", "Onychonema", "Tetmemorus", "Oocardium", "Docidium", "Haplotaenium", "Sphaerozosma") ~ "Desmidiaceae",
      genus %in% c("Crucigeniella", "Ecballocystis", "Willea") ~ "Oocystaceae",
      genus %in% c("Stokesiella", "Kephyriopsis") ~ "Dinobryaceae",
      genus %in%c("Vibrio", "Amoeba", "Polychaos") ~ "Amoebidae",
      genus %in% c("Biblarium", "Microneis", "Discoplea", "Monema") ~ "Bacillariophyceae familia incertae sedis",
      genus %in% c("Synuropsis", "Chrysodendron") ~ "Ochromonadaceae",
      genus %in% c("Limnomonas", "Ettlia") ~ "Chlamydomonadales familia incertae sedis",
      genus %in% c("Carteria", "Sphaerellopsis", "Microglena") ~ "Chlamydomonadaceae",
      genus %in% c("Chrysoxys", "Saccochrysis", "Amphichrysis") ~ "Chromulinaceae",
      genus %in% c("Geminella", "Chlorella", "Aliichlorella", "Micractinium") ~ "Chlorellaceae",
      genus %in% c("Coccomyxa", "Microglena", "Paradoxia") ~ "Coccomyxaceae",
      genus %in% c("Raphidiopsis", "Anabaena") ~ "Aphanizomenonaceae",
      genus %in% c("Lemmermannia", "Crucigenia") ~ "Trebouxiophyceae incertae sedis",
      genus %in% c("Coenocystis", "Palmococcus", "Radiococcus", "Sphaerochloris") ~ "Radiococcaceae",
      genus %in% c("Monoraphidium", "Chlorolobion", "Kirchneriella", "Planktococcomyxa") ~ "Selenastraceae",
      
      genus %in% c("Rhabdoderma", "Romeria") ~ "Cymatolegaceae",
      genus == "Ulothrix" ~ "Ulotrichaceae",
      genus == "Jaaginema" ~ "Synechococcales familia incertae sedis",
      genus == "Speroidium" ~ "Pfiesteriaceae",
      genus == "Lobocystis" ~ "Chlorophyceae familia incertae sedis",
      genus == "Pleurostauron" ~ "Staurosiraceae",
      genus == "Polyedriopsis" ~ "Sphaeropleales incertae sedis",
      genus %in% c("Xanthonema", "Brachynematella") ~ "Tribonemataceae",
      genus == "Picochlorum" ~ "Chlorellales incertae sedis",
      genus == "Echinopus" ~ "Baetidae",
      genus == "Fallacia" ~ "Sellaphoraceae",
      genus == "Gomphonella" ~ "Cymbellales incertae sedis",
      genus %in% c("Nitzschia", "Bacillaria") ~ "Bacillariaceae",
      genus == "Iconella" ~ "Surirellaceae",
      genus == "Trichodina" ~ "Trichodinidae",
      genus == "Schroederia" ~ "Schroederiaceae",
      genus == "Apodochloris" ~ "Chlorococcaceae",
      genus == "Chroostipes" ~ "Cyanophyceae familia incertae sedis",
      genus == "Dactylosphaerium" ~ "Dactylopodida incertae sedis",
      genus == "Baldinia" ~ "Borghiellaceae",
      genus == "Balticola" ~ "Haematococcaceae",
      genus == "Hortobagyiella" ~ "Koliellaceae",
      genus == "Monodus" ~ "Pleurochloridaceae",
      genus == "Phaeobotrys" ~ "Phaeothamniaceae",
      genus == "Pseudochlorangium" ~ "Chlorangiellaceae",
      genus == "Pyrenomonas" ~ "Pyrenomonadaceae",
      genus == "Schizothrix" ~ "Trichocoleusaceae",
      genus == "Lebouridinium" ~ "Gymnodiniales incertae sedis",
      genus == "Spirotaenia" ~ "Mesotaeniaceae",
      genus == "Spirulina" ~ "Spirulinaceae",
      genus == "Euplotes" ~ "Euplotidae",
      genus == "Mougeotia" ~ "Zygnemataceae",
      genus == "Diplopora" ~ "Diploporaceae",
      genus == "Diaphanosoma" ~ "Sididae",
      genus == "Astasia" ~ "Astasiidae",
      genus == "Microcystis" ~ "Microcystaceae",
      genus == "Peridinium" ~ "Peridiniaceae",
      genus == "Cyanobium" ~ "Prochlorococcaceae",
      
      genus %in% c("Gloeocapsa", "Gloeocapsopsis") ~ "Aliterellaceae",
      genus == "Geitlerinema" ~ "Geitlerinemataceae",
      genus == "Sphaerocystis" ~ "Sphaerocystidaceae",
      genus == "Chrysopora" ~ "Chrysocapsaceae",
      genus == "Nais" ~ "Naididae",
      genus == "Raciborskiella" ~ "Wislouchiaceae",
      genus == "Pompholyx" ~ "Testudinellidae",
      genus == "Actinocyclus" ~ "Hemidiscaceae",
      genus == "Epicystis" ~ "Chrysosphaeraceae",
      genus == "Cryptoglena" ~ "Euglenaceae",
      genus == "Euglena" ~ "Euglenaceae",
      genus == "Chaetophora" ~ "Chaetophoraceae",
      genus == "Sida" ~ "Sididae",
      genus == "Brachynema" ~ "Brachynematella",
      genus == "Opercularia" ~ "Operculariidae",
      genus == "Chrysodendron" ~ "Ochromonadales",
      genus == "Ceratium" ~ "Ceratiaceae",
      genus == "Asterococcus" ~ "Palmellopsidaceae",
      genus == "Xenococcus" ~ "Pleurocapsaceae",
      genus == "Nodularia" ~ "Nodulariaceae",
      genus == "Didymocystis" ~ "Oocystaceae",
      genus == "Acanthosphaera" ~ "Chlorellaceae",
      
      family == "Spirodinium" ~ "Gymnodiniaceae",
      family == "Microcystaceae_A" ~ "Microcystaceae",

      TRUE ~ family
    ),
    
    order = case_when(
      
      # not a species
      resolved.taxa.name %in% c("Unapertura latens") ~ NA,
      
      # Gaps
      family %in% c("Chlorellaceae", "Oocystaceae") ~ "Chlorellales",
      family %in% c("Potamididae", "Thiaridae", "Paludomidae") ~ "Caenogastropoda incertae sedis",
      family %in% c("Chlamydomonadaceae", "Haematococcaceae", "Chlorococcaceae", "Chlorangiellaceae", "Wislouchiaceae") ~ "Chlamydomonadales",
      family %in% c("Schroederiaceae", "Selenastraceae", "Radiococcaceae") ~ "Sphaeropleales",
      family %in% c("Mesotaeniaceae", "Zygnemataceae") ~ "Zygnematales",
      
      family == "Cymatolegaceae" ~ "Nodosilineales", 
      family == "Wilmottiaceae" ~ "Coleofasciculales",
      family == "Prochlorococcaceae" ~ "Synechococcales",
      family == "Ulotrichaceae" ~ "Ulotrichales",
      family == "Desmidiaceae" ~ "Desmidiales",
      family == "Bicosoecaceae" ~ "Bicosoecales",
      family == "Pfiesteriaceae" ~ "Thoracosphaerales",
      family == "Coccomyxaceae" ~ "Trebouxiophyceae ordo incertae sedis",
      family == "Dinobryaceae" ~ "Chromulinales",
      family == "Amphidiniaceae" ~ "Amphidiniales",
      family == "Amoebidae" ~ "Euamoebida",
      family == "Katablepharidaceae" ~ "Katablepharidales",
      family == "Staurosiraceae" ~ "Fragilariales",
      family == "Ebriaceae" ~ "Ebriales",
      family == "Paramastigaceae" ~ "Spironematellales",
      family == "Tribonemataceae" ~ "Tribonematales",
      family == "Gnesiocerotidae" ~ "Polycladida",
      family == "Trinematidae" ~ "Euglyphida",
      family == "Sellaphoraceae" ~ "Naviculales",
      family == "Bacillariaceae" ~ "Bacillariales",
      family == "Surirellaceae" ~ "Surirellales",
      family == "Neogosseidae" ~ "Chaetonotida",
      family == "Ochromonadaceae" ~ "Ochromonadales",
      family == "Chrysosaccaceae" ~ "Chrysosaccales",
      family == "Koliellaceae" ~ "Prasiolales",
      family == "Lepidochromonadaceae" ~ "Paraphysomonadales",
      family == "Pleurochloridaceae" ~ "Mischococcales",
      family == "Pyrenomonadaceae" ~ "Pyrenomonadales",
      family == "Spirulinaceae" ~ "Spirulinales",
      family == "Euplotidae" ~ "Euplotida",
      family %in% c("Aphanizomenonaceae", "Nostocaceae", "Nodulariaceae", "Hapalosiphonaceae", "Scytonemataceae", "Rivulariaceae", "Tolypothrichaceae", "Leptobasaceae", "Stigonemataceae") ~ "Nostocales",
      family == "Diploporaceae" ~ "Dasycladales",
      family == "Astasiidae" ~ "Natomonadida",
      family == "Trichodinidae" ~ "Mobilida",
      family == "Trichocoleusaceae" ~ "Leptolyngbyales",
      family == "Gymnodiniaceae" ~ "Gymnodiniales",
      family == "Peridiniaceae" ~ "Peridiniales",
      family == "Thermostichaceae" ~ "Thermostichales",
      family == "Baetidae" ~ "Ephemeroptera",
      
      
      # Assigned wrong
      family %in% c("Microcystaceae", "Gomphosphaeriaceae", "Cyanobacteriaceae", "Chroococcaceae", "Cyanothrichaceae", "Pleurocapsaceae", "Geminocystaceae") ~ "Chroococcales",
      family %in% c("Chamaesiphonaceae", "Gomontiellaceae") ~ "Gomontiellales",
      family %in% c("Microcoleaceae", "Oscillatoriaceae", "Phormidiaceae", "Aerosakkonemataceae") ~ "Oscillatoriales",
      family %in% c("Xenococcaceae", "Hyellaceae", "Dermocarpellaceae") ~ "Pleurocapsales",
      family == "Coleofasciculaceae" ~ "Coleofasciculales",
      family == "Desertifilaceae" ~ "Desertifilales",
      family %in% c("Chroococcidiopsidaceae", "Aliterellaceae") ~ "Chroococcidiopsidales",
      family == "Geitlerinemataceae" ~ "Geitlerinematales",
      resolved.taxa.name == "Chroococcales" ~ "Chroococcales",
      resolved.taxa.name == "Oscillatoriales" ~ "Oscillatoriales",
      family == "Sphaerocystidaceae" ~ "Chlamydomonadales",
      family == "Euglyphidae" ~ "Euglyphida",
      family %in% c("Chrysocapsaceae", "Chromulinaceae") ~ "Chromulinales",
      genus == "Lobocystis" ~ "Chlorophyceae ordo incertae sedis",
      family == "Borghiellaceae" ~ "Suessiales",
      family == "Phaeothamniaceae" ~ "Phaeothamniales",
      family == "Naididae" ~ "Tubificida",
      family == "Testudinellidae" ~ "Flosculariaceae",
      family == "Pelonemataceae" ~ "Pelonematales",
      family == "Hemidiscaceae" ~ "Coscinodiscales",
      family == "Chrysosphaeraceae" ~ "Chrysosphaerales",
      family == "Euglenaceae" ~ "Euglenales",
      family == "Chaetophoraceae" ~ "Chaetophorales",
      family == "Sididae" ~ "Ctenopoda",
      family == "Operculariidae" ~ "Sessilida",
      family == "Ceratiaceae" ~ "Gonyaulacales",
      family == "Palmellopsidaceae" ~ "Chlamydomonadales",
      family == "Hexamitidae" ~ "Diplomonadida",
      
      genus %in% c("Lemmermannia", "Crucigenia") ~ "Trebouxiophyceae ordo incertae sedis",
      genus %in% c("Biblarium", "Microneis", "Discoplea", "Monema") ~ "Bacillariophyceae ordo incertae sedis",
      genus == "Chroostipes" ~ "Cyanophyceae ordo incertae sedis",
      genus == "Lebouridinium" ~ "Gymnodiniales",
      
      TRUE ~ order
    ),
    
    class = case_when(
      
      # not species
      resolved.taxa.name %in% c("Unapertura latens") ~ NA,
      
      # gaps
      order %in% c("Nodosilineales", "Coleofasciculales", "Leptolyngbyales", "Spirulinales", "Nostocales", "Chroococcales", "Thermostichales", "Cyanophyceae ordo incertae sedis", "Pelonematales") ~ "Cyanophyceae",
      order %in% c("Tribonematales", "Mischococcales") ~ "Xanthophyceae",
      order %in% c("Naviculales", "Bacillariales", "Surirellales") ~ "Bacillariophyceae",
      order %in% c("Chlamydomonadales", "Sphaeropleales", "Chaetophorales") ~ "Chlorophyceae",
      order %in% c("Gymnodiniales", "Peridiniales", "Thoracosphaerales", "Gonyaulacales") ~ "Dinophyceae",
      
      order %in% c("Ulotrichales", "Dasycladales") ~ "Ulvophyceae",
      order %in% c("Desmidiales", "Zygnematales") ~ "Zygnematophyceae",
      order == "Bicosoecales" ~ "Bicosoecophyceae",
      order %in% c("Chlorellales", "Trebouxiophyceae ordo incertae sedis") ~ "Trebouxiophyceae",
      order == "Chromulinales" ~ "Chrysophyceae",
      order == "Ebriales" ~ "Thecofilosea",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order == "Euglyphida" ~ "Imbricatea",
      order == "Euamoebida" ~ "Tubulinea",
      order == "Pyrenomonadales" ~ "Cryptophyceae",
      order == "Natomonadida" ~ "Peranemea",
      order == "Euplotida" ~ "Spirotrichea",
      order == "Mobilida" ~ "Oligohymenophorea",
      order == "Ephemeroptera" ~ "Hexapoda",
      order == "Synechococcales" ~ "Cyanophyceae",
      order == "Euglenales" ~ "Euglenophyceae",
      
      # Assigned wrong
      class == "Cyanobacteriia" ~ "Cyanophyceae",
      order == "Phaeothamniales" ~ "Phaeophyceae",
      order %in% c("Ochromonadales", "Chrysosphaerales") ~ "Chrysophyceae",
      order == "Prasiolales" ~ "Trebouxiophyceae",
      order == "Tubificida" ~ "Clitellata",
      order == "Flosculariaceae" ~ "Eurotatoria",
      order == "Coscinodiscales" ~ "Bacillariophyceae",
      order == "Ctenopoda" ~ "Branchiopoda",
      order == "Sessilida" ~ "Oligohymenophorea",
      class == "Gymnostomatea" ~ "Litostomatea",
      class == "Hypotrichea" ~ "Spirotrichea",
      class == "Insecta" ~ "Hexapoda",
      class == "Lobosa" ~ "Tubulinea",
      class == "Prymnesiophyceae" ~ "Coccolithophyceae",
      class == "Zygnematophyceae" ~ "Conjugatophyceae",
      order == "Diplomonadida" ~ "Eopharyngea",
      
      TRUE ~ class
    ),
    
    phylum = case_when(
      
      # not species
      resolved.taxa.name %in% c("Unapertura latens", "Pica cyana") ~ NA,
      
      
      class == "Cyanophyceae" ~ "Cyanobacteria",
      class == "Zygnematophyceae" ~ "Charophyta",
      class == "Dinophyceae" ~ "Myzozoa",
      class %in% c("Chrysophyceae", "Xanthophyceae", "Bacillariophyceae") ~ "Ochrophyta",
      class == "Polycystina" ~ "Radiozoa",
      class %in% c("Thecofilosea", "Imbricatea") ~ "Cercozoa",
      
      class == "Spironematellophyceae" ~ "Spironematellophyta",
      class %in% c("Hexapoda", "Branchiopoda") ~ "Arthropoda",
      class == "Tubulinea" ~ "Amoebozoa",
      class %in% c("Oligohymenophorea", "Spirotrichea", "Oligohymenophorea") ~ "Ciliophora",
      class == "Cryptophyceae" ~ "Cryptophyta",
      class %in% c("Peranemea", "Euglenophyceae") ~ "Euglenozoa",
      class %in% c("Chlorophyceae", "Ulvophyceae", "Trebouxiophyceae") ~ "Chlorophyta",
      class == "Clitellata" ~ "Annelida",
      class == "Eurotatoria" ~ "Rotifera",
      class == "Eopharyngea" ~ "Loukozoa",
      
      TRUE ~ phylum
    ),
    
    kingdom = case_when(
      
      # not species
      resolved.taxa.name %in% c("Pennate", "Unapertura latens", "Pica cyana") ~ NA,
      
      # gaps
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Plantae",
      phylum %in% c("Cercozoa", "Myzozoa", "Ochrophyta", "Ciliophora", "Cryptophyta") ~ "Chromista",
      phylum == "Cyanobacteria" ~ "Bacteria",
      phylum %in% c("Annelida", "Arthropoda", "Rotifera") ~ "Animalia",
      phylum %in% c("Amoebozoa", "Euglenozoa", "Loukozoa") ~ "Protozoa",
      
      TRUE ~ kingdom
    ),
    
    # make a rank column
    rank = case_when(
      !(is.na(species)) ~ "Species",
      is.na(species) & !(is.na(genus)) ~ "Genus",
      is.na(species) & is.na(genus) & !(is.na(family)) ~ "Family",
      is.na(species) & is.na(genus) & is.na(family) & !(is.na(order)) ~ "Order",
      is.na(species) & is.na(genus) & is.na(family) & is.na(order) & !(is.na(class)) ~ "Class",
      is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class) & !(is.na(phylum)) ~ "Phylum",
      is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class) & is.na(phylum) & !(is.na(kingdom)) ~ "Kingdom",
      TRUE ~ NA
    ),
    
    # make another column with the most updtaed names
    taxa.name = case_when(
      rank == "Species" ~ species,
      rank == "Genus" ~ genus,
      rank == "Family" ~ family,
      rank == "Order" ~ order,
      rank == "Class" ~ class,
      rank == "Phylum" ~ phylum,
      rank == "Kingdom" ~ kingdom,
      TRUE ~ "No rank"
    )
  ) %>% 
  
  # Filter out NAs and things that aren't phyto or zoo
  
  filter(
    kingdom != "Fungi",
    !(is.na(kingdom)),
    !(phylum %in% c("Tracheophyta", "Proteobacteria", "Actinobacteriota", "Microsporidia"))
  ) %>% 
  
  mutate(
    # Make a column for is they are phyto or zoo
    type = case_when(
      kingdom == "Animalia" ~ "Zooplankton",
      phylum %in% c("Amoebozoa", "Radiozoa", "Cercozoa") ~ "Zooplankton",
      TRUE ~ "Phytoplankton"
    )
  ) %>% 
  relocate(
    resolved.taxa.name, taxa.name, rank, type, species, genus, family, order, class, phylum, kingdom
  )

# Add in a tax.uid column 
z <- tax_manual_changes %>% 
  filter(
    type == "Zooplankton"
  ) %>% 
  
  group_by(taxa.name) %>% 
  
  mutate(
    tax.uid = paste0("Z", cur_group_id())
  )

p <- tax_manual_changes %>% 
  filter(
    type == "Phytoplankton"
  ) %>% 
  
  group_by(taxa.name) %>% 
  
  mutate(
    tax.uid = paste0("P", cur_group_id())
  )

tax_list_multiples <- bind_rows(z, p)

saveRDS(tax_list_multiples, file = "R/data_outputs/taxonomy/gbif/tax_list_multiples.rds")

# Add to main data ----

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

saveRDS(bodysize_taxonomy, file = "R/data_outputs/full_database/bodysize_taxonomy.rds")

# Make a distinct tax list
tax_list_distinct <- tax_list_multiples %>% 
  distinct(
    tax.uid, .keep_all = TRUE
  ) %>% 
  select(
    - resolved.taxa.name
  )

# save
saveRDS(tax_list_distinct, file = "R/data_outputs/taxonomy/gbif/tax_list_distinct.rds")




