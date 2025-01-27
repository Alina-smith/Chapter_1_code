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

## Classification: GBIF - bumped up rerun ----

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

## Classification: GBIF - Clean and add into full list ----

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
# decided to use just gbif data as it covers the most species and means all species will use the same format

tax_final <- tax_gbif_2_cleaned %>% 
  
  ungroup() %>% 
  # don't have infor for all of these so only keep species up
  select(
    -form,
    -variety,
    -subspecies
  ) %>% 
  
  mutate(
    
    tax.uid = row_number(),
    
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
      genus %in% c("Epicystis", "Chrysopora") ~ "Chrysosphaeraceae",
      genus == "Nais" ~ "Cryptomonadaceae",
      genus == "Raciborskiella" ~ "Wislouchiaceae",
      genus == "Pompholyx" ~ "Testudinellidae",
      
      # missing ones
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
      genus %in% c("Ecballocystis", "Crucigeniella") ~ "Oocystaceae",
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
      family == "Euglyphidae" ~ "	Euglyphida",
      family == "Chrysosphaeraceae" ~ "Chrysosphaerales",
      family == "Testudinellidae" ~ "Flosculariaceae",
      family == "Amoebidae" ~ "Euamoebida",
      family == "Amphidiniaceae" ~ "Amphidiniales",
      family == "Bicosoecaceae" ~ "Bicosoecales",
      family %in% c("Chlorellaceae", "Oocystaceae") ~ "Chlorellales",
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
      family %in% c("Chlamydomonadaceae", "Haematococcaceae", "Chlorococcaceae", "Cryptomonadaceae", "Wislouchiaceae") ~ "Chlamydomonadales",
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
      order == "Flosculariaceae" ~ "Eurotatoria",
      order == "Ebriales" ~ "Thecofilosea",
      order %in% c("Nodosilineales", "Coleofasciculales", "Leptolyngbyales", "Spirulinales", "Nostocales") ~ "Cyanophyceae",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order == "Bicosoecales" ~ "Bicoecidea",
      order %in% c("Chromulinales", "Chrysosphaerales") ~ "Chrysophyceae",
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
      class == "Eurotatoria" ~ "Rotifera",
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
      # missing
      phylum == "Rotifera" ~ "Animalia",
      phylum == "Cyanobacteria" ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Plantae",
      phylum %in% c("Ochrophyta", "Cryptista") ~ "Chromista",
      phylum %in% c("Amoebozoa", "Euglenophyta") ~ "Protozoa",
      
      TRUE ~ kingdom
    )
  ) %>% 
  
  relocate(tax.uid, resolved.taxa.name, species, genus, family, order, class, phylum, kingdom)
  
  
x <- tax_final %>% 
  distinct(
    phylum
  )

y <- tax_final %>% 
  filter(
    phylum == "Mollusca"
  )









