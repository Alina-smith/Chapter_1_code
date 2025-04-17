# Aim of script: Getting taxonomy info for all the species and adding it to the raw data

# Flow of script:
  # 1) Clean names:     Run the names through taxize::gna_verifier to fix spelling mistakes and then manually fix any that weren't picked up
  # 2) Resolved names:  Run the cleaned names through rotl::tnrs_match_names to get most up to date names and then manually fix any that weren't picked up
  # 3) Taxonomy:        Run cleaned names through taxize::classification with tol to get taxonomy and then manuallu fill in any gaps

# Last updated: 18/03/2025
# Data run through veryfier on 19/01/2025
# Data run through tnrs_match_names on 19/03/2025 
# Data ran through classification on 19/03/2025


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
library(ape)

install.packages(c("treeplyr", "BiocManager"))
library(BiocManager)
# 
# install.packages(c("ggtree","treeio"))
# BiocManager::install("ggtree", force = TRUE)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)

# Import data ----
bodysize_raw <- readRDS("R/data_outputs/database_products/final_products/bodysize_raw.rds")
not_genus <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "not_genus")

# Clean names ----

## gna ----
# run through gna_verifier to fix any spelling

# get a list of all distinct names 
names_list <- select(bodysize_raw, original.taxa.name) %>% 
  
  distinct(original.taxa.name) %>% 

  # Convert to a string of names
  pull(original.taxa.name)

# View names
glimpse(names_list)

# run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
cleaned_gna <- do.call(rbind, lapply(names_list, function(name) {
  tryCatch(
    {
      # Process each name and return the result
      gna_verifier(names_list) %>%
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
  }
  )
  ) %>% 
  rename(
    original.taxa.name = submittedName,
    cleaned.taxa.name.gna = matchedCanonicalFull,
    cleaned.source.gna = dataSourceTitleShort
    )

# Save
saveRDS(cleaned_gna, file = "R/data_outputs/database_products/taxonomy/cleaned_gna.rds")

## Manual ----

# 1) Find all the taxa.names from clean_gna to manually resolve based on the criteria in the comments
to_clean_manually <- cleaned_gna %>% 
  
  mutate(
    
    manual = case_when(
      # Ones that weren't picked up by gna_veryier at all - gave NA for the cleaned.taxa.name
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
write_csv(to_clean_manually, "R/data_outputs/database_products/taxonomy/to_clean_manually.csv")

# 2) Update cleaned.taxa.name with the manually cleaned names

# Import the manually cleaned names
manually_cleaned <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "cleaned_tol")

# Replace cleaned.taxa.name with manually cleaned names when ones is present
cleaned <- cleaned_gna %>% 
  
  # left join all the manually cleaned ones from manualy_cleaned spreadsheet
  left_join(
    manually_cleaned,
    by = "original.taxa.name"
  ) %>% 
  
  # when a name has been cleaned manually (cleaned.source = "manually") then select that name else keep the current one
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
  
  # remove any that couldn't be cleaned
  filter(
    !(is.na(cleaned.taxa.name))
  )

# Save
saveRDS(cleaned, file = "R/data_outputs/database_products/taxonomy/cleaned.rds")

cleaned <- readRDS("R/data_outputs/database_products/taxonomy/cleaned.rds")

# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them

# Resolve names ----
## TOL ----

# Import cleaned data
cleaned <- readRDS("R/data_outputs/database_products/taxonomy/cleaned.rds")

# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)
resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name)

# Save
saveRDS(resolved_tol, file = "R/data_outputs/database_products/taxonomy/resolved_tol.rds")

## Manual ----
# 1) Select all the ones that weren't picked up by tol and manually resolve their names

to_resolve_manually <- resolved_tol %>% 
  
  filter(
    is.na(unique_name)
  )

# Save
write_csv(to_resolve_manually, "R/data_outputs/database_products/taxonomy/to_resolve_manually.csv")

# 2) Add in the manually resolved names to the full name list

# Import the manually resolved names
manually_resolved_subset <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "resolve")

# Add to main names list
resolved_manual <- left_join(resolved_tol, manually_resolved_subset, by = "search_string") %>% 
  
  # When a manually resolved name is present use this instead use the original resolved name
  mutate(
    resolved.taxa.name = if_else(
      !is.na(unique_name_manual),
      unique_name_manual,
      unique_name
    ),
    
    # Manual changes from extra ones
    resolved.taxa.name = case_when(
      resolved.taxa.name == "Cymbella cuspidata" ~ "Cymbopleura cuspidata",
      resolved.taxa.name == "Pseudopodosira kosugii" ~ "Pseudopodosira echinus",
      resolved.taxa.name == "Hippodonta lueneburgensis" ~ "Hippodonta luneburgensis",
      resolved.taxa.name == "Gleocapsa" ~ "Gloeocapsa",
      
      TRUE ~ resolved.taxa.name
      
    ),
    
    # remove variety and forms just to make things easier 
    resolved.taxa.name = case_when(
      stri_detect_regex(resolved.taxa.name, "var\\.|f\\.") ~ stri_extract_first_regex(resolved.taxa.name, "\\w+ \\w+"),
      
      TRUE ~ resolved.taxa.name
    )
  ) %>%
  
  # Remove any that couldn't be resolved
  filter(
    !is.na(resolved.taxa.name)
  ) %>% 
  
  # Select relevant columns
  select(
    search_string, unique_name, resolved.taxa.name
  )

# Save
saveRDS(resolved_manual, "R/data_outputs/database_products/taxonomy/resolved_manual.rds")























## Fix multiple hits ----
# Some taxa have the same name as others so want to check that any that have more than one hit the correct one was chosen

### Make list of multis ----


# 1) Rerun through tnrs_match_names to resolve all again
multi_tnrs_full <- tnrs_match_names(resolved_manual$resolved.taxa.name)

# Save
saveRDS(multi_tnrs_full, file = "R/data_outputs/database_products/taxonomy/multi_tnrs_full.rds")

# 2) get any that have more than one match
multi <- multi_tnrs_full %>% 
  
  filter(
    number_matches >1
  ) %>% 
  
  mutate(
    # want to make a column that act as a uid for later steps
    uid = paste0(search_string, ott_id),
    original = "yes",
  ) 

# 3) Loop through all the names in the multi list to get a list of all the hits

# make an empty list
multi_list <- list()

# Loop through the indices of search_string
for (i in 1:length(multi$search_string)) {
  multi_list[[i]] <- inspect(multi_tnrs_full, taxon_name = multi$search_string[i])
}

# make into a data frame
multi_all <- bind_rows(multi_list)

# 4) Run the otts through classification with tol to get the taxonomy info

multi_tax <- multi_all %>% 
  
  mutate(
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for that taxa
      tax_lineage(taxonomy_taxon_info(ott_id, include_lineage = TRUE))# rows = 1 so that is only takes the first one and doesn't require you select options for each one
    )[[1]] # select the first element of the list
  ) %>% 
  
  # take the info out of the list column so now it has multiple of the same search_string
  unnest(
    col = everything(), names_sep = "."
  ) %>% 
  
  filter(
    tax.rank != "no rank"
  ) %>% 
  
  select(
    unique_name,
    ott_id,
    tax.unique_name,
    tax.rank
  ) %>% 
  
  # Pivot so the ranks are now columns
  pivot_wider(
    names_from = tax.rank,
    values_from = tax.unique_name) %>% 
  
  unnest_wider(
    col = everything(), names_sep = "."
  )

# Save
saveRDS(multi_tax, file = "R/data_outputs/database_products/taxonomy/multi_tax.rds")

### Manually fix ----
# check through list and fix any that have been assigned wrong in the full tnr list
resolved <- multi_tnrs_full %>% 
  
  mutate(
    ott_id = case_when(

      # Ones that had a multiple hit that needs to be changed 
      search_string == "bicoeca cylindrica" ~ 5385268,
      search_string == "bicoeca campanulata" ~ 5385258,
      search_string == "oscillatoria amphibia" ~ 707845,
      search_string == "anacystis incerta" ~ 4016649,
      search_string == "gaarderia compressa" ~ 5409388,
      search_string == "microglena" ~ 5362912,
      
      search_string == "colpidium campylum" ~ 427000,
      search_string == "cryptoglena" ~ 312183,
      search_string == "entosiphon sulcatus" ~ 598957,
      search_string == "euglena charkowiensis" ~ 150267,
      search_string == "gloeochloris" ~ 4735204,
      search_string == "na" ~ NA,
      search_string == "planktonema" ~ 307972,
      
      
      search_string == "bicosoeca socialis" ~ 4023445,
      search_string == "coscinodiscus subsalsus" ~ 2812112,
      search_string == "aphanocapsa nubilum" ~ 5376792,
      search_string == "aphanocapsa koordersi" ~ 4016689,
      search_string == "achnanthidium macrocephalum" ~ 6388726,
      search_string == "mayamaea recondita" ~ 2818322,
      search_string == "aphanocapsa littoralis" ~ 4016662,
      search_string == "crucigeniella neglecta" ~ 5153022,
      search_string == "synechocystis minima" ~ 4016583,
      search_string == "chrysococcus rufescens" ~ 4016510,
      search_string == "ophiothrix" ~ 25914,
      
      # Random ones that were found that were not multis but just assigned wrong 
      search_string == "acanthosphaera (genus in subkingdom sar)" ~ 6001434,
      search_string %in% c("schizothrix lacustris (species in domain eukaryota)", "schizothrix pulvinata (species in domain eukaryota)") ~ 7563454,
      search_string == "agmenellum elegans" ~ 4016804,
      search_string == "spirocoleus tenuis" ~ 447081,
      search_string == "Neobodo curvifilus" ~ 606326,
      
      TRUE ~ ott_id
    ),
    
    unique_name = case_when(
      
      # Ones that had a multiple hit that needs to be changed 
      ott_id == "5385268" ~ "Bicosoeca cylindrica",
      ott_id == "5385258" ~ "Bicosoeca campanulata",
      ott_id == "707845" ~ "Anagnostidinema amphibium",
      ott_id == "4016649" ~ "Aphanocapsa incerta",
      ott_id == "5409388" ~ "Gaarderiella compressa",
      ott_id == "5362912" ~ "Microglena (genus in kingdom Archaeplastida)",
      
      ott_id == "427000" ~ "Dexiostoma campyla",
      ott_id == "312183" ~ "Cryptoglena (genus in infrakingdom Excavata)",
      ott_id == "598957" ~ "Entosiphon sulcatum",
      ott_id == "150267" ~ "Lepocinclis oxyuris",
      ott_id == "4735204" ~ "Chlorangiogloea",
      ott_id == "NA" ~ NA,
      ott_id == "307972" ~ "Planctonema",
      ott_id == "155852" ~ "Cryptaulax (genus in infrakingdom Excavata)",

      ott_id == "4023445" ~ "Diplomitella socialis",
      ott_id == "2812112" ~ "Actinocyclus normanii",
      ott_id == "5376792" ~ "Aphanocapsa nubila",
      ott_id == "4016689" ~ "Aphanocapsa koordersii",
      ott_id == "2818322" ~ "Navicula recondita",
      ott_id == "4016662" ~ "Aphanocapsa litoralis",
      ott_id == "5153022" ~ "Willea neglecta",
      ott_id == "4016583" ~ "Synechocystis bourrellyi",
      ott_id == "6388726" ~ "Achnanthidium microcephalum",
      ott_id == "25914" ~ "Ophiocytium",
      
      # Random ones that were found that were not multis but just assigned wrong 
      ott_id == "6001434" ~ "Acanthosphaera",
      ott_id == "7563454" ~ "Schizothrix",
      ott_id == "4016804" ~ "Merismopedia elegans",
      ott_id == "447081" ~ "Leptolyngbya tenuis",
    
      TRUE ~ unique_name
    )
    
  ) %>% 
  
  # Remove the NA one that was changed from "na"
  filter(
    !is.na(ott_id),
    unique_name != "Nitzschia palea var. debilis",
    
    # remove weird ones
    !(unique_name %in% c("Poria chlorina", "Erinella subtilissima", "Cothurniopsis valvata", "Pyramidomonas", "Vorticellides aquadulcis", "Parkeria sphaerica", "Bromeliothrix metopoides",
                         "Praestephanos carconensis", "Sphaerastrum fockii", "Marssoniella", "Paramecium tetraurelia", "Palaeacmea", "Glaucomides bromelicola", "Actinophrys sol")
      )
    ) %>% 
  
  select(
    search_string,
    unique_name,
    ott_id
  )

# Save
saveRDS(resolved, file = "R/data_outputs/database_products/taxonomy/resolved.rds")

# Taxonomy: ----

## Initially retrieve taxonomy info ----
taxonomy_raw <- resolved %>% 
  
  mutate(
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for that taxa
      tax_lineage(taxonomy_taxon_info(ott_id, include_lineage = TRUE))# rows = 1 so that is only takes the first one and doesn't require you select options for each one
    )[[1]] # select the first element of the list
  ) %>% 
  
  # take the info out of the list column so now it has multiple of the same search_string
  unnest(
    col = everything(), names_sep = "."
  ) %>% 
  
  filter(
    tax.rank != "no rank"
  ) %>% 
  
  select(
    unique_name,
    ott_id,
    tax.unique_name,
    tax.rank
  ) %>% 
  
  # Pivot so the ranks are now columns
  pivot_wider(
    names_from = tax.rank,
    values_from = tax.unique_name) %>% 
  
  unnest_wider(
    col = everything(), names_sep = "."
  )

# Save
saveRDS(taxonomy_raw, file = "R/data_outputs/database_products/taxonomy/taxonomy_raw.rds")

## Format taxonomy ----

### Initial edits ----
# Do any edits that can easily be done within mutate

taxonomy_formatted <- taxonomy_raw %>% 
  
  rename(
    tol.taxa.name = unique_name.1,
    ott.id = ott_id.1
  ) %>% 
  
  mutate(
    
    ##### Fill in gaps ----
    # Fill in gaps in taxonomy - this isn't changing where anything goes in the phylogeny just filling in gaps that tol didn't put in
    # Before any changes were done the taxa was first checked against the multiples list and changed to the multiple if needed
    
    # Resolved.taxa.name
    resolved.taxa.name = case_when(
      # Some taxa have been updated into a different genus but haven't had their species name changed so their genus is not the same as the genus part of the species name
      # Update these - can keep the same ott_id as it is in the right genus but just need the name updated
      
      tol.taxa.name == "Eolimna minima" ~ "Sellaphora nigri",
      tol.taxa.name == "Brebissonia lanceolata" ~ "Gomphonema lanceolatum",
      tol.taxa.name == "Diaptomus oregonensis" ~ "Skistodiaptomus oregonensis",
      tol.taxa.name == "Schizonema seminoides" ~ "Navicula seminoides",
      tol.taxa.name == "Sphaerellopsis lateralis" ~ "Vitreochlamys lateralis",
      tol.taxa.name == "Sphaerellopsis mucosa" ~ "Vitreochlamys mucosa",
      tol.taxa.name == "Sphaerellopsis velata" ~ "Vitreochlamys velata",
      tol.taxa.name == "Sphaerellopsis ampla" ~ "Vitreochlamys ampla",
      tol.taxa.name == "Crumenula texta" ~ "Euglena texta",
      
      TRUE ~ tol.taxa.name
    ),
    
    # Species
    # It doesn't have the species in the lineage info so need to find all that are at species level (has a space) some have a space because they have brackets so need to remove brackets first
    name.crop = if_else(
      stri_detect_regex(resolved.taxa.name, "\\("),
      stri_extract_first_regex(resolved.taxa.name, ("\\w+ \\w*")),
      resolved.taxa.name
    ),
    
    name.crop = stri_trim(name.crop),
    
    species = if_else(
      stri_detect_regex(name.crop, " "),
      name.crop,
      NA
    ),
    
    # genus
    # First just do ones I have
    genus = case_when(
      stri_detect_regex(name.crop, "Euglena") ~ "Euglena", 
      stri_detect_regex(name.crop, "Cryptaulax") ~ "Cryptaulax",
      stri_detect_regex(name.crop, "Amoeba") ~ "Amoeba", 
      stri_detect_regex(name.crop, "Karlodinium") ~ "Karlodinium",
      stri_detect_regex(name.crop, "Karenia") ~ "Karenia",
      
      TRUE ~ genus.1,
    ),
    
    # Now select any that have a species and use the genus name
    # When species is filled but genus is missing select just the first name in the species name
    genus = if_else(
      is.na(genus) & !is.na(species),
      stri_extract_first_regex(species, "\\w+"),
      genus
    ),
    
    # Finally just take the resolved.taxa.name and then if it is a higher rank can change it later on
    genus = if_else(
      is.na(genus),
      resolved.taxa.name,
      genus
    ),
    
    genus = if_else(
      genus %in% not_genus$name,
      NA,
      genus
    ),
    
    # family
    family = case_when(
      family.1 == "Bicosoecidae" ~ "Bicosoecaceae",
      
      genus == "Euglena" ~ "Euglenaceae",
      genus == "Cryptaulax" ~ "Cryptomonadaceae",
      genus == "Amoeba" ~ "Amoebidae",
      genus %in% c("Karlodinium", "Karenia") ~ "Kareniaceae",
      
      TRUE ~ family.1
    ),
    
    # order
    order = case_when(
      order.1 == "Bicosoecida" ~ "Bicosoecales",
      
      family == "Cryptomonadaceae" ~ "Cryptomonadales",
      family == "Euglenaceae" ~ "Euglenales",
      family == "Amoebidae" ~ "Euamoebida",
      family == "Kareniaceae" ~ "Gymnodiniales",
      
      resolved.taxa.name == "Euglenida" ~ "Euglenida",
      
      TRUE ~ order.1
    ),
    
    # class
    class = case_when(
      class.1 == "Haptophyta" ~ "Pavlovophyceae",
      class.1 == "Glaucophyta" ~ "Glaucophyceae",
      
      order == "Euglenales" ~ "Euglenophyceae",
      order == "Cryptomonadales" ~ "Cryptophyceae",
      order == "Euglenida" ~ "Euglenoidea",
      order == "Euamoebida" ~ "Tubulinea",
      order == "Gymnodiniales" ~ "Dinophyceae",
      
      resolved.taxa.name %in% c("Cryptophyceae", "Dinophyceae", "Raphidophyceae", "Chrysophyceae") ~ resolved.taxa.name,
      
      TRUE ~ class.1
    ),
    
    # Phylum
    phylum = case_when(
      phylum.1 == "Ciliophora (phylum in subkingdom SAR)" ~ "Ciliophora",
      phylum.1 == "Streptophyta" ~ "Charophyta",
      phylum.1 == "Cryptophyceae" ~ "Cryptophyta",
      phylum.1 == "Euglenida" ~ "Euglenozoa",
      
      class %in% c("Dictyochophyceae", "Chrysophyceae", "Xanthophyceae", "Phaeothamniophyceae", "Raphidophyceae", "Actinophryidae") ~ "Ochrophyta",
      class == "Dinophyceae" ~ "Myzozoa",
      
      order == "Bicosoecales" ~ "Bigyra",
      order == "Craspedida" ~ "Choanozoa",
      order %in% c("Eubodonida", "Neobodonida", "Trypanosomatida") ~ "Euglenozoa",
      order %in% c("Eustigmatales", "Synurales") ~ "Ochrophyta",
      order == "Cryptomonadales" ~ "Cryptophyta",
      order == "Noctilucales" ~ "Myzozoa",
      
      resolved.taxa.name %in% c("Cyanobacteria","Chlorophyta", "Rotifera", "Haptophyta") ~ resolved.taxa.name,
      resolved.taxa.name == "Dinoflagellata" ~ "Myzozoa",
      resolved.taxa.name == "Ciliophora (phylum in subkingdom SAR)" ~ "Ciliophora (phylum in subkingdom SAR)",
      resolved.taxa.name == "Chrysomeridales" ~ "Ochrophyta",
      
      
      
      TRUE ~ phylum.1
    )
  ,
  
  kingdom = case_when(
    phylum %in% c("Cyanobacteria", "Actinobacteria", "Proteobacteria (phylum silva:A16379/#2)", "Chloroflexi", "Actinobacteria") ~ "Bacteria",
    phylum %in% c("Chlorophyta", "Charophyta", "Rhodophyta", "Glaucophyta", "Cryptophyta", "Streptophyta") ~ "Plantae",
    phylum %in% c("Ochrophyta", "Bacillariophyta", "Haptophyta", "Bigyra", "Myzozoa", "Ciliophora (phylum in subkingdom SAR)", "Cercozoa", "Foraminifera") ~ "Chromista",
    phylum %in% c("Euglenozoa", "Amoebozoa", "Choanozoa") ~ "Protozoa",
    phylum %in% c("Arthropoda", "Mollusca", "Rotifera", "Gastrotricha", "Cnidaria", "Bryozoa", "Chordata", "Annelida", "Platyhelminthes", "Nematoda") ~ "Animalia",
    phylum %in% c("Ascomycota", "Basidiomycota") ~ "Fungi",
  ),
  
  type = case_when(
    class %in% c("Amphibia", "Anthozoa", "Arachnida", "Malacostraca", "Insecta") ~ "remove",
    
    kingdom %in% c("Bacteria", "Plantae") ~ "Phytoplankton",
    kingdom == "Animalia" ~ "Zooplankton",
    
    phylum %in% c("Ochrophyta", "Haptophyta", "Bigyra", "Myzozoa", "Euglenozoa", "Bacillariophyta", "Cryptophyta") ~ "Phytoplankton",
    phylum %in% c("Cercozoa", "Amoebozoa", "Foraminifera", "Apusozoa", "Ciliophora", "Sarcomastigophora") ~ "Zooplankton",
    
    TRUE ~ "remove"
    )
  ) %>% 
  
  # Remove non plankton
  filter(
    type != "remove"
  ) %>% 
  
  # Rename
  rename(
    taxa.name = resolved.taxa.name
  ) %>% 
  
  # Select columns
  select(
    ott.id, tol.taxa.name, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  )

### Family ----
# want to manually input missing ranks for family as will take too long to do in a case_when

# Make a list of missing family with their genus
missing_family <- taxonomy_formatted %>% 
  select(
    genus,
    family
  ) %>% 
  
  filter(
    is.na(family)
  ) %>% 
  
  distinct(
    genus
  )

# save
write_csv(missing_family, "R/data_outputs/database_products/taxonomy/missing_family.csv")

# Import the manually filled family
manually_family <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "family")

# Update family
taxonomy_family <- taxonomy_formatted %>% 
  
  left_join(manually_family, by = "genus", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    family = if_else(
      !is.na(family.manual),
      family.manual,
      family.tol
    ),
    
    family = case_when(
      taxa.name %in% c("Potamididae", "Bithyniidae", "Littorinidae", "Bosminidae", "Daphniidae", "Desmidiaceae", "Chlorellaceae", "Stephanodiscaceae", "Characiaceae", "Chlamydomonadaceae",
                       "Palmellaceae","Microcystaceae", "Closteriaceae", "Fragilariaceae", "Aphanizomenonaceae", "Tetrasporaceae", "Nostocaceae", "Oscillatoriaceae", "Gymnodiniaceae",
                       "Thalassiosiraceae", "Chromulinaceae") ~ taxa.name,
      
      TRUE ~ family
      )
  ) %>% 
  
  select(
    - family.manual,
    - family.tol
  )

### Order ----
# Want to manually input missing ranks for order as will take too long to do in a case_when

# Make a list of missing order with their family
missing_order <- taxonomy_family %>% 
  select(
    family,
    order
  ) %>% 
  
  filter(
    is.na(order)
  ) %>% 
  
  distinct(
    family
  )

# Save
write_csv(missing_order, "R/data_outputs/database_products/taxonomy/missing_order.csv")

# Import the manually filled order
manually_order <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "order")

# Update order
taxonomy_order <- taxonomy_family %>% 
  
  left_join(manually_order, by = "family", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    order = if_else(
      !is.na(order.manual),
      order.manual,
      order.tol
    ),
    
    order = case_when(
      taxa.name %in% c("Chromulinales", "Gymnodiniales", "Ochromonadales", "Oscillatoriales", "Peridiniales", "Chrysomeridales", "Chroococcales", "Chlamydomonadales") ~ taxa.name,
      
      TRUE ~ order
    )
  ) %>% 
  
  select(
    - order.manual,
    - order.tol
  ) %>% 
  
  relocate(
    ott.id, tol.taxa.name, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  )

### Final edits ----
# Final edits can just be done with case_when

taxonomy <- taxonomy_order %>% 
  
  mutate(
    
    # Class
    # Fill in missing class rank
    
    class = case_when(
      order == "Eustigmatales" ~ "Eustigmatophyceae",
      order == "Pyramimonadales" ~ "Pyramimonadophyceae",
      order %in% c("Fragilariales", "Tabellariales", "Coscinodiscales", "Aulacoseirales", "Melosirales", "Thalassiosirales", "Leptocylindrales", "Paraliales") ~ "Bacillariophyceae",
      order == "Synurales" ~ "Chrysophyceae",
      order == "Bangiales" ~ "Bangiophyceae",
      order == "Chrysomeridales" ~ "Chrysomeridophyceae",
      order %in% c("Chroococcales", "Oscillatoriales", "Nostocales", "Pseudanabaenales", "Pleurocapsales", "Synechococcales", "Nodosilineales", "Spirulinales", "Leptolyngbyales") ~ "Cyanophyceae",
      order == "Noctilucales" ~ "Dinophyceae",
      order == "Vaginulinida" ~ "Nodosariata",
      order == "Euglenales" ~ "Euglenophyceae",
      order == "Mischococcales" ~ "Xanthophyceae",
      order %in% c("Gomontiellales", "Coleofasciculales", "Pelonematales", "Chroococcidiopsidales", "Gloeobacterales", "Geitlerinematales") ~ "Cyanophyceae",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order == "Picocystales" ~ "Picocystophyceae",
      order == "Bicosoecales" ~ "Bicoecidea",
      order == "Chaetonotida" ~ "Gastrotricha incertae sedis",
      
      taxa.name %in% c("Arachnida", "Bacillariophyceae", "Bdelloidea (class in Lophotrochozoa)", "Chlorophyceae", "Euglenophyceae", "Ostracoda", "Trebouxiophyceae", "Zygnemophyceae") ~ taxa.name,
      
      TRUE ~ class
    )
  ) %>% 
  
  # Reorder
  relocate(
    ott.id, tol.taxa.name, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  ) %>% 
  
  # get distinct taxa
  distinct(
    ott.id, .keep_all = TRUE
  )

# Save
saveRDS(taxonomy, file = "R/data_outputs/database_products/taxonomy/taxonomy.rds")

# Plot phylogeny ----
# plot the phylogeny to check that all groups are plotted into the right places

## Select relevant data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data <- taxonomy %>% 
  
  select(
    ott.id,
    tol.taxa.name,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 

  distinct(ott.id, .keep_all = TRUE)

## Get phylo relationships from a list of taxa: ----

# 1) Match my names with taxa names in OTT (open tree taxonomy)
taxa <- tnrs_match_names(unique(phylo_plot_data$tol.taxa.name))

# 2) Update ott_ids with ones that were changed above
# Find ones to change
taxonomy2 <- taxonomy %>% 
  mutate(
    tol.taxa.name = tolower(tol.taxa.name)
  )

to_change <- taxa %>% 
  left_join(
    select(
      taxonomy2, tol.taxa.name, ott.id
    ), by = c("search_string" = "tol.taxa.name")
  ) %>% 
  filter(
    ott_id != ott.id
  )
  
# Change them
taxa2 <- taxa %>% 
  mutate(
    ott_id = case_when(
      ott_id == 4004663 ~ 6001434,
      ott_id == 4922332 ~ 4016510,
      TRUE ~ ott_id
    ),
    ott_id = as.integer(ott_id)
  ) 

# Map the search_string and unique_name columns 
taxon_map <- structure(
  taxa2$search_string, names = taxa2$unique_name
) 

# Check for any that havent been picked up by tol and have a missing unique_name
unique(is.na(taxon_map)) # false means there are no missing names so don't need to do anything

## Remove species that aren't in synthetic tree ----
# Some taxa aren't in the OTL synthetic tree so need to find which ones aren't and remove them from my list

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_tax <- is_in_tree(ott_ids = taxa2$ott_id)

# Save
saveRDS(in_tree_tax, "R/data_outputs/database_products/taxonomy/in_tree_tax.rds")

in_tree_tax

sum(in_tree_tax == TRUE) # 4955
sum(in_tree_tax == FALSE) # 814

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 
# tips = end of the tree branches aka the genera in this case
# internal nodes = the inner links in the tree
taxa_in_tree <- taxa2[in_tree_tax, ] # get list of just species in the tree

tree <- tol_induced_subtree(ott_ids = taxa_in_tree$ott_id) # Make tree

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

## Plot tree square ----
plot(tree, show.tip.label = FALSE)

## Edit tip labels ----
# Replace the tip labels on the plot with the corresponding names in the dataset

# Make a vector of taxa names with extra info (ott number) removed so it is just the taxa names
tips_ott_removed <- strip_ott_ids(tree$tip.label, remove_underscores = TRUE)

head(tips_ott_removed) # now just has the taxa name as the tip label instead of the taxa name and ott

# use the taxon map made earlier to map the new tip labels with the dataset names and replace with the dataset names
tree_new_tips <- tree
tree_new_tips$tip.label <- unname(taxon_map[tips_ott_removed])
tree_new_tips$node.label<- NULL #remove node labels 

# Plot with new names
plot(tree_new_tips, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tree_new_tips, show.tip.label = FALSE)

## Update lists ----
# Want to update all the lists to contain just the taxa that are present in the tree

# taxon map
taxon_map_in_tree <- taxon_map[taxon_map %in% tree_new_tips$tip.label]

# taxa
# add in extra info

phylo_plot_data2 <-  phylo_plot_data %>% 
  mutate(
    tol.taxa.name = tolower(tol.taxa.name)
  )

taxa_update_in_tree <- taxa_in_tree %>% 
  
  left_join(
    select(
      phylo_plot_data2, tol.taxa.name, phylum, kingdom
    ), by = c("search_string" = "tol.taxa.name")
  ) %>% 
  
  mutate( 
    # make new column called tip.label that is the same as taxa.name
    tip.label = search_string
  ) %>% 
  
  # group for plotting
  group_by(tip.label)

# explore data a bit
length(unique(taxa_update_in_tree$tip.label)) # check that there are still the same number of species - shhould be same as sum(in_tree == TRUE) (3081)
table(taxa_update_in_tree$phylum)

## Plotting tree circular ----

# initial tree
circular_plot <- ggtree(tree_new_tips, branch.length='none', layout='circular') 
circular_plot

# Adding in group info

taxa_update_in_tree_edit <- taxa_update_in_tree %>%
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    phylum_label = paste0("Phylum: ", phylum)
  )

circular_plot <- circular_plot %<+% taxa_update_in_tree_edit +
  geom_tippoint(aes(x = x + 5, color = phylum_label), size = 3, show.legend = TRUE)+
  geom_tippoint(aes(x = x + 7, color = kingdom_label), size = 3, show.legend = TRUE)+
  
  scale_color_manual(values =
                       c("Kingdom: Plantae" = "#006400", "Kingdom: Bacteria" = "#9B59B6", "Kingdom: Protozoa" = "#CC5500", "Kingdom: Chromista" = "#00008B", "Kingdom: Animalia" = "#8B0000",
                         "Phylum: Chlorophyta" = "#e5f5e0", "Phylum: Charophyta" = "#a1d99b", "Phylum: Glaucophyta" = "#74c476", "Phylum: Rhodophyta" = "#238b45", "Phylum: Cryptophyta" = "#41ab5d",
                         "Phylum: Cyanobacteria" = "#B57EDC",
                         "Phylum: Euglenozoa" = "#FFD8A8", "Phylum: Amoebozoa" = "#FFA500",
                         "Phylum: Myzozoa" = "#6495ED", "Phylum: Bacillariophyta"= "#CFE2F3", "Phylum: Ochrophyta" = "#87CEEB", "Phylum: Haptophyta" = "#ADD8E6", "Phylum: Bigyra"= "#003366", "Phylum: Ciliophora" = "#1E3A5F", "Phylum: Cercozoa" = "#4682B4",
                         "Phylum: Cnidaria" = "#FFCCCB", "Phylum: Mollusca" = "#F08080", "Phylum: Arthropoda" = "#FF6347", "Phylum: Rotifera" = "#D32F2F", "Phylum: Gastrotricha" = "#B71C1C"
                       )
  )

circular_plot

# Final tax list ----
# All good so can use classification as tax_list

tax_list_raw <- taxonomy

# Save
saveRDS(tax_list_raw, file = "R/data_outputs/database_products/taxonomy/tax_list_raw.rds")

# Add to main data ----
# need to join things inn sequence because there were a couple of steps to get from the raw names to the formatted names

## Linking new to original names ----
### Classification to resolved ----

class_to_resolved <- left_join(
  select(
    resolved, search_string, ott_id
    ), select(
      taxonomy, ott.id, taxa.name
      ), by = c("ott_id" = "ott.id")
  ) %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(taxa.name)
  )

### Resolved to cleaned ----

# 1) Resolved to multi
resolved_to_multi <- left_join(
  select(
    multi_tnrs_full, search_string
    ), select(
      class_to_resolved, search_string, ott_id
      ), by = "search_string") %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(ott_id)
  )

# 2) multi to resolved.manual
multi_to_rm <- resolved_manual %>% 
  
  select(
    resolved.taxa.name,
    search_string
  ) %>% 
  
  # need to make lower case to match
  mutate(
    resolved.taxa.name = tolower(resolved.taxa.name)
  ) %>% 
  
  left_join(., resolved_to_multi, by = c("resolved.taxa.name" = "search_string")) %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(ott_id)
  )

# 3) resolved.manual to cleaned
multi_to_cleaned <- cleaned %>% 
  
  # need to make lower case to match
  mutate(
    cleaned.taxa.name = tolower(cleaned.taxa.name)
  ) %>% 
  
  left_join(
    ., select(
      multi_to_rm, search_string, ott_id
      ), by = c("cleaned.taxa.name" = "search_string")
    ) %>% 
  
  # Remove ones that were renoved in the classification step
  filter(
    !is.na(ott_id)
  )

## Linking to raw data ----

bodysize_taxonomy <-  bodysize_raw %>% 
  
  # Join ott_id onto raw data
  left_join(
    ., multi_to_cleaned, by = "original.taxa.name"
  ) %>% 
  
  rename(
    ott.id = ott_id
  ) %>% 
  
  # add in taxonomy data
  left_join(
    ., tax_list_raw, by = "ott.id"
  ) %>% 

  # Select and reorder
  select(
    uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
    join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
    join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
    individual.uid, original.taxa.name, taxa.name, ott.id, type, species, genus, family, order, class, phylum, kingdom,
    life.stage, sex, nu, ind.per.nu,
    min.body.size, max.body.size, body.size,
    bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type,
    sample.year, sample.month
  ) %>% 
  
  # Remove any without a taxa.name
  filter(
    !is.na(taxa.name)
  )

# Save
saveRDS(bodysize_taxonomy, file = "R/data_outputs/database_products/bodysize_taxonomy.rds")


