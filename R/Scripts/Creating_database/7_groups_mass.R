## Calculating the body size for ones I don't have it for
# all volume = um3
# all length, width and height = um,
# all mass = ug

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)

# Formatting all data ----
# Final list of all data including phyto and zooplankton with groups and mass

## Import data ----
bodysize_location <- readRDS("R/Data_outputs/database_products/bodysize_location.rds")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")
sources_list_update <- readRDS("R/Data_outputs/database_products/sources_list_update.rds")

## Initial edits ----

format_all_bs <- bodysize_location %>% 
  
  mutate(
    
    # Small misc edits
    
    # change types for later merging
    uid = as.character(uid),
    
    # change measurement type to single when its raw but only has one value because going to get averages of raw data later on so don't want these ones in there
    measurement.type = if_else(
      uid %in% c("340977", "341039", "343115", "338767", "338672", "338649"),
      "single",
      measurement.type
    ),
    
    # assume all na life stage are adults
    life.stage = if_else(
      is.na(life.stage),
      "adult",
      life.stage
    ),
    
    # Convert all units to be the same to make it easier
    body.size = case_when(
      units %in% c("mg", "mm") ~ body.size*1000,
      TRUE ~ body.size
    ),
    
    units = case_when(
      units == "mg" ~ "µg",
      units == "mm" ~ "µm",
      TRUE ~ units
    ),
    
    # set nu for missing one
    nu = if_else(
      uid == "321726",
      "individual",
      nu
    ), 
    
    # change life stage so all phyto are either active or dormant
    life.stage = case_when(
      type == "Phytoplankton" & life.stage == "adult" ~ "active",
      type == "Phytoplankton" & life.stage == "juvenile" ~ "dormant",
      type == "Zooplankton" & life.stage == "active" ~ "adult",
      type == "Zooplankton" & life.stage == "dormant" ~ "juvenile",
      TRUE ~ life.stage
    )
  ) %>%
  
  # don't need units because each measurement type is the same now
  select(
    - units
  ) %>% 
  
  # nu column
  
  mutate(
    nu = case_when(
      ind.per.nu > 1 ~ "multi-cellular",
      ind.per.nu == 1 ~ "individual",
      
      nu %in% c("multi-cell", "multi-cellular", "colony", "filament") ~ "multi-cellular",
      nu == "individual" ~ "individual",
      TRUE ~ NA
    )
  ) %>% 
  
  # select and relocate columns
  
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    ott.id, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes
  )

## Convert raw to averages ----

# get location info to left join to bs_raw
location_info <- format_all_bs %>% 
  select(
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  distinct(
    location.code, .keep_all = TRUE
  )

# Calculate average
bs_raw <- format_all_bs %>% 
  
  # Select raw data
  filter(
    measurement.type == "raw"
  ) %>% 
  
  # Group by all the below factors so that it gives average for different locations and dates
  group_by(
    ott.id, source.code, original.sources, location.code, bodysize.measurement, life.stage, sex, sample.year, sample.month, nu
  ) %>% 
  
  # Calculate mean
  summarise(
    body.size.mean = mean(body.size),
    sample.size = n(),
    sd = sd(body.size),
    error = sd/sqrt(sample.size),
    .groups = "drop"
  ) %>% 
  
  rename(
    body.size = body.size.mean
  ) %>% 
  
  ungroup() %>% 
  
  # Join back in extra info
  left_join(
    location_info, by = "location.code"
  )%>% 
  
  left_join(
    tax_list_raw, by = "ott.id"
  ) %>% 
  
  left_join(
    select(
      sources_list_update, doi, source.code, new.source.code
    ), by = c("source.code" = "new.source.code")
  ) %>% 
  
  # Make new individual.uid
  group_by(
    ott.id, source.code, original.sources, location.code, life.stage, sex, sample.year, sample.month, nu
  ) %>% 
  
  mutate(
    individual.uid = paste(doi, cur_group_id(), sep = "-")
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    
    # Add in misc extra info
    uid = paste("r", row_number(), sep = "-"),
    ind.per.nu = 1, # all individuals
    reps = NA,
    error.type = "se",
    measurement.type = "average",
    bodysize.measurement.notes = NA,
    
    # change tyes for later merging
    sample.size = as.character(sample.size),
    error = as.character(error)
  ) %>% 
  
  ungroup() %>% 
  
  # Select and relocate
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    ott.id, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes
  )

## Combine all together ----
bodysize_groups <- format_all_bs %>% 
  
  # remove raw data
  filter(
    !(measurement.type == "raw")
  ) %>% 
  
  # add in the new averages of raw data
  bind_rows(
    bs_raw
  ) %>% 
  
  # remove columns that aren't needed
  select(
    - measurement.type # all average now
  ) %>% 
  
  # Calculate average for ones that are the same individual in the same location and time
  group_by(
    individual.uid, bodysize.measurement,life.stage
  ) %>% 
  
  mutate(
    body.size = mean(body.size)
  ) %>% 
  
  distinct(
    body.size, .keep_all = TRUE
  ) %>% 
  
  ungroup() %>% 
  
  # Add in groups 
  
  mutate(
    group = case_when(
      phylum %in% c("Cyanobacteria", "Glaucophyta") ~ "Blue/green",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Green",
      phylum == "Bacillariophyta" ~ "Diatom",
      phylum == "Rhodophyta" ~ "Red",
      phylum == "Euglenozoa" ~ "Euglenoid",
      phylum == "Cryptophyta" ~ "Cryptomonads",
      phylum == "Haptophyta" ~ "Haptophytes",
      phylum == "Ciliophora (phylum in subkingdom SAR)" ~ "Ciliate",
      
      class %in% c("Chrysophyceae", "Dictyochophyceae", "Bicosoecophyceae") ~ "Golden-brown",
      class == "Dinophyceae" ~ "Dinoflagellate",
      class == "Raphidophyceae" ~ "Raphidophytes",
      class == "Xanthophyceae" ~ "Yellow-green",
      class == "Eustigmatophyceae" ~ "Eustigmatophytes",
      class == "Phaeothamniophyceae" ~ "Brown",
      
      # Zooplankton - more for the length weight joining
      genus == "Bosmina" ~ "bosmina",
      genus == "Daphnia" ~ "daphnia",
      family == "Daphniidae" ~ "daphniidae",
      order == "Diplostraca" ~ "cladocera",
      phylum == "Rotifera" ~ "rotifer",
      phylum == "Arthropoda" ~ "copepoda",
      
      TRUE ~ NA
    )
  ) 

## Reynolds groups ----

### Import data ----
# These are all large databases that I have already used that include R groups

# Rimmet - 2018, padisak and reynolds
rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet") %>% 
  select(
    `Genus + species name`,
    `Functional groups (Padisak 2009)`
  ) %>% 
  
  rename(
    taxa.name = `Genus + species name`,
    reynolds.group = `Functional groups (Padisak 2009)`
  ) %>% 
  
  mutate(
    ref = "rimmet"
  )
# Kruk - 2017, reynolds
kruk <- read_xlsx("raw_data/kruk_groups.xlsx") %>% 
  select(
    Species_name,
    `Classification by Experts`,
  ) %>% 
  
  rename(
    taxa.name = Species_name,
    reynolds.group = `Classification by Experts`
  ) %>% 
  
  mutate(
    ref = "kruk"
  )
# Padisak - 2009, padisak
padisak <- read_xlsx("raw_data/functional_groups_padisak.xlsx", sheet = "groups") %>% 
  select(
     padisak.r.group,
     taxa.name
  ) %>% 
  
  rename(
    reynolds.group = padisak.r.group
  )%>% 
  
  mutate(
    ref = "padisak"
  )
# Lt - 2021, reynolds using updated padisak
lt <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture") %>% 
  select(
    Taxa_Name,
    Reynolds_Group
  ) %>% 
  
  rename(
    taxa.name = Taxa_Name,
    reynolds.group = Reynolds_Group
  )%>% 
  
  mutate(
    ref = "lt"
  )

### Join together ----
r_groups_raw <- bind_rows(padisak, rimmet, kruk, lt) %>% 
  
  # put all taxa onto seperate rows
  separate_rows(taxa.name, sep = ",|/") %>% 
  
  mutate(
    # Get rid of any white spaces on the ends
    old.taxa.name = str_trim(taxa.name),
    
    # Make all groups upper case because some are a mix of upper and lower
    reynolds.group = toupper(reynolds.group),
    
    # remove the var. and f.
    old.taxa.name = if_else(
      stri_detect_regex(old.taxa.name, "var\\.|f\\."),
      stri_extract_first_regex(old.taxa.name, "(\\S+ )*\\S+ \\S+(?= var\\.| f\\.)"),
      old.taxa.name
    )
  ) %>% 
  
  select(
    - taxa.name
  ) %>% 
  
  # remove any that have no reynolds group
  filter(
    !is.na(reynolds.group),
    reynolds.group != "#NA"
  )

### Update taxa.names ----
# These have the names pre taxonomy step so need to update them
# Rimmet, Kruk and lt data have already gone through the taxonomy step so can just add in the names from the bodysize_taxonomy data
# Padisak hasn't gone through the taxonomy step so need to do these

#### Import data ----

bodysize_taxonomy <- readRDS("R/Data_outputs/database_products/bodysize_taxonomy.rds") %>% 
  select(
    original.taxa.name,
    taxa.name
  ) %>% 
  
  distinct(
    original.taxa.name, .keep_all = TRUE
  ) %>% 
  
  mutate(
    same = "yes"
  )

updated_spec_char <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "special_characters")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")

#### Special characters ----

spec_char <- r_groups_raw %>% 
  
  distinct(
    old.taxa.name
  ) %>%
  
  # Need to edit the names to remove species characters as done in join_db script so that the original.taxa.names are the same for left joining
  mutate(
    # remove any random capitals - set just first letter to upper case, gna_verify doesn't work with anything else
    sc.taxa.name = tolower(old.taxa.name), # set all to lower case
    sc.taxa.name = paste(
      toupper(
        str_sub(
          sc.taxa.name, 1,1 # select first letter and set to upper case
        )
      ),
      str_sub(sc.taxa.name, 2), # paste remaining word
      sep = ""
    ),
    
    # add in *SpecChar* to replace special characters
    sc.taxa.name = stri_replace_all_regex(sc.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*"),
  ) %>% 
  
  # Join updated names
  left_join(
    updated_spec_char, by = c("sc.taxa.name" = "original.taxa.name")
  ) %>% 
  
  mutate(
    # replace old with new
    updated.taxa.name = if_else(
      !is.na(new.taxa.name), new.taxa.name, old.taxa.name
    ),
    
    # Remove any white spaces if there are any
    updated.taxa.name = trimws(updated.taxa.name)
  ) %>% 
  
  filter(
    updated.taxa.name != "Unknown"
  ) %>% 
  
  left_join(.,
            select(
              bodysize_taxonomy, original.taxa.name, taxa.name
            ), by = c("updated.taxa.name" = "original.taxa.name")
  ) %>% 
  
  distinct(
    old.taxa.name, .keep_all = TRUE
  ) %>% 
  
  select(
    old.taxa.name,
    taxa.name
  ) %>% 
  
  # For ones in Padisak that havent been through the taxonomy step first check if there are any that are already in taxa.name and use that
  mutate(
    taxa.name = case_when(
      is.na(taxa.name) & old.taxa.name %in% bodysize_taxonomy$taxa.name ~ old.taxa.name,
      
      TRUE ~ taxa.name
    )
  )
  
#### Run remaning names through taxonomy steps ----

##### clean ----
# Get a list of all distinct names with no taxa.name
to_clean <- spec_char %>% 
  
  filter(
    is.na(taxa.name)
  ) %>% 
  
  select(
    old.taxa.name
  ) %>% 
  
  distinct(old.taxa.name) %>% 
  
  # Convert to a string of names
  pull(old.taxa.name)

# View names
glimpse(to_clean)

##### gna_verifier ----
# Run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
cleaned_gna <- do.call(rbind, lapply(to_clean, function(name) {
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
}
)
) %>% 
  rename(
    original.taxa.name = submittedName,
    cleaned.taxa.name.gna = matchedCanonicalFull,
    cleaned.source.gna = dataSourceTitleShort
  )


##### Manual clean ----
cleaned <- cleaned_gna %>% 
  
  mutate(
    cleaned.taxa.name = case_when(
      
      original.taxa.name == "Nitszchia" ~ "Nitschia",
      original.taxa.name == "Ulothrichales" ~ "Ulotrichales",
      original.taxa.name == "Limnothrix-Planktothrix agardhii" ~ "Planktothrix agardhii",
      original.taxa.name == "Leptolynbgya cf. notata" ~ "Leptolyngbya notata",
      original.taxa.name == "siliceous Chrysophyceae" ~ "Chrysophyceae",
      original.taxa.name == "Pedimonas sp." ~ "Pedinomonas",
      original.taxa.name == "Dynobrioncylindricum" ~ "Dinobryon cylindricum",
      original.taxa.name == "aff. Teleaulax sp." ~ "Teleaulax",
      original.taxa.name == "Cryptopmonas" ~ "Cryptomonas",
      original.taxa.name == "Colonial Chlorococcaleans (Botryococcus" ~ "Botryococcus",
      original.taxa.name == "Coenochlorys" ~ "Coenochloris",
      original.taxa.name == "Oocystis)" ~ "Oocystis",
      original.taxa.name == "Coenochlorys sp." ~ "Coenochloris",
      original.taxa.name == "Ketablepharis" ~ "Katablepharis",
      original.taxa.name == "Micratinium" ~ "Micractinium",
      original.taxa.name == "Planktotrhix rubescens" ~ "Planktothrix rubescens",
      original.taxa.name == "Planktotrhix agardhii" ~ "Planktothrix agardhii",
      original.taxa.name == "Trachelmonas" ~ "Trachelomonas",
      original.taxa.name == "Cell of Dinobryon" ~ "Dinobryon",
      original.taxa.name == "Ceratium hirundinella" ~ "Ceratium hirundinella",
      original.taxa.name == "Aulacoseira granulata f. curvata" ~ "Aulacoseira granulata",
      original.taxa.name == "Chlorella vulgaris var. autotrophica" ~ "Chlorella vulgaris",
      original.taxa.name == "Desmidium laticeps var. quadrangulare" ~ "Desmidium laticeps",
      original.taxa.name == "Scenedesmus acuminatum var. bernardii" ~ "Scenedesmus acuminatus",
      original.taxa.name == "Staurastrum sebaldi var ornatum Nordst" ~ "Staurastrum sebaldi",
      original.taxa.name == "Trachelomonas hispida var. hispida" ~ "Trachelomonas hispida",
      original.taxa.name == "Trachelomonas volvocina Ehr var volvocina Ehr" ~ "Trachelomonas volvocina",
      original.taxa.name == "Urosolenia eriensis var. morsa" ~ "Urosolenia eriensis",
      
      TRUE ~ cleaned.taxa.name.gna
    )
  ) %>% 
  
  filter(
    !is.na(cleaned.taxa.name),
    cleaned.taxa.name != "Monoraphidium pseudomirabilis"
  )

saveRDS(cleaned, "R/data_outputs/database_products/cleaned_groups.rds")

##### tnrs_match_names ----
# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)

resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name) %>% 
  
  select(
    unique_name,
    search_string
  ) %>% 
  
  mutate(
    unique_name = if_else(
      search_string == "aulacoseira ambigua ambigua",
      "Aulacoseira ambigua",
      unique_name
    )
  ) %>% 
  
  rename(
    old.taxa.name = search_string,
    taxa.name = unique_name
  )

# save
saveRDS(resolved_tol, "R/data_outputs/database_products/resolved_tol_groups.rds")

##### final updated names list ----

names_list_updated <- spec_char %>% 
  
  # Remove the ones from spec_char that didn't have a taxa.name
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  # Add back in the names removed above with the taxa.names added in
  bind_rows(
    resolved_tol
  ) %>% 
  
  # just remove two random var. ones as have the species version in there
  filter(
    !(taxa.name %in% c("Aulacoseira granulata var. angustissima", "Staurastrum avicula var. lunatum"))
  )%>% 
  
  # Make old.taxa.name lower case so all are the same because some have upper and some have lower
  mutate(
    old.taxa.name = tolower(old.taxa.name)
  ) %>% 
  
  distinct(
    old.taxa.name, .keep_all = TRUE
  )

#### Format r_groups_raw ----

r_groups <- r_groups_raw %>%
  
  # Update taxa.names
  
  mutate(
    # make old.taxa.name lower case for joining
    old.taxa.name = tolower(old.taxa.name)
  ) %>% 
  
  left_join(
    names_list_updated,  by = "old.taxa.name"
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  select(
    - old.taxa.name
  ) %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  pivot_wider(
    names_from = ref,
    values_from = c(reynolds.group),
    values_fn = function(x) paste(unique(x), collapse = "/")
  ) %>% 
  
  ungroup() %>% 
  
  rename(
    `r.group (padisak 2009)` = padisak,
    `r.group (rimmet 2018)` = rimmet,
    `r.group (Laplace-Treyture 2021)` = lt,
    `r.group (kruk 2017)` = kruk
  ) %>% 
  
  # select preference - first lt, then padisak from rimmet then padisak from padisak then reynolds in date order
  mutate(
    
    r.group.source = case_when(
      !is.na(`r.group (Laplace-Treyture 2021)`) ~ "Laplace-Treyture(2021)",
      is.na(`r.group (Laplace-Treyture 2021)`) & !is.na(`r.group (rimmet 2018)`) ~ "rimmet(2018)",
      is.na(`r.group (Laplace-Treyture 2021)`) & is.na(`r.group (rimmet 2018)`) & !is.na(`r.group (padisak 2009)`) ~ "padisak(2009)",
      is.na(`r.group (Laplace-Treyture 2021)`) & is.na(`r.group (rimmet 2018)`) & is.na(`r.group (padisak 2009)`) & !is.na(`r.group (kruk 2017)`) ~ "kruk(2017)",
      
      TRUE ~ NA
    ),
    
    r.group = case_when(
      !is.na(`r.group (Laplace-Treyture 2021)`) ~ `r.group (Laplace-Treyture 2021)`,
      is.na(`r.group (Laplace-Treyture 2021)`) & !is.na(`r.group (rimmet 2018)`) ~ `r.group (rimmet 2018)`,
      is.na(`r.group (Laplace-Treyture 2021)`) & is.na(`r.group (rimmet 2018)`) & !is.na(`r.group (padisak 2009)`) ~ `r.group (padisak 2009)`,
      is.na(`r.group (Laplace-Treyture 2021)`) & is.na(`r.group (rimmet 2018)`) & is.na(`r.group (padisak 2009)`) & !is.na(`r.group (kruk 2017)`) ~ `r.group (kruk 2017)`,
      
      TRUE ~ NA
    )
  ) %>% 
  
  select(
    taxa.name, r.group.source, r.group
  ) %>% 
  
  # There are some that have multiple inputs for the r.group so select one for these
  # When there is a clear majority use this to be the most representative
  # when it is evenly split select the most representative lake type
  mutate(
    r.group = case_when(
      taxa.name == "Raphidiopsis curvata" ~ "S1",
      taxa.name == "Staurodesmus phimus" ~ "N",
      taxa.name == "Chlorolobium braunii" ~ "X1",
      taxa.name == "Dendromonas virgaria" ~ "X3",
      taxa.name == "Actinotaenium perminutum" ~ "X1",
      taxa.name == "Closteriopsis" ~ "J",
      taxa.name == "Kephyrion ampulla" ~ "X3",
      taxa.name == "Lemmermanniella pallida" ~ "LO",
      taxa.name == "Chromulina grandis" ~ "X3",
      taxa.name == "Cryptomonas tetrapyrenoidosa" ~ "X2",
      taxa.name == "Dolichospermum (inconsistent in FamilyI (in SubsectionIV))" ~ "H1",
      taxa.name == "Micractinium bornhemiense" ~ "F",
      taxa.name == "Bacillariophyceae" ~ "D",
      taxa.name == "Desmodesmus brasiliensis" ~ "J",
      taxa.name == "Vitreochlamys" ~ "X2",
      taxa.name == "Cyclotella comta" ~ "B",
      taxa.name == "Stephanodiscus rotula" ~ "C",
      taxa.name == "Chlorolobium" ~ "X1",
      taxa.name == "Cyanodictyon" ~ "K",
      taxa.name == "Ceratium" ~ "LO",
      taxa.name == "Encyonema minutum" ~ "D",
      taxa.name == "Gymnodinium mitratum" ~ "LO",
      taxa.name == "Brebissonia lanceolata" ~ "MP",
      taxa.name == "Klebsormidium subtile" ~ "V",
      
      TRUE ~ r.group
    )
  ) %>% 
  
  # taxonomy
  
  left_join(
    tax_list_raw, by = "taxa.name"
  ) %>% 
  
  # want to fix the genera that are different to taxa name
  mutate(
    taxa.name = case_when(
      !is.na(genus) & is.na(species) ~ genus,
      taxa.name == "Dinoflagellata" ~ "Myzozoa",
      # ones with var.
      taxa.name == "Aulacoseira granulata var. angustissima" ~ "Aulacoseira granulata",
      taxa.name == "Staurastrum avicula var. lunatum" ~ "Staurastrum avicula",
      
      TRUE ~ taxa.name
    )
  ) %>% 
  
  mutate(
    rank = case_when(
      taxa.name == species ~ "species",
      taxa.name == genus ~ "genus",
      taxa.name == family ~ "family",
      taxa.name == order ~ "order",
      taxa.name == class ~ "class",
      taxa.name == phylum ~ "phylum",
      
      # Remaining ones that don't match
      stri_detect_regex(taxa.name, " ") ~ "species", # all remaining ones with a space are species
      taxa.name %in% c("Chlorolobium", "Chromatium", "Pyrobotrys", "Dimorphococcus", "Binuclearia") ~ "genus",
      taxa.name == "Cryptomonadales" ~ "order",
      taxa.name %in% c("Euglenophyceae", "Xanthophyceae") ~ "class",
      
      TRUE ~ NA),
    
    # fill in missing tax - just makes later steps easier
    species = if_else(
      rank == "species" & is.na(species),
      taxa.name,
      species
    ),
    
    genus = if_else(
      rank == "genus" & is.na(genus),
      taxa.name,
      genus
    ),
    
    family = if_else(
      rank == "family" & is.na(family),
      taxa.name,
      family
    ),
    
    order = if_else(
      rank == "order" & is.na(order),
      taxa.name,
      order
    ),
    
    class = if_else(
      rank == "class" & is.na(class),
      taxa.name,
      class
    ),
    
    phylum = if_else(
      rank == "phylum" & is.na(phylum),
      taxa.name,
      phylum
      )
    ) %>% 
  
  select(
    taxa.name, rank, r.group, r.group.source, species, genus, family, order, class, phylum
  ) 

# save
saveRDS(r_groups, "R/data_outputs/database_products/r_groups.rds")


#### Add groups to main data ----

# Get lists of all the different ranks
species_groups <- r_groups %>% 
  filter(
    !is.na(species)
  )

genus_groups <- r_groups %>% 
  filter(
    !is.na(genus),
    is.na(species)
  ) %>% 
  distinct(
    genus, .keep_all = TRUE
    )

family_groups <- r_groups %>% 
  filter(
    !is.na(family),
    is.na(genus)
  ) %>% 
  distinct(
    family, .keep_all = TRUE
  )

order_groups <- r_groups %>% 
  filter(
    !is.na(order),
    is.na(family)
  ) %>% 
  distinct(
    order, .keep_all = TRUE
  )

class_groups <- r_groups %>% 
  filter(
    !is.na(class),
    is.na(order)
  ) %>% 
  distinct(
    class, .keep_all = TRUE
  )

phylum_groups <- r_groups %>% 
  filter(
    !is.na(phylum),
    is.na(class)
  ) %>% 
  distinct(
    phylum, .keep_all = TRUE
  )

# Add to data
bodysize_formatted <- bodysize_groups %>% 
  
  left_join(
    select(
      species_groups, species, r.group
      ), by = "species"
  ) %>% 
  
  rename(
    r.group.species = r.group
  ) %>% 
  
  left_join(
    select(
      genus_groups, genus, r.group
    ), by = "genus"
  ) %>% 
  
  rename(
    r.group.genus = r.group
  ) %>% 
  
  left_join(
    select(
      family_groups, family, r.group
    ), by = "family"
  ) %>% 
  
  rename(
    r.group.family = r.group
  ) %>% 
  
  left_join(
    select(
      order_groups, order, r.group
    ), by = "order"
  ) %>% 
  
  rename(
    r.group.order = r.group
  )%>% 
  
  left_join(
    select(
      class_groups, class, r.group
    ), by = "class"
  ) %>% 
  
  rename(
    r.group.class = r.group
  )%>% 
  
  left_join(
    select(
      phylum_groups, phylum, r.group
    ), by = "phylum"
  ) %>% 
  
  rename(
    r.group.phylum = r.group
  )

# save
saveRDS(bodysize_r_groups, "R/data_outputs/database_products/bodysize_r_groups.rds")




# Calculating masses ----
## Import data

bodysize_r_groups <- readRDS("R/data_outputs/database_products/bodysize_r_groups.rds")

## Calculate ----
mass <- bodysize_r_groups %>% 
  
  # Get all the different bodysize.measurements on one row per individual
  pivot_wider(
    id_cols = individual.uid,
    names_from = bodysize.measurement,
    values_from = body.size
  ) %>%
  
  rename(
    dry.mass = `dry mass`,
    body.mass = `body mass`
  ) %>% 
  
  # join all extra data back 
  left_join(
    select(
      bodysize_r_groups, - body.size, bodysize.measurement, - uid
    ), by = "individual.uid"
  ) %>% 
  
  distinct(
    individual.uid, .keep_all = TRUE
  ) %>% 
  
  mutate(
    # very few dry mass for phyto so just get rid of them
    # When biovolume is given use this over dry/wet mass
    dry.mass = if_else(
      type == "Phytoplankton",
      NA,
      dry.mass
    ),
    
    # calculate biovolume for ones that have just body mass
    biovolume = case_when(
      is.na(biovolume) & !is.na(body.mass) & type == "Phytoplankton" ~ body.mass/(1*10^-6),
      TRUE ~ biovolume
    ),
    
    # calculate mass from biovolume
    
    # 1) go through pg c
    # first convert to pg C
    pg.c = case_when(
      group == "Diatom" ~ 0.288*biovolume^0.811, # diatom specific one
      type == "Phytoplankton" ~ 0.216*biovolume^0.939, # general phytoplankton one
      TRUE ~ NA
    ),
    
    # Next convert to mass
    mass.c = case_when(
      !is.na(body.mass) & type == "Phytoplankton" ~ body.mass,
      is.na(body.mass) & type == "Phytoplankton" ~ 1e-12*0.07*pg.c,
      
      TRUE ~ NA
    ),
    
    # 2) assume density of 1
    mass.d = case_when(
      !is.na(body.mass) & type == "Phytoplankton" ~ body.mass,
      is.na(body.mass) & type == "Phytoplankton" ~ biovolume*(1*10^-6),
      TRUE ~ NA
    ),
    
    mass = case_when(
      !is.na(body.mass) & type == "Zooplankton" ~ body.mass,
      
      group == "bosmina" ~ 3.0896 + (3.0395*log(length/1000)),
      group == "daphnia" ~ 1.4681 + (2.8292*log(length/1000)),
      group == "daphniidae" ~ 1.5072 + (2.761*log(length/1000)),
      group == "cladocera" ~ 1.7512 + (2.653*log(length/1000)), 
      group == "copepoda" ~ 1.9526 + (2.399*log(length/1000)),
      
      TRUE ~ NA
    ),
    
    # unlog the mass
    mass = exp(mass),
    
    # make a mld column
    mld = case_when(
      type == "Zooplankton" ~ length,
      TRUE ~ pmax(length, width, height, diameter, na.rm = TRUE)
    )
  ) %>% 
  
  # select columns and reorder
  select(
    individual.uid, source.code, original.sources, taxa.name,
    nu, ind.per.nu, pg.c, mass.c, mass.d, mass, biovolume, mld,
    ott.id, type, species, genus, family, order, class, phylum, kingdom,
    r.group.species, r.group.genus, r.group.family, r.group.order, r.group.class, r.group.phylum, group,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  # remove any without a mass measurement (mass.c and mass.d should have the same amount so can filter by either)
  mutate(
    remove = case_when(
      is.na(mass.c) & is.na(mass) & is.na(biovolume) ~ "remove",
      TRUE ~ "keep"
    )
  ) %>% 
  
  filter(
    remove == "keep"
  )

## Remove only multi-cellular ----
# Find any that only have multi-cellular values but not single cell values
multi_ind <- mass %>% 
  
  group_by(nu, taxa.name) %>% 
  
  distinct(taxa.name, .keep_all = TRUE) %>% 
  
  ungroup() %>% 
  
  group_by(taxa.name) %>% 
  
  mutate(
    n = n()
  ) %>% 
  
  filter(
    n == 1,
    nu == "multi-cellular"
  ) %>% 
  
  ungroup()

# Remove ones with no single cell values ----
bodysize <- mass %>% 
  
  filter(
    !(individual.uid %in% multi_ind$individual.uid)
  )

# save
saveRDS(bodysize, file = "R/Data_outputs/database_products/final_products/bodysize.rds")






# Select phyto ----

phyto_all <- bodysize %>% 
  
  filter(
    type == "Phytoplankton"
  ) 

# save
saveRDS(phyto_all, file = "R/Data_outputs/database_products/final_products/phyto_all.rds")

# Phyto subset ----
# select just species and genus level

phyto_subset <- phyto_all %>% 
  
  filter(
    !is.na(genus)
  ) 

# save
saveRDS(phyto_subset, file = "R/Data_outputs/database_products/final_products/phyto_subset.rds")





