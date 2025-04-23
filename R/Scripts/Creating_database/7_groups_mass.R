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

# Formatting data ----

## Import data ----
bodysize_location <- readRDS("R/Data_outputs/database_products/bodysize_location.rds")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")
sources_list_update <- readRDS("R/Data_outputs/database_products/sources_list_update.rds")

## All data ----
# Final list of all data including phyto and zooplankton

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
bodysize_formatted <- format_all_bs %>% 
  
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
  
  ungroup()


# save
saveRDS(bodysize_formatted, file = "R/Data_outputs/database_products/final_products/bodysize_formatted.rds")


# Functional groups ----
# Only have phyto functional groups but need to groups to calulate phyto mass so do it now and then can have one data set of masses

## Import data ----

rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet") # 2018, padisak and reynolds
kruk <- read_xlsx("raw_data/kruk_groups.xlsx") # 2017, reynolds
padisak <- read_xlsx("raw_data/functional_groups_padisak.xlsx", sheet = "groups") # 2009, padisak

## Format ----

### padisak ----
padisak_format <- padisak %>% 
  
  # Put each species on its own line
  separate(taxa.name, sep = ",", into = c("taxa.name.1", "taxa.name.2", "taxa.name.3", "taxa.name.4", "taxa.name.5", "taxa.name.6", "taxa.name.7", "taxa.name.8", "taxa.name.9", "taxa.name.10", "taxa.name.11", "taxa.name.12", "taxa.name.13", "taxa.name.14", "taxa.name.15", "taxa.name.16", "taxa.name.17", "taxa.name.18", "taxa.name.19", "taxa.name.20", "taxa.name.21", "taxa.name.22", "taxa.name.23", "taxa.name.24", "taxa.name.25")) %>% 
  
  pivot_longer(
    cols = taxa.name.1:taxa.name.25,
    values_to = "taxa.name"
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  mutate(
    taxa.name = str_trim(taxa.name)
  ) %>% 
  
  select(
    - name
  ) %>% 
  
  group_by(
    taxa.name, padisak.r.group, reynolds.group
  ) %>% 
  
  distinct(
    taxa.name
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    ref = "padisak"
  )

### Kruk ----
kruk_format <- kruk %>% 
  
  select(
    Species_name,
    `Classification by Experts`,
  ) %>% 
  
  rename(
    taxa.name = Species_name,
    reynolds.group = `Classification by Experts`
  ) %>%
  
  group_by(
    taxa.name, reynolds.group
  ) %>% 
  
  distinct(
    taxa.name
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    ref = "kruk"
  )

### rimmet ----
rimmet_format <- rimmet %>% 
  
  select(
    `Genus + species name`,
    `Functional groups (Reynolds 2002)`,
    `Functional groups (Padisak 2009)`
  ) %>% 
  
  rename(
    taxa.name = `Genus + species name`,
    reynolds.group = `Functional groups (Reynolds 2002)`,
    padisak.r.group = `Functional groups (Padisak 2009)`
  ) %>% 
  
  group_by(
    taxa.name, padisak.r.group, reynolds.group
  ) %>% 
  
  distinct(
    taxa.name
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    ref = "rimmet"
  )

### Join ----
names_list <- bind_rows(padisak_format, rimmet_format, kruk_format) %>% 
  select(
    taxa.name
  )

## Join padisaks together ----
padisak_raw <- bind_rows(padisak_format, rimmet_format) %>% 
  
  select(
    - reynolds.group
  ) %>% 
  
  group_by(
    taxa.name, padisak.r.group
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  pivot_wider(
    names_from = ref,
    values_from = c(padisak.r.group),
    values_fn = function(x) paste(unique(x), collapse = "/")
  ) %>% 
  
  rename(
    `r.group.padisak (padisak 2009)` = padisak,
    `r.group.padisak (rimmet 2018)` = rimmet
  )

## Join reynolds together ----
reynolds_raw <- bind_rows(padisak_format, rimmet_format, kruk_format) %>% 
  
  select(
    - padisak.r.group
  ) %>% 
  
  group_by(
    taxa.name, reynolds.group
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  pivot_wider(
    names_from = ref,
    values_from = c(reynolds.group),
    values_fn = function(x) paste(unique(x), collapse = "/")
  ) %>% 
  
  rename(
    `r.group.reynolds (padisak 2009)` = padisak,
    `r.group.reynolds (rimmet 2018)` = rimmet,
    `r.group.reynolds (kruk 2017)` = kruk,
  )

## Add to names list ----
functional_raw <- names_list %>% 
  
  left_join(
    reynolds_raw, by = "taxa.name"
  ) %>% 
  
  left_join(
    padisak_raw, by = "taxa.name"
  ) %>% 
  
  rename(
    old.taxa.name = taxa.name
  )

## Update taxa.names ----
# These have the names pre taxonomy step so need to update them
# Rimmet and Kruk data have already gone through the taxonomy step so can just add in the names from the bodysize_taxonomy data
# Padisak hasn't gone through the taxonomy step so need to do these

### Import data ----

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

### update names ----

names_list_updated <- functional_raw %>% 
  
  select(
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

### Run remaning names through taxonomy steps ----

#### clean ----
# Get a list of all distinct names with no taxa.name
names_list_clean <- names_list_updated %>% 
  
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
glimpse(names_list_clean)

### run through gna_verifier ----
# Run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
cleaned_gna <- do.call(rbind, lapply(names_list_clean, function(name) {
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

### Run through tnrs_match_names ----
# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)

resolved_tol <- tnrs_match_names(cleaned$cleaned.taxa.name) %>% 
  
  select(
    unique_name,
    search_string
  )

### Add to main list

add_list <- cleaned %>% 
  
  mutate(
    cleaned.taxa.name = tolower(cleaned.taxa.name)
  ) %>% 
  
  left_join(
    ., resolved_tol, by = c("cleaned.taxa.name" = "search_string")
  ) %>% 
  
  select(
    original.taxa.name,
    unique_name
  ) %>% 
  
  rename(
    taxa.name = unique_name,
    old.taxa.name = original.taxa.name
  )

# add to functional_raw
names_list_final <- names_list_updated %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  bind_rows(
    add_list
  )

# functional
functional <- functional_raw %>% 
  
  left_join(
    names_list_final, by = "old.taxa.name"
  ) %>% 
  
  select(
    - old.taxa.name
  ) %>% 
  
  # select preference - first padisak from rimmet then padisak from padisak then reynolds in date order
  mutate(
    
    r.group.source = case_when(
      !(is.na(`r.group.padisak (rimmet 2018)`)) ~ "rimmet(2018)",
      is.na(`r.group.padisak (rimmet 2018)`) & !(is.na(`r.group.padisak (padisak 2009)`)) ~ "padisak(2009)",
      is.na(`r.group.padisak (rimmet 2018)`) & is.na(`r.group.padisak (padisak 2009)`) & !(is.na(`r.group.reynolds (rimmet 2018)`)) ~ "rimmet(2018)",
      is.na(`r.group.padisak (rimmet 2018)`) & is.na(`r.group.padisak (padisak 2009)`) & is.na(`r.group.reynolds (rimmet 2018)`) & !(is.na(`r.group.reynolds (kruk 2017)`)) ~ "kruk(2017)",
      is.na(`r.group.padisak (rimmet 2018)`) & is.na(`r.group.padisak (padisak 2009)`) & is.na(`r.group.reynolds (rimmet 2018)`) & is.na(`r.group.reynolds (kruk 2017)`) & !(is.na(`r.group.reynolds (padisak 2009)`)) ~ "padisak(2009)",
      
      TRUE ~ NA
    ),
    
    r.group = case_when(
      !(is.na(`r.group.padisak (rimmet 2018)`)) ~ `r.group.padisak (rimmet 2018)`,
      is.na(`r.group.padisak (rimmet 2018)`) & !(is.na(`r.group.padisak (padisak 2009)`)) ~ `r.group.padisak (padisak 2009)`,
      is.na(`r.group.padisak (rimmet 2018)`) & is.na(`r.group.padisak (padisak 2009)`) & !(is.na(`r.group.reynolds (rimmet 2018)`)) ~ `r.group.reynolds (rimmet 2018)`,
      is.na(`r.group.padisak (rimmet 2018)`) & is.na(`r.group.padisak (padisak 2009)`) & is.na(`r.group.reynolds (rimmet 2018)`) & !(is.na(`r.group.reynolds (kruk 2017)`)) ~ `r.group.reynolds (kruk 2017)`,
      is.na(`r.group.padisak (rimmet 2018)`) & is.na(`r.group.padisak (padisak 2009)`) & is.na(`r.group.reynolds (rimmet 2018)`) & is.na(`r.group.reynolds (kruk 2017)`) & !(is.na(`r.group.reynolds (padisak 2009)`)) ~ `r.group.reynolds (padisak 2009)`,
      
      TRUE ~ NA
    )
  ) %>% 
  
  left_join(
    tax_list_raw, ., by = "taxa.name"
  ) %>% 
  
  select(
    - resolved.taxa.name, - type
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
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
  ) %>% 
  
  select(
    taxa.name, ott.id, r.group, r.group.source, species, genus, family, order, class, phylum, kingdom, domain, group
  ) %>% 
  
  # Remove any without a group
  filter(
    !(is.na(group))
  ) %>% 
  
  # make all r.groups upper case
  mutate(
    r.group = toupper(r.group)
  )

saveRDS(functional, "R/data_outputs/database_products/functional.rds")

### Add groups to main data ----
data_groups <- bodysize_formatted %>% 
  
  left_join(
    select(
      functional, group, taxa.name
    ), by = "taxa.name"
  )
  
# save
saveRDS(data_groups, "R/data_outputs/database_products/data_groups.rds")



# Calculating masses ----

## Calculate ----
data_groups_mass <- data_groups %>% 
  
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
      data_groups, - body.size, bodysize.measurement, - uid
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
    group,
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

# Find any that only have multi-cellular values but not single cell values
multi_ind <- data_groups_mass %>% 
  
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
bodysize <- data_groups_mass %>% 
  
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

# genus
functional_genus <- functional %>% 
  
  rename(
    r.group.genus = r.group
  ) %>% 
  
  filter(
    !is.na(r.group.genus),
    is.na(species),
    !is.na(genus)
  ) %>% 
  
  distinct(
    genus, .keep_all = TRUE
  ) %>% 
  
  group_by(
    genus
  ) %>% 
  
  mutate(
    genus.uid = cur_group_id()
  ) %>% 
  
  ungroup()

# species
functional_species <- functional %>% 
  
  rename(
    r.group.species = r.group
  ) %>% 
  
  filter(
    !is.na(r.group.species),
    !is.na(species)
  ) %>% 
  
  group_by(
    species
  ) %>% 
  
  mutate(
    n = n()
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    remove = if_else(
      n > 1 & stri_detect_regex(taxa.name, "var\\."),
      "remove",
      "keep"
    )
  ) %>% 
  
  filter(
    remove == "keep"
  ) %>% 
  
  group_by(species) %>% 
  
  mutate(
    species.uid = cur_group_id()
  ) %>% 
  
  ungroup()
  
  

phyto_subset <- phyto_all %>% 
  
  filter(
    !is.na(genus)
  ) %>% 
  
  left_join(
    select(
      functional_species, species.uid, species
    ), by = "species"
  ) %>% 
  
  left_join(
    select(
      functional_genus, genus.uid, genus
    ), by = "genus"
  ) %>% 
  
  left_join(
    select(
      functional_species, species.uid, r.group.species
    ), by = "species.uid"
  ) %>% 
  
  left_join(
    select(
      functional_genus, genus.uid, r.group.genus
    ), by = "genus.uid"
  ) %>% 
  
  # make a rank column to know which ones are species and which are to genus level
  mutate(
    rank = if_else(
      is.na(species),
      "Genus",
      "Species"
    )
  )

# save
saveRDS(phyto_subset, file = "R/Data_outputs/database_products/final_products/phyto_subset.rds")

