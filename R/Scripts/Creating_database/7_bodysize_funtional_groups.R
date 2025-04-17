## Calculating the body size for ones I don't have it for
# all volume = um3
# all length, width and height = um,
# all mass = ug

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Formatting data ----

# Import data ----
bodysize_location <- readRDS("R/Data_outputs/database_products/bodysize_location.rds")
bodysize_taxonomy <- readRDS("R/Data_outputs/database_products/bodysize_taxonomy.rds")
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


# temp bodysizes ----

temp_bodysize_1 <- bodysize_formatted %>% 
  
  filter(
    type == "Phytoplankton"
  ) %>% 
  
  mutate(
    group = if_else(
      phylum == "Bacillariophyta",
      "Diatom",
      "Not diatom"
    )
  )

temp_bodysize <- temp_bodysize_1 %>% 
  
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
      temp_bodysize_1, - body.size, bodysize.measurement, - uid
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
    
    # make a mld column
    mld = pmax(length, width, height, diameter, na.rm = TRUE)
  ) %>% 
  
  # select columns and reorder
  select(
    individual.uid, source.code, original.sources, taxa.name,
    nu, ind.per.nu, pg.c, mass.c, mass.d, biovolume, mld,
    ott.id, type, species, genus, family, order, class, phylum, kingdom,
    group,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  # remove any without a mass measurement (mass.c and mass.d should have the same amount so can filter by either)
  mutate(
    remove = case_when(
      is.na(mass.c) & is.na(biovolume) ~ "remove",
      TRUE ~ "keep"
    )
  ) %>% 
  
  filter(
    remove == "keep"
  )

# Find any that only have multi-cellular values but not single cell values
multi_ind <- temp_bodysize %>% 
  
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
bodysize_phyto <- temp_bodysize %>% 
  
  filter(
    !(individual.uid %in% multi_ind$individual.uid)
  )

# save
saveRDS(bodysize_phyto, file = "R/Data_outputs/database_products/final_products/bodysize_phyto.rds")


# Select ones just to species/genus level ----
phyto_subset <- bodysize_phyto %>% 
  
  filter(
    !is.na(genus)
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



















# Functional groups ----
# Only have phyto functional groups but need to groups to calulate phyto mass so do it now and then can have one data set of masses

## Import data ----
padisak <- read_xlsx("raw_data/functional_groups_padisak.xlsx", sheet = "groups")

## Format data ----

padisak_format <- padisak %>% 
  
  separate(Species, sep = ",", into = c("species.1", "species.2", "species.3", "species.4", "species.5", "species.6", "species.7", "species.8", "species.9", "species.10", "species.11", "species.12", "species.13", "species.14", "species.15", "species.16", "species.17", "species.18", "species.19", "species.20", "species.21", "species.22", "species.23", "species.24", "species.25")) %>% 
  
  pivot_longer(
    cols = species.1:species.25,
    values_to = "species"
  ) %>% 
  
  filter(
    !is.na(species)
  ) %>% 
  
  mutate(
    species = str_trim(species)
  ) %>% 
  
  select(
    - name
  ) %>% 
  
  relocate(
    uid, species
  )

padisak_edits <- padisak_format %>% 
  
  select(
    uid, species, Note
  ) %>% 
  
  filter(
    !is.na(Note)
  )

write_csv(padisak_edits, "R/data_outputs/database_products/padisak_edits.csv")
  
  mutate(
    # when the new code is NA make it the old code
    # redo sources
    new.codon.sources = if_else(
      is.na(new.codon),
      sources,
      "Padisak (2009)"
    ),
    
    new.codon = if_else(
      is.na(new.codon),
      old.codon,
      new.codon
    )
  ) %>% 
  
  str_c(Species)
    
    
    
    ,
    species.spec = stri_replace_all_regex(species, "[^\\x20-\\x7E]", "*SpecChar*")
  )

%>% 
  
  select(
    - old.codon,
    - sources
  )

x <- padisak_format %>% 
  group_by(
    species
  ) %>% 
  
  mutate(
    n = n()
  ) %>% 
  filter(
    n>1
  )

Ceratium hirundinella
Chlorella sp.
Chroococcus minor
Cosmarium sp.
Cyanodictyon
Cyclotella comta
Cyclotella ocellata
Fragilaria
Glenodinium
Gymnodinium
Microcystis
Microcystis flos-aquae

Microcystis wesenbergii
Navicula
Nitzschia sp.
Ochromonas
Oscillatoria
Oscillatoria sp.
Phacotus
Picoplankton
Planktolyngbya
Planktothrix
Planktothrix agardhii
Pseudanabaena
Scenedesmus ellipticus
Staurodesmus
Stephanodiscus rotula
Synechococcus spp.
Synura


























## Import data ----
rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet")
lt <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture")
kruk <- read_xlsx("raw_data/kruk_groups.xlsx")

updated_spec_char <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "special_characters")

## Update original.taxa.names ----
# need to update the names to match the new ones from the taxonomy step

# 1) Select desired columns from databases
# Rimmet
rimmet_names <- rimmet %>% 
  select(
    `Genus + species name`
  ) %>% 
  rename(
    old.taxa.name = `Genus + species name`
  ) %>% 
  distinct(
    old.taxa.name
  )

# Laplace-treyture
lt_names <- lt %>% 
  select(
    Taxa_Name
  ) %>% 
  rename(
    old.taxa.name = Taxa_Name
  ) %>% 
  distinct(
    old.taxa.name
  )

# Kruk
kruk_names <- kruk %>% 
  select(
    Species_name
  ) %>% 
  rename(
    old.taxa.name = Species_name
  ) %>% 
  distinct(
    old.taxa.name
  )

# 2) Update names

# Combine together
names_list_ft <- bind_rows(rimmet_names, lt_names, kruk_names) %>% 
  
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
  )

## Get list of R groups ----

### Kruk ----
kruk_group <- kruk %>% 
  
  select(
    Species_name,
    `Classification by Experts`,
  ) %>% 
  
  rename(
    old.taxa.name = Species_name,
    reynolds.group = `Classification by Experts`
  ) %>% 
  
  distinct(
    old.taxa.name, .keep_all = TRUE
  ) %>% 
  
  # add in the new names
  left_join(names_list_ft, by = "old.taxa.name") %>% 
  
  # remove original.taxa.name
  select(-old.taxa.name) %>% 
  
  # get only distinct non NA names
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  # edit format to make it fir twith everything else
  mutate(
    reynolds.group = toupper(reynolds.group),
    
    paper = "kruk"
  )


### Rimmet ----
rimmet_group <- rimmet %>% 
  
  select(
    `Genus + species name`,
    `Functional groups (Reynolds 2002)`,
    `Functional groups (Padisak 2009)`
  ) %>% 
  
  rename(
    old.taxa.name = `Genus + species name`,
    reynolds.group = `Functional groups (Reynolds 2002)`,
    padisak.group = `Functional groups (Padisak 2009)`
  ) %>% 
  
  distinct(
    old.taxa.name, .keep_all = TRUE
  ) %>% 
  
  # add in the new names
  left_join(names_list_ft, by = "old.taxa.name") %>% 
  
  # remove original.taxa.name
  select(-old.taxa.name) %>% 
  
  # get only distinct non NA names
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  # edit format to make it fir twith everything else
  mutate(
    reynolds.group = toupper(reynolds.group),
    
    paper = "rimmet"
  )

### LT ----
lt_group <- lt %>% 
  
  select(
    Taxa_Name,
    Reynolds_Group,
  ) %>% 
  
  rename(
    old.taxa.name = Taxa_Name,
    reynolds.group = Reynolds_Group,
  ) %>% 
  
  filter(
    !(reynolds.group == "#NA")
  ) %>% 
  
  distinct(
    old.taxa.name, .keep_all = TRUE
  ) %>% 
  
  # add in the new names
  left_join(names_list_ft, by = "old.taxa.name") %>% 
  
  # remove original.taxa.name
  select(-old.taxa.name) %>% 
  
  # get only distinct non NA names
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  # edit format to make it fir twith everything else
  mutate(
    reynolds.group = toupper(reynolds.group),
    
    paper = "lt"
  )

## Join together ----
# Make one big list of groups

r_groups <- bind_rows(kruk_group, rimmet_group, lt_group) %>% 
  
  # Pivot so each group from each paper is in its own column
  select(
    -padisak.group
  ) %>% 
  
  pivot_wider(
    names_from = paper,
    values_from = reynolds.group
  ) %>% 
  
  rename(
    r.group.kruk = kruk,
    r.group.rimmet = rimmet,
    r.group.lt = lt
  ) %>% 
  
  # add in the padisak groups
  left_join(
    select(
      rimmet_group, taxa.name, padisak.group
    ), by = "taxa.name"
  ) %>% 
  
  # select the r group, prioritise padisak and then most up to dat
  mutate(
    r.group = case_when(
      !is.na(padisak.group) ~ padisak.group,
      is.na(padisak.group) & !is.na(r.group.kruk) ~ r.group.kruk,
      is.na(padisak.group) & is.na(r.group.kruk) & !is.na(r.group.rimmet) ~ r.group.rimmet,
      is.na(padisak.group) & is.na(r.group.kruk) & is.na(r.group.rimmet) & !is.na(r.group.lt) ~ r.group.lt,
      
      TRUE ~ NA
    ),
    
    # make source column
    r.group.source = case_when(
      !is.na(padisak.group) ~ "1",
      is.na(padisak.group) & !is.na(r.group.kruk) ~ "153",
      is.na(padisak.group) & is.na(r.group.kruk) & !is.na(r.group.rimmet) ~ "1",
      is.na(padisak.group) & is.na(r.group.kruk) & is.na(r.group.rimmet) & !is.na(r.group.lt) ~ "80",
      
      TRUE ~ NA
    )
  ) %>% 
  
  # select columns
  select(
    taxa.name,
    r.group,
    r.group.source
  )

## Add to taxonomy info ----
# Make a list of all taxa in the data and add in the functional group info
# When there isn't functional group info for that data put unnasinged

functional_groups_raw <- tax_list_raw %>% 
  
  left_join(
    r_groups, by = "taxa.name"
  ) %>% 
  
  left_join(
    r_groups, by = c("genus" = "taxa.name"), suffix = c(".species", ".genus")
  ) %>% 
  
  left_join(
    r_groups, by = c("family" = "taxa.name")
  ) %>% 
  
  left_join(
    r_groups, by = c("order" = "taxa.name"), suffix = c(".family", ".order")
  ) %>% 
  
  left_join(
    r_groups, by = c("class" = "taxa.name")
  ) %>% 
  
  left_join(
    r_groups, by = c("phylum" = "taxa.name"), suffix = c(".class", ".phylum")
  ) %>% 
  
  mutate(
    r.group = case_when(
      !is.na(r.group.species) ~ r.group.species,
      is.na(r.group.species) & !is.na(r.group.genus) ~ r.group.genus,
      is.na(r.group.species) & is.na(r.group.genus) & !is.na(r.group.family) ~ r.group.family,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & !is.na(r.group.order) ~ r.group.order,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & !is.na(r.group.class) ~ r.group.class,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & is.na(r.group.class) & !is.na(r.group.phylum) ~ r.group.phylum,
      
      TRUE ~ "Unassigned"
    ),
    
    r.group.source = case_when(
      !is.na(r.group.source.species) ~ r.group.source.species,
      is.na(r.group.source.species) & !is.na(r.group.source.genus) ~ r.group.source.genus,
      is.na(r.group.source.species) & is.na(r.group.source.genus) & !is.na(r.group.source.family) ~ r.group.source.family,
      is.na(r.group.source.species) & is.na(r.group.source.genus) & is.na(r.group.source.family) & !is.na(r.group.source.order) ~ r.group.source.order,
      is.na(r.group.source.species) & is.na(r.group.source.genus) & is.na(r.group.source.family) & is.na(r.group.source.order) & !is.na(r.group.source.class) ~ r.group.source.class,
      is.na(r.group.source.species) & is.na(r.group.source.genus) & is.na(r.group.source.family) & is.na(r.group.source.order) & is.na(r.group.source.class) & !is.na(r.group.source.phylum) ~ r.group.source.phylum,
      
      TRUE ~ NA
    ),
  ) %>% 
  
  select(
    ott.id, taxa.name, type, species, genus, family, order, class, phylum, kingdom, r.group, r.group.source
  ) %>% 
  
  # make a group column
  
  mutate(
    group = case_when(
      phylum %in% c("Cyanobacteria", "Glaucophyta") ~ "Blue/green",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Green",
      phylum == "Bacillariophyta" ~ "Diatom",
      phylum == "Rhodophyta" ~ "Red",
      phylum == "Euglenozoa" ~ "Euglenoid",
      phylum == "Cryptophyta" ~ "Cryptomonads",
      phylum == "Haptophyta" ~ "Haptophytes",
      
      class %in% c("Chrysophyceae", "Dictyochophyceae") ~ "Golden-brown",
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

## Find missing ----
# Find any that are missing an R group but have been assigned elsewhere
functional_groups <- functional_groups_raw %>% 
  
  r.group = case_when(
    genus == "Acanthoceras" ~ "A",
    genus == "Achnanthidium" ~ "Tb",
    
  )
  

# save
saveRDS(functional_groups, "R/data_outputs/database_products/final_products/functional_groups.rds")

## Add to main data ----
full_data_traits <- bodysize_formatted %>% 
  
  left_join(
    select(
      functional_groups, r.group, r.group.source, group, ott.id
    ), by = "ott.id"
  )

# save
saveRDS(full_data_traits, "R/data_outputs/database_products/full_data_traits.rds")


# Calculating masses ----

## Calculate ----
full_data_traits_mass <- full_data_traits %>% 
  
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
      full_data_traits, - body.size, bodysize.measurement, - uid
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
    r.group, r.group.source, group,
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
multi_ind <- full_data_traits_mass %>% 
  
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
bodysize <- full_data_traits_mass %>% 
  
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

# Select ones just to species/genus level ----
phyto_subset <- phyto_all %>% 
  
  filter(
    !is.na(genus)
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







