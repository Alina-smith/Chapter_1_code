## Calculating the body size for ones I don't have it for
# all volum = um3
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
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name.full, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    tax.uid, species, genus, family, order, class, phylum, kingdom,
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
    tax.uid, source.code, original.sources, location.code, bodysize.measurement, life.stage, sex, sample.year, sample.month, nu
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
      tax_list_raw, by = "tax.uid"
  ) %>% 
  
  left_join(
    select(
      sources_list_update, doi, source.code, new.source.code
    ), by = c("source.code" = "new.source.code")
  ) %>% 
  
  # Make new individual.uid
  group_by(
    tax.uid, source.code, original.sources, location.code, life.stage, sex, sample.year, sample.month, nu
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
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name.full, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    tax.uid, species, genus, family, order, class, phylum, kingdom,
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
  )

# save
saveRDS(bodysize_formatted, file = "R/Data_outputs/database_products/final_products/bodysize_formatted.rds")

## Select phyto data ----

phyto_formatted <- bodysize_formatted %>% 
  
  mutate(
    
    # set depth in bodysize.measurment as height as there is no overlap between these
    bodysize.measurement = if_else(
      bodysize.measurement == "depth",
      "height",
      bodysize.measurement
    )
  ) %>% 
  
  # Select adult/active phytoplankton
  
  filter(
    type == "Phytoplankton",
    life.stage == "active" # select just active and not dormant
  ) %>% 
  
  select(
    
    # don't need life.stage and sex for phyto as all active and don't have sex info
    - life.stage,
    - sex,
    
    # don't have error data for majority so leaving it out
    - error,
    - error.type,
    - reps,
    - sample.size,
    
    - bodysize.measurement.notes, # not needed
    - type # all phyto
  )

# save
saveRDS(phyto_formatted, file = "R/Data_outputs/database_products/final_products/phyto_formatted.rds")

# Functional groups ----
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
      is.na(padisak.group) & !is.na(r.group.kruk) ~ "152",
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

functional_groups <- tax_list_raw %>% 
  
  filter(
    type == "Phytoplankton",
    !is.na(genus)
  ) %>% 
  
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
    taxa.name.full, taxa.name, species, genus, family, order, class, phylum, kingdom, r.group, r.group.source
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
      
      TRUE ~ NA
    )
  ) 

# save
saveRDS(functional_groups, "R/data_outputs/database_products/final_products/functional_groups.rds")

## Add to main data ----
phyto_traits <- phyto_formatted %>% 
  
  left_join(
    select(
      functional_groups, r.group, r.group.source, group, taxa.name
    ), by = "taxa.name"
  )

# save
saveRDS(phyto_traits, "R/data_outputs/database_products/final_products/phyto_traits.rds")


# Calculating masses ----

phyto_mass <- phyto_traits %>% 
  
  # Calculate cell mass
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
      phyto_traits, - body.size, bodysize.measurement, - uid
      ), by = "individual.uid"
  ) %>% 
  
  # Rename from body to cell
  rename(
    cells.per.nu = ind.per.nu
  ) %>% 
  
  mutate(
    nu = if_else(
      nu == "individual",
      "cell",
      nu
    )
  ) %>% 
  
  distinct(
    individual.uid, .keep_all = TRUE
  ) %>% 
  
  mutate(
    # very few dry mass so just get rid of them
    # When biovolume is given use this over dry/wet mass
    dry.mass = if_else(
      !is.na(biovolume) & !is.na(dry.mass),
      dry.mass == NA,
      dry.mass
    )
  ) %>% 
  
  # Remove only dry mass
  filter(
    is.na(dry.mass)
  ) %>% 
  
  select(
    -dry.mass
  ) %>% 
  
  mutate(
    # calculate biovolume for ones that have just body mass
    biovolume = case_when(
      is.na(biovolume) & !is.na(body.mass) ~ body.mass/(1*10^-6),
      TRUE ~ biovolume
    ),
    
    # calculate mass from biovolume
    
    # 1) go through pg c
    # first convert to pg C
    pg.c = case_when(
      group == "Diatom" ~ 0.288*biovolume^0.811, # diatom specific one
      TRUE ~ 0.216*biovolume^0.939 # general phytoplankton one
    ),
    
    # Next convert to mass
    mass.c = case_when(
      !is.na(body.mass) ~ body.mass,
      is.na(body.mass) ~ 1e-12*0.07*pg.c
    ),
    
    # 2) assume density of 1
    mass.d = case_when(
      !is.na(body.mass) ~ body.mass,
      is.na(body.mass) ~ biovolume*(1*10^-6),
      TRUE ~ NA
    ),
    
    # make a mld column
    mld = pmax(length, width, height, diameter, na.rm = TRUE)
  ) %>% 
  
  # select columns and reorder
  select(
    individual.uid, source.code, original.sources, taxa.name.full, taxa.name,
    nu, cells.per.nu, pg.c, mass.c, mass.d, biovolume, mld,
    tax.uid, species, genus, family, order, class, phylum, kingdom,
    r.group, r.group.source, group,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  # remove any without a mass measurement (mass.c and mass.d should have the same amount so can filter by either)
  filter(
    !is.na(mass.c)
  )

# Find any that only have multi-cellular values but not single cell values
multi_cell <- phyto_mass %>% 
  
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
phyto_mass <- phyto_mass %>% 
  
  filter(
    !(individual.uid %in% multi_cell$individual.uid)
  )

# save
saveRDS(phyto_mass, file = "R/Data_outputs/database_products/final_products/phyto_mass.rds")

# Select ones just to species/genus level ----
phyto_mass_subset <- phyto_mass %>% 
  
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
saveRDS(phyto_mass_subset, file = "R/Data_outputs/database_products/final_products/phyto_mass_subset.rds")










# filter outliers for cell values
# split into cell and multi cell

cell <- phyto_mass_subset %>% 
  
  filter(
    nu == "cell"
  )

outliers <- cell %>% 
  # filter out outliers - over 2 st from mean of log
  
  mutate(
    log.mass = log10(mass),
    log.biovol = log10(biovolume)
  ) %>% 
  
  group_by(taxa.name) %>% 
  
  mutate(
    avg.log.biovol = mean(log.biovol),
    st_d = sd(log.biovol),
    upp_limit = avg.log.biovol+(st_d*2),
    low_limit = avg.log.biovol-(st_d*2)
  ) %>% 
  
  ungroup() %>% 
  
  filter(
    low_limit == upp_limit
  )
  
  filter(
    log.biovol <= low_limit & log.biovol >= upp_limit
  )








  

multi_cellular <- phyto_mass_subset %>% 
  
  filter(
    nu == "multi-cellular"
  )

phyto_mass_genus_species <- cell %>% 
  
  # filter out outliers - over 2 st from mean of log
  
  mutate(
    log.mass = log10(mass),
    log.biovol = log10(biovolume)
  ) %>% 
  
  group_by(taxa.name) %>% 
  
  mutate(
    avg.log.biovol = mean(log.biovol),
    st_d = sd(log.biovol),
    upp_limit = avg.log.biovol+(st_d*2),
    low_limit = avg.log.biovol-(st_d*2)
  ) %>% 
  
  ungroup() %>% 
  
  filter(
    log.biovol >= low_limit & log.biovol <= upp_limit
  ) %>% 
  
  select(
    individual.uid, source.code, original.sources, taxa.name.full, taxa.name,
    nu, cells.per.nu, mass, biovolume, mld,
    tax.uid, rank, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  # add the multi cellular measurments back in
  bind_rows(
    ., multi_cellular
  )



# save
saveRDS(phyto_mass_genus_species, file = "R/Data_outputs/database_products/final_products/phyto_mass_genus_species.rds")



