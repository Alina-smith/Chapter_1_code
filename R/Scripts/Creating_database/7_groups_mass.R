## Calculating the body sizes using length-weight and biovolume conversions
# Have decided to go to genus level as this provides the most data points and locations compared to species level
# focusing on adult individuals

# all volume = um3
# all length, width and height = um,
# all mass = ug

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(rotl)

# Final list of all data including phyto and zooplankton with groups and mass

# Import data ----
bodysize_formatted <- readRDS("R/Data_outputs/database_products/final_products/bodysize_formatted.rds")
traits_list_all <- readRDS("R/Data_outputs/database_products/traits_list_all.rds")

# Getting averages for each source ----

## Initial formatting ----
bs_format <- bodysize_formatted %>% 
  
  # remove columns not needed now
  select(
    - sample.size,
    - sample.year,
    - sample.month,
    - sex,
    - reps
  ) %>% 
  
  mutate(
    
    # Small misc edits
    
    # change types for later merging
    uid = as.character(uid),
    
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
  
  filter(
    # Select just adult individuals
    life.stage %in% c("adult", "active"),
    nu %in% "individual",
    
    # remove any that are higher than genus
    !is.na(genus)
  ) %>% 
  
  # select and relocate columns - removing life stage and nu now as all adult individuals
  
  select(
    individual.uid, uid, source.code, original.sources, type,
    nu, ind.per.nu, body.size, bodysize.measurement, units,
    ott.id, taxa.name, species, genus, family, order, class, phylum, kingdom,
    location.code, habitat, location, country, continent, latitude, longitude
  )

## Get averages ----

# Get extra info to add in
# All the extra info for the multiples of the same individual are the same and so can just left join by individual.uid
extra_info <- bs_format %>% 
  
  select(
    - body.size,
    - bodysize.measurement,
    - uid
  ) %>% 
  
  distinct(
    individual.uid, .keep_all = TRUE
  )

# Find averages
bs_avg <- bs_format %>% 

  # get an average for each individual.uid
  group_by(
    individual.uid, bodysize.measurement
  ) %>% 
  
  summarise(
    body.size = mean(body.size),
    .groups = "drop"
  ) %>% 
  
  left_join(
    extra_info, by = "individual.uid"
  ) %>% 
  
  # make a new uid column
  mutate(
    uid = row_number()
  )


## Genus ott ids ----
# get ott ids for genus to use for later on when agragating to genus
# Rerun names through tol now they are genus to get ott ids
genus_list <- bs_avg %>% 
  distinct(genus)

genus_ott <- tnrs_match_names(genus_list$genus)

# check all have been matched to an ott id - if false then okay
unique(is.na(genus_ott$ott_id))

# Add in ott_ids to data
bodysize_genus_ottid <- bs_avg %>% 
  
  # Set the taxa.name to lower case to match the search string
  mutate(
    genus = tolower(genus)
  ) %>% 
  
  left_join(
    select(
      genus_ott, search_string, unique_name, ott_id
    ), by = c("genus" = "search_string")
  ) %>% 
  
  # get rid of old genus and replace with new genus
  select(
    -genus
  ) %>% 
  
  rename(
    genus = unique_name,
    genus.ott.id = ott_id
  ) %>% 
  
  select(
    individual.uid, uid, source.code, original.sources, type,
    nu, ind.per.nu, body.size, bodysize.measurement, units,
    ott.id, genus.ott.id, taxa.name, species, genus, family, order, class, phylum, kingdom,
    location.code, habitat, location, country, continent, latitude, longitude
  )

saveRDS(bodysize_genus_ottid, "R/data_outputs/database_products/bodysize_genus_ottid.rds")


############################## Now have a dataset of raw bodysizes with just one per individual but not functional group data

# Add in trait info ----

# Add to data
bs_fg <- bodysize_genus_ottid %>% 
  
  # genus
  left_join(
      traits_list_all, by = "taxa.name"
  ) %>% 
  
  # family
  left_join(
      traits_list_all, by = c("family" = "taxa.name"),
      suffix = c(".s.g", ".family")
  ) %>% 
  
  # order
  left_join(
    traits_list_all, by = c("order" = "taxa.name")
  ) %>% 
  
  # class
  left_join(
    traits_list_all, by = c("class" = "taxa.name"),
      suffix = c(".order", ".class")
  ) %>% 

  # phylum
  left_join(
    traits_list_all, by = c("phylum" = "taxa.name")
  ) %>% 
  
  # select that group with the lowest res
  mutate(
    # morpho.class
    morpho.class = case_when(
      !is.na(morpho.class.s.g) ~ morpho.class.s.g,
      !is.na(morpho.class.family) ~ morpho.class.family,
      !is.na(morpho.class.order) ~ morpho.class.order,
      !is.na(morpho.class.class) ~ morpho.class.class,
      !is.na(morpho.class) ~ morpho.class,
      TRUE ~ NA
    ),
    morpho.class.source = case_when(
      !is.na(morpho.class.source.s.g) ~ morpho.class.source.s.g,
      !is.na(morpho.class.source.family) ~ morpho.class.source.family,
      !is.na(morpho.class.source.order) ~ morpho.class.source.order,
      !is.na(morpho.class.source.class) ~ morpho.class.source.class,
      !is.na(morpho.class.source) ~ morpho.class.source,
      TRUE ~ NA
    ),
    
    # fg
    fg = case_when(
      !is.na(fg.s.g) ~ fg.s.g,
      !is.na(fg.family) ~ fg.family,
      !is.na(fg.order) ~ fg.order,
      !is.na(fg.class) ~ fg.class,
      !is.na(fg) ~ fg,
      TRUE ~ NA
    ),
    fg.source = case_when(
      !is.na(fg.source.s.g) ~ fg.source.s.g,
      !is.na(fg.source.family) ~ fg.source.family,
      !is.na(fg.source.order) ~ fg.source.order,
      !is.na(fg.source.class) ~ fg.source.class,
      !is.na(fg.source) ~ fg.source,
      TRUE ~ NA
    ),
    
    # body.shape
    body.shape = case_when(
      !is.na(body.shape.s.g) ~ body.shape.s.g,
      !is.na(body.shape.family) ~ body.shape.family,
      !is.na(body.shape.order) ~ body.shape.order,
      !is.na(body.shape.class) ~ body.shape.class,
      !is.na(body.shape) ~ body.shape,
      TRUE ~ NA
    ),
    body.shape.source = case_when(
      !is.na(body.shape.source.s.g) ~ body.shape.source.s.g,
      !is.na(body.shape.source.family) ~ body.shape.source.family,
      !is.na(body.shape.source.order) ~ body.shape.source.order,
      !is.na(body.shape.source.class) ~ body.shape.source.class,
      !is.na(body.shape.source) ~ body.shape.source,
      TRUE ~ NA
    ),
    
    # motility
    motility = case_when(
      !is.na(motility.s.g) ~ motility.s.g,
      !is.na(motility.family) ~ motility.family,
      !is.na(motility.order) ~ motility.order,
      !is.na(motility.class) ~ motility.class,
      !is.na(motility) ~ motility,
      TRUE ~ NA
    ),
    motility.source = case_when(
      !is.na(motility.source.s.g) ~ motility.source.s.g,
      !is.na(motility.source.family) ~ motility.source.family,
      !is.na(motility.source.order) ~ motility.source.order,
      !is.na(motility.source.class) ~ motility.source.class,
      !is.na(motility.source) ~ motility.source,
      TRUE ~ NA
    ),
    
    # motility.method
    motility.method = case_when(
      !is.na(motility.method.s.g) ~ motility.method.s.g,
      !is.na(motility.method.family) ~ motility.method.family,
      !is.na(motility.method.order) ~ motility.method.order,
      !is.na(motility.method.class) ~ motility.method.class,
      !is.na(motility.method) ~ motility.method,
      TRUE ~ NA
    ),
    motility.method.source = case_when(
      !is.na(motility.method.source.s.g) ~ motility.method.source.s.g,
      !is.na(motility.method.source.family) ~ motility.method.source.family,
      !is.na(motility.method.source.order) ~ motility.method.source.order,
      !is.na(motility.method.source.class) ~ motility.method.source.class,
      !is.na(motility.method.source) ~ motility.method.source,
      TRUE ~ NA
    ),
    
    # trophic.group
    trophic.group = case_when(
      !is.na(trophic.group.s.g) ~ trophic.group.s.g,
      !is.na(trophic.group.family) ~ trophic.group.family,
      !is.na(trophic.group.order) ~ trophic.group.order,
      !is.na(trophic.group.class) ~ trophic.group.class,
      !is.na(trophic.group) ~ trophic.group,
      TRUE ~ NA
    ),
    trophic.group.source = case_when(
      !is.na(trophic.group.source.s.g) ~ trophic.group.source.s.g,
      !is.na(trophic.group.source.family) ~ trophic.group.source.family,
      !is.na(trophic.group.source.order) ~ trophic.group.source.order,
      !is.na(trophic.group.source.class) ~ trophic.group.source.class,
      !is.na(trophic.group.source) ~ trophic.group.source,
      TRUE ~ NA
    ),
    
    # life.form
    life.form = case_when(
      !is.na(life.form.s.g) ~ life.form.s.g,
      !is.na(life.form.family) ~ life.form.family,
      !is.na(life.form.order) ~ life.form.order,
      !is.na(life.form.class) ~ life.form.class,
      !is.na(life.form) ~ life.form,
      TRUE ~ NA
    ),
    life.form.source = case_when(
      !is.na(life.form.source.s.g) ~ life.form.source.s.g,
      !is.na(life.form.source.family) ~ life.form.source.family,
      !is.na(life.form.source.order) ~ life.form.source.order,
      !is.na(life.form.source.class) ~ life.form.source.class,
      !is.na(life.form.source) ~ life.form.source,
      TRUE ~ NA
    ),
    
    # feeding.type
    feeding.type = case_when(
      !is.na(feeding.type.s.g) ~ feeding.type.s.g,
      !is.na(feeding.type.family) ~ feeding.type.family,
      !is.na(feeding.type.order) ~ feeding.type.order,
      !is.na(feeding.type.class) ~ feeding.type.class,
      !is.na(feeding.type) ~ feeding.type,
      TRUE ~ NA
    ),
    feeding.type.source = case_when(
      !is.na(feeding.type.source.s.g) ~ feeding.type.source.s.g,
      !is.na(feeding.type.source.family) ~ feeding.type.source.family,
      !is.na(feeding.type.source.order) ~ feeding.type.source.order,
      !is.na(feeding.type.source.class) ~ feeding.type.source.class,
      !is.na(feeding.type.source) ~ feeding.type.source,
      TRUE ~ NA
    )
  ) %>% 
  
  select(
    !(matches(".s.g|.family|.order|.class|.phylum"))
  ) %>% 
  
  # assign traditional groups 
  mutate(
    taxonomic.group = case_when(
      # Phyto
      phylum == "Cyanobacteria" ~ "Blue-green",
      phylum == "Glaucophyta" ~ "Glaucocystid",
      phylum %in% c("Spironematellophyta", "Chlorophyta") ~ "Green",
      class == "Bacillariophyceae" ~ "Diatom",
      phylum == "Euglenophyta" ~ "Euglenoid-flagellate",
      phylum == "Cryptophyta" ~ "Cryptomonad",
      phylum == "Haptophyta" ~ "Haptophyte",
      phylum == "Dinoflagellata" ~ "Dino-flagellate",
      phylum == "Charophyta" ~ "Charophyte",
      phylum == "Heterokontophyta" ~ "Heterokont",
      phylum == "Choanozoa" ~ "Opisthokont",
      phylum == "Bigyra" ~ "Bigyra",
      
      # Zoo
      class == "Branchiopoda" ~ "Cladoceran",
      class == "Hexanauplia" ~ "Copepod",
      phylum == "Rotifera" ~ "Rotifer",
      phylum == "Ciliophora (phylum in subkingdom SAR)" ~ "Ciliate",
      class == "Ostracoda" ~ "Ostracod",
      
      TRUE ~ NA
      
    ),
    
    # assign zooplankton to groups for length-weight
    lw.group = case_when(
      
      taxa.name == "Bosmina" ~ "bosmina",
      taxa.name == "Daphnia" ~ "daphnia",
      family == "Daphniidae" ~ "daphniidae",
      order == "Diplostraca" ~ "cladocera",
      phylum == "Rotifera" ~ "rotifer",
      phylum == "Arthropoda" ~ "copepoda",
      phylum == "Ciliophora (phylum in subkingdom SAR)" ~ "ciliate",
      
      TRUE ~ NA
    ) 
  ) %>% 
  
  select(individual.uid, uid, source.code, original.sources, type, taxa.name,
               body.size, bodysize.measurement,
               ott.id, genus.ott.id, taxonomic.group, lw.group, fg, fg.source, taxa.name, species, genus, family, order, class, phylum, kingdom,
               location.code, habitat, location, country, continent, latitude, longitude)

# Calculating masses ----
# Calculate the masses from length weight and remove any obvious errors

# get extra info to add in
extra_raw <- bs_fg %>% 
  distinct(individual.uid, .keep_all = TRUE) %>% 
  
  select(
    - bodysize.measurement,
    - body.size
  )

bodysize_raw <- bs_fg %>% 
  
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
    extra_raw, by = "individual.uid"
  ) %>% 
  
  # Dry and wet mass
  # Zooplankton - There is a very small amount of info for wet mass and so just discard this and focus on length-weight and dry mass
  # Phytoplankton - There is very small amount of info for dry mass so discard this and focus on biovolume
  
  # Remove the wet mass column
  select(
    - `wet mass`
  ) %>% 
  
  mutate(
  
    # Change dry mass to NA for phytoplankton
    dry.mass = if_else(
      type == "Phytoplankton",
      NA,
      dry.mass
    ),
    
    # Calculate mass

    # a) convert linear measurements to dry weight
    dw = case_when(
      
      # using length weight regression equations - calculates micrograms so don't need to chnage any of those
      # measurements are in micro so need to turn to mm first for zooplankton
      !is.na(length) & lw.group == "bosmina" ~ 3.0896 + (3.0395*log(length/1000)),
      !is.na(length) & lw.group == "daphnia" ~ 1.4681 + (2.8292*log(length/1000)),
      !is.na(length) & lw.group == "daphniidae" ~ 1.5072 + (2.761*log(length/1000)),
      !is.na(length) & lw.group == "cladocera" ~ 1.7512 + (2.653*log(length/1000)), 
      !is.na(length) & lw.group == "copepoda" ~ 1.9526 + (2.399*log(length/1000)),
      
      # using mass info
      is.na(length) & !is.na(body.mass) ~ body.mass,
      is.na(length) & !is.na(dry.mass) ~ dry.mass,
      
      # Rotifers
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Anuraeopsis") ~ 0.33*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Ascomorpha") ~ 0.52*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Asplanchna") ~ 0.52*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name,"Brachionus") ~ 0.52*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Conochilus") ~ 0.26*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Collotheca") ~ 0.26*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Euchlanis") ~ 0.26*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Filinia") ~ 0.52*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Gastropus") ~ 0.80*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Hexarthra") ~ 0.26*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Kellicottia") ~ 0.26*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Notholca") ~ 0.13*(3*(length/1000)*(width/1000)*(height/1000) + 4*((height/1000)^3)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Ploesoma") ~ 0.52*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Polyarthra") ~ ((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Pompbolyx") ~ 0.40*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Synchaeta") ~ 0.26*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Testudinella") ~ 0.40*((length/1000)*(width/1000)*(height/1000)),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Trichocerca") ~ 0.52*((length/1000)*(width/1000)^2),
      lw.group == "rotifer" & stri_detect_regex(taxa.name, "Keratella") ~ 0.13*((length/1000)*(width/1000)^2),
      
      TRUE ~ NA
    ),
    
    # unlog the dw
    dw = exp(dw),
    
    # b) convert dry weight and volume to carbon mass
    c.pg = case_when(
      taxonomic.group == "Diatom" ~ 0.288*(biovolume^0.811),
      taxonomic.group == "Blue/green" ~ 0.218*(biovolume^0.85),
      type == "Phytoplankton" ~ 0.216*(biovolume^0.939),
      taxonomic.group == "Ciliate" ~ 0.310*(biovolume^0.983),
      !is.na(dw) ~ (dw/0.5)*1000000, # first part is converting to ug and then divide by 1000000 to get pg
      
      TRUE ~ NA
    ),
    
    # make a mld column
    mld = case_when(
      type == "Zooplankton" ~ length,
      TRUE ~ pmax(length, width, height, diameter, na.rm = TRUE)
    )
  ) %>% 
  
  # remove obvious outliers and any without a mass measurement
  mutate(
    outlier = case_when(
      # na
      is.na(c.pg) ~ "outlier",
      # phyto
      c.pg > 200000 & type == "Phytoplankton" ~ "outlier",
      # zoo
      c.pg > 2.5e+10 & type == "Zooplankton" ~ "outlier",
      
      # random ones
      uid %in% c("4231", "4241", "4234", "4235", "4252", "4259", "4275", "90258", "98394", "98658", "98655", "98650") ~ "outlier",
      
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    is.na(outlier)
  ) %>% 
  
  select(
    uid, source.code, original.sources, taxa.name,
    c.pg, mld,
    ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
    taxonomic.group, fg, fg.source,
    location.code, habitat, location, country, continent, latitude, longitude
  )

  
# calculate the mean and standard deviation 
outliers_info <- bodysize_raw %>% 
  
  group_by(
    genus
  ) %>% 
  
  summarise(
    mean = mean(c.pg),
    sd = sd(c.pg),
    .groups = "drop"
  )

# Remove outliers and get average for multiple of the same individual.uid
bodysize_outliers <- bodysize_raw %>% 
  
  left_join(outliers_info, by = "genus") %>% 
  
  filter(
    c.pg <= mean + (sd * 2) & c.pg >= mean - (sd * 2)
  ) %>% 
  
  # calculate average for each source and location
  group_by(
    uid, location.code, source.code, original.sources
  ) %>% 
  
  summarise(
    mass = mean(c.pg),
    mld = mean(mld),
    .groups = "drop"
  )

# add in extra info
locations_updated <- bodysize_raw %>% 
  select(
    location.code,
    habitat, 
    location,
    country,
    continent,
    longitude,
    latitude
  ) %>% 

  distinct(
    location.code, .keep_all = TRUE
  )%>% 
  
  filter(
    location.code %in% bodysize_outliers$location.code
  )

tax_updated <- bodysize_raw %>% 
  select(
    uid,
    ott.id,
    genus.ott.id,
    type,
    taxonomic.group,
    fg,
    fg.source,
    taxa.name,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 
  
  distinct(
    uid, .keep_all = TRUE
  ) %>% 
  
  filter(
    uid %in% bodysize_outliers$uid
  )


# join in the extra info to outliers
trait_data_all <- bodysize_outliers %>%
  
  left_join(
    locations_updated, by = "location.code"
  ) %>% 
  
  left_join(
    tax_updated, by = "uid"
  ) %>% 
  
  rename(
    functional.group = fg
  ) %>% 

  # select columns and reorder
  select(
    uid, source.code, original.sources, taxa.name,
    mass, mld,
    ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
    taxonomic.group, functional.group, fg.source,
    location.code, habitat, location, country, continent, latitude, longitude
  ) 

# save
saveRDS(trait_data_all, file = "R/Data_outputs/database_products/trait_data_all.rds")

# Select species
plankton_species_traits <- trait_data_all %>% 
  
  filter(
    !is.na(species)
  ) %>% 
  
  select(
    -genus.ott.id,
    -species
  )

# save
saveRDS(plankton_species_traits, file = "R/Data_outputs/database_products/final_products/plankton_species_traits.rds")


# Select genus
genus_traits <- trait_data_all %>% 
  
  # select columns
  select(
    genus, functional.group, fg.source
  ) %>% 
  
  # get ones only with fg info
  filter(
    !is.na(functional.group)
  ) %>% 
  
  # get list of each taxa.name with each unique fg
  group_by(genus, functional.group, fg.source) %>% 
  distinct(genus) %>% 
  ungroup() %>% 
  group_by(genus) %>% 
  summarise(
    functional.group = paste(unique(functional.group), collapse = "/"),
    fg.source = paste(unique(fg.source), collapse = "/")
    )
  
# add to main data
plankton_genus_traits <- trait_data_all %>% 
  
  select(
    - ott.id,
    - taxa.name,
    - species,
    - functional.group,
    - fg.source
  ) %>% 
  
  left_join(
    genus_traits, by = "genus"
  ) %>% 
  
  rename(
    taxa.name = genus,
    ott.id = genus.ott.id
  )


# save
saveRDS(plankton_genus_traits, file = "R/Data_outputs/database_products/final_products/plankton_genus_traits.rds")


# View data ----
ggplot(plankton_genus_traits, aes(x = log10(mass), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()

ggplot(plankton_species_traits, aes(x = log10(mass), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()




