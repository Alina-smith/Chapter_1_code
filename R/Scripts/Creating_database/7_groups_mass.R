## Calculating the body sizes using length-weight and biovolume conversions
# Have decided to go to genus level as this provides the most data points and locations compared to species level
# focusing on adult individuals

# all volume = um3
# all length, width and height = um,
# all mass = ug

# Flow of script:
# 1) final formatting changes
# 2) get averages for any individuals that have multiple measurements


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










# Misc formatting ----
# make any final formatting changes

bs_format <- bodysize_formatted %>% 
  
  # remove columns not needed now
  select(
    - sample.size, - sample.year,- sample.month, - sex, - reps
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
    
    # Convert all units to be the same to make it easier (ug for mass and um for dimensions)
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










# Get averages ----
# When there are multiple measurements for an individual then take an average of this for each measurement type

## Get extra info to add back in ----
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

## Find averages ----
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










# Genus ott ids ----
# get ott ids for genus to use for later on when agragating to genus

## tol ----
# Rerun names through tol now they are genus to get ott ids
genus_list <- bs_avg %>% 
  distinct(genus)

genus_ott <- tnrs_match_names(genus_list$genus)

# check all have been matched to an ott id - if false then okay
unique(is.na(genus_ott$ott_id))

## Add to data ----
bodysize_genus_ottid <- bs_avg %>% 
  
  # Set the genus to lower case to match the search string
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
  
  mutate(
    # fix random wrong ones
    genus = case_when(
      genus == "Karotomorpha (genus in subkingdom SAR)" ~ "Filinia",
      TRUE~ genus
    ),
    
    genus.ott.id = case_when(
      genus.ott.id == "164852" ~ 424467,
      TRUE ~ genus.ott.id
      )
  ) %>% 
  
  select(
    individual.uid, uid, source.code, original.sources, type,
    nu, ind.per.nu, body.size, bodysize.measurement, units,
    ott.id, genus.ott.id, taxa.name, species, genus, family, order, class, phylum, kingdom,
    location.code, habitat, location, country, continent, latitude, longitude
  )

saveRDS(bodysize_genus_ottid, "R/data_outputs/database_products/bodysize_genus_ottid.rds")


############################## Now have a dataset of raw bodysizes for species and genera with just one per individual with no mass formatting or functional group data








# Trait info ----
# Add trait info to data

bs_fg <- bodysize_genus_ottid %>% 
  
  ## left join ----
  # go up in resolution until a match is found

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
  
  ## Select lowest res ----
  # select the trait with the lowest res - might be quicker way to do this but can't work it out
  
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
  
  # remove excess columns
  select(
    !(matches(".s.g|.family|.order|.class|.phylum"))
  ) %>% 
  
  ## Taxonomic groups ----
  # Assign groupings based on taxonomy
  mutate(
    taxonomic.group = case_when(
      # Phyto:
      # assigned using the groupings in Menden-Deuer & Lessard 2000 to allow for conversions later on
      order == "Pyramimonadales" ~ "Prasinophytes",
      class == "Bacillariophyceae" ~ "Diatom",
      class == "Chrysophyceae" ~ "Chrysophyte",
      phylum %in% c("Spironematellophyta", "Chlorophyta") ~ "Green",
      phylum == "Dinoflagellata" ~ "Dinoflagellate",
      phylum == "Haptophyta" ~ "Prymnesiophyte",
      
      phylum == "Cyanobacteria" ~ "Blue-green",
      phylum %in% c("Glaucophyta", "Euglenophyta", "Charophyta", "Choanozoa", "Bigyra", "Heterokontophyta", "Cryptophyta") ~ "Protist",
      
      # Zoo
      order == "Choreotrichida" ~ "Loricate ciliate",
      class == "Branchiopoda" ~ "Cladoceran",
      class == "Hexanauplia" ~ "Copepod",
      class == "Ostracoda" ~ "Ostracod",
      phylum == "Ciliophora (phylum in subkingdom SAR)"~ "Aloricate ciliate",
      phylum == "Rotifera" ~ "Rotifer",
      
      TRUE ~ NA
      
    )
  ) %>% 
  
  select(individual.uid, uid, source.code, original.sources, type, taxa.name,
               body.size, bodysize.measurement,
               ott.id, genus.ott.id, taxonomic.group, fg, fg.source, taxa.name, species, genus, family, order, class, phylum, kingdom,
               location.code, habitat, location, country, continent, latitude, longitude)










# Calculate masses ----
# Calculate the masses from length weight and remove any obvious errors

## Get the equation info for zooplankton ----
crustacean_lw <- read_xlsx("raw_data/length_weight.xlsx", sheet = "crustacean_epa")
rotifer_lw <- read_xlsx("raw_data/length_weight.xlsx", sheet = "rotifer_karabin")

# join together
zoo_lw <- bind_rows(crustacean_lw, rotifer_lw) %>% 
  select(taxa.name, lna, b, ff.w, eq.w, ff.v, eq.v, average.appendices, length.range.min, length.range.max, ww.dw)

### Tol ----
# run names through tol to make sure they all are up to date 

# get list of name
names_lw <- zoo_lw %>% 
  distinct(taxa.name)

names_lw_tol <- tnrs_match_names(names_lw$taxa.name)

# check all have been matched to an ott id - if false then okay
# the ones that weren't matched are juvenile forms
unique(is.na(names_lw_tol$ott_id))

# join new names back to data
zoo_lw_nn <- zoo_lw %>% 
  
  mutate(
    taxa.name = tolower(taxa.name)
  ) %>% 
  
  left_join(
    select(names_lw_tol, search_string, unique_name),
    by = c("taxa.name" = "search_string")
  ) %>% 
  
  select(-taxa.name) %>% 
  rename(taxa.name = unique_name) %>% 

  mutate(
    # manual edits
    taxa.name = case_when(
      taxa.name == "Brachionus (genus in subkingdom SAR)" ~ "Brachionus (genus in Opisthokonta)",
      TRUE ~ taxa.name
    )
  ) %>% 
  filter(!is.na(taxa.name)) %>% 
  distinct(
    taxa.name, .keep_all = TRUE
  )



## Extra info ----
# get extra info to add back in
extra_raw <- bs_fg %>% 
  distinct(individual.uid, .keep_all = TRUE) %>% 
  
  select(
    - bodysize.measurement,
    - body.size
  )



## Calculate mass ----
bodysize_raw <- bs_fg %>% 
  
  ## Put measurements on one row per individual
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
  
  
  ## Dry and wet mass
  # First calculate dry and wet mass to use in converting to carbon mass 

  ### removing measurements
  # remove measurements according to :
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
    
    # remove data not needed for cilliates
    length = if_else(
      taxonomic.group == "Ciliate",
      NA,
      length
    )
  ) %>% 
  
  ### Calculate mass
  # 1) convert linear measurements to dry weight
  
  # join equation info
  left_join(
    zoo_lw_nn, by = "taxa.name"
  ) %>% 
  left_join(
    zoo_lw_nn, by = c("genus" = "taxa.name"),
    suffix = c(".t", ".g")
  ) %>% 
  left_join(
    zoo_lw_nn, by = c("family" = "taxa.name")
  ) %>% 
  left_join(
    zoo_lw_nn, by = c("order" = "taxa.name"),
    suffix = c(".f", ".o")
  ) %>% 
  left_join(
    zoo_lw_nn, by = c("class" = "taxa.name")
  ) %>% 
  left_join(
    zoo_lw_nn, by = c("phylum" = "taxa.name"),
    suffix = c(".c", ".p")
  ) %>% 
  mutate(
    lna = case_when(
      !is.na(lna.t) ~ lna.t,
      !is.na(lna.g) ~ lna.g,
      !is.na(lna.f) ~ lna.f,
      !is.na(lna.o) ~ lna.o,
      !is.na(lna.c) ~ lna.c,
      !is.na(lna.p) ~ lna.p,
      TRUE ~ NA
    ),
    b = case_when(
      !is.na(b.t) ~ b.t,
      !is.na(b.g) ~ b.g,
      !is.na(b.f) ~ b.f,
      !is.na(b.o) ~ b.o,
      !is.na(b.c) ~ b.c,
      !is.na(b.p) ~ b.p,
      TRUE ~ NA
    ),
    ff.v = case_when(
      !is.na(ff.v.t) ~ ff.v.t,
      !is.na(ff.v.g) ~ ff.v.g,
      !is.na(ff.v.f) ~ ff.v.f,
      !is.na(ff.v.o) ~ ff.v.o,
      !is.na(ff.v.c) ~ ff.v.c,
      !is.na(ff.v.p) ~ ff.v.p,
      TRUE ~ NA
    ),
    eq.v = case_when(
      !is.na(eq.v.t) ~ eq.v.t,
      !is.na(eq.v.g) ~ eq.v.g,
      !is.na(eq.v.f) ~ eq.v.f,
      !is.na(eq.v.o) ~ eq.v.o,
      !is.na(eq.v.c) ~ eq.v.c,
      !is.na(eq.v.p) ~ eq.v.p,
      TRUE ~ NA
    ),
    ff.w = case_when(
      !is.na(ff.w.t) ~ ff.w.t,
      !is.na(ff.w.g) ~ ff.w.g,
      !is.na(ff.w.f) ~ ff.w.f,
      !is.na(ff.w.o) ~ ff.w.o,
      !is.na(ff.w.c) ~ ff.w.c,
      !is.na(ff.w.p) ~ ff.w.p,
      TRUE ~ NA
    ),
    eq.w = case_when(
      !is.na(eq.w.t) ~ eq.w.t,
      !is.na(eq.w.g) ~ eq.w.g,
      !is.na(eq.w.f) ~ eq.w.f,
      !is.na(eq.w.o) ~ eq.w.o,
      !is.na(eq.w.c) ~ eq.w.c,
      !is.na(eq.w.p) ~ eq.w.p,
      TRUE ~ NA
    ),
    average.appendices = case_when(
      !is.na(average.appendices.t) ~ average.appendices.t,
      !is.na(average.appendices.g) ~ average.appendices.g,
      !is.na(average.appendices.f) ~ average.appendices.f,
      !is.na(average.appendices.o) ~ average.appendices.o,
      !is.na(average.appendices.c) ~ average.appendices.c,
      !is.na(average.appendices.p) ~ average.appendices.p,
      TRUE ~ NA
    ),
    length.range.min = case_when(
      !is.na(length.range.min.t) ~ length.range.min.t,
      !is.na(length.range.min.g) ~ length.range.min.g,
      !is.na(length.range.min.f) ~ length.range.min.f,
      !is.na(length.range.min.o) ~ length.range.min.o,
      !is.na(length.range.min.c) ~ length.range.min.c,
      !is.na(length.range.min.p) ~ length.range.min.p,
      TRUE ~ NA
    ),
    length.range.max = case_when(
      !is.na(length.range.max.t) ~ length.range.max.t,
      !is.na(length.range.max.g) ~ length.range.max.g,
      !is.na(length.range.max.f) ~ length.range.max.f,
      !is.na(length.range.max.o) ~ length.range.max.o,
      !is.na(length.range.max.c) ~ length.range.max.c,
      !is.na(length.range.max.p) ~ length.range.max.p,
      TRUE ~ NA
    ),
    ww.dw = case_when(
      !is.na(ww.dw.t) ~ ww.dw.t,
      !is.na(ww.dw.g) ~ ww.dw.g,
      !is.na(ww.dw.f) ~ ww.dw.f,
      !is.na(ww.dw.o) ~ ww.dw.o,
      !is.na(ww.dw.c) ~ ww.dw.c,
      !is.na(ww.dw.p) ~ ww.dw.p,
      TRUE ~ NA
    )
  ) %>% 
  select(
    !(ends_with(c(".t", ".g", ".f", ".o", ".c", ".p")))
  ) %>% 
  
  # Filter out ones that lie outside the ranges for the equations
  mutate(
    outliers = case_when(
      # Crustaceans
      genus == "Ceriodaphnia" & length >= 300 & length <= 700 ~ "in-range",
      genus == "Daphnia" & length >= 600 & length <= 4000 ~ "in-range",
      genus %in% c("Bosmina", "Eubosmina") & length >= 280 & length <= 950 ~ "in-range",
      genus == "Diaphanosoma" & length >= 440 & length <= 1440 ~ "in-range",
      taxonomic.group == "Cladoceran" & length >= 280 & length <= 4830 ~ "in-range",
      taxonomic.group == "Copepod" & length >= 140 & length <= 2450 ~ "in-range",
      
      # Rotifers
      #stri_detect_regex(w.factor, "a") & length >= length.range.min & length <= length.range.max ~ "in-range",
      #stri_detect_regex(w.factor, "b") & width >= length.range.min & width <= length.range.max ~ "in-range",
      #taxonomic.group == "Rotifer" & !is.na(biovolume) ~ "in-range",
      #taxonomic.group == "Rotifer" & !is.na(dry.mass) ~ "in-range",
      
      # Others
      taxonomic.group %in% c("Loricate ciliate", "Aloricate ciliate", "Rotifer") ~ "in-range",
      type == "Phytoplankton" ~ "in-range",
      
      TRUE ~ "outlier"
    )
  ) %>% 
  
  filter(
    outliers == "in-range"
  ) %>% 
    
  mutate(
    # 1) Crustaceans
    # a ) Dry weight
    dw.ug = case_when(
      # Equation: lnW (ug) = lna + (b * lnL (mm))
      # Paper used: EPA guidlines; appendix 1 - slope and intercepts collated from multiple sources; Bottrell et al (1976) - pooled data used when isn't present in EPA
      !is.na(lna) ~ exp(lna + (b*log(length/1000))),
      is.na(lna) & taxonomic.group %in% c("Cladoceran", "Copepod") & !is.na(dry.mass) ~ dry.mass,
      is.na(lna) & taxonomic.group %in% c("Cladoceran", "Copepod") & !is.na(body.mass) ~ body.mass,
      TRUE ~ NA
    ),
    
    # c) Carbon mass
    # Paper: Gaedke et al. 2004
    c.pg = case_when(
      taxonomic.group %in% c("Cladoceran", "Copepod") ~ (dw.ug*0.5)*10^6,
      TRUE ~ NA
    ),
    
    # Rotifers
    # a) Wet weight
    ff.w = ff.w*1000, # ff needs to be multiplied by 1000
    
    ww.ug = case_when(
      # Equation: 
      # equation takes length (mm) and gives dry mass (mg) so need to times by 1000 to get ug - length is currently in um so need to convert to mm for equation
      # Paper: Ejsmont-Karabin, J 1998 table 2
      eq.w == 1 & !is.na(length) ~ (ff.w * (length/1000)^3),
      eq.w == 2 & !is.na(length) & !is.na(width) ~ (ff.w * ((length/1000) * (width/1000)^2)),
      eq.w == 3 & !is.na(width) ~ (ff.w * (width/1000)^3),
      TRUE ~ NA
    ),
    
    # b) Dry weight
    # paper: EPA guidlines appendix 2
    dw.ug = case_when(
      taxonomic.group == "Rotifer" ~ ww.ug * ww.dw,
      TRUE ~ dw.ug
    ),
    
    # c) Carbon mass
    # Paper: Gaedke et al. 2004
    c.pg = case_when(
      taxonomic.group == "Rotifer" ~ (dw.ug*0.5)*10^6,
      TRUE ~ c.pg
    ),
    
    # 3) From biovolume - phytoplankton + cilliates
    
    # b) Calculate carbon mass
    c.pg = case_when(
      # Paper: Menden-Deuer & Lessard 2000; table 4
      taxonomic.group == "Protist" & biovolume > 3000 ~ 10^(-0.665 + (0.939 * log10(biovolume))),
      taxonomic.group == "Protist" & biovolume < 3000 ~ 10^(-0.583 + (0.860 * log10(biovolume))),
      taxonomic.group == "Diatom" & biovolume > 3000 ~ 10^(-0.541 + (0.811 * log10(biovolume))),
      taxonomic.group == "Diatom" & biovolume < 3000 ~ 10^(-0.933 + (0.881 * log10(biovolume))),
      taxonomic.group == "Green" ~ 10^(-1.026 + (1.088 * log10(biovolume))),
      taxonomic.group == "Chrysophyte" ~ 10^(-1.694 + (1.218 * log10(biovolume))),
      taxonomic.group == "Dinoflagellate" ~ 10^(-0.353 + (0.864 * log10(biovolume))),
      taxonomic.group == "Prasinophyte" ~ 10^(-0.545 + (0.886 * log10(biovolume))),
      taxonomic.group == "Prymnesiophytes" ~ (10^(-0.642 + (0.899 * log10(biovolume)))),
      taxonomic.group == "Loricate ciliate" ~ (10^(-0.168 + (0.841 * log10(biovolume)))),
      taxonomic.group == "Aloricate ciliate" ~ (10^(-0.639 + (0.984 * log10(biovolume)))),
      
      # Paper: Mahlmann et al 2008
      taxonomic.group == "Blue-green" ~ biovolume * 0.127 * 10^-3,
  
      TRUE ~ c.pg
    ),
    
    # Dry weight
    # Equation: 1 dw(pg) = 0.4 c(pg)
    # Paper: Moloney & Field 1989
    dw.ug = case_when(
      !is.na(c.pg) & type == "Phytoplankton" ~ (0.4 * c.pg)/1000000,
      !is.na(c.pg) & taxonomic.group %in% c("Loricate ciliate", "Aloricate ciliate") ~ (0.4 * c.pg)/1000000,
      is.na(c.pg) & type == "Phytoplankton" ~ body.mass,
      is.na(c.pg) & taxonomic.group %in% c("Loricate ciliate", "Aloricate ciliate") ~ body.mass,
      TRUE ~ dw.ug
    ),
    
    # Convert carbon weight to ug
    c.ug = c.pg/1000000,

    # make a mld column
    mld = case_when(
      type == "Zooplankton" ~ length,
      TRUE ~ pmax(length, width, height, diameter, na.rm = TRUE)
    )
  ) %>% 
  
  # rename biovolume column
  rename(
    bv.um3 = biovolume
  ) %>% 
  
  select(
    uid, source.code, original.sources, taxa.name,
    c.ug, dw.ug, bv.um3, mld,
    ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
    taxonomic.group, fg, fg.source,
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 

  # remove obvious outliers and any without a mass measurement
  mutate(
    outlier = case_when(
      # na
      is.na(dw.ug) ~ "outlier",
      # phyto
      #c.pg > 1e+8 & type == "Phytoplankton" ~ "outlier",
      # zoo
      #c.pg > 5e+9 & type == "Zooplankton" ~ "outlier",
      
      #taxonomic.group == "Ciliate" ~ "outlier",
      #taxonomic.group == "Rotifer" ~ 

      # random ones
      #uid %in% c("100238", "100201", "100492", "100581", "100275", "99693", "100402", "98876", "92100", "91647", "90535", "90536", "90534", "91507", "91685", "91686", "91678", "91403", "3822") ~ "outlier",

      TRUE ~ NA
    )
  ) %>%
  
  filter(
    is.na(outlier)
  ) %>% 
  
  rename(
    functional.group = fg
  ) %>% 
  
  select(
    uid, source.code, original.sources, taxa.name,
    c.ug, dw.ug, bv.um3, mld,
    ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
    taxonomic.group, functional.group, fg.source,
    location.code, habitat, location, country, continent, latitude, longitude
  ) 

# View data
ggplot(bodysize_raw, aes(x = log10(dw.ug), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  #facet_wrap(~type, ncol = 1)+
  scale_y_log10()

  
# calculate the mean and standard deviation 
#outliers_info <- bodysize_raw %>% 
  
#  group_by(
#    taxa.name
#  ) %>% 
  
#  summarise(
#    mean = mean(c.pg),
#    sd = sd(c.pg),
#    .groups = "drop"
#  )

# Remove outliers and get average for multiple of the same individual.uid
# bodysize_outliers <- bodysize_raw %>% 
#   
#   left_join(outliers_info, by = "taxa.name") %>% 
#   
#   filter(
#     #c.pg <= mean + (sd * 2) & c.pg >= mean - (sd * 2)
#   ) %>% 
#   
#   # calculate average for each source and location
#   group_by(
#     ott.id, location.code, source.code, original.sources
#   ) %>% 
#   
#   summarise(
#     mass = mean(c.pg),
#     mld = mean(mld),
#     .groups = "drop"
#   )

# # add in extra info
# locations_updated <- bodysize_raw %>% 
#   select(
#     location.code,
#     habitat, 
#     location,
#     country,
#     continent,
#     longitude,
#     latitude
#   ) %>% 
# 
#   distinct(
#     location.code, .keep_all = TRUE
#   )%>% 
#   
#   filter(
#     location.code %in% bodysize_outliers$location.code
#   )
# 
# tax_updated <- bodysize_raw %>% 
#   select(
#     ott.id,
#     genus.ott.id,
#     type,
#     taxonomic.group,
#     fg,
#     fg.source,
#     taxa.name,
#     species,
#     genus,
#     family,
#     order,
#     class,
#     phylum,
#     kingdom
#   ) %>% 
#   
#   distinct(
#     ott.id, .keep_all = TRUE
#   ) 


# # join in the extra info to outliers
# plankton_traits_all <- bodysize_outliers %>%
#   
#   #left_join(
#   #  locations_updated, by = "location.code"
#   #) %>% 
#   
#   #left_join(
#   #  tax_updated, by = "ott.id"
#   #) %>% 
#   
#   rename(
#     functional.group = fg
#   )%>% 
# 
#   # select columns and reorder
#   select(
#     source.code, original.sources, taxa.name,
#     c.pg, mld,
#     ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
#     taxonomic.group, functional.group, fg.source,
#     location.code, habitat, location, country, continent, latitude, longitude
#   ) 

plankton_traits_all <- bodysize_raw

# save
saveRDS(plankton_traits_all, file = "R/Data_outputs/database_products/final_products/plankton_traits_all.rds")

# Select species
plankton_species_traits <- plankton_traits_all %>% 
  
  filter(
    !is.na(species)
  ) %>% 
  
  mutate(
    uid = paste("species", row_number(), sep = "-")
  ) %>% 
  
  select(
    -genus.ott.id,
    -species
  )

# save
saveRDS(plankton_species_traits, file = "R/Data_outputs/database_products/final_products/plankton_species_traits.rds")


# Select genus
genus_traits <- plankton_traits_all %>% 
  
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
plankton_genus_traits <- plankton_traits_all %>% 
  
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
  
  mutate(
    uid = paste("genus", row_number(), sep = "-")
  ) %>% 
  
  rename(
    taxa.name = genus,
    ott.id = genus.ott.id
  )


# save
saveRDS(plankton_genus_traits, file = "R/Data_outputs/database_products/final_products/plankton_genus_traits.rds")


# View data ----
ggplot(plankton_genus_traits, aes(x = log10(dw.ug), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()

ggplot(plankton_species_traits, aes(x = log10(dw.ug), fill = type)) +
  geom_histogram(binwidth = 0.3)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()

