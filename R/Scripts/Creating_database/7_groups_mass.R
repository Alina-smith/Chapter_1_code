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
      # Phyto
      phylum == "Cyanobacteria" ~ "Blue-green",
      phylum == "Glaucophyta" ~ "Glaucocystid",
      phylum %in% c("Spironematellophyta", "Chlorophyta") ~ "Green",
      class == "Bacillariophyceae" ~ "Heterokont (Diatom)",
      phylum == "Euglenophyta" ~ "Euglenoid-flagellate",
      phylum == "Cryptophyta" ~ "Cryptomonad",
      phylum == "Haptophyta" ~ "Haptophyte",
      phylum == "Dinoflagellata" ~ "Dino-flagellate",
      phylum == "Charophyta" ~ "Charophyte",
      phylum == "Heterokontophyta" ~ "Heterokont (non-Diatom)",
      phylum == "Choanozoa" ~ "Opisthokont",
      phylum == "Bigyra" ~ "Bigyra",
      
      # Zoo
      class == "Branchiopoda" ~ "Cladoceran",
      class == "Hexanauplia" ~ "Copepod",
      phylum == "Rotifera" ~ "Rotifer",
      phylum == "Ciliophora (phylum in subkingdom SAR)" ~ "Ciliate",
      class == "Ostracoda" ~ "Ostracod",
      
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
crustacean <- read_xlsx("raw_data/length_weight.xlsx", sheet = "crustacean_lw")
rotifer <- read_xlsx("raw_data/length_weight.xlsx", sheet = "rotifer_lw")

# Crustaceans
crustacean_lw <- crustacean %>% 
  
  mutate(
    info = stri_replace_first_regex(info, "\\S+ ", ""),
    taxa.name = as.character(stri_extract_all_regex(info, "^(([^\\d\\s]+\\s+){0,3}[^\\d\\s]+)")),
    lna = as.numeric(stri_extract_first_regex(info, "\\d(\\.\\d+)*")),
    info = stri_replace_first_regex(info, "\\d(\\.\\d+)*", ""),
    b = as.numeric(stri_extract_first_regex(info, "\\d(\\.\\d+)*")),
    group = "crustacean",
    
    # remove spp
    taxa.name = stri_replace_all_regex(taxa.name, " spp\\.| sp\\.| -", ""),
    
    # fix odd ones
    lna = if_else(taxa.name == "Leptodora kindti", -0.822, lna)
  ) %>% 
  
  select(
    -info
  )

# Rotifers
# all species are the same for eacxh genus so just aggregate to genus
rotifer_lw <- rotifer %>% 
  
  mutate(
    info = stri_replace_first_regex(info, "\\S+ ", ""),
    taxa.name = as.character(stri_extract_all_regex(info, "^([^\\d\\s]+\\s+){1}")),
    info = stri_replace_first_regex(info, "^(([^\\d\\s]+\\s+){0,3}[^\\d\\s]+) ", ""),
    group = "rotifer",
    
    # remove spp
    taxa.name = stri_replace_all_regex(taxa.name, " spp\\.| sp\\.", ""),
    # fix off ones
    taxa.name = case_when(
      taxa.name == "Keratella cochlearis f. tecta" ~ "Keratella cochlearis",
      taxa.name == "Keratella valga f. tropica" ~ "Keratella valga",
      taxa.name == "Rotifera bdelloid" ~ "Bdelloidea",
      taxa.name == "Keratella cochlearis hispida" ~ "keratella cochlearis",
      TRUE ~ taxa.name
    )
  ) %>% 
  
  separate(
    info, into = c("eq", "ff", "bv", "ww.dw"), sep = " "
  ) %>% 
  
  # change types
  mutate(
    eq = as.numeric(eq),
    ff = as.numeric(ff),
    bv = as.numeric(bv),
    ww.dw = as.numeric(ww.dw)
  ) %>% 
  
  distinct(taxa.name, .keep_all = TRUE)

# join together
zoo_lw <- bind_rows(crustacean_lw, rotifer_lw)

### Tol ----
# run names through tol to make sure they all are up to date 

# get list of name
names_lw <- zoo_lw %>% 
  distinct(taxa.name)

names_lw_tol <- tnrs_match_names(names_lw$taxa.name)

# check all have been matched to an ott id - if false then okay
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
  
  # manual edits
  mutate(
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
    group = case_when(
      !is.na(group.t) ~ group.t,
      !is.na(group.g) ~ group.g,
      !is.na(group.f) ~ group.f,
      !is.na(group.o) ~ group.o,
      !is.na(group.c) ~ group.c,
      !is.na(group.p) ~ group.p,
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
    eq = case_when(
      !is.na(eq.t) ~ eq.t,
      !is.na(eq.g) ~ eq.g,
      !is.na(eq.f) ~ eq.f,
      !is.na(eq.o) ~ eq.o,
      !is.na(eq.c) ~ eq.c,
      !is.na(eq.p) ~ eq.p,
      TRUE ~ NA
    ),
    ff = case_when(
      !is.na(ff.t) ~ ff.t,
      !is.na(ff.g) ~ ff.g,
      !is.na(ff.f) ~ ff.f,
      !is.na(ff.o) ~ ff.o,
      !is.na(ff.c) ~ ff.c,
      !is.na(ff.p) ~ ff.p,
      TRUE ~ NA
    ),
    bv = case_when(
      !is.na(bv.t) ~ bv.t,
      !is.na(bv.g) ~ bv.g,
      !is.na(bv.f) ~ bv.f,
      !is.na(bv.o) ~ bv.o,
      !is.na(bv.c) ~ bv.c,
      !is.na(bv.p) ~ bv.p,
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
  
  mutate(
    
    # calculate rotifer biovolume - needs um in equation and gives um^3 as a result
    #biovolume = case_when(
    #  eq == 1 ~ (length^3 * ff) + (bv * length^3 * ff),
    #  eq == 2 ~ (width^3 * ff),
    #  eq == 3 ~ (length * width^2 * ff) + (bv * length * width^2 * ff),
      
    #  TRUE ~ biovolume
    #),
    
    dw = case_when(
      # calculate crustacean dry weight - needs mm in equation and gives ug as result
      !is.na(lna) ~ exp(lna + (b*log(length/1000))),
      is.na(lna) & taxonomic.group %in% c("Cladoceran", "Copepod") & !is.na(dry.mass) ~ dry.mass,
      is.na(lna) & taxonomic.group %in% c("Cladoceran", "Copepod") & !is.na(body.mass) ~ body.mass,
      
      # calculate rotifer dry mass - needs um in equation and gives um^3 as a result
      eq == 1 ~ (((length^3 * ff) + (bv * length^3 * ff)) * 10^-6) * ww.dw,
      eq == 2 ~ (((width^3 * ff)) * 10^-6) * ww.dw,
      eq == 3 ~ (((length * width^2 * ff) + (bv * length * width^2 * ff)) * 10^-6) * ww.dw,
      taxonomic.group == "Rotifer" & !is.na(biovolume) ~ (biovolume * 10^-6) * ww.dw,
      
      TRUE ~ NA
    ),
    
    # b) convert to carbon mass
    c.pg = case_when(
      # From biovol measurements:
      # Paper: Menden-Deuer & Lessard 2000 ; table 4
      taxonomic.group == "Heterokont (Diatom)" ~ 10^(0.541 + (0.811 * log10(biovolume))),
      taxonomic.group == "Heterokont (non-Diatom)" ~ 10^(1.694 + (1.218 * log10(biovolume))),
      order == "Pyramimonadales" ~ 10^(0.545 + (0.886 * log10(biovolume))),
      taxonomic.group == "Green" & order != "Pyramimonadales" ~ 10^(0.026 + (1.088 * log10(biovolume))),
      taxonomic.group == "Dino-flagellate" ~ 10^(0.353 + (0.864 * log10(biovolume))),
      taxonomic.group == "Ciliate" & order == "Choreotrichida" ~ (10^(0.168 + (0.841 * log10(biovolume)))),
      taxonomic.group == "Ciliate" & order != "Choreotrichida" ~ (10^(0.639 + (0.984 * log10(biovolume)))),
      taxonomic.group == "Haptophyte" ~ (10^(0.642 + (0.899 * log10(biovolume)))),
      taxonomic.group %in% c("Cryptomonad", "Euglenoid-flagellate") ~ 10^(0.665 + (0.939 * log10(biovolume))),
      # Paper: Mahlmann et al 2008
      taxonomic.group == "Blue-green" ~ biovolume * 0.127 * 10^-3,
      
      # Paper: unknown
      #taxonomic.group == "Rotifer" ~ 0.053*(biovolume^0.99),
      
      # From dry mass
      # Paper: Gaedke et al. 2004
      !is.na(dw) ~ (dw*0.5)*10^6,
      TRUE ~ NA
    ),

    # make a mld column
    mld = case_when(
      type == "Zooplankton" ~ length,
      TRUE ~ pmax(length, width, height, diameter, na.rm = TRUE)
    )
  ) %>% 
  

  
  select(
    uid, source.code, original.sources, taxa.name,
    c.pg, mld,
    ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
    taxonomic.group, fg, fg.source,
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 

  # remove obvious outliers and any without a mass measurement
  mutate(
    outlier = case_when(
      # na
      is.na(c.pg) ~ "outlier",
      # phyto
      #c.pg > 1e+8 & type == "Phytoplankton" ~ "outlier",
      # zoo
      #c.pg > 5e+9 & type == "Zooplankton" ~ "outlier",
      
      # random ones
      #uid %in% c("100238", "100201", "100492", "100581", "100275", "99693", "100402", "98876", "92100", "91647", "90535", "90536", "90534", "91507", "91685", "91686", "91678", "91403", "3822") ~ "outlier",
      
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

# View data
ggplot(bodysize_raw, aes(x = log10(c.pg), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  #facet_wrap(~type, ncol = 1)+
  scale_y_log10()

  
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
    #c.pg <= mean + (sd * 2) & c.pg >= mean - (sd * 2)
  ) %>% 
  
  # calculate average for each source and location
  group_by(
    ott.id, location.code, source.code, original.sources
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
    ott.id, .keep_all = TRUE
  ) 


# join in the extra info to outliers
plankton_traits_all <- bodysize_outliers %>%
  
  left_join(
    locations_updated, by = "location.code"
  ) %>% 
  
  left_join(
    tax_updated, by = "ott.id"
  ) %>% 
  
  rename(
    functional.group = fg
  ) %>% 

  # select columns and reorder
  select(
    source.code, original.sources, taxa.name,
    mass, mld,
    ott.id, genus.ott.id, type, taxa.name, species, genus, family, order, class, phylum, kingdom,
    taxonomic.group, functional.group, fg.source,
    location.code, habitat, location, country, continent, latitude, longitude
  ) 

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
ggplot(plankton_genus_traits, aes(x = log10(mass), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()

ggplot(plankton_species_traits, aes(x = log10(mass), fill = type)) +
  geom_histogram(binwidth = 0.3)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()


x <- bodysize_raw %>% 
  
  filter(
    taxonomic.group == "Ciliate"
  ) %>% 
  
  mutate(
    x = case_when(
      order == "Choreotrichida" ~ "loricate",
      class %in% c("Spirotrichea", "Litostomatea", "Oligohymenophorea", "Prostomatea", "Colpodea", "Armophorea") ~ "aloricate",
      TRUE ~ NA
    )
  )

