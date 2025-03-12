# adding in available functional group info

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ----
bodysize_taxonomy <- readRDS("R/Data_outputs/full_database/tol/bodysize_taxonomy_tol2.rds")
phyto_mass_all <- readRDS("R/Data_outputs/full_database/tol/phyto_mass_all_tol2.rds")

rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet")
lt <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture")
kruk <- read_xlsx("raw_data/kruk_groups.xlsx")

updated_spec_char <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "special_characters")
tax_list_raw <- readRDS("R/Data_outputs/taxonomy/tol2/tax_list_raw.rds")


# taxonomy
# need to update the names
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

# Kruk ----

# get list of updated names with the reynolds groups and other relevent info
kruk_traits <- kruk %>% 
  
  # select columns
  select(
    Species_name,
    `Classification by Experts`,
    Life_form,
    Aerotopes,
    Flagella,
    Mucilage,
    Akinete,
    Heterocite,
    Wall_of_Si,
    Wall_not_of_Si
  ) %>% 
  
  rename(
    old.taxa.name = Species_name,
    reynolds.group = `Classification by Experts`,
    life.form = Life_form,
    aerotopes = Aerotopes,
    flagella = Flagella,
    mucilage = Mucilage,
    akinete = Akinete,
    heterocite = Heterocite,
    wall_of_Si = Wall_of_Si,
    wall_not_of_Si = Wall_not_of_Si
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
    
    life.form = case_when(
      life.form == 1 ~ "Cell",
      life.form == 2 ~ "Colonial",
      life.form == 3 ~ "Filamentous",
      TRUE ~ NA
    ),
    
    mobility.apparatus = if_else(
      flagella == 1,
      "Flagella",
      NA
    ),
    
    siliceous.wall = case_when(
      wall_of_Si == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    mucilage = case_when(
      mucilage == 1 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>% 
  
  select(
    taxa.name, reynolds.group, life.form, mobility.apparatus, siliceous.wall, mucilage
    )

# Rimmet ----
# get all the functional trait data from rimmet

rimmet_traits <- rimmet %>% 
  
  select(
    `Genus + species name`,
    `Functional groups (Reynolds 2002)`,
    `Functional groups (Padisak 2009)`,
    `Morpho-classification (Kruk 2010)`,
    `Mobility apparatus`,
    `Mobility apparatus: Flagella`,
    `Mobility apparatus: Raphe`,
    Colonial,
    Filament,
    Heterotrophic,
    Mixotrophic,
    Autotrophic
  ) %>% 
  
  rename(
    old.taxa.name = `Genus + species name`,
    reynolds.group = `Functional groups (Reynolds 2002)`,
    padisak.group = `Functional groups (Padisak 2009)`,
    morpho.classification = `Morpho-classification (Kruk 2010)`,
    mobility.apparatus = `Mobility apparatus`,
    flagella = `Mobility apparatus: Flagella`,
    raphe = `Mobility apparatus: Raphe`,
    colonial = Colonial,
    filament = Filament,
    heterotrophic = Heterotrophic,
    mixotrophic = Mixotrophic,
    autotrophic = Autotrophic
  ) %>% 
  
  left_join(
    names_list_ft, by = "old.taxa.name"
  ) %>% 
  
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  select(-old.taxa.name) %>% 
  
  # edit format to fit with everything else
  mutate(
    
    reynolds.group = toupper(reynolds.group),
    
    padisak.group = toupper(padisak.group),
    
    life.form = case_when(
      colonial == 0 & filament == 0 ~ "Cell",
      colonial == 1 & filament == 0 ~ "Colonial",
      colonial == 0 & filament == 1 ~ "Filamentous",
      colonial == 1 & filament == 1 ~ "Filamentous/Colonial",
      TRUE ~ NA
      ),
    
    motile = case_when(
      mobility.apparatus == 0 ~ "No",
      mobility.apparatus == 1 ~ "Yes"
    ),
    
    mobility.apparatus = case_when(
      flagella == 1 ~ "Flagella",
      raphe == 1 ~ "Raphe",
      TRUE ~ NA
    ),
    
    feeding.guild = case_when(
      heterotrophic == 1 ~ "Heterotroph",
      autotrophic == 1 ~ "Autotroph",
      mixotrophic == 1 ~ "Mixotroph",
      TRUE ~ NA
    )
  ) %>% 
  select(
    taxa.name, life.form, feeding.guild, motile, mobility.apparatus, reynolds.group, padisak.group, morpho.classification, 
  )

# LT ----

lt_traits <- lt %>% 
  
  select(
    Taxa_Name,
    Nutrition,
    Reynolds_Group,
    Siciliceous_Skeleton,
    Mucilage,
    Motility,
    Flagellum,
    Life_Form
  ) %>% 
  
  rename(
    old.taxa.name = Taxa_Name,
    reynolds.group = Reynolds_Group,
    mobility = Motility,
    flagella = Flagellum,
    feeding.guild = Nutrition,
    mucilage = Mucilage,
    siliceous.wall = Siciliceous_Skeleton,
    life.form = Life_Form
  ) %>% 
  
  left_join(
    names_list_ft, by = "old.taxa.name"
  ) %>% 
  
  # get distinct non NAs
  distinct(
    taxa.name, .keep_all = TRUE
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  select(-old.taxa.name) %>% 
  
  # make format changes to fit others
  mutate(
    
    reynolds.group = toupper(reynolds.group),
    
    motile = case_when(
      mobility == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    reynolds.group = na_if(reynolds.group, "#NA"),
    
    mobility.apparatus = case_when(
      flagella == 1 ~ "Flagella",
      TRUE ~ NA
    ),
    
    siliceous.wall = case_when(
      siliceous.wall == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    mucilage = case_when(
      mucilage == 1 ~ "Yes",
      TRUE ~ "No"
    ),
    
    life.form = case_when(
      life.form == "Cel." ~ "Cell",
      life.form == "Col." ~ "Colonial",
      life.form == "Fil." ~ "Filamentous"
    )
  ) %>% 
  
  select(
    taxa.name, life.form, feeding.guild, motile, mobility.apparatus, reynolds.group, siliceous.wall, mucilage
  )

# Join together ----
# chose the most recent paper as preferece when multiple

functional_traits <- full_join(kruk_traits, rimmet_traits, by = "taxa.name", suffix = c(".kruk", ".rimmet")) %>% 
  
  mutate(
    mobility.apparatus = case_when(
      !is.na(mobility.apparatus.rimmet) ~ mobility.apparatus.rimmet,
      TRUE ~ mobility.apparatus.kruk
    ),
    
    life.form = case_when(
      life.form.rimmet == "Filamentous/Colonial" & !is.na(life.form.kruk) ~ life.form.kruk,
      !is.na(life.form.rimmet) ~ life.form.rimmet,
      TRUE ~ life.form.kruk
    ),
    
    reynolds.group = case_when(
      !is.na(reynolds.group.rimmet) ~ reynolds.group.rimmet,
      TRUE ~ reynolds.group.kruk
    )
  ) %>% 
  
  select(
    - mobility.apparatus.kruk,
    - mobility.apparatus.rimmet,
    - life.form.kruk,
    - life.form.rimmet,
    - reynolds.group.kruk,
    - reynolds.group.rimmet
  ) %>% 
  
  full_join(., lt_traits, by = "taxa.name", suffix = c(".old", ".lt")) %>% 
  
  mutate(
    life.form = case_when(
      life.form.old == "Filamentous/Colonial" & life.form.lt %in% c("Colonial", "Filamentous") ~ life.form.lt,
      !is.na(life.form.old) ~ life.form.old,
      TRUE ~ life.form.lt
    ),
    
    reynolds.group = case_when(
      !is.na(reynolds.group.old) ~ reynolds.group.old,
      TRUE ~ reynolds.group.lt
    ),
    
    siliceous.wall = case_when(
      !is.na(siliceous.wall.old) ~ siliceous.wall.old,
      TRUE ~ siliceous.wall.lt
    ),
    
    mucilage = case_when(
      !is.na(mucilage.old) ~ mucilage.old,
      TRUE ~ mucilage.lt
    ),
    
    feeding.guild = case_when(
      !is.na(feeding.guild.old) ~ feeding.guild.old,
      TRUE ~ feeding.guild.lt
    ),
    
    motile = case_when(
      !is.na(motile.old) ~ motile.old,
      TRUE ~ motile.lt
    ),
    
    mobility.apparatus = case_when(
      !is.na(mobility.apparatus.old) ~ mobility.apparatus.old,
      TRUE ~ mobility.apparatus.lt
    )
  ) %>% 
  
  # make a group column
  
  # need the taxonomy infor for this
  left_join(
    tax_list_raw, by = "taxa.name"
  ) %>% 
  
  select(
    taxa.name, life.form, reynolds.group, padisak.group, morpho.classification, feeding.guild, motile, mobility.apparatus, mucilage, siliceous.wall
  )

# Add to main data ----

phyto_traits_all <- phyto_mass_all %>% 
  
  left_join(
    functional_traits, by = "taxa.name"
    ) %>% 
  
  left_join(
    functional_traits, by = c("genus" = "taxa.name"),
    suffix = c("", ".genus")
  ) %>% 
  
  left_join(
    functional_traits, by = c("phylum" = "taxa.name"),
    suffix = c("", ".phylum")
  ) %>% 
  
  left_join(
    functional_traits, by = c("kingdom" = "taxa.name"),
    suffix = c("", ".kingdom")
  ) %>% 
  
  mutate(
    life.form = case_when(
      !is.na(life.form) ~ life.form,
      is.na(life.form) & !is.na(life.form.genus) ~ life.form.genus,
      is.na(life.form) & is.na(life.form.genus) & !is.na(life.form.phylum) ~ life.form.phylum,
      is.na(life.form) & is.na(life.form.genus) & is.na(life.form.phylum) & !is.na(life.form.kingdom) ~ life.form.kingdom,
      TRUE ~ NA
    ),
   
    reynolds.group = case_when(
      !is.na(reynolds.group) ~ reynolds.group,
      is.na(reynolds.group) & !is.na(reynolds.group.genus) ~ reynolds.group.genus,
      is.na(reynolds.group) & is.na(reynolds.group.genus) & !is.na(reynolds.group.phylum) ~ reynolds.group.phylum,
      is.na(reynolds.group) & is.na(reynolds.group.genus) & is.na(reynolds.group.phylum) & !is.na(reynolds.group.kingdom) ~ reynolds.group.kingdom,
      TRUE ~ NA
    ), 
    
    siliceous.wall = case_when(
      !is.na(siliceous.wall) ~ siliceous.wall,
      is.na(siliceous.wall) & !is.na(siliceous.wall.genus) ~ siliceous.wall.genus,
      is.na(siliceous.wall) & is.na(siliceous.wall.genus) & !is.na(siliceous.wall.phylum) ~ siliceous.wall.phylum,
      is.na(siliceous.wall) & is.na(siliceous.wall.genus) & is.na(siliceous.wall.phylum) & !is.na(siliceous.wall.kingdom) ~ siliceous.wall.kingdom,
      TRUE ~ NA
    ), 
    
    mucilage = case_when(
      !is.na(mucilage) ~ mucilage,
      is.na(mucilage) & !is.na(mucilage.genus) ~ mucilage.genus,
      is.na(mucilage) & is.na(mucilage.genus) & !is.na(mucilage.phylum) ~ mucilage.phylum,
      is.na(mucilage) & is.na(mucilage.genus) & is.na(mucilage.phylum) & !is.na(mucilage.kingdom) ~ mucilage.kingdom,
      TRUE ~ NA
    ), 
    
    feeding.guild = case_when(
      !is.na(feeding.guild) ~ feeding.guild,
      is.na(feeding.guild) & !is.na(feeding.guild.genus) ~ feeding.guild.genus,
      is.na(feeding.guild) & is.na(feeding.guild.genus) & !is.na(feeding.guild.phylum) ~ feeding.guild.phylum,
      is.na(feeding.guild) & is.na(feeding.guild.genus) & is.na(feeding.guild.phylum) & !is.na(feeding.guild.kingdom) ~ feeding.guild.kingdom,
      TRUE ~ NA
    ), 
    
    motile = case_when(
      !is.na(motile) ~ motile,
      is.na(motile) & !is.na(motile.genus) ~ motile.genus,
      is.na(motile) & is.na(motile.genus) & !is.na(motile.phylum) ~ motile.phylum,
      is.na(motile) & is.na(motile.genus) & is.na(motile.phylum) & !is.na(motile.kingdom) ~ motile.kingdom,
      TRUE ~ NA
    ), 
    
    mobility.apparatus = case_when(
      !is.na(mobility.apparatus) ~ mobility.apparatus,
      is.na(mobility.apparatus) & !is.na(mobility.apparatus.genus) ~ mobility.apparatus.genus,
      is.na(mobility.apparatus) & is.na(mobility.apparatus.genus) & !is.na(mobility.apparatus.phylum) ~ mobility.apparatus.phylum,
      is.na(mobility.apparatus) & is.na(mobility.apparatus.genus) & is.na(mobility.apparatus.phylum) & !is.na(mobility.apparatus.kingdom) ~ mobility.apparatus.kingdom,
      TRUE ~ NA
    )
  ) %>% 
  
  # select relevent columns - don't need tax.uid anymore
  select(
    individual.uid, source.code, original.sources, taxa.name.full, taxa.name,
    nu, cells.per.nu, mass, biovolume, mld, 
    tax.uid, species, genus,family, order, class, phylum, kingdom,
    sample.year, sample.month,
    location.code, habitat, location, country, continent, latitude, longitude,
    life.form,reynolds.group, padisak.group, morpho.classification, feeding.guild, motile, mobility.apparatus, mucilage, siliceous.wall
  )

x <- phyto_traits_all %>% 
  
  distinct(tax.uid, .keep_all = TRUE) %>% 
  
  mutate(
    x = case_when(
      !is.na(reynolds.group) ~ "yes",
      !is.na(padisak.group) ~ "yes",
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    is.na(x)
  )

# save
saveRDS(phyto_traits_all, file = "R/Data_outputs/final_products/tol/phyto_traits_all.rds")


# filter for just species and genus level data

# Species
phyto_traits_species <- phyto_traits_all %>%
  
  filter(
    !is.na(species)
  ) %>% 
  
  # can get rid of redundant columns
  select(
    - taxa.name
  ) %>% 
  
  relocate(
    individual.uid, source.code, original.sources, taxa.name.full, species,
    nu, cells.per.nu, mass, biovolume, mld, 
    tax.uid, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month,
    location.code, habitat, location, country, continent, latitude, longitude,
    life.form,reynolds.group, padisak.group, morpho.classification, feeding.guild, motile, mobility.apparatus, mucilage, siliceous.wall
  )

# save
saveRDS(phyto_traits_species, file = "R/Data_outputs/final_products/tol/phyto_traits_species.rds")

# Genus
phyto_traits_genus <- phyto_traits_all %>%
  
  filter(
    !is.na(genus)
  ) %>% 
  
  # update taxa.name.full so its just genus
  mutate(
    taxa.name.extra.info = case_when(
      stri_detect_regex(taxa.name.full, "\\(") ~ stri_replace_first_regex(taxa.name.full, "\\w+ \\w+ |\\w+ (?=\\()", ""),
      
      TRUE ~ NA
    ),
    
    taxa.name.full = case_when(
      !is.na(taxa.name.extra.info) ~ stri_c(genus, taxa.name.extra.info, sep = " "),
      
      TRUE ~ genus
    )
  ) %>% 
  
  # can get rid of redundant columns
  select(
    - taxa.name,
    - species,
    - taxa.name.extra.info
  ) %>% 
  
  relocate(
    individual.uid, source.code, original.sources, taxa.name.full, genus,
    nu, cells.per.nu, mass, biovolume, mld, 
    tax.uid, family, order, class, phylum, kingdom,
    sample.year, sample.month,
    location.code, habitat, location, country, continent, latitude, longitude,
    life.form,reynolds.group, padisak.group, morpho.classification, feeding.guild, motile, mobility.apparatus, mucilage, siliceous.wall
  )

# save
saveRDS(phyto_traits_genus, file = "R/Data_outputs/final_products/tol/phyto_traits_genus.rds")



