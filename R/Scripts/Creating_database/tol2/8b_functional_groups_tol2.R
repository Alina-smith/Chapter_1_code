# adding in available functional group info

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)

# Data ----
bodysize_taxonomy <- readRDS("R/Data_outputs/full_database/tol/bodysize_taxonomy_tol2.rds")
phyto_mass_subset <- readRDS("R/Data_outputs/full_database/tol/phyto_mass_subset_tol2.rds")

rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet")
lt <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture")
kruk <- read_xlsx("raw_data/kruk_groups.xlsx")

updated_spec_char <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "special_characters")
tax_list_raw <- readRDS("R/Data_outputs/taxonomy/tol2/tax_list_raw.rds")


# Update names ----
# need to update the names to match the new ones from the taxonomy step

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

# Get list of groups ----

## Kruk ----
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


## Rimmet ----
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


## LT ----
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

## join together ----
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

# Add functional groups to taxonomy
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


x <- functional_groups %>% 
  filter(
    is.na(group)
  )

%>% 
  
  # select columns
  select(
    taxa.name,
    r.group,
    r.group.source,
    group
  )




























