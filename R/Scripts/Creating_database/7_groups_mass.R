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
library(taxize)
library(rotl)

# Final list of all data including phyto and zooplankton with groups and mass

# Import data ----
bodysize_location <- readRDS("R/Data_outputs/database_products/bodysize_location.rds")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")
sources_list_update <- readRDS("R/Data_outputs/database_products/sources_list_update.rds")

# Getting averages for each source ----

## Initial formatting ----
bs_format <- bodysize_location %>% 
  
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
  
  # Select just adult individuals
  filter(
    life.stage %in% c("adult", "active"),
    nu %in% "individual"
  ) %>% 
  
  # select and relocate columns - removing life stage and nu now as all adult individuals
  
  select(
    individual.uid, uid, source.code, original.sources, type,
    nu, ind.per.nu, body.size, bodysize.measurement, units,
    genus, family, order, class, phylum, kingdom,
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  # rename the taxa.name as genus as I am only going to genus level and filter out ones that are a higher resolution
  rename(
    taxa.name = genus
  ) %>% 
  
  filter(
    !is.na(taxa.name)
  )

## Get averages ----

# Get extra info to add in
# All the extra infor for the multiples of the same individual are the same and so can just left join by individual.uid
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
# Rerun names through tol now they are genus to get ott ids
genus_list <- bs_avg %>% 
  distinct(taxa.name)

genus_ott <- tnrs_match_names(genus_list$taxa.name)

# check all have been matched to an ott id - if false then okay
unique(is.na(genus_ott$ott_id))

# Add in ott_ids to data
bs_ott <- bs_avg %>% 
  
  # Set the taxa.name to lower case to match the search string
  mutate(
    taxa.name = tolower(taxa.name)
  ) %>% 
  
  left_join(
    select(
      genus_ott, search_string, unique_name, ott_id
    ), by = c("taxa.name" = "search_string")
  ) %>% 
  
  # get rid of old taxa.name and replace with new taxa.name
  select(
    -taxa.name
  ) %>% 
  
  rename(
    taxa.name = unique_name,
    ott.id = ott_id
  )

saveRDS(bs_ott, "R/data_outputs/database_products/bs_ott.rds")


############################## Now have a dataset of raw bodysizes with just one per individual but not functional group data

# New functional groups ----

## Import data ----
# These are all large databases that I have already used that include R groups

### Reynolds ----

# Rimmet - 2018, padisak and reynolds
rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet") %>% 
  select(
    `Genus + species name`,
    `Functional groups (Padisak 2009)`
  ) %>% 
  
  rename(
    taxa.name = `Genus + species name`,
    fg = `Functional groups (Padisak 2009)`
  ) %>% 
  
  mutate(
    ref = "rimmet"
  )

# Kruk - 2017, reynolds
kruk <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "kruk") %>% 
  select(
    Species_name,
    `Classification by Experts`,
  ) %>% 
  
  rename(
    taxa.name = Species_name,
    fg = `Classification by Experts`
  ) %>% 
  
  mutate(
    ref = "kruk"
  )

# Padisak - 2009, padisak
padisak <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "padisak") %>% 
  select(
     padisak.r.group,
     taxa.name
  ) %>% 
  
  rename(
    fg = padisak.r.group
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
    fg = Reynolds_Group
  )%>% 
  
  mutate(
    ref = "lt"
  )

# wang - uses non updated reynolds
wang <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "wang")%>% 
  
  mutate(
    ref = "wang"
  )

### Zoo ----
gavrilko <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Gavrilko") %>% 
  
  select(
    species,
    functional.group
  ) %>% 
  
  rename(
    taxa.name = species,
    fg = functional.group
  ) %>% 
  
  mutate(
    taxa.name = stri_extract_first_regex(taxa.name, "\\w+(?= )"),
    
    ref = "gavrilko",
    fg = as.character(fg)
  ) %>%
  distinct(
    taxa.name, .keep_all = TRUE
  )

### Join together ----
r_groups_raw <- bind_rows(padisak, rimmet, kruk, lt, wang, gavrilko) %>% 
  
  # put all taxa onto seperate rows
  separate_rows(taxa.name, sep = ",|/") %>% 
  
  mutate(
    # Get rid of any white spaces on the ends
    old.taxa.name = str_trim(taxa.name),
    
    # Make all groups upper case because some are a mix of upper and lower
    fg = toupper(fg),
    
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
    !is.na(fg),
    fg != "#NA"
  )

## Update taxa.names ----
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
cleaned_gna_groups <- do.call(rbind, lapply(to_clean, function(name) {
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
cleaned_groups <- cleaned_gna_groups %>% 
  
  mutate(
    cleaned.taxa.name = case_when(
      
      original.taxa.name == "Nitzchia" ~ "Nitschia",
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
      original.taxa.name == "Chloroccum" ~ "Chlorococcum",
      original.taxa.name == "Cyclostesphanos" ~ "Cyclostephanos",
      
      TRUE ~ cleaned.taxa.name.gna
    )
  ) %>% 
  
  filter(
    !is.na(cleaned.taxa.name),
    cleaned.taxa.name != "Monoraphidium pseudomirabilis"
  )

saveRDS(cleaned_groups, "R/data_outputs/database_products/cleaned_groups.rds")

##### tnrs_match_names ----
# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)

resolved_tol_groups <- tnrs_match_names(cleaned_groups$cleaned.taxa.name) %>% 
  
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
saveRDS(resolved_tol_groups, "R/data_outputs/database_products/resolved_tol_groups.rds")

##### final updated names list ----

names_list_updated <- spec_char %>% 
  
  # Remove the ones from spec_char that didn't have a taxa.name
  filter(
    !is.na(taxa.name)
  ) %>% 
  
  # Add back in the names removed above with the taxa.names added in
  bind_rows(
    resolved_tol_groups
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
  
  # put each source on it's own column
  group_by(
    taxa.name
  ) %>% 
  
  pivot_wider(
    names_from = ref,
    values_from = c(fg),
    values_fn = function(x) paste(unique(x), collapse = "/")
  ) %>% 
  
  ungroup() %>% 
  
  rename(
    `r.group (padisak 2009)` = padisak,
    `r.group (rimmet 2018)` = rimmet,
    `r.group (Laplace-Treyture 2021)` = lt,
    `r.group (kruk 2017)` = kruk,
    `r.group (Wang 2024)` = wang,
    `r.group (Gavrilko, 2022)` = gavrilko,
  ) %>% 
  
  # add in rank info
  left_join(
    select(tax_list_raw, rank, taxa.name), by = "taxa.name"
  ) %>% 
  
  # add in the couple extra that needed to be done manually
  mutate(
    rank = case_when(
      taxa.name == "Chlorolobium" ~ "Genus",
      taxa.name == "Cryptomonadales" ~ "Order",
      taxa.name == "Chromatium" ~ "Genus",
      taxa.name == "Pyrobotrys" ~ "Genus",
      taxa.name == "Dimorphococcus" ~ "Genus",
      taxa.name == "Xanthophyceae" ~ "Class",
      taxa.name == "Binuclearia" ~ "Genus",
      
      TRUE ~ rank
    )
  ) %>% 
  
  # can now remove any without a rank because these are ones that are not present in the data and also species level ones as i am only going to genus level
  filter(
    !is.na(rank),
    rank != "Species"
  ) %>% 
  
  # change Dinoflagellata to Myzozoa
  mutate(
    taxa.name = if_else(
      taxa.name == "Dinoflagellata",
      "Myzozoa",
      taxa.name
    )
  ) %>% 
  
  # select preference - lt, rimmet, padisak, reynolds and wang (wang last beacuse doesn't use updated padisak ones)
  # order colums in order of preference
  relocate(
    `r.group (Laplace-Treyture 2021)`, `r.group (rimmet 2018)`, `r.group (padisak 2009)`, `r.group (kruk 2017)`, `r.group (Wang 2024)`, `r.group (Gavrilko, 2022)`
  ) %>% 
  
  mutate(
    
    fg.source = case_when(
      !is.na(`r.group (Laplace-Treyture 2021)`) ~ "Laplace-Treyture(2021)",
      !is.na(`r.group (rimmet 2018)`) ~ "rimmet(2018)",
      !is.na(`r.group (padisak 2009)`) ~ "padisak(2009)",
      !is.na(`r.group (kruk 2017)`) ~ "kruk(2017)",
      !is.na(`r.group (Wang 2024)`) ~ "Wang(2024)",
      !is.na(`r.group (Gavrilko, 2022)`) ~ "Gavrilko(2022)",
      
      TRUE ~ NA
    ),
    
    fg = case_when(
      !is.na(`r.group (Laplace-Treyture 2021)`) ~ `r.group (Laplace-Treyture 2021)`,
      !is.na(`r.group (rimmet 2018)`) ~ `r.group (rimmet 2018)`,
      !is.na(`r.group (padisak 2009)`) ~ `r.group (padisak 2009)`,
      !is.na(`r.group (kruk 2017)`) ~ `r.group (kruk 2017)`,
      !is.na(`r.group (Wang 2024)`) ~ `r.group (Wang 2024)`,
      !is.na(`r.group (Gavrilko, 2022)`) ~ `r.group (Gavrilko, 2022)`,
      
      TRUE ~ NA
    )
  ) %>% 
  
  select(
    taxa.name, rank, fg.source, fg
  ) %>% 
  
  # There are some that have multiple inputs for the r.group so select one for these
  # When there is a clear majority use this to be the most representative
  # when it is evenly split select the most representative lake type
  mutate(
    fg = case_when(
      taxa.name == "Closteriopsis" ~ "J",
      taxa.name == "Dolichospermum (inconsistent in FamilyI (in SubsectionIV))" ~ "H1",
      taxa.name == "Bacillariophyceae" ~ "D",
      taxa.name == "Vitreochlamys" ~ "X2",
      taxa.name == "Chlorolobium" ~ "X1",
      taxa.name == "Cyanodictyon" ~ "K",
      taxa.name == "Ceratium" ~ "LO",
      taxa.name == "Lyngbya (genus in domain Bacteria)" ~ "S1",
      taxa.name == "Chlorophyceae" ~ "X1",
      taxa.name == "Chlamydomonadales" ~ "X1",
      taxa.name == "Copepoda" ~ "3",
       
      TRUE ~ fg
    )
  ) 


# save
saveRDS(r_groups, "R/data_outputs/database_products/r_groups.rds")

#### Add groups to main data ----

# Add to data
bs_fg <- bs_ott %>% 
  
  # genus
  left_join(
    select(
      r_groups, taxa.name, fg
      ), by = "taxa.name"
  ) %>% 
  
  rename(
    fg.genus = fg
  ) %>% 
  
  # family
  left_join(
    select(
      r_groups, taxa.name, fg
    ), by = c("family" = "taxa.name")
  ) %>% 
  
  rename(
    fg.family = fg
  ) %>% 
  
  # order
  left_join(
    select(
      r_groups, taxa.name, fg
    ), by = c("order" = "taxa.name")
  ) %>% 
  
  rename(
    fg.order = fg
  ) %>% 
  
  # class
  left_join(
    select(
      r_groups, taxa.name, fg
    ), by = c("class" = "taxa.name")
  ) %>% 
  
  rename(
    fg.class = fg
  ) %>% 
  
  # phylum
  left_join(
    select(
      r_groups, taxa.name, fg
    ), by = c("phylum" = "taxa.name")
  ) %>% 
  
  rename(
    fg.phylum = fg
  ) %>% 
  
  # select that group with the lowest res
  mutate(
    fg = case_when(
      !is.na(fg.genus) ~ fg.genus,
      !is.na(fg.family) ~ fg.family,
      !is.na(fg.order) ~ fg.order,
      !is.na(fg.class) ~ fg.class,
      !is.na(fg.phylum) ~ fg.phylum,
      
      TRUE ~ "Unassigned"
    )
  ) %>% 
  
  # assign traditional groups 
  mutate(
    group = case_when(
      # Phyto
      phylum == "Cyanobacteria" ~ "Blue/green",
      phylum %in% c("Ochrophyta", "Bigyra") ~ "Stramenopile",
      phylum == "Glaucophyta" ~ "Glaucophyte",
      phylum %in% c("Chlorophyta", "Charophyta") ~ "Green",
      phylum == "Bacillariophyta" ~ "Diatom",
      phylum == "Euglenozoa" ~ "Euglenoid",
      phylum == "Cryptophyta" ~ "Cryptomonad",
      phylum == "Haptophyta" ~ "Haptophyte",
      phylum == "Myzozoa" ~ "Dinoflagellate",
      phylum == "Choanozoa" ~ "Opisthokont",
      
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
               ott.id, group, lw.group, fg, family, order, class, phylum, kingdom,
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
      group == "Diatom" ~ 0.288*(biovolume^0.811),
      group == "Blue/green" ~ 0.218*(biovolume^0.85),
      type == "Phytoplankton" ~ 0.216*(biovolume^0.939),
      group == "Ciliate" ~ 0.310*(biovolume^0.983),
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
      uid %in% c("4260", "4253", "4276","4255") ~ "outlier",
      
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    is.na(outlier)
  ) %>% 
  
  select(
    uid, individual.uid, source.code, original.sources, taxa.name,
    c.pg, mld,
    ott.id, type, family, order, class, phylum, kingdom,
    group, fg, 
    location.code, habitat, location, country, continent, latitude, longitude
  )

  
# calculate the mean and standard deviation 
outliers_info <- bodysize_raw %>% 
  
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    mean = mean(c.pg),
    sd = sd(c.pg),
    .groups = "drop"
  )

# Remove outliers and get average for multiple of the same individual.uid
bodysize_outliers <- bodysize_raw %>% 
  
  left_join(outliers_info, by = "taxa.name") %>% 
  
  filter(
    c.pg <= mean + (sd * 2) & c.pg >= mean - (sd * 2)
  ) %>% 
  
  # calculate average for each source and location
  group_by(
    individual.uid, location.code, source.code, original.sources
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
    individual.uid,
    ott.id,
    type,
    group,
    fg,
    taxa.name,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 
  distinct(
    individual.uid, .keep_all = TRUE
  ) %>% 
  
  filter(
    individual.uid %in% bodysize_outliers$individual.uid
  )


# Final bits
bodysize <- bodysize_outliers %>%
  
  left_join(
    locations_updated, by = "location.code"
  ) %>% 
  
  left_join(
    tax_updated, by = "individual.uid"
  ) %>% 
  
  mutate(
    uid = row_number()
  )%>% 
  
  # select columns and reorder
  select(
    uid, source.code, original.sources, taxa.name,
    mass, mld,
    ott.id, type, family, order, class, phylum, kingdom,
    group, fg, 
    location.code, habitat, location, country, continent, latitude, longitude
  )


# View data ----
ggplot(bodysize, aes(x = log10(mass), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()


# save
saveRDS(bodysize, file = "R/Data_outputs/database_products/final_products/bodysize.rds")




