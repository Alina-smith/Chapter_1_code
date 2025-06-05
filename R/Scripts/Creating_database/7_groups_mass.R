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
bodysize_formatted <- readRDS("R/Data_outputs/database_products/final_products/bodysize_formatted.rds")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")
sources_list_update <- readRDS("R/Data_outputs/database_products/sources_list_update.rds")

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
bodysize_genus <- bs_avg %>% 
  
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

saveRDS(bodysize_genus, "R/data_outputs/database_products/final_products/bodysize_genus.rds")


############################## Now have a dataset of raw bodysizes with just one per individual but not functional group data


#### Add groups to main data ----

# Add to data
bs_fg <- bodysize_genus %>% 
  
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
bodysize_traits <- bodysize_outliers %>%
  
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
ggplot(bodysize_traits, aes(x = log10(mass), fill = type)) +
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~type, ncol = 1)+
  scale_y_log10()


# save
saveRDS(bodysize_traits, file = "R/Data_outputs/database_products/final_products/bodysize_traits.rds")




