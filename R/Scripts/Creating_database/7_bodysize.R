## Calculating the body size for ones I don't have it for
# all volum = um3
# all length, width and height = um,
# all mass = ug

# just taking the phytoplankton now

# Packages 
library(here)
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(data.table)

# Data ----
bodysize_location <- readRDS("R/Data_outputs/full_database/bodysize_location.rds")
tax_list <- readRDS("R/Data_outputs/taxonomy/gbif/tax_list.rds")

# Final all raw list including phyto and zooplankton ----

# make a list of all individual uids that has more than one measurement type to use in next step to remove datapoints I don't want
format_all_bs <- bodysize_location %>% 
  
  mutate(
    
    # change types for later merging
    uid = as.character(uid),
    
    # change measurement type to single when its raw but only has one value
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
    # First convert all units to be the same to make it easier
    body.size = case_when(
      units %in% c("mg", "mm") ~ body.size*1000,
      TRUE ~ body.size
      ),
    units = case_when(
      units == "mg" ~ "µg",
      units == "mm" ~ "µm",
      TRUE ~ units
      ),
    # set form number for missing one
    form = if_else(
      uid == "321726",
      "individual",
      form
      )
    ) %>%
  
  # don't need units because each measurement type is the same now
  select(
    - units
  ) %>% 
  
  # make nu column and rename form.no
  rename(
    ind.per.nu = form.no
  ) %>% 
  
  mutate(
  
    nu = case_when(
      ind.per.nu > 1 ~ "multi-cellular",
      ind.per.nu == 1 ~ "individual",
      
      form %in% c("multi-cell", "multi-cellular", "colony", "filament") ~ "multi-cellular",
      form == "individual" ~ "individual",
      TRUE ~ NA
    )
  ) %>% 
  
  # select and relocate columns
  
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    tax.uid, rank, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes, form
  )


# get location info to left join to bw_raw
location_info <- format_all_bs %>% 
  select(
    location.code, habitat, location, country, continent, latitude, longitude
  ) %>% 
  
  distinct(
    location.code, .keep_all = TRUE
  )

# Turn raw into averages
bs_raw <- format_all_bs %>% 
  
  filter(
    measurement.type == "raw"
  )%>% 
  
  group_by(
    tax.uid, source.code, original.sources, location.code, bodysize.measurement, life.stage, sex, sample.year, sample.month, nu
  ) %>% 
  
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
  
  left_join(
    location_info, by = "location.code"
  ) %>% 
  
  left_join(
    tax_list, by = "tax.uid"
  ) %>% 
  
   # add new individual.uid
  
  left_join(
    select(
      source_list, doi, new.source.code
    ), by = c("source.code" = "new.source.code")
  ) %>% 
  
  group_by(
    tax.uid, source.code, original.sources, location.code, life.stage, sex, sample.year, sample.month, nu
  ) %>% 
  
  mutate(
    individual.uid = paste(doi, cur_group_id(), sep = "-")
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    uid = paste("r", row_number(), sep = "-"),
    ind.per.nu = 1, # all individuals
    reps = NA,
    error.type = "se",
    measurement.type = "average",
    bodysize.measurement.notes = NA,
    form = nu,
    
    # change tyes for later merging
    sample.size = as.character(sample.size),
    error = as.character(error)
  ) %>% 
  
  ungroup() %>% 
  
  select(
    uid, individual.uid, source.code, original.sources, type, life.stage, sex, taxa.name,
    nu, ind.per.nu, body.size, bodysize.measurement, reps, sample.size, error, error.type, measurement.type,
    tax.uid, rank, species, genus, family, order, class, phylum, kingdom,
    sample.year, sample.month, location.code, habitat, location, country, continent, latitude, longitude,
    bodysize.measurement.notes, form
  )

# Combine all together
all_bodysize <- format_all_bs %>% 
  
  filter(
    !(measurement.type == "raw")
  ) %>% 
  
  bind_rows(
    bs_raw
  )

# save
saveRDS(all_bodysize, file = "R/Data_outputs/full_database/all_bodysize.rds")


# Just phyto ----

x <- all_bodysize %>% 
  distinct(
    life.stage
  )

cell_volume <- all_bodysize %>% 
  
  filter(
    type == "Phytoplankton",
    life.stage %in% c("adult", "active")
  ) %>% 
  filter(
    nu == "multi-cellular"
  )
%>% 
  pivot_wider(
    id_cols = individual.uid,
    names_from = nu,
    values_from = body.size
  ) %>% 
  unnest()












# Make a column for individual and nu body size
ind.body.size = case_when(
  nu == "individual" ~ body.size,
  nu == "multi-cellular" & !is.na(ind.per.nu) ~ body.size/ind.per.nu,
  nu == "multi-cellular" & is.na(ind.per.nu) ~ NA,
  TRUE ~ NA
),

nu.size = body.size



  
  
cell_volume <- format_all_bs %>% 
  
  filter(
    !(measurement.type == "raw")
  ) %>% 
  
  # remove colum ns not relevent for phytoplankton
  select(
    - life.stage,
    - sex
  )

# save
saveRDS(all_bodysize, file = "R/Data_outputs/full_database/all_bodysize.rds")


raw_size <- all_bodysize %>% 
  filter(
    type == "Phytoplankton",
    measurement.type == 
  )



cell_volume <- all_bodysize %>% 
  
  filter(
    type == "Phytoplankton",
    life.stage != "dormant" # keep just active ones
  ) %>% 
  
  # rename nu columns to cells
  rename(
    cells.per.nu = ind.per.nu,
    cell.size = ind.body.size,
  ) %>% 
  
  group_by(
    uid, tax.uid, source.code, original.sources, location.code, bodysize.measurement
  ) %>% 
  
  summarise(
    individual.body.size = mean(cell.size),
    multicellular.body.size = mean(nu.size),
    #uid = cur_group_id()
  ) %>% 
  
  ungroup() 
%>% 
  
  group_by(
    tax.uid, source.code, original.sources, location.code
  ) %>% 
  
  mutate(
    uid = cur_group_id()
  ) %>% 
  
  ungroup()







%>% 
  
  pivot_wider(
    id_cols = individual.uid,
    names_from = bodysize.measurement,
    values_from = c(cell.size, nu.size)
  ) %>%
  
  unnest() %>% 
  
  rename(
    cell.volume = cell.size_biovolume,
    cell.length = cell.size_length,
    cell.width = cell.size_width,
    cell.height = cell.size_height,
    cell.depth = cell.size_depth,
    cell.diameter = cell.size_diameter,
    cell.dry.mass = `cell.size_dry mass`,
    
    nu.volume = nu.size_biovolume,
    nu.length = nu.size_length,
    nu.width = nu.size_width,
    nu.height = nu.size_height,
    nu.depth = nu.size_depth,
    nu.diameter = nu.size_diameter,
    nu.dry.mass = `nu.size_dry mass`,
  )






  
  # calculate mass from biovolume
  mutate(
    cell.mass = case_when(
      bodysize.measurement == "biovolume" ~ cell.size*(1*10^-6),
      TRUE ~ NA
    )
  )

x <- cell_volume %>% 
  filter(
    is.na(cell.mass)
  )

mass = volume*(1*10^-6)

x <- cell_volume %>% 
  distinct(
    bodysize.measurement
  )



%>% 
    
    # make two columns for nu (multiple cells) and individuals
    # some are written wrong in the raw data so just covering all variations to be sure
    
    cell.size = case_when(
      nu == "individual" ~ body.size,
      nu == "multi-cellular" & !is.na(cells.per.nu) ~ body.size/cells.per.nu,
      nu == "multi-cellular" & is.na(cells.per.nu) ~ NA,
      TRUE ~ NA
    ),
    
    nu.size = body.size
  )
  




%>% 
  
  pivot_wider(
    id_cols = individual.uid,
    names_from = bodysize.measurement,
    values_from = c(body.size)
  ) %>%
  
  unnest()
      
  
  # get average for each source for taxa at each location for each measurement type - means all sources will be averages now
  
  group_by(
    uid, tax.uid, source.code, original.sources, location.code, bodysize.measurement
  ) %>% 
  
  summarise(
    individual.body.size = mean(cell.size),
    multicellular.body.size = mean(nu.size),
    #uid = cur_group_id()
  ) %>% 
  
  ungroup() %>% 
  
  group_by(
    tax.uid, source.code, original.sources, location.code
  ) %>% 
  
  mutate(
    uid = cur_group_id()
  ) %>% 
  
  ungroup()

%>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = bodysize.measurement,
    values_from = c(body.size)
  )
  


# add in rest of data
# get distinct location codes
dist_loc_code <- bodysize_location %>% 
  select(
    location.code, latitude, longitude, location, country, continent, habitat
  ) %>% 
  distinct(
    location.code, .keep_all = TRUE
  )
  
body_size <- average_bodysize %>% 

  left_join(
    select(
      tax_list, - resolved.taxa.name),
    by = "tax.uid"
    ) %>% 
  
  left_join(
      dist_loc_code,
      by = "location.code"
    ) %>% 
  mutate(
    uid = row_number()
  ) %>% 
  
  pivot_wider(
    id_cols = uid,
    names_from = bodysize.measurement,
    values_from = c(individual.body.size, multicellular.body.size)
  ) %>% 
  
  rename(
    ind.volume = individual.body.size_biovolume,
    ind.length = individual.body.size_length,
    ind.width = individual.body.size_width,
    ind.height = individual.body.size_height,
    ind.depth = individual.body.size_depth,
    ind.diameter = individual.body.size_diameter,
    ind.dry.mass = `individual.body.size_dry mass`,
    
    multi.volume = multicellular.body.size_biovolume,
    multi.length = multicellular.body.size_length,
    multi.width = multicellular.body.size_width,
    multi.height = multicellular.body.size_height,
    multi.depth = multicellular.body.size_depth,
    multi.diameter = multicellular.body.size_diameter,
    multi.dry.mass = `multicellular.body.size_dry mass`,
  ) 
%>% 
  
  unnest( cols = everything()
  ) %>% 
  
  mutate(
    uid = paste(uid_biovolume, uid_length, uid_height, uid_width, uid_diameter, `uid_dry mass`, uid_depth, sep = ","),
    uid = stri_replace_all_regex(uid, "NA,|NA|,NA", ""),
    uid = na_if(uid, ""),
    uid = as.numeric(stri_extract_first_regex(uid, "\\d+"))
  ) %>% 
  
  select(
    -starts_with("uid_")
  ) %>% 
  
  left_join(
    ., select(
      bodysize_location, - body.size, - units, - bodysize.measurement, - individual.uid),
    by = "uid"
  ) %>% 
  
  select(
    - life.stage,
    - sample.year, # getting rid of dates because not need them for what I'm doing but will keep in raw data
    - sample.month,
    - sex, # only relevent for the zooplankton
    - bodysize.measurement.notes, # don't need
    - reps,
    - sample.size,
    - error,
    - error.type
  )
  
# get average from 
x <- multiple_measurements %>% 
  filter(
    is.na(sample.year)
  )

group_by(
  individual.uid, sample.year, bodysize.measurement, location.code
) %>% 
  
  summarise(
    individual.body.size = mean(individual.body.size),
    multicellular.body.size = mean(multicellular.body.size)
  )
  
  # get averages from raw data so all are averages
  
  group_by(taxa.name, source.code) %>% 
  
  mutate(
    volume = if_else(
      measurement.type == "raw",
      mean(volume),
      volume 
    )
  ) 

raw <- multiple_measurements %>% 
  filter(
    measurement.type == "raw"
  )
%>% 
  
  mutate(
    mass = volume*(1*10^-6)
  ) %>% 
  


  
  ## Averages for multiple indivdual.uid measurments
  # some papers gave averages for each individual and some gave raw so get averages for ones that are raw
  group_by(
    individual.uid
  ) %>% 
  
  mutate(
    mass = mean(mass)
  ) %>% 
  
  distinct(individual.uid, .keep_all = TRUE) %>% 
  
  ungroup() %>% 
  
  mutate(
    group = case_when(
      bodysize.measurement == "biovolume" ~ "phytoplankton",
      TRUE ~ "zooplankton"
    )
  ) %>% 
  
  relocate(
    uid, individual.uid, tax.uid, source.code, original.sources, accepted.taxa.name, form, form.no, life.stage, sex, mass, min.body.size, max.body.size, body.size, units, bodysize.measurement, sample.size, error, error.type, sample.year, sample.month
  )

# save
saveRDS(bodysize_data, file = "R/Data_outputs/full_database/bodysize_data.rds")
