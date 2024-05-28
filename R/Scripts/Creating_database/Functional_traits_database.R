## Functional traits list
# work flow
# get traits for each database
# attach taxonomic hierachy to each database
# left join all of them together so that any species that appear more than once will have all the traits put in the same row

########################################## data ====
rimet <- read_xlsx("./Raw data/Master biovolume_30 Oct 23.xlsx", sheet = "Rimmet")
kremer <- read_xlsx("./Raw data/Master biovolume_30 Oct 23.xlsx", sheet = "Kremer")
odume <- read_xlsx("./Raw data/Master biovolume_30 Oct 23.xlsx", sheet = "Odume")
hebert <- read_xlsx("./Raw data/Master biovolume_30 Oct 23.xlsx", sheet = "Hebert")

resolved <- readRDS("Data/taxize/resolved.rds")
tax_list <- readRDS("Data/taxize/tax_list.rds")
##################### Rimmet ====
# Reorganise and mutate
rimmet_groups <- rimet %>% 
  # Select columns I need and rename
  select(
    `Genus + species name`,
    `Organic carbon ratio (Wetzel et Likens 2000)`,
    `Functional groups (Reynolds 2002)`,
    `Functional groups (Padisak 2009)`,
    `Morpho-classification (Kruk 2010)`,
    `Nano/microphytoplankton`,
    `Mobility apparatus`,
    `Mobility apparatus: Flagella`,
    `Mobility apparatus: Raphe`,
    `Heterotrophic`,
    `Mixotrophic`,
    `Autotrophic`,
    `Colonial`,
    `Filament`
  ) %>% 
  rename(
    original.taxa.name = `Genus + species name`,
    organic.carbon.ration = `Organic carbon ratio (Wetzel et Likens 2000)`,
    fg.reynolds = `Functional groups (Reynolds 2002)`,
    fg.padisak = `Functional groups (Padisak 2009)`,
    fg.kruk = `Morpho-classification (Kruk 2010)`,
    size = `Nano/microphytoplankton`,
    mobility = `Mobility apparatus`,
    flagella = `Mobility apparatus: Flagella`,
    raphe = `Mobility apparatus: Raphe`
  ) %>%
  
  ## edit the boolean columns to make more readable
  mutate(
    ## mobility
    mobility = case_when(
      mobility == "1" ~ "mobile",
      mobility == "0" ~ "non mobile",
      TRUE ~ NA
    ),
    mobility.aparatus = case_when(
      flagella == "1" ~ "flagella",
      raphe == "1" ~ "raphe",
      TRUE ~ "non mobile"
    ),
    ## nutrient acquition
    nutrient.acquisition = case_when(
      Heterotrophic == "1" ~ "heterotrophic",
      Mixotrophic == "1" ~ "mixotrophic",
      Autotrophic == "1" ~ "Autotrophic",
      TRUE ~ NA
    ),
    ## form
    form = case_when(
      Colonial == 1 & Filament == 1 ~ "filament",
      Colonial == 0 & Filament == 1 ~ "filament",
      Colonial == 1 & Filament == 0 ~ "colony",
      Colonial == 0 & Filament == 0 ~ "unicellular"
    ),
    # source info
    source = 'Rimet & Druart., 2018'
  ) %>% 
  left_join(., select(resolved, original.taxa.name, taxa.name), by = "original.taxa.name") %>% 
  left_join(., tax_list, by = "taxa.name") %>% 
  ## remove redundant columns
  select(-flagella, -raphe, -Heterotrophic, -Mixotrophic, -Autotrophic, -Colonial, - Filament, -original.taxa.name) %>% 
  relocate(source, taxa.name, form, organic.carbon.ration, fg.reynolds, fg.padisak, fg.kruk, size, mobility, mobility.aparatus, nutrient.acquisition)

saveRDS(rimmet_groups, file = "Data/data_processing/rimmet_groups.rds")

##################### Odume ====
# Reorganise and mutate all
odume_groups <- odume %>% 
  
  # Select columns I need and rename
  select(`Taxon`,
         `Body shape`,
         `Mobility`,
         `Attachment mechanism`,
         `Functional feeding group (FFG) (Schael 2006)`,
         `Emergence season`
  )%>% 
  rename(
    original.taxa.name = Taxon,
    body.shape = `Body shape`,
    mobility = `Mobility`,
    attachment.mechanism = `Attachment mechanism`,
    ffg.schael = `Functional feeding group (FFG) (Schael 2006)`,
    emergence.season = `Emergence season`
  ) %>%
  
  # Remove the first rows with extra column headers
  slice(-(1:11)) %>% 
  mutate(
    source = 'Odume et al., 2023 ',
  ) %>%
  
  # add in taxa.name info
  left_join(., select(resolved, taxa.name, original.taxa.name), by = "original.taxa.name") %>% 
  left_join(., tax_list, by = "taxa.name") %>% 
  select(-original.taxa.name) %>% 
  relocate(source, taxa.name, body.shape, mobility, attachment.mechanism, ffg.schael, emergence.season)

saveRDS(odume_groups, file = "Data/data_processing/odume_groups.rds")

##################### Hebert ====
# Reorganise and mutate
hebert_subset <- hebert %>%
  
  # Select columns I need
  select(Column1, Column2, Column5, Column8, Column9, Column10, Column11, Column12, Column13, Column14, Column15) %>% 
  rename(
    genus = Column1,
    species = Column2,
    habitat = Column5,
    avg.length = Column8,
    min.length = Column9,
    max.length = Column10,
    length.ref = Column11,
    avg.biomass = Column12,
    min.biomass = Column13,
    max.biomass = Column14,
    biomass.ref = Column15
  ) %>%
  
  # Remove the first line with extra headings
  slice(-1) %>% 
  
  # filter out marine species and remove habitat column
  filter(habitat == "Freshwater") %>% 
  select(-habitat) %>% 
  
  # combine genus and species column to get a full taxa name and remove extra columns
  mutate(original.taxa.name = stri_c(genus, species, sep = " ")) %>% 
  select(-genus, -species) %>% 
  
  # Change "NA" to NA
  mutate_all(., ~na_if(., "NA"))%>% 
  
  # Change the commas to decimals
  mutate(  
    avg.length = as.numeric(stri_replace_all_regex(avg.length, ",", ".")),
    min.length = as.numeric(stri_replace_all_regex(min.length, ",", ".")),
    max.length = as.numeric(stri_replace_all_regex(max.length, ",", ".")),
    avg.biomass = as.numeric(stri_replace_all_regex(avg.biomass, ",", ".")),
    min.biomass = (stri_replace_all_regex(min.biomass, " ", "")), # get rid of unecessary spaces
    min.biomass = as.numeric(stri_replace_all_regex(min.biomass, ",", ".")),
    max.biomass = as.numeric(stri_replace_all_regex(max.biomass, ",", "."))
  ) %>% 
  
  #make a column to calculate the average length and average dm when only min and max are given and add it into avg.length 
  mutate(
    calc.avg.length = (min.length+max.length)/2,
    avg.length = coalesce(avg.length, calc.avg.length),
    calc.avg.biomass = (min.biomass+max.biomass)/2,
    avg.biomass = coalesce(avg.biomass, calc.avg.biomass)
  ) %>% 
  select(-calc.avg.length, - calc.avg.biomass) %>% 
  
  # when there is not value for avg.biomass but something in min or max take that as average
  mutate(
    avg.biomass = case_when(
      is.na(avg.biomass) & min.biomass > 0 ~ min.biomass,
      is.na(avg.biomass) & max.biomass > 0 ~ max.biomass,
      .default = avg.biomass
    )
  ) %>% 
  
  # convert avg.biomass to ug
  mutate(
    min.biomass = min.biomass*1000,
    max.biomass = max.biomass*1000,
    avg.biomass = avg.biomass*1000
  ) %>% 
  
  #Add in extra info
  mutate(
    type = 'Zooplankton',
    source = 'Hebert et al., 2016',
    life.stage = "adult",
    min.width = NA,
    max.width = NA,
    avg.width = NA,
    min.height = NA,
    max.height = NA,
    avg.height = NA,
    location = NA,
    country = NA,
    continent = NA,
    latitude = NA,
    longitude = NA,
    sample.year = NA,
    sample.month = NA,
    data.method = NA
  ) %>%
  relocate(source, type, original.taxa.name, biomass.ref, length.ref, sample.year, sample.month, location, country, continent, latitude, longitude, min.length, max.length, avg.length, min.width, max.width, avg.width, min.height, max.height, avg.height, min.biomass, max.biomass, avg.biomass, data.method)

saveRDS(hebert_subset, file = "Data/data_processing/hebert_subset.rds")

# get all the refernces
hebert_ref_biomass <-  hebert_subset %>% 
  select(biomass.ref) %>% 
  separate(biomass.ref, into = c("1", "2", "3"), sep = ",", convert = TRUE) %>% 
  pivot_longer(cols = c("1","2","3"), values_to = "value") %>% 
  distinct(value)%>% 
  mutate(
    Ref.code = value
  )%>% 
  select(-value ) %>% 
  left_join(., h_ref, by = "Ref.code")

saveRDS(hebert_ref_biomass, file = "Data/data_processing/hebert_biomass_ref.rds")

hebert_ref_length <-  hebert_subset %>% 
  select(length.ref) %>% 
  separate(length.ref, into = c("1", "2", "3"), sep = ",", convert = TRUE) %>% 
  pivot_longer(cols = c("1","2","3"), values_to = "value") %>% 
  distinct(value)%>% 
  mutate(
    Ref.code = value
  )%>% 
  select(-value ) %>% 
  left_join(., h_ref, by = "Ref.code")

saveRDS(hebert_ref_length, file = "Data/data_processing/hebert_length_ref.rds")

############################################# Combine into phyto and zoo ====
# Combine into phyto and zoo, only keep measurements and location data, functional trait data will be used in another 
phyto_raw <- bind_rows(kremer_subset, rimmet_subset)

%>% 
  # add in a uid number
  mutate(
    type.uid = "P",
    no.uid = row_number(),
    uid = stri_c(type.uid, no.uid, sep = "")
  ) %>% 
  select(-no.uid,-type.uid, - type.uid) %>% 
  relocate(uid, source, original.taxa.name, nu, life.stage, cell.biovol, nu.biovol, nu.biovol.mucilage, cells.per.nu, biovol.ref, sample.year, sample.month, location, country, continent, latitude, longitude)

zoo_raw <- rbind(hebert_subset, odume_subset) %>% 
  # add in a uid number
  mutate(
    type.uid = "Z",
    no.uid = row_number(),
    uid = stri_c(type.uid, no.uid, sep = ""),
    type = 'Zooplankton'
  ) %>% 
  select(-no.uid,-type.uid, - type.uid) %>%
  relocate(uid, source, original.taxa.name, biomass.ref, length.ref, sample.year, sample.month, location, country, continent, latitude, longitude, min.length, max.length, avg.length, min.width, max.width, avg.width, min.height, max.height, avg.height, min.biomass, max.biomass, avg.biomass, data.method)

saveRDS(phyto_raw, file = "Data/data_processing/phyto_raw.rds")
saveRDS(zoo_raw, file = "Data/data_processing/zoo_raw.rds")