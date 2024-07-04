## Formatting raw data from WOS search to copy into master excel sheet

## packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## Raw Data ----
wander_hl_2024 <- read.csv(here("Raw_data/WOS/Raw", "Wander_HL_2024.csv"))
arnoldt_s_2024 <- read.csv(here("Raw_data/WOS/Raw", "Arnoldt_S_2024.csv"))
sildever_s_2024 <- read.csv(here("Raw_data/WOS/Raw", "Sildever_S_2024.csv"))
zou_yp_2024 <- read.csv(here("Raw_data/WOS/Raw", "Zou_YP_2024.csv"))
blackburn_dp_2023 <- read.csv(here("Raw_data/WOS/Raw", "Blackburn_DP_2023.csv"))
rogalski_ma_2023 <- read.csv(here("Raw_data/WOS/Raw", "Rogalski_MA_2023.csv"))
ramaekers_l_2023_adult <- read.csv(here("Raw_data/WOS/Raw", "Ramaekers_L_2023_adult.csv"))
ramaekers_l_2023_juvenile <- read.csv(here("Raw_data/WOS/Raw", "Ramaekers_L_2023_juvenile.csv"))
klasios_n_2023 <- read.csv(here("Raw_data/WOS/Raw", "Klasios_N_2023.csv"))
theus_me_2023 <- read.csv(here("Raw_data/WOS/Raw", "Theus_ME_2023.csv"))

murphy_ca_2023 <- read.csv(here("Raw_data/WOS/Raw", "Murphy_CA_2023.csv"))

## Formatted data ----
wander_hl_2024_subset <- readRDS("Raw_data/WOS/Formatted/wander_hl_2024_subset.rds")
arnoldt_s_2024_subset <- readRDS("Raw_data/WOS/Formatted/arnoldt_s_2024_subset.rds")
sildever_s_2024_subset <- readRDS("Raw_data/WOS/Formatted/sildever_s_2023_subset.rds")
zou_yp_2024_subset <- readRDS("Raw_data/WOS/Formatted/zou_yp_2023_subset.rds")

## 2 Wander_hl_2024 ----
wander_hl_2024_subset <- wander_hl_2024 %>% 
  # filter out any without length measurments
  filter(
    !is.na(MarksInOcularMicrometer_No.)
  ) %>% 
  # rename columns I want to keep as is
  rename(
    original.taxa.name = TaxaID,
    life.stage = Nauplius
  ) %>% 
  mutate(
    # editing existing info
    
    # calculate length
    ObjectiveMagnification = as.numeric(stri_replace_all_fixed(ObjectiveMagnification, "x", "")),
    OcularMagnification = as.numeric(stri_replace_all_fixed(OcularMagnification, "x", "")),
    magnification = (ObjectiveMagnification*OcularMagnification),
    
    location = case_when(
      Reservoir == "BVR" ~ "Beaverdam Reservoir",
      Reservoir == "FCR" ~ "Falling Creek Reservoir",
      Reservoir == "CCR" ~ "Carvins Cove Reservoir",
      Reservoir == "GWR" ~ "Gatewood Reservoir",
      Reservoir == "SHR" ~ "Spring Hollow Reservoir",
      TRUE ~ NA
    ),
    latitude = case_when(
      Reservoir == "BVR" & Site == "50" ~ "37.31288",
      Reservoir == "BVR" & Site == "51" ~ "37.31267",
      Reservoir == "BVR" & Site == "49" ~ "37.312596",
      Reservoir == "CCR" ~ "37.3706",
      Reservoir == "FCR" ~ "37.30325",
      Reservoir == "GWR" ~ "37.0443528070071",
      Reservoir == "SHR" ~ "37.2308522269357",
      TRUE ~ NA
    ),
    longitude = case_when(
      Reservoir == "BVR" & Site == "50" ~ "-79.8159",
      Reservoir == "BVR" & Site == "51" ~ "-79.816592",
      Reservoir == "BVR" & Site == "49" ~ "-79.815657",
      Reservoir == "CCR" ~ "-79.9582",
      Reservoir == "FCR" ~ "-79.8373",
      Reservoir == "GWR" ~ "-80.8632991493157",
      Reservoir == "SHR" ~ "-80.1759001355518",
      TRUE ~ NA
    ),
    sample.year = stri_extract_first_regex(DateTime, "\\d{4}"),
    sample.month = stri_extract_first_regex(DateTime, "-\\d+-"),
    sample.month = stri_replace_all_fixed(sample.month, "-", ""),
    
    life.stage = case_when(
      stri_detect_regex(original.taxa.name, "Nauplii|Nauplius") ~ "Juvenile",
      TRUE ~ "Adult"
        ),
    
    # add new columns
    source.code = "2",
    original.source.code = "2",
    data.type = "Raw",
    sample.origin = "Captured",
    country = "USA",
    continent = "North America",
    habitat = "Reservoir",
    measuring.technique = "Body length",
    sex = NA
  ) %>% 
  select(
    -Reservoir, -DateTime, - Site, -StartDepth_m, -EndDepth_m, -Rep, -CollectionMethod, -Subsample, -LowestTaxonomicLevelOfID, -ObjectiveMagnification, -OcularMagnification
  ) %>% 
  relocate(
    source.code, original.source.code, original.taxa.name, life.stage, sex, measuring.technique, data.type, sample.type, sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude
  )

## Save
saveRDS(wander_hl_2024_subset, file = "Raw_data/WOS/Formatted/wander_hl_2024_subset.rds")

## 3 Arnoldt S 2024 ----
arnoldt_s_2024_subset <- arnoldt_s_2024 %>% 
  filter(
    Type == "Freshwater",
    Taxon != "Unknown"
  ) %>% 
  rename(
    original.taxa.name = Taxon,
    body.length = Prosome_length_µm,
    sex = Sex
  ) %>% 
  mutate(
   # Add NAs for missonf data
   sex = na_if(sex, ""),
   
   # life.stage
   life.stage = case_when(
     stri_detect_regex(Stage, "V|IV") ~ "Juvenile",
     TRUE ~ "Adult"
   ),
   
   # Date
   sample.year = stri_extract_first_regex(Acq_Date_Time, "\\d{4}"),
   sample.month = stri_extract_first_regex(Acq_Date_Time, "\\/\\d{2}\\/"),
   sample.month = stri_replace_all_regex(sample.month, "\\/", ""),
   
   # location info
   habitat = case_when(
     stri_detect_regex(Sample, "F1") ~ "Artifical pond",
     stri_detect_regex(Sample, "F2") ~ "Lake",
     stri_detect_regex(Sample, "F3") ~ "Lake",
     stri_detect_regex(Sample, "F4") ~ "Lake",
     stri_detect_regex(Sample, "F5") ~ "Lake",
     stri_detect_regex(Sample, "F6") ~ "Lake",
     TRUE ~ NA
   ),
   
   location = case_when(
     stri_detect_regex(Sample, "F1") ~ "Botanical garden, Gothenburg",
     stri_detect_regex(Sample, "F2") ~ "Trindemossen",
     stri_detect_regex(Sample, "F3") ~ "Finnsmossen",
     stri_detect_regex(Sample, "F4") ~ "Stora Delsjön",
     stri_detect_regex(Sample, "F5") ~ "Valmossen",
     stri_detect_regex(Sample, "F6") ~ "Torneträsk",
     TRUE ~ NA
   ),
     
   latitude = case_when(
     stri_detect_regex(Sample, "F1") ~ "57.677273",
     stri_detect_regex(Sample, "F2") ~ "57.665891",
     stri_detect_regex(Sample, "F3") ~ "57.675041",
     stri_detect_regex(Sample, "F4") ~ "57.680699",
     stri_detect_regex(Sample, "F5") ~ "57.643242",
     stri_detect_regex(Sample, "F6") ~ "68.361747",
     TRUE ~ NA
   ),
   
   longitude = case_when(
     stri_detect_regex(Sample, "F1") ~ "11.953868",
     stri_detect_regex(Sample, "F2") ~ "11.965299",
     stri_detect_regex(Sample, "F3") ~ "11.952768",
     stri_detect_regex(Sample, "F4") ~ "12.045753",
     stri_detect_regex(Sample, "F5") ~ "11.773005",
     stri_detect_regex(Sample, "F6") ~ "18.802152",
     TRUE ~ NA
   ), 
   
   country = "Sweeden",
   continent = "Europe",
   
   ## Extra info
   # source
   source.code = "3",
   original.source.code = "3",
   experimental.design = "In-situ",
   data.type = "Raw",
   sample.origin = "Captured",
   measuring.technique = "Prosome length"
  ) %>% 
  select(
    source.code, original.source.code, experimental.design, original.taxa.name, life.stage, sex, measuring.technique, data.type, sample.origin, sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length
  )

## Save
saveRDS(arnoldt_s_2024_subset, file = "Raw_data/WOS/Formatted/arnoldt_s_2024_subset.rds")

## 4 sildever_s_2023 ----
sildever_s_2024_subset <- sildever_s_2024 %>% 
  select(-X.1, -X.2, -Accession.numbers.of.DNA.markers) %>% 
  ## rename and reformat
  rename(
    strain = X,
    cell.length = X.3,
    min.cell.length = X.4,
    max.cell.length = X.5,
    cell.width = X.6,
    min.cell.width = X.7,
    max.cell.width = X.8
  ) %>% 
  .[-1,] %>% 
  mutate(
    min.cell.length = as.numeric(min.cell.length),
    max.cell.length = as.numeric(max.cell.length),
    min.cell.width = as.numeric(min.cell.width),
    max.cell.width = as.numeric(max.cell.width),
    
    cell.length.se = as.numeric(stri_extract_last_regex(cell.length, "\\d+\\.\\d*")),
    cell.length = as.numeric(stri_extract_first_regex(cell.length, "\\d+\\.\\d*")),
    cell.width.se = as.numeric(stri_extract_last_regex(cell.width, "\\d+\\.\\d*")),
    cell.width = as.numeric(stri_extract_first_regex(cell.width, "\\d+\\.\\d*")),
    
    ## size data
    # Calculate biovolume from min, max and average length ad width measurements assuming a shape of prolate spheroid
    min.cell.biovol = pi/6*min.cell.width^2*min.cell.length,
    max.cell.biovol = pi/6*max.cell.width^2*max.cell.length,
    cell.biovol = pi/6*cell.width^2*cell.length,
    
    # nu
    nu = "cell",
    
    ## location info
    location = case_when(
      stri_detect_fixed(strain, "VR66") ~ "Lake Västra Ringsjön",
      stri_detect_regex(strain, "R86|R129") ~ "Lake Ryssbysjön",
      stri_detect_fixed(strain, "SK147") ~ "Lake Skedviken",
      stri_detect_fixed(strain, "CCAP 11/119") ~ "Blelham Tarn",
    ),
    
    latitude = case_when(
      stri_detect_fixed(strain, "VR66") ~ "55.896517",
      stri_detect_regex(strain, "R86|R129") ~ "57.703567",
      stri_detect_fixed(strain, "SK147") ~ "59.768184",
      stri_detect_fixed(strain, "CCAP 11/119") ~ "54.396576",
    ),
    
    longitude = case_when(
      stri_detect_fixed(strain, "VR66") ~ "13.469619",
      stri_detect_regex(strain, "R86|R129") ~ "14.639673",
      stri_detect_fixed(strain, "SK147") ~ "18.274054",
      stri_detect_fixed(strain, "CCAP 11/119") ~ "-2.976605",
    ),
    
    country = case_when(
      location == "Blelham Tarn" ~ "United Kingdom",
      TRUE ~ "Sweden"
    ),
    
    habitat = "Lake",
    continent = "Europe",
    
    # Extra info
    original.taxa.name = "Limnomonas gaiensis",
    source.code = "4",
    original.source.code = "4",
    measuring.technique = "Geometric - Hillebrand/Sun&Liu",
    data.type = "Average",
    sample.origin = "Captured",
    life.stage = NA,
    sample.year = NA_character_,
    sample.month = NA_character_,
    sex = NA,
    experimental.design = "Ex-situ - lab"
      ) %>% 
  select(-strain) %>% 
  relocate(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    nu,
    min.cell.length, max.cell.length, cell.length, cell.length.se, min.cell.width, max.cell.width, cell.width, cell.width.se, min.cell.biovol, max.cell.biovol, cell.biovol
    )
  
## Save
saveRDS(sildever_s_2024_subset, file = "Raw_data/WOS/Formatted/sildever_s_2024_subset.rds")

## 5 Zou_YP_2023 ----
zou_yp_2024_subset <- zou_yp_2024 %>% 
  # Rename columns I want to keep
  rename(
    cell.biovol = volume
  ) %>% 
  mutate(
    ## add in full taxa names
    original.taxa.name = case_when(
      species == "ANK" ~ "Ankistrodesmus falcatus",
      species == "CHLA" ~ "Chlamydomonas sp",
      species == "SCE" ~ "Scenedesmus quadricauda",
      species == "SEL" ~ "Selenastrum capricornutum",
      species == "STA" ~ "Staurastrum gracile",
      TRUE ~ NA
    ),
    
    ## nu
    nu = "Cell",
    
    ## extra info
    source.code = "5",
    original.source.code = "5",
    life.stage = NA,
    sex = NA,
    measuring.technique = "Geometric - Hillebrand",
    data.type = "Raw",
    sample.origin = "Cultured",
    sample.year = NA_character_,
    sample.month = NA_character_,
    habitat = "Lab",
    location = "Carolina Biological Supply Company",
    country = "USA",
    continent = "North America",
    latitude = "36.048254",
    longitude = "-79.592173",
    experimental.design = "Ex-situ - lab"
    ) %>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    nu, cell.biovol
  )
  
## Save
saveRDS(zou_yp_2024_subset, file = "Raw_data/WOS/Formatted/zou_yp_2024_subset.rds")
  
## Murphy_CA_2023 ----
murphy_ca_2023_subset <- murphy_ca_2023 %>% 
  # Rename columsn i want to keep
  rename(
    original.taxa.name = Species,
    sample.year = Year,
    sample.month = Month,
    location = Site
  ) %>% 
  mutate(
    # calculate cell/nu biovolume
    cell.biovol = Biovolume_um3_mL/Density_N_mL,
    nu = "cell",
    nu.biovol = cell.biovol,
    
    # extra info
    #source.code = ,
    #original.source.code = ,
    life.stage = NA,
    sex = NA,
    measuring.technique = "Density/total biovol",
    data.type = "Raw",
    sample.type = "Measured from location",
    habitat = "Reservoirs",
    country = "USA",
    continent = "North America"
  ) %>% 
  select(
    source.code, original.source.code, original.taxa.name, life.stage, sex, measuring.technique, data.type, sample.type, sample.year, sample.month,
    habitat, location, country, continent,
    nu, nu.biovol, cell.biovol
  )
  
## 6 blackburn_dp_2023 ----
blackburn_dp_2023_subset <- blackburn_dp_2023 %>% 
  # rename columns i want to keep
  rename(
    body.length = zoo_length_measurement,
    life.stage = zoo_lifestage,
    sample.year = date_yyyy_mm_dd,
    original.taxa.name = zoo_taxa
  ) %>% 
  mutate(
    sample.year = as.character(sample.year),
    # location
    location = "Greiner lake",
    
    habitat = case_when(
      lake %in% c("1ST Lake", "CBL5", "CBL7", "ERA 4", "ERA 5", "Greiner Lake", "LL1") ~ "Lake",
      lake %in% c("Pond 12", "Pond 18", "Pond 2") ~ "Pond"
    ),
    
    latitude = case_when(
      lake == "1ST Lake" ~ "69.2083",
      lake == "CBL5" ~ "69.22305",
      lake == "CBL7" ~ "69.296388",
      lake == "ERA 4" ~ "69.28944",
      lake == "ERA 5" ~ "69.23777",
      lake == "Greiner Lake" ~ "69.15777",
      lake == "LL1" ~ "69.103055",
      lake == "Pond 12" ~ "69.171388",
      lake == "Pond 18" ~ "69.285277",
      lake == "Pond 2" ~ "69.200833",
      TRUE ~ NA
    ),
    
    longitude = case_when(
      lake == "1ST Lake" ~ "-104.7503",
      lake == "CBL5" ~ "-10475666",
      lake == "CBL7" ~ "-104.7575",
      lake == "ERA 4" ~ "-104.89611",
      lake == "ERA 5" ~ "-104.85138",
      lake == "Greiner Lake" ~ "-104.9992",
      lake == "LL1" ~ "-104.540833",
      lake == "Pond 12" ~ "-104.62111",
      lake == "Pond 18" ~ "-104.75666",
      lake == "Pond 2" ~ "-104.755278",
      TRUE ~ NA
    ),
    
    ## extra info
    source.code = "6",
    original.source.code = "6",
    sex = NA,
    measuring.technique = "Body length",
    data.type = "Raw",
    sample.origin = "Captured",
    country = "Canada",
    continent = "North America",
    sample.month = "08",
    experimental.design = "In-situ"
  ) %>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length
  )

## Save
saveRDS(blackburn_dp_2023_subset, file = "Raw_data/WOS/Formatted/blackburn_dp_2023_subset.rds")

## 8 rogalski_ma_2023 ----
rogalski_ma_2023_subset <- rogalski_ma_2023 %>% 
  ## rename columns i want to keep
  rename(
    body.length = Day7_length_mm
  ) %>% 
  ## Select data I want
  mutate(
    # rename treatment lakes to find ones to use
    Trt = case_when(
      Trt == "E" ~ "Egypt",
      Trt == "S" ~ "Sewell",
      Trt == "H" ~ "Hall",
      TRUE ~ NA
    )
  ) %>% 
  filter(
   Lake == Trt, # keep ones which were reared in the same water they were caught from
   Lake != "Sewell" # this is a brackish location
  ) %>% 
  mutate(
    # location info
    location = case_when(
      Lake == "Egypt" ~ "Egypt pond",
      Lake == "Hall" ~ "Hall pond",
      TRUE ~ NA
    ),
    
    latitude = case_when(
      location == "Egypt pond" ~ "44.518972",
      location == "Hall pond" ~"44.217999"
    ),
    
    longitude = case_when(
      location == "Egypt pond" ~ "-70.050440",
      location == "Hall pond" ~ "-70.456746"
    ),
    
    habitat = "Pond",
    country = "USA",
    continent = "North America",
    
    ## extra info
    original.taxa.name = "Daphnia ambigua",
    source.code = "8",
    original.source.code = "8",
    sex = NA,
    life.stage = "Adult",
    measuring.technique = "Eye to base of tail spine",
    data.type = "Raw",
    sample.origin = "Captured",
    sample.year = "2019",
    sample.month = NA,
    experimental.design = "Ex-situ - lab"
  )%>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length
  )

## Save
saveRDS(rogalski_ma_2023_subset, file = "Raw_data/WOS/Formatted/rogalski_ma_2023_subset.rds")

## 9 Ramaekers_l_2023 ----
ramaekers_l_2023_adult_subset <- ramaekers_l_2023_adult %>% 
  ## select control data
  filter(
    Treatment == "C"
  ) %>% 
  ## rename columns I want to keep
  rename(
    body.length = BS
  ) %>% 
  mutate(
    ## location
    habitat = "Pond",
    location = "West-Flanders",
    country = "Belgium",
    continent = "Europe",
    latitude = "51.053069",
    longitude = "3.145816",
    
    ## extra info
    original.taxa.name = "Daphnia magna",
    source.code = "9",
    original.source.code = "9",
    sex = NA,
    life.stage = "Adult",
    measuring.technique = "Eye to base of tail spine",
    data.type = "Raw",
    sample.origin = "Captured",
    sample.year = "2019",
    sample.month = NA,
    experimental.design = "Ex-situ - lab"
  )%>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length
  )

ramaekers_l_2023_juvenile_subset <-  ramaekers_l_2023_juvenile %>% 
  ## select control data
  filter(
    Treatment == "C"
  ) %>% 
  ## rename columns I want to keep
  rename(
    body.length = MeanBS
  ) %>% 
  mutate(
    ## location
    habitat = "Pond",
    location = "West-Flanders",
    country = "Belgium",
    continent = "Europe",
    latitude = "51.053069",
    longitude = "3.145816",
    
    ## extra info
    original.taxa.name = "Daphnia magna",
    source.code = "9",
    original.source.code = "9",
    sex = NA,
    life.stage = "Juvenile",
    measuring.technique = "Eye to base of tail spine",
    data.type = "Average",
    sample.origin = "Captured",
    sample.year = "2019",
    sample.month = NA,
    experimental.design = "Ex-situ - lab"
  )%>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length
  )
  
## Join adult and juvenile together
ramaekers_l_2023_subset <- rbind(ramaekers_l_2023_juvenile_subset, ramaekers_l_2023_adult_subset)
## Save
saveRDS(ramaekers_l_2023_subset, file = "Raw_data/WOS/Formatted/ramaekers_l_2023_subset.rds")

## klasios_n_2023 ----
klasios_n_2023_subset <- klasios_n_2023 %>% 
  ## Rename columns to keep
  rename(
    original.taxa.name = ZooplanktonGroup,
    body.length = LengthMicron,
    body.width = WidthMicron
  ) %>% 
  mutate(
    ## Location info
    habitat = "Lake",
    location = stri_c(Lake, "lake", sep = " "),
    country = "Canada",
    continent = "North America",
    
    latitude = case_when(
      Lake == "alta" ~ "50.116730",
      Lake == "brohm" ~ "49.821217",
      Lake == "chilliwack" ~ "49.057445",
      Lake == "cultus" ~ "49.057367",
      Lake == "deer" ~ "49.236103",
      Lake == "cowichan" ~ "48.876629",
      Lake == "lizard" ~ "48.606232",
      Lake == "pixie" ~ "48.595024"
    ),
    
    longitude = case_when(
      Lake == "alta" ~ "-122.981553",
      Lake == "brohm" ~ "-123.135023",
      Lake == "chilliwack" ~ "-121.414463",
      Lake == "cultus" ~ "-121.982622",
      Lake == "deer" ~ "-122.971390",
      Lake == "cowichan" ~ "-124.266104",
      Lake == "lizard" ~ "-124.223017",
      Lake == "pixie" ~ "-124.197592"
    ),
    
    ## extra info
    source.code = "10",
    original.source.code = "10",
    sex = NA,
    life.stage = NA,
    measuring.technique = NA,
    data.type = "Raw",
    sample.origin = "Captured",
    sample.year = "2021",
    sample.month = "08 - 09",
    experimental.design = "In-situ"
  )%>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length, body.width
  )

## Save
saveRDS(klasios_n_2023_subset, file = "Raw_data/WOS/Formatted/klasios_n_2023_subset.rds")

## theus_me_2023 ----
theus_me_2023_subset <- theus_me_2023 %>% 
  ## keep temp 18 and 23 and bpa 0 and 2 as these are reported naturally
  filter(
    Temp %in% c("18", "23") & BPA %in% c("0", "2")
  ) %>% 
  ## rename columns to keep
  rename(
    cell.biovol = avg_bv_per_count
  ) %>% 
  mutate(
    ## species name
    original.taxa.name = case_when(
      Species == "Cv" ~ "Chlorella vulgaris",
      Species == "Sq" ~ "Scenedesmus quadricauda",
      Species == "Ab" ~ "Ankistrodesmus braunii",
    ),
    
    ## location
    habitat = "Lab",
    location = "University of Texas",
    country = "USA",
    continent = "North America",
    latitude = "30.287523",
    longitude = "-97.739757",
    
    ## extra info
    source.code = "11",
    original.source.code = "11",
    sex = NA,
    life.stage = "Active",
    measuring.technique = "Geometric - Sun and Liu",
    data.type = "Raw",
    sample.origin = "Cultured",
    sample.year = "2021",
    sample.month = "01 - 03",
    experimental.design = "Ex-situ - lab"
  )%>% 
  select(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    cell.biovol
  )

## Save
saveRDS(theus_me_2023_subset, file = "Raw_data/WOS/Formatted/theus_me_2023_subset.rds")


















## combine together ----
all_wos_raw <- bind_rows(arnoldt_s_2024_subset, sildever_s_2024_subset, zou_yp_2024_subset, blackburn_dp_2023_subset, rogalski_ma_2023_subset, ramaekers_l_2023_subset, klasios_n_2023_subset, theus_me_2023_subset) %>% 
  relocate(
    source.code, original.source.code, experimental.design, data.type, sample.origin, measuring.technique, original.taxa.name, life.stage, sex,
    sample.year, sample.month,
    habitat, location, country, continent, latitude, longitude,
    body.length, body.width,
    nu, min.cell.length, max.cell.length, cell.length, cell.length.se,
    min.cell.width, max.cell.width, cell.width, cell.width.se,
    min.cell.biovol, max.cell.biovol, cell.biovol
  )

exp <- all_wos_raw %>% 
  filter(
    sample.origin == "Lab culture"
  )







