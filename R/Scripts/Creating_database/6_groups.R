# formatting trait data to add into the full database in next script

# Packages 
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)

# Import data ----
# These are all large databases that I have already used that include R other traits
# Phyto
rimmet <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimmet")
kruk <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "kruk")
lt <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Laplace-Treyture")
kremer <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Kremer")
rimet_2012 <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Rimet_2012")
padisak <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "padisak")
wang <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "wang")

# Zoo
gavrilko <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Gavrilko")
odume <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Odume")
no <- read_xlsx("raw_data/master_db_data.xlsx", sheet = "Neury-Ormanni")

# Format----
## Phyto ----
### rimet_2012 ----
t_rimet_2012 <- rimet_2012 %>% 
  select(`genus + species + var`, Shape, Mobile, Colonial, `Non-colonial`, `Filament colony`) %>% 
  rename(taxa.name = `genus + species + var`, body.shape = Shape, motility = Mobile) %>% 
  # Make edits
  mutate(
    motility = if_else(
      motility == 1,
      "Motile",
      "Non-motile"
    ),
    life.form = case_when(
      `Non-colonial` == 1 ~ "Uni-cellular",
      Colonial == 1 & `Filament colony` == 1 ~ "Colonial/Filamentous",
      Colonial == 1 ~ "Colonial",
      `Filament colony` == 1 ~ "Filamentous",
      TRUE ~ NA
    ),
    ref = "rimet_2012",
    remove = if_else(
      is.na(body.shape) & is.na(life.form) & is.na(motility),
      "remove",
      "keep"
    )
  ) %>% 
  filter(
    remove == "keep"
  ) %>% 
  select(-Colonial, -`Non-colonial`, -`Filament colony`, -remove)
  

### Kremer ----
t_kremer <- kremer %>% 
  select(original.taxa.name,nu) %>%
  rename(taxa.name = original.taxa.name) %>% 
  # Make edits
  mutate(
    motility = case_when(
      stri_detect_regex(nu, "Motile") ~ "Motile",
      stri_detect_regex(nu, "Nonmotile") ~ "Non-motile",
      TRUE ~ NA
    ),
    life.form = case_when(
      stri_detect_regex(nu, "Colonial") ~ "Colonial",
      stri_detect_regex(nu, "Filament") ~ "Filamentous",
      TRUE ~ NA
    ),
    ref = "kremer",
    remove = if_else(
      is.na(motility) & is.na(life.form),
      "remove",
      "keep"
    )
  ) %>%
  filter(remove == "keep") %>% 
  select(- nu, -remove) %>% 
  distinct(taxa.name, .keep_all = TRUE)
  
### Rimmet ----
#2018, padisak and reynolds

t_rimmet <- rimmet %>% 
  select(`Genus + species name`, `Functional groups (Padisak 2009)`, `Geometrical shape of the cell`, `Morpho-classification (Kruk 2010)`, `Mobility apparatus`, `Mobility apparatus: Flagella`, `Mobility apparatus: Raphe`, `Colonial`, `Filament`, `Heterotrophic`, `Mixotrophic`) %>% 
  rename(taxa.name = `Genus + species name`, fg = `Functional groups (Padisak 2009)`, body.shape = `Geometrical shape of the cell`, morpho.class.kruk2010 = `Morpho-classification (Kruk 2010)`) %>% 
  # make edits
  mutate(
    motility = if_else(
      `Mobility apparatus` == 1,
      "Motile",
      "Non-motile"
    ),
    motility.method = case_when(
      `Mobility apparatus: Flagella` == 1 & `Mobility apparatus: Raphe` == 1 ~ "Flagella/Raphe",
      `Mobility apparatus: Flagella` == 1 ~ "Flagella",
      `Mobility apparatus: Raphe` == 1 ~ "Raphe",
      
      TRUE ~ NA
    ),
    trophic.group = case_when(
      `Heterotrophic` == 1 & `Mixotrophic` == 1 ~ "Heterotroph/Mixotroph",
      `Heterotrophic` == 1 ~ "Heterotroph",
      `Mixotrophic` == 1 ~ "Mixotroph",
      
      TRUE ~ NA
    ),
    ref = "rimmet",
    remove = if_else(
      is.na(fg) & is.na(morpho.class.kruk2010) & is.na(motility.method) & is.na(body.shape) & is.na(trophic.group) & is.na(motility),
      "remove",
      "keep"
    )
  ) %>%
  filter(remove == "keep") %>% 
  select(-remove, - `Mobility apparatus`, - `Mobility apparatus: Flagella`, - `Mobility apparatus: Raphe`, - `Colonial`, - `Filament`, - `Heterotrophic`, - `Mixotrophic`)

### Kruk ----
# 2017, reynolds

t_kruk <- kruk %>% 
  select(Species_name, `Classification by Experts`, `Life_form`) %>% 
  rename(taxa.name = Species_name, fg = `Classification by Experts`, life.form = `Life_form`
  ) %>% 
  # Make edits
  mutate(
    life.form = case_when(
      life.form == 1 ~ "Uni-cellular",
      life.form == 2 ~ "Colonial",
      life.form == 3 ~ "Filamentous",
    ),
    ref = "kruk",
    remove = if_else(
      is.na(fg) & is.na(life.form),
      "remove",
      "keep"
    )
  ) %>% 
  filter(remove == "keep") %>% 
  select(-remove)

### Padisak -----
# 2009, padisak

t_padisak <- padisak %>% 
  select(padisak.r.group, taxa.name) %>% 
  rename(fg = padisak.r.group)%>% 
  # make edits
  mutate(ref = "padisak") %>% 
  filter(!is.na(fg))

### Lt ----
#2021, reynolds using updated padisak

t_lt <- lt %>% 
  select(Taxa_Name, Reynolds_Group, Life_Form, Motility, Flagellum, Phytobs_Cell_Form, Nutrition) %>% 
  rename(taxa.name = Taxa_Name, fg = Reynolds_Group, motility = Motility, motility.method = Flagellum, life.form = Life_Form, body.shape = Phytobs_Cell_Form, trophic.group = Nutrition) %>% 
  # make edits
  mutate(
    motility = if_else(
      motility == 1,
      "Motile",
      "Non-motile"
    ),
    motility.method = if_else(
      motility.method == 1,
      "Flagella",
      NA
    ),
    life.form = case_when(
      life.form == "Cel." ~ "Uni-cellular",
      life.form == "Col." ~ "Colonial",
      life.form == "Fil." ~ "Filamentous",
    ),
    ref = "lt",
    remove = if_else(
      is.na(fg) & is.na(motility.method) & is.na(body.shape) & is.na(trophic.group) & is.na(motility) & is.na(life.form),
      "remove",
      "keep"
    )
  ) %>%
  filter(remove == "keep") %>% 
  select(-remove)

### Wang ----
#  uses non updated reynolds

t_wang <- wang %>% 
  mutate(ref = "wang") %>% 
  filter(!is.na(fg))

## Zoo ----

### Gavrilko ----
t_gavrilko <- gavrilko %>% 
  select(species, functional.group, swimming, crawling, capture, primary.filtration, secondary.filtration, verification, suction, gathering) %>% 
  rename(taxa.name = species, fg = functional.group) %>% 
  # Make edits
  mutate(
    motility = "Motile",
    motility.method = case_when(
      swimming == 1 & crawling == 1 ~ "Swimming/Crawling",
      swimming == 1 ~ "Swimming",
      crawling == 1 ~ "Crawling",
      TRUE ~ NA
    ),
    capture = if_else(capture == 1, "Capture", ""),
    primary.filtration = if_else(primary.filtration == 1, "Primary.filtration", ""),
    secondary.filtration = if_else(secondary.filtration == 1, "Secondary.filtration", ""),
    verification = if_else(verification == 1, "Verification", ""),
    suction = if_else(suction == 1, "Suction", ""),
    gathering = if_else(gathering == 1, "Gathering", ""),
    feeding.type = stri_c(capture, primary.filtration, secondary.filtration, verification, suction, gathering, sep = " "),
    feeding.type = stri_trim(feeding.type, side = "both"),
    feeding.type = stri_replace_all_regex(feeding.type, "     |    |   |  | ", "/",),
    feeding.type = case_when(
      stri_detect_regex(feeding.type, "Primary.filtration") ~ stri_replace_first_regex(feeding.type, "Primary.filtration", "Primary filtration"),
      stri_detect_regex(feeding.type, "Secondary.filtration") ~ stri_replace_first_regex(feeding.type, "Secondary.filtration", "Secondary filtration"),
      TRUE ~ feeding.type
    ),
    ref = "gavrilko",
    fg = as.character(fg),
    remove = if_else(
      is.na(fg) & is.na(motility.method) & is.na(motility) & is.na(feeding.type),
      "remove",
      "keep"
    )
  ) %>%
  filter(remove == "keep") %>% 
  select(- capture, -primary.filtration, -secondary.filtration, -verification, -suction, -gathering, -swimming, -crawling, -remove) %>% 
  distinct(taxa.name, .keep_all = TRUE) 

### odume ----
t_odume <- odume %>% 
  select(Taxon, Mobility, `Functional feeding group (FFG) (Schael 2006)`, `Body shape`, `Trophic status preferences`) %>% 
  rename(taxa.name = Taxon, motility.method = Mobility, feeding.type = `Functional feeding group (FFG) (Schael 2006)`, body.shape = `Body shape`, trophic.group = `Trophic status preferences`) %>% 
  filter(
    !is.na(taxa.name),
    !(taxa.name == "Taxon")
  ) %>% 
  # make edits
  mutate(
    motility = "Motile",
    motility.method = case_when(
      motility.method == "Crawler" ~ "Crawling",
      motility.method == "Burrower" ~ "Burrowing",
      motility.method == "Swimmer" ~ "Swimming",
      motility.method %in% c("Walker", "walker") ~ "Walking",
      motility.method == "Sprawler" ~ "Sprawling",
      motility.method == "Skater" ~ "Skating",
      motility.method == "Climber" ~ "Climbing",
      TRUE ~ NA
    ),
    feeding.type = case_when(
      stri_detect_regex(feeding.type, "(?i)Predator") ~ "Capture",
      stri_detect_regex(feeding.type, "(?i)Filter feeder") ~ "Filtration",
      stri_detect_regex(feeding.type, "(?i)Shredder") ~ "Shredder",
      stri_detect_regex(feeding.type, "(?i)Scraper") ~ "Scraper",
      stri_detect_regex(feeding.type, "(?i)Grazer") ~ "Grazer",
      stri_detect_regex(feeding.type, "(?i)Deposit feeder") ~ "Deposit feeder",
      TRUE ~ NA
    ),
    trophic.group = case_when(
      stri_detect_regex(feeding.type, "(?i)Omnivore") ~ "Omnivore",
      TRUE ~ NA
    ),
    body.shape = case_when(
      stri_detect_regex(body.shape, "(?i)Streamlined") ~ "Streamlined",
      stri_detect_regex(body.shape, "(?i)Flattened") ~ "Flattened",
      stri_detect_regex(body.shape, "(?i)Spherical") ~ "Spherical",
      stri_detect_regex(body.shape, "(?i)Cylindrical") ~ "Cylindrical",
      stri_detect_regex(body.shape, "(?i)Ovate") ~ "Oval",
      TRUE ~ NA
    ),
    ref = "odume",
    remove = if_else(
      is.na(body.shape) & is.na(motility.method) & is.na(motility) & is.na(feeding.type) & is.na(trophic.group),
      "remove",
      "keep"
    )
  ) %>%
  filter(remove == "keep") %>% 
  select(-remove)
  

### no ----

t_no <- no %>% 
  select(Species, BodyShape, SubstrateRelation, TrophicType, FeedMode) %>% 
  rename(taxa.name = Species, body.shape = BodyShape, motility.method = SubstrateRelation, trophic.group = TrophicType, feeding.type = FeedMode,) %>% 
  # make edits
  mutate(
    motility = if_else(
      !is.na(motility.method),
      "Motile",
      NA
    ),
    ref = "no",
    remove = if_else(
      is.na(body.shape) & is.na(motility.method) & is.na(motility) & is.na(feeding.type) & is.na(trophic.group),
      "remove",
      "keep"
    )
  ) %>%
  filter(remove == "keep") %>% 
  select(-remove)


### Join together ----
# join together to make it easier to standadise all the the different columns and then separate out again
traits_raw <- bind_rows(t_padisak, t_rimmet, t_kruk, t_lt, t_wang, t_gavrilko, t_kremer, t_no, t_odume, t_rimet_2012) %>% 
  mutate(
    fg = toupper(fg),
    fg = if_else(
      fg == "#NA",
      NA,
      fg
    ),
    motility.method = case_when(
      motility.method == "Crawling_gliding" ~ "Crawling",
      motility.method == "Endobenthic" ~ NA,
      motility.method == "Swimmer" ~ "Swimming",
      motility.method == "Walker" ~ "Walking",
      TRUE ~ motility.method
    ),
    feeding.type = case_when(
      feeding.type == "Absorber_sucker" ~ "Absorbtion",
      feeding.type == "Filter_feeder" ~ "Filtration",
      TRUE ~ feeding.type
    ),
    trophic.group = case_when(
      trophic.group == "Autotrophic" ~ "Autotroph",
      trophic.group == "Mixotrophic" ~ "Mixotroph",
      trophic.group == "Heterotrophic" ~ "Heterotroph",
      trophic.group == "ND" ~ NA,
      TRUE ~ trophic.group
    ),
    body.shape = tolower(body.shape),
    body.shape = stri_trim(body.shape, side = "both"),
    body.shape = case_when(
      body.shape %in% c("#na", "palmate", "straurastrum", "monopodial", "ceratium form", "biovolume omnidia de ptco", "biovolume omnidia ptla", "omnidia", "hemisphere", "polypodial", "tetrahedre") ~ NA,
      body.shape == "cylindrical" ~ "cylinder",
      body.shape == "conical" ~ "cone",
      body.shape %in% c("ell", "elliptic", "rotationnal ellipsoid") ~ "ellipsoid",
      body.shape == "oval" ~ "ovoid",
      body.shape %in% c("sphe", "spherical") ~ "sphere",
      body.shape == "sphe/3" ~ "1/3 of a sphere",
      body.shape %in% c("tub", "barrel") ~ "tube",
      body.shape == "cone with half sphere" ~ "cone + half sphere",
      body.shape %in% c("box", "rectangular") ~ "rectangular box",
      body.shape == "doco" ~ "double cone",
      body.shape == "ellcyl" ~ "elliptic cylinder",
      body.shape == "rhp" ~ "rhomboid prism",
      body.shape == "trel" ~ "triaxial ellipsoid",
      TRUE ~ body.shape
    )
  ) %>%
  # put all taxa onto seperate rows
  separate_rows(taxa.name, sep = ",|/") %>% 
  mutate(
    # Get rid of any white spaces on the ends
    old.taxa.name = str_trim(taxa.name),
    # remove the var., f. and cf.
    old.taxa.name = case_when(
      stri_detect_regex(old.taxa.name, "cf\\.|aff.") ~ stri_replace_first_regex(old.taxa.name, "cf\\.|aff.", " "),
      stri_detect_regex(old.taxa.name, "var\\.|f\\.| var ") ~ stri_extract_first_regex(old.taxa.name, "(\\S+ )*\\S+ \\S+(?= var\\.| f\\.| var )"),
      taxa.name == "" ~ NA,
      TRUE ~ old.taxa.name
    ),
    old.taxa.name = stri_trim(old.taxa.name, side = "both"),
    old.taxa.name = stri_replace_all_regex(old.taxa.name, "  ", "")
  ) %>% 
  select(- taxa.name)

# save
saveRDS(traits_raw, "R/data_outputs/database_products/traits_raw.rds")

## Update taxa.names ----
# These have the names pre taxonomy step so need to update them
# Rimmet, Kruk and lt data have already gone through the taxonomy step so can just add in the names from the bodysize_taxonomy data
# Padisak hasn't gone through the taxonomy step so need to do these

#### Import data ----

bodysize_taxonomy <- readRDS("R/Data_outputs/database_products/bodysize_taxonomy.rds") %>% 
  select(original.taxa.name, taxa.name) %>% 
  distinct(original.taxa.name, .keep_all = TRUE)
traits_raw <- readRDS("R/Data_outputs/database_products/traits_raw.rds")
updated_spec_char <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "special_characters")
tax_list_raw <- readRDS("R/Data_outputs/database_products/taxonomy/tax_list_raw.rds")

#### Special characters ----

spec_char <- traits_raw %>% 
  distinct(old.taxa.name) %>%
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
    updated.taxa.name = stri_trim(updated.taxa.name, side = "both")
  ) %>% 
  
  filter(updated.taxa.name != "Unknown") %>% 
  
  left_join(
    .,select(bodysize_taxonomy, original.taxa.name, taxa.name),
    by = c("updated.taxa.name" = "original.taxa.name")
  ) %>% 
  
  distinct(old.taxa.name, .keep_all = TRUE) %>% 
  
  select(old.taxa.name, taxa.name) %>% 
  
  # For ones in Padisak that havent been through the taxonomy step first check if there are any that are already in taxa.name and use that
  mutate(
    taxa.name = if_else(
      is.na(taxa.name) & old.taxa.name %in% bodysize_taxonomy$taxa.name,
      old.taxa.name,
      taxa.name
    )
  )

saveRDS(spec_char, "R/data_outputs/database_products/spec_char")
#### Run remaning names through taxonomy steps ----

##### clean ----
# Get a list of all distinct names with no taxa.name
to_clean <- spec_char %>% 
  # Select ones I need to manually clean
  filter(is.na(taxa.name)) %>% 
  select(old.taxa.name) %>% 
  distinct(old.taxa.name) %>% 
  # Convert to a string of names
  pull(old.taxa.name)

# View names
glimpse(to_clean)

##### gna_verifier ----
# Run string through the verifyer and select columns I want - run through a catchall because it will throw an error for the whole thing when it doesn't recognize a name
cleaned_gna_traits <- do.call(rbind, lapply(to_clean, function(name) {
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

# save
saveRDS(cleaned_gna_traits, "R/data_outputs/database_products/cleaned_gna_traits.rds")

##### Manual clean ----
cleaned_traits <- cleaned_gna_traits %>% 
  
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
    !is.na(cleaned.taxa.name)
  )

saveRDS(cleaned_traits, "R/data_outputs/database_products/cleaned_traits.rds")

##### tnrs_match_names ----
# Run the cleaned names through tnrs_match_names to get updated versions of names from open tree of life (otl)

resolved_tol_traits <- tnrs_match_names(cleaned_traits$cleaned.taxa.name) %>% 
  select(unique_name, search_string) %>% 
  mutate(
    unique_name = if_else(
      search_string == "aulacoseira ambigua ambigua",
      "Aulacoseira ambigua",
      unique_name
    )
  ) %>% 
  rename(old.taxa.name = search_string, taxa.name = unique_name)

# save
saveRDS(resolved_tol_traits, "R/data_outputs/database_products/resolved_tol_traits.rds")

##### final updated names list ----
# import data
resolved_tol_traits <- readRDS("R/data_outputs/database_products/resolved_tol_traits.rds")
spec_char <- readRDS("R/data_outputs/database_products/spec_char")
traits_raw <- readRDS("R/data_outputs/database_products/traits_raw.rds")

names_list_updated <- spec_char %>% 
  # Remove the ones from spec_char that didn't have a taxa.name
  filter(!is.na(taxa.name)) %>% 
  
  # Add back in the names removed above with the taxa.names added in
  bind_rows(resolved_tol_traits) %>% 
  
  # just remove two random var. ones as have the species version in there
  filter(
    !(taxa.name %in% c("Aulacoseira granulata var. angustissima", "Staurastrum avicula var. lunatum"))
  )%>% 
  
  # Make old.taxa.name lower case so all are the same because some have upper and some have lower
  mutate(
    old.taxa.name = tolower(old.taxa.name)
  ) %>% 
  
  distinct(old.taxa.name, .keep_all = TRUE)

#### Format traits_raw ----

traits_list_all <- traits_raw %>%
  # Update taxa.names
  mutate(
    # make old.taxa.name lower case for joining
    old.taxa.name = tolower(old.taxa.name)
  ) %>% 
  left_join(names_list_updated,  by = "old.taxa.name") %>% 
  filter(
    !is.na(taxa.name),
    !is.na(fg),
    ref %in% c("padisak", "rimmet", "kruk", "lt", "wang", "gavrilko")
    ) %>% 
  select(taxa.name, fg, ref) %>% 
  group_by(taxa.name, fg) %>% 
  distinct(taxa.name, .keep_all = TRUE) %>% 
  pivot_wider(names_from = ref, values_from = fg, values_fn = function(x) paste(unique(x), collapse = "/")) %>% 
  
  # change Dinoflagellata to Myzozoa
  mutate(
    taxa.name = if_else(
      taxa.name == "Dinoflagellata",
      "Myzozoa",
      taxa.name
    )
  ) %>% 
  
  # select preference - lt, rimmet, padisak, reynolds and wang (wang last beacuse doesn't use updated padisak ones)
  # odume, lt, gav, no, rimet, kruk, kremer, rimet, padisak, wang
  # order colums in order of preference
  relocate(lt, gavrilko, rimmet, kruk, padisak, wang) %>% 
  mutate(
    functional.group = case_when(
      !is.na(lt) ~ lt,
      !is.na(gavrilko) ~ gavrilko,
      !is.na(rimmet) ~ rimmet,
      !is.na(kruk) ~ kruk,
      !is.na(padisak) ~ padisak,
      !is.na(wang) ~ wang,
      TRUE ~ NA
    ),
    
    fg.source = case_when(
      !is.na(lt) ~ "db-6",
      !is.na(gavrilko) ~ "db-5",
      !is.na(rimmet) ~ "db-1",
      !is.na(kruk) ~ "1313",
      !is.na(padisak) ~ "t-1",
      !is.na(wang) ~ "t-2",
      TRUE ~ NA
    )
  ) %>% 
  
  # sort out ones with multiple values
  # When there is a clear majority use this to be the most representative
  # when it is evenly split select the most representative lake type
  mutate(
    functional.group = case_when(
      taxa.name %in% c("Desmodesmus brasiliensis") ~ "J",
      taxa.name == "Dolichospermum (inconsistent in FamilyI (in SubsectionIV))" ~ "H1",
      taxa.name %in% c("Encyonema minutum") ~ "D",
      taxa.name %in% c("Chlorolobium braunii", "Ochromonas", "Chlamydomonas")~ "X2",
      taxa.name %in% c("Ceratium", "Gymnodinium mitratum", "Peridinium cinctum", "Chroococcus minor", "Gymnodinium", "Microcystis wesenbergii") ~ "LO",
      taxa.name %in% c("Raphidiopsis mediterranea", "Raphidiopsis curvata", "Phormidium", "Lyngbya (genus in domain Bacteria)") ~ "S1",
      taxa.name == "Dendromonas virgaria" ~ "E",
      taxa.name == "Cyclotella comta" ~ "B",
      taxa.name %in% c("Stephanodiscus rotula", "Cyclotella ocellata") ~ "C",
      taxa.name %in% c("Actinotaenium perminutum", "Cosmarium contractum", "Staurodesmus phimus", "Chlorolobium", "Scenedesmus ellipticus", "Chlorella (genus in order Chlorellales)", "Willea rectangularis") ~ "X1",
      taxa.name %in% c("Klebsormidium subtile", "Brebissonia lanceolata", "Ulnaria ulna", "Oscillatoria") ~ "MP",
      taxa.name %in% c("Cyclotella") ~ "A",
      taxa.name %in% c("Cyanodictyon", "Microcystis (genus in domain Bacteria)", "Microcystis flos-aquae", "Microcystis aeruginosa") ~ "M",
      taxa.name %in% c("Synechococcus") ~ "K",
      taxa.name %in% c("Cosmarium", "Cosmarium bioculatum") ~ "N",
      
      
      
      TRUE ~ functional.group
    )
  ) %>% 
  
  select(
    taxa.name, functional.group, fg.source
  )

# save
saveRDS(traits_list_all, "R/data_outputs/database_products/traits_list_all.rds")

# add in new sources
sources_list_nd <- readRDS("R/Data_outputs/database_products/sources_list_nd.rds")

ts <- read_xlsx("raw_data/functional_groups.xlsx", sheet = "sources") %>% 
  mutate(
    year = as.character(year),
    volume = as.character(volume),
    issue = as.character(issue),
    start.page = as.character(start.page),
    end.page = as.character(end.page),
  )

source_list_wt <- bind_rows(sources_list_nd, ts)
  
# save
saveRDS(source_list_wt, "R/data_outputs/database_products/source_list_wt.rds")


# Add FGs to data ----
# import data
bodysize_formatted <- readRDS("R/Data_outputs/database_products/final_products/bodysize_formatted.rds")
traits_list_all <- readRDS("R/Data_outputs/database_products/traits_list_all.rds")

species_fg <- bodysize_formatted %>% 

  # go up in resolution until a match is found
  left_join(traits_list_all, by = "taxa.name") %>% # genus
  left_join(traits_list_all, by = c("family" = "taxa.name"),suffix = c(".s.g", ".family")) %>% # family
  left_join(traits_list_all, by = c("order" = "taxa.name")) %>% # order
  left_join(traits_list_all, by = c("class" = "taxa.name"),suffix = c(".order", ".class")) %>% # class
  left_join(traits_list_all, by = c("phylum" = "taxa.name")) %>% # phylum
  
  # Select lowest res
  # select the trait with the lowest res - might be quicker way to do this but can't work it out
  mutate(
    functional.group.species = case_when(
      !is.na(functional.group.s.g) ~ functional.group.s.g,
      !is.na(functional.group.family) ~ functional.group.family,
      !is.na(functional.group.order) ~ functional.group.order,
      !is.na(functional.group.class) ~ functional.group.class,
      !is.na(functional.group) ~ functional.group,
      TRUE ~ NA
    ),
    fg.source.species = case_when(
      !is.na(fg.source.s.g) ~ fg.source.s.g,
      !is.na(fg.source.family) ~ fg.source.family,
      !is.na(fg.source.order) ~ fg.source.order,
      !is.na(fg.source.class) ~ fg.source.class,
      !is.na(fg.source) ~ fg.source,
      TRUE ~ NA
    )
  ) %>% 
  # remove excess columns
  select(
    !(matches(".s.g|.family|.order|.class|.phylum")),
    - functional.group
  ) 

# Find the most common one for each genera
genus_fg <- species_fg %>% 
  filter(!is.na(functional.group.species)) %>% 
  group_by(genus, functional.group.species) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  group_by(genus) %>% 
  slice_max(count, with_ties = FALSE) %>%
  select(genus, functional.group.genus = functional.group.species)
  
# Add in genus fg and assign taxaonomic groups
bodysize_groups <- species_fg %>% 
  left_join(genus_fg, by = "genus") %>% 
  
  # Assign groupings based on taxonomy
  mutate(
    taxonomic.group = case_when(
      # Phyto:
      # assigned using the groupings in Menden-Deuer & Lessard 2000 to allow for conversions later on
      class == "Bacillariophyceae" ~ "Diatoms",
      class == "Chrysophyceae" ~ "Chrysophytes",
      phylum == "Chlorophyta" ~ "Green",
      phylum == "Dinoflagellata" ~ "Dinoflagellates",
      
      phylum == "Haptophyta" ~ "Haptophytes",
      phylum == "Cyanobacteria" ~ "Blue-green",
      
      #protists
      phylum == "Euglenophyta" ~ "Euglenoids",
      phylum == "Cryptophyta" ~ "Cryptomonads",
      phylum == "Heterokontophyta" ~ "Heterokonts",
      phylum == "Glaucophyta" ~ "Glaucophytes",
      phylum %in% c("Ochrophyta", "Bigyra") ~ "Stramenopiles",
      phylum == "Charophyta" ~ "Charophytes",
      
      # Zoo
      class == "Branchiopoda" ~ "Cladocerans",
      class == "Hexanauplia" ~ "Copepods",
      class == "Ostracoda" ~ "Ostracods",
      phylum == "Ciliophora (phylum in subkingdom SAR)"~ "Ciliates",
      phylum == "Rotifera" ~ "Rotifers",
      
      TRUE ~ NA
      )
    ) %>% 
  
  select(individual.uid, uid, source.code, original.sources, type, taxa.name,
         body.size, bodysize.measurement,
         ott.id, taxonomic.group, functional.group.species, fg.source, functional.group.genus, taxa.name, species, genus, family, order, class, phylum, kingdom,
         habitat, location.code, latitude, longitude, water.body, place, country, continent, area,
         sample.size, sample.year, sample.month, sex, reps, life.stage, units, nu, ind.per.nu)


# save
saveRDS(bodysize_groups, "R/data_outputs/database_products/final_products/bodysize_groups.rds")












