## Join the DB and WOS data together and format anything to make the same

# Data ----
wos_raw <- readRDS("R/Data_outputs/databases/wos_formatted.rds")
db_raw <- readRDS("R/Data_outputs/databases/db_formatted.rds")

# Joining db and wos ----
# Make a dataframe of all the db and wos data - will filter this for duplicate sources and then add location data to
# There are a couple of data points where the original sources couldn't be found - when this is the case the original.source.1 will be NA

bodysize_joined <- bind_rows(db_raw, wos_raw) %>% 
  
  mutate(
    ## Make uid column ----
    # to give each data.point a uid aswell as each individual a uid
    uid = row_number(),
    
    # set original.taxa.name to lower case to make later steps with taxonomy easier
    original.taxa.name = tolower(original.taxa.name),
    
    # Remove any invisible chacters from the original.taxa.name - replace with an easily identifiable sequence (*SpecChar* stands for special character) to help when doing taxonomy next (don't want to do a space or remove because that could change it to a different name)
    original.taxa.name = stri_replace_all_regex(original.taxa.name, "[^\\x20-\\x7E]", "*SpecChar*")
  ) %>% 
  
  ## reorder - make easier for mutating across
  relocate(uid, source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6, original.source.code.7, original.source.code.8, original.source.code.9, original.source.code.10, original.source.code.11, original.source.code.12, original.source.code.13, original.source.code.14, original.source.code.15, original.source.code.16, original.source.code.17, original.source.code.18,
           join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
           join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type) %>% 
    
  mutate(
    ## Change NA - original.source.codes ----
    # set NA original.source.code.1 to "unknown"
    original.source.code.1 = if_else(
      is.na(original.source.code.1),
      "unknown",
      original.source.code.1
    ),
    
    # set Na in the original.source.columns to no.source
    across(
      .cols = original.source.code.2:original.source.code.18,
      ~ if_else(
        is.na(.),
        "no.source",
        .
        )
      ),
    
    ## Change NA - join.source ----
    # set join.source to no.source for NAs - this isn't for if there isn't a source for the location it means there isn't that many locations fir this source so it would be an NA for this column but changing it to a NA code instead to avoid confusion what left joining later as some of the join,location codes are "NA" for a country
    across(
      .cols = join.location.2:join.location.17, # all join.location.1 column should have a value as the unknonws have their own data points
      ~ if_else(
        is.na(.),
        "no.source",
        .)
      )
  )

saveRDS(bodysize_joined, file = "R/Data_outputs/databases/bodysize_joined.rds")


