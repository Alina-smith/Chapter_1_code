## Standadizing the WOS data


# Data ---- 
wos_raw_body <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "bodysize", guess_max = 40000)
wos_source_list <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "source_list")

# Standardising ----
wos_formatted <- wos_raw_body %>%
  ## Filter ----
  filter(
    !(original.taxa.name == "unknown"), # remove "unknown" taxa names
  ) %>% 
  
  ## Select columns ----
  select(
    - experimental.design,
    - treatment.1.name,
    - treatment.1.value,
    - treatment.2.name,
    - treatment.2.value,
  ) %>% 
  
  mutate(
    ## Body.size ----
    # body.size
    body.size = stri_replace_all_regex(body.size, ",", "."), # replace , with .
    body.size = case_when(
      body.size %in% c("NA", "na", "-", "0") ~ NA, # make NA
      individual.uid == "10.1002/etc.5034-2" & body.size == "0.000.16" ~ "0.00016", # change one with two decimal places to one
      TRUE ~ body.size
    ),
    
    # min.body.size
    min.body.size = if_else(
      min.body.size == "NA",
      NA, # make NA
      min.body.size
    ),
    
    # max.body.size
    max.body.size = if_else(
      max.body.size == "NA",
      NA, # make NA
      max.body.size
    ),
    
    # calculating values when average isn't given
    # make the same type
    total.bs = as.numeric(total.bs),
    abundance = as.numeric(abundance),
    body.size = as.numeric(body.size),
    min.body.size = as.numeric(min.body.size),
    max.body.size = as.numeric(max.body.size),
    
    body.size = case_when(
      # divide total population biovolume by abundance
      !is.na(total.bs) ~ total.bs/abundance,
      # take average of range values when the average isn't given already
      measurement.type == "range" & is.na(body.size) ~ (min.body.size*max.body.size)/2,
      # select min values
      measurement.type == "min" & is.na(body.size) ~ min.body.size,
      # select max values
      measurement.type == "max" & is.na(body.size) ~ max.body.size,
      # keep the rest the same
      TRUE ~ body.size
    )
  ) %>% 
  
  # filter out body sizes with NA only for all min,max and body.size
  mutate(
    keep = if_else(
      is.na(body.size) & is.na(min.body.size) & is.na(max.body.size),
      "NA",
      "keep"
    )
  ) %>% 
  # keep onlt ones with "keep"
  filter(
    keep == "keep"
  ) %>% 
  # remove redundant columns
  select(
    - keep,
    - total.bs,
    - abundance
  ) %>% 
  
  mutate(
    ## body size measurements ----
    # seperate out the body size measurement method into the method and then a column with additional notes on the method like what is measured etc
    bodysize.measurement.notes = stri_extract_first_regex(bodysize.measurement, "(?<=\\- ).*"),
    bodysize.measurement = stri_replace_all_regex(bodysize.measurement, " \\- .*", ""),
    
    # fix spelling mistakes
    bodysize.measurement = stri_replace_all_regex(bodysize.measurement, "B", "b",),
    
    # make into the same
    bodysize.measurement = case_when(
      stri_detect_regex(bodysize.measurement, "length") ~ "length",
      stri_detect_regex(bodysize.measurement, "width") ~ "width",
      stri_detect_regex(bodysize.measurement, "diameter") ~ "diameter",
      stri_detect_regex(bodysize.measurement, "depth") ~ "depth",
      stri_detect_regex(bodysize.measurement, "height") ~ "height",
      TRUE ~ bodysize.measurement
    ),
    
    ## units ----
    # change any mistakes or synonyms to the same
    units = case_when(
      units == "μg ind^-1" ~ "μg",
      units %in% c("fl/cell", "fl cell^-1") ~ "μm^3", # one femtoliter is the same as one micrometer cubed
      source.code == "16" ~ "mm", # the units were written wrong in the supplementary data, changed to units used in main paper
      TRUE ~ units
    ),
    
    # standardize to the same units
    body.size = case_when(
      units == "mg" ~ body.size*1000,
      units == "cm" ~ body.size*10,
      units == "nm" ~ body.size/1000,
      TRUE ~ body.size
    ),
    
    units = case_when(
      units == "mg" ~ "μg",
      units == "cm" ~ "mm",
      units == "nm" ~ "μm",
      TRUE ~ units
    ),
    
    ## life stage ----
    # sort out captals
    life.stage = stri_replace_all_regex(life.stage, "A", "a"),
    
    # change to either adult or juvenile
    life.stage = case_when(
      # adults
      stri_detect_fixed(life.stage, "adult") ~ "adult",
      source.code %in% c("61", "263") & stri_detect_regex(life.stage, "C") ~ "adult",
      life.stage == "7 days" ~ "adult",
      is.na(life.stage) ~ "adult", # assume they are adults unless specified otherwise
      # juvenile
      stri_detect_regex(life.stage, "Copepodite|neonate|copepodid|juvenile|metamorphasis") ~ "juvenile",
      source.code %in% c("61", "263") & stri_detect_regex(life.stage, "N|Instar") ~ "juvenile",
      source.code == "5" & life.stage != "adult" ~ "juvenile",
      source.code == "66" ~ "juvenile",
      
      TRUE ~ life.stage
    ),
    
    ## Sample dates ----
    # make the same type
    sample.start.year = as.character(sample.start.year),
    sample.end.year = as.character(sample.end.year),
    
    # Year
    sample.start.year = case_when(
      !is.na(sample.start.date.full) ~ stri_extract_first_regex(sample.start.date.full, "\\d{4}"),
      TRUE ~ sample.start.year
    ),
    
    sample.end.year = case_when(
      !is.na(sample.end.date.full) ~ stri_extract_first_regex(sample.end.date.full, "\\d{4}"),
      TRUE ~ sample.end.year
    ),
    
    # Month
    sample.start.month = case_when(
      # american style date
      !is.na(sample.start.date.full) & source.code == "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<!\\/)\\d+(?=\\/)"), # take the two digits that don't have a / infront but have on following it - dd/
      
      # non-american style dates
      stri_detect_regex(sample.start.date.full, "\\/") & source.code != "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\/)\\d+(?=\\/)"), # take the two digits inbetween an / - /dd/
      stri_detect_regex(sample.start.date.full, "\\.") & source.code != "8" ~ stri_extract_first_regex(sample.start.date.full, "(?<=\\.)\\d+(?=\\.)"), # take the two digits inbetween an . - .dd.
      TRUE ~ sample.start.month
    ),
    
    sample.end.month = case_when(
      # american style date
      !is.na(sample.end.date.full) & source.code == "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<!\\/)\\d+(?=\\/)"), # take the two digits that don't have a / infront but have on following it - dd/
      
      # non-american style dates
      stri_detect_regex(sample.end.date.full, "\\/") & source.code != "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\/)\\d+(?=\\/)"), # take the two digits inbetween an / - /dd/
      stri_detect_regex(sample.end.date.full, "\\.") & source.code != "8" ~ stri_extract_first_regex(sample.end.date.full, "(?<=\\.)\\d+(?=\\.)"), # take the two digits inbetween an . - .dd.
      TRUE ~ sample.end.month
    ),
    
    # Make it into ranges in one column
    # year
    sample.year = case_when(
      is.na(sample.end.year) ~ sample.start.year, # when there is only one sample year use that one
      sample.start.year == sample.end.year ~ sample.start.year, # when the start and end year are the same just keep one
      sample.start.year != sample.end.year ~ paste(sample.start.year, sample.end.year, sep = "-"), # when it is over multiple years make a range
      TRUE ~ NA),
    
    # month
    sample.month = case_when(
      is.na(sample.end.month) ~ sample.start.month, # when there is only one sample year use that one
      sample.start.month == sample.end.month ~ sample.start.month, # when the start and end year are the same just keep one
      sample.start.month != sample.end.month ~ paste(sample.start.month, sample.end.month, sep = "-"), # when it is over multiple years make a range
      TRUE ~ NA),
  ) %>% 
  
  # remove redundant columns
  select(
    -sample.start.date.full,
    -sample.end.date.full,
    -sample.start.month,
    -sample.start.year,
    -sample.end.month,
    -sample.end.year
  ) %>% 
  
  mutate(
    ## Form ----
    form = if_else(
      form == "coenobium",
      "colony",
      form
    ),
    
    ## Form no ----
    form.no = case_when(
      !is.na(form.no) ~ form.no,
      form == "individual" ~ 1,
      TRUE ~ NA
    ),
    
    ## Unknown
    # change unknown to NA is reps and sample.size
    sample.size = if_else(
      sample.size == "unknown",
      NA,
      sample.size
    ),
    
    reps = if_else(
      reps == "unknown",
      NA,
      reps
  ), 
  
  ## original.source.codes: ----
  # change types to merge with db
    source.code = as.character(source.code),
    original.source.code.1 = as.character(original.source.code.1),
    original.source.code.2 = as.character(original.source.code.2),
    original.source.code.3 = as.character(original.source.code.3),
    original.source.code.4 = as.character(original.source.code.4),
    original.source.code.5 = as.character(original.source.code.5),
    original.source.code.6 = as.character(original.source.code.6)
  ) %>% 
  
  # reorder
  relocate(source.code, original.source.code.1, original.source.code.2, original.source.code.3, original.source.code.4, original.source.code.5, original.source.code.6,
           sample.year, sample.month,
           join.location.1, join.location.2, join.location.3, join.location.4, join.location.5, join.location.6, join.location.7, join.location.8, join.location.9, join.location.10,
           join.location.11, join.location.12, join.location.13, join.location.14, join.location.15, join.location.16, join.location.17,
           individual.uid, original.taxa.name, life.stage, sex, form, form.no,
           min.body.size, max.body.size, body.size,
           bodysize.measurement, bodysize.measurement.notes, units, measurement.type, sample.size, reps, error, error.type)

# Save
saveRDS(wos_formatted, file = "R/Data_outputs/databases/wos_formatted.rds")
