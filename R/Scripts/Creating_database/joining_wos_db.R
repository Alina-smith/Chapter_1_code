## Combining DB and WOS

# Aim of this script
  # Combine WOS and DB dataframes into one
  # cross reference sources and remove any duplicates with the same taxa.name
  # add in location info

# Data ----
wos_raw <- readRDS("R/Data_outputs/databases/wos_formatted.rds")
db_raw <- readRDS("R/Data_outputs/databases/db_formatted.rds")

all_raw <- bind_rows(db_raw, wos_raw)

