## locations

# have made a sheet with all the locations with their join location on it so that has multiples of the same locations
# need to join location codes using the sheet above - want to merge source code with join code to make it easier as there are duplicates of join.locations
# once location codes are joined then make final location list by getting distinct loc.code

locations_raw <- read_xlsx(here("Raw_data","location_data_for_wos_db.xlsx"), sheet = "location_raw")

distinct_locations <- locations_raw %>% 
  distinct(
    continent
  )
