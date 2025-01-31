# make final sources and tax list now all the ones i dont want have been taken out

# for location check for ones that aren't present in the the main data anymore and remove

# Check for unused ----
# check for any locations that are in the location_raw sheet but not used in the main data and if so remove them
location_check <- bodysize_location %>% 
  
  # Select columns
  select(
    location.code.1:location.code.17
  ) %>% 
  
  # Pivot to get location.codes on seperate lines 
  pivot_longer(., cols = 1:17, values_to = "location.code")%>% 
  
  filter(
    !is.na(location.code)
  ) %>% 
  
  # get distinct ones
  distinct(location.code) %>% 
  
  # anti_join to leave any that are in locations raw but not bodysize_location
  anti_join(location_raw, ., by = "location.code")

# Final location list ----
# get final sheet by getting distinct location.codes - might need to be changed after later steps incase they remove any more data points
location_list <- location_raw %>% 
  
  filter(
    !(location.code %in% c("loc-121", "loc-135", "loc-"))
  )

distinct(location.code, .keep_all = TRUE) %>% 
  select(
    - join.location,
    - source.code
  )

# save - list of all distinct locations used in the data
saveRDS(location_list, file = "R/Data_outputs/full_database/location_list.rds")

