## Calculating the body size for ones I don't have it for

# Data ----
bodysize <- readRDS("R/Data_outputs/databases/bodysize_joined.rds")
x <- bodysize %>% 
  distinct(individual.uid, .keep_all = TRUE)
bodysize_calculated <- bodysize %>% 
  
  # calculate the average fo multiple measurements of the same individual
  group_by(
    individual.uid, bodysize.measurement
  ) %>% 
  
  summarise(
    mean = mean(body.size)
  ) %>% 
  
  left_join(
    x,
    by = "individual.uid"
  )

