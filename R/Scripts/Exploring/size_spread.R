# body size spread

bodysize_spread <- bodysize_data %>% 
  # select ones with data
  filter(
    form == "individual",
    !is.na(mass),
    !is.na(genus),
    life.stage %in% c("adult", "active"),
    bodysize.measurement != "wet mass",
    !(stri_detect_regex(individual.uid, "10.1111/geb.13575")),
    !(stri_detect_regex(individual.uid, "10.1093/plankt/fbae017"))
  ) %>% 
  
  # get mean mass for each taxa
  group_by(tax.uid) %>% 
  
  dplyr::summarise(
    mean.mass = mean(mass)
  ) %>% 
  
  ungroup() %>% 
  
  # add in taxonomy info
  left_join(
    select(
      bodysize_data, tax.uid, group
    ),
    by = "tax.uid"
  )

x <- bodysize_data %>% 
  filter(
    group == "zooplankton",
    !is.na(mass),
    bodysize.measurement != "wet mass"
  )



ggplot(bodysize_spread, aes(x = log(mean.mass), fill = group)) +
geom_histogram(alpha = 0.7, binwidth = 1)+
  facet_wrap(~group, ncol = 1, scales = "free_y")

+
  facet_wrap(~group)

zoo <- ggplot(filter(phylogeny_plot,group == "zooplankton"), aes(x = log(mean.mass), fill = group)) +
  geom_histogram()

phyto+zoo


