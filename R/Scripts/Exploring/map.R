## map ----
map_plot <- bodysize_data %>% 
  filter(
    form == "individual",
    !is.na(mass),
    !is.na(genus)
  )

world <- ne_countries(scale = "small", returnclass = "sf")
ggplot(world)+
  geom_sf(fill = "grey90", colour = "black")+
  coord_sf(ylim = c(-50, 80), xlim = c(-135, 180), expand = FALSE) +
  geom_point(data = map_plot, aes(x = longitude, y = latitude, colour = "red"))













