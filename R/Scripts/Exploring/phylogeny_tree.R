# Packages
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)
library(ape)
library(ggnewscale)
library(paletteer)
library(ggthemes)
library(patchwork)

# Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
  BiocManager::install(version = "3.20")

BiocManager::install(c("ggtree","treeio","ggreeExtra"), force = TRUE)
BiocManager::install("ggtreeExtra")
BiocManager::install("treeio", force = TRUE)
BiocManager::install("ggtree", force = TRUE)

library(ggtree)
library(treeio)
library(ggtreeExtra)

# Import data ----
bodysize_traits <- readRDS("R/data_outputs/database_products/final_products/plankton_genus_traits.rds")

tree_pre_plot_p <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_p.nex")
tree_pre_plot_z <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_z.nex")
taxa_in_tree_p <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_p.rds")
taxa_in_tree_z <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_z.rds")

# Square Plot ----
# Inital plot to see it
plot(tree_pre_plot_p, show.tip.label = FALSE)
plot(tree_pre_plot_z, show.tip.label = FALSE)

## Edit tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

### Phyto ----
taxa_in_tree_full_p <- as.data.frame(tree_pre_plot_p$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_pre_plot_p$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE),
    stripped.tip.label = tolower(stripped.tip.label)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_p, by = c("stripped.tip.label" = "taxa.name")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, taxa.name, type, phylum, family, kingdom, class, order, functional.group, taxonomic.group
  ) %>% 
  
  mutate(
    fg_label = paste0("FG: ", functional.group),
    group_label = paste0("Group: ", taxonomic.group)
  )

### Zoo ----
taxa_in_tree_full_z <- as.data.frame(tree_pre_plot_z$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_pre_plot_z$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE),
    stripped.tip.label = tolower(stripped.tip.label)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_z, by = c("stripped.tip.label" = "taxa.name")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, taxa.name, type, phylum, family, kingdom, class, order, functional.group, taxonomic.group
  ) %>% 
  
  mutate(
    fg_label = paste0("FG: ", functional.group),
    group_label = paste0("Group: ", taxonomic.group)
  ) 

# Save
saveRDS(taxa_in_tree_full_p, "R/data_outputs/phylo_tree/taxa_in_tree_full_p.rds")
saveRDS(taxa_in_tree_full_z, "R/data_outputs/phylo_tree/taxa_in_tree_full_z.rds")

# Circular plot ----

## Phyto ----
circular_plot_p <- ggtree(tree_pre_plot_p, branch.length='none', layout='circular')
circular_plot_p

### Group info ----
circular_plot_groups_p <- circular_plot_p %<+% taxa_in_tree_full_p +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA)+
  
  labs(colour = "Traditional groupings") +
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = fg_label), size = 2.5, show.legend = NA) +
  
  labs(colour = "Functional group")

circular_plot_groups_p

# Save
ggsave("R/data_outputs/plots/circular_plot_groups_p.pdf", width = 7, height = 5, limitsize = FALSE)

#### Mass info ----

##### Format data ----
mass_data_p <- bodysize_traits %>% 
  
  filter(
    type == "Phytoplankton"
  ) %>% 
  
  # First calculate mean for each source
  group_by(
    source.code, taxa.name
  ) %>% 
  
  summarise(
    mean.dw = mean(dw.ug),
    .groups = "drop"
  ) %>% 
  
  # get mean mass for each genera - calculating mean of means not weighted mean
  group_by(taxa.name) %>% 
  
  summarise(
    mean.mass = mean(mean.dw),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.mass = log10(mean.mass),
    taxa.name = tolower(taxa.name)
  ) %>% 
  
  left_join(
    taxa_in_tree_full_p, by = "taxa.name"
  ) %>% 
  
  # filter ones with no tip label as these are ones that are not in tree
  filter(
    !is.na(tip.label)
  ) %>% 
  
  select(log.mass, tip.label) %>% 
  
  column_to_rownames(var = "tip.label")

##### Plot ----
circular_plot_mass_p = circular_plot_groups_p+ new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass_p <- gheatmap(circular_plot_mass_p, mass_data_p, offset=3, width=0.2, colnames = F)+
  
  scale_fill_gradient(name = "Mass (µg)", low = "pink", high = "purple") +
  
  ggtitle("a)")+
  
  theme(
    legend.box = "horizontal",
    #legend.text = element_text(size = 20),
    #legend.title = element_text(size = 20),
    #title = element_text(size = 20)
  )

circular_plot_mass_p

ggsave("R/Data_outputs/plots/circular_plot_mass_p.png", width = 9, height = 5, limitsize = FALSE)


### Zoo ----
circular_plot_z <- ggtree(tree_pre_plot_z, branch.length='none', layout='circular') 
circular_plot_z

#### Group info ----
circular_plot_groups_z <- circular_plot_z %<+% taxa_in_tree_full_z +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA)+
  
  labs(colour = "Traditional groupings") +
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = fg_label), size = 2.5, show.legend = NA) +
  
  labs(colour = "Functional group")

circular_plot_groups_z

# Save
ggsave("R/data_outputs/plots/circular_plot_groups_z.pdf", width = 7, height = 5, limitsize = FALSE)

#### Mass info ----
##### Format data ----
mass_data_z <- bodysize_traits %>% 
  
  filter(
    type == "Zooplankton"
  ) %>% 
  
  # get mean mass for each genera
  group_by(taxa.name) %>% 
  
  summarise(
    mean.mass = mean(dw.ug),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.mass = log10(mean.mass),
    taxa.name = tolower(taxa.name)
  ) %>% 
  
  left_join(
    taxa_in_tree_full_z, by = "taxa.name"
  ) %>% 
  
  # filter ones with no tip label as these are ones that are not in tree
  filter(
    !is.na(tip.label)
  ) %>% 
  
  select(log.mass, tip.label) %>% 
  
  column_to_rownames(var = "tip.label")

##### Plot ----
circular_plot_mass_z = circular_plot_groups_z+ new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass_z <- gheatmap(circular_plot_mass_z, mass_data_z, offset=3, width=0.2, colnames = F)+
  
  scale_fill_gradient(name = "Mass (µg)", low = "pink", high = "purple")+
  
  ggtitle("b)")+
  
  theme(
    legend.box = "horizontal",
    #legend.text = element_text(size = 20),
    #legend.title = element_text(size = 20),
    #title = element_text(size = 20)
  )

circular_plot_mass_z

ggsave("R/Data_outputs/plots/circular_plot_mass_z.png", width = 9, height = 5, limitsize = FALSE)


### Join together ----
circular_plot_mass <- circular_plot_mass_p / circular_plot_mass_z
circular_plot_mass

ggsave("R/Data_outputs/plots/circular_plot_mass.png", width = 15, height = 15, limitsize = FALSE)

