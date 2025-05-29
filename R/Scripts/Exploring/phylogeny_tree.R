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
bodysize <- readRDS("R/data_outputs/database_products/final_products/bodysize.rds")

# Format data ----
# Get a taxonomy list to add in in later steps

plot_data <- bodysize %>% 
  
  select(
    ott.id, taxa.name, type, phylum, kingdom, family, order, class, fg, group
  ) %>% 
  
  distinct(taxa.name, .keep_all = TRUE) %>% 
  
  mutate(
    taxa.name = tolower(taxa.name)
  ) 

# Filter for in tree ----

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_ids = plot_data$ott.id)

# Save
saveRDS(in_tree, "R/data_outputs/phylo_tree/in_tree.rds")

# View data
in_tree

# See which ones are in and out
sum(in_tree == TRUE) # 819
sum(in_tree == FALSE) # 152

# Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

## Phyto ----
# get list of just species in the tree split by phyto and zoo
taxa_in_tree_p <- plot_data[in_tree, ] %>% 
  
  left_join(
    taxa, by = c("taxa.name" = "search_string")
  ) %>% 
  
  filter(
    type =="Phytoplankton"
  )

# Save
saveRDS(taxa_in_tree_p, "R/data_outputs/phylo_tree/taxa_in_tree_p.rds")

## Zoo ----
taxa_in_tree_z <- plot_data[in_tree, ] %>% 
  
  left_join(
    taxa, by = c("taxa.name" = "search_string")
  ) %>% 
  
  filter(
    type =="Zooplankton"
  )

# Save
saveRDS(taxa_in_tree_z, "R/data_outputs/phylo_tree/taxa_in_tree_z.rds")

# Make trees
tree_pre_plot_p <- tol_induced_subtree(ott_ids = taxa_in_tree_p$ott_id)
tree_pre_plot_z <- tol_induced_subtree(ott_ids = taxa_in_tree_z$ott_id)

#save tree out here before adapting it for plotting: 
write.nexus(tree_pre_plot_p, file="R/data_outputs/phylo_tree/tree_pre_plot_p.nex")
write.nexus(tree_pre_plot_z, file="R/data_outputs/phylo_tree/tree_pre_plot_z.nex")

tol_about() # gives info about the current synthetic tree
tree_pre_plot_p # shows info about my tree
tree_pre_plot_z
class(tree_pre_plot_p) # check that it is a phylo
class(tree_pre_plot_z) # check that it is a phylo

# Plot trees ----

## Read in data ----
tree_pre_plot_p <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_p.nex")
tree_pre_plot_z <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_z.nex")
taxa_in_tree_p <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_p.rds")
taxa_in_tree_z <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_z.rds")

## Square ----
# Inital plot to see it
plot(tree_pre_plot_p, show.tip.label = FALSE)
plot(tree_pre_plot_z, show.tip.label = FALSE)

## Edit tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

### Phyto ----
new_tips_p <- as.data.frame(tree_pre_plot_p$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_pre_plot_p$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_p, by = c("stripped.tip.label" = "unique_name")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, taxa.name, type, phylum, family, kingdom, class, order, fg, group
  ) %>% 
  
  mutate(
    fg_label = paste0("FG: ", fg),
    group_label = paste0("Group: ", group)
  )

### Zoo ----
new_tips_z <- as.data.frame(tree_pre_plot_z$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_pre_plot_z$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_z, by = c("stripped.tip.label" = "unique_name")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, taxa.name, type, phylum, family, kingdom, class, order, fg, group
  ) %>% 
  
  mutate(
    fg_label = paste0("FG: ", fg),
    group_label = paste0("Group: ", group)
  )

## Circular ----

### Phyto ----
circular_plot_p <- ggtree(tree_pre_plot_p, branch.length='none', layout='circular') 
circular_plot_p

#### Group info ----
circular_plot_groups_p <- circular_plot_p %<+% new_tips_p +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA)+
  
  labs(colour = "Traditional groupings")+
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = fg_label), size = 2.5, show.legend = NA) +
  
  labs(colour = "Functional group")

circular_plot_groups_p

# Save
ggsave("R/data_outputs/plots/circular_plot_groups_p.pdf", width = 7, height = 5, limitsize = FALSE)

#### Mass info ----

##### Format data ----
mass_data_p <- bodysize %>% 
  
  # get mean mass for each genera
  group_by(taxa.name) %>% 
  
  summarise(
    mean.mass = mean(mass),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.mass = log10(mean.mass)
  ) %>% 
  
  left_join(
    new_tips_p, by = "taxa.name"
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
  
  ggtitle("Phytoplankton")+
  
  theme(
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(-0.1, 'cm'),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    title = element_text(size = 50)
  )

circular_plot_mass_p

ggsave("R/Data_outputs/plots/circular_plot_mass_p.png", width = 9, height = 5, limitsize = FALSE)


### Zoo ----
circular_plot_z <- ggtree(tree_pre_plot_z, branch.length='none', layout='circular') 
circular_plot_z

#### Group info ----
circular_plot_groups_z <- circular_plot_z %<+% new_tips_z +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA)+
  
  labs(colour = "Traditional groupings")+
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = fg_label), size = 2.5, show.legend = NA) +
  
  labs(colour = "Functional group")

circular_plot_groups_z

# Save
ggsave("R/data_outputs/plots/circular_plot_groups_z.pdf", width = 7, height = 5, limitsize = FALSE)

#### Mass info ----
##### Format data ----
mass_data_z <- bodysize %>% 
  
  # get mean mass for each genera
  group_by(taxa.name) %>% 
  
  summarise(
    mean.mass = mean(mass),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.mass = log10(mean.mass)
  ) %>% 
  
  left_join(
    new_tips_z, by = "taxa.name"
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
  
  scale_fill_gradient(name = "Mass (µg)", low = "pink", high = "purple") +
  
  ggtitle("Zooplankton")+
  
  theme(
    legend.box = "horizontal",
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(-0.1, 'cm'),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    title = element_text(size = 50)
  )

circular_plot_mass_z

ggsave("R/Data_outputs/plots/circular_plot_mass_z.png", width = 9, height = 5, limitsize = FALSE)


### Join together ----
circular_plot_mass <- circular_plot_mass_p + circular_plot_mass_z
circular_plot_mass

ggsave("R/Data_outputs/plots/circular_plot_mass.png", width = 30, height = 20, limitsize = FALSE)
