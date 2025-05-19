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
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    taxa.name, type, phylum, kingdom, family, order, class, fg, group
  ) %>% 
  
  distinct(taxa.name, .keep_all = TRUE) %>% 
  
  mutate(
    taxa.name = tolower(taxa.name)
  )

## Find only ones in the tree ----
# Need to run through tnrs match again because there will be some that i don't have an ott_id for the genus
taxa <- tnrs_match_names(plot_data$taxa.name)

# Save
saveRDS(taxa, "R/data_outputs/phylo_tree/taxa.rds")

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_ids = ott_id(taxa))

# Save
saveRDS(in_tree, "R/data_outputs/phylo_tree/in_tree.rds")

# View data
in_tree

# See which ones are in and out
sum(in_tree == TRUE) # 769
sum(in_tree == FALSE) # 155

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree <- plot_data[in_tree, ] %>% 
  
  left_join(
    taxa, by = c("taxa.name" = "search_string")
  )

# Save
saveRDS(taxa_in_tree, "R/data_outputs/phylo_tree/taxa_in_tree.rds")

# Make tree
tree <- tol_induced_subtree(ott_ids = taxa_in_tree$ott_id)

#save tree out here before adapting it for plotting: 
write.nexus(tree, file="R/data_outputs/phylo_tree/tree_pre_plot.nex")

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

## Plot square tree ----

# Read in data
tree <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot.nex")
taxa_in_tree <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree.rds")

plot(tree, show.tip.label = FALSE)

## Format data with tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

phylo_plot_data_update <- as.data.frame(tree$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree, by = c("stripped.tip.label" = "unique_name")
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

## Plotting circular tree ----

circular_plot <- ggtree(tree, branch.length='none', layout='circular') 
circular_plot

# Create plot
circular_plot_groups <- circular_plot %<+% phylo_plot_data_update +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA)+
  
  labs(colour = "Traditional groupings")+
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = fg_label), size = 2.5, show.legend = NA) +
  
  labs(colour = "Functional group")

circular_plot_groups

# Save
ggsave("R/data_outputs/phylo_tree/circular_plot_groups_genus.pdf", width = 7, height = 5, limitsize = FALSE)


## adding in mass info ----

phylo_plot_data_mass <- bodysize %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  # get mean mass for each genera
  group_by(taxa.name) %>% 
  
  summarise(
    mean.mass = mean(mass.all.d),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.mass = log10(mean.mass)
  ) %>% 
  
  left_join(
    phylo_plot_data_update, by = "taxa.name"
  ) %>% 
  
  # filter ones with no tip label as these are ones that are not in tree
  filter(
    !is.na(tip.label)
  ) %>% 
  
  select(log.mass, tip.label) %>% 
  distinct(tip.label, .keep_all = TRUE) %>% 

  column_to_rownames(var = "tip.label")

# plot
circular_plot_mass = circular_plot_groups+ new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass <- gheatmap(circular_plot_mass, phylo_plot_data_mass, offset=3, width=0.2, colnames = F)+
  
  scale_fill_gradient(name = "Mass (µg)", low = "pink", high = "purple") +
  
  theme(
    legend.box = "horizontal",
    legend.key.size = unit(0.1, "cm"),
    legend.spacing.x = unit(-0.3, 'cm'),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4),
  )

circular_plot_mass

ggsave("R/Data_outputs/plots/circular_plot_mass.png", width = 9, height = 5, limitsize = FALSE)

























