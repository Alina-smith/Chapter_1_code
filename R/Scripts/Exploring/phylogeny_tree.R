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

# Genus ----

## Import data ----
phyto_subset <- readRDS("R/data_outputs/database_products/final_products/phyto_subset.rds")


## Format data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data_genus <- phyto_subset %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    taxa.name, phylum, kingdom, family, order, class, r.group, group
  ) %>% 
  
  distinct(taxa.name, .keep_all = TRUE) %>% 
  
  mutate(
    taxa.name = tolower(taxa.name)
  )

## Find only ones in the tree ----
# Need to run through tnrs match again because there will be some that i don't have an ott_id for the genus
taxa_genus <- tnrs_match_names(phylo_plot_data_genus$taxa.name)

# Save
saveRDS(taxa_genus, "R/data_outputs/phylo_tree/taxa_genus.rds")

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_genus <- is_in_tree(ott_ids = ott_id(taxa_genus))

# Save
saveRDS(in_tree_genus, "R/data_outputs/phylo_tree/in_tree_genus.rds")

# View data
in_tree_genus

# See which ones are in and out
sum(in_tree_genus == TRUE) # 655
sum(in_tree_genus == FALSE) # 154

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree_genus <- phylo_plot_data_genus[in_tree_genus, ] %>% 
  
  left_join(
    taxa_genus, by = c("taxa.name" = "search_string")
  )

# Save
saveRDS(taxa_in_tree_genus, "R/data_outputs/phylo_tree/taxa_in_tree_genus.rds")

# Make tree
tree_genus <- tol_induced_subtree(ott_ids = taxa_in_tree_genus$ott_id)

#save tree out here before adapting it for plotting: 
write.nexus(tree_genus, file="R/data_outputs/phylo_tree/tree_pre_plot_genus.nex")

tol_about() # gives info about the current synthetic tree
tree_genus # shows info about my tree
class(tree_genus) # check that it is a phylo

## Plot square tree ----

# Read in data
tree_genus <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_genus.nex")
taxa_in_tree_genus <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_genus.rds")

plot(tree_genus, show.tip.label = FALSE)

## Format data with tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

phylo_plot_data_update_genus <- as.data.frame(tree_genus$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_genus$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_genus, by = c("stripped.tip.label" = "unique_name")
  ) %>% 
  
  # Add in to make plotting labels look neater
  mutate(
    taxa.name = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, taxa.name, phylum, family, kingdom, class, order, r.group, group
  ) %>% 
  
  mutate(
    r_label = paste0("R.group: ", r.group),
    group_label = paste0("Group: ", group)
  )

## Plotting circular tree ----

circular_plot_genus <- ggtree(tree_genus, branch.length='none', layout='circular') 
circular_plot_genus 

## Plotting circular tree with group info ----

## Plotting circular tree with group info ----
group_colors_genus <- paletteer_d("colorBlindness::Blue2Green14Steps", (length(unique(phylo_plot_data_update_genus$group_label))))

# Create plot
circular_plot_groups_genus <- circular_plot_genus %<+% phylo_plot_data_update_genus +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA) +
  scale_color_manual(values = group_colors_genus, name = "Group Label") +
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = r_label), size = 2.5, show.legend = NA) 

circular_plot_groups_genus

# Save
ggsave("R/data_outputs/phylo_tree/circular_plot_groups_genus.pdf", width = 7, height = 5, limitsize = FALSE)


## adding in mass info ----

phylo_plot_data_mass_genus <- phyto_subset %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  # get mean mass for each species
  group_by(taxa.name) %>% 
  
  summarise(
    mean.biovol = mean(biovolume),
    mean.mass.c = mean(mass.c),
    mean.mass.d = mean(mass.d),
    .groups = "drop"
  ) 


# format mass info
log_mass_genus <- phylo_plot_data_update_genus %>% 
  
  left_join(phylo_plot_data_mass_genus, by = "taxa.name") %>% 
  
  select(mean.mass.d, tip.label) %>% 
  
  mutate(
    log.mass = log10(mean.mass.d)
  ) %>% 
  
  select(
    - mean.mass.d
  ) %>% 
  distinct(tip.label, .keep_all = TRUE) %>% 
  column_to_rownames(var = "tip.label")

# plot
circular_plot_mass_genus = circular_plot_groups_genus+ new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass_genus <- gheatmap(circular_plot_mass_genus, log_mass_genus, offset=3, width=0.2, colnames = F)+
  
  theme(
    legend.box = "horizontal",
    legend.key.size = unit(0.1, "cm"),
    legend.spacing.x = unit(-0.3, 'cm'),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4),
  )

circular_plot_mass_genus

ggsave("R/Data_outputs/plots/circular_plot_mass_genus.png", width = 9, height = 5, limitsize = FALSE)






# Phyto + zoo ----

## Import data ----
bodysize <- readRDS("R/data_outputs/database_products/final_products/bodysize.rds")

## Format data ----
# Get a taxonomy list to add in in later steps

pz_plot_data <- bodysize %>% 
  
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
taxa_pz <- tnrs_match_names(pz_plot_data$taxa.name)

# Save
saveRDS(taxa_pz, "R/data_outputs/phylo_tree/taxa_pz.rds")

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_pz <- is_in_tree(ott_ids = ott_id(taxa_pz))

# Save
saveRDS(in_tree_pz, "R/data_outputs/phylo_tree/in_tree_pz.rds")

# View data
in_tree_pz

# See which ones are in and out
sum(in_tree_pz == TRUE) # 720
sum(in_tree_pz == FALSE) # 160

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree_pz <- pz_plot_data[in_tree_pz, ] %>% 
  
  left_join(
    taxa_pz, by = c("taxa.name" = "search_string")
  )

# Save
saveRDS(taxa_in_tree_pz, "R/data_outputs/phylo_tree/taxa_in_tree_pz.rds")

# Make tree
tree_pz <- tol_induced_subtree(ott_ids = taxa_in_tree_pz$ott_id)

#save tree out here before adapting it for plotting: 
write.nexus(tree_pz, file="R/data_outputs/phylo_tree/tree_pre_plot_pz.nex")

tol_about() # gives info about the current synthetic tree
tree_pz # shows info about my tree
class(tree_pz) # check that it is a phylo

## Plot square tree ----

# Read in data
tree_pz <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_pz.nex")
taxa_in_tree_pz <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_pz.rds")

plot(tree_pz, show.tip.label = FALSE)

## Format data with tip.labels ----
# Need to add in the tip labels to the data for plotting with group info

phylo_plot_data_update_pz <- as.data.frame(tree_pz$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_pz$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    stripped.tip.label = strip_ott_ids(tip.label, remove_underscores = TRUE)
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_pz, by = c("stripped.tip.label" = "unique_name")
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

circular_plot_pz <- ggtree(tree_pz, branch.length='none', layout='circular') 
circular_plot_pz

## Plotting circular tree with group info ----
group_colors_genus <- paletteer_d("colorBlindness::Blue2Green14Steps", (length(unique(phylo_plot_data_update_pz$group_label))))

# Create plot
circular_plot_groups_pz <- circular_plot_pz %<+% phylo_plot_data_update_pz +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA) +
  scale_color_manual(values = group_colors_genus, name = "Group Label") +
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = fg_label), size = 2.5, show.legend = NA) 

circular_plot_groups_pz

# Save
ggsave("R/data_outputs/phylo_tree/circular_plot_groups_genus.pdf", width = 7, height = 5, limitsize = FALSE)


## adding in mass info ----

phylo_plot_data_mass_pz <- bodysize %>% 
  
  filter(
    nu == "individual",
    #taxa.name %in% taxa_in_tree_pz$taxa.name
  ) %>% 
  
  mutate(
    mass.all = if_else(
      !is.na(mass),
      mass,
      mass.d
    )
  ) %>% 
  
  # get mean mass for each genera
  group_by(taxa.name) %>% 
  
  summarise(
    mean.mass = mean(mass.all),
    .groups = "drop"
  ) %>% 
  
  mutate(
    log.mass = log10(mean.mass)
  ) %>% 
  
  left_join(
    phylo_plot_data_update_pz, by = "taxa.name"
  ) %>% 
  
  # filter ones with no tip label as these are ones that are not in tree
  filter(
    !is.na(tip.label)
  ) %>% 
  
  select(log.mass, tip.label) %>% 
  distinct(tip.label, .keep_all = TRUE) %>% 

  column_to_rownames(var = "tip.label")

# plot
circular_plot_mass_pz = circular_plot_groups_pz+ new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass_pz <- gheatmap(circular_plot_mass_pz, phylo_plot_data_mass_pz, offset=3, width=0.2, colnames = F)+
  
  theme(
    legend.box = "horizontal",
    legend.key.size = unit(0.1, "cm"),
    legend.spacing.x = unit(-0.3, 'cm'),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4),
  )

circular_plot_mass_pz

ggsave("R/Data_outputs/plots/circular_plot_mass_genus.png", width = 9, height = 5, limitsize = FALSE)

























