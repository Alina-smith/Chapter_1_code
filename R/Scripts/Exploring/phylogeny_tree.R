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
phyto_subset <- readRDS("R/data_outputs/database_products/final_products/phyto_subset.rds")

# Genus ----
## Format data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data_genus <- phyto_subset %>% 
  
  mutate(
    # when the r.group for the species is missing that use the next highest one
    r.group = case_when(
      !is.na(r.group.genus) ~ r.group.genus,
      is.na(r.group.genus) & !is.na(r.group.family) ~ r.group.family,
      is.na(r.group.genus) & is.na(r.group.family) & !is.na(r.group.order) ~ r.group.order,
      is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & !is.na(r.group.class) ~ r.group.class,
      is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & is.na(r.group.class) & !is.na(r.group.phylum) ~ r.group.phylum,
      
      TRUE ~ "unasigned"
    )
  ) %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    species, genus, phylum, kingdom, family, order, class, r.group, group
  ) %>% 
  
  distinct(genus, .keep_all = TRUE) %>% 
  
  mutate(
    genus = tolower(genus)
  )

## Find only ones in the tree ----
# Need to run through tnrs match again because there will be some that i don't have an ott_id for the genus
taxa_genus <- tnrs_match_names(phylo_plot_data_genus$genus)

# Save
saveRDS(taxa_genus, "R/data_outputs/phylo_tree/taxa_genus.rds")

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_genus <- is_in_tree(ott_ids = ott_id(taxa_genus))

# Save
saveRDS(in_tree_genus, "R/data_outputs/phylo_tree/in_tree_genus.rds")

# View data
in_tree_genus

# See which ones are in and out
sum(in_tree_genus == TRUE) # 656
sum(in_tree_genus == FALSE) # 154

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree_genus <- phylo_plot_data_genus[in_tree_genus, ] %>% 
  
  left_join(
    taxa_genus, by = c("genus" = "search_string")
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
    genus = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, genus, phylum, family, kingdom, class, order, r.group, group
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
r_label_colors_genus <- paletteer_c("grDevices::RdYlGn", length(unique(phylo_plot_data_update_genus$r_label)))

# Create plot
circular_plot_groups_genus <- circular_plot_genus %<+% phylo_plot_data_update_genus +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 2.5, show.legend = NA) +
  scale_color_manual(values = group_colors_genus, name = "Group Label") +
  
  ggnewscale::new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = r_label), size = 2.5, show.legend = NA) +
  scale_colour_manual(values = r_label_colors_genus, name = "R Label")

circular_plot_groups_genus

# Save
ggsave("R/data_outputs/phylo_tree/circular_plot_groups_genus.pdf", width = 7, height = 5, limitsize = FALSE)


## adding in mass info ----

phylo_plot_data_mass_genus <- phyto_subset %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  # get mean mass for each species
  group_by(genus) %>% 
  
  summarise(
    mean.biovol = mean(biovolume),
    mean.mass.c = mean(mass.c),
    mean.mass.d = mean(mass.d),
    .groups = "drop"
  ) 


# format mass info
log_mass_genus <- phylo_plot_data_update_genus %>% 
  
  left_join(phylo_plot_data_mass_genus, by = "genus") %>% 
  
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






# Species ----
## Format data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data_species <- phyto_subset %>% 
  
  # get just species level ones
  filter(
    !is.na(species),
    nu == "individual",
    !(stri_detect_regex(taxa.name, "var\\."))
  ) %>% 
  
  mutate(
    # when the r.group for the species is missing that use the next highest one
    r.group = case_when(
      !is.na(r.group.species) ~ r.group.species,
      is.na(r.group.species) & !is.na(r.group.genus) ~ r.group.genus,
      is.na(r.group.species) & is.na(r.group.genus) & !is.na(r.group.family) ~ r.group.family,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & !is.na(r.group.order) ~ r.group.order,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & !is.na(r.group.class) ~ r.group.class,
      is.na(r.group.species) & is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & is.na(r.group.class) & !is.na(r.group.phylum) ~ r.group.phylum,
      
      TRUE ~ "unasigned"
    )
  ) %>% 
  
  select(
    ott.id, species, genus, phylum, kingdom, family, order, class, r.group, group
  ) %>% 
  
  distinct(species, .keep_all = TRUE) 


## Find only ones in the tree ----

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
# already have the ott.ids for this so can skip the tnrs_match_names step

in_tree_species <- is_in_tree(ott_ids = phylo_plot_data_species$ott.id)

# Save
saveRDS(in_tree_species, "R/data_outputs/phylo_tree/in_tree_species.rds")

# View data
in_tree_species

# See which ones are in and out
sum(in_tree_species == TRUE) # 3064
sum(in_tree_species == FALSE) # 515

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree_species <- phylo_plot_data_species[in_tree_species, ]

# Save
saveRDS(taxa_in_tree_species, "R/data_outputs/phylo_tree/taxa_in_tree_species.rds")

# Make tree
tree_species <- tol_induced_subtree(ott_ids = taxa_in_tree_species$ott.id)

#save tree out here before adapting it for plotting: 
write.nexus(tree_species, file="R/data_outputs/phylo_tree/tree_pre_plot_species.nex")

tol_about() # gives info about the current synthetic tree
tree_species # shows info about my tree
class(tree_species) # check that it is a phylo

## Plot square tree ----

# Read in data
tree_species <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_species.nex")
taxa_in_tree_species <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_species.rds")

plot(tree_species, show.tip.label = FALSE)

## Format data with tip.labels ----
# Need to add in the tip labels to the data for plotting with group info ott463356

phylo_plot_data_update_species <- as.data.frame(tree_species$tip.label) %>% # get the tip labels in the tree
  
  rename(
    tip.label = `tree_species$tip.label`
  ) %>% 
  
  mutate(
    # Make a new column with the tip labels stripped so just the name and no other info
    species = strip_ott_ids(tip.label, remove_underscores = TRUE),
    
    species = case_when(
      # remove the brackets as well
      stri_detect_regex(species, "\\(") ~ stri_extract_first_regex(species, "\\w+ \\w+(?= \\()"),
      # edit [Chlorella] fusca because it went weird
      tip.label == "_fusca_ott463356" ~ "[Chlorella] fusca",
      TRUE ~ species
    )
  ) %>% 
  
  # Join in the extra data
  left_join(
    taxa_in_tree_species, by = "species"
  ) %>% 
  
  select(
    tip.label, species, genus, phylum, family, kingdom, class, order, r.group, group
  ) %>% 
  
  mutate(
    group_label = paste0("Group: ", group),
    r_label = paste0("R.group: ", r.group)
  )

## Plotting circular tree ----

circular_plot_species <- ggtree(tree_species, branch.length='none', layout='circular') 
circular_plot_species


## Plotting circular tree with group info ----
group_colors <- paletteer_d("colorBlindness::Blue2Green14Steps", (length(unique(phylo_plot_data_update_species$group_label))))
r_label_colors <- paletteer_c("grDevices::RdYlGn", length(unique(phylo_plot_data_update_species$r_label)))

# Create plot
circular_plot_groups_species <- circular_plot_species %<+% phylo_plot_data_update_species +
  geom_tippoint(aes(x = x + 1, color = group_label), size = 5, show.legend = NA) +
  scale_color_manual(values = group_colors, name = "Group Label") +
  
  new_scale_color() +  # Reset color scale for next use
  
  geom_tippoint(aes(x = x + 3, color = r_label), size = 5, show.legend = NA) +
  scale_colour_manual(values = r_label_colors, name = "R Label")+
  
  theme(
    legend.key.size = unit(0.2, "cm"),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4),
  )

circular_plot_groups_species

# Save
ggsave("R/data_outputs/phylo_tree/circular_plot.pdf", width = 7, height = 5, limitsize = FALSE)


## adding in mass info ----

phylo_plot_data_mass_species <- phyto_subset %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  # get mean mass for each species
  group_by(species) %>% 
  
  summarise(
    mean.biovol = mean(biovolume),
    mean.mass.c = mean(mass.c),
    mean.mass.d = mean(mass.d),
    .groups = "drop"
  ) 


# format mass info
log_mass_species <- phylo_plot_data_update_species %>% 
  
  left_join(phylo_plot_data_mass_species, by = "species") %>% 
  
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
circular_plot_mass_species = circular_plot_groups_species + new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass_species <- gheatmap(circular_plot_mass_species, log_mass_species, offset=3, width=0.2, colnames = F)+
  
  theme(
    legend.box = "horizontal",
    legend.key.size = unit(0.1, "cm"),
    legend.spacing.x = unit(-0.3, 'cm'),
    legend.text = element_text(size = 4),
    legend.title = element_text(size = 4),
  )

circular_plot_mass_species

ggsave("R/Data_outputs/plots/circular_plot_mass_species.png", width = 7, height = 5, limitsize = FALSE)
