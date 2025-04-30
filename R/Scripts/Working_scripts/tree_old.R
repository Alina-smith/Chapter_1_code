# Plotting

# packages ----
library(plyr)
library(tidyverse)
library(rotl)
library(ape)

install.packages(c("treeplyr", "BiocManager"))
library(BiocManager)

install.packages(c("ggtree","treeio"))
BiocManager::install("ggtree", force = TRUE)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)

# Species ----

# Data ----
data <- readRDS("R/Data_outputs/database_products/final_products/phyto_subset.rds") %>% 
  # select just individuals for now
  filter(
    nu == "individual"
  )

resolved <- readRDS("R/Data_outputs/database_products/taxonomy/resolved.rds")

# Format data ----

## Select relavent data ----
# Get a taxonomy list to add in in later steps
extra_data <- trait_data %>% 
  
  select(
    ott.id,
    taxa.name,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom,
    r.group,
    group,
    nu
  ) %>% 
  
  distinct(ott.id, .keep_all = TRUE)

# get mean body size for each species
phylo_plot_data_species <- trait_data %>% 
  
  filter(
    !is.na(species)
  ) %>% 
  
  # get mean mass for each species
  group_by(ott.id) %>% 
  
  summarise(
    mean.biovol = mean(biovolume),
    mean.mass.c = mean(mass.c),
    mean.mass.d = mean(mass.d),
    .groups = "drop"
  ) %>% 
  
  left_join(
    extra_data, by = "ott.id"
  ) %>% 
  
  # need to add in the names of ones that were changed but with the same ott.id in taxonomy script
  
  left_join(
    select(
      resolved, ott_id, unique_name
    ), by = c("ott.id" = "ott_id")
  ) %>% 
  
  mutate(
    taxa.name = if_else(
      taxa.name != unique_name,
      unique_name,
      taxa.name
    )
  ) %>% 
  
  distinct(
    ott.id, .keep_all = TRUE
  ) %>% 
  
  select(
    - unique_name
  )


# Initial explore of data
glimpse(phylo_plot_data_species)

length(phylo_plot_data_species$ott.id) # 3597 species

hist(log(phylo_plot_data_species$mean.biovol))
hist(log(phylo_plot_data_species$mean.mass.c))
hist(log(phylo_plot_data_species$mean.mass.d))

## Get phylo relationships from a list of taxa: ----

# 1) Match my names with taxa names in OTT (open tree taxonomy)
taxa_species <- tnrs_match_names(phylo_plot_data_species$taxa.name)

head(taxa_species) # view data

# 2) Check that all the names I inputted have been assigned a name by OTT

# Map the search_string and unique_name columns 
taxon_map_species <- structure(
  taxa_species$search_string, names = taxa_species$unique_name
  ) 

# check for any that havent been picked up by tol and have a missing unique_name
unique(is.na(taxon_map_species)) # false means there are no missing names so don't need to do anything

## Remove species that aren't in synthetic tree ----
# Some taxa aren't in the OTL synthetic tree so need to find which ones aren't and remove them from my list

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_species <- is_in_tree(ott_id = taxa_species$ott_id)

saveRDS(in_tree_species, "R/data_outputs/phylogeny/in_tree_species.rds")

in_tree_species

sum(in_tree_species == TRUE) # 3081 species in tree
sum(in_tree_species == FALSE) # 516 species not in tree

# Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 
# tips = end of the tree branches aka the genera in this case
# internal nodes = the inner links in the tree

taxa_species_in_tree <- taxa_species[in_tree_species, ] # get list of just species in the tree

tree_species <- tol_induced_subtree(ott_ids = taxa_species_in_tree$ott_id) # Make tree

tol_about() # gives info about the current synthetic tree
tree_species # shows info about my tree
class(tree_species) # check that it is a phylo

# save tree out here before adapting it for plotting: 
write.nexus(tree_species, file="R/Data_outputs/phylogeny/phylo_tree_species.nex")

# Square plot ----

## Plot tree ----
plot(tree_species, show.tip.label = FALSE)

## Edit tip labels ----
# Replace the tip labels on the plot with the corresponding names in the dataset

# Make a vector of taxa names with extra info (ott number) removed so it is just the taxa names
tips_ott_removed <- strip_ott_ids(tree_species$tip.label, remove_underscores = TRUE)

head(tips_ott_removed) # now just has the taxa name as the tip label instead of the taxa name and ott

# use the taxon map made earlier to map the new tip labels with the dataset names and replace with the dataset names
tree_species_new_tips <- tree_species
tree_species_new_tips$tip.label <- unname(taxon_map_species[tips_ott_removed])
tree_species_new_tips$node.label<- NULL #remove node labels 

# Plot with new names
plot(tree_species_new_tips, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tree_species_new_tips, show.tip.label = FALSE)

# Update lists ----
# Want to update all the lists to contain just the taxa that are present in the tree

## taxon map ----
taxon_map_species_in_tree <- taxon_map_species[taxon_map_species %in% tree_species_new_tips$tip.label]

## taxa ----
# make the taxa names lower case to match the taxa object

phylo_plot_data_species_2 <-  phylo_plot_data_species %>% 
  mutate(
    taxa.name = tolower(taxa.name)
  )

taxa_species_in_tree_2 <- taxa_species_in_tree %>% 
  
  # join in the taxa info 
  left_join(
    phylo_plot_data_species_2, by = c("search_string" = "taxa.name")
  ) %>% 
  
  distinct(
    ott_id, .keep_all = TRUE
  ) %>% 
  
  # make new column called tip.label that is the same as genus
  mutate(
    tip.label = search_string
  ) %>% 
  
  # group for plotting
  group_by(tip.label)

# explore data a bit
length(taxa_species_in_tree_2$tip.label) # check that there are still the same number of species - shhould be same as sum(in_tree == TRUE) (3081)
table(taxa_species_in_tree_2$phylum)

# Plotting tree circular ----

## initial tree ----
circular_plot_species <- ggtree(tree_species_new_tips, branch.length='none', layout='circular') 
circular_plot_species

## adding in group info ----

# need to make a new column just to make the labels of the tree a bit easier to read
taxa_species_in_tree_edit <- taxa_species_in_tree_2 %>%
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    phylum_label = paste0("Phylum: ", phylum),
    r.group_label = paste0("r.group: ", r.group)
    )

# plot

circular_plot_species_groups <- circular_plot_species %<+% taxa_species_in_tree_edit +
  geom_tippoint(aes(color = r.group_label), size = 2, show.legend = F) +
  geom_tippoint(aes(x = x + 1.5, color = phylum_label), size = 2, show.legend = F) +
  geom_tippoint(aes(x = x + 3, color = kingdom_label), size = 2, show.legend = F) +
  
  # edit colours
  scale_color_manual(values =
    c("Kingdom: Plantae" = "#228B22", "Kingdom: Bacteria" = "#800000", "Kingdom: Protozoa" = "#4682B4", "Kingdom: Chromista" = "#4B0082",
      "Phylum: Chlorophyta" = "#90EE90", "Phylum: Charophyta" = "#66CDAA", "Phylum: Glaucophyta" = "#3CB371", "Phylum: Rhodophyta" = "#2E8B57", "Phylum: Cyanobacteria" = "#CD5C5C", "Phylum: Euglenozoa" = "#87CEEB", "Phylum: Myzozoa" = "#E6A9EC", "Phylum: Cryptophyta" = "#D8B2D1", "Phylum: Bacillariophyta"= "#C080C0", "Phylum: Ochrophyta" = "#9B4F96", "Phylum: Haptophyta" = "#7F3FBF", "Phylum: Bigyra"= "#6A1E9C",
      "r.group: H1" = "#FFB3BA", "r.group: Unassigned" = "#FFDFBA", "r.group: J" = "#FFFFBA", "r.group: W1" = "#BAFFB3", "r.group: LO" = "#BAE1FF", "r.group: X2" = "#FFB3FF", "r.group: N" = "#FFCCFF", 
      "r.group: P" = "#D4E157", "r.group: X1" = "#FFEB3B", "r.group: LM" = "#FFD54F", "r.group: MP" = "#FF7043", "r.group: Tb" = "#D32F2F", "r.group: F" = "#FF8A65", "r.group: W2" = "#B2FF59", "r.group: X3" = "#64B5F6",
      "r.group: C" = "#4DD0E1", "r.group: M" = "#FFCDD2", "r.group: Tc" = "#FFEBEE", "r.group: D" = "#A5D6A7", "r.group: Y" = "#B3E5FC", "r.group: E" = "#FFF176", "r.group: K" = "#FF7043", "r.group: Lo" = "#E57373",
      "r.group: S1" = "#81C784", "r.group: T" = "#FF8A80", "r.group: A" = "#FFEE58", "r.group: B" = "#FFEB3B", "r.group: U" = "#F06292", "r.group: Z" = "#BA68C8", "r.group: Ws" = "#4CAF50", "Sr.group: N" = "#FF9800",
      "r.group: G" = "#9C27B0", "r.group: H2" = "#2196F3", "r.group: Xph" = "#FF9800", "r.group: S2" = "#8BC34A", "r.group: Q" = "#D1C4E9", "r.group: R" = "#B3E5FC"
      )
    )

circular_plot_species_groups

# Save
ggsave("R/data_outputs/database_products/plots/circular_plot_groups.pdf", width = 7, height = 5, limitsize = FALSE)

## adding in mass info ----

# format mass info
log.mass <- as.data.frame(taxa_species_in_tree_2) %>% 
  
  select(mean.mass.c, tip.label) %>% 
  
  mutate(
    log.mass = log(mean.mass.c)
  ) %>% 
  
  select(
    - mean.mass.c
  ) %>% 
  
  column_to_rownames(var = "tip.label")

# plot
circular_plot_mass = circular_plot_species_groups + new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass <- gheatmap(circular_plot_mass, log.mass, offset=2, width=0.2, colnames = F)
circular_plot_mass

ggsave("R/Data_outputs/database_products/plots/circular_plot_mass.png", width = 7, height = 5, limitsize = FALSE)

# genus ----

# Data ----
trait_data <- readRDS("R/Data_outputs/database_products/final_products/phyto_all.rds") %>% 
  # select just individuals for now
  filter(
    nu == "individual",
    !is.na(genus)
  )

# Format data ----

## Select relavent data ----
# Get a taxonomy list to add in in later steps
extra_data_genus <- trait_data %>% 
  
  filter(
    r.group != "Unassigned"
  ) %>% 
  
  select(
    genus,
    family,
    order,
    class,
    phylum,
    kingdom,
    r.group,
    group,
    nu
  ) %>% 
  
  distinct(genus, .keep_all = TRUE) %>% 
  
  mutate(
    taxa.name = genus
  )

# get mean body size for each species
phylo_plot_data_genus <- trait_data %>% 
  
  filter(
    r.group != "Unassigned"
  ) %>% 
  
  mutate(
    taxa.name = genus
  ) %>% 
  
  # get mean mass for each species
  group_by(taxa.name) %>% 
  
  summarise(
    mean.biovol = mean(biovolume),
    mean.mass.c = mean(mass.c),
    mean.mass.d = mean(mass.d),
    .groups = "drop"
  ) %>% 
  
  left_join(
    extra_data_genus, by = "taxa.name"
  ) 


# Initial explore of data
glimpse(phylo_plot_data_genus)

length(phylo_plot_data_genus$taxa.name) # 649 genera

hist(log(phylo_plot_data_genus$mean.biovol))
hist(log(phylo_plot_data_genus$mean.mass.c))
hist(log(phylo_plot_data_genus$mean.mass.d))

## Get phylo relationships from a list of taxa: ----

# 1) Match my names with taxa names in OTT (open tree taxonomy)
taxa_genus <- tnrs_match_names(phylo_plot_data_genus$taxa.name)

head(taxa_genus) # view data

# 2) Check that all the names I inputted have been assigned a name by OTT

# Map the search_string and unique_name columns 
taxon_map_genus <- structure(
  taxa_genus$search_string, names = taxa_genus$unique_name
) 

# check for any that havent been picked up by tol and have a missing unique_name
unique(is.na(taxon_map_genus)) # false means there are no missing names so don't need to do anything

## Remove species that aren't in synthetic tree ----
# Some taxa aren't in the OTL synthetic tree so need to find which ones aren't and remove them from my list

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_genus <- is_in_tree(ott_id = taxa_genus$ott_id)

saveRDS(in_tree_genus, "R/data_outputs/database_products/plots/in_tree_genus.rds")

in_tree_genus

sum(in_tree_genus == TRUE) # 518 genera in tree
sum(in_tree_genus == FALSE) # 131 genera not in tree

# Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 
# tips = end of the tree branches aka the genera in this case
# internal nodes = the inner links in the tree

taxa_genus_in_tree <- taxa_genus[in_tree_genus, ] # get list of just species in the tree

tree_genus <- tol_induced_subtree(ott_ids = taxa_genus_in_tree$ott_id) # Make tree

tol_about() # gives info about the current synthetic tree
tree_genus # shows info about my tree
class(tree_genus) # check that it is a phylo

# save tree out here before adapting it for plotting: 
write.nexus(tree_genus, file="R/Data_outputs/database_products/plots/phylo_tree_genus.nex")

# Square plot ----

## Plot tree ----
plot(tree_genus, show.tip.label = FALSE)

## Edit tip labels ----
# Replace the tip labels on the plot with the corresponding names in the dataset

# Make a vector of taxa names with extra info (ott number) removed so it is just the taxa names
tips_ott_removed <- strip_ott_ids(tree_genus$tip.label, remove_underscores = TRUE)

head(tips_ott_removed) # now just has the taxa name as the tip label instead of the taxa name and ott

# use the taxon map made earlier to map the new tip labels with the dataset names and replace with the dataset names
tree_genus_new_tips <- tree_genus
tree_genus_new_tips$tip.label <- unname(taxon_map_genus[tips_ott_removed])
tree_genus_new_tips$node.label<- NULL #remove node labels 

# Plot with new names
plot(tree_genus_new_tips, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tree_genus_new_tips, show.tip.label = FALSE)

# Update lists ----
# Want to update all the lists to contain just the taxa that are present in the tree

## taxon map ----
taxon_map_genus_in_tree <- taxon_map_genus[taxon_map_genus %in% tree_genus_new_tips$tip.label]

## taxa ----
# make the taxa names lower case to match the taxa object

phylo_plot_data_genus_2 <-  phylo_plot_data_genus %>% 
  mutate(
    taxa.name = tolower(taxa.name)
  )

taxa_genus_in_tree_2 <- taxa_genus_in_tree %>% 
  
  # join in the taxa info 
  left_join(
    phylo_plot_data_genus_2, by = c("search_string" = "taxa.name")
  ) %>% 
  
  distinct(
    ott_id, .keep_all = TRUE
  ) %>% 
  
  # make new column called tip.label that is the same as genus
  mutate(
    tip.label = search_string
  ) %>% 
  
  # group for plotting
  group_by(tip.label)

# explore data a bit
length(taxa_genus_in_tree_2$tip.label) # check that there are still the same number of species - shhould be same as sum(in_tree == TRUE) (3081)
table(taxa_genus_in_tree_2$phylum)

# Plotting tree circular ----

## initial tree ----
circular_plot_genus <- ggtree(tree_genus_new_tips, branch.length='none', layout='circular') 
circular_plot_genus

## adding in group info ----

# need to make a new column just to make the labels of the tree a bit easier to read
taxa_genus_in_tree_edit <- taxa_genus_in_tree_2 %>%
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    phylum_label = paste0("Phylum: ", phylum),
    r.group_label = paste0("r.group: ", r.group)
  )

# plot

circular_plot_genus_groups <- circular_plot_genus %<+% taxa_genus_in_tree_edit +
  geom_tippoint(aes(color = r.group_label), size = 2, show.legend = F) +
  geom_tippoint(aes(x = x + 1.5, color = phylum_label), size = 2, show.legend = F) +
  geom_tippoint(aes(x = x + 3, color = kingdom_label), size = 2, show.legend = F) +
  
  # edit colours
  scale_color_manual(values =
                       c("Kingdom: Plantae" = "#228B22", "Kingdom: Bacteria" = "#800000", "Kingdom: Protozoa" = "#4682B4", "Kingdom: Chromista" = "#6A1E9C",
                         "Phylum: Chlorophyta" = "#90EE90", "Phylum: Charophyta" = "#66CDAA", "Phylum: Cyanobacteria" = "#CD5C5C", "Phylum: Euglenozoa" = "#87CEEB", "Phylum: Myzozoa" = "#E6A9EC", "Phylum: Cryptophyta" = "#D8B2D1", "Phylum: Bacillariophyta"= "#C080C0", "Phylum: Ochrophyta" = "#9B4F96", "Phylum: Haptophyta" = "#7F3FBF",
                         "r.group: A"= "#FFB3BA", "r.group: Tb"= "#FFDFBA", "r.group: MP"= "#FFFFBA", "r.group: J"= "#BAFFB3", "r.group: X2" = "#BAE1FF", "r.group: X1"= "#FFB3FF", "r.group: D" = "#FFCCFF", "r.group: LO" = "#D4E157", "r.group: X3" = "#FFEB3B", "r.group: S1" = "#FFD54F", "r.group: K"= "#FF7043",
                         "r.group: S2"= "#D32F2F", "r.group: C"= "#FF8A65", "r.group: F"= "#B2FF59", "r.group: P" = "#64B5F6", "r.group: G" = "#4DD0E1", "r.group: Lo" = "#FFCDD2", "r.group: V" = "#FFEBEE", "r.group: E" = "#A5D6A7", "r.group: N" = "#B3E5FC", "r.group: Y" = "#FFF176", "r.group: B" = "#FF7043",
                         "r.group: W1" = "#81C784", "r.group: T" = "#FF8A80", "r.group: Lm" = "#FFEE58", "r.group: Q" = "#FFEB3B", "r.group: Z" = "#F06292", "r.group: Xph" = "#BA68C8", "r.group: LM" = "#E57373", "r.group: W2" = "#4CAF50", "r.group: Ws" = "#D1C4E9", "r.group: U" = "#B3E5FC"  
                       )
  )

circular_plot_genus_groups



# Save
ggsave("R/data_outputs/database_products/plots/circular_plot_genus_groups.pdf", width = 7, height = 5, limitsize = FALSE)

## adding in mass info ----

# format mass info
log_mass_genus <- as.data.frame(taxa_genus_in_tree_2) %>% 
  
  select(mean.mass.d, tip.label) %>% 
  
  mutate(
    log.mass = log10(mean.mass.d)
  ) %>% 
  
  select(
    - mean.mass.d
  ) %>% 
  
  column_to_rownames(var = "tip.label")

# plot
circular_plot_genus_mass = circular_plot_genus_groups + new_scale_fill() # so new geoms added in can use a new scale

circular_plot_genus_mass <- gheatmap(circular_plot_genus_mass, log_mass_genus, offset=2, width=0.2, colnames = F)
circular_plot_genus_mass

ggsave("R/Data_outputs/database_products/plots/circular_plot_genus_mass.png", width = 7, height = 5, limitsize = FALSE)


































