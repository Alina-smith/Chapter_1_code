# Plotting

# packages ----
library(plyr)
library(tidyverse)
library(rotl)
library(ape)
library(ggplot2)

install.packages(c("treeplyr", "BiocManager"))
library(BiocManager)

install.packages(c("ggtree","treeio"))
BiocManager::install("ggtree", force = TRUE)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)


# Data ----
trait_data <- readRDS("R/Data_outputs/final_products/tol/phyto_traits_species.rds") %>% 
  # select just individuals for now
  filter(
    nu == "individual"
  )

# Format data ----

## Select relavent data ----
# Get a taxonomy list to add in in later steps
extra_data <- trait_data %>% 
  
  select(
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom,
    reynolds.group,
    padisak.group,
    nu
  ) %>% 
  
  distinct(species, .keep_all = TRUE) %>% 
  
  mutate(
    functional.group = case_when(
      !is.na(padisak.group) ~ padisak.group,
      is.na(padisak.group) & !is.na(reynolds.group) ~ reynolds.group,
      TRUE ~ "Unknown"
    )
  ) %>% 
  
  select(
    - reynolds.group,
    - padisak.group
  ) 

# get mean body size for each genera
phylo_plot_data <- trait_data %>% 
  
  # get mean mass for each taxa
  group_by(species) %>% 
  
  summarise(
    mass.mean = mean(mass),
    .groups = "drop"
  ) %>% 
  
  left_join(
    extra_data, by = "species"
  )

# Initial explore of data
glimpse(phylo_plot_data)

length(unique(phylo_plot_data$species)) # 4317 species

hist(log(phylo_plot_data$mass.mean))

## Get phylo relationships from a list of taxa: ----

# 1) Match my names with taxa names in OTT (open tree taxonomy)
taxa <- tnrs_match_names(unique(phylo_plot_data$species))

head(taxa) # view data

# 2) Check that all the names I inputted have been assigned a name by OTT

# Map the search_string and unique_name columns 
taxon_map <- structure(
  taxa$search_string, names = taxa$unique_name
  ) 

# check for any that havent been picked up by tol and have a missing unique_name
unique(is.na(taxon_map)) # false means there are no missing names so don't need to do anything

## Remove species that aren't in synthetic tree ----
# Some taxa aren't in the OTL synthetic tree so need to find which ones aren't and remove them from my list

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_id(taxa))
in_tree

sum(in_tree == TRUE) # 3085 species in tree
sum(in_tree == FALSE) # 522 species not in tree

# Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 
# tips = end of the tree branches aka the genera in this case
# internal nodes = the inner links in the tree

tree <- tol_induced_subtree(ott_id(taxa)[in_tree])

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

# save tree out here before adapting it for plotting: 
write.nexus(tree, file="R/Data_outputs/exploring/species_phylo_tree.nex")

# Square plot ----

## Plot tree ----
plot(tree, show.tip.label = FALSE)

## Edit tip labels ----
# Replace the tip labels on the plot with the corresponding names in the dataset

# Make a vector of taxa names with extra info (ott number) removed so it is just the taxa names
tips_ott_removed <- strip_ott_ids(tree$tip.label, remove_underscores = TRUE)

head(tips_ott_removed) # now just has the taxa name as the tip label instead of the taxa name and ott

# use the taxon map made earlier to map the new tip labels with the dataset names and replace with the dataset names
tree_new_tips <- tree
tree_new_tips$tip.label <- unname(taxon_map[tips_ott_removed])
tree_new_tips$node.label<- NULL #remove node labels 

# Plot with new names
plot(tree_new_tips, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tree_new_tips, show.tip.label = FALSE)

# Update lists ----
# Want to update all the lists to contain just the taxa that are present in the tree

## taxon map ----
taxon_map_in_tree <- taxon_map[taxon_map %in% tree_new_tips$tip.label]

## taxa ----
# make the taxa names lower case to match the taxa object
taxa_in_tree <- phylo_plot_data %>% 
  
  # make lower case to match tip.label
  mutate(
    species = tolower(species)
  ) %>% 
  
  filter(
    species %in% tree_new_tips$tip.label # tip.labels were changed to the ones used in the database so can just do this
  ) %>% 
  
  # join in the taxa info 
  left_join(
    select(
    taxa, search_string, unique_name
    ), by = c("species" = "search_string")
  ) %>% 
  
  # make new column called tip.label that is the same as genus
  mutate(
    tip.label = species
  ) %>% 
  
  # group for plotting
  group_by(tip.label)

# explore data a bit
length(unique(taxa_in_tree$tip.label)) # check that there are still the same number of species - shhould be same as sum(in_tree == TRUE) (3085)
table(taxa_in_tree$phylum)

# Plotting tree circular ----

## initial tree ----
circular_plot <- ggtree(tree_new_tips, branch.length='none', layout='circular') 
circular_plot

## adding in group info ----

circular_plot_groups <-  circular_plot %<+% taxa_in_tree +
  geom_tippoint(aes(color = phylum), size=2, show.legend=TRUE) 

+
  geom_tippoint(aes(color = functional.group), size=1, show.legend=TRUE)


circular_plot_groups

# check if there are any differences between my tip.labels and the tol tip.labels
setdiff(taxa_in_tree$tip.label, tree_new_tips$tip.label)

# Save
ggsave("R/Data_outputs/exploring/species_tree_groups.pdf", width = 7, height = 5, limitsize = FALSE)

## adding in mass info ----

# format mass info
masses <- as.data.frame(taxa_in_tree) %>% 
  select(mass.mean, tip.label) %>% 
  mutate(
    log.mass = log(mass.mean)
  ) %>% 
  select(
    - mass.mean
  ) %>% 
  column_to_rownames(var = "tip.label")

# plot
circular_plot_mass = circular_plot_groups + new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass <- gheatmap(circular_plot_mass, masses, offset=2, width=0.2, colnames = F)
circular_plot_mass


ggsave("R/Data_outputs/exploring/circular_plot_mass.pdf", width = 7, height = 5, limitsize = FALSE)


write_csv(bodysize_data_phylogeny_plot, "R/Data_outputs/databases/bodysize_phylogeny_plot.csv")



