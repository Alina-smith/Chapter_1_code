# Packages
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)
library(ape)
library(ggnewscale)

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

## Import data ----
phyto_subset <- readRDS("R/data_outputs/database_products/final_products/phyto_subset.rds")

## Format data ----
# Get a taxonomy list to add in in later steps

phylo_plot_data_genus <- phyto_subset %>% 
  
  select(
    nu, genus, phylum, kingdom, family, order, class, r.group.genus
  ) %>% 
  distinct(genus, .keep_all = TRUE) %>% 
  
  mutate(
    genus = tolower(genus)
  )

## Find only ones in the tree ----

taxa_genus <- tnrs_match_names(phylo_plot_data_genus$genus)

# Save
saveRDS(taxa_genus, "R/data_outputs/database_products/taxonomy/taxa_genus.rds")

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_genus <- is_in_tree(ott_ids = ott_id(taxa_genus))

# Save
saveRDS(in_tree_genus, "R/data_outputs/database_products/taxonomy/in_tree_genus.rds")

# View data
in_tree_genus

# See which ones are in and out
sum(in_tree_genus == TRUE) # 658
sum(in_tree_genus == FALSE) # 154

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

# get list of just species in the tree
taxa_in_tree <- phylo_plot_data_genus[in_tree_genus, ] %>% 
  
  left_join(
    taxa_genus, by = c("genus" = "search_string")
  ) 

# Make tree
tree <- tol_induced_subtree(ott_ids = taxa_in_tree$ott_id)

#save tree out here before adapting it for plotting: 
write.nexus(tree, file="R/data_outputs/exploring/tree_pre_plot.nex")

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

## Plot square tree ----
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
    genus = stripped.tip.label
  ) %>% 
  
  select(
    tip.label, genus, phylum, family, kingdom, class, order, r.group.genus
  ) %>% 
  
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    phylum_label = paste0("Phylum: ", phylum),
    r_label = paste0("R.group: ", r.group.genus)
  )

## Plotting circular tree ----

circular_plot <- ggtree(tree, branch.length='none', layout='circular') 
circular_plot

## Plotting circular tree with group info ----

circular_plot <- circular_plot %<+% phylo_plot_data_update +
  geom_tippoint(aes(x = x + 1, color = phylum_label), size = 3, show.legend = TRUE)+
  geom_tippoint(aes(x = x + 3, color = kingdom_label), size = 3, show.legend = TRUE) +
  geom_tippoint(aes(x = x + 5, color = r_label), size = 3, show.legend = TRUE)+
  scale_color_manual(values = c(
    "Kingdom: Plantae" = "#006400", "Kingdom: Bacteria" = "#9B59B6", "Kingdom: Protozoa" = "#8B0000", "Kingdom: Chromista" = "#00008B",
                                
    "Phylum: Chlorophyta" = "#e5f5e0", "Phylum: Charophyta" = "#a1d99b", "Phylum: Glaucophyta" = "#74c476", "Phylum: Rhodophyta" = "#238b45", "Phylum: Cryptophyta" = "#41ab5d",
    "Phylum: Cyanobacteria" = "#B57EDC",
    "Phylum: Euglenozoa" = "#F08080", "Phylum: Amoebozoa" = "#FF6347", "Phylum: Choanozoa" = "#D32F2F",
    "Phylum: Myzozoa" = "#6495ED", "Phylum: Bacillariophyta" = "#CFE2F3", "Phylum: Ochrophyta" = "#87CEEB", "Phylum: Haptophyta" = "#ADD8E6", "Phylum: Bigyra"= "#003366", "Phylum: Ciliophora (phylum in subkingdom SAR)" = "#1E3A5F", "Phylum: Cercozoa" = "#4682B4","Phylum: Heliozoa" = "#EBF5FA",
                       
    "R.group: A" = "#8DD3C7", "R.group: B" = "#FFFFB3", "R.group: C" = "#BEBADA", "R.group: D" = "#FB8072", "R.group: E" = "#80B1D3", "R.group: F" = "#FDB462", "R.group: G" = "#B3DE69", "R.group: J" = "#FCCDE5", "R.group: K" = "#D9D9D9", "R.group: LM" = "#BC80BD", "R.group: LO" = "#CCEBC5",
    "R.group: M" = "#FFED6F", "R.group: MP" = "#A6CEE3", "R.group: N" = "#1F78B4", "R.group: NA" = "#B2DF8A", "R.group: P" = "#33A02C", "R.group: Q" = "#FB9A99", "R.group: S1" = "#E31A1C", "R.group: S2" = "#FDBF6F", "R.group: T" = "#FF7F00", "R.group: TB" = "#CAB2D6",
    "R.group: U" = "#6A3D9A", "R.group: V" = "#FFFF99", "R.group: W0" = "#B15928", "R.group: W1" = "#F781BF", "R.group: W2" = "#999999", "R.group: WS" = "#66C2A5", "R.group: X1" = "#FC8D62", "R.group: X2" = "#E78AC3", "R.group: X3" = "#A6D854", "R.group: XPH" = "#FFD92F", "R.group: Y" = "#E5C494"
    )
    )

circular_plot

# Save
ggsave("R/data_outputs/exploring/circular_plot.pdf", width = 7, height = 5, limitsize = FALSE)


## adding in mass info ----


phylo_plot_data_mass <- phyto_subset %>% 
  
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
log_mass_genus <- phylo_plot_data_update %>% 
  
  left_join(phylo_plot_data_mass, by = "genus") %>% 
  
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
circular_plot_mass = circular_plot + new_scale_fill() # so new geoms added in can use a new scale

circular_plot_mass <- gheatmap(circular_plot_mass, log_mass_genus, offset=7, width=0.2, colnames = F)
circular_plot_mass

ggsave("R/Data_outputs/database_products/plots/circular_plot_mass.png", width = 7, height = 5, limitsize = FALSE)


