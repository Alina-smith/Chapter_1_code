# Packages
library(readxl)
library(tidyr)
library(tidyverse)
library(stringi)
library(taxize)
library(rotl)
library(ape)
library(ggplot2)

library(ggtree)
library(ggtreeExtra)
library(ggnewscale)


# Import data ----
taxonomy <- readRDS("R/data_outputs/database_products/taxonomy/taxonomy.rds")


phylo_plot_data <- taxonomy %>% 
  
  select(
    ott.id,
    tol.taxa.name,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 
  
  distinct(ott.id, .keep_all = TRUE)

## Get phylo relationships from a list of taxa: ----

# 1) Match my names with taxa names in OTT (open tree taxonomy)
taxa <- tnrs_match_names(unique(phylo_plot_data$tol.taxa.name))

# 2) Update ott_ids with ones that were changed above
# Find ones to change
taxonomy2 <- taxonomy %>% 
  mutate(
    tol.taxa.name = tolower(tol.taxa.name)
  )

to_change <- taxa %>% 
  left_join(
    select(
      taxonomy2, tol.taxa.name, ott.id
    ), by = c("search_string" = "tol.taxa.name")
  ) %>% 
  filter(
    ott_id != ott.id
  )

# Change them
taxa2 <- taxa %>% 
  mutate(
    ott_id = case_when(
      ott_id == 4004663 ~ 6001434,
      ott_id == 4922332 ~ 4016510,
      TRUE ~ ott_id
    ),
    ott_id = as.integer(ott_id)
  ) 

# Map the search_string and unique_name columns 
taxon_map <- structure(
  taxa2$search_string, names = taxa2$unique_name
) 

# Check for any that havent been picked up by tol and have a missing unique_name
unique(is.na(taxon_map)) # false means there are no missing names so don't need to do anything

## Remove species that aren't in synthetic tree ----
# Some taxa aren't in the OTL synthetic tree so need to find which ones aren't and remove them from my list

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_ids = taxa2$ott_id)

in_tree <- in_tree_tax

in_tree
saveRDS(in_tree, file = "R/in_tree.rds")

sum(in_tree == TRUE) # 4956
sum(in_tree == FALSE) # 814

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 
# tips = end of the tree branches aka the genera in this case
# internal nodes = the inner links in the tree
taxa_in_tree <- taxa2[in_tree, ] # get list of just species in the tree

phylo_plot_data2 <- phylo_plot_data %>% 
  mutate(
    tol.taxa.name = tolower(tol.taxa.name)
  )

taxa_in_tree_all <- taxa_in_tree %>% 
  left_join(
    select(phylo_plot_data2, - ott.id), by = c("search_string" = "tol.taxa.name")
  ) 

phyla <- taxa_in_tree_all %>% 
  distinct(
    phylum
  )

class <- taxa_in_tree_all %>% 
  filter(
    order == "Prasiolales"
  ) %>% 
  
  distinct(
    family
  )

order <- taxa_in_tree_all %>% 
  filter(
    class == "Arthropoda"
  ) %>% 
  
  distinct(
    order
  )

taxa_in_tree_subset <- taxa_in_tree_all 
%>% 
  
  filter(
    !(unique_name %in% c("Sphaerastrum fockii")),
    !(genus %in% c("Trochiscia")),
    #!(phylum %in% c("Rhodophyta", "Glaucophyta", "Cryptophyta")),
    #!(order %in% c("Zygnematales")),
    #!(family %in% c("Gonatozygaceae", "Peniaceae", "Closteriaceae")),
    #!(kingdom %in% c("Animalia", "Bacteria")),
    #phylum == "Chlorophyta",
    #!(phylum %in% c("Glaucophyta", "Rhodophyta")),
    #class == "Chlorophyceae",
    #order == "Sphaeropleales"
    #kingdom %in% c("Plantae")
    family %in% c("Characiaceae", "Sphaeropleaceae")
    #genus %in% c("Gloeobotrys", "Ellipsoidion", "Ducellieria", "Monodus", "Nephrodiella", "Ophiocytium", "Monallantus")
  )

# get tree
tree <- tol_induced_subtree(ott_ids = taxa_in_tree_subset$ott_id) # Make tree

## Plot tree square ----
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

## Update lists ----
# Want to update all the lists to contain just the taxa that are present in the tree

# taxon map
taxon_map_in_tree <- taxon_map[taxon_map %in% tree_new_tips$tip.label]

# taxa

taxa_update_in_tree <- taxa_in_tree_subset %>% 
  
  mutate( 
    # make new column called tip.label that is the same as taxa.name
    tip.label = search_string
  ) %>% 
  
  # group for plotting
  group_by(tip.label)

## Plotting tree circular ----
# initial tree
circular_plot <- ggtree(tree_new_tips, branch.length='none', layout='circular') 
circular_plot

# Adding in group info

taxa_update_in_tree_edit <- taxa_update_in_tree %>%
  mutate(
    kingdom_label = paste0("Kingdom: ", kingdom),
    class_label = paste0("Class: ", class),
    phylum_label = paste0("Phylum: ", phylum),
    order_label = paste0("Order: ", order),
    family_label = paste0("Family: ", family),
    genus_label = paste0("Genus: ", genus)
  )

circular_plot <- circular_plot %<+% taxa_update_in_tree_edit +
  #geom_tippoint(aes(color = genus_label), size = 3, show.legend = TRUE)+
  #geom_tippoint(aes(x = x + 1, color = family_label), size = 3, show.legend = TRUE)
  #geom_tippoint(aes(x = x + 2, color = order_label), size = 3, show.legend = TRUE)
  #geom_tippoint(aes(x = x + 3, color = class_label), size = 3, show.legend = TRUE)
  #geom_tippoint(aes(x = x + 5, color = phylum_label), size = 3, show.legend = TRUE)+
  geom_tippoint(aes(x = x + 7, color = kingdom_label), size = 3, show.legend = TRUE)+
  
  scale_color_manual(values =
                       c("Kingdom: Plantae" = "#006400", "Kingdom: Bacteria" = "#9B59B6", "Kingdom: Protozoa" = "#CC5500", "Kingdom: Chromista" = "#00008B", "Kingdom: Animalia" = "#8B0000",
                         "Phylum: Chlorophyta" = "#CDEFCB", "Phylum: Charophyta" = "#88D498", "Phylum: Glaucophyta" = "#4CAF50", "Phylum: Rhodophyta" = "#2E8B57",
                         "Phylum: Cyanobacteria" = "#B57EDC",
                         "Phylum: Euglenozoa" = "#FFD8A8", "Phylum: Amoebozoa" = "#FFA500",
                         "Phylum: Myzozoa" = "#CFE2F3", "Phylum: Cryptophyta" = "#B3E0FF", "Phylum: Bacillariophyta"= "#ADD8E6", "Phylum: Ochrophyta" = "#87CEEB", "Phylum: Haptophyta" = "#6495ED", "Phylum: Bigyra"= "#4682B4", "Phylum: Ciliophora" = "#1E3A5F", "Phylum: Cercozoa" = "#003366", "Phylum: Foraminifera" = "#002F4D",
                         "Phylum: Cnidaria" = "#FFCCCB", "Phylum: Mollusca" = "#F08080", "Phylum: Arthropoda" = "#FF6347", "Phylum: Rotifera" = "#D32F2F", "Phylum: Gastrotricha" = "#B71C1C"
                       )
  )

circular_plot

x <- classification %>% 
  filter(
    order == "Desmidiales"
  ) %>% 
  
  group_by(
    genus
  ) %>% 
  
  summarise(
    n = n()
  )

x <- classification %>% 
  filter(
    order == "Sphaeropleales"
  ) %>% 
  distinct(
    family
  )


bs <- bodysize_taxonomy %>% 
  select(
    taxa.name, original.taxa.name, body.size, bodysize.measurement, source.code
  )

classification("Tetraedron bitridens", db = "tol")

taxa_exp <- tnrs_match_names("Rhabdoderma")
taxa_exp

comparison_info <- inspect(taxa_exp, taxon_name = "Rhabdoderma")

comparison_info

tax_hist <- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[1], include_lineage = TRUE)))
tax_hist




x <- resolved %>% 
  head(20)

x_tnrs <- tnrs_match_names(unique(x$unique_name))

x_tax <- x_tnrs %>% 
  
  mutate(
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for that taxa
        tax_lineage(taxonomy_taxon_info(ott_id, include_lineage = TRUE))# rows = 1 so that is only takes the first one and doesn't require you select options for each one
      )[[1]] # select the first element of the list
    ) %>% 
  
  # take the info out of the list column so now it has multiple of the same search_string
  unnest(
    col = everything(), names_sep = "."
  ) %>% 
  
  filter(
    tax.rank != "no rank"
  ) %>% 
  
  select(
    unique_name,
    ott_id,
    tax.unique_name,
    tax.rank
  ) %>% 
  
  # Pivot so the ranks are now columns
  pivot_wider(
    names_from = tax.rank,
    values_from = tax.unique_name) %>% 
  
  unnest_wider(
    col = everything(), names_sep = "."
  )

x_tax <- list()  

# Loop through the indices of search_string
for (i in 1:length(x_tnrs$unique_name)) {
  x_tax[[i]] <- tax_lineage(taxonomy_taxon_info(x_tnrs$ott_id[i], include_lineage = TRUE))
                            }

x_tax_2 <- as.data.frame(x_tax)
%>% 
  



