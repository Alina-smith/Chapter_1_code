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
trait_data <- readRDS("R/Data_outputs/final_products/tol/phyto_traits_genus.rds")

# Format data ----

# Get a taxonomy list to add in in later steps
taxonomy <- trait_data %>% 
  
  select(
    genus,
    family,
    order,
    class,
    phylum,
    kingdom
  ) %>% 
  
  distinct(genus, .keep_all = TRUE)

# get mean body size for each genera
phylogeny_plot_data <- trait_data %>% 
  
  # get mean mass for each taxa
  group_by(genus) %>% 
  
  summarise(
    mass.mean = mean(mass),
    .groups = "drop"
  ) %>% 
  
  left_join(
    taxonomy, by = "genus"
  )

# Initial explore of data
glimpse(phylogeny_plot_data)

phylogeny_plot_data_subset <- phylogeny_plot_data %>% 
  select(., mass.mean, genus)

length(unique(phylogeny_plot_data_subset$genus)) #958 genera

hist(log(phylogeny_plot_data_subset$mass.mean))

# Plotting inial tree ----
## Get phylo relationships from a list of taxa: ----

# 1) Match my names with taxa names in OTT (open tree taxonomy)
taxa <- tnrs_match_names(unique(phylogeny_plot_data_subset$genus))

head(taxa) # view data

# 2) Check that all the names I inputted have been assigned a name by OTT

# Map the search_string and unique_name columns 
taxon_map <- structure(
  taxa$search_string, names = taxa$unique_name
  ) 

# check for any that havent been picked up by tol and have a missing unique_name
unique(is.na(taxon_map)) # false means there are no missing names so don't need to do anything

# 3) Remove species that aren't in synthetic tree
# Some taxa aren't in the OTL synthetic tree so need to find which ones aren't and remove them from my list

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_id(taxa))
in_tree

sum(in_tree == TRUE) # 761 genus in tree
sum(in_tree == FALSE) # 197 Genus not in tree

## Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

tree <- tol_induced_subtree(ott_id(taxa)[in_tree])

# 4) Make results as a phylo object
# tips = end of the tree branches aka the genera in this case
# internal nodes = the inner links in the tree

tol_about() # gives info about the current synthetic tree
tree # shows info about my tree
class(tree) # check that it is a phylo

## Plot ----
plot(tree, show.tip.label = FALSE)

#check some tip labels
tree$tip.label[1:5] # has the taxa name and the ott as the tip label

# save tree out here before adapting it for plotting: 
write.nexus(tree, file="R/Data_outputs/exploring/genus_tree_phylo_pre_plot.nex")

# Editing initial plot ----

## Replace the tip labels with the labels in the dataset ----

# Make a vector of taxa names with extra info (ott number) removed so it is just the taxa names
tips_ott_removed <- strip_ott_ids(tree$tip.label, remove_underscores = TRUE)

head(tips_ott_removed) # now just has the taxa name as the tip label instead of the taxa name and ott

# use the taxon map made earlier to map the new tip labels with the dataset names and replace with the dataset names
tree_new_tips <- tree
tree_new_tips$tip.label <- unname(taxon_map[tips_ott_removed])

# Plot with new names
plot(tree_new_tips, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tree_new_tips, show.tip.label = FALSE)

tree$node.label<- NULL #remove node labels 

# remove the taxa from taxon map that aren't in the tree
taxon_map_used <- taxon_map[taxon_map %in% tree_new_tips$tip.label]

# Plotting tree circular ----

# make the taxa names lower case to match the taxa object
phylogeny_plot_data_subset_lower <- phylogeny_plot_data_subset %>% 
  
  mutate(
    genus = tolower(genus)
  )


# think this gives the data that actually has its Genus mapped into the tree
phylogeny_plot_data_in_tree <- phylogeny_plot_data_subset_lower %>% 
  filter(
    genus %in% tree_new_tips$tip.label
  )

table(df4)

# get phylogeny_plot_data_in_tree with unique name used in otl and kingdom or mass
# make my Genus lower case to match a column from taxa search in rotl
# df4$Genus already like that

#get the 2 columns interested
taxa2<- taxa %>% 
  select(
    search_string, 
    unique_name
    )

names(taxa2)[names(taxa2) == "search_string"] <- "genus"


#match them up 
df5 <- left_join(df4, taxa2, by = "genus")
df6 <- df5 %>% 
  select(., c("genus", "unique_name", "mass.mean"))

df6$tip.label <- df6$genus #using old names here to make figure

#table(df5$met_category)

df7 <- df6 %>% 
  group_by(tip.label) %>% 
  mutate(Mean_mass_uniq = mean(mass.mean), na.rm=TRUE)



#need to make tip.labels match
df5$tip.label <- sub(" .*", "", df5$tip.label)


q <- ggtree(tr, branch.length='none', layout='circular') 
q
q <-  q %<+% df7 + geom_tippoint(aes(color = mass.mean), 
                                 size=1, show.legend=TRUE) 
q

setdiff(df7$tip.label, tr$tip.label)


ggsave("R/Data_outputs/exploring/Tree_phylo_kingdom_colour.pdf", width = 7, height = 5, limitsize = FALSE)

glimpse(df7)

df7<- df7 %>% 
  select(., c("Mean_mass_uniq") )

df7 <- unique(df7)

blag = df7$tip.label
#make row names the tip labels
rownames(df7) <- df7$tip.label
glimpse(df7)

#df7 <- ungroup(df7)


df7$tip.label = NULL
df7 = as.data.frame(df7)

rownames(df7) <- blag

library(ggnewscale)

q2 = q + new_scale_fill()

df7$Mean_mass_uniq = log(df7$Mean_mass_uniq)
fin_plot <- gheatmap(q2, df7, offset=2, width=0.2, colnames = F) #+
#scale_fill_gradient2("Mean_mass_uniq", guide = "colourbar") 

fin_plot

ggsave("R/Data_outputs/exploring/Tree_phylo_Group_mass.pdf", width = 7, height = 5, limitsize = FALSE)


write_csv(bodysize_data_phylogeny_plot, "R/Data_outputs/databases/bodysize_phylogeny_plot.csv")



