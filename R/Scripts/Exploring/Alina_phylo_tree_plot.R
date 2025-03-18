#Phylo Tree start Alina
#23/10/24


#pkgs ----

library(plyr)
library(tidyverse)
library(rotl)
library(ape)
library(ggplot2)

install.packages(c("treeplyr", "BiocManager"))
library(BiocManager)

install.packages(c("ggtree","treeio"))
BiocManager::install("ggtree")
library(ggtree)
library(ggtreeExtra)

#Data ----

df <- readRDS("R/Data_outputs/final_products/tol/phyto_traits_genus.rds")

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
df <- df %>% 
  
  # get mean mass for each taxa
  group_by(genus) %>% 
  
  summarise(
    mass.mean = mean(mass),
    .groups = "drop"
  ) %>% 
  
  left_join(
    taxonomy, by = "genus"
  )

#have a quick look at the data
glimpse(df)

df <- df %>% 
  select(., mass.mean, kingdom, genus)

length(unique(df$genus)) #981 genera

hist(log(df$mass.mean))

#Get phylo relationships from a list of taxa: ----

#match my names with taxa names in OTT (open tree taxonomy) (first check)
d_taxa <- tnrs_match_names(unique(df$genus))
head(d_taxa)

#this is where you go through the warnings

d_taxon_map <- structure(d_taxa$search_string, names = d_taxa$unique_name) 
#shows a map between OTL names and my names

unique(is.na(d_taxon_map)) #if false is great and we can keep going
#for now will keep going but need to check

#some taxa not in OTL so...
#Removing the taxa missing from the synthetic tree
d_in_tree <- is_in_tree(ott_id(d_taxa))
d_in_tree

sum(d_in_tree ==TRUE) #772 
sum(d_in_tree==FALSE) #198 Genus not in tree

#tree with only the taxa that are in the synthetic tree
d_tr <- tol_induced_subtree(ott_id(d_taxa)[d_in_tree])
#results as a phylo object
tol_about()
d_tr
class(d_tr)

plot(d_tr, show.tip.label = FALSE)

#check some tip labels
d_tr$tip.label[1:5]

#save tree out here before adapting it for plotting: 
write.nexus(tr, file="Data/tree_phylo_pre_plot.nex")



#remove the extra information from the tip labels (only shows the ones i used for the tree)
d_otl_tips <- strip_ott_ids(d_tr$tip.label, remove_underscores = TRUE)

head(d_otl_tips)


d_tr2 <- d_tr
d_tr2$tip.label <- unname(d_taxon_map[d_otl_tips])

#use taxon map to replace the tip labels in the tree with the Genus names from dataset.
d_tr$tip.label <-  taxon_map[d_otl_tips] #older way

plot(d_tr, cex=.8, label.offset =.1, no.margin=TRUE)
plot(d_tr, show.tip.label = FALSE)

####
d_tr$node.label<- NULL #remove node labels 


#genera actually in tree
d_taxon_map_used <- d_taxon_map[d_taxon_map %in% d_tr$tip.label]

#Plotting tree circular ----

df$genus <- tolower(df$genus)


#think this gives the data that actually has its Genus mapped into the tree
df4 <- df[df$genus %in% d_tr$tip.label, ]



table(df4$kingdom)

#get df with unique name used in otl and kingdom or mass
#make my Genus lower case to match a column from taxa search in rotl
# df4$Genus already like that

#get the 2 columns interested
d_taxa2<- d_taxa %>% select(., search_string, unique_name)
names(d_taxa2)[names(d_taxa2) == "search_string"] <- "genus"


#match them up 
df5 <- left_join(df4, d_taxa2, by = "genus")
df6 <- df5 %>% 
  select(., c("genus", "unique_name", "mass.mean", "kingdom"))

df6$tip.label <- df6$genus #using old names here to make figure

#table(df5$met_category)

df7 <- df6 %>% 
  group_by(tip.label) %>% 
  mutate(Mean_mass_uniq = mean(mass.mean), na.rm=TRUE)



#need to make tip.labels match
#df5$tip.label <- sub(" .*", "", df5$tip.label)


q <- ggtree(d_tr, branch.length='none', layout='circular')
q
q <-  q %<+% df7 + geom_tippoint(aes(color = kingdom), 
                                 size=1, show.legend=TRUE) 
q

setdiff(df7$tip.label, d_tr$tip.label)


ggsave("Results/Tree_phylo_kingdom_colour.pdf", width = 7, height = 5, limitsize = FALSE)

glimpse(df7)

df7<- taxa_in_tree %>% 
  select(mass.mean, tip.label)

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

ggsave("Results/Tree_phylo_king_mass.pdf", width = 7, height = 5, limitsize = FALSE)




