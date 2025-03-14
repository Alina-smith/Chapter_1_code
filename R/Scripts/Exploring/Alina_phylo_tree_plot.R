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

df <- read.csv("Data/bodysize_phylogeny_plot.csv")

#have a quick look at the data
glimpse(df)

df <- df %>% 
  select(., mean.mass, kingdom, genus)

length(unique(df$genus)) #981 genera

hist(log(df$mean.mass))

#Get phylo relationships from a list of taxa: ----

#match my names with taxa names in OTT (open tree taxonomy) (first check)
taxa <- tnrs_match_names(unique(df$genus))
head(taxa)

#this is where you go through the warnings

taxon_map <- structure(taxa$search_string, names = taxa$unique_name) 
#shows a map between OTL names and my names

unique(is.na(taxon_map)) #if false is great and we can keep going
#for now will keep going but need to check

#some taxa not in OTL so...
#Removing the taxa missing from the synthetic tree
in_tree <- is_in_tree(ott_id(taxa))
in_tree

sum(in_tree ==TRUE) #772 
sum(in_tree==FALSE) #198 Genus not in tree

#tree with only the taxa that are in the synthetic tree
tr <- tol_induced_subtree(ott_id(taxa)[in_tree])
#results as a phylo object
tol_about()
tr
class(tr)

plot(tr, show.tip.label = FALSE)

#check some tip labels
tr$tip.label[1:5]

#save tree out here before adapting it for plotting: 
write.nexus(tr, file="Data/tree_phylo_pre_plot.nex")



#remove the extra information from the tip labels (only shows the ones i used for the tree)
otl_tips <- strip_ott_ids(tr$tip.label, remove_underscores = TRUE)

head(otl_tips)


tr2 <- tr
#tr2$tip.label <- unname(taxon_map[otl_tips])

#use taxon map to replace the tip labels in the tree with the Genus names from dataset.
tr$tip.label <-  taxon_map[otl_tips] #older way

plot(tr, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tr, show.tip.label = FALSE)

####
tr$node.label<- NULL #remove node labels 


#genera actually in tree
taxon_map_used <- taxon_map[taxon_map %in% tr$tip.label]

#Plotting tree circular ----

df$genus <- tolower(df$genus)


#think this gives the data that actually has its Genus mapped into the tree
df4 <- df[df$genus %in% tr$tip.label, ]



table(df4$kingdom)

#get df with unique name used in otl and kingdom or mass
#make my Genus lower case to match a column from taxa search in rotl
# df4$Genus already like that

#get the 2 columns interested
taxa2<- taxa %>% select(., search_string, unique_name)
names(taxa2)[names(taxa2) == "search_string"] <- "genus"


#match them up 
df5 <- left_join(df4, taxa2, by = "genus")
df6 <- df5 %>% 
  select(., c("genus", "unique_name", "mean.mass", "kingdom"))

df6$tip.label <- df6$genus #using old names here to make figure

#table(df5$met_category)

df7 <- df6 %>% 
  group_by(tip.label) %>% 
  mutate(Mean_mass_uniq = mean(mean.mass), na.rm=TRUE)



#need to make tip.labels match
#df5$tip.label <- sub(" .*", "", df5$tip.label)


q <- ggtree(tr, branch.length='none', layout='circular') 
q
q <-  q %<+% df7 + geom_tippoint(aes(color = kingdom), 
                                 size=1, show.legend=TRUE) 
q

setdiff(df7$tip.label, tr$tip.label)


ggsave("Results/Tree_phylo_kingdom_colour.pdf", width = 7, height = 5, limitsize = FALSE)

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

ggsave("Results/Tree_phylo_king_mass.pdf", width = 7, height = 5, limitsize = FALSE)




