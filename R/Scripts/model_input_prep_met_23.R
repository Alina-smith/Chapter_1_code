#model input prep for Metabolism

#Penelope Blyth
#3/2/23

#formally:
#model_input_prep_met-22.R #formally phylo_met_tree_22


#Code to clean/organise/make the dataframes needed for brms models 1-3 for metabolism
#make phylo tree for metabolic data and get taxon info
#based on phylo_met_tree.R original code

#pkgs ----
#library(plyr)
#install.packages(c("treeplyr", "BiocManager"))
#library(BiocManager)
#install(c("ggtree","treeio"))

library(tidyverse)
library(ape)
library(phytools)
library(ggplot2)
library(rotl)
library(ggtree)
library(Rphylopars)
library(cowplot)
library(ggeffects)


# Check Data ----
df <- read.csv("../New_Data_Search/Data/combined_met_22_raw.csv")
#make sure this is the up to date combined csv


table(df$group, df$met_category)
table(df$met_category) 
length(unique(df$Genus))

#check all the data is there
unique(is.na(df$temp.C.all))
unique(is.na(df$mass.g))
unique(is.na(df$lntempMR))

#initial plot on temp corrected met rate (E=0.63)
explore_data <- ggplot(data = df, aes(x = lnMass, y = lntempMR, fill = met_category)) +
  geom_point(pch = 21, col = 'black', size = 2)+
  labs(x='ln(Mass) (g)', y = bquote("ln(I"~ e^{E/kT}~")  (W)")) +
  #scale_fill_viridis(discrete = T, option = 'viridis')+
  theme_bw() +
  theme(plot.margin=unit(c(1,1,1,1),"cm"))
#theme(legend.position = '')

explore_data
#looks like endo vert might be the only diff, but need to check temps on them as the diff of using body temp for endo might contribute to this


## checking for duplicated Genera in multi met groups ----

unique(df$group)
unique(df$Genus)

genbygroup <- unique(df[,c('group','Genus')]) #df of unique combinations of genera and met group

n_occur <- data.frame(table(genbygroup$Genus)) #number of times genus name occurs in that df, if only in 1 met group should =1
unique(n_occur$Freq) #shows some genera in multi groups

n_occur[n_occur$Freq > 1,] #which genera have multiples

genbygroup[genbygroup$Genus %in% n_occur$Var1[n_occur$Freq > 1],]

#only repeated genera between heterotrophic protozoa and unicells (so probs the same)



# Make Genus Level Tree ----

#match my names with taxa names in OTT (open tree taxonomy) (first check)
taxa <- tnrs_match_names(unique(df$Genus))
head(taxa)

class(taxa)
attributes(taxa)

##Dealing with warnings: ----

#aciculioiditermes, meloid are not matched and i couldn't find in gbif etc so remove
df <- df %>% 
  filter(., Genus != "Aciculioiditermes", 
         Genus != "Meloid")

##error from taxa:1: Some names were duplicated: ‘pseudophonus’. due to spellings, if fine
#df$Genus <- recode(df$Genus, "PSeudophonus" = "Pseudophonus")


### Some names were duplicated 

#‘pseudophonus’. due to spellings, so replace with correct spelling
#in this case is a synonym anyway so will replace with updated name

for (i in 1:length(df$Genus)){
  if (df$Genus[i] == "PSeudophonus" && df$species[i] =="rufipes"){
    df$name[i] ="Harpalus rufipes"
  }
  if (df$Genus[i] == "Pseudophonus" && df$species[i] =="rufipes"){
    df$name[i] ="Harpalus rufipes"
  }
  
}

df$Genus <- recode(df$Genus, "PSeudophonus" = "Harpalus")
df$Genus <- recode(df$Genus, "Pseudophonus" = "Harpalus")


#names as vector to check
genus_names <- as.vector(df$Genus)
genus_names_uni <- unique(genus_names)

#check difference between my input and the OTT output of names
setdiff(tolower(df$Genus), taxa$search_string)

#a couple not included probably due to special characters "aëdes" and "nausithoë"
df$Genus <- recode(df$Genus, "Aëdes" = "Aedes", "Nausithoë" = "Nausithoe")

#one Genus is called "unidentified so need to remove
df <- df %>% 
  filter(., Genus != "Unidentified")

#Remove if name is not to at least genus level
df <- df %>% 
  filter(., Genus != "Polydesmida", Genus != "Julidae", 
         Genus != "Opiliones", Genus != "Geophilidae")

# #### points to look at later (possibly to add in) ----
# #remove as cannot find class (can check later)
# df <- df %>% 
#   filter(., Genus != "Actinosphaerium")
# 
# #remove as OTL classes it wrong 
# #Platichthys as a microb when its a fish and Eisenia as a algae when a worm
# df <- df %>% 
#   filter(., Genus != "Platichthys", Genus != "Eisenia") 
# 
# ### end of points to check


## check ones that are only at genus level ----
table(df$group)

length(table(df$Genus[is.na(df$species)])) #number of genera with NA as species info in for some entries; 21
table(df$Genus[is.na(df$species)]) #number of na in species column per genera

#remove if no species level data
length(unique(df$Genus)) #1778

df2 <- df %>% 
  filter(., is.na(species) == FALSE)

length(unique(df2$Genus)) #1763


table(df2$group)
length(table(df2$Genus[is.na(df2$species)])) #number of genera with NA as species info in for some entries, should now be 0
table(df2$Genus[is.na(df2$species)]) #number of na in species column per genera


## Try matching names again in OTT ----

#names as vector to check
genus_names2 <- as.vector(df2$Genus)
genus_names_uni2 <- unique(genus_names2)

taxa2 <- tnrs_match_names(unique(df2$Genus))

head(taxa2)

unique(taxa2$number_matches) #NAs are now gone, but still some with multiple entries. 

length(taxa2$unique_name) #1763 Genera at this point. 


taxon_map <- structure(taxa2$search_string, names = taxa2$unique_name) 
#shows a map between OTL names and my names

taxon_map["Harpalus"] #get my genus name from OTT name
taxon_map["Aedes"] 

unique(is.na(taxon_map)) #if false is great and we can keep going

## Remove taxa not in OTL ----

#Removing the taxa missing from the synthetic tree
in_tree <- is_in_tree(ott_id(taxa2))
in_tree #1763 long as original 

sum(in_tree ==TRUE) #1428
sum(in_tree==FALSE) #335 Genus not in tree

#check flags
unique(taxa2$flags)
#might go back to these and sort out, this could be where a lot of my data points are lost. 
#if not in tree its either because 1) taxa is invalid or 2) not monophyletic
#to get around this need to remove taxa not in synthetic tree

## Make tree ----
#tree with only the taxa that are in the synthetic tree
tr <- tol_induced_subtree(ott_id(taxa2)[in_tree]) #still drops some... singleton node ones i think
#results as a phylo object
tol_about()
tr #1391 genera in tree
class(tr)

plot(tr, show.tip.label = FALSE)

#plot(tr, cex=.8, label.offset =.1, no.margin=TRUE)

#see tip labels for a few
tr$tip.label[1:5]

#save tree out here before adapting it for plotting: 
write.nexus(tr, file="Data/phylo_tree/Metabolism/tree_met.nex")


## assessing tip labels ----

#remove the extra information from the tip labels (only shows the ones i used for the tree)
otl_tips <- strip_ott_ids(tr$tip.label, remove_underscores = TRUE)

head(otl_tips)

#make copy of tree before alterations
orig_tr <- tr
#tr2$tip.label <- unname(taxon_map[otl_tips])

#use taxon map to replace the tip labels in the tree with the Genus names from dataset.
tr$tip.label <-  taxon_map[otl_tips] #older way

#plot(tr, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tr, show.tip.label = FALSE)

#remove node labels 
tr$node.label<- NULL 

#genera actually in tree
taxon_map_used <- taxon_map[taxon_map %in% tr$tip.label]
df2$Genus <- tolower(df2$Genus)


# Plotting tree ----

#think this gives the data that actually has its Genus mapped into the tree
df4 <- df2[df2$Genus %in% tr$tip.label, ]


table(df2$group, df2$met_category)
table(df4$group, df4$met_category)

table(df4$met_category)

#get df with unique name used in otl and met_category
#make my Genus lower case to match a column from taxa search in rotl
# df4$Genus already like that

#get the 2 columns interested
taxa3<- taxa2 %>% select(., search_string, unique_name, ott_id)
names(taxa3)[names(taxa3) == "search_string"] <- "Genus"


#match them up 
df5 <- left_join(df4, taxa3, by = "Genus")
df6 <- df5 %>% 
  select(., c("Genus", "unique_name", "met_category", "ott_id"))

df6$tip.label <- df6$Genus #using old names here to make figure

#table(df5$met_category)

df6<- unique(df6)

#need to make tip.labels match
#df5$tip.label <- sub(" .*", "", df5$tip.label)


q <- ggtree(tr, branch.length='none', layout='circular') 
q
q <-  q %<+% df6 + geom_tippoint(aes(color = met_category), 
                                 size=1, show.legend=TRUE) 

q <- q + scale_colour_manual(na.translate=F, name="Grouping", 
                             #values = c("#F8766D", "#00BA38", "#619CFF"),
                             values = c("#2c7bb6", "#fdae61", "#d7191c"),
                             labels = c("Ecto Invertebrate", "Ecto Vertebrate", "Endo Vertebrate"))
q

ggsave("Results/Metabolism/web_before_fix.jpeg", plot=q)

setdiff(df6$tip.label, tr$tip.label)


##################################
#from image seems like some have been miss-labeled
#would be good to compare taxonomy (e.g. phylum level) to met_category
#this would let us see if the genus input has been miss classified by OTL
##################################



#Check/fix issues with the tree ----

## make sure correct genus info retrieved by otl ----

#need to check that when there are multiple results for a genus name going in that
#the correct one is used to get ottid. 


#get genus that returns 2 or more number_matches
taxa_multi <- taxa2 %>% 
  filter(., number_matches >1)
#out of 1763, 362 have multiple matches....

#make sure getting the correct ottid/tree info for our genera
#can look at the met_category and original data to determine correct one. 

#How to change the ott ids assigned to my taxa?
#look at taxa2 which has full list and taxa_multi_intree which is the multi ottid/issue ones

#however the reason some might not be in tree could be due to this (an improvement would be to examine all). 
#now remove ones not in the tree/filter for ones in the tree:
taxa_multi_intree <- taxa_multi[taxa_multi$search_string %in% df6$Genus, ]
#263


#can inspect on the subset and then update on the full
#need to re-pull them out of otl as it wasnt working otherwise
taxa_multi_resolved_names <-tnrs_match_names(taxa_multi_intree$search_string) 

setdiff(taxa_multi_intree$search_string, taxa_multi_resolved_names$search_string)
setdiff(taxa_multi_intree$ott_id, taxa_multi_resolved_names$ott_id)
setdiff(taxa_multi_intree$number_matches, taxa_multi_resolved_names$number_matches)
#no differences so can continue


#get the ones with multi potential ottids and compare to the rank got from otl
#idea <- taxonomy_output[taxonomy_output$Genus %in% taxa_multi_resolved_names$search_string,]



#have to go through each one and decide the correct ottid based on taxonomy
#will need to keep track of the changes as will probably need to update them on the full taxonomy tree

comparison_info <- inspect(taxa_multi_resolved_names, 
                           taxon_name = taxa_multi_resolved_names$search_string[31])

comparison_info

tax_hist <- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[1], include_lineage = TRUE)))
tax_hist
tax_hist2<- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[2], include_lineage = TRUE)))
tax_hist2
tax_hist3<- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[3], include_lineage = TRUE)))
tax_hist3
tax_hist4<- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[4], include_lineage = TRUE)))
tax_hist4
# tax_hist5<- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[5], include_lineage = TRUE)))
# tax_hist5
# tax_hist6<- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[6], include_lineage = TRUE)))
# tax_hist6


### corrections/cleaning to original data (df2) ----

### 1. remove / remove as no species level data ## remove by the species column not genus one. 
#1a:
#17 proaciculitermes has "sp. E" and "sp. A"
#20 perisphaeria has "sp."
# some of geotrupes "sp." and " sp." (28 but only some as there is a sp = spiniger given as well)
#30 anomala "sp."
#38 peristeptus "sp."
#39 Helius #no info on it at all (genus)
#44 diplocapsis "sp." and " sp."
#52 carinum "sp."
#56 promachus "sp. 2"
#65 euglyphis "sp."
#67 megalpyge "sp."
#118 reichebnachia "sp."
#125 aega "sp."
#128 hyperia "sp." #only some are sp. 
#132 pyrocypris "sp."
#Bathycalanus "sp." "sp. A" for some, not all. 
#197 paramecium "spp." (are more with sp names given for this genus)

#1b:
#21 pternonemobius #even though is pronemobius (line 5) remove as cannot find info on the sp given. 
#87 Micromys extinct (in rotl it is, not sure why as is least concern in reality but remove anyway)

#df2 is the working df for our first and second tree. will now alter to remove more missing species data

#part 1a remove species with no name given (from section above and search in df2)
listremove <- c("sp. E", "sp. A","sp. C","sp. B"," sp. A", " sp. C", " sp. E",
                           "sp.", " sp.", "sp. 2", "spp.", " sp. 1", " sp. 2")

df2_clean <- df2[!df2$species %in% listremove,]

#part 1b remove if sp doesn't have info or is extinct
df2_clean <- df2_clean %>% 
                filter(., Genus != "pternonemobius")

df2_clean <- df2_clean %>% 
  filter(., Genus != "micromys")

#now for future trees using df2_clean#


### 2. keep most the same but be aware of spelling (is corrected/sorted out in the unique_name column but still wrong in search)
#e.g. 9. menemeruns should be menemerus
#22 spelling ceuthophilis should be Ceuthophilus 
#32 Sisiyphys should be Sisyphus
#120 Euatideus is aetideus 
#154 Halitrehees should be Halitrephes
#175 pitupophis should be pituophis
#177 thamnodyastes should be Thamnodynastes
#216 Acrocephalus should be Acrocephalus
#221 casurarius should be casuarius 
#222 catharactus should be Catharacta (but is a synonymn anyway so gets changed to stercorarius)
#236 Loxoides spelling should be Loxioides (but no issue)

#ones to alter:
#need to make sure Genus, species and name all changed correctly

#156 Nausithoë rubra (just need name changed as genus and sp sorted)
df2_clean[df2_clean$name == "Nausithoë rubra", "name"] <- "Nausithoe rubra"

#Aëdes campestris
df2_clean[df2_clean$name == "Aëdes campestris", "name"] <- "Aedes campestris"

#Eisenia foetida needs changing to Eisenia fetida is accepted name
df2_clean[df2_clean$name == "Eisenia foetida", "name"] <- "Eisenia fetida"
df2_clean[df2_clean$name == "Eisenia fetida", "species"] <- "fetida"

#Eisenia rosea to Aporrectodea rosea
df2_clean[df2_clean$name == "Eisenia rosea", "name"] <- "Aporrectodea rosea"
df2_clean[df2_clean$name == "Aporrectodea rosea", "Genus"] <- "aporrectodea"


#159 Chiromantis petersi sp name should be petersii
df2_clean[df2_clean$name == "Chiromantis petersi", "name"] <- "Chiromantis petersii"
df2_clean[df2_clean$name == "Chiromantis petersii", "species"] <- "petersii"

#174 Phrynosoma mcallii (currently wrong with m'calli)
df2_clean[df2_clean$name == "Phrynosoma m'calli", "name"] <- "Phrynosoma mcallii"
df2_clean[df2_clean$name == "Phrynosoma mcallii", "species"] <- "mcallii"



### 3. specifics to check ###
#150 genus Uca; think the species names are correct but belong to different genera within family Ocypodidae
#Uca leptodactyla should be Leptuca leptodactyla
df2_clean[df2_clean$name == "Uca leptodactyla", "name"] <- "Leptuca leptodactyla"
df2_clean[df2_clean$name == "Leptuca leptodactyla", "Genus"] <- "leptuca"
#Uca thayeri to Leptuca thayeri
df2_clean[df2_clean$name == "Uca thayeri", "name"] <- "Leptuca thayeri"
df2_clean[df2_clean$name == "Leptuca thayeri", "Genus"] <- "leptuca"
#Uca pugilator to Leptuca pugilator
df2_clean[df2_clean$name == "Uca pugilator", "name"] <- "Leptuca pugilator"
df2_clean[df2_clean$name == "Leptuca pugilator", "Genus"] <- "leptuca"
#Uca minax to Minuca minax
df2_clean[df2_clean$name == "Uca minax", "name"] <- "Minuca minax"
df2_clean[df2_clean$name == "Minuca minax", "Genus"] <- "minuca"
#Uca mordax to Minuca mordax
df2_clean[df2_clean$name == "Uca mordax", "name"] <- "Minuca mordax"
df2_clean[df2_clean$name == "Minuca mordax", "Genus"] <- "minuca"
#Uca pugnas to Minuca pugnax
df2_clean[df2_clean$name == "Uca pugnas", "name"] <- "Minuca pugnax"
df2_clean[df2_clean$name == "Minuca pugnax", "Genus"] <- "minuca"
df2_clean[df2_clean$name == "Minuca pugnax", "species"] <- "pugnax"
#Uca rapax to Minuca rapax
df2_clean[df2_clean$name == "Uca rapax", "name"] <- "Minuca rapax"
df2_clean[df2_clean$name == "Minuca rapax", "Genus"] <- "minuca"

#166 Anarbylus switaki, accepted name now Coleonyx switaki but didn't come up as option but is in otis (ott677279 for genus i think)
df2_clean[df2_clean$name == "Anarbylus switaki", "name"] <- "Coleonyx switaki"
df2_clean[df2_clean$name == "Coleonyx switaki", "Genus"] <- "coleonyx"

#168 Cnemidophorus tigris, accepted name now Aspidoscelis tigris (but the cnemidophorus murinus is fine)
df2_clean[df2_clean$name == "Cnemidophorus tigris", "name"] <- "Aspidoscelis tigris"
df2_clean[df2_clean$name == "Aspidoscelis tigris", "Genus"] <- "aspidoscelis"

#171 Lichanura roseofusca is old name for Lichanura trivirgata
df2_clean[df2_clean$name == "Lichanura roseofusca", "name"] <- "Lichanura trivirgata"
df2_clean[df2_clean$name == "Lichanura trivirgata", "species"] <- "trivirgata"

#195 Sarda chiliensis lineolata should be Sarda lineolata
df2_clean[df2_clean$name == "Sarda chiliensis lineolata", "name"] <- "Sarda lineolata"
df2_clean[df2_clean$name == "Sarda lineolata", "species"] <- "lineolata"

#200 Tetrahymena geleii and Tetrahymena geleii (pyriformis) should be Tetrahymena pyriformis (synonymn thing)
df2_clean[df2_clean$name == "Tetrahymena geleii", "name"] <- "Tetrahymena pyriformis"
df2_clean[df2_clean$name == "Tetrahymena geleii (pyriformis)", "name"] <- "Tetrahymena pyriformis"
df2_clean[df2_clean$name == "Tetrahymena pyriformis", "species"] <- "pyriformis"

#201 Lagurus curtatus needs changing to Lemmiscus curtatus (the accepted name, this one in itis, some might need to check gbif to ref)
df2_clean[df2_clean$name == "Lagurus curtatus", "name"] <- "Lemmiscus curtatus"
df2_clean[df2_clean$name == "Lemmiscus curtatus", "Genus"] <- "lemmiscus"

#209 Spinus (Carduelus) pinus and Spinus (Carduelus) tristis need the (Carduelus) removed
df2_clean[df2_clean$name == "Spinus (Carduelus) pinus", "name"] <- "Spinus pinus"
df2_clean[df2_clean$name == "Spinus pinus", "species"] <- "pinus"
df2_clean[df2_clean$name == "Spinus (Carduelus) tristis", "name"] <- "Spinus tristis"
df2_clean[df2_clean$name == "Spinus tristis", "species"] <- "tristis"

#211 Wilsonia citrina needs changing to Setophaga citrina (think the genus ott would be 285198)
df2_clean[df2_clean$name == "Wilsonia citrina", "name"] <- "Setophaga citrina"
df2_clean[df2_clean$name == "Setophaga citrina", "Genus"] <- "setophaga"

#214 Acanthis cannabina needs changing to Linaria cannabina (but Acanthis flammea is fine)
df2_clean[df2_clean$name == "Acanthis cannabina", "name"] <- "Linaria cannabina"
df2_clean[df2_clean$name == "Linaria cannabina", "Genus"] <- "linaria"

#219 Authus needs changing to Anthus as otherwise gives wrong tree info/names!  
df2_clean[df2_clean$name == "Authus pratensis", "name"] <- "Anthus pratensis"
df2_clean[df2_clean$name == "Anthus pratensis", "Genus"] <- "anthus"

#227 Coleus monedula needs changing to Corvus monedula (is a synonymn of the actual correct spelling coloeus but is in the tree)
df2_clean[df2_clean$name == "Coleus monedula", "name"] <- "Corvus monedula"
df2_clean[df2_clean$name == "Corvus monedula", "Genus"] <- "corvus"

df2_clean[df2_clean$name == "Corvus corone cornix", "name"] <- "Corvus cornix"
df2_clean[df2_clean$name == "Corvus cornix", "species"] <- "cornix"

#229 (3 sp, 2 need synonymn changes in names) Diomedea chrysostoma change to Thalassarche chrysostoma / Diomedea immutabilis change to Phoebastria immutabilis
df2_clean[df2_clean$name == "Diomedea chrysostoma", "name"] <- "Thalassarche chrysostoma"
df2_clean[df2_clean$name == "Thalassarche chrysostoma", "Genus"] <- "thalassarche"
df2_clean[df2_clean$name == "Diomedea immutabilis", "name"] <- "Phoebastria immutabilis"
df2_clean[df2_clean$name == "Phoebastria immutabilis", "Genus"] <- "phoebastria"

#231 Eudocimus albus (Guara alba) need to remove the (Guara alba) part from genus and name
df2_clean[df2_clean$name == "Eudocimus albus (Guara alba)", "name"] <- "Eudocimus albus"
df2_clean[df2_clean$name == "Eudocimus albus", "species"] <- "albus"

#250 lasiurus; Lasiurus cinereus --> Aeorestes cinereus / Lasiurus intermedius --> Dasypterus intermedius
df2_clean[df2_clean$name == "Lasiurus cinereus", "name"] <- "Aeorestes cinereus"
df2_clean[df2_clean$name == "Aeorestes cinereus", "Genus"] <- "aeorestes"

df2_clean[df2_clean$name == "Lasiurus intermedius", "name"] <- "Dasypterus intermedius"
df2_clean[df2_clean$name == "Dasypterus intermedius", "Genus"] <- "dasypterus"

#130 pontogeneia not given as option but accepted name of my species is Gondogeneia antarctica so new genus gondogeneia is ottid (1075607)
df2_clean[df2_clean$name == "Pontogeneia antarctica", "name"] <- "Gondogeneia antarctica"
df2_clean[df2_clean$name == "Gondogeneia antarctica", "Genus"] <- "gondogeneia"

#Cornus ruficollis (plant) should be Corvus ruficollis (bird)
df2_clean[df2_clean$name == "Cornus ruficollis", "name"] <- "Corvus ruficollis"
df2_clean[df2_clean$name == "Corvus ruficollis", "Genus"] <- "corvus"

#Cleistosoma edwardsii needs to be Danielella edwardsii
df2_clean[df2_clean$name == "Cleistosoma edwardsii", "name"] <- "Danielella edwardsii"
df2_clean[df2_clean$name == "Danielella edwardsii", "Genus"] <- "danielella"

######
#Astasia longa is in otl as its synonymn Euglena longa so needs changing but the family and order should stay the same. 
df2_clean[df2_clean$name == "Astasia longa", "name"] <- "Euglena longa"
df2_clean[df2_clean$name == "Euglena longa", "Genus"] <- "euglena"

#Crithidia (Strigomonas) oncopelti needs to be Strigomonas oncopelti; order = Trypanosomatida, family =Trypanosomatidae
df2_clean[df2_clean$name == "Crithidia (Strigomonas) oncopelti", "name"] <- "Strigomonas oncopelti"
df2_clean[df2_clean$name == "Strigomonas oncopelti", "Genus"] <- "strigomonas"
df2_clean[df2_clean$name == "Strigomonas oncopelti", "species"] <- "oncopelti"

#Eimeria stiedae needs to be Eimeria stiedai
df2_clean[df2_clean$name == "Eimeria stiedae", "name"] <- "Eimeria stiedai"
df2_clean[df2_clean$name == "Eimeria stiedai", "species"] <- "stiedai"

#Schizotrypanum verpertilionis should be Trypanosoma vespertilionis #884651
df2_clean[df2_clean$name == "Schizotrypanum verpertilionis", "name"] <- "Trypanosoma vespertilionis"
df2_clean[df2_clean$name == "Trypanosoma vespertilionis", "Genus"] <- "trypanosoma"
df2_clean[df2_clean$name == "Trypanosoma vespertilionis", "species"] <- "vespertilionis"

#Trypanosoma (Schizotrypanum) cruzi should be Trypanosoma cruzi
df2_clean[df2_clean$name == "Trypanosoma (Schizotrypanum) cruzi", "name"] <- "Trypanosoma cruzi"
df2_clean[df2_clean$name == "Trypanosoma cruzi", "Genus"] <- "trypanosoma"
df2_clean[df2_clean$name == "Trypanosoma cruzi", "species"] <- "cruzi"

########
#finding other ones with ()

#Acanthamoeba (Hartmanella) castellani
df2_clean[df2_clean$name == "Acanthamoeba (Hartmanella) castellani", "name"] <- "Acanthamoeba castellani"
df2_clean[df2_clean$name == "Acanthamoeba castellani", "Genus"] <- "acanthamoeba"
df2_clean[df2_clean$name == "Acanthamoeba castellani", "species"] <- "castellani"

#Meliphaga (Lichenostomus) virescens otl uses synonymn Gavicalis virescens
df2_clean[df2_clean$name == "Meliphaga (Lichenostomus) virescens", "name"] <- "Gavicalis virescens"
df2_clean[df2_clean$name == "Gavicalis virescens", "Genus"] <- "gavicalis"
df2_clean[df2_clean$name == "Gavicalis virescens", "species"] <- "virescens"

#Crithida (Strigomonas) fasciculata
df2_clean[df2_clean$name == "Crithida (Strigomonas) fasciculata", "name"] <- "Crithidia fasciculata"
df2_clean[df2_clean$name == "Crithidia fasciculata", "Genus"] <- "crithidia"
df2_clean[df2_clean$name == "Crithidia fasciculata", "species"] <- "fasciculata"

#Trichomonas (Tritrichomonas) foetus
df2_clean[df2_clean$name == "Trichomonas (Tritrichomonas) foetus", "name"] <- "Tritrichomonas foetus"
df2_clean[df2_clean$name == "Tritrichomonas foetus", "Genus"] <- "tritrichomonas"
df2_clean[df2_clean$name == "Tritrichomonas foetus", "species"] <- "foetus"

#Trichomonas foetus needs to be Tritrichomonas foetus
df2_clean[df2_clean$name == "Trichomonas foetus", "name"] <- "Tritrichomonas foetus"
df2_clean[df2_clean$name == "Tritrichomonas foetus", "Genus"] <- "tritrichomonas"
df2_clean[df2_clean$name == "Tritrichomonas foetus", "species"] <- "foetus"


#4.
#also need to check between repeats between ehnes and makarieva! 
#might effect line numbers so have genus names down as well just incase

#Indexes of duplicated rows: 
duplicate_df <- which(duplicated(df2_clean[c('Genus','species', 'W',"temp.C.all", "mass.g")]),) 
duplicate_df 

#Get data without duplicates. 
df2_clean_up <- df2_clean[!duplicated(df2_clean[c('Genus','species', 'W', "temp.C.all","mass.g")]),] 
#View(new_uniq_df) 

table(df2_clean$group)
table(df2_clean_up$group)

table(df2_clean_up$group, df2_clean_up$met_category)



### changes to tree/ottid by choosing correct id from number_multiple ----

## we can use the ott_id to replace this taxon: e.g.
## resolved_names <- update(resolved_names, taxon_name = "xxx",
##                          new_ott_id = 12345)
#or line number 
# resolved_names <- update(resolved_names,
#                          taxon_name = "diadema",
#                          new_row_number = 2
# )

#update list of taxa 
resolved_names <- tnrs_match_names(unique(df2_clean_up$Genus))


###5. ones to change ###
#update ottid for the ones with multi entries

#5.eisenia line 2 (316447) #is worm not algae
resolved_names_update <- update(resolved_names,
                      taxon_name = "eisenia",
                      new_ott_id = 316447) 

#11. isotoma line 2 (525309) #is collembola
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "isotoma",
                                new_ott_id = 525309) 

#13 hermannia line 2 (856695) #is a acari
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "hermannia",
                                new_ott_id = 856695) 

#14 eupterotegaeus line 2 (4703636) #is a acari
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "eupterotegaeus",
                                new_ott_id = 4703636) 

#19 leucophaea line 2 (839587) #correct synonym 
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "leucophaea",
                                new_ott_id = 839587) 

#27 corydalis line 2 (1062257) #Megaloptera
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "corydalis",
                                new_ott_id = 1062257) 

#28 sphaeridium line 2 (474937)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "sphaeridium",
                                new_ott_id = 474937) 

#36 adesmia line 2 (5337753) #coleoptera
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "adesmia",
                                new_ott_id = 5337753) 

#41 taeinotes line 4 (3378849) #is a spelling issue but need line 4 Taeniotes (coleoptera/Cerambycidae)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "taeinotes",
                                new_ott_id = 3378849) 

#45 smicronyx line 2 (4613041)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "smicronyx",
                                new_ott_id = 4613041)

#46 calandra line 2 (865235) #synonymn correct is sitophilus
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "calandra",
                                new_ott_id = 865235) 

#51 evarthrus line 2 (1075948) #synonymn correct is cyclotrachelus
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "evarthrus",
                                new_ott_id = 1075948) 

#54 campalita line 2 (407045) #synonymn correct is calosoma
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "campalita",
                                new_ott_id = 407045) 

#61 thais line 2 (725020)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "thais",
                                new_ott_id = 725020) 

#75 echinops line 2  (222356)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "echinops",
                                new_ott_id = 222356) 

#80 aotus line 2 (791708)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "aotus",
                                new_ott_id = 791708) 

#84 malacothrix line 2 (600707)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "malacothrix",
                                new_ott_id = 600707) 

#101 macroderma line 2 (289140)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "macroderma",
                                new_ott_id = 289140) 

#109 galerella line 2 (111026)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "galerella",
                                new_ott_id = 111026) 

#122 gaussia line 2 (955892)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "gaussia",
                                new_ott_id = 955892) 

#124 pachyptilus line 2 (2956539)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "pachyptilus",
                                new_ott_id = 2956539) 

#127 byblis line 2 (166903)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "byblis",
                                new_ott_id = 166903) 

#131 waldeckia line 2 (5335552)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "waldeckia",
                                new_ott_id = 5335552) 

#145 parathelphusa line 3 (5755399)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "parathelphusa",
                                new_ott_id = 5755399) 

#147 sclerocrangon ferox line 2 (1005756)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "sclerocrangon",
                                new_ott_id = 1005756) 

#173 morelia line 2 (717652)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "morelia",
                                new_ott_id = 717652) 

#176 salvadora line 2 (335529)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "salvadora",
                                new_ott_id = 335529) 

#193 orthodon line 3 (319965)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "orthodon",
                                new_ott_id = 319965) 

#194 platichthys line 2 (883417)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "platichthys",
                                new_ott_id = 883417) 


# #201 lagurus line 2 (1073073) #not in anymore
# resolved_names_update <- update(resolved_names_update,
#                                 taxon_name = "lagurus",
#                                 new_ott_id = 1073073) 

#210 zonotrichia line 2 (789032)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "zonotrichia",
                                new_ott_id = 789032) 

#212 scardafella line 2 (935135) as is synonymn 
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "scardafella",
                                new_ott_id = 935135) 

#218 arenaria line 2 (821756)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "arenaria",
                                new_ott_id = 821756) 

#225 chloris line 2 (3597427)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "chloris",
                                new_ott_id = 3597427) 

#230 eremophila line 2 (420987)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "eremophila",
                                new_ott_id = 420987) 

#234 glaucidium line 2 (1070587)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "glaucidium",
                                new_ott_id = 1070587) 

#240 prunella line 2 (699626)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "prunella",
                                new_ott_id = 699626) 

#242 scardefella line 2 (935135) (like 212 but spelling is different to scardafella)
resolved_names_update <- update(resolved_names_update,
                                 taxon_name = "scardefella",
                                 new_ott_id = 935135) 

#245 zonotricha line 2 (789032)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "zonotricha",
                                new_ott_id = 789032) 

#250 lasiurus line 2 (447954) 
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "lasiurus",
                                new_ott_id = 447954) 

#251 cebuella line 2 (1015231) #but might be broken node and think line 1 worked. (synonymn issue)
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "cebuella",
                                new_ott_id = 1015231) 



#ones that have been altered above from the original ottid search: 
altered<- c("eisenia","isotoma","hermannia","eupterotegaeus", "leucophaea",
          "corydalis","sphaeridium", "adesmia", "taeinotes", "smicronyx",
          "calandra", "evarthrus", "campalita", "thais", "echinops", "aotus", 
          "malacothrix", "macroderma", "galerella", "gaussia", "pachyptilus",
          "byblis","waldeckia","parathelphusa","sclerocrangon","morelia",
          "salvadora","orthodon","platichthys","lagurus","zonotrichia",
          "scardafella","arenaria","chloris","eremophila","glaucidium", 
          "prunella","scardefella","zonotricha","lasiurus","cebuella")


#ones from original ottid search that returned multiple ottid numbers but haven't been adjusted
to_keep_1 <- taxa_multi_resolved_names[!(taxa_multi_resolved_names$search_string %in% altered),]

#however in the df2_clean_up some of these have potentially been removed or names altered. 
resolved_names_update_check<- resolved_names_update[(resolved_names_update$search_string %in% to_keep_1$search_string),]

setdiff( to_keep_1$search_string, resolved_names_update_check$search_string)
#20 genera are gone now compared to when i checked for multiple entry number ottids per genus. 

#update to make sure these are ottid for the first line from the options:
for (i in 1:length(resolved_names_update_check$search_string)){
  resolved_names_update <- update(resolved_names_update,
                                  taxon_name = resolved_names_update_check$search_string[i],
                                  new_row_number = 1)
  
}


################################

#for ones that were updated in name during the df2_clean_up process

check_afterclean <- c("leptuca", "minuca", "coleonyx", "aspidoscelis", "lemmiscus", 
           "setophaga", "linaria", "anthus", "corvus", "thalassarche", 
           "phoebastria", "aeorestes", "dasypterus", "gondogeneia", "danielella", 
           "euglena", "strigomonas", "eimeria", "trypanosoma",
           "acanthamoeba", "gavicalis", "crithidia", "tritrichomonas")


#check their ottid/if they have multiple choose correct one
comparison_info <- inspect(resolved_names_update, 
                           taxon_name = check_afterclean[23])

comparison_info

tax_hist <- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[1], include_lineage = TRUE)))
tax_hist
# tax_hist2 <- as.data.frame(tax_lineage(taxonomy_taxon_info(comparison_info$ott_id[2], include_lineage = TRUE)))
# tax_hist2

#linaria line 2, need to update this:
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "linaria",
                                new_row_number = 2)

#euglena line 2, need to update this:
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "euglena",
                                new_row_number = 2)

#trypanosoma line 2, need to update this:
resolved_names_update <- update(resolved_names_update,
                                taxon_name = "trypanosoma",
                                new_row_number = 2)

#all the others can go with line 1
check_afterclean2 <- c("leptuca", "minuca", "coleonyx", "aspidoscelis", "lemmiscus", 
                      "setophaga", "anthus", "corvus", "thalassarche", 
                      "phoebastria", "aeorestes", "dasypterus", "gondogeneia", "danielella",
                      "strigomonas", "eimeria", "acanthamoeba", "gavicalis", "crithidia", "tritrichomonas")

for (i in 1:length(check_afterclean2)){
  resolved_names_update <- update(resolved_names_update,
                                  taxon_name = check_afterclean2[i],
                                  new_row_number = 1)
  
}



#make new tree


#Removing the taxa missing from the synthetic tree
in_tree_update <- is_in_tree(ott_id(resolved_names_update))
in_tree_update #1763 original, now 1709

sum(in_tree_update ==TRUE) #1428, now 1377
sum(in_tree_update ==FALSE) #335 Genus not in tree now 332

#check flags
unique(resolved_names_update$flags)
#might go back to these and sort out, this could be where a lot of my data points are lost. 
#if not in tree its either because 1) taxa is invalid or 2) not monophyletic
#to get around this need to remove taxa not in synthetic tree

## Make tree ----
#tree with only the taxa that are in the synthetic tree
tr_update <- tol_induced_subtree(ott_id(resolved_names_update)[in_tree_update]) #still drops some... singleton node ones i think
#results as a phylo object
tol_about()
tr_update #1336 genera in tree
class(tr_update)

plot(tr_update, show.tip.label = FALSE)

#plot(tr, cex=.8, label.offset =.1, no.margin=TRUE)

#see tip labels for a few
tr_update$tip.label[1:5]



## assessing tip labels for updated tree (corrected ottid) ----

taxon_map_update <- structure(resolved_names_update$search_string, names = resolved_names_update$unique_name) 
#shows a map between OTL names and my names

taxon_map_update["Harpalus"] #get my genus name from OTT name

unique(is.na(taxon_map_update)) #if false is great and we can keep going






#remove the extra information from the tip labels (only shows the ones i used for the tree)
otl_tips_update <- strip_ott_ids(tr_update$tip.label, remove_underscores = TRUE)

head(otl_tips_update)

#make copy of tree before alterations
orig_tr_update <- tr_update
#tr2$tip.label <- unname(taxon_map[otl_tips])
#1336 genera kept in tree

write.nexus(orig_tr_update, file="Data/phylo_tree/Metabolism/tree_met_correct_ottid.nex")
#write out tree before adjusting tip lables or plotting

#use taxon map to replace the tip labels in the tree with the Genus names from dataset.
tr_update$tip.label <-  taxon_map_update[otl_tips_update] #older way

#plot(tr, cex=.8, label.offset =.1, no.margin=TRUE)
plot(tr, show.tip.label = FALSE)

#remove node labels 
tr_update$node.label<- NULL 

#genera actually in tree
taxon_map_used_update <- taxon_map_update[taxon_map_update %in% tr_update$tip.label]
df2_clean_up$Genus <- tolower(df2_clean_up$Genus)


## Plotting tree (corrected ottid) ----
#plot circle tree to check for other potential errors

#think this gives the data that actually has its Genus mapped into the tree
df4_up <- df2_clean_up[df2_clean_up$Genus %in% tr_update$tip.label, ]
unique(df4_up$Genus)

table(df2_clean_up$group, df2_clean_up$met_category)
table(df4_up$group, df4_up$met_category)

table(df4_up$met_category)

#get df with unique name used in otl and met_category
#make my Genus lower case to match a column from taxa search in rotl
# df4$Genus already like that

#get the 2 columns interested
resolved_names_update2<- resolved_names_update %>% select(., search_string, unique_name, ott_id)
names(resolved_names_update2)[names(resolved_names_update2) == "search_string"] <- "Genus"


#match them up 
df5_up <- left_join(df4_up, resolved_names_update2, by = "Genus")
unique(df5_up$Genus)
df6_up <- df5_up %>% 
  select(., c("Genus", "unique_name", "met_category", "ott_id"))

df6_up$tip.label <- df6_up$Genus #using old names here to make figure

df6_up<- unique(df6_up)
table(df6_up$met_category)

#need to make tip.labels match
#df5$tip.label <- sub(" .*", "", df5$tip.label)


q_up <- ggtree(tr_update, branch.length='none', layout='circular') 
q_up
q_up <-  q_up %<+% df6_up + geom_tippoint(aes(color = met_category), 
                                 size=1, show.legend=TRUE) 

q_up <- q_up + scale_colour_manual(na.translate=F, name="Grouping", 
                             #values = c("#F8766D", "#00BA38", "#619CFF"),
                             values = c("#2c7bb6", "#fdae61", "#d7191c"),
                             labels = c("Ecto Invertebrate", "Ecto Vertebrate", "Endo Vertebrate"))
q_up


ggsave("Results/Metabolism/web_after_ottid_fix.jpeg", plot=q_up)

# tiff("Results/BES_Poster/web_after_ottid_fix_metabolism.tiff", units="cm", width=40, height=40, res=300)
# q_up
# dev.off()

setdiff(df6_up$tip.label, tr_update$tip.label)


plot_grid(q,q_up)


#get taxonomic info from tree and sort out ones that give multiple values back per taxonomic group (itis/gbif?)


#notes 
#251 is odd as Cebuella pygmaea is in tree as Cebuella genus but also called callithrix pygmaea on tree
#will leave as this might be sorted when imputating
#some like 174/177 have 2 orders written down still. would need to select out correct one
#seems like that for alot of the reptiles
#some seem to be in odd orders e.g. fish some orders are classified as other taxonomic levels... might be due to the tree organisation e.g. Kuhlia sandvicensis
#198 plasmodium might struggle as no order/family info found
#might need to check these new genus names given as could have multi possible number_matches


#Get taxonomic relationships from ottid (initial attempt) ----

#get taxonomy info

#df6_up$ott_id

#taxonomy_taxon_info(5065738)

#make empty output df
taxonomy_output <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(taxonomy_output) <- c("family", "order", "class", "phylum","kingdom","domain","Genus", "unique_name", "met_category", "ott_id")
#Genus is from df6 and is the input for tree getting
str(taxonomy_output)

taxonomy_output <- taxonomy_output %>%
  mutate_all(as.character)

for (i in 1:length(df6_up$ott_id)){   #(df6_up$ott_id)
  #use ottid of genus to get higher taxonomic rank
  taxonomy <- as.data.frame(tax_lineage(taxonomy_taxon_info(df6_up$ott_id[i], include_lineage = TRUE)))
  colnames(taxonomy) <- c("rank", "name", "unique_name", "ott_id")
  
  #extract only the ranks of interest
  taxonomy2 <- taxonomy %>% 
    filter(.,rank =="domain" | rank =="kingdom"| rank == "phylum" | rank == "class"| rank == "order"|rank == "family")
  
  taxonomy3 <- taxonomy2 %>% 
    select(., rank, unique_name)
  
  #t(taxonomy3) #family, order, class, phylum
  
  #############
  ###need to convert row to columns and make sure it still works if there isn't all 4 taxons of info
  taxonomy4<- t(taxonomy3)
  rownames(taxonomy4)<- NULL
  colnames(taxonomy4) <- taxonomy4[1,]
  taxonomy4<- data.frame(taxonomy4)
  taxonomy4 <- (taxonomy4[-1,])
  #taxonomy4$family <- as.character(taxonomy4$family)
  
  taxonomy4$Genus <- as.character(df6_up$Genus[i])
  taxonomy4$unique_name <- as.character(df6_up$unique_name[i])
  taxonomy4$met_category <- as.character(df6_up$met_category[i])
  taxonomy4$ott_id <- as.character(df6_up$ott_id[i])
  #str(taxonomy4)
  
  #colnames(taxonomy4) <- c("family", "order", "class", "phylum", "Genus", "met_category", "ott_id")
  
  taxonomy_output <- bind_rows(taxonomy_output, taxonomy4)
  print(i)
}



#for search entry genera that returned >1 value for a taxon rank need to check 
#which to use... likely there were multiple entries and it picked the first
#this might not be the correct ottid for that genus e.g. if a fish and bacteria have the 
#same genus name it might return the tree/phylo relationship for the wrong organism. 

#some might also be misspelled 

#check which have multi rank info per rank
colnames(taxonomy_output)

#get ones that either have more than 1 entry per rank or don't have info (NA) for the main ranks
taxonomy_output2 <- taxonomy_output %>% 
  filter(., !is.na(order.1) | !is.na(order.2) | !is.na(order.3) | 
           is.na(family) | is.na(order) | is.na(class) |
           is.na(phylum) | is.na(kingdom))

taxonomy_output2 
length(taxonomy_output2$family) #385 after doing the ottid correction step earlier. (403 before)

#see that kingdom and domains are fine so can remove. 
#only going to family and order level so also get rid of phylum and class
taxonomy_output3 <- taxonomy_output2 %>% 
  select(., family, order, Genus, unique_name, met_category, ott_id, order.1, order.2,order.3)

taxonomy_output4 <- taxonomy_output3 %>% 
  filter(., !is.na(order.1) | !is.na(order.2) | !is.na(order.3) | 
           is.na(family) | is.na(order))


#need to check out NAs for family... can then go through familys and sort out order etc. 
taxonomy_output5 <- taxonomy_output4 %>% 
  filter(., is.na(family) ==TRUE | is.na(order)==TRUE)

#get columns needed and fill in/correct family/order level missing data.
#this is the new df i'm working on/changes will be done to!
taxonomy_output_clean <- taxonomy_output %>% 
  select(., family, order, Genus, unique_name, met_category, ott_id)

taxonomy_output_clean$ott_id <- as.numeric(taxonomy_output_clean$ott_id)
str(taxonomy_output_clean)

#when finding family am looking at the genus/spp level info from original data and then checking otl. 


taxonomy_output_clean[taxonomy_output_clean$ott_id == 222356, "order"]<- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 922732, "order"]<- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 542053, "order"]<- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 542050, "order"]<- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 542058, "order"]<- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 680966, "order"]<- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 680970, "order"]<- "Afrosoricida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 574738, "order"] <- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 1094520, "order"] <- "Afrosoricida"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 672747, "order"] <- "Afrosoricida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 1073325, "order"] <- "Euglenida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 250601, "order"] <- "Amoebida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 4806067, "order"] <-  "Suctorida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 1043395, "family"] <- "Lepidogalaxiidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 120249, "family"]<- "Acanthamoebidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 446830, "family"]<- "Actinophryidae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 446830, "order"]<- "Stramenopiles"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 833446, "family"]<- "Euglenaceae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 833446, "order"]<- "Euglenida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 546638, "family"]<- "Colpodidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 775249, "family"]<- "Eimeriidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 796511, "family"]<- "Trypanosomatidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 919495, "family"]<- "Frontoniidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 568126, "family"]<- "Parameciidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 474836, "family"]<- "Plasmodiidae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 474836, "order"]<- "Haemospororida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 493861, "family"]<- "Spirostomidae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 493861, "order"]<- "Heterotrichida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 703498, "family"]<- "Stentoridae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 703498, "order"]<- "Heterotrichida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 47284, "family"]<- "Tetrahymenidae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 47284, "order"]<- "Hymenostomatida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 960883, "family"]<- "Trichosidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 1060050, "family"]<- "Urostylidae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 1060050, "order"]<- "Urostylida"


taxonomy_output_clean[taxonomy_output_clean$ott_id == 172642, "family"]<- "Hydrobatidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 745847, "family"]<- "Trichomonadidae"
taxonomy_output_clean[taxonomy_output_clean$ott_id == 745847, "order"]<- "Trichomonadida"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 192258, "family"]<- "Trypanosomatidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 154607, "family"]<- "Trypanosomatidae"

taxonomy_output_clean[taxonomy_output_clean$ott_id == 779558, "family"]<- "Trypanosomatidae"


#check 
taxonomy_output_clean %>% 
  filter(., is.na(family) ==TRUE | is.na(order) ==TRUE)




#check if the order.1-3 are extinct clades?
#looks more like some orders have been given wrong level info e.g. subfamily etc. 
#e.g.
tax_lineage(taxonomy_taxon_info(448493, include_lineage = TRUE))
#genus=coptotermes,unique_name=Coptotermes, order= Termitoidae, order.1=Blattodea

#need to see what is miss classified and also what needs extra ranks looking up
unique(taxonomy_output4$order)
unique(taxonomy_output4$family)

family_list <- unique(taxonomy_output4$family)
#go through the family levels and assign correct order based off itis/other background info

#if unsure/orders give by otl are different to other sources i stick with the first order the tree suggests as then its consistent with the other taxa that i don't need to look at. 




for(i in 1:length(taxonomy_output_clean$family)){

  if (taxonomy_output_clean$family[i] == "Rhinotermitidae" |
      taxonomy_output_clean$family[i] == "Termitidae") {

     taxonomy_output_clean$order[i] = "Blattodea"
    #order = Blattodea
    #family = Rhinotermitidae, Termitidae
  }
  
  if (taxonomy_output_clean$family[i] == "Tenrecidae" | 
     taxonomy_output_clean$family[i] == "Chrysochloridae") {
     
     taxonomy_output_clean$order[i] = "Afrosoricida"
     #order	Afrosoricida
     #family = Tenrecidae, Chrysochloridae
  }  
   
  if (taxonomy_output_clean$family[i] == "Lacertidae" | taxonomy_output_clean$family[i] == "Elapidae" |
     taxonomy_output_clean$family[i] == "Scincidae" | taxonomy_output_clean$family[i] == "Boidae" |
       taxonomy_output_clean$family[i] == "Acrochordidae" | taxonomy_output_clean$family[i] == "Iguanidae" |
       taxonomy_output_clean$family[i] == "Agamidae" | taxonomy_output_clean$family[i] == "Eublepharidae" |
       taxonomy_output_clean$family[i] == "Anguidae" | taxonomy_output_clean$family[i] == "Anniellidae" |
       taxonomy_output_clean$family[i] == "Pythonidae" | taxonomy_output_clean$family[i] == "Blanidae" |
       taxonomy_output_clean$family[i] == "Teiidae" | taxonomy_output_clean$family[i] == "Colubridae" |
       taxonomy_output_clean$family[i] == "Viperidae" | taxonomy_output_clean$family[i] == "Dipsadidae" |
       taxonomy_output_clean$family[i] == "Trogonophidae" | taxonomy_output_clean$family[i] == "Phyllodactylidae" |
       taxonomy_output_clean$family[i] == "Sphaerodactylidae" | taxonomy_output_clean$family[i] == "Gekkonidae" |
       taxonomy_output_clean$family[i] == "Xantusiidae" | taxonomy_output_clean$family[i] == "Phrynosomatidae" |
       taxonomy_output_clean$family[i] == "Varanidae") {
         
       taxonomy_output_clean$order[i] = "Squamata (order in Deuterostomia)"
  }
     
  if (taxonomy_output_clean$family[i] == "Alligatoridae"){
     taxonomy_output_clean$order[i] = "Crocodilia"
  } 
   
  if (taxonomy_output_clean$family[i] == "Chelydridae" | taxonomy_output_clean$family[i] == "Emydidae" |
       taxonomy_output_clean$family[i] == "Testudinidae") {
     
     taxonomy_output_clean$order[i] = "Testudines"
  } 
  # 
  if (taxonomy_output_clean$family[i] == "Sphenodontidae") {
     taxonomy_output_clean$order[i] = "Rhynchocephalia"
  } 
   
  if (taxonomy_output_clean$family[i] == "Anarhichadidae" | taxonomy_output_clean$family[i] == "Serranidae" |
       taxonomy_output_clean$family[i] == "Channichthyidae" | taxonomy_output_clean$family[i] == "Trachinidae" |
       taxonomy_output_clean$family[i] == "Percidae" | taxonomy_output_clean$family[i] == "Nototheniidae" |
       taxonomy_output_clean$family[i] == "Bathydraconidae" | taxonomy_output_clean$family[i] == "Harpagiferidae" |
       taxonomy_output_clean$family[i] == "Sciaenidae" | taxonomy_output_clean$family[i] == "Zoarcidae" |
       taxonomy_output_clean$family[i] == "Gasterosteidae" | taxonomy_output_clean$family[i] == "Hexagrammidae" |
       taxonomy_output_clean$family[i] == "Sebastidae") {
     
       taxonomy_output_clean$order[i] = "Perciformes"
  } 

  if (taxonomy_output_clean$family[i] == "Blenniidae") {
    taxonomy_output_clean$order[i] = "Blenniiformes"
  }
  #
  if (taxonomy_output_clean$family[i] == "Sparidae") {
    taxonomy_output_clean$order[i] = "Spariformes"
  }

  if (taxonomy_output_clean$family[i] == "Labridae") {
    taxonomy_output_clean$order[i] = "Labriformes"
  }

  if (taxonomy_output_clean$family[i] == "Cichlidae") {
    taxonomy_output_clean$order[i] = "Cichliformes"
  }

  if (taxonomy_output_clean$family[i] == "Kyphosidae" | taxonomy_output_clean$family[i] == "Kuhliidae" |
      taxonomy_output_clean$family[i] == "Centrarchidae") {

    taxonomy_output_clean$order[i] = "Centrarchiformes"
  }

  if (taxonomy_output_clean$family[i] == "Gobiidae") {
    taxonomy_output_clean$order[i] = "Gobiiformes"
  }

  if (taxonomy_output_clean$family[i] == "Anabantidae" | taxonomy_output_clean$family[i] == "Channidae" |
      taxonomy_output_clean$family[i] == "Osphronemidae") {

    taxonomy_output_clean$order[i] = "Anabantiformes"
  }

  if (taxonomy_output_clean$family[i] == "Chiasmodontidae" |
      taxonomy_output_clean$family[i] == "Scombridae") {

    taxonomy_output_clean$order[i] = "Scombriformes"
  }

  if (taxonomy_output_clean$family[i] == "Coryphaenidae" |
      taxonomy_output_clean$family[i] == "Carangidae") {

    taxonomy_output_clean$order[i] = "Carangiformes"
  }

  if (taxonomy_output_clean$family[i] == "Uranoscopidae") {
    taxonomy_output_clean$order[i] = "Uranoscopiformes"
  }

  if (taxonomy_output_clean$family[i] == "Anoplogasteridae" |
      taxonomy_output_clean$family[i] == "Melamphaidae") {

    taxonomy_output_clean$order[i] = "Beryciformes"
  }

  if (taxonomy_output_clean$family[i] == "Aphaniidae") {
    taxonomy_output_clean$order[i] = "Cyprinodontiformes"
  }

  if (taxonomy_output_clean$family[i] == "Gadidae" | taxonomy_output_clean$family[i] == "Lotidae" |
      taxonomy_output_clean$family[i] == "Melanonidae") {

    taxonomy_output_clean$order[i] = "Gadiformes"
  }

  if (taxonomy_output_clean$family[i] == "Cyprinodontidae") {
    taxonomy_output_clean$order[i] = "Cyprinodontiformes"
  }

  if (taxonomy_output_clean$family[i] == "Carapidae") {
    taxonomy_output_clean$order[i] = "Ophidiiformes"
  }

  if (taxonomy_output_clean$family[i] == "Syngnathidae" |
      taxonomy_output_clean$family[i] == "Dactylopteridae") {

    taxonomy_output_clean$order[i] = "Syngnathiformes"
  }


  if (taxonomy_output_clean$family[i] == "Pleuronectidae") {
    taxonomy_output_clean$order[i] = "Pleuronectiformes"
  }

  if (taxonomy_output_clean$family[i] == "Petromyzontidae") {
    taxonomy_output_clean$order[i] = "Petromyzontiformes"
  }

  if (taxonomy_output_clean$family[i] == "Mugilidae") {
    taxonomy_output_clean$order[i] = "Mugiliformes"
  }

  if (taxonomy_output_clean$family[i] == "Melanocetidae") {
    taxonomy_output_clean$order[i] = "Lophiiformes"
  }

  if (taxonomy_output_clean$family[i] == "Synbranchidae") {
    taxonomy_output_clean$order[i] = "Synbranchiformes"
  }

  if (taxonomy_output_clean$family[i] == "Batrachoididae") {
    taxonomy_output_clean$order[i] = "Batrachoidiformes"
  }

  if (taxonomy_output_clean$family[i] == "Poeciliidae") {
    taxonomy_output_clean$order[i] = "Cyprinodontiformes"
  }

  if (taxonomy_output_clean$family[i] == "Pelomyxidae") {
    taxonomy_output_clean$order[i] = "Amoebida"
    #for family Pelomyxidae; (might have some issues here as genus should probs be chaos but otl isn't good with that)
  }

  if (taxonomy_output_clean$family[i] == "Podophryidae") {
    taxonomy_output_clean$order[i] = "Suctorida"
  }

  if (taxonomy_output_clean$family[i] == "Podargidae" | taxonomy_output_clean$family[i] == "Eurostopodidae" |
      taxonomy_output_clean$family[i] == "Caprimulgidae") {

    taxonomy_output_clean$order[i] = "Caprimulgiformes"
  }

  if (  taxonomy_output_clean$family[i] == "Aegithalidae" | taxonomy_output_clean$family[i] == "Corvidae" |
        taxonomy_output_clean$family[i] == "Estrildidae" | taxonomy_output_clean$family[i] == "Thraupidae" |
        taxonomy_output_clean$family[i] == "Turdidae" | taxonomy_output_clean$family[i] == "Paridae" |
        taxonomy_output_clean$family[i] == "Meliphagidae" | taxonomy_output_clean$family[i] == "Fringillidae" |
        taxonomy_output_clean$family[i] == "Icteridae" | taxonomy_output_clean$family[i] == "Pipridae" |
        taxonomy_output_clean$family[i] == "Passerellidae" | taxonomy_output_clean$family[i] == "Parulidae" |
        taxonomy_output_clean$family[i] == "Tyrannidae" | taxonomy_output_clean$family[i] == "Sylviidae" |
        taxonomy_output_clean$family[i] == "Nectariniidae" | taxonomy_output_clean$family[i] == "Alaudidae" |
        taxonomy_output_clean$family[i] == "Pycnonotidae" | taxonomy_output_clean$family[i] == "Bombycillidae" |
        taxonomy_output_clean$family[i] == "Irenidae"| taxonomy_output_clean$family[i] == "Cinclidae" |
        taxonomy_output_clean$family[i] == "Muscicapidae" | taxonomy_output_clean$family[i] == "Hirundinidae" |
        taxonomy_output_clean$family[i] == "Motacillidae" | taxonomy_output_clean$family[i] =="Passeridae" |
        taxonomy_output_clean$family[i] == "Thamnophilidae") {

    taxonomy_output_clean$order[i] = "Passeriformes"
  }

  if (taxonomy_output_clean$family[i] == "Trochilidae" |
      taxonomy_output_clean$family[i] == "Apodidae") {

    taxonomy_output_clean$order[i] = "Apodiformes"
  }

  if (taxonomy_output_clean$family[i] == "Phasianidae" | taxonomy_output_clean$family[i] == "Odontophoridae" |
      taxonomy_output_clean$family[i] == "Cracidae") {

    taxonomy_output_clean$order[i] = "Galliformes"
  }

  if (taxonomy_output_clean$family[i] == "Columbidae") {
    taxonomy_output_clean$order[i] = "Columbiformes"
  }

  if (taxonomy_output_clean$family[i] == "Pandionidae" | taxonomy_output_clean$family[i] == "Accipitridae" |
      taxonomy_output_clean$family[i] == "Cathartidae") {

    taxonomy_output_clean$order[i] = "Accipitriformes"
  }

  if (taxonomy_output_clean$family[i] == "Neomorphidae" | taxonomy_output_clean$family[i] == "Cuculidae" |
      taxonomy_output_clean$family[i] == "Centropidae") {

    taxonomy_output_clean$order[i] = "Cuculiformes"
  }

  if (taxonomy_output_clean$family[i] == "Spheniscidae") {
    taxonomy_output_clean$order[i] = "Sphenisciformes"
  }

  if (taxonomy_output_clean$family[i] == "Todidae" |
      taxonomy_output_clean$family[i] == "Meropidae") {

    taxonomy_output_clean$order[i] = "Coraciiformes"
  }

  if (taxonomy_output_clean$family[i] == "Strigidae") {
    taxonomy_output_clean$order[i] = "Strigiformes"
  }

  if (taxonomy_output_clean$family[i] == "Psittacidae" | taxonomy_output_clean$family[i] == "Cacatuidae" |
      taxonomy_output_clean$family[i] == "Psittaculidae") {

    taxonomy_output_clean$order[i] = "Psittaciformes"
  }

  if (taxonomy_output_clean$family[i] == "Anatidae" |
      taxonomy_output_clean$family[i] == "Anhimidae") {

    taxonomy_output_clean$order[i] = "Anseriformes"
  }

  if (taxonomy_output_clean$family[i] == "Apterygidae") {
    taxonomy_output_clean$order[i] = "Apterygiformes"
  }

  if (taxonomy_output_clean$family[i] == "Scolopacidae" | taxonomy_output_clean$family[i] == "Stercorariidae" |
      taxonomy_output_clean$family[i] == "Haematopodidae" | taxonomy_output_clean$family[i] == "Charadriidae" |
      taxonomy_output_clean$family[i] == "Laridae" | taxonomy_output_clean$family[i] == "Turnicidae" |
      taxonomy_output_clean$family[i] == "Alcidae") {

    taxonomy_output_clean$order[i] = "Charadriiformes"
  }

  if (taxonomy_output_clean$family[i] == "Ardeidae" | taxonomy_output_clean$family[i] == "Threskiornithidae" |
      taxonomy_output_clean$family[i] == "Pelecanidae" | taxonomy_output_clean$family[i] == "Anhingidae" |
      taxonomy_output_clean$family[i] == "Fregatidae" | taxonomy_output_clean$family[i] == "Sulidae" ) {

    taxonomy_output_clean$order[i] = "Pelecaniformes"
  }


  if (taxonomy_output_clean$family[i] == "Casuariidae") {
    taxonomy_output_clean$order[i] = "Casuariiformes"
  }

  if (taxonomy_output_clean$family[i] == "Rallidae") {
    taxonomy_output_clean$order[i] = "Gruiformes"
  }


  if (taxonomy_output_clean$family[i] == "Diomedeidae" | taxonomy_output_clean$family[i] == "Procellariidae" |
      taxonomy_output_clean$family[i] == "Pelecanoididae") {

    taxonomy_output_clean$order[i] = "Procellariiformes"
  }

  if (taxonomy_output_clean$family[i] == "Ciconiidae" |
      taxonomy_output_clean$family[i] == "Thinocoridae") {

    taxonomy_output_clean$order[i] = "Ciconiiformes"
  }

  if (taxonomy_output_clean$family[i] == "Phoenicopteridae") {
    taxonomy_output_clean$order[i] = "Phoenicopteriformes"
  }

  if (taxonomy_output_clean$family[i] == "Struthionidae") {
    taxonomy_output_clean$order[i] = "Struthioniformes"
  }

  if (taxonomy_output_clean$family[i] == "Trogonidae") {
    taxonomy_output_clean$order[i] = "Trogoniformes"
  }

  if (taxonomy_output_clean$family[i] == "Picidae") {
    taxonomy_output_clean$order[i] = "Piciformes"
  }

  if (taxonomy_output_clean$family[i] == "Planariidae") {
    taxonomy_output_clean$order[i] = "Tricladida"
  }

  if (taxonomy_output_clean$family[i] == "Phoeniculidae" |
      taxonomy_output_clean$family[i] == "Upupidae") {

    taxonomy_output_clean$order[i] = "Upupiformes"
  }

  else {
     taxonomy_output_clean$order[i] = taxonomy_output_clean$order[i]
  }

}
  
taxonomy_output_clean

#DF for M1-3: combine rates and taxonomy info ----
#need to combine taxonomy_output_clean and data with met rates (df2_clean_up, df5_up) to make the df for M1-M3
#taxonomy_output_clean

#df2_clean_up but only columns needed
glimpse(df5_up)
data_to_comb <- df5_up %>% 
  select(.,1:12, 17:23)
unique(data_to_comb$Genus)

df_m123 <- full_join(data_to_comb, taxonomy_output_clean, by = c("Genus", "met_category", "unique_name", "ott_id"))

names(df_m123)[names(df_m123) == "ott_id"] <- "genus_ott_id"
names(df_m123)[names(df_m123) == "unique_name"] <- "genus_unique_name" #might be tip.label but need to work out
df_m123$tip.label <- df_m123$Genus


#make columns with rate calculated with Ei=0.6
#tempMR and lntempMR use Ei=0.63 
##how the met rate is calculated from Watts to temp corrected met rate (new)
#Get temp corrected met rate and ln. 
k <- 0.00008617343
E <- 0.6
df_m123$tempMR_E0.6 <- df_m123$W*exp(E/(k*(df_m123$temp.C.all+273.15)))

df_m123$lntempMR_E0.6 <- log(df_m123$tempMR_E0.6)

#now with Ea = 0.63
Ea=0.63

df_m123$tempMR_E0.63 <- df_m123$W*exp(Ea/(k*(df_m123$temp.C.all+273.15)))

df_m123$lntempMR_E0.63 <- log(df_m123$tempMR_E0.63)

#save out df for M1:3
write.csv(df_m123, file= "./Data/met_brms_input/met_rates_m123_apr_22.csv", row.names = FALSE)

colnames(df_m123)

#need to get median values for MR and mass
median_data <-  aggregate(cbind(tempMR_E0.6, tempMR_E0.63, mass.g) ~ met_category + tip.label + family +
                          order + genus_ott_id + genus_unique_name,
                        data = df_m123, median)

mean_data <-  aggregate(cbind(tempMR_E0.6, tempMR_E0.63,mass.g) ~ met_category + tip.label + family +
                          order + genus_ott_id + genus_unique_name,
                        data = df_m123, mean)

#need ln of mass and temp corrected MR to check data
mean_data$LNMass.g <- log(mean_data$mass.g)
mean_data$LNMR0.6 <- log(mean_data$tempMR_E0.6)
mean_data$LNMR0.63 <- log(mean_data$tempMR_E0.63)

#get tree tip names as a column in our data
#due to the tree was got we already have these
mean_data$phylo <- mean_data$tip.label


write.csv(median_data, file= "./Data/met_brms_input/median_met_rates_m123_apr_22.csv", row.names = FALSE)

write.csv(mean_data, file= "./Data/met_brms_input/mean_met_rates_m123_apr_22.csv", row.names = FALSE)

#mean_data <- read.csv("./Data/met_brms_input/mean_met_rates_m123_apr_22.csv")

###genus level tree to match this data and for M4 is tree_met_corrected_ottid.nex
#before the tip labels were changed. 


#Get tree with correct tip labels ---- 
taxon_map_used_update
tr_update$tip.label<- unname(tr_update$tip.label)

#make sure no differences between tip.label and genus. 
setdiff(df_m123$Genus, tr_update$tip.label)


write.nexus(tr_update, file="./Data/phylo_tree/Metabolism/tree_met_names_sorted.nex")

#Add branch lengths to tree
tr_update_bl <- compute.brlen(tr_update, method = "Grafen", power=1)

is.ultrametric(tr_update_bl) #v important true
is.rooted(tr_update_bl)

write.nexus(tr_update_bl, file="./Data/phylo_tree/Metabolism/tree_met_names_sorted_incBL.nex")



##Plot data total data----
#initial plot on temp corrected met rate (E=0.6)
explore_data2 <- ggplot(data = mean_data, aes(x = LNMass.g, y = LNMR0.63, fill = met_category)) +
  geom_point(pch = 21, col = 'black', size = 5)+
  labs(x='ln(Mass) (kg)', y = bquote("ln(Production"~ e^{E/kT}~")")) +
  #scale_fill_viridis(discrete = T, option = 'viridis')+
  theme_classic() +
  theme(plot.margin=unit(c(1,1,1,1),"cm"), 
        axis.text=element_text(size=24),
        axis.title=element_text(size=30),
        legend.position="bottom") 


# + scale_fill_manual(values = c("ecto_invert" = "#2c7bb6",
#                               "ecto_vert"="#fdae61",
#                               "endo_vert"="#d7191c",
#                                "plant"="purple"),
#                    labels = c("Ecto Invertebrate", "Ecto Vertebrate", "Endo Vertebrate", "Producer"))

explore_data2



tiff("Results/Metabolism/mean_metabolism_rateVmass.tiff", units="cm", width=40, height=35, res=300)
explore_data2
dev.off()

#smaller images have these specs as example: 
# tiff("Results/BES_Poster/mean_met_poster_smaller.tiff", units="cm", width=20, height=17.5, res=300)
# explore_data_smaller
# dev.off()


#####################################################

#Finally save out data needed for Models ----

##Get data and trees as separate metabolic categorys ----


#Tree (genera level)

#met phylo tree (inc branch lengths)
phylo_tree <- read.nexus("./Data/phylo_tree/Metabolism/tree_met_names_sorted_incBL.nex")
                        
#plot(phylo_tree)

is.ultrametric(phylo_tree) #v important true
is.rooted(phylo_tree)

##endotherms ----
mean_endo <- mean_data %>% 
  filter(., met_category == "endo_vert")

phylo_tree_endo <- drop.tip(phylo_tree, setdiff(phylo_tree$tip.label, mean_endo$tip.label))

#save df and tree endotherms
write.csv(mean_endo, file= "./Data/met_brms_input/mean_met_endo_m123_feb_23.csv", row.names = FALSE)
write.nexus(phylo_tree_endo, file="./Data/phylo_tree/Metabolism/tree_met_names_sorted_incBL_endo.nex")


##ectothermic verts ----
mean_ecto_vert <- mean_data %>% 
  filter(., met_category == "ecto_vert")

phylo_tree_ecto_vert <- drop.tip(phylo_tree, setdiff(phylo_tree$tip.label, mean_ecto_vert$tip.label))

#save df and tree ecto vert
write.csv(mean_ecto_vert, file= "./Data/met_brms_input/mean_met_ecto_vert_m123_feb_23.csv", row.names = FALSE)
write.nexus(phylo_tree_ecto_vert, file="./Data/phylo_tree/Metabolism/tree_met_names_sorted_incBL_ecto_vert.nex")


##Inverts ----
unique(mean_data$met_category)
mean_invert <- mean_data %>% 
  filter(., met_category == "ecto_invert")

length(unique(mean_invert$genus_ott_id))


phylo_tree_invert <- drop.tip(phylo_tree, setdiff(phylo_tree$tip.label, mean_invert$tip.label))

#save df and tree invert
write.csv(mean_invert, file= "./Data/met_brms_input/mean_met_invert_m123_feb_23.csv", row.names = FALSE)
write.nexus(phylo_tree_invert, file="./Data/phylo_tree/Metabolism/tree_met_names_sorted_incBL_invert.nex")



#imputation for old M4 can be found in the older version of this script but nolonger using. 



#looking at data volume before meaning across Genera 

vol_cleaned_df <- read.csv(file= "./Data/met_brms_input/met_rates_m123_apr_22.csv")


table(vol_cleaned_df$group, vol_cleaned_df$met_category)
table(vol_cleaned_df$met_category) 

mean_cleaned_df <- read.csv(file= "./Data/met_brms_input/mean_met_rates_m123_apr_22.csv")

table(mean_cleaned_df$met_category)
