# Imputation to get missing functional groups

# Build a synthetic subtree with rotl and then use that for the imputataion


# Packages ----

packages <- c("corHMM","ape","phytools","rotl","dplyr",
              "tidyr")
newpkgs  <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newpkgs)) install.packages(newpkgs)
library(corHMM)   # discrete-trait HMMs
library(ape)      # tree utilities
library(phytools) # a few helper functions
library(rotl)     # Open Tree interface
library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(2025)    # for repeatability
library(ggtree)
library(treeio)
library(ggtreeExtra)
library(stringi)


# Import data ----
bs_traits <- readRDS("R/data_outputs/database_products/final_products/plankton_genus_traits.rds") %>% 
  mutate(
    taxa.name = tolower(taxa.name)
    )

# Phyto ----
## Format data ----
p_data <- bs_traits %>% 
  
  filter(
    type == "Phytoplankton"
  ) %>% 
  
  select(
    ott.id, taxa.name, type, phylum, kingdom, family, order, class, functional.group, taxonomic.group
  ) %>% 
  
  distinct(taxa.name, .keep_all = TRUE)

## Filter for in tree ----
# get ott_ids
taxa_p <- tnrs_match_names(p_data$taxa.name)

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_p <- is_in_tree(ott_id(taxa_p))

# Save
saveRDS(in_tree_p, "R/data_outputs/phylo_tree/in_tree_p.rds")

# View data
in_tree_p

# See which ones are in and out
sum(in_tree_p == TRUE) # 614
sum(in_tree_p == FALSE) # 138

## Get tree ----
# Select just taxa that are in the tree
taxa_in_tree_p <- taxa_p[in_tree_p, ]

# Save
saveRDS(taxa_in_tree_p, "R/data_outputs/phylo_tree/taxa_in_tree_p.rds")

# Make tree 
tree_p <- tol_induced_subtree(ott_ids = taxa_in_tree_p$ott_id)

# Save
write.nexus(tree_p, file="R/data_outputs/phylo_tree/tree_p.nex")

## Remove extra nodes from tree ----
# import trees and data
tree_p <- read.nexus("R/data_outputs/phylo_tree/tree_p.nex")
taxa_in_tree_p <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_p.rds")

# make tip label column in data
taxa_in_tree_update_p <- taxa_in_tree_p %>% 
  
  # Select columns
  select(
    search_string, unique_name, ott_id
  ) %>% 
  
  mutate(
    # Make tip label column
    tip.label = paste0(unique_name, "_ott", ott_id),
    tip.label = stri_replace_all_regex(tip.label, " ", "_"),
    
    # Taxa.name
    taxa.name = unique_name
  ) %>% 
  
  # Join trait data in
  left_join(p_data, by = c("search_string" = "taxa.name")) %>% 
  
  # Remove columns
  select(
    - unique_name, - search_string, - ott_id
  )

# Remove tips from tree that aren't in taxa_in_tree_update
tree_p <- drop.tip(tree_p, setdiff(tree_p$tip.label, taxa_in_tree_update_p$tip.label))

## Imputation ----
# Get branch lengths
phy_p = compute.brlen(tree_p)

# Format data
# assign groups numbers for corrhmm
fg_no_p <- data.frame(
  functional.group.imputed = c("X2", "X1", "K", "TB", "F", "J", "P", "B", "E", "X3", "LM", "LO", "XPH", "C", "N", "Y", "S1", "MP", "Q", "W1", "G", "D", "S2", "A", "H2", "TC", "SN", "U", "M", "T" ),
  functional.group.cor = as.character(1:30)
)

# format data
dat_cor_p <- taxa_in_tree_update_p %>%
  
  # add in number versions of functional.groups
  left_join(fg_no_p, by = "functional.group") %>% 

  # Set NAs to a regex that corHMM recognises
  mutate(
    functional.group.cor = ifelse(is.na(functional.group.cor), "?", functional.group.cor),
    functional.group.cor = as.character(functional.group.cor)
  ) %>% 
  
  # Select columns I want
  select(
    tip.label, functional.group.cor
  ) 

# Run corHMM

fit_p <- corHMM(phy       = phy_p,
                 data         = dat_cor_p,
                 rate.cat     = 1,         # simple Mk model
                 model        = "ARD",     # all rates differ
                 node.states  = "marginal",
                 get.tip.states = TRUE)    # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)

# Save
saveRDS(fit_p, "R/data_outputs/database_products/fit_p.rds")

# Get tip states from fit
tip_recon_p <- as.data.frame(fit_p$tip.states) %>% 
  mutate(
    tip.label = rownames(.)
  )

# Get imputed states
imputed_groups_p = dat_cor_p %>%
  # Join in dat_cor data
  left_join(
    tip_recon_p, by = "tip.label"
  ) %>% 
  
  # Remove the V from the titles
  rename_with(
    ~ sub("^V", "", .x)
  ) %>% 
  
  # Select find highest tip state and select that column name
  mutate(
    functional.group.imputed.no = if_else(
      functional.group.cor == "?",
      apply(.[3:32], 1, function(x) names(.[3:32])[which.max(x)]),
      functional.group.cor
      )
  ) %>% 
  
  # add back in original group names and taxonomy data
  left_join(
    fg_no_p, by = c("functional.group.imputed.no" = "functional.group.cor")
  ) %>% 
  left_join(
    taxa_in_tree_update_p, by = "tip.label"
  ) %>% 
  
  # Select columns
  select(ott.id, tip.label, taxa.name, taxonomic.group, functional.group.imputed, type, family, order, class, phylum, kingdom)
  

# Zoo ----
## Format data ----
z_data <- bs_traits %>% 
  
  filter(
    type == "Zooplankton"
  ) %>% 
  
  select(
    ott.id, taxa.name, type, phylum, kingdom, family, order, class, functional.group, taxonomic.group
  ) %>% 
  
  distinct(taxa.name, .keep_all = TRUE)

## Filter for in tree ----
# get ott_ids
taxa_z <- tnrs_match_names(z_data$taxa.name)

# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree_z <- is_in_tree(ott_id(taxa_z))

# Save
saveRDS(in_tree_z, "R/data_outputs/phylo_tree/in_tree_z.rds")

# View data
in_tree_z

# See which ones are in and out
sum(in_tree_z == TRUE) # 185
sum(in_tree_z == FALSE) # 7

## Get tree ----
# Select just taxa that are in the tree
taxa_in_tree_z <- taxa_z[in_tree_z, ]

# Save
saveRDS(taxa_in_tree_z, "R/data_outputs/phylo_tree/taxa_in_tree_z.rds")

# Make tree 
tree_z <- tol_induced_subtree(ott_ids = taxa_in_tree_z$ott_id)

# Save
write.nexus(tree_z, file="R/data_outputs/phylo_tree/tree_z.nex")

## Remove extra nodes from tree ----
# import trees and data
tree_z <- read.nexus("R/data_outputs/phylo_tree/tree_z.nex")
taxa_in_tree_z <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_z.rds")

# make tip label column in data
taxa_in_tree_update_z <- taxa_in_tree_z %>% 
  
  # Select columns
  select(
    search_string, unique_name, ott_id
  ) %>% 
  
  mutate(
    # Make tip label column
    tip.label = paste0(unique_name, "_ott", ott_id),
    tip.label = stri_replace_all_regex(tip.label, " ", "_"),
    
    # Taxa.name
    taxa.name = unique_name
  ) %>% 
  
  # Join trait data in
  left_join(z_data, by = c("search_string" = "taxa.name")) %>% 
  
  # Remove columns
  select(
    - unique_name, - search_string, - ott_id
  )

# Remove tips from tree that aren't in taxa_in_tree_update
tree_z <- drop.tip(tree_z, setdiff(tree_z$tip.label, taxa_in_tree_update_z$tip.label))

## Imputation ----
# Get branch lengths
phy_z = compute.brlen(tree_z)

# Format data
# assign groups numbers for corrhmm
fg_no_z <- data.frame(
  functional.group.imputed = c("4", "16", "9", "7", "2", "1", "17", "10", "14", "13", "11", "12", "8", "6"),
  functional.group.cor = as.character(1:14)
)

# format data
dat_cor_p <- taxa_in_tree_update_p %>%
  
  # add in number versions of functional.groups
  left_join(fg_no, by = "functional.group") %>% 
  
  # Set NAs to a regex that corHMM recognises
  mutate(
    functional.group.cor = ifelse(is.na(functional.group.cor), "?", functional.group.cor),
    functional.group.cor = as.character(functional.group.cor)
  ) %>% 
  
  # Select columns I want
  select(
    tip.label, functional.group.cor
  ) 

# Run corHMM

fit_p <- corHMM(phy       = phy_p,
                data         = dat_cor_p,
                rate.cat     = 1,         # simple Mk model
                model        = "ARD",     # all rates differ
                node.states  = "marginal",
                get.tip.states = TRUE)    # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)

# Save
saveRDS(fit_p, "R/data_outputs/database_products/fit_p.rds")

# Get tip states from fit
tip_recon_p <- as.data.frame(fit_p$tip.states) %>% 
  mutate(
    tip.label = rownames(.)
  )

# Get imputed states
imputed_groups_p = dat_cor_p %>%
  # Join in dat_cor data
  left_join(
    tip_recon_p, by = "tip.label"
  ) %>% 
  
  # Remove the V from the titles
  rename_with(
    ~ sub("^V", "", .x)
  ) %>% 
  
  # Select find highest tip state and select that column name
  mutate(
    functional.group.imputed.no = if_else(
      functional.group.cor == "?",
      apply(.[3:32], 1, function(x) names(.[3:32])[which.max(x)]),
      functional.group.cor
    )
  ) %>% 
  
  # add back in original group names and taxonomy data
  left_join(
    fg_no, by = c("functional.group.imputed.no" = "functional.group.cor")
  ) %>% 
  left_join(
    taxa_in_tree_update_p, by = "tip.label"
  ) %>% 
  
  # Select columns
  select(ott.id, tip.label, taxa.name, taxonomic.group, functional.group.imputed, type, family, order, class, phylum, kingdom)



























############################################################
## 2.  Data -----------------------------------------------
############################################################
data("primate.data")
primate.data$diet = NA
primate.data$diet = ifelse(primate.data$Skull_length < 60, 1, primate.data$diet)
primate.data$diet = ifelse(primate.data$Skull_length > 60 & primate.data$Skull_length < 110, 2, primate.data$diet)
primate.data$diet = ifelse(primate.data$Skull_length > 110, 3, primate.data$diet)
primate.data$species = rownames(primate.data)

# Introduce missing values
traits_me <-  primate.data[,c(8, 7, 4)] %>% 
  mutate(
    species = tolower(species),
    diet_intact = diet,
    
    diet = as.character(diet)
  )

miss <- sample(seq_len(nrow(traits_me)), 0.20*nrow(traits_me))   # 10 %
traits_me$diet[miss] <- NA



############################################################
## 4.  Build a matching phylogeny (Open Tree) --------------
############################################################

# Get ott_ids ----
taxa <- tnrs_match_names(traits_me$species)

# Filter for in tree ----
# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_id(taxa))

# View data
in_tree

# See which ones are in and out
sum(in_tree == TRUE) # 87
sum(in_tree == FALSE) # 3

# Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

taxa_in_tree <- taxa[in_tree, ] # get list of just species in the tree
tree <- tol_induced_subtree(ott_id(taxa_in_tree)) # make tree

# Remove extra nodes from tree ----
# update names in taxa_in_tree
taxa_in_tree_update <- taxa_in_tree %>% 
  
  # Select columns
  select(
    search_string, unique_name, ott_id
  ) %>% 
  
  # Make tip label column
  mutate(
    tip.label = paste0(unique_name, "_ott", ott_id),
    tip.label = stri_replace_all_regex(tip.label, " ", "_")
  ) %>% 
  
  # Join trait data in
  left_join(traits_me, by = c("search_string" = "species"))%>% 
  
  # update species name with the new names from tol
  mutate(
    species = tolower(unique_name)
  ) %>% 
  
  # Remove columns
  select(
    - unique_name, - search_string, - ott_id
  )

# Remove tips from tree that aren't in taxa_in_tree_update
tree <- drop.tip(tree, setdiff(tree$tip.label, taxa_in_tree_update$tip.label))

# Imputation ----
## Get branch lengths ----

#keep     <- intersect(tree$tip.label, taxa_in_tree_update$tip.label)
#phy_me      <- drop.tip(tree, setdiff(tree$taxa_in_tree_update, keep))
phy_me = compute.brlen(tree)

## Format data ----
dat_cor_me <- taxa_in_tree_update %>%
  
  # Set NAs to a regex that corHMM recognises
  mutate(
    diet = ifelse(is.na(diet), "?", diet),
    diet = as.character(diet)
    ) %>% 
  # Select columns I want
  select(
    tip.label, diet
  )

############################################################
## 5.  Run corHMM and impute the missing tips --------------
############################################################

fit_me <- corHMM(phy       = phy_me,
                 data         = dat_cor_me,
                 rate.cat     = 1,         # simple Mk model
                 model        = "ARD",     # all rates differ
                 node.states  = "marginal",
                 get.tip.states = TRUE)    # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)

# Pick the ML state for the missing species ----------------
tip_recon_me <- data.frame(tip.label = rownames(fit_me$tip.states),
                           fit_me$tip.states)

diet_levels = 3

imputed = dat_cor_me %>%
  bind_cols(select(tip_recon_me, -tip.label)) %>%
  mutate(imputed_state = ifelse(diet == "?",
                                max.col(tip_recon_me[,c(2:4)]),
                                as.character(diet)))

imputed_all = left_join(imputed, select(
  all_info, - diet
), by = "tip.label")


a <- tip_recon_me %>% select(tip.label)
b <- dat_cor_me %>% select(tip.label)
x <- setdiff(a, b)

test = subset(imputed_all, diet == "?")

ggplot(test) +
  geom_bar(aes(x = imputed_state))+
  facet_wrap(~ diet_intact)

















############################################################
## 4.  Build a matching phylogeny (Open Tree) --------------
############################################################
bs_traits <- bs_traits %>% 
  filter(type == "Phytoplankton") %>% 
  distinct(taxa.name, .keep_all = TRUE)

# Get ott_ids ----
taxa <- tnrs_match_names(bs_traits$taxa.name)

# Filter for in tree ----
# Check which ones are in tree with is_in_tree function - True = in tree, false = not in tree
in_tree <- is_in_tree(ott_id(taxa))
in_tree <- is_in_tree(ott_ids = bs_traits$ott.id)

# View data
in_tree

# See which ones are in and out
sum(in_tree == TRUE) # 87
sum(in_tree == FALSE) # 3

# Get tree ----
# Retrieve a tree from the OTL API that contains the taxa that is in in_tree 

taxa_in_tree <- taxa[in_tree, ] # get list of just species in the tree
taxa_in_tree <- bs_traits[in_tree, ]

tree <- tol_induced_subtree(ott_ids = taxa_in_tree$ott.id) # make tree

# Remove extra nodes from tree ----
# update names in taxa_in_tree
taxa_in_tree_update <- taxa_in_tree %>% 
  
  # # Select columns
  # select(
  #   search_string, unique_name, ott_id
  # ) %>% 
  
  # Make tip label column
  mutate(
    tip.label = paste0(taxa.name, "_ott", ott.id),
    tip.label = stri_replace_all_regex(tip.label, " ", "_"),
  ) 
%>% 
  
  # Join trait data in
  left_join(bs_traits, by = c("search_string" = "taxa.name")) %>% 
  
  # update species name with the new names from tol
  mutate(
    taxa.name = unique_name
  ) %>% 
  
  # Remove columns
  select(
    - unique_name, - search_string, - ott_id
  )

# Remove tips from tree that aren't in taxa_in_tree_update
tree <- drop.tip(tree, setdiff(tree$tip.label, taxa_in_tree_update$tip.label))

# Imputation ----
## Get branch lengths ----

#keep     <- intersect(tree$tip.label, taxa_in_tree_update$tip.label)
#phy_me      <- drop.tip(tree, setdiff(tree$taxa_in_tree_update, keep))
phy_me = compute.brlen(tree)

## Format data ----
dat_cor_me <- taxa_in_tree_update %>%
  
  # Set NAs to a regex that corHMM recognises
  # mutate(
  #   functional.group = ifelse(is.na(functional.group), "?", functional.group),
  #   functional.group = as.character(functional.group)
  # ) %>% 
  # Select columns I want
  select(
    tip.label, functional.group
  )

############################################################
## 5.  Run corHMM and impute the missing tips --------------
############################################################

fit_me <- corHMM(phy       = phy_me,
                 data         = dat_cor_me,
                 rate.cat     = 1,         # simple Mk model
                 model        = "ARD",     # all rates differ
                 node.states  = "marginal",
                 get.tip.states = TRUE)    # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)

# Pick the ML state for the missing species ----------------
tip_recon_me <- data.frame(tip.label = rownames(fit_me$tip.states),
                           fit_me$tip.states)

diet_levels = 3

imputed = dat_cor_me %>%
  bind_cols(select(tip_recon_me, -tip.label)) %>%
  mutate(imputed_state = ifelse(diet == "?",
                                max.col(tip_recon_me[,c(2:4)]),
                                as.character(diet)))

imputed_all = left_join(imputed, select(
  all_info, - diet
), by = "tip.label")


a <- tip_recon_me %>% select(tip.label)
b <- dat_cor_me %>% select(tip.label)
x <- setdiff(a, b)

test = subset(imputed_all, diet == "?")

ggplot(test) +
  geom_bar(aes(x = imputed_state))+
  facet_wrap(~ diet_intact)