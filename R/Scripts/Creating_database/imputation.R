# imputation


#############################################
# Packages ----
#############################################
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


#############################################
# Import data ----
#############################################
bs_traits <- readRDS("R/data_outputs/database_products/final_products/bodysize_traits.rds")
tree_pre_plot_p <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_p.nex")
tree_pre_plot_z <- read.nexus("R/data_outputs/phylo_tree/tree_pre_plot_z.nex")

traits_p <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_full_p.rds") %>% 
  select(
    tip.label, taxa.name, fg
  ) %>% 
  mutate(
    fg = if_else(fg == "Unassigned", "?", fg)
  )
traits_z <- readRDS("R/data_outputs/phylo_tree/taxa_in_tree_full_z.rds") %>% 
  select(
    tip.label, taxa.name, fg
  ) %>% 
  mutate(
    fg = if_else(fg == "Unassigned", "?", fg)
  )


#############################################
# Get branch lengths ----
#############################################
## Phyto ----
keep_p <- intersect(tree_pre_plot_p$tip.label, traits_p$tip.label)
tree_filtered_p <- drop.tip(tree_pre_plot_p, setdiff(tree_pre_plot_p$tip.label, keep_p))
phy_p = compute.brlen(tree_filtered_p)

## Zoo ----
keep_z <- intersect(tree_pre_plot_z$tip.label, traits_z$tip.label)
tree_filtered_z <- drop.tip(tree_pre_plot_z, setdiff(tree_pre_plot_z$tip.label, keep_z))
phy_z = compute.brlen(tree_filtered_z)


#############################################
# final trait table matched to the tree ----
#############################################
## Phyto ----
dat_cor_p <- traits_p %>%
  select(
    tip.label, fg
  )

## Zoo ----
dat_cor_z <- traits_z %>%
  select(
    tip.label, fg
  )


#############################################
# Run corHMM and impute the missing tips ----
#############################################
## Phyto ----
fit_p <- corHMM(phy = phy_p,
                 data = dat_cor_p,
                 rate.cat = 1, # simple Mk model
                 model = "ARD", # all rates differ
                 node.states = "marginal",
                fixed.nodes = TRUE,
                 get.tip.states = TRUE) # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)

## Zoo ----
fit_z <- corHMM(phy = phy_z,
                data = dat_cor_z,
                rate.cat = 1, # simple Mk model
                model = "ARD", # all rates differ
                node.states = "marginal",
                get.tip.states = TRUE) # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)


#############################################
# Get the imputed data ----
#############################################
## Phyto ----
tip_recon_z <- data.frame(tip.label = rownames(fit_z$tip.states),
                           fit_z$tip.states)

# Define how many functional groups
levels_z = 17

# Select the imputed data
imputed_z <-  dat_cor_z %>%
  bind_cols(select(tip_recon_z, -tip.label)) %>%
  mutate(imputed_state = ifelse(fg == "?",
                                max.col(tip_recon_z[,c(2:4)]),
                                as.character(fg)))

imputed_all_z = left_join(imputed_z, select(
  all_info, - diet
), by = "tip.label")


test = subset(imputed_all, diet == "?")

ggplot(test) +
  geom_bar(aes(x = imputed_state))+
  facet_wrap(~ diet_intact)








































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

## Phyto ----
# get list of just species in the tree split by phyto and zoo
taxa_in_tree <- taxa[in_tree, ]

# make tree
tree_raw_me <- tol_induced_subtree(ott_id(taxa_in_tree))

# replace OTT labels with binomials & drop tips lacking traits

all_info <- taxa_in_tree %>% 
  
  select(
    search_string,
    unique_name,
    ott_id
  ) %>% 
  
  mutate(
    tip.label = paste0(unique_name, "_ott", ott_id),
    tip.label = stri_replace_all_regex(tip.label, " ", "_")
  ) %>% 
  
  left_join(traits_me, by = c("search_string" = "species")) %>% 
  
  # update species name with the new names from tol
  mutate(
    species = strip_ott_ids(tip.label, remove_underscores = TRUE),
    species = stri_replace_all_regex(species, " ", "_"),
    species = tolower(species)
  ) %>% 
  
  select(
    - unique_name,
    - search_string,
    - ott_id
  )

# Get branch lengths

keep     <- intersect(tree_raw_me$tip.label, all_info$tip.label)
phy_me      <- drop.tip(tree_raw_me, setdiff(tree_raw_me$tip.label, keep))
phy_me = compute.brlen(phy_me)

phy_me = compute.brlen(tree_raw_me)

# final trait table matched to the tree --------------------
dat_cor_me <- all_info %>%
  
  mutate(
    diet = ifelse(is.na(diet), "?", diet)
  ) %>% 
  
  filter(tip.label %in% phy_me$tip.label) %>% 
  mutate(diet = as.character(diet)) %>% 
  
  select(
    tip.label,
    diet
  )

x <- as.data.frame(tree_raw_me$tip.label)

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


test = subset(imputed_all, diet == "?")

ggplot(test) +
  geom_bar(aes(x = imputed_state))+
  facet_wrap(~ diet_intact)