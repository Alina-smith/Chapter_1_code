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
  left_join(fg_no_p, by = c("functional.group" = "functional.group.imputed")) %>% 

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
  
  # Apply rowwise for the function below
  rowwise() %>% 
  
  # Select find highest tip state and select that column name
  mutate(
    functional.group.imputed.no = if_else(
      functional.group.cor == "?",
      colnames(across(3:31))[which.max(c_across(3:31))],
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
dat_cor_z <- taxa_in_tree_update_z %>%
  
  # add in number versions of functional.groups
  left_join(fg_no_z, by = c("functional.group" = "functional.group.imputed")) %>% 
  
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

fit_z <- corHMM(phy       = phy_z,
                data         = dat_cor_z,
                rate.cat     = 1,         # simple Mk model
                model        = "ARD",     # all rates differ
                node.states  = "marginal",
                get.tip.states = TRUE)    # keep reconstructed tips
# `fit$tip.states` is a matrix: rows = tips, cols = 5 states (likelihoods)

# Save
saveRDS(fit_z, "R/data_outputs/database_products/fit_z.rds")

# Get tip states from fit
tip_recon_z <- as.data.frame(fit_z$tip.states) %>% 
  mutate(
    tip.label = rownames(.)
  )

# Get imputed states
imputed_groups_z = dat_cor_z %>%
  # Join in dat_cor data
  left_join(
    tip_recon_z, by = "tip.label"
  ) %>% 
  
  # Remove the V from the titles
  rename_with(
    ~ sub("^V", "", .x)
  ) %>% 
  
  # Apply rowwise for the function below
  rowwise() %>% 
  
  # Select find highest tip state and select that column name
  mutate(
    functional.group.imputed.no = if_else(
      functional.group.cor == "?",
      colnames(across(3:16))[which.max(c_across(3:16))],
      functional.group.cor
    )
  ) %>% 
  
  # add back in original group names and taxonomy data
  left_join(
    fg_no_z, by = c("functional.group.imputed.no" = "functional.group.cor")
  ) %>% 
  left_join(
    taxa_in_tree_update_z, by = "tip.label"
  ) %>% 
  
  # Select columns
  select(ott.id, tip.label, taxa.name, taxonomic.group, functional.group.imputed, type, family, order, class, phylum, kingdom)

# Join together ----
# Read in data
imputed_tax <- bind_rows(imputed_groups_z, imputed_groups_p)

# Save
saveRDS(imputed_tax, "R/data_outputs/database_products/imputed_tax.rds")


# Add to main data ----
# Read in data
plankton_genus_traits <- readRDS("R/data_outputs/database_products/final_products/plankton_genus_traits.rds")

# Add in imputed groups
plankton_database <- plankton_genus_traits %>% 
  
  left_join(
    select(
      imputed_tax, functional.group.imputed, taxa.name
      ), by = c("genus" = "taxa.name")
  ) %>% 
  
  mutate(
    # add in source for imputed
    fg.source = case_when(
      !is.na(functional.group) ~ fg.source,
      is.na(functional.group.imputed) ~ NA,
      TRUE ~ "Imputed"
    ),
    
    # insert fgs for ones that weren't in the tree
    functional.group = if_else(
      !is.na(functional.group),
      functional.group,
      functional.group.imputed
    ),

    # change NA fg to "unkown"
    functional.group = if_else(
      is.na(functional.group),
      "Unknown",
      functional.group
      )
    ) %>% 
  
  select(
    uid, source.code, original.sources, taxa.name,
    c.ug, dw.ug, mld,
    ott.id, ott.id, type, taxa.name, genus, family, order, class, phylum, kingdom,
    taxonomic.group, functional.group, fg.source,
    habitat, location.code, latitude, longitude, water.body, place, country,area, continent
  )

# Save
saveRDS(plankton_database, "R/data_outputs/database_products/final_products/plankton_database.rds")






















