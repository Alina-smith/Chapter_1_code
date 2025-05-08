# imputation

#packages
library(Rphylopars)
library(ape)

# Import data ----
phyto_subset <- readRDS("R/data_outputs/database_products/final_products/phyto_subset.rds")
tree <- read.tree("R/data_outputs/phylo_tree/tree_pre_plot_genus.nex")

# Trait data with missing values (as NA)
phyto_format <- phyto_subset %>% 
  
  mutate(
    # when the r.group for the species is missing that use the next highest one
    r.group = case_when(
      !is.na(r.group.genus) ~ r.group.genus,
      is.na(r.group.genus) & !is.na(r.group.family) ~ r.group.family,
      is.na(r.group.genus) & is.na(r.group.family) & !is.na(r.group.order) ~ r.group.order,
      is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & !is.na(r.group.class) ~ r.group.class,
      is.na(r.group.genus) & is.na(r.group.family) & is.na(r.group.order) & is.na(r.group.class) & !is.na(r.group.phylum) ~ r.group.phylum,
      
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    nu == "individual"
  ) %>% 
  
  select(
    genus, r.group
  ) %>% 
  
  distinct(genus, .keep_all = TRUE) %>% 
  
  as.data.frame()


# convert data to a named vector
# Set genus as rownames
rownames(phyto_format) <- phyto_format$genus

phyto_ <- phyto_format %>% 
  
  mutate(
    genus = rownames(genus)
  ) %>% 
  
  pull() %>% 
  as.factor() %>% 
  as.numeric()

# View factor levels for mapping
levels(trait_factor)







# find missing genus from tree
missing_from_tree <- anti_join(phylo_plot_data_genus, taxa_in_tree_genus, by = "genus")

no_r_group <- phylo_plot_data_genus %>% 
  filter(
    r.group == "unasigned"
  )
x <- taxa_in_tree_genus %>% 
  filter(
    r.group == "unasigned"
  )
ones_missing_with_no_r_group <- anti_join(phylo_plot_data_genus, taxa_in_tree_genus, by = "genus") %>% 
  
  select(
    -species
  ) %>% 
  
  filter(
    r.group == "unasigned"
  )

length(unique(phylo_plot_data_genus$genus))
## Genus
# total number of genera - 810
# without R group - 154
# missing from tree - 154
# with no R group missing from tree - 38

# with R group - 662
810-154

## Species
# total number of species - 3579
# without R group - 188
# with R group - 3391
# missing from tree - 515
# missing from tree with no r group - 42
# in tree - 3064


# find missing species from tree
missing_from_tree <- anti_join(phylo_plot_data_species, taxa_in_tree_species, by = "species")

x <- taxa_in_tree_species %>% 
  filter(
    r.group != "unasigned"
  )

no_r_group <- phylo_plot_data_species %>% 
  filter(
    r.group = "unasigned"
  )

ones_missing_with_no_r_group <- anti_join(phylo_plot_data_species, taxa_in_tree_species, by = "species") %>% 
  
  filter(
    r.group == "unasigned"
  )

length(unique(phylo_plot_data_genus$genus))
# genus missing from tree - 154
# genus with no R group missing from tree - 38
# genus with no R group - 154
# genus with R group - 662
# total number of genera - 810


species <- phylo_plot_data_species %>% 
  distinct(genus)

genus <- phylo_plot_data_genus %>% 
  distinct(genus)









