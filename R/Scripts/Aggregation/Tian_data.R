## Organising tian data to use in aggregation experiments

## Work flow
# Step 1: Get taxonomy
#   1) Resolve name using grn_resolve()
#   2) Manually resolve any remaining names
#   3) Get taxonomy information from classification()
#   4) Manually add in any missing or wrong taxonomy data
#   5) Add taxonomy to data
# Step 2: aggregate into groups and get average body sizes for them

## Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)

## Data
tian <- read_xlsx("./Raw data/Tian data.xlsx", sheet = "tian_data")

##### Get taxonomy data ----

##### Resolve names

#£ 1) first run through of gnr_resolve to resolved spelling mistakes
tian_resolved_raw <- select(tian, original.taxa.name, code)%>%
  rowwise() %>% 
  mutate(
    # Run through resolver
    resolved = list(gnr_resolve(sci = original.taxa.name, http = "post", canonical = TRUE, best_match_only = TRUE)),
    # Seperate into columns
    taxa.name.gnr = ifelse(
      "matched_name2" %in% colnames(resolved),
      resolved$matched_name2,
      NA
    ),
    resolved.source = ifelse(
      "matched_name2" %in% colnames(resolved),
      resolved$data_source_title,
      NA
    )
  ) %>% 
  # remove excess info
  select(-resolved)

saveRDS(tian_resolved_raw, file = "Data/Aggregation/tian_resolved_raw.rds")

## 2) Manually resolve any that didn't get picked up by resolver
tian_resolved <- tian_resolved_raw %>% 
  mutate(
    resolved.source = 
      case_when(
        original.taxa.name == "Tintionnopsis" ~ "Manually",
        TRUE ~ resolved.source
      ),
    taxa.name.gnr = case_when(
      original.taxa.name == "Tintionnopsis" ~ "Tintinnopsis",
      TRUE ~ taxa.name.gnr
    )
  )
# Save
saveRDS(tian_resolved, file = "Data/Aggregation/tian_resolved.rds")

##### Get taxonomy data

## 1) Initial run through classification
tian_worms_list_raw <- tian_resolved %>% 
  distinct(taxa.name.gnr) %>% 
  mutate(
    # get taxonomic hierarchy info for species
    taxonomy = list(classification(taxa.name.gnr, db = "worms", marine_only = FALSE, return_id = TRUE, rows = 1))
  ) %>%
  as.data.frame(tian_resolved_taxonomy_raw) %>% 
  mutate(
    tax.uid.resolved = row_number()
  )
saveRDS(tian_worms_list_raw, file = "Data/Aggregation/tian_worms_list_raw.rds")

## 2) extract taxonomy info from raw columns
tian_worms_list_1 <- tian_worms_list_raw %>% 
  rowwise() %>% 
  mutate(
    # Set to NA if there is no data
    taxonomy = ifelse(is.data.frame(taxonomy[[1]]), list(taxonomy[[1]]), NA),
    
    # Extract data from the nested dataframes for both sources
    rank = ifelse("rank" %in% colnames(taxonomy),as.character(taxonomy$rank[nrow(taxonomy)]),NA_character_),
    genus = ifelse("Genus" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Genus"], NA_character_),
    family = ifelse("Family" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Family"], NA_character_),
    order = ifelse("Order" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Order"], NA_character_),
    class = ifelse("Class" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Class"], NA_character_),
    phylum = ifelse("Phylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Phylum"], NA_character_),
    phylum = ifelse("Phylum (Division)" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Phylum (Division)"], phylum),
    kingdom = ifelse("Kingdom" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "Kingdom"], NA_character_)
  ) %>% 
  select(-taxonomy)

## 3) Manually edit any missing or wring taxonomy
tian_taxonomy_list <- tian_worms_list_1 %>% 
  mutate(
    family = case_when(
      taxa.name.gnr == "Gomphonema" ~ "Gomphonemataceae",
      TRUE ~ family
    ),
    
    order = case_when(
      family == "Gomphonemataceae" ~ "Cymbellales",
      TRUE ~ order
    ),
    # add in a group column to say if it is phyto or zoo
    group = case_when(
      phylum %in% c("Ciliophora", "Cercozoa", "Amoebozoa", "Rotifera", "Arthropoda") ~ "zooplankton",
      TRUE ~ "phytoplankton"
    )
  ) %>% 
  rename(
    tax.uid = tax.uid.resolved
  )

# Save
saveRDS(tian_taxonomy_list, file = "Data/Aggregation/tian_taxonomy_list.rds")

#### Add taxonomy to raw data ----

## 1) Join taxonomy list to resolved list so original taxa name is there
tian_tax_resolved <- left_join(tian_resolved, tian_taxonomy_list, by = "taxa.name.gnr") %>% 
  select(-code)

## 2) Join to raw data
tian_tax <- left_join(tian, tian_tax_resolved, by = "original.taxa.name" ) %>% 
  select(-resolved.source) %>% 
  rename(
    mass.uid = code,
    taxa.name = taxa.name.gnr
  ) %>% 
  relocate(mass.uid, original.taxa.name, taxa.name, tax.uid, rank, genus, family, order, class, phylum, kingdom, group, mass.ug, mass.mg, mass.g)


##### Aggregate into groups ----
tian_genus <- tian_tax %>% 
  group_by(
    genus
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  ) %>% 
  left_join(select(tian_taxonomy_list, genus, family, order, class, phylum, kingdom, group),  by = "genus") %>% 
  unique()

tian_family <- tian_tax %>% 
  group_by(
    family
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  )%>% 
  left_join(select(tian_taxonomy_list, family, order, class, phylum, kingdom, group), by = "family") %>% 
  unique()

tian_order <- tian_tax %>% 
  group_by(
    order
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  ) %>% 
  left_join(select(tian_taxonomy_list, order, class, phylum, kingdom, group), by = "order")%>% 
  unique()

tian_class <- tian_tax %>% 
  group_by(
    class
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  ) %>% 
  left_join(select(tian_taxonomy_list, class, phylum, kingdom, group), by = "class")%>% 
  unique()

tian_phylum <- tian_tax %>% 
  group_by(
    phylum
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  ) %>% 
  left_join(select(tian_taxonomy_list, phylum, kingdom, group), by = "phylum")%>% 
  unique()

tian_kingdom<- tian_tax %>% 
  group_by(
    kingdom
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  ) %>% 
  left_join(select(tian_taxonomy_list, kingdom, group), by = "kingdom")%>% 
  unique()

tian_group <- tian_tax %>% 
  group_by(
    group
  ) %>% 
  summarise(
    mean.mass.ug = mean(mass.ug)
  ) %>% 
  left_join(select(tian_taxonomy_list, group), by = "group")%>% 
  unique()

## Save
write.csv(tian_genus, file = "Data/Aggregation/tian_genus.csv")
write.csv(tian_family, file = "Data/Aggregation/tian_family.csv")
write.csv(tian_order, file = "Data/Aggregation/tian_order.csv")
write.csv(tian_class, file = "Data/Aggregation/tian_class.csv")
write.csv(tian_phylum, file = "Data/Aggregation/tian_phylum.csv")
write.csv(tian_kingdom, file = "Data/Aggregation/tian_kingdom.csv")
write.csv(tian_group, file = "Data/Aggregation/tian_group.csv")