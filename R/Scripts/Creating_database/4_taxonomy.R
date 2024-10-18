# Adding taxonomy data to the species list 
# data run through resolver on 17/10/2024
# data ran through taxize on 21/5/2024

# Aim of script
# 1) resolved_gnr - Run the names through the gnr_resolve first to get rid of any spelling mistakes 
# 2) Manually resolve any names that weren't picked up by resolver and also change ones that are form or variety back as they resolver gets rid of the var. f. and I want to keep them

# Import data ----
# set file path:
master_db_path <- here("Raw_data", "Master_db_traits.xlsx")

bodysize_joined <- readRDS("R/Data_outputs/databases/bodysize_joined.rds")

# Resolve names ----
# Run the names through the resolver to fix any spelling mistakes 

# Resolve - gnr_resolve ----
resolved_gnr <- select(bodysize_joined, original.taxa.name) %>% 
  
  # Select all distinct original.taxa.names from all_raw
  distinct(original.taxa.name) %>% 
  
  rowwise() %>% # have to set as rowwise otherwise the gnr_resolve assigns the first species as all the species
  mutate(
    # Run through resolver
    resolved = list(gnr_resolve(sci = original.taxa.name, http = "post", canonical = TRUE, best_match_only = TRUE)),
    
    # extract information
    resolved.taxa.name = ifelse(
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

# Save
saveRDS(resolved_gnr, file = "R/Data_outputs/taxonomy/resolved_gnr.rds")

# Resolve - manual resolve ----
# Find all the taxa.names that needed to be resolved manually from the gnr_resolve dataframe and manually resolved them in a separate excel sheet
# How taxa.names were chosen for manual resolving:
  # Ones that were bumped up a taxonomic rank by the resolver - if the species is valid keep that otherwise keep the higher rank
  # Ones where the resolved.taxa.name was set to the name of the juvenile form or a common name instead of the taxa.name (e.g. nauplii or cyclops instead of copepoda)
  # Ones that weren't picked up by the resolver at all - gave NA for the resolved.taxa.name
  # Ones with the *SpecChar* regex put in to replcae special characters when joining the data together just to check that the regex didn't cause the resolver to resolve it weirdly

# How manual resolving was carried out:
  # When the species name could be found then use that
  # When the original.taxa.taxa.name has a species name but with the wrong genus the species is chosen and the genus is changed to match that species
  # when the species can't be found then the next highest rank is chosen
  # When two species are stated then the closet common higher group is used

## To manually resolve list ----
# Finding all the taxa.names from resolved_gnr to manually resolve based on the criteria above

# Weren't resolved:
NAs <- resolved_gnr %>% 
  filter(
    is.na(resolved.taxa.name)
  )

# Bumped up a taxonomic level:
## Find all that has a space in the original.taxa.name and not in the resolved.taxa.name as this will indicate that it was two names (species) and now one name (higher rank)
bumped_up <- resolved_gnr %>% 
  mutate(
    bumped = case_when(
      stri_detect_regex(original.taxa.name, " ") & !(stri_detect_regex(resolved.taxa.name, " ")) ~ "bumped",
      TRUE ~ "same"
    ),
    
    sp = case_when(
      stri_detect_regex(original.taxa.name, "\\b(?i)sp\\.|\\b(?i)spp\\.|\\bsp\\b|\\bspp\\b|\\bsp(\\d+)|\\bssp\\b") ~ "sp",
      TRUE ~ "not"
    )
  ) %>% 
  filter(
    bumped == "bumped",
    sp == "not",
  )

# Resolved.taxa.name is juvenile form or common name:
resolved_wrong <- resolved_gnr %>% 
  mutate(
    wrong = case_when(
      resolved.taxa.name %in% c("Cyst", "Nauplius", "Centric", "Centric diatom", "Volvocales", "Cyclops") ~ "yes",
      TRUE ~ "no"
    )
  ) %>% 
  filter(
    wrong == "yes"
  )

# Special charaters:
spec_char <- resolved_gnr %>% 
  filter(
    stri_detect_regex(original.taxa.name, "\\*SpecChar\\*")
  )

# Join together - list of names to resolve manually
to_resolve_manually <- bind_rows(NAs, bumped_up, resolved_wrong, spec_char) %>% 
  # get distinct original.taxa.names in case some got picked up more than once
  distinct(
    original.taxa.name
  )

# Save
write_csv(to_resolve_manually, "R/Data_outputs/Taxonomy/to_resolve_manually.csv")

## Join in manually resolved ones ----
# replace the resolved.taxa.name of the ones in resolved_gnr to the manually resolved ones when a manually resolved name is present

# Import the to_resolve_manual list with the now manually resolved names
manually_resolved <- read_xlsx(here("Raw_data","manually_resolved.xlsx"), sheet = "manually_resolved")

# Join the manually resolved names from the manually_resolved
resolved_taxa_list <- resolved_gnr %>% 
  # left join all the manually resolved ones from manual_resolve spreadsheet
  left_join(
    manually_resolved,
    by = "original.taxa.name",
    suffix = c(".gnr", ".manual")
  ) %>% 
  
  # select the ones left joined in
  mutate(
    resolved.taxa.name = case_when(
      resolved.taxa.name.manual == "couldn't resolve" ~ NA, # change the ones that couldn't be resolved to NA
      !is.na(resolved.taxa.name.manual) ~ resolved.taxa.name.manual,
      TRUE ~ resolved.taxa.name.gnr
    ),
    
    resolved.source = case_when(
      resolved.taxa.name.manual == "couldn't resolve" ~ NA,
      !is.na(resolved.source.manual) ~ resolved.source.manual,
      TRUE ~ resolved.source.gnr
    )
  ) %>% 

  select(
    original.taxa.name,
    resolved.taxa.name,
    resolved.source
  )
  
# Save
saveRDS(resolved_taxa_list, file = "R/Data_outputs/taxonomy/resolved_taxa_list.rds")
















### Resolve using gnr reolver
## 1) Create list of distinct original.taxa.names and run through gnr_resolve
resolved_all_sources <- bodysize_joined %>% 
  # Select all distinct original.taxa.names from all_raw
  distinct(original.taxa.name) %>% 
  
  head(10) %>% 
  
  rowwise() %>% 
  mutate(
    # Run through resolver
    resolved = list(gnr_resolve(sci = original.taxa.name, http = "post", canonical = TRUE, fields = "all"))
  )
# Save
saveRDS(resolved_all_sources, file = "Data/taxize/gnr_resolve/resolved_all_sources.rds")

resolved_all_sources <- as.data.frame(resolved_all_sources) %>% 
  mutate(
    tax.uid.resolved = row_number()
  )

## 2) select relevant information from resolved_all_sources 
resolved_tax <- resolved_all_sources %>% 
  rowwise() %>% 
  mutate(
    # select data for each source and make a column for them
    algaebase.raw = ifelse("AlgaeBase" %in% resolved$data_source_title,
                           list(filter(resolved, resolved$data_source_title == "AlgaeBase")),
                           list(data.frame(user_supplied_name = NA))),
    gbif.raw =  ifelse("GBIF Backbone Taxonomy" %in% resolved$data_source_title,
                       list(filter(resolved, resolved$data_source_title == "GBIF Backbone Taxonomy")),
                       list(data.frame(user_supplied_name = NA))),
    
    # handle any that have more than one entry for a data source
    algaebase.raw = ifelse(nrow(algaebase.raw) > 1,
                           list(algaebase.raw[1,]),
                           list(algaebase.raw)),
    gbif.raw = ifelse(nrow(gbif.raw) > 1,
                      list(gbif.raw[1,]),
                      list(gbif.raw)),
    
    # make taxonomy dataframe for each
    # algaebase
    names.algaebase = ifelse(is.na(algaebase.raw$user_supplied_name),
                             list(NA),
                             list(stri_split_regex(algaebase.raw$classification_path, "\\|"))),
    ranks.algaebase = ifelse(is.na(algaebase.raw$user_supplied_name),
                             list(NA),
                             list(stri_split_regex(algaebase.raw$classification_path_ranks, "\\|"))),
    taxonomy.algaebase = ifelse(names.algaebase == "",
                                list(data.frame(name = NA, rank = NA)),
                                list(data.frame(name = unlist(names.algaebase), rank = unlist(ranks.algaebase)))),
    # gbif
    names.gbif = ifelse(is.na(gbif.raw$user_supplied_name),
                        list(NA),
                        list(stri_split_regex(gbif.raw$classification_path, "\\|"))),
    ranks.gbif = ifelse(is.na(gbif.raw$user_supplied_name),
                        list(NA),
                        list(stri_split_regex(gbif.raw$classification_path_ranks, "\\|"))),
    taxonomy.gbif = ifelse(names.gbif == "",
                           list(data.frame(name = NA, rank = NA)),
                           list(data.frame(name = unlist(names.gbif), rank = unlist(ranks.gbif)))),
    
    # extract out data
    id.algaebase = ifelse("taxon_id" %in% colnames(algaebase.raw), algaebase.raw$taxon_id, NA_character_),
    id.gbif = ifelse("taxon_id" %in% colnames(gbif.raw), gbif.raw$taxon_id, NA_character_),
    
    rank.algaebase = ifelse("rank" %in% colnames(taxonomy.algaebase),as.character(taxonomy.algaebase$rank[nrow(taxonomy.algaebase)]),NA_character_),
    rank.gbif = ifelse("rank" %in% colnames(taxonomy.gbif),as.character(taxonomy.gbif$rank[nrow(taxonomy.gbif)]),NA_character_),
    
    species.algaebase = ifelse("species" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "species"], NA_character_),
    species.gbif = ifelse("species" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "species"], NA_character_),
    
    genus.algaebase = ifelse("genus" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "genus"], NA_character_),
    genus.gbif = ifelse("genus" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "genus"], NA_character_),
    
    family.algaebase = ifelse("family" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "family"], NA_character_),
    family.gbif = ifelse("family" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "family"], NA_character_),
    
    order.algaebase = ifelse("order" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "order"], NA_character_),
    order.gbif = ifelse("order" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "order"], NA_character_),
    
    class.algaebase = ifelse("class" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "class"], NA_character_),
    class.gbif = ifelse("class" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "class"], NA_character_),
    
    phylum.algaebase = ifelse("phylum" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "phylum"], NA_character_),
    phylum.gbif = ifelse("phylum" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "phylum"], NA_character_),
    
    kingdom.algaebase = ifelse("kingdom" %in% taxonomy.algaebase$rank, taxonomy.algaebase$name[taxonomy.algaebase$rank == "kingdom"], NA_character_),
    kingdom.gbif = ifelse("kingdom" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "kingdom"], NA_character_),
  ) %>% 
  select(-resolved, -algaebase.raw, -gbif.raw, -names.algaebase, -names.gbif, -ranks.algaebase, - ranks.gbif, -taxonomy.algaebase, -taxonomy.gbif,) %>% 
  relocate(tax.uid.resolved, taxa.name.gnr, id.algaebase, id.gbif, rank.algaebase, rank.gbif, species.algaebase, species.gbif, genus.algaebase, genus.gbif,
           family.algaebase, family.gbif, order.algaebase, order.gbif, class.algaebase, class.gbif, phylum.algaebase, phylum.gbif, kingdom.algaebase, kingdom.gbif)