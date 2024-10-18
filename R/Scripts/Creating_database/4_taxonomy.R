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
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them


# Taxonomy - gnr ----

# Taxonomy - gnr_resolve ----
# get the taxonony from the gnr_resolve as this allows access to algae base which is the best for harmonising names for the phytoplankton and can gte a list of all other databases so don't need to run through multiple times

taxonomy_gnr_raw <- resolved_taxa_list %>% 
  # Get list of distinct resolved.taxa.names to speed it up
  distinct(
    resolved.taxa.name
    ) %>% 
  
  # Run through gnr_resolve
  mutate(
    # set to all fields to get taxonomy from all the APIs that is avaiable
    taxonomy = list(gnr_resolve(sci = resolved.taxa.name, http = "post", canonical = TRUE, fields = "all"))
  )
  
# Save
saveRDS(taxonomy_gnr_raw, file = "R/Data_outputs/taxonomy/taxonomy_gnr_raw.rds")

# Taxonomy gnr - taxonomy list ----
# format the information from taxonomy_gnr_raw

taxonomy_gnr_extracted <- taxonomy_gnr_raw %>% 
  # rename taxonomy column for ease of understanding
  rename(
    tax.raw = taxonomy
  ) %>% 
  
  mutate(
    ## Separate out sources ----
    
    # assign the information for each source to their own columns
    ab.raw = ifelse("AlgaeBase" %in% tax.raw$data_source_title,
                    list( # make it a list so that it will return that line of information
                      filter( 
                        tax.raw, tax.raw$data_source_title == "AlgaeBase") %>% # select ones from algaebase
                        slice(1) # some have more than one entry for the same database so select just the first one
                      ),
                    list(data.frame(user_supplied_name = NA))),  # need to make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    tol.raw = ifelse("Open Tree of Life Reference Taxonomy" %in% tax.raw$data_source_title,
                    list( # make it a list so that it will return that line of information
                      filter( 
                        tax.raw, tax.raw$data_source_title == "Open Tree of Life Reference Taxonomy") %>% # select ones from algaebase
                        slice(1) # some have more than one entry for the same database so select just the first one
                    ),
                    list(data.frame(user_supplied_name = NA))),  # need to make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    itis.raw = ifelse("Integrated Taxonomic Information SystemITIS" %in% tax.raw$data_source_title,
                     list( # make it a list so that it will return that line of information
                       filter( 
                         tax.raw, tax.raw$data_source_title == "Integrated Taxonomic Information SystemITIS") %>% # select ones from algaebase
                         slice(1) # some have more than one entry for the same database so select just the first one
                     ),
                     list(data.frame(user_supplied_name = NA))),  # need to make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    gbif.raw = ifelse("GBIF Backbone Taxonomy" %in% tax.raw$data_source_title,
                      list( # make it a list so that it will return that line of information
                        filter( 
                          tax.raw, tax.raw$data_source_title == "GBIF Backbone Taxonomy") %>% # select ones from algaebase
                          slice(1) # some have more than one entry for the same database so select just the first one
                      ),
                      list(data.frame(user_supplied_name = NA))),  # need to make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    ## Extract information from sources ----
    # Algaebase
    tax.ab = if_else(
      is.na(ab.raw$user_supplied_name),
      # if there is no taxonomy info then make a dataframe of NAs
      list(
        data.frame(
          names = NA,
          ranks = NA
          )
        ),
      # if there is taxonomy info then make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
      list(
        data.frame(
          names = unlist(stri_split_fixed(ab.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(ab.raw$classification_path_ranks, "|"))
          )
        )
      ),
    
    # Tree of life
    tax.tol = if_else(
      is.na(tol.raw$user_supplied_name),
      # if there is no taxonomy info then make a dataframe of NAs
      list(
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      # if there is taxonomy info then make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
      list(
        data.frame(
          names = unlist(stri_split_fixed(tol.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(tol.raw$classification_path_ranks, "|"))
        )
      )
    ),
    
    # ITIS
    tax.itis = if_else(
      is.na(itis.raw$user_supplied_name),
      # if there is no taxonomy info then make a dataframe of NAs
      list(
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      # if there is taxonomy info then make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
      list(
        data.frame(
          names = unlist(stri_split_fixed(itis.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(itis.raw$classification_path_ranks, "|"))
        )
      )
    ),
    
    # GBIF
    tax.gbif = if_else(
      is.na(gbif.raw$user_supplied_name),
      # if there is no taxonomy info then make a dataframe of NAs
      list(
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      # if there is taxonomy info then make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
      list(
        data.frame(
          names = unlist(stri_split_fixed(gbif.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(gbif.raw$classification_path_ranks, "|"))
        )
      )
    )
  ) %>% 
  select(
    -ab.raw:-gbif.raw
  )
    
# Save
saveRDS(taxonomy_gnr_extracted, file = "R/Data_outputs/taxonomy/taxonomy_gnr_extracted.rds")


# Taxonomy - classification ----
# gbif chosen because recognised the most and hadled sysnonyms well

# run through with rows = 1 on as would take too long to manually select all
taxonomy_class_raw <- resolved_taxa_list %>% 
  
  # Get list of distinct resolved.taxa.names to speed it up
  distinct(
    resolved.taxa.name
  ) %>% 

  mutate(
    # run through classification
    taxonomy.gbif = list(classification(resolved.taxa.name, db = "gbif", return_id = TRUE, rows = 1)[[1]])
  ) %>% 
  
  # make tax.uid to help with later steps
  # make a data.frame to remove rowwise from when it was saved as rds from the resolving stage
  as.data.frame(taxonomy_class_raw) %>% 
  
  mutate(
    # make tax.uid to help with later steps
    tax.uid = paste("tax", row_number(), sep = "-")
  )

# Save
saveRDS(taxonomy_class_raw, file = "R/Data_outputs/taxonomy/taxonomy_class_raw.rds")
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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