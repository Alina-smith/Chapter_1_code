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


# Spelling ----
## gnr ----
# run through gnr_resolve to fix any spellin
resolved_spellings_raw <- select(bodysize_joined, original.taxa.name) %>% 
  
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
    resolved.source.gnr = ifelse(
      "matched_name2" %in% colnames(resolved),
      resolved$data_source_title,
      NA
    )
  ) %>% 
  # remove excess info
  select(-resolved)

# Save
saveRDS(resolved_spellings_raw, file = "R/Data_outputs/taxonomy/resolved_spellings_raw.rds")

## manual ----
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
NAs <- resolved_spellings_raw %>% 
  filter(
    is.na(resolved.taxa.name.gnr)
  )

# Bumped up a taxonomic level:
## Find all that has a space in the original.taxa.name and not in the resolved.taxa.name as this will indicate that it was two names (species) and now one name (higher rank)
bumped_up <- resolved_spellings_raw %>% 
  mutate(
    bumped = case_when(
      stri_detect_regex(original.taxa.name, " ") & !(stri_detect_regex(resolved.taxa.name.gnr, " ")) ~ "bumped",
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
resolved_wrong <- resolved_spellings_raw %>% 
  mutate(
    wrong = case_when(
      resolved.taxa.name.gnr %in% c("Cyst", "Nauplius", "Centric", "Centric diatom", "Volvocales", "Cyclops") ~ "yes",
      TRUE ~ "no"
    )
  ) %>% 
  filter(
    wrong == "yes"
  )

# Special charaters:
spec_char <- resolved_spellings_raw %>% 
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
manually_resolved <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "resolve")

# Join the manually resolved names from the manually_resolved
resolved_spellings <- resolved_spellings_gnr %>% 
  # left join all the manually resolved ones from manual_resolve spreadsheet
  left_join(
    manually_resolved,
    by = "original.taxa.name"#,
    #suffix = c(".gnr", ".manual")
  ) %>% 
  
  # select the ones left joined in
  mutate(
    resolved.taxa.name.spelling = case_when(
      resolved.taxa.name.manual == "couldn't resolve" ~ NA, # change the ones that couldn't be resolved to NA
      !is.na(resolved.taxa.name.manual) ~ resolved.taxa.name.manual,
      TRUE ~ resolved.taxa.name.gnr
    ),
    
    resolved.source.spelling = case_when(
      resolved.taxa.name.manual == "couldn't resolve" ~ NA,
      !is.na(resolved.source.manual) ~ resolved.source.manual,
      TRUE ~ resolved.source.gnr
    )
  ) %>% 

  select(
    original.taxa.name,
    resolved.taxa.name.spelling,
    resolved.source.spelling
  )

# Save
saveRDS(resolved_spellings, file = "R/Data_outputs/taxonomy/resolved_spellings.rds")
# Don't want to get distinct resolved names yet as this will be used to left join onto data by original.taxa.name so need to keep all of them


# Synonymns ----
# Run through gnr_resolve again and set to access the classification pathway as this gives taxonomic info
# Get information for all sources that give a pathway and then decide which ones to use for resolving names
  # Algaebase was decided to resolved synonyms for phytoplankton as it was the only one that handled synonyms the best and gave a pathway - zooplankton synonyms will be done with gbif but through classification() later on as this function is better for handling synonyms for that api
# Extract out the algae base synonyms

## Initial gnr run ----
# get all the raw data from each source from gnr to extract later

resolved_synoymns_raw <- resolved_taxa_list %>% 
  # Get list of distinct resolved.taxa.names to speed it up
  distinct(
    resolved.taxa.name.spelling
    ) %>% 
  
  # Run through gnr_resolve
  mutate(
    # set to all fields to get taxonomy from all the APIs that is avaiable
    taxonomy = list(gnr_resolve(sci = resolved.taxa.name.spelling, http = "post", canonical = TRUE, fields = "all"))
  )

# Save
saveRDS(resolved_synonymns_raw, file = "R/Data_outputs/taxonomy/resolved_synoymns_raw.rds")

## Extract info ----
# Extract out all the sources into separate columns

resolved_synoymns_extracted <- resolved_synoymns_raw %>% 
  # rename taxonomy column for ease of understanding
  rename(
    tax.raw = taxonomy
  ) %>% 
  
  mutate(
    ## Separate out sources
    
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
    
    ## Extract information from sources
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
saveRDS(resolved_synoymns_extracted, file = "R/Data_outputs/taxonomy/resolved_synoymns_extracted.rds")


## Resolve synonyms ----
# change the resolved.taxa.name to the taxa.name from algae base when it is different

resolved_synonymns <- resolved_synoymns_gnr_extracted %>% 
  
  # resolve synonyms 
  mutate(
    # extract the taxa.names out into their own columns
    synonym.ab = ifelse(
      "names" %in% colnames(tax.ab), # if the is a column called id
      as.character(tax.ab$names[nrow(tax.ab)]), # select the last row 
      NA_character_),
    
    # replace the resolved.taxa.name with any resolved ones - no fuzy matching with gnr_resolve so when it doesn't recognise the resolved.taxa.name it won't give a wrong one
    resolved.taxa.name.synonym = case_when(
      synonym.ab == "" ~ resolved.taxa.name.spelling,
      is.na(synonym.ab) ~ resolved.taxa.name.spelling,
      synonym.ab == resolved.taxa.name.spelling ~ resolved.taxa.name.spelling,
      synonym.ab != resolved.taxa.name.spelling ~ synonym.ab,
      TRUE ~ resolved.taxa.name.spelling
    ),
    
    resolved.source.synonym = if_else(
      resolved.taxa.name.spelling != resolved.taxa.name.synonym,
      "algaebase",
      NA
      )
  )%>% 
    
  # select columns
  select(
    resolved.taxa.name.synonym,
    resolved.taxa.name.spelling,
    resolved.source.synonym
  )

# Save
saveRDS(resolved_synonymns, file = "R/Data_outputs/taxonomy/resolved_synonymns.rds")

# Final list ----

## Merge spelling and synonyms ----
resolved_taxa_list <- left_join(resolved_spellings, resolved_synonymns, by = "resolved.taxa.name.spelling") %>% 
  mutate(
    resolved.taxa.name = if_else(
      resolved.taxa.name.spelling != resolved.taxa.name.synonym,
      resolved.taxa.name.synonym,
      resolved.taxa.name.spelling
    ),
    
    resolved.source = if_else(
      is.na(resolved.source.synonym),
      resolved.source.spelling,
      resolved.source.synonym
    )
  ) %>% 
  
  select(
    resolved.taxa.name,
    resolved.source,
    original.taxa.name
  )

# Save
saveRDS(resolved_taxa_list, file = "R/Data_outputs/taxonomy/resolved_taxa_list.rds")

















