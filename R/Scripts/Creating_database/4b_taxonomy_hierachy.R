## get taxonomic hierachy for resolved names

# Break down of things:
  # taxonomy_step_1 <- list of taxonomic hierachy for each taxa taken from gnr_resolve from algae base - ones that didn't have info from algaebase will be NA
  # taxonomy_step_2 <- list of taxonomic hierachy with the NAs filled in from gbif by running through classification with rows = 1 turned on to get best match

  # taxonomy_step_1 <- initial list of taxonomic hierachy after runnin resolved.taxa.name through classification() without any incorect ones bein fixed
  # taxonomy_step_2 <- list of taxonomic hierachy after correcting any that were bumped up a taxonomic level by classification() by rerunning classification with rows = 1 turned off so i could manually change any that needed to
  # taxonomy_step_3 <- list of taxonomic hierachy after manually editing any that were still bumped up a taxonomic level or just weren't picked up by classificatiom()


# Step 1: GNR ----
# getting taxonomic hierachy from algaebase when it is available
# run resolved_taxa_list through gnr_resolve and get info for all sources again because it is just usefull to have

## Initial run through gnr ----
taxonomy_raw_step_1 <- resolved_taxa_list %>% 
  # remove any replicated
  distinct(
    resolved.taxa.name
  ) %>% 
  
  # Run through gnr_resolve
  mutate(
    # set to all fields to get taxonomy from all the APIs that is avaiable
    taxonomy = list(gnr_resolve(sci = resolved.taxa.name, http = "post", canonical = TRUE, fields = "all")),
  ) %>% 
  
  # make a column for tax.uid for joining late
  # need to make a data frame to remove the rowwise from saving as an rds 
  as.data.frame(taxonomy_raw_step_1) %>% 
  
  mutate(
    tax.uid = paste("tax", row_number(), sep = "-")
  )
  
# Save
saveRDS(taxonomy_raw_step_1, file = "R/Data_outputs/taxonomy/taxonomy_raw_step_1.rds")


## Extract data ----
taxonomy_extracted_step_1 <- taxonomy_raw_step_1 %>% 
  # rename taxonomy column for ease of understanding
  rename(
    tax.raw = taxonomy
  ) %>% 
  
  rowwise() %>% 
  
  mutate(
    ## Separate out sources
    
    # assign the information for each source to their own columns
    ab.raw = ifelse("AlgaeBase" %in% tax.raw$data_source_title,
                    list( # make it a list so that it will return that line of information
                      filter( 
                        tax.raw, tax.raw$data_source_title == "AlgaeBase") %>% # select ones from algaebase
                        slice(1) # some have more than one entry for the same database so select just the first one
                    ),
                    list(data.frame(user_supplied_name = NA)) # need to make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
                    ),  
    
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
                      list(data.frame(user_supplied_name = NA))
                      ),  # need to make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    ## Extract information from sources
    # Algaebase
    tax.ab = ifelse(
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
saveRDS(taxonomy_extracted_step_1, file = "R/Data_outputs/taxonomy/taxonomy_extracted_step_1.rds")

## Assign taxonomy ----
taxonomy_step_1 <- taxonomy_extracted_step_1 %>% 
  mutate(
    # separate out each rank into it's own column
    form = ifelse("form" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "form"], NA_character_),
    variety = ifelse("variety" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "variety"], NA_character_),
    species = ifelse("species" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "species"], NA_character_),
    genus = ifelse("genus" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "genus"], NA_character_),
    family = ifelse("family" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "family"], NA_character_),
    order = ifelse("order" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "order"], NA_character_),
    class = ifelse("class" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "class"], NA_character_),
    phylum = ifelse("phylum" %in% tax.ab$ranks, tax.ab$names[tax.ab$ranks == "phylum"], NA_character_),
    
    # algae base doesnt have kingdom so that can be don later on
    kingdom = NA,
    rank = ifelse(
      "ranks" %in% colnames(tax.ab),
      as.character(tax.ab$ranks[nrow(tax.ab)]),
      NA_character_),
    source = "AlgaeBase" # say where the info comes from
  ) %>% 
  
  select(
    resolved.taxa.name,
    tax.uid,
    form:source
  )

# Save
saveRDS(taxonomy_step_1, file = "R/Data_outputs/taxonomy/taxonomy_step_1.rds")
  

# Step 2: Classification ----
# get the taxonomy for the remaining taxa from gbif - not using the info from gnr for gbif because it's not as good with the synonyms and classification allows to select options with the fuzy matching

## Subset species ----
# make a subset of the species from taxonomy_step_1 that didn't get any info from gnr

taxonomy_raw_step_2 <- taxonomy_step_1 %>% 
  
  # select columns and rows I want
  filter(
    is.na(phylum)
  ) %>% 
  
  select(
    tax.uid,
    resolved.taxa.name
  ) %>% 
  
  mutate(
    taxonomy = list(classification(resolved.taxa.name, db = "gbif", return_id = TRUE, rows = 1)[[1]])
  )

# Save
saveRDS(taxonomy_raw_step_2, file = "R/Data_outputs/taxonomy/taxonomy_raw_step_2.rds")
  
## Extract info ----
taxonomy_extracted_step_2 <- taxonomy_raw_step_2 %>% 
  mutate(
    # Set all taxa that didn't come back with a name to NA
    taxonomy = ifelse(
      "name" %in% colnames(taxonomy),
      list(taxonomy),
      NA
    ),
    
    # Extract information
    form = ifelse(
      "form" %in% taxonomy$rank, # if form is present in the rank column
      taxonomy$name[taxonomy$rank == "form"], # select the row of the name column that matches the row for form
      NA_character_),
    variety = ifelse(
      "variety" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "variety"],
      NA_character_),
    species = ifelse(
      "species" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "species"],
      NA_character_),
    genus = ifelse(
      "genus" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "genus"],
      NA_character_),
    family = ifelse(
      "family" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "family"],
      NA_character_),
    order = ifelse(
      "order" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "order"],
      NA_character_),
    class = ifelse(
      "class" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "class"],
      NA_character_),
    phylum = ifelse(
      "phylum" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "phylum"],
      NA_character_),
    kingdom = ifelse(
      "kingdom" %in% taxonomy$rank,
      taxonomy$name[taxonomy$rank == "kingdom"],
      NA_character_),
    rank = ifelse(
      "rank" %in% colnames(taxonomy),
      as.character(taxonomy$rank[nrow(taxonomy)]),
      NA_character_),
    
    # source
    source = "gbif"
  ) %>% 
  
  select(
    -taxonomy
  )

# Save
saveRDS(taxonomy_extracted_step_2, file = "R/Data_outputs/taxonomy/taxonomy_extracted_step_2.rds")

## Add all together ----
# join the info from step 2 to step 1
taxonomy_step_2 <- left_join(
  taxonomy_step_1,
  taxonomy_extracted_step_2,
  by = "tax.uid",
  suffix = c(".ab", ".gbif")
  ) %>% 
  mutate(
    form = if_else(
      !is.na(source.gbif),
      form.gbif,
      form.ab
    ),
    
    variety = if_else(
      !is.na(source.gbif),
      variety.gbif,
      variety.ab
    ),
    
    species = if_else(
      !is.na(source.gbif),
      species.gbif,
      species.ab
    ),
    
    genus = if_else(
      !is.na(source.gbif),
      genus.gbif,
      genus.ab
    ),
    
    family = if_else(
      !is.na(source.gbif),
      family.gbif,
      family.ab
    ),
    
    class = if_else(
      !is.na(source.gbif),
      class.gbif,
      class.ab
    ),
    
    order = if_else(
      !is.na(source.gbif),
      order.gbif,
      order.ab
    ),
    
    phylum = if_else(
      !is.na(source.gbif),
      phylum.gbif,
      phylum.ab
    ),
    
    kingdom = if_else(
      !is.na(source.gbif),
      kingdom.gbif,
      kingdom.ab
    ),
    
    rank = if_else(
      !is.na(rank.gbif),
      rank.gbif,
      rank.ab
    ),
    
    source = if_else(
      !is.na(source.gbif),
      source.gbif,
      source.ab
    ),
  ) %>% 
  
  rename(
    resolved.taxa.name = resolved.taxa.name.ab
  ) %>% 
  
  select(
    resolved.taxa.name,
    tax.uid,
    form:source
  )

# Save
saveRDS(taxonomy_step_2, file = "R/Data_outputs/taxonomy/taxonomy_step_2.rds")


# Step 3 - Fix bumped up ones with classiciation ----
# select all ones that have been bumped up taxonomic levels by and then rerun classification with rows = 1 off so that I can manually select any that have multiple options
# Ones that have been selected:
  # select ones that have a rank other than species some of these will be valid as there are higher taxonomic ranks for the resolved name but not many so easier to just check through all
  # for genus select ones that had a species name as resolved name by looking for a space in the resolved name but has been given a rank of genus
# How they have been manually fixed:
  # when the higher rank is correct then select that
  # when a correct synonym or correct version is there then select that
  # when the species is valid but gbif isn't giving the species it is noted down and then changed later on

## Subset incorrect ones and run classification ----
classification_fix_raw <- taxonomy_step_2 %>% 
  # select ones I want to go back over based on above criteria
  mutate(
    check = case_when(
      rank %in% c("kingdom", "phylum", "class", "order", "family") ~ "check",
      stri_detect_regex(resolved.taxa.name, " ") & rank == "genus" ~ "check",
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    check == "check"
  ) %>% 
  
  # select columns i want - resolved.taxa.name column to run through classification and tax.uid to left join
  select(
    resolved.taxa.name,
    tax.uid
  ) %>% 
  
  # run through classification - rows = 1 turned off for manual checking
  mutate(
    taxonomy.gbif = list(classification(resolved.taxa.name, db = "gbif", return_id = TRUE)[[1]])
  )

# Save
saveRDS(classification_fix_raw, file = "R/Data_outputs/taxonomy/classification_fix_raw.rds")

## Extract info ----

classification_fix_extracted <- classification_fix_raw %>% 
  #rowwise() %>% 
  mutate(
    # Set all taxa that didn't come back with a name to NA
    taxonomy.gbif = ifelse(
      "name" %in% colnames(taxonomy.gbif),
      list(taxonomy.gbif),
      NA
    ),
    
    # Extract information
    form = ifelse(
      "form" %in% taxonomy.gbif$rank, # if form is present in the rank column
      taxonomy.gbif$name[taxonomy.gbif$rank == "form"], # select the row of the name column that matches the row for form
      NA_character_),
    variety = ifelse(
      "variety" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "variety"],
      NA_character_),
    species = ifelse(
      "species" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "species"],
      NA_character_),
    genus = ifelse(
      "genus" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "genus"],
      NA_character_),
    family = ifelse(
      "family" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "family"],
      NA_character_),
    order = ifelse(
      "order" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "order"],
      NA_character_),
    class = ifelse(
      "class" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "class"],
      NA_character_),
    phylum = ifelse(
      "phylum" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "phylum"],
      NA_character_),
    kingdom = ifelse(
      "kingdom" %in% taxonomy.gbif$rank,
      taxonomy.gbif$name[taxonomy.gbif$rank == "kingdom"],
      NA_character_),
    rank = ifelse(
      "rank" %in% colnames(taxonomy.gbif),
      as.character(taxonomy.gbif$rank[nrow(taxonomy.gbif)]),
      NA_character_),
    
    source = "gbif"
  )

# Save
saveRDS(classification_fix_extracted, file = "R/Data_outputs/taxonomy/classification_fix_extracted.rds")

## Combine subset with rest of data ----
taxonomy_step_3 <- left_join(
  taxonomy_step_2,
  classification_fix_extracted,
  by = "tax.uid",
  suffix = c(".initial", ".manual")
  ) %>% 
  
  mutate(
    # select desired information - when manual is avaiable select otherwise keep initial
    form = if_else(
      !is.na(form.manual),
      form.manual,
      form.initial
    ),
    
    variety = if_else(
      !is.na(variety.manual),
      variety.manual,
      variety.initial
    ),
    
    species = if_else(
      !is.na(species.manual),
      species.manual,
      species.initial
    ),
    
    genus = if_else(
      !is.na(genus.manual),
      genus.manual,
      genus.initial
    ),
    
    family = if_else(
      !is.na(family.manual),
      family.manual,
      family.initial
    ),
    
    order = if_else(
      !is.na(order.manual),
      order.manual,
      order.initial
    ),
    
    class = if_else(
      !is.na(class.manual),
      class.manual,
      class.initial
    ),
    
    phylum = if_else(
      !is.na(phylum.manual),
      phylum.manual,
      phylum.initial
    ),
    
    kingdom = if_else(
      !is.na(kingdom.manual),
      kingdom.manual,
      kingdom.initial
    ),
    
    rank = if_else(
      !is.na(rank.manual),
      rank.manual,
      rank.initial
    ),
    
    source = if_else(
      !is.na(source.manual),
      source.manual,
      source.initial
    )
  ) %>% 
  
  select(
    resolved.taxa.name.initial,
    
    form:source
  ) %>% 
  
  rename(
    resolved.taxa.name = resolved.taxa.name.initial
  )

# Save
saveRDS(taxonomy_step_3, file = "R/Data_outputs/taxonomy/taxonomy_step_3.rds")

# Step 3 - Manually fix the rest ----

## Find bumped up ones ----
# Find all that were picked up by classification (NA)
# Find all ones that have been bumped up a taxonomic level by first looking through all the ones that are a rank higher than genus and checking if thats correct
# If not correct then put into a separate excel sheet to edit
manual_fix <- taxonomy_step_2 %>% 
  mutate(
    check = case_when(
      rank %in% c("kingdom", "phylum", "class", "order", "family") ~ "check",
      stri_detect_regex(resolved.taxa.name, " ") & rank == "genus" ~ "check",
      is.na(gbif.id) ~ "check",
      TRUE ~ NA
    )
  ) %>% 
  
  filter(
    check == "check"
  )

x <- taxonomy_step_2 %>% 
  filter(
    is.na(gbif.id)
  )

write_csv(x, "x.csv")

# kingdom - 41 , 14 (all wrong)
# phylum - 12, 7 (2 wrong)
# class - 24, 14 (all correct)
# order - 22, 16 (1 wrong)
# family - 45 (all correct)
# genus


