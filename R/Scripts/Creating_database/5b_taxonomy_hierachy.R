## get taxonomic hierachy for resolved names
# run through gnr_resolve and classification 21/10/2024

# Break down of things:
  # taxonomy_step_1 <- list of taxonomic hierachy for each taxa taken from gnr_resolve from algae base - ones that didn't have info from algaebase will be NA
  # taxonomy_step_2 <- list of taxonomic hierachy with the NAs filled in from gbif by running through classification with rows = 1 turned on to get best match
  # taxonomy_step_3 <- list of taxonomic hierachy after correcting any that were bumped up a taxonomic level by classification() by rerunning classification with rows = 1 turned off so i could manually change any that needed to

## Diplopora oregonensis - extinct and removed

# Step 1: GNR ----
## Getting taxonomic hierachy from algaebase by running through gnr_resolve

## Initial run through gnr ----
taxonomy_raw_step_1 <- resolved_taxa_list %>% 
  # remove any replicated
  distinct(
    resolved.taxa.name
  ) %>% 
  
  # Run through gnr_resolve
  mutate(
    # set to all fields to get taxonomy from all the APIs that is avaiable - usefull to have
    taxonomy = list(gnr_resolve(sci = resolved.taxa.name, http = "post", canonical = TRUE, fields = "all")),
  ) %>% 
  
  # make a column for tax.uid for joining in later steps
  as.data.frame(taxonomy_raw_step_1) %>% # need to make a data frame to remove the rowwise from saving as an rds 
  
  mutate(
    tax.uid = paste("tax", row_number(), sep = "-")
  )
  
## Save
saveRDS(taxonomy_raw_step_1, file = "R/Data_outputs/taxonomy/taxonomy_raw_step_1.rds")


## Extract data ----
## Get the data so that I have all the taxonomy for each source in its own column in a list for each row

taxonomy_extracted_step_1 <- taxonomy_raw_step_1 %>% 
  # rename taxonomy column for ease of understanding
  rename(
    tax.raw = taxonomy
  ) %>% 
  
  rowwise() %>% # set to rowwise because as.data.frame removed this in the previous step
  
  mutate(
    # Separate out sources into their own columns

    ab.raw = ifelse("AlgaeBase" %in% tax.raw$data_source_title, # if algae base is in that column
                    list( # make it a list so that it will return that line of information
                      filter( 
                        tax.raw, tax.raw$data_source_title == "AlgaeBase") %>% # select ones from algaebase
                        slice(1) # some have more than one entry for the same database so select just the first one
                    ),
                    list(data.frame(user_supplied_name = NA)) # else make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
                    ),  
    
    tol.raw = ifelse("Open Tree of Life Reference Taxonomy" %in% tax.raw$data_source_title, # if tol is in that column
                     list( # make it a list so that it will return that line of information
                       filter( 
                         tax.raw, tax.raw$data_source_title == "Open Tree of Life Reference Taxonomy") %>% # select ones from tol
                         slice(1) # some have more than one entry for the same database so select just the first one
                     ),
                     list(data.frame(user_supplied_name = NA))),  # else make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    itis.raw = ifelse("Integrated Taxonomic Information SystemITIS" %in% tax.raw$data_source_title, # if itis is in that column
                      list( # make it a list so that it will return that line of information
                        filter( 
                          tax.raw, tax.raw$data_source_title == "Integrated Taxonomic Information SystemITIS") %>% # select ones from itis
                          slice(1) # some have more than one entry for the same database so select just the first one
                      ),
                      list(data.frame(user_supplied_name = NA))),  # else make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    gbif.raw = ifelse("GBIF Backbone Taxonomy" %in% tax.raw$data_source_title, # if gbif is in that column
                      list( # make it a list so that it will return that line of information
                        filter( 
                          tax.raw, tax.raw$data_source_title == "GBIF Backbone Taxonomy") %>% # select ones from gbif
                          slice(1) # some have more than one entry for the same database so select just the first one
                      ),
                      list(data.frame(user_supplied_name = NA))
                      ),  # else make a new dataframe with user_supplied_name and assign it NA so that in later steps there is a common column between all rows
    
    # Format it into a table within each cell
    ## Algaebase
    tax.ab = ifelse(
      is.na(ab.raw$user_supplied_name), # if there if no taxonomy info
      list( # then make a dataframe of NAs
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      list( # else make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
        data.frame(
          names = unlist(stri_split_fixed(ab.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(ab.raw$classification_path_ranks, "|"))
        )
      )
    ),
    
    # Tree of life
    tax.tol = if_else(
      is.na(tol.raw$user_supplied_name),  # if there if no taxonomy info
      list( # then make a dataframe of NAs
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      list( # else make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
        data.frame(
          names = unlist(stri_split_fixed(tol.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(tol.raw$classification_path_ranks, "|"))
        )
      )
    ),
    
    # ITIS
    tax.itis = if_else(
      is.na(itis.raw$user_supplied_name),  # if there if no taxonomy info
      list( # then make a dataframe of NAs
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      list( # else make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
        data.frame(
          names = unlist(stri_split_fixed(itis.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(itis.raw$classification_path_ranks, "|"))
        )
      )
    ),
    
    # GBIF
    tax.gbif = if_else(
      is.na(gbif.raw$user_supplied_name),  # if there if no taxonomy info
      list( # then make a dataframe of NAs
        data.frame(
          names = NA,
          ranks = NA
        )
      ),
      list( # else make a taxonomy dataframe by taking the information in classification_path for the names and classification_path_ranks for the corresponding rank
        data.frame(
          names = unlist(stri_split_fixed(gbif.raw$classification_path, "|")),
          ranks = unlist(stri_split_fixed(gbif.raw$classification_path_ranks, "|"))
        )
      )
    )
  ) %>% 
  
  # Remove columns I don't want
  select(
    -ab.raw:-gbif.raw
  ) 

# Save
saveRDS(taxonomy_extracted_step_1, file = "R/Data_outputs/taxonomy/taxonomy_extracted_step_1.rds")

## Assign taxonomy ----
# Separate the taxonomy data frame within each row into columns in the main data frame
taxonomy_step_1 <- taxonomy_extracted_step_1 %>% 
  
  mutate(
    # separate out each rank into it's own column
    
    form = ifelse(
      "form" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "form"],
      NA_character_),
    
    variety = ifelse(
      "variety" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "variety"],
      NA_character_),
    
    species = ifelse(
      "species" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "species"],
      NA_character_),
    
    genus = ifelse(
      "genus" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "genus"],
      NA_character_),
    
    family = ifelse(
      "family" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "family"],
      NA_character_),
    
    order = ifelse(
      "order" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "order"],
      NA_character_),
    
    class = ifelse(
      "class" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "class"],
      NA_character_),
    
    phylum = ifelse(
      "phylum" %in% tax.ab$ranks,
      tax.ab$names[tax.ab$ranks == "phylum"],
      NA_character_),
    
    # algae base doesnt have kingdom so that can be don later on
    kingdom = NA,
    
    rank = ifelse(
      "ranks" %in% colnames(tax.ab),
      as.character(tax.ab$ranks[nrow(tax.ab)]),
      NA_character_),
    
    source = "AlgaeBase" # say where the info comes from
  ) %>% 
  
  # Select columns I want
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
## make a subset of the species from taxonomy_step_1 that didn't get any info from gnr

taxonomy_raw_step_2 <- taxonomy_step_1 %>% 
  
  # select columns and rows I want
  filter(
    is.na(phylum) # algaebase only went up to phylum so any with NA phylum are ones that didn't get picked up by algaebase
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
# Separate the taxonomy data frame within each row into columns in the main data frame

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
  
  # select the correct info for each
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
  
  # Keep resolved.taxa.name.ab and this has all the resolved names
  rename(
    resolved.taxa.name = resolved.taxa.name.ab
  ) %>% 
  
  # select columns I want
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
# select all the desired ones base on above criteria and run through classification

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
# Separate the taxonomy data frame within each row into columns in the main data frame

classification_fix_extracted <- classification_fix_raw %>% 
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
    
    source = "gbif",
    
    # add in a column so I know which ones the select when left joining later
    class.fix = "yes"
  ) %>% 
  
  # select columns I want
  select(
    -taxonomy.gbif
  )

# Save
saveRDS(classification_fix_extracted, file = "R/Data_outputs/taxonomy/classification_fix_extracted.rds")

## Combine subset with rest of data ----
taxonomy_step_3 <- left_join(
  taxonomy_step_2,
  classification_fix_extracted,
  by = "tax.uid",
  suffix = c(".initial", ".class.fix")
  ) %>% 
  
  # keep resolved.taxa.name.initial as this has all the resolved taxa names
  rename(
    resolved.taxa.name = resolved.taxa.name.initial
  ) %>% 

  mutate(
    # select desired information - when class.fix is avaiable select otherwise keep initial
    form = if_else(
      !is.na(class.fix),
      form.class.fix,
      form.initial
    ),
    
    variety = if_else(
      !is.na(class.fix),
      variety.class.fix,
      variety.initial
    ),
    
    species = if_else(
      !is.na(class.fix),
      species.class.fix,
      species.initial
    ),
    
    genus = if_else(
      !is.na(class.fix),
      genus.class.fix,
      genus.initial
    ),
    
    family = if_else(
      !is.na(class.fix),
      family.class.fix,
      family.initial
    ),
    
    order = if_else(
      !is.na(class.fix),
      order.class.fix,
      order.initial
    ),
    
    class = if_else(
      !is.na(class.fix),
      class.class.fix,
      class.initial
    ),
    
    phylum = if_else(
      !is.na(class.fix),
      phylum.class.fix,
      phylum.initial
    ),
    
    kingdom = if_else(
      !is.na(class.fix),
      kingdom.class.fix,
      kingdom.initial
    ),
    
    rank = if_else(
      !is.na(class.fix),
      rank.class.fix,
      rank.initial
    ),
    
    source = if_else(
      !is.na(class.fix),
      source.class.fix,
      source.initial
    ),
    
    # change synonyms for ranks to the same
    rank = case_when(
      rank == "forma" ~ "form",
      TRUE ~ rank
    )
  ) %>% 
  
  # select columns I want
  select(
    resolved.taxa.name,
    tax.uid,
    form:source
  )

# Save
saveRDS(taxonomy_step_3, file = "R/Data_outputs/taxonomy/taxonomy_step_3.rds")

# Step 4 - Manually fix the rest ----

## Find bumped up ones ----
# Criteria to look for:
  # Ones that weren't picked up by classification (NA)
  # Ones that were at a variety/form level but have the species missing
  # Ones that have been bumped up a taxonomic level
  # Find ones that have blanks in random columns - "" or NA
# If not correct then put into a separate excel sheet to edit

manual_taxonomy <- taxonomy_step_3 %>% 
  mutate(
    check = case_when(
      # Bumped up a level - select everything that isn't species and go through and select the ones that were bumped up - some resolved taxa.names were higher than a species anyway so these will be kept as is
      rank == "kingdom" & resolved.taxa.name != kingdom ~ "check",
      rank == "phylum" & resolved.taxa.name != phylum ~ "check",
      rank == "class" & resolved.taxa.name != class ~ "check",
      rank == "order" & resolved.taxa.name != order ~ "check",
      rank == "family" & resolved.taxa.name != family ~ "check",
      
      # bumped from species to genus - ones that had a space in the original name indicating they were likely a species but have a rank of genus
      stri_detect_regex(resolved.taxa.name, " ") & rank == "genus" ~ "check",
      
      # all the sub species were bumped
      rank == "subspecies" ~ "check",
      
      # NAs
      is.na(rank) ~ "check",
      rank == "" ~ "check",
      
      # blanks
      kingdom == "" ~ "check",
      phylum == "" ~ "check",
      class == "" ~ "check",
      order == "" ~ "check",
      family == "" ~ "check",
      genus == "" ~ "check",
      species == "" ~ "check",
      
      is.na(species) & rank %in% c("form", "variety") ~ "check",
      is.na(genus) & rank %in% c("form", "variety", "species") ~ "check",
      is.na(family) & rank %in% c("form", "variety", "species", "genus") ~ "check",
      is.na(order) & rank %in% c("form", "variety", "species", "genus", "family") ~ "check",
      is.na(class) & rank %in% c("form", "variety", "species", "genus", "family", "order") ~ "check",
      is.na(phylum) & rank %in% c("form", "variety", "species", "genus", "family", "order", "class") ~ "check",
      is.na(kingdom) & rank %in% c("form", "variety", "species", "genus", "family", "order", "class", "phylum") ~ "check",
      TRUE ~ NA)
    ) %>% 
  
  filter(
    check == "check"
  )

write_csv(manual_taxonomy, "R/Data_outputs/Taxonomy/manual_taxonomy.csv")

## Import manually fixed version ----
manual_taxonomy_fixed <- read_xlsx(here("Raw_data","manual_taxonomy.xlsx"), sheet = "taxonomy")

## Join to data ----
taxonomy_step_4 <- taxonomy_step_3 %>% 
  left_join(
    manual_taxonomy_fixed,
    by = "tax.uid",
    suffix = c(".initial", ".manual")
  ) %>% 
  
  # Rename resolve.taxa.name.intial as this is the full resolved.taxa.name list
  rename(
    resolved.taxa.name = resolved.taxa.name.initial
  ) %>% 
  
  # replace missing info with manually updated info
  mutate(
    form = if_else(
      !is.na(manual.fix),
      form.manual,
      form.initial
    ),
    
    variety = if_else(
      !is.na(manual.fix),
      variety.manual,
      variety.initial
    ),
    
    species = if_else(
      !is.na(manual.fix),
      species.manual,
      species.initial
    ),
    
    genus = if_else(
      !is.na(manual.fix),
      genus.manual,
      genus.initial
    ),
    
    family = if_else(
      !is.na(manual.fix),
      family.manual,
      family.initial
    ),
    
    order = if_else(
      !is.na(manual.fix),
      order.manual,
      order.initial
    ),
    
    class = if_else(
      !is.na(manual.fix),
      class.manual,
      class.initial
    ),
    
    phylum = if_else(
      !is.na(manual.fix),
      phylum.manual,
      phylum.initial
    ),
    
    kingdom = if_else(
      !is.na(manual.fix),
      kingdom.manual,
      kingdom.initial
    ),
    
    rank = if_else(
      !is.na(manual.fix),
      rank.manual,
      rank.initial
    ),
    
    source = if_else(
      !is.na(manual.fix),
      source.manual,
      source.initial
      )
    ) %>% 
    
    # select columns I want
    select(
      resolved.taxa.name,
      tax.uid,
      form:source
    )

# Save
saveRDS(taxonomy_step_4, file = "R/Data_outputs/taxonomy/taxonomy_step_4.rds")

# Step 5: fix final things with taxonomy ----

final_taxonomy <- taxonomy_step_4 %>% 
  mutate(
    phylum = case_when(
      phylum =="Bacteroidota" ~ "Bacteroidetes",
      phylum == "Cryptista" ~ "Cryptophyta",
      phylum == "Euglenophyta" ~ "Discomitochondria",
      phylum == "Mycetozoa" ~ "Amoebozoa",
      phylum == "Heterokontophyta" ~ "Ochrophyta",
      TRUE ~ phylum
    ),
    
    rank = case_when(
      rank %in% c("variety", "form") ~ "species",
      TRUE ~ rank
    ),
    
    accepted.taxa.name = case_when(
      rank == "species" ~ species,
      rank == "genus" ~ genus,
      rank == "family" ~ family,
      rank == "order" ~ order,
      rank == "class" ~ class,
      rank == "phylum" ~ phylum,
      rank == "kingdom" ~ kingdom,
      TRUE ~ NA
    )
  ) %>% 
  select(
    - form,
    - variety
  ) %>% 
  
  mutate(
    # Add a group column for whether it is phyto plankton or zooplankton
    group = case_when(
      kingdom == "Animalia" ~ "zooplankton",
      TRUE ~ "phytoplankton"
    )
  ) %>% 
  
  filter(
    !is.na(accepted.taxa.name)
  ) %>% 
  
  relocate(
    tax.uid,
    resolved.taxa.name,
    accepted.taxa.name,
    rank,
    species,
    genus,
    family,
    order,
    class,
    phylum,
    kingdom,
    group,
    source
  )
# Save
saveRDS(final_taxonomy, file = "R/Data_outputs/taxonomy/final_taxonomy.rds")

# Add to all data ----

bodysize_taxonomy <- bodysize_joined %>% 
  
  # left join the resolved taxa names to the 
  left_join(
    select(
      resolved_taxa_list, original.taxa.name, resolved.taxa.name
    ), by = "original.taxa.name"
  ) %>% 
  
  # join the taxonomy data
  left_join(
    final_taxonomy,
    by = "resolved.taxa.name"
  ) %>% 
  
  select(
    -resolved.taxa.name,
    -original.taxa.name
  )

# Save
saveRDS(bodysize_taxonomy, file = "R/Data_outputs/databases/bodysize_taxonomy.rds")