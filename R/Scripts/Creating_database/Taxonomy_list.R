# Adding taxonomy data to the species list 
# data ran through taxize on 21/5/2024

# packages
library(tidyr)
library(dplyr)
library(taxize)
library(stringi)
library(purrr)
library(tibble)
library(taxadb)

# Import data
all_raw <- readRDS("Data/data_processing/all_raw.rds")

##### Resolve names ----

## 1) Initial run through gnr_resolve to fix any spelling mistakes
resolved_gnr_raw <- select(all_raw, original.taxa.name) %>% 
  # Select all distinct original.taxa.names from all_raw
  distinct(original.taxa.name, .keep_all = TRUE)%>% 
  rowwise() %>% 
  mutate(
    # Run through resolver
    resolved = list(gnr_resolve(sci = original.taxa.name, http = "post", canonical = TRUE, best_match_only = TRUE)),
    
    # extract information
    taxa.name = ifelse(
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
saveRDS(resolved_gnr_raw, file = "Data/taxize/resolved_gnr_raw.rds")

## 2) Manually resolve any names that didn't get picked up on first run through or were resolved wrong
resolved_gnr_manual <- resolved_gnr_raw %>% 
  mutate(
    resolved.source.gnr = ifelse(
      is.na(taxa.name)|
        stri_detect_regex(original.taxa.name, '\\b(?i)var.|\\b(?i)f.|(?i)\\bNauplii\\b|(?i)\\bCyst\\b|(?i)\\bCysts\\b|Bythotrephes longimanus|\\b(?i)centric\\b')|
        original.taxa.name %in% c("Dinobryon anneciense", "Chydorus bicuspidatus", "Kirchneriella dichotomococcoides", "Fallacia difficillimoides", "Kephyrion gracilis", "Coelosphaerium kutzingii",
                                  "Gomphosphaeria pallidum", "Ictinogomphus ruwenzorica", "Ictinogomphus selysi", "Allocnemis singularis", "Ictinogomphus soror", "Peridiniopsis umbonatum",
                                  "Pseudostaurastrum alternans", "Cyclops bicuspidatus-thomasi", "Diacyclops bicuspidatus-thomasi", "Diacyclops bicuspifatus-thomas", "Cyclotella distinguenda-unipunctata",
                                  "Diaptomus graciloides-(M)", "Cyclops leuckarti-(M)", "Bosmina longispina-maritima", "Navicula utermhii", "Fotterella tetrachlorella", "micro Chlorophyceae", "Gomphonema ba var. icum",
                                  "Cyclops sp.", "Polypedilum subgenus Polypedilum", "Polypedilum subgenus Pentapedium")
      ,
      "Manually",
      resolved.source),
    taxa.name.gnr = case_when(
      # weird ones
      original.taxa.name == "micro Chlorophyceae" ~ "Chlorophyceae",
      original.taxa.name == "Gomphonema ba var. icum" ~ "Gomphonema bavaricum",
      stri_detect_regex(original.taxa.name, "\\b(?i)centric\\b") ~ "Bacillariophyceae",
      original.taxa.name == "Gomphonema ba var. icum" ~ "Gomphonema bavaricum",
      original.taxa.name == "Polypedilum subgenus Pentapedium" ~ "Polypedilum",
      original.taxa.name == "Polypedilum subgenus Polypedilum" ~ "Polypedilum",
      original.taxa.name == "Cyclops sp." ~ "Copepoda",
      original.taxa.name == 'Cyst of Ceratium hirundinella' ~ 'Ceratium hirundinella',
      original.taxa.name == "Daphnia hyalinaxgaleata" ~ "Daphnia",
      
      # Any that have a variety or form keep the same as the resolver can't handle them
      stri_detect_regex(original.taxa.name, '\\b(?i)var\\.|\\b(?i)f\\.') ~ original.taxa.name,
      
      # manually resolve any that were not picked up (NA)
      original.taxa.name == 'Bulbochaeta' ~ "Bulbochaete",
      original.taxa.name == 'Chlorelloidea sp.' ~ 'Myxococcoides chlorelloidea',
      original.taxa.name == 'cf Mantellum sp.' ~ 'Mantellum',
      original.taxa.name == 'Cell of Dinobryon' ~ "Dinobryon",
      original.taxa.name == 'cf. Katodinium fongiforme' ~ 'Katodinium fungiforme',
      original.taxa.name == 'Volvocale 4 flagella' ~ 'Chlamydomonadales',
      original.taxa.name == 'Palmeacea sp.' ~ 'Palmellaceae',
      original.taxa.name == 'Ulothricophyceae sp.' ~ "Chlorophyceae",
      original.taxa.name == 'Bosminids spp.' ~ "Bosminidae",
      original.taxa.name == 'Ceriopdaphnia sp.' ~ "Ceriodaphnia",
      original.taxa.name == 'Daphniids spp.' ~ "Daphniidae",
      original.taxa.name == 'Diaptomiids spp.' ~ "Diaptomus",
      original.taxa.name == 'Harpacticoids spp.' ~ "Harpacticoida",
      original.taxa.name == 'Thaumeliidae' ~ "Thaumaleidae",
      original.taxa.name == 'Adenophleboides' ~"Adenophlebiodes",
      original.taxa.name == 'Gomphidia quarreli confinii' ~ "Gomphidia quarrei",
      original.taxa.name == 'Copepodit Calanoida' ~ "Calanoida",
      original.taxa.name == 'Copepodit Cyclopoida' ~ "Cyclopoida",
      original.taxa.name == 'Anabaena flos-aquae f. lemmermannii' ~ "Dolichospermum lemmermannii",
      original.taxa.name == 'Anabaena flos-aquae var. treleasii' ~ "Dolichospermum flosaquae",
      original.taxa.name == 'Staurastrum sebaldii var. ornatum f quadriradiata' ~ "Staurastrum manfeldtii",
      original.taxa.name == 'Achnanthidium minutissima var. affinis' ~ "Achnanthidium affine",
      original.taxa.name == 'Stichtochironomus' ~ 'Chironomus',
      original.taxa.name == 'Paralauterboniella' ~ 'Paralauterborniella',
      # keep any that were not picked up but cannot be manually resolved the same as original.taxa.name
      original.taxa.name %in% c('Afroneurus ethiopicus', "Nadinatella") ~ original.taxa.name,
      
      # change any that contain cyst/nauplii/stomatocyst
      stri_detect_regex(original.taxa.name, '(?i)\\bNauplii\\b') ~ "Copepoda",
      stri_detect_regex(original.taxa.name, '(?i)\\bStomatocyst\\b') ~ "Chrysophyceae",
      stri_detect_regex(original.taxa.name, '(?i)\\bCyst\\b|(?i)\\bCysts\\b') & stri_detect_regex(original.taxa.name, '(?i)Dinobryon|(?i)péridinien') ~ "Dinophyceae",
      stri_detect_regex(original.taxa.name, '(?i)\\bCyst\\b|(?i)\\bCysts\\b') & stri_detect_regex(original.taxa.name, '(?i)Chrysophyceae|(?i)Chrysophycee') ~ "Chrysophyceae",
      
      # # Change any that were originally species but bumped to genus level 
      original.taxa.name == "Pseudostaurastrum alternans" ~ "Staurastrum alternans",
      original.taxa.name %in% c("Cyclops bicuspidatus-thomasi", "Diacyclops bicuspidatus-thomasi", "Diacyclops bicuspifatus-thomas")  ~ "Diacyclops thomasi",
      original.taxa.name == "Cyclotella distinguenda-unipunctata" ~ "Cyclotella distinguenda var. unipunctata",
      original.taxa.name == "Diaptomus graciloides-(M)" ~ "Eudiaptomus graciloides",
      original.taxa.name == "Cyclops leuckarti-(M)" ~ "Mesocyclops leuckarti",
      stri_detect_regex(original.taxa.name, "Bythotrephes longimanus") ~ "Bythotrephes longimanus",
      original.taxa.name == "Bosmina longispina-maritima" ~ "Bosmina (Eubosmina) coregoni",
      original.taxa.name == "Navicula utermhii" ~ "Navicula utermoehlii",
      original.taxa.name == "Fotterella tetrachlorella" ~ "Fotterella tetrachlorelloides",
      original.taxa.name == 'Anabaena flos-aquae' ~ "Dolichospermum flosaquae",
      original.taxa.name == 'Merismopedia tenuis' ~ "Merismopedia tenuissima",
      
      # keep the rest of the ones bumped to genus the same as original.taxa.name
      original.taxa.name %in% c("Dinobryon anneciense", "Chydorus bicuspidatus", "Kirchneriella dichotomococcoides", "Fallacia difficillimoides", "Kephyrion gracilis", "Coelosphaerium kutzingii",
                                "Gomphosphaeria pallidum", "Ictinogomphus ruwenzorica", "Ictinogomphus selysi", "Allocnemis singularis", "Ictinogomphus soror", "Peridiniopsis umbonatum") ~ original.taxa.name,
      
      # Keep the rest the same
      TRUE ~ taxa.name
    ),
    
    # remove any double spaces as resolver haven't picked these up
    taxa.name.gnr = stri_replace_all_regex(taxa.name.gnr, "  ", " ")
  ) %>% 
  select(-taxa.name, -resolved.source)
# Save
saveRDS(resolved_gnr_manual, file = "Data/taxize/resolved_gnr_manual.rds")


# making a chgange to add to github






























### Resolve using gnr reolver
## 1) Create list of distinct original.taxa.names and run through gnr_resolve
resolved_all_sources <- resolved_gnr_manual %>% 
  # Select all distinct original.taxa.names from all_raw
  distinct(taxa.name.gnr) %>% 
  rowwise() %>% 
  mutate(
    # Run through resolver
    resolved = list(gnr_resolve(sci = taxa.name.gnr, http = "post", canonical = TRUE, fields = "all"))
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