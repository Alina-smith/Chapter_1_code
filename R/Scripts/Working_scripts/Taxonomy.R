#### taxonomic hierarchy - classification - gbif ----
##### 1) - run resolved names through gbif with no manual changes

### 1a) - intial run through gbif
tax_list_raw <- resolved %>%
  # remove repeats of the same original.taxa.name to speed up getting the hierachy, not distinct taxa.name to help adding onto raw data later on
  select(-resolved.source) %>% 
  distinct(original.taxa.name, .keep_all = TRUE) %>% 
  mutate(
    # get taxonomic hierarchy info for species
    taxonomy = list(classification(taxa.name, db = "gbif", return_id = TRUE, rows = 1)[[1]]),
    # make a column to show which round it was resolved it
    round = "1"
  ) %>% 
  # make a column for tax.uid, need to make as data frame because the previous rsd saves mean the rowise is still in play
  as.data.frame(tax_list_raw) %>% 
  mutate(
    tax.uid = row_number()
  )

# Save
saveRDS(tax_list_raw, file = "Data/taxize/tax_list_raw.rds")

### 1b) -  expand taxonomy columns
tax_list_1 <- tax_list_raw %>%
  rowwise() %>% 
  mutate(
    # make any ones that didn't have taxonomic info NA
    taxonomy = ifelse(
      "name" %in% colnames(taxonomy),
      list(taxonomy),
      NA
    ),
    # separate out into columns
    gbif.id = ifelse("id" %in% colnames(taxonomy),as.character(taxonomy$id[nrow(taxonomy)]),NA_character_),
    form = ifelse("form" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "form"], NA_character_),
    variety = ifelse("variety" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "variety"], NA_character_),
    species = ifelse("species" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "species"], NA_character_),
    genus = ifelse("genus" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "genus"], NA_character_),
    family = ifelse("family" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "family"], NA_character_),
    suborder = ifelse("suborder" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "suborder"], NA_character_),
    order = ifelse("order" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "order"], NA_character_),
    subclass = ifelse("subclass" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "subclass"], NA_character_),
    class = ifelse("class" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "class"], NA_character_),
    subphylum = ifelse("subphylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "subphylum"], NA_character_),
    phylum = ifelse("phylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "phylum"], NA_character_),
    kingdom = ifelse("kingdom" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "kingdom"], NA_character_),
    rank = case_when(
      !is.na(species) ~ "species",
      !is.na(genus) ~ "genus",
      !is.na(family) ~ "family",
      !is.na(suborder) ~ "suborder",
      !is.na(order) ~ "order",
      !is.na(subclass) ~ "subclass",
      !is.na(class) ~ "class",
      !is.na(subphylum) ~ "subphylum",
      !is.na(phylum) ~ "phylum",
      !is.na(kingdom) ~ "kingdom",
      TRUE ~ NA
    )
  ) %>% 
  select(-taxonomy) %>% 
  relocate(tax.uid, original.taxa.name, taxa.name, round, gbif.id, form, variety, rank, species, genus, family, suborder, order, subclass, class, subphylum, phylum, kingdom)
# Save
saveRDS(tax_list_1, file = "Data/taxize/tax_list_1.rds")

##### Step 2 - Manually edit any taxonomy info that was incorrect/wasn't picked up by taxize

### 2a) - re-running any that were only classified to kingdom level through gbif or had a species level taxa.name but not described to species level
#         when there is a synonym or accepted name with exact match type that is chosen, if not higher rank match type is chosen
tax_list_2a_raw <- tax_list_1 %>% 
  # split taxa.name into two columns and select any that have a value in both columns as these are ones that are described to species level
  separate(taxa.name, into = c("taxa.name.1", "taxa.name.2"), sep = " ", convert = TRUE, remove = FALSE) %>% 
  # select rows that are only described to kingdom level or are described to species level but do not have a value in the species column
  filter(rank == "kingdom"|
           !is.na(taxa.name.1)&!is.na(taxa.name.2)&is.na(species)
  ) %>% 
  #select columns I want
  select(tax.uid,
         taxa.name,
         original.taxa.name
  ) %>% 
  rowwise() %>% 
  mutate(
    # get taxonomic hierachy with rows = 1 removed so I can manually chose which ones
    taxonomy = list(classification(taxa.name, db = "gbif", return_id = TRUE)[[1]]), # row = 1 turned off to allow for manual selection
    # make a column for which round it was done in
    round = "2"
  )
# Save
saveRDS(tax_list_2a_raw, file = "Data/taxize/tax_list_2a_raw.rds")

# 2b) - Separate out into columns 
tax_list_2a <- tax_list_2a_raw %>% 
  mutate(
    # make any ones that didn't have taxonomic info NA
    taxonomy = ifelse(
      "name" %in% colnames(taxonomy),
      list(taxonomy),
      NA
    ),
    # separate out into columns
    gbif.id = ifelse("id" %in% colnames(taxonomy),as.character(taxonomy$id[nrow(taxonomy)]),NA_character_),
    form = ifelse("form" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "form"], NA_character_),
    variety = ifelse("variety" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "variety"], NA_character_),
    species = ifelse("species" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "species"], NA_character_),
    genus = ifelse("genus" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "genus"], NA_character_),
    family = ifelse("family" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "family"], NA_character_),
    suborder = ifelse("suborder" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "suborder"], NA_character_),
    order = ifelse("order" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "order"], NA_character_),
    subclass = ifelse("subclass" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "subclass"], NA_character_),
    class = ifelse("class" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "class"], NA_character_),
    subphylum = ifelse("subphylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "subphylum"], NA_character_),
    phylum = ifelse("phylum" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "phylum"], NA_character_),
    kingdom = ifelse("kingdom" %in% taxonomy$rank, taxonomy$name[taxonomy$rank == "kingdom"], NA_character_),
    rank = case_when(
      !is.na(species) ~ "species",
      !is.na(genus) ~ "genus",
      !is.na(family) ~ "family",
      !is.na(suborder) ~ "suborder",
      !is.na(order) ~ "order",
      !is.na(subclass) ~ "subclass",
      !is.na(class) ~ "class",
      !is.na(subphylum) ~ "subphylum",
      !is.na(phylum) ~ "phylum",
      !is.na(kingdom) ~ "kingdom",
      TRUE ~ NA
    )
  ) %>% 
  select(-taxonomy) %>% 
  relocate(tax.uid, original.taxa.name, taxa.name, round, gbif.id, form, variety, rank, species, genus, family, suborder, order, subclass, class, subphylum, phylum, kingdom)
# Save
saveRDS(tax_list_2a, file = "Data/taxize/tax_list_2a.rds")

# 2c) - When updated taxonomy data is available replace with this data
tax_list_2 <- tax_list_1 %>% 
  left_join(tax_list_2a, by = "tax.uid") %>% 
  mutate(
    taxa.name = taxa.name.x,
    original.taxa.name = original.taxa.name.x,
    round = ifelse(!is.na(gbif.id.y), round.y, round.x),
    gbif.id = ifelse(!is.na(gbif.id.y), gbif.id.y, gbif.id.x),
    form = ifelse(!is.na(gbif.id.y), form.y, form.x),
    variety = ifelse(!is.na(gbif.id.y), variety.y, variety.x),
    rank = ifelse(!is.na(gbif.id.y), rank.y, rank.x),
    species = ifelse(!is.na(gbif.id.y), species.y, species.x),
    genus = ifelse(!is.na(gbif.id.y), genus.y, genus.x),
    family = ifelse(!is.na(gbif.id.y), family.y, family.x),
    suborder = ifelse(!is.na(gbif.id.y), suborder.y, suborder.x),
    order = ifelse(!is.na(gbif.id.y), order.y, order.x),
    subclass = ifelse(!is.na(gbif.id.y), subclass.y, subclass.x),
    class = ifelse(!is.na(gbif.id.y), class.y, class.x),
    subphylum = ifelse(!is.na(gbif.id.y), subphylum.y, phylum.x),
    phylum = ifelse(!is.na(gbif.id.y), phylum.y, phylum.x),
    kingdom = ifelse(!is.na(gbif.id.y), kingdom.y, kingdom.x)
  ) %>% 
  select(tax.uid, original.taxa.name, taxa.name, round, gbif.id, form, variety, rank, species, genus, family, suborder, order, subclass, class, subphylum, phylum, kingdom)
# Save
saveRDS(tax_list_2, file = "Data/taxize/tax_list_2.rds")

### 3) - Manually resolve any names and add in any missing taxonomic levels, This information is taken mostly from worms and google

tax_list_3 <- tax_list_2 %>% 
  # Remove sub levels
  select(-suborder, -subclass, -subphylum)%>% 
  # add in missing taxonomy info
  mutate(
    # variety
    variety = case_when(
      taxa.name == "Tribonema microchloron cylindricum" ~ "Tribonema microchloron var. cylindricum",
      taxa.name == "Scenedesmus bicaudatus brevicaudatus" ~ "Scenedesmus bicaudatus var. brevicaudatus",
      taxa.name == "Chlamydomonas gloeophila irregularis" ~ "Chlamydomonas gloeophila var. irregularis",
      taxa.name == 'Hippodonta arctica' ~ 'Navicula hungarica var. arctica',
      TRUE ~ variety
    ),
    
    # species
    species = case_when(
      taxa.name == 'Kirchneriella subsolitaria' ~ 'Nephrochlamys subsolitaria',
      taxa.name == "Deasonia gigantica" ~ "Deasonia prolifera",
      taxa.name == "Aphanizomenon ovalisporum" ~ "Umezakia ovalisporum",
      taxa.name == "Crucigenia staurogeniaeformis" ~ "Tetrastrum staurogeniiforme",
      taxa.name == "Eunotia nymanii" ~ "Eunotia nymanniana",
      taxa.name == "Closterium minutum" ~ "Closterium cynthia",
      taxa.name == "Monochrysis angilissima" ~ "Monochrysis agilissima",
      taxa.name == "Closterium moniliforme" ~ "Closterium moniliferum",
      taxa.name == 'Schroederia antillarum' ~ 'Pseudoschroederia antillarum',
      taxa.name == 'Anabaena flos-aquae f. lemmermannii' ~ 'Dolichospermum lemmermannii',
      taxa.name == 'Scenedesmus lunatus' ~ 'Desmodesmus lunatus',
      taxa.name == 'Tribonema microchloron cylindricum' ~ 'Tribonema microchloron',
      taxa.name == 'Salpingoeca globulosa' ~ 'Lagenoeca globulosa',
      taxa.name == 'Scenedesmus bicaudatus brevicaudatus' ~ 'Scenedesmus bicaudatus',
      taxa.name == 'Chlamydomonas gloeophila irregularis' ~ 'Chlamydomonas gloeophila',
      taxa.name == 'Salpingoeca butschlii' ~ 'Salpingoeca buetschlii',
      taxa.name == 'Phacus granulatus' ~ 'Phacus pleuronectes',
      taxa.name == 'Polyedriopsis bitridens' ~ 'Chlorotetraëdron bitridens',
      taxa.name == 'Katodinium hiemale' ~ 'Borghiella pascheri',
      taxa.name == 'Calycomonas pascheri' ~ 'Ollicola pascheri',
      taxa.name == 'Chrysosporum minor' ~ 'Chrysosporum minus',
      taxa.name == 'Hippodonta arctica' ~ 'Navicula hungarica',
      taxa.name == 'Naviculadicta seminulum' ~ 'Sellaphora seminulum',
      taxa.name == 'Xanthidium alpinum' ~ 'Xanthidium robinsonianum',
      taxa.name == 'Heterothrix constricta' ~ 'Xanthonema constrictum',
      taxa.name == 'Coccomyxa litoralis' ~ 'Coccomyxa litoralis',
      taxa.name %in% c("Chlamydomonas inepta", 'Cystomonas starrii', 'Chlamydomonas pumilio', 'Lobomonas verrucosa', 'Cyanogranis ferruginea', 'Pseudanabaena franquetii', 'Cyanocatena planctonica', 'Myxobaktron salinum',
                       'Bitrichia ochridana', 'Kephyrion gracile', 'Planktolyngbya regularis', 'Sphaerellocystis ellipsoidea', 'Gomphonema geitleri', 'Encyonema tumida', 'Scenedesmus balatonicus', 'Geitlerinema tenuius',
                       'Chloromonas prona', 'Anabaenopsis tanganyikae', 'Snowella arachnoidea', 'Radiocystis aphanothecoidea', 'Woronichinia ruzickae', 'Oscillatoria janus', 'Siderocelis oblonga', 'Synechococcus ambiguus',
                       'Pseudokephyrion striatum', 'Oocystis bispora', ' Ankyra inermis', 'Stichococcus mirabilis', 'Heleochloris pallida', 'Scenedesmus fusiformis', 'Bicoeca paropsis', 'Ochromonas tenera', 'Mallomonas lefevriana',
                       'Spondylosium clepsydra', 'Scenedesmus tibiscensis', 'Bicoeca cylindrica', 'Chromulina erkensis', 'Cyanosarcina chroococcoides', 'Synechocystis septentrionalis', 'Quadrichloris carterioides', 'Pseudokephyrion latum',
                       'Rayssiella hemisphaerica', 'uadrigula quaternata', 'Tetraselmis arnoldii', 'Lyngbya truncicola', 'Golenkinia viridis', 'Epipyxis planctonica', 'Papenfussiomonas cordata', 'Katodinium simile', 'Oocystis polymorpha',
                       'Mallomonas genevensis', 'Cosmarium octhodes', 'Chlorolobion glareosum', 'Kephyrion colliferum', 'Kephyrion mastigophorum', 'Paraphysomonas bourrellyi', 'Kephyrion welshii', 'Pseudokephyrion cylindricum', 
                       'Cyanobacterium notatum', 'Cyanogranis libera', 'Limnolyngbya circumcreta', 'Romeria okensis', 'Cosmarium zygomorphicum', 'Heterothrix tenuissima', 'Rhabdoderma tenuissimum', 'Romeria chlorina', 'Ulothrix limnetica',
                       'Paradoxia pelletieri', 'Stokesiella epipyxis', 'Amoeba leningradensis') ~ taxa.name,
      TRUE ~ species),
    
    genus = case_when(
      # Add in missing names
      taxa.name %in% c('Centric diatom', 'Centric', 'Micro') ~ NA,
      taxa.name == 'Cosmarium skujae' ~ 'Cosmarium',
      taxa.name == 'Lanistes' ~ 'Lanistes',
      taxa.name == 'Heterothrix' ~ 'Xanthonema',
      species == 'Amoeba leningradensis' ~ 'Amoeba',
      species == 'Cosmarium zygomorphicum' ~ 'Cosmarium',
      species == 'Heterothrix tenuissima' ~ 'Xanthonema',
      species == 'Paradoxia pelletieri' ~ 'Paradoxia',
      species == 'Pseudoschroederia antillarum' ~ 'Pseudoschroederia',
      species == 'Rhabdoderma tenuissimum' ~ 'Rhabdoderma',
      species == 'Romeria chlorina' ~ 'Romeria',
      species == 'Romeria okensis' ~ 'Romeria',
      species == 'tokesiella epipyxis' ~ 'tokesiella',
      species == 'Ulothrix limnetica' ~ 'Ulothrix',
      species == 'Xanthidium robinsonianum' ~ 'Xanthidium',
      species == 'Xanthonema constrictum' ~ 'Xanthonema',
      species == 'Dolichospermum lemmermannii' ~ 'Dolichospermum',
      species == 'Stokesiella epipyxis' ~ 'Stokesiella',
      species == 'Coccomyxa litoralis' ~ 'Coccomyxa',
      TRUE ~ genus
    ),
    
    # Family
    family = case_when(
      taxa.name == 'Micro' ~ NA,
      genus == 'Microcystis' ~ 'Microcystaceae',
      genus == 'Nodularia' ~ 'Nodulariaceae',
      genus == 'Sida' ~ 'Sididae',
      genus == 'Lanistes' ~ 'Ampullariidae',
      genus %in% c("Staurastrum", "Cosmarium", "Staurodesmus", "Teilingia", "Desmidium", "Bambusina", "Euastrum", "Hyalotheca", "Micrasterias", "Octacanthium",
                   "Onychonema", "Pleurotaenium", "Spondylosium", "Tetmemorus", "Xanthidium") ~ "Desmidiaceae",
      genus == "Jaaginema" ~ "Synechococcales familia incertae sedis ",
      genus == "Crucigenia" ~ "Trebouxiophyceae familia incertae sedis ",
      genus %in% c("Romeria", "Rhabdoderma") ~ "Cymatolegaceae",
      genus == "Vibrio" ~ "Amoebidae",
      genus %in% c("Biblarium", "Microneis", "Monema", "Gloeonema", "Himantidium", "Discoplea") ~ "Bacillariophyceae familia incertae sedis",
      genus == "Synuropsis" ~ "Ochromonadaceae",
      genus == "Polyedriopsis" ~ "Sphaeropleales familia incertae sedis",
      genus == "Xanthonema" ~ "Tribonemataceae",
      genus == "Picochlorum" ~ "Chlorellales familia incertae sedis",
      genus == "Pseudoschroederia" ~ "Characiaceae",
      genus == "Ulothrix" ~ "Ulotrichaceae",
      genus == "Coenocystis" ~ "Radiococcaceae",
      genus == "Hyaloraphidium" ~ "Fungi familia incertae sedis",
      genus %in% c('Paradoxia', 'Coccomyxa') ~ 'Coccomyxaceae',
      genus == "Lobocystis" ~ "Chlorophyceae familia incertae sedis",
      genus == "Polychaos" ~ "Euamoebida familia incertae sedis",
      genus == "Amoeba" ~ "Amoebidae",
      genus == "Dolichospermum" ~ "Aphanizomenonaceae",
      genus == "Stokesiella" ~ "Dinobryaceae",
      genus == "Pleurostauron" ~ "Stauroneidaceae",
      genus == "Cyanobium" ~ "Prochlorococcaceae",
      genus %in% c('Geminella', 'Acanthosphaera') ~ 'Chlorellaceae',
      genus %in% c('Carteria', 'Sphaerellopsis') ~ 'Chlamydomonadaceae',
      genus == 'Nais' ~ "Naididae",
      genus == 'Ceratium' ~ 'Ceratiaceae',
      genus %in% c('Chaetophora incrassata', 'Chaetophora') ~ 'Chaetophoraceae',
      genus == 'Didymocystis' ~ 'Oocystaceae',
      genus == 'Sphaerocystis' ~ 'Sphaerocystidaceae',
      genus == 'Xenococcus' ~ 'Pleurocapsaceae',
      genus == 'Asterococcus' ~ 'Palmellopsidaceae',
      TRUE ~ family
    ),
    
    # Order
    order = case_when(
      # resolve spelling mistakes
      order == 'Cyclotrichida' ~ 'Cyclotrichiida',
      # add in missing data
      taxa.name == 'Micro' ~ NA,
      genus == 'Crucigenia' ~ "Trebouxiophyceae ordo incertae sedis",
      genus == 'Polychaos' ~ "Euamoebida",
      genus %in% c('Biblarium', 'Discoplea', 'Gloeonema', 'Himantidium', 'Microneis', 'Monema') ~ "Bacillariophyceae ordo incertae sedis",
      family == 'Wilmottiaceae' ~ "Coleofasciculales",
      family == 'Sididae' ~ "Diplostraca",
      family == 'Bicosoecaceae' ~ "Bicosoecales",
      family == 'Amphidiniaceae' ~ "Amphidiniales",
      family == 'Cymatolegaceae' ~ "Nodosilineales",
      family == 'Prochaetodermatidae' ~ "Chaetodermatida",
      family == 'Amoebidae' ~ "Euamoebida",
      family == 'Stauroneidaceae' ~ "Naviculales",
      family == 'Desmidiaceae' ~ "Desmidiales",
      family == 'Sertulariidae' ~ "Leptothecata",
      family == 'Ebriaceae' ~ "Ebriales",
      family == 'Paramastigaceae' ~ "Spironematellales",
      family == 'Tribonemataceae' ~ "Tribonematales",
      family == 'Characiaceae' ~ "Sphaeropleales",
      family == 'Ulotrichaceae' ~ "Ulotrichales",
      family == 'Radiococcaceae' ~ "Sphaeropleales",
      family == 'Fungi familia incertae sedis' ~ "Fungi ordo incertae sedis",
      family == 'Coccomyxaceae' ~ "Trebouxiophyceae ordo incertae sedis",
      family == 'Dinobryaceae' ~ "Chromulinales",
      family %in% c('Burnupiidae', 'Planorbidae', 'Lymnaeidae', 'Physidae') ~ "Hygrophila",
      family %in% c('Potamididae', 'Paludomidae', 'Thiaridae') ~ "Caenogastropoda incertae sedis",
      family %in% c('Katablepharidaceae', 'Euglyphidae', 'Trinematidae') ~ "Euglyphida",
      family == 'Ulotrichaceae' ~ "Cryptophyta ordo incertae sedis",
      family == 'Prochlorococcaceae' ~ "Synechococcales",
      family == 'Ampullariidae' ~ "Architaenioglossa",
      family %in% c('Chlorellaceae', 'Oocystaceae') ~ 'Chlorellales',
      family == 'Naididae' ~ "Tubificida",
      family == 'Chaetophoraceae' ~ 'Chaetophorales',
      family == 'Pelonemataceae' ~ 'Pelonematales',
      family %in% c('Chlamydomonadaceae', 'Sphaerocystidaceae', 'Palmellopsidaceae') ~ 'Chlamydomonadales',
      family == 'Ceratiaceae' ~ 'Gonyaulacales',
      family == 'Chilodonellidae' ~ 'Chlamydodontida',
      family == 'Bodonidae' ~ 'Bodonida',
      family == 'Katablepharidaceae' ~ 'Cryptophyta ordo incertae sedis',
      family %in% c('Nodulariaceae', 'Aphanizomenonaceae') ~ 'Nostocales',
      family %in% c('Microcystaceae', 'Pleurocapsaceae') ~ 'Chroococcales',
      TRUE ~ order
    ),
    
    # Class
    class = case_when(
      # resolve spelling mistakes
      class == 'Cyanobacteriia' ~ 'Cyanophyceae',
      class == 'Gymnostomatea' ~ 'Litostomatea',
      class == 'Prymnesiophyceae' ~ 'Coccolithophyceae',
      class == 'Zygnematophyceae' ~ 'Conjugatophyceae',
      # add in missing names
      taxa.name %in% c('Centric diatom', 'Centric') ~ 'Bacillariophyceae',
      taxa.name == 'Micro' ~ "Chlorophyceae",
      family == 'Coccomyxaceae' ~ "Trebouxiophyceae",
      family == 'Katablepharidaceae' ~ 'Cryptophyta incertae sedis',
      order == 'Chlorellales' ~ "Trebouxiophyceae",
      order == 'Bicosoecales' ~ "Bicosoecophyceae",
      order %in% c('Nodosilineales', 'Pelonematales', 'Nostocales', 'Chroococcales', 'Coleofasciculales') ~ "Cyanophyceae",
      order == 'Chaetodermatida' ~ "Caudofoveata",
      order == 'Desmidiales' ~ "Zygnematophyceae",
      order == 'Leptothecata' ~ "Hydrozoa",
      order == 'Ebriales' ~ "Thecofilosea",
      order == 'Spironematellales' ~ "Spironematellophyceae",
      order == 'Tribonematales' ~ "Xanthophyceae",
      order %in% c('Sphaeropleales', 'Chaetophorales', 'Chlamydomonadales') ~ "Chlorophyceae",
      order == 'Ulotrichales' ~ "Ulvophyceae",
      order == 'Chromulinales' ~ "Chrysophyceae",
      order == 'Euamoebida' ~ "Tubulinea",
      order == 'Eugregarinida' ~ "Conoidasida",
      order == 'Euglyphida' ~ "Imbricatea",
      order == 'Architaenioglossa' ~ "Gastropoda",
      order == 'Diplostraca' ~ "Branchiopoda",
      order == 'Tubificida' ~ "Clitellata",
      order == 'Gonyaulacales' ~ 'Dinophyceae',
      order == 'Chlamydodontida' ~ 'Phyllopharyngea',
      order == 'Euplotida' ~ "Spirotrichea",
      order == 'Chaetosphaeridiales' ~ "Coleochaetophyceae",
      TRUE ~ class
    ),
    
    # Phylum
    phylum = case_when(
      family == 'Katablepharidaceae' ~ 'Cryptophyta',
      class == "Cyanophyceae" ~ "Cyanobacteria",
      class %in% c("Bacillariophyceae", "Xanthophyceae", "Chrysophyceae") ~ "Ochrophyta",
      class == "Zygnematophyceae" ~ "Charophyta",
      class == "Thecofilosea" ~ "Cercozoa",
      class == "Spironematellophyceae" ~ "Spironematellophyta",
      class %in% c("Chlorophyceae", "Ulvophyceae", "Trebouxiophyceae") ~ "Chlorophyta",
      class == "Polycystina" ~ "Radiozoa",
      class == "Tubulinea" ~ "Amoebozoa",
      class == "Imbricatea" ~ "Cercozoa",
      class == "Gastropoda" ~ "Mollusca",
      class == 'Branchiopoda' ~ "Arthropoda",
      class == 'Clitellata' ~ "Annelida",
      class == 'Dinophyceae' ~ "Myzozoa",
      class == 'Phyllopharyngea' ~ 'Ciliophora',
      TRUE ~ phylum
    ),
    
    # Kingdom
    kingdom = case_when(
      phylum == 'Chlorophyta' ~ "Plantae",
      phylum == 'Amoebozoa' ~ "Protozoa",
      phylum == 'Cyanobacteria' ~ "Bacteria",
      phylum %in% c('Ochrophyta', 'Dinophyceae', 'Myzozoa', 'Ciliophora', 'Cercozoa', 'Cercozoa') ~ "Chromista",
      phylum %in% c('Mollusca', "Arthropoda", "Annelida") ~ "Animalia",
      TRUE ~ kingdom
    ),
    
    # Rank
    rank = case_when(
      !is.na(species) ~ 'species',
      is.na(species) & !is.na(genus) ~ 'genus',
      is.na(species) & is.na(genus) & !is.na(family) ~ 'family',
      is.na(species) & is.na(genus) & is.na(family) & !is.na(order) ~ 'order',
      is.na(species) & is.na(genus) & is.na(family) & is.na(order) & !is.na(class) ~ 'class',
      is.na(species) & is.na(genus) & is.na(family) & is.na(order) & is.na(class) & !is.na(phylum) ~ 'phylum',
      TRUE ~ 'kingdom'
    )
  ) %>% 
  filter(
    !(taxa.name %in% c("Echinopus", 'Marssoniella', 'Hyaloraphidium contortum', 'Abrolophus harrisoni', 'Pyramidomonas'))
  ) %>% 
  # update any taxa.name that are different now
  mutate(
    taxa.name = case_when(
      rank == "form" ~ form,
      rank == "variety" ~ variety,
      rank == "species" ~ species,
      rank == "genus" ~ genus,
      rank == "family" ~ family,
      rank == "order" ~ order,
      rank == "class" ~ class, 
      rank == "phylum" ~ phylum,
      rank == "kingdom" ~ kingdom
    )
  )
# save
saveRDS(tax_list_3, file = "Data/taxize/tax_list_3.rds")

##### Make a tax list of distinct species
tax_list <- tax_list_3 %>% 
  select(-original.taxa.name) %>% 
  distinct(taxa.name, .keep_all = TRUE)
# save
saveRDS(tax_list, file = "Data/taxize/tax_list.rds")


#### Resolve synonyms - using classification - gbif ----
## 3) Run through gbif to resolve any synonyms
resolved_gbif_raw <- resolved_gnr_manual %>% 
  mutate(
    # run through gbif
    taxonomy.gbif = list(classification(taxa.name.gnr, db = "gbif", return_id = TRUE, rows = 1)),
    # make all a dataframe inclusing NAs
    taxonomy.gbif = list(as.data.frame(taxonomy.gbif[[1]]))
  )
# Save
saveRDS(resolved_gbif_raw, file = "Data/taxize/resolved_gbif_raw.rds")

## 4) Manually edit any and expand columns
resolved <- resolved_gbif_raw %>% 
  mutate(
    # select the lowest rank in taxonony.gbif to get new name
    taxonomy.gbif.name = ifelse("id" %in% colnames(taxonomy.gbif),as.character(taxonomy.gbif$name[nrow(taxonomy.gbif)]),NA_character_),
    
    # manually change any
    taxa.name.gbif = case_when(
      # resolved wrong 
      stri_detect_regex(taxa.name.gnr, "\\bEchinopus\\b") ~ taxa.name.gnr,
      
      # ones not picked up by gbif (NAs)
      taxa.name.gnr %in% c("Romeria okensis", "Heterothrix tenuissima", "Rhabdoderma tenuissimum", "Romeria chlorina", "Ulothrix limnetica", "Paradoxia pelletieri", "Stokesiella epipyxis", "Kirchneriella dichotomococcoides", "Amoeba leningradensis", "Fallacia difficillimoides") ~ taxa.name.gnr,
      taxa.name.gnr == "Schroederia antillarum" ~ "Pseudoschroederia antillarum",
      taxa.name.gnr == "Heterothrix constricta" ~ "Xanthonema constrictum",
      
      # Ones that were bumped up a taxonomic level by gbif
      taxa.name.gnr %in% c("Chlamydomonas inepta", "Cystomonas starrii", "Lyngbya subtilis", "Chlamydomonas pumilio", "Lobomonas verrucosa", "Cyanogranis ferruginea", "Cyanogranis ferruginea", "Cyanocatena planctonica", "Myxobaktron salinum", "Bitrichia ochridana",
                           "Tetraedron regulare", "Tetraedron trigonum", "Planktolyngbya regularis", "Sphaerellocystis ellipsoidea", "Closterium minutum", "Cosmarium zygomorphicum", "Scenedesmus balatonicus", "Geitlerinema tenuius", "Pseudostaurastrum limneticum",
                           "Gymnodinium albulum", "Chloromonas prona", "Anabaenopsis tanganyikae", "Snowella arachnoidea", "Radiocystis aphanothecoidea", "Komvophoron skujae", "Oscillatoria janus", "Siderocelis oblonga", "Synechococcus ambiguus", "Oocystis bispora",
                           "Ankyra inermis", "Stichococcus mirabilis", "Heleochloris pallida", "Scenedesmus fusiformis", "Bicoeca paropsis", "Ochromonas tenera", "Mallomonas lefevriana", "Spondylosium clepsydra", "Scenedesmus tibiscensis", "Bicoeca cylindrica",
                           "Chromulina erkensis", "Cyanosarcina chroococcoides", "Cosmarium skujae", "Bicoeca ovata", "Synechocystis septentrionalis", "Quadrichloris carterioides", "Pseudokephyrion latum", "Bicoeca campanulata", "Rayssiella hemisphaerica", "Quadrigula quaternata",
                           "Xanthidium alpinum", "Tetraselmis arnoldii", "Lyngbya truncicola", "Golenkinia viridis", "Euglena korsikovii", "Papenfussiomonas cordata", "Katodinium simile", "Oocystis polymorpha", "Mallomonas genevensis", "Gymnodinium palustre", "Cosmarium octhodes",
                           "Chlorolobion glareosum", "Kephyrion mastigophorum", "Kephyrion welshii", "Paraphysomonas bourrellyi", "Pseudokephyrion cylindricum", "Cyanobacterium notatum", "Cyanogranis libera", "Limnolyngbya circumcreta", "Aphanothece clathrata var. rosea",
                           "Scenedesmus ecornis var. polymorphus", "Cosmarium pygmaeum var. heimerlii", "Chlamydomonas gloeophila var. irregularis", "Cosmarium polygonum var. hexagonum", "Peridinium inconspicuum var. contactum", "Cosmarium depressum var. planctonicum",
                           "Scenedesmus lefevrei var. manguinii", "Caloneis schumanniana var. biconstricta f. minor", "Gomphonema pumilum var. rigidum", "Anabaena oscillarioides f. elliptica", "Cosmarium margaritiferum f. regularius", "Scenedesmus granulatus f. spinosus",
                           "Caloneis schumanniana var. biconstricta f. minor", "Anabaena flos-aquae f. lemmermannii", "Achnanthidium minutissima var. affinis", "Pseudanabaena franquetii") ~ taxa.name.gnr,
      taxa.name.gnr == "Dactylococcopsis irregularis" ~ "Dactylococcopsis irregularis",
      taxa.name.gnr == "Quadrigula lacustris" ~ "Gregiochloris lacustris",
      taxa.name.gnr == "Scenedesmus dimorphus" ~ "Tetradesmus dimorphus",
      taxa.name.gnr == "Deasonia gigantica" ~ "Deasonia prolifera",
      taxa.name.gnr == "Scenedesmus bicaudatus" ~ "Desmodesmus armatus var. bicaudatus",
      taxa.name.gnr == "Elakatothrix viridis" ~ "Fusola viridis",
      taxa.name.gnr == "Aphanizomenon ovalisporum" ~ "Umezakia ovalisporum",
      taxa.name.gnr == "Ceratoneis arcus" ~ "Hannaea arcus",
      taxa.name.gnr == "Carteria cordiformis" ~ "Tetraselmis cordiformis",
      taxa.name.gnr %in% c("Ulothrix subtilissima", "Stichococcus subtilis") ~ "Klebsormidium subtile",
      taxa.name.gnr ==  "Actinoptychus senarius" ~ "Actinocyclus senarius",
      taxa.name.gnr == "Crucigenia staurogeniaeformis" ~ "Tetrastrum staurogeniiforme",
      taxa.name.gnr == "Tetraedron limneticum" ~ "Pseudostaurastrum limneticum",
      taxa.name.gnr == "Actinoptychus undulatus" ~ "Actinocyclus senarius",
      taxa.name.gnr == "Cymbella lanceolata" ~ "Frustulia lanceolata",
      taxa.name.gnr == "Eunotia nymanii" ~ "Eunotia nymanniana",
      taxa.name.gnr == "Frustulia viridula" ~ "Colletonema viridulum",
      taxa.name.gnr == "Frustulia crassinervia" ~ "Navicula crassinervia var. crassinervia",
      taxa.name.gnr == "Gomphoneis geitleri" ~ "Gomphosinica geitleri",
      taxa.name.gnr == "Nitzschia constricta" ~ "Psammodictyon constrictum f. parvum",
      taxa.name.gnr == "Pseudostaurosira elliptica" ~ "Fragilaria zeilleri var. elliptica",
      taxa.name.gnr == "Fallacia tenera" ~ "Navicula tenera",
      taxa.name.gnr == "Karayevia suchlandtii" ~ "Achnanthes suchlandtii var. suchlandtii",
      taxa.name.gnr == "Encyonopsis evergladianum" ~ "Encyonema evergladianum",
      taxa.name.gnr == "Monochrysis angilissima" ~ "Monochrysis agilissima",
      taxa.name.gnr == "Tabellaria binalis" ~ "Fragilaria binalis",
      taxa.name.gnr == "Peridinium penardiforme" ~ "Glochidinium penardiforme",
      taxa.name.gnr == "Pleurotaenium minutum" ~ "Haplotaenium minutum",
      taxa.name.gnr == "Schroederia antillarum" ~ "Pseudoschroederia antillarum",
      taxa.name.gnr == "Scenedesmus lunatus" ~ "Desmodesmus lunatus",
      taxa.name.gnr == "Salpingoeca globulosa" ~ "Lagenoeca globulosa",
      taxa.name.gnr == "Salpingoeca butschlii" ~ "Salpingoeca buetschlii",
      taxa.name.gnr == "Phacus granulatus" ~ "Phacus pleuronectes",
      taxa.name.gnr == "Polyedriopsis bitridens" ~ "Chlorotetraëdron bitridens",
      taxa.name.gnr == "Katodinium hiemale" ~ "Borghiella pascheri",
      taxa.name.gnr == "Calycomonas pascheri" ~ "Ollicola pascheri",
      taxa.name.gnr == "Staurastrum dejectum" ~ "Staurastrum dejectum",
      taxa.name.gnr == "Hippodonta arctica" ~ "Navicula hungarica var. arctica",
      taxa.name.gnr == "Naviculadicta schmassmannii" ~ "Humidophila schmassmannii",
      taxa.name.gnr == "Aphanothece clathrata var. rosea colony 10 µm" ~ "Aphanothece clathrata var. rosea",
      taxa.name.gnr == "Aphanizomenon flos-aquae var. gracile" ~ "Aphanizomenon gracile",
      taxa.name.gnr == "Scenedesmus bicaudatus var. fenestratus" ~ "Scenedesmus denticulatus var. fenestratus",
      taxa.name.gnr == "Tribonema microchloron var. cylindricum" ~ "Tribonema microchloron",
      taxa.name.gnr == "Anabaena bergii var. limnetica" ~ "Anabaena minderi",
      taxa.name.gnr =="Scenedesmus bicaudatus var. brevicaudatus" ~ "Desmodesmus armatus var. bicaudatus",
      taxa.name.gnr == "Scenedesmus bicaudatus var. fenestratus" ~ "Scenedesmus denticulatus var. fenestratus",
      taxa.name.gnr == "Dinobryon cf stokesii var. neustonicum" ~ "Dinobryon stokesii var. neustonicum",
      taxa.name.gnr == "Pediastrum simplex var. sturmii" ~ "Monactinus simplex var. sturmii",
      taxa.name.gnr %in% c("Fragilaria ulna var. angustissima", "Ulnaria delicatissima var. angustissima") ~ "Synedra delicatissima var. angustissima", 
      taxa.name.gnr == "Caloneis schumanniana var. biconstricta" ~ "Navicula limosa f. limosa",
      taxa.name.gnr == "Eunotia exigua var. tenella" ~ "Eunotia arcus var. tenella",
      taxa.name.gnr == "Fragilaria capucina var. rumpens" ~ "Synedra rumpens var. rumpens",
      taxa.name.gnr == "Scenedesmus disciformis f. disciformis" ~ "Scenedesmus obtusus f. disciformis",
      taxa.name.gnr %in% c("Fragilaria construens f. binodis", "Fragilaria construens f. binodis") ~ "Neidiomorpha binodis",
      taxa.name.gnr == "Anabaena circinalis f. tenuis" ~ "Anabaena hassallii f. tenuis",
      taxa.name.gnr == "Gomphosphaeria pallidum" ~ "Coelosphaerium pallidum",
      taxa.name.gnr == "Gomphonema geitleri" ~ "Gomphosinica geitleri",
      taxa.name.gnr == "Closterium moniliforme" ~ "Closterium moniliferum",
      taxa.name.gnr == "Coelosphaerium kutzingii" ~ "Coelosphaerium kuetzingianum",
      taxa.name.gnr == "Peridiniopsis umbonatum" ~ "Parvodinium umbonatum",
      taxa.name.gnr == "Anopheles aureosquamiger" ~ "Anopheles natalensis",
      taxa.name.gnr == "Naviculadicta seminulum" ~ "Sellaphora saugerresii",
      taxa.name.gnr %in% c("Kephyrion gracile", "Pseudokephyrion striatum", "Mutelocloeon thomasorum", "Nesydemius polhemusorum", "Woronichinia ruzickae", "Chrysosporum minor", "Epipyxis planctonica", "Asplanchna henrietta", "Trichotria similis", "Platyias polyacanthus",
                           "Kephyrion gracilis", "Dinobryon anneciense", "Chydorus bicuspidatus", "Ictinogomphus ruwenzorica", "Ictinogomphus selysi", "ctinogomphus soror", "Allocnemis singularis", "Sphaerastrum fockii", "Fallacia difficillimoides",
                           "Neritina gagates", "Encyonema tumida", "Kephyrion colliferum", "Myxococcoides chlorelloidea", "Ictinogomphus soror") ~ taxa.name.gnr,
      TRUE ~ taxonomy.gbif.name
    )
  )%>% 
  # Keep the variety picked up by gbif but add in var. or f.
  separate(taxonomy.gbif.name, into = c("genus", "species", "variety"), sep = " ", convert = TRUE, remove = FALSE) %>% 
  mutate(
    taxa.name.gbif = case_when(
      !is.na(variety) & stri_detect_regex(original.taxa.name, "f\\.") ~ paste(genus, species, "f.", variety),
      !is.na(variety) & stri_detect_regex(original.taxa.name, "var\\.") ~ paste(genus, species, "var.", variety),
      TRUE ~ taxa.name.gbif
    )
  ) %>% 
  select(-genus, -species, -variety, - taxonomy.gbif.name) %>% 
  relocate(original.taxa.name, taxa.name.gnr, taxa.name.gbif, resolved.source.gnr, taxonomy.gbif)
# Save
saveRDS(resolved, file = "Data/taxize/resolved.rds")

#### Taxonomic hierachy - classification - itis, gbif, worms and ncbi ----
## 1) Initial run through classification for worms, ncbi, itis and gbif
# Set API key
Sys.setenv(ENTREZ_KEY = "d785583fbc197e9f4cbe96737aa830f3a508")
Sys.getenv("ENTREZ_KEY")
ENTREZ_KEY=Sys.getenv("ENTREZ_KEY")

# Classification
tax_list_raw <- resolved %>% 
  distinct(taxa.name) %>% 
  rowwise() %>% 
  mutate(
    # Get hierachy from worms (worms chosen because gives the most up to date taxonomy for the most species)
    # Worms gives an error when there isn't a taxonomy for the taxa so set it so when an error occurs it assigns NA
    taxonomy.itis = tryCatch(
      {
        list(classification(taxa.name, db = "itis", return_id = TRUE, rows = 1))
      },
      error = function(e) {
        NA  # Return NA if an error occurs
      }
    ),
    taxonomy.worms = tryCatch(
      {
        list(classification(taxa.name, db = "worms", marine_only = FALSE, return_id = TRUE, rows = 1))
      },
      error = function(e) {
        NA  # Return NA if an error occurs
      }
    ),
    taxonomy.ncbi = tryCatch(
      {
        list(classification(taxa.name, db = "ncbi", return_id = TRUE, rows = 1))
      },
      error = function(e) {
        NA  # Return NA if an error occurs
      }
    ),
    taxonomy.gbif = tryCatch(
      {
        list(classification(taxa.name, db = "gbif", return_id = TRUE, rows = 1))
      },
      error = function(e) {
        NA  # Return NA if an error occurs
      }
    ),
    # Make a column to show what round the taxonomy was taken from
    round = "1"
  ) %>% 
  # make a column for tax.uid, need to make as data frame because the previous rsd saves mean the rowise is still in play
  as.data.frame() %>% 
  mutate(
    tax.uid = row_number()
  )
# Save
saveRDS(tax_list_raw, file = "Data/taxize/tax_list_raw.rds")    

## 2) extract taxonomy info from raw columns
tax_list_1 <- tax_list_raw %>% 
  rowwise() %>% 
  mutate(
    # set all to dataframes to make NAs dataframes to make it easier to extract information
    taxonomy.itis = list(as.data.frame(taxonomy.itis[[1]])),
    taxonomy.worms = list(as.data.frame(taxonomy.worms[[1]])),
    taxonomy.gbif = list(as.data.frame(taxonomy.gbif[[1]])),
    taxonomy.ncbi = list(as.data.frame(taxonomy.ncbi[[1]])),
    
    # Extract data from the nested dataframes for both sources
    id.gbif = ifelse("id" %in% colnames(taxonomy.gbif),as.character(taxonomy.gbif$id[nrow(taxonomy.gbif)]),NA_character_),
    id.worms = ifelse("id" %in% colnames(taxonomy.worms),as.character(taxonomy.worms$id[nrow(taxonomy.worms)]),NA_character_),
    id.ncbi = ifelse("id" %in% colnames(taxonomy.ncbi),as.character(taxonomy.ncbi$id[nrow(taxonomy.ncbi)]),NA_character_),
    id.itis = ifelse("id" %in% colnames(taxonomy.itis),as.character(taxonomy.itis$id[nrow(taxonomy.itis)]),NA_character_),
    
    form.gbif = ifelse("form" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "form"], NA_character_),
    form.worms = ifelse("Form" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Form"], NA_character_),
    form.ncbi = ifelse("Form" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "Form"], NA_character_),
    form.itis = ifelse("Form" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "Form"], NA_character_),
    
    variety.gbif = ifelse("variety" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "variety"], NA_character_),
    variety.worms = ifelse("Variety" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Variety"], NA_character_),
    variety.ncbi = ifelse("variety" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "variety"], NA_character_),
    variety.itis = ifelse("Variety" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "Variety"], NA_character_),
    
    species.gbif = ifelse("species" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "species"], NA_character_),
    species.worms = ifelse("Species" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Species"], NA_character_),
    species.ncbi = ifelse("species" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "species"], NA_character_),
    species.itis = ifelse("species" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "species"], NA_character_),
    
    genus.gbif = ifelse("genus" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "genus"], NA_character_),
    genus.worms = ifelse("Genus" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Genus"], NA_character_),
    genus.ncbi = ifelse("genus" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "genus"], NA_character_),
    genus.itis = ifelse("genus" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "genus"], NA_character_),
    
    family.gbif = ifelse("family" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "family"], NA_character_),
    family.worms = ifelse("Family" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Family"], NA_character_),
    family.ncbi = ifelse("family" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "family"], NA_character_),
    family.itis = ifelse("family" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "family"], NA_character_),
    
    suborder.gbif = ifelse("suborder" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "suborder"], NA_character_),
    suborder.worms = ifelse("Suborder" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Suborder"], NA_character_),
    suborder.ncbi = ifelse("suborder" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "suborder"], NA_character_),
    suborder.itis = ifelse("suborder" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "suborder"], NA_character_),
    
    order.gbif = ifelse("order" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "order"], NA_character_),
    order.worms = ifelse("Order" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Order"], NA_character_),
    order.ncbi = ifelse("order" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "order"], NA_character_),
    order.itis = ifelse("order" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "order"], NA_character_),
    
    subclass.gbif = ifelse("subclass" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "subclass"], NA_character_),
    subclass.worms = ifelse("Subclass" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Subclass"], NA_character_),
    subclass.ncbi = ifelse("subclass" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "subclass"], NA_character_),
    subclass.itis = ifelse("subclass" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "subclass"], NA_character_),
    
    class.gbif = ifelse("class" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "class"], NA_character_),
    class.worms = ifelse("Class" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Class"], NA_character_),
    class.ncbi = ifelse("class" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "class"], NA_character_),
    class.itis = ifelse("class" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "class"], NA_character_),
    
    subphylum.gbif = ifelse("subphylum" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "subphylum"], NA_character_),
    subphylum.worms = ifelse("Subphylum" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Subphylum"], NA_character_),
    subphylum.worms = ifelse("Subphylum (Division)" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Subphylum (Division)"], subphylum.worms),
    subphylum.ncbi = ifelse("subphylum" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "subphylum"], NA_character_),
    subphylum.itis = ifelse("subphylum" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "subphylum"], NA_character_),
    
    phylum.gbif = ifelse("phylum" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "phylum"], NA_character_),
    phylum.worms = ifelse("Phylum" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Phylum"], NA_character_),
    phylum.worms = ifelse("Phylum (Division)" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Phylum (Division)"], phylum.worms),
    phylum.ncbi = ifelse("phylum" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "phylum"], NA_character_),
    phylum.itis = ifelse("phylum" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "phylum"], NA_character_),
    
    subkingdom.gbif = ifelse("subkingdom" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "subkingdom"], NA_character_),
    subkingdom.worms = ifelse("Subingdom" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Subkingdom"], NA_character_),
    subkingdom.ncbi = ifelse("subkingdom" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "subkingdom"], NA_character_),
    subkingdom.itis = ifelse("subkingdom" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "subkingdom"], NA_character_),
    
    kingdom.gbif = ifelse("kingdom" %in% taxonomy.gbif$rank, taxonomy.gbif$name[taxonomy.gbif$rank == "kingdom"], NA_character_),
    kingdom.worms = ifelse("Kingdom" %in% taxonomy.worms$rank, taxonomy.worms$name[taxonomy.worms$rank == "Kingdom"], NA_character_),
    kingdom.ncbi = ifelse("kingdom" %in% taxonomy.ncbi$rank, taxonomy.ncbi$name[taxonomy.ncbi$rank == "kingdom"], NA_character_),
    kingdom.itis = ifelse("kingdom" %in% taxonomy.itis$rank, taxonomy.itis$name[taxonomy.itis$rank == "kingdom"], NA_character_),
    
    # Make a rank column
    rank.gbif = case_when(
      !is.na(variety.gbif) ~ "variety",
      !is.na(form.gbif) ~ "form",
      !is.na(species.gbif) ~ "species",
      !is.na(genus.gbif) ~ "genus",
      !is.na(family.gbif) ~ "family",
      !is.na(suborder.gbif) ~ "suborder",
      !is.na(order.gbif) ~ "order",
      !is.na(subclass.gbif) ~ "subclass",
      !is.na(class.gbif) ~ "class",
      !is.na(subphylum.gbif) ~ "subphylum",
      !is.na(phylum.gbif) ~ "phylum",
      !is.na(kingdom.gbif) ~ "kingdom",
      TRUE ~ NA
    ),
    rank.worms = case_when(
      !is.na(variety.worms) ~ "variety",
      !is.na(form.worms) ~ "form",
      !is.na(species.worms) ~ "species",
      !is.na(genus.worms) ~ "genus",
      !is.na(family.worms) ~ "family",
      !is.na(suborder.worms) ~ "suborder",
      !is.na(order.worms) ~ "order",
      !is.na(subclass.worms) ~ "subclass",
      !is.na(class.worms) ~ "class",
      !is.na(subphylum.worms) ~ "subphylum",
      !is.na(phylum.worms) ~ "phylum",
      !is.na(kingdom.worms) ~ "kingdom",
      TRUE ~ NA
    ),
    rank.ncbi = case_when(
      !is.na(variety.ncbi) ~ "variety",
      !is.na(form.ncbi) ~ "form",
      !is.na(species.ncbi) ~ "species",
      !is.na(genus.ncbi) ~ "genus",
      !is.na(family.ncbi) ~ "family",
      !is.na(suborder.ncbi) ~ "suborder",
      !is.na(order.ncbi) ~ "order",
      !is.na(subclass.ncbi) ~ "subclass",
      !is.na(class.ncbi) ~ "class",
      !is.na(subphylum.ncbi) ~ "subphylum",
      !is.na(phylum.ncbi) ~ "phylum",
      !is.na(kingdom.ncbi) ~ "kingdom",
      TRUE ~ NA
    ),
    rank.itis = case_when(
      !is.na(variety.itis) ~ "variety",
      !is.na(form.itis) ~ "form",
      !is.na(species.itis) ~ "species",
      !is.na(genus.itis) ~ "genus",
      !is.na(family.itis) ~ "family",
      !is.na(suborder.itis) ~ "suborder",
      !is.na(order.itis) ~ "order",
      !is.na(subclass.itis) ~ "subclass",
      !is.na(class.itis) ~ "class",
      !is.na(subphylum.itis) ~ "subphylum",
      !is.na(phylum.itis) ~ "phylum",
      !is.na(kingdom.itis) ~ "kingdom",
      TRUE ~ NA
    ),
  ) %>% 
  relocate(
    tax.uid, round, taxa.name,
    id.worms, id.gbif, id.ncbi, id.itis,
    rank.worms, rank.gbif, rank.ncbi, rank.itis,
    form.worms, form.gbif, form.ncbi, form.itis,
    variety.worms, variety.gbif, variety.ncbi, variety.itis,
    species.worms, species.gbif, species.ncbi, species.itis,
    genus.worms, genus.gbif, genus.ncbi, genus.itis,
    family.worms, family.gbif, family.ncbi, family.itis,
    suborder.worms, suborder.gbif, suborder.ncbi, suborder.itis,
    order.worms, order.gbif, order.ncbi, order.itis,
    subclass.worms, subclass.gbif, subclass.ncbi, subclass.itis,
    class.worms, class.gbif, class.ncbi, class.itis,
    subphylum.worms, subphylum.gbif, subphylum.ncbi, subphylum.itis,
    phylum.worms, phylum.gbif, phylum.ncbi, phylum.itis,
    subkingdom.worms, subkingdom.gbif, subkingdom.ncbi, subkingdom.itis,
    kingdom.worms, kingdom.gbif, kingdom.ncbi, kingdom.itis,
  )


#### Adding taxonomy to raw data ----
# 1) phyto/zoo_tax - join resolve to tax_list by taxa.name to get original.taxa.name and then join to raw data by original.taxa.name
all_raw_tax <- left_join(all_raw, tax_list_3, by = 'original.taxa.name') %>% 
  # remove any that were not included in the taxonomy because they couldn't be resolved
  filter(!is.na(tax.uid)) %>% 
  # remove unecessary columns
  select(-round, -gbif.id) %>% 
  # add in a type column whether zoo/phyto/insect
  mutate(
    type = case_when(
      #Zooplankton
      phylum %in% c("Rotifera", "Ciliophora", "Amoebozoa") ~ "Holoplankton",
      phylum == "Arthropoda" & class != "Insecta" & class != "Malacostraca" ~ "Holoplankton",
      phylum == "Annelida" ~ "Annelida",
      class == "Insecta" & life.stage == "adult" ~ "Insect",
      class == "Insecta" & life.stage != "adult" ~"Meroplankton",
      class == "Malacostraca" & life.stage != "adult" ~"Meroplankton",
      class == "Malacostraca" & life.stage == "adult" ~"Malacostraca",
      phylum == "Mollusca" & life.stage != "adult" ~"Meroplankton",
      phylum == "Mollusca" & life.stage == "adult" ~"Mollusca",
      # phytoplankton
      phylum %in% c('Spironematellophyta', 'Cercozoa', 'Rhodophyta', 'Choanozoa', 'Myzozoa', 'Bigyra', 'Euglenozoa', 'Chlorophyta', 'Charophyta', 'Haptophyta', 'Ochrophyta', 'Cyanobacteria', 'Cryptophyta') ~ "Phytoplankton"
    )
  ) %>% 
  # change adult to active in phytoplankton
  mutate(
    life.stage = case_when(
      type == "Phytoplankton" & life.stage == "adult" ~ "active",
      TRUE ~ life.stage
    )
  ) %>% 
  relocate(raw.uid, original.taxa.name, taxa.name, life.stage, rank, tax.uid, form, variety, species, genus, family, order, class, phylum, kingdom, cells.per.nu, nu.biovol, nu.biovol.mucilage, cell.biovol, biovol.ref, min.length, max.length, avg.length, min.width, max.width, avg.width, min.height, max.height, avg.height, min.biomass, max.biomass, avg.biomass, length.ref, biomass.ref, source, data.method, sample.start.year, sample.end.year, sample.month, location, country, continent, longitude, latitude)

# save 
saveRDS(all_raw_tax, file = "Data/taxize/all_raw_tax.rds")








## Initial run through classification ----
classification_raw <- resolved %>% 
  
  # Get disinct otts - use ott as this will give the exact taxa and won't mess up the duplicates changes done earlier
  distinct(ott_id) %>%
  
  # Use rowwise so it looks at each row one at a time - otherwise it will just use the first name and use this for all the rows
  rowwise() %>%
  
  mutate(
    
    # Run through classification
    tax = list( # need to set as list so that it makes it into a list column with each row containing a dataframe for that taxa
      classification(
        ott_id, db = "tol", return_id = FALSE, rows = 1 # rows = 1 so that is only takes the first one and doesn't require you select options for each one
      )[[1]] # select the first element of the list
    ),
    
    # Change ones that didn't get classified from just NA to a dataframe of NAs
    tax = ifelse(
      is.data.frame(tax),
      list(tax),
      list(data.frame(name = NA, rank = "no rank"))
    ),
    
    # Pivot tax so that it makes columns for each rank
    pivot_wider(tax,
                names_from = rank,
                values_from = name)
  ) %>% 
  
  # remove unnecessary columns
  select(
    - tax,
    - `no rank`
  ) %>% 
  
  # Separate columns that have multiple names for a rank into multiple columns
  unnest_wider(
    col = everything(), names_sep = "."
  ) %>% 
  
  rename(
    ott_id = ott_id.1
  ) %>% 
  
  ungroup() # ungroup to remove rowwise 






### Initial edits ----
# Do any edits that can easily be done within mutate

classification_formatted <- classification_raw %>% 
  
  #### Join in resolved.taxa.name ----
# Because it was run with the ott_ids it doesn't have the taxa.name so left join these on from resolved
left_join(
  select(
    resolved, ott_id, unique_name
  ),
  by = "ott_id"
) %>% 
  
  rename(
    tol.taxa.name = unique_name
  ) %>% 
  
  mutate(
    
    ##### Fill in gaps ----
    # Fill in gaps in taxonomy - this isn't changing where anything goes in the phylogeny just filling in gaps that tol didn't put in
    # Before any changes were done the taxa was first checked against the multiples list and changed to the multiple if needed
    
    # Resolved.taxa.name
    resolved.taxa.name = case_when(
      # Some taxa have been updated into a different genus but haven't had their species name changed so their genus is not the same as the genus part of the species name
      # Update these - can keep the same ott_id as it is in the right genus but just need the name updated
      
      tol.taxa.name == "Eolimna minima" ~ "Sellaphora nigri",
      tol.taxa.name == "Brebissonia lanceolata" ~ "Gomphonema lanceolatum",
      tol.taxa.name == "Diaptomus oregonensis" ~ "Skistodiaptomus oregonensis",
      tol.taxa.name == "Schizonema seminoides" ~ "Navicula seminoides",
      tol.taxa.name == "Sphaerellopsis lateralis" ~ "Vitreochlamys lateralis",
      tol.taxa.name == "Sphaerellopsis mucosa" ~ "Vitreochlamys mucosa",
      tol.taxa.name == "Sphaerellopsis velata" ~ "Vitreochlamys velata",
      tol.taxa.name == "Sphaerellopsis ampla" ~ "Vitreochlamys ampla",
      
      TRUE ~ tol.taxa.name
    ),
    
    # Species
    species = case_when(
      
      # Where the species name was missing but resolved.taxa.name was a species - these are generally ones that are in the OTL database but not in the synthetic tree
      resolved.taxa.name %in% c("Chrysastrella furcata", "Cystodinium cornifax", "Mytilina mucronata", "Mytilina ventralis", "Praetriceratium inconspicuum",
                                "Aulacoseira ambigua", "Navicula menisculus", "Cymbella proxima", "Conticribra weissflogii", "Adlafia parabryophila", "Hippodonta arkonensis",
                                "Daphnia sinensis", "Geissleria acceptata", "Dinobryon cylindricum", "Lenticulina muensteri", "Cymbopleura cuspidata", "Hippodonta lueneburgensis",
                                "Pseudopodosira echinus", "Stephanodiscus carconensis", "Sellaphora nigri", "Leptolyngbya tenuis", "Gomphonema lanceolatum", "Skistodiaptomus oregonensis",
                                "Navicula seminoides", "Vitreochlamys lateralis", "Vitreochlamys mucosa", "Vitreochlamys velata", "Vitreochlamys ampla") ~ resolved.taxa.name,
      
      TRUE ~ species.1
    ),
    
    # Genus
    genus = case_when(
      # Weird ones
      resolved.taxa.name == "Dinobryon (in Ochromonas sup.)" ~ "Dinobryon",
      resolved.taxa.name == "Rhizosolenia (in Bacillariophytina)" ~ "Rhizosolenia",
      resolved.taxa.name == "Palaeacmea" ~ "Palaeacmea",
      
      # Where the genus name was missing but the resolved.taxa.name was a genus - these are generally ones that are in the OTL database but not in the synthetic tree
      resolved.taxa.name %in% c("Cystodinium", "Tetralithus", "Lithoperidinium", "Petersophlebia", "Chrysastrella") ~ resolved.taxa.name,
      
      # Use the genus.2 column when it has a value in as these were correct
      !is.na(genus.2) ~ genus.2,
      
      # Otherwise just select genus.1
      TRUE ~ genus.1
    ),
    
    # When species is filled but genus is missing select just the first name in the species name
    genus = if_else(
      is.na(genus) & !is.na(species),
      stri_extract_first_regex(species, "\\w+"),
      genus
    ),
    
    # Some genus have a different name in their species name compared to genus, change to species one as all have been checked and they are just synonyms but still in same place in phylogeny
    species.1 = species
  ) %>% 
  
  separate(
    species.1, into = c("g", "s"), sep = " "
  ) %>% 
  
  mutate(
    g = if_else(
      is.na(g),
      genus,
      g
    ),
    
    genus.exp = if_else(
      genus != g,
      g,
      genus
    ),
    
    # Family
    # Too many missing family rows to do in mutate so will do this in excel and read into R
    
    family = case_when(
      # Use the family.2 column when it has a value in as these were correct
      !is.na(family.2) ~ family.2,
      
      # Otherwise use family.1
      TRUE ~ family.1
    ),
    
    # Order
    # Too many missing order rows to do in mutate so will do this in excel and read into R
    
    order = case_when(
      TRUE ~ order.1
    ),
    
    # Class
    # Will do class manually after order is done
    class = case_when(
      
      # Minor edits - these are phylums so changing to class and then will fill the phylum column with them
      class.1 == "Haptophyta" ~ "Coccolithophyceae",
      class.1 == "Glaucophyta" ~ "Glaucophyceae",
      
      TRUE ~ class.1
    ),
    
    # Phylum
    # Fill in phylum now because I need to get the types to work out which ones aren't zoo or phyto to remove before later steps
    
    phylum = case_when(
      
      # These are classes so change to the corresponding phylum
      phylum.1 == "Cryptophyceae" ~ "Cryptophyta",
      phylum.1 == "Euglenida" ~ "Euglenozoa",
      
      infraphylum.1 == "Dinoflagellata" ~ "Myzozoa",
      
      genus == "Acanthosphaera" ~ "Chlorophyta",
      genus == "Amoeba" ~ "Amoebozoa",
      genus %in% c("Crumenula", "Euglena") ~ "Euglenozoa",
      genus %in% c("Oscillatoria", "Schizothrix") ~ "Cyanobacteria",
      genus %in% c("Karlodinium", "Karenia") ~ "Myzozoa",
      
      family == "Collodictyonidae" ~ "Apusozoa",
      
      order == "Bicosoecida" ~ "Bigyra",
      order == "Choanoflagellida" ~ "Choanozoa",
      order %in% c("Eustigmatales", "Synurales", "Chrysomeridales") ~ "Ochrophyta",
      order == "Kinetoplastea" ~ "Euglenozoa",
      order == "Noctilucales" ~ "Myzozoa",
      order == "Centrohelida" ~ "Chlorophyta",
      
      class %in% c("Chrysophyceae", "Dictyochophyceae", "Raphidophyceae", "Xanthophyceae", "Phaeothamniophyceae", "Actinophryidae") ~ "Ochrophyta",
      class == "Coccolithophyceae" ~ "Haptophyta",
      class == "Glaucophyceae" ~ "Glaucophyta", 
      class == "Dinophyceae" ~ "Myzozoa",
      class %in% c("Zygnemophyceae", "Klebsormidiaceae") ~ "Charophyta",
      
      !is.na(phylum.2) ~ phylum.2, # Phylum.2 is more accurate than 1 so use this when it is not NA
      
      TRUE ~ phylum.1
    ),
    
    # Kingdom 
    kingdom = case_when(
      
      phylum %in% c("Cyanobacteria") ~ "Bacteria",
      phylum %in% c("Chlorophyta", "Charophyta", "Rhodophyta", "Glaucophyta") ~ "Plantae",
      phylum %in% c("Ochrophyta", "Bacillariophyta", "Haptophyta", "Cryptophyta", "Bigyra", "Myzozoa", "Ciliophora", "Cercozoa", "Foraminifera", "Bacillariophyta") ~ "Chromista",
      phylum %in% c("Choanozoa", "Euglenozoa", "Amoebozoa") ~ "Protozoa",
      phylum %in% c("Arthropoda", "Mollusca", "Rotifera", "Gastrotricha", "Cnidaria", "Bryozoa", "Chordata") ~ "Animalia",
      
      TRUE ~ NA
    ),
    
    # Type
    # Assign either phytoplankton or zooplankton and when it is neither then NA
    # Do have some insect larvae in there but not much so discarding it
    
    type = case_when(
      class %in% c("Amphibia", "Anthozoa", "Arachnida", "Malacostraca", "Insecta") ~ "remove",
      
      kingdom %in% c("Bacteria", "Plantae") ~ "Phytoplankton",
      kingdom == "Animalia" ~ "Zooplankton",
      
      phylum %in% c("Ochrophyta", "Haptophyta", "Bigyra", "Myzozoa", "Euglenozoa", "Bacillariophyta", "Cryptophyta") ~ "Phytoplankton",
      phylum %in% c("Cercozoa", "Amoebozoa", "Foraminifera", "Apusozoa", "Ciliophora", "Sarcomastigophora") ~ "Zooplankton",
      
      TRUE ~ "remove"
    )
  ) %>% 
  
  # Remove non plankton
  filter(
    !(resolved.taxa.name == "Marssoniella (genus in kingdom Archaeplastida)"),
    type != "remove"
  ) %>% 
  
  # Rename ott_id column - just cus there used to be tax.uid but has been changed to this so keep name as tax.uid to make it run with the rest of the code
  rename(
    taxa.name = resolved.taxa.name
  ) %>% 
  
  # Select columns
  select(
    ott_id, tol.taxa.name, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  )

### Family ----
# want to manually input missing ranks for family as will take too long to do in a case_when

# Make a list of missing family with their genus
missing_family <- classification_formatted %>% 
  select(
    genus,
    family
  ) %>% 
  
  filter(
    is.na(family)
  ) %>% 
  
  distinct(
    genus
  )

# save
write_csv(missing_family, "R/data_outputs/database_products/taxonomy/missing_family.csv")

# Import the manually filled family
manually_family <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "family_tol")

# Update family
classification_family <- classification_formatted %>% 
  
  left_join(manually_family, by = "genus", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    family = if_else(
      !is.na(family.manual),
      family.manual,
      family.tol
    )
  ) %>% 
  
  select(
    - family.manual,
    - family.tol
  )

### Order ----
# Want to manually input missing ranks for order as will take too long to do in a case_when

# Make a list of missing order with their family
missing_order <- classification_family %>% 
  select(
    family,
    order
  ) %>% 
  
  filter(
    is.na(order)
  ) %>% 
  
  distinct(
    family
  )

# Save
write_csv(missing_order, "R/data_outputs/database_products/taxonomy/missing_order.csv")

# Import the manually filled order
manually_order <- read_xlsx("raw_data/manual_taxonomy.xlsx", sheet = "order_tol")

# Update order
classification_order <- classification_family %>% 
  
  left_join(manually_order, by = "family", suffix = c(".tol", ".manual")) %>% 
  
  mutate(
    order = if_else(
      !is.na(order.manual),
      order.manual,
      order.tol
    ),
    
    order = case_when(
      taxa.name == "Euglenida" ~ "Euglenida",
      
      TRUE ~ order
    )
  ) %>% 
  
  select(
    - order.manual,
    - order.tol
  ) %>% 
  
  relocate(
    ott_id, tol.taxa.name, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  )

### Final edits ----
# Final edits can just be done with case_when

classification <- classification_order %>% 
  
  mutate(
    
    # Class
    # Fill in missing class rank
    
    class = case_when(
      class == "Mediophyceae" ~ "Bacillariophyceae",
      
      order == "Kinetoplastea" ~ "Kinetoplastea",
      
      order == "Choanoflagellida" ~ "Zoomastigophora",
      order == "Eustigmatales" ~ "Eustigmatophyceae",
      order == "Pyramimonadales" ~ "Pyramimonadophyceae",
      order == "Cryptomonadales" ~ "Cryptophyceae",
      order %in% c("Fragilariales", "Tabellariales", "Coscinodiscales", "Aulacoseirales", "Melosirales", "Thalassiosirales", "Leptocylindrales", "Paraliales") ~ "Bacillariophyceae",
      order == "Synurales" ~ "Chrysophyceae",
      order == "Bangiales" ~ "Bangiophyceae",
      order == "Centrohelida" ~ "Centrohelea",
      order == "Chrysomeridales" ~ "Chrysomeridophyceae",
      order %in% c("Chroococcales", "Oscillatoriales", "Nostocales", "Pseudanabaenales", "Pleurocapsales", "Synechococcales", "Nodosilineales", "Spirulinales", "Leptolyngbyales") ~ "Cyanophyceae",
      order %in% c("Noctilucales", "Gymnodiniales") ~ "Dinophyceae",
      order == "Bicosoecida" ~ "Bicosoecophyceae",
      order == "Heteronematales" ~ "Euglenida",
      order == "Vaginulinida" ~ "Nodosariata",
      order %in% c("Euglenales", "Eutreptiales") ~ "Euglenophyceae",
      order == "Natomonadida" ~ "Peranemea", 
      order == "Mischococcales" ~ "Xanthophyceae",
      order %in% c("Gomontiellales", "Coleofasciculales", "Pelonematales", "Chroococcidiopsidales", "Gloeobacterales", "Geitlerinematales") ~ "Cyanophyceae",
      order == "Spironematellales" ~ "Spironematellophyceae",
      order == "Goniomonadales" ~ "Goniomonadophyceae",
      order == "Petalomonadida" ~ "Stavomonadea",
      order == "Amphilepidida" ~ "Ophiuroidea",
      order == "Picocystales" ~ "Picocystophyceae",
      order == "Enteropneusta incertae sedis" ~ "Enteropneusta",
      order == "Euglenida" ~ "Euglenoidea",
      order == "Actinophryida" ~ "Raphidophyceae",
      
      taxa.name == "Cryptophyceae" ~ "Cryptophyceae",
      
      TRUE ~ class
    ),
    
    # Change order Kinetoplastea
    order = case_when(
      order == "Kinetoplastea" ~ NA,
      TRUE ~ order
    )
    
  ) %>% 
  
  # Reorder
  relocate(
    ott_id, tol.taxa.name, taxa.name, type, species, genus, family, order, class, phylum, kingdom
  ) %>% 
  
  # get distinct taxa
  distinct(
    ott_id, .keep_all = TRUE
  )

# Save
saveRDS(classification, file = "R/data_outputs/database_products/taxonomy/classification.rds")









