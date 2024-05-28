## exploring the data

# packages
library(tidyr)
library(dplyr)
library(patchwork)
library(ggplot2)

# data
all_raw_tax <- readRDS("Data/taxize/all_raw_tax.rds")
tax_list <- readRDS("Data/taxize/tax_list.rds")
zoo <- readRDS("Data/data_procesing/zoo_tax.rds")
phyto <- readRDS("Data/phyto_tax.rds")


###### histograms of groups ----
group_hist <- all_raw_tax %>%
  distinct(taxa.name, .keep_all = TRUE) %>% 
  filter(type == "Phytoplankton" | type ==  "Meroplankton" | type == "Holoplankton") %>% 
  mutate(
    group = case_when(
      type == "Phytoplankton" ~ phylum,
      class == "Insecta" & life.stage == 'larvae' ~ "Insect larvae",
      class == "Insecta" & life.stage == 'nymph' ~ "Insect nymph",
      class == "Copepoda" ~ "Copepod",
      order == "Diplostraca" ~ "Cladoceran",
      phylum == "Ciliophora" ~ "Ciliate",
      phylum == "Rotifera" ~ "Rotifer",
      phylum == "Amoebozoa" ~ "Amoebozoa",
      TRUE ~ "Uniassigned"
    ),
    group = factor(group, levels = c('Ochrophyta', 'Chlorophyta', 'Charophyta', 'Cyanobacteria', 'Myzozoa', 'Cryptophyta', 'Bigyra', 
                                     'Choanozoa', "Ciliophora", "Euglenozoa","Rhodophyta", "Spironematellophyta", "Haptophyta", "Cercozoa", "Cladoceran",
                                     "Copepod", "Rotifer", "Amoebozoa", "Ciliate", "Insect nymph", "Insect larvae")
    )
  )

ggplot(group_hist, aes(x = group, fill = type))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("#458B74","#458B74", "#6CA6CD"))+
  labs(x = "", title = "Zooplankton Groups")+
  guides(fill = FALSE)


## spread of body size
log_biomass <- avg_biomass %>% 
  mutate(
    log.indv.biomass = log(mean.biomass)
  )

phyto_biomass <- log_biomass %>% 
  filter(type == "Phytoplankton")

zoo_biomass <- log_biomass %>% 
  filter(type == "Holoplankton"| type == "Meroplankton")

phyto_plot <- ggplot(phyto_biomass, aes(log.indv.biomass))+
  geom_histogram(binwidth = 1, fill = "#458B74")+
  labs(title = "Phytoplankton", x = "log mean biomass")+
  scale_x_continuous(limits = c(-30, 6))

zoo_plot <- ggplot(zoo_biomass, aes(log.indv.biomass))+
  geom_histogram(binwidth = 1, fill = "#6CA6CD")+
  labs(title = "Zoooplankton", x = "log mean biomass")+
  scale_x_continuous(limits = c(-30, 6))

(phyto_plot / zoo_plot)









group_hist <- all_raw_tax %>%
  distinct(taxa.name, .keep_all = TRUE) %>% 
  filter(type == "Phytoplankton" | type ==  "Meroplankton" | type == "Holoplankton") %>% 
  mutate(
    group = case_when(
      # Phytoplankton
      phylum %in% c("Charophyta", "Chlorophyta") ~ 'Green aglae',
      class == "Xanthophyceae" ~ 'Yellow-green algae',
      class == "Bacillariophyceae" ~ 'Diatoms',
      class %in% c('Cryptophyceae', "Chrysophyceae", 'Phaeophyceae', 'Dinophyceae', "Haptophyta") ~ "Brown algae",
      class == "Cyanophyceae" ~ 'Blue/green algae',
      phylum == "Rhodophyta" ~ "Red algae",
      # Zooplankton
      class == "Insecta" & life.stage == 'larvae' ~ "Insect larvae",
      class == "Insecta" & life.stage == 'nymph' ~ "Insect nymph",
      class == "Copepoda" ~ "Copepod",
      order == "Diplostraca" ~ "Cladoceran",
      phylum == "Ciliophora" ~ "Ciliate",
      phylum == "Rotifera" ~ "rotifer", 
      TRUE ~ "Unassigned"
    )
    
    
    