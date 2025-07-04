# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(patchwork)

# Import data ----
taxonomy_list <- readRDS("R/data_outputs/database_products/final_products/taxonomy_list.rds")
bs <- readRDS("R/data_outputs/database_products/final_products/plankton_genus_traits.rds") %>% 
  
  # First calculate mean for each source
  group_by(
    source.code, taxa.name
  ) %>% 
  
  summarise(
    mean.dw = mean(dw.ug),
    .groups = "drop"
  ) %>% 
  
  # add back in taxonomy info
  left_join(
    taxonomy_list, by = "taxa.name"
  )
  
# reorder groups 
bs$taxonomic.group <- factor(bs$taxonomic.group, levels = c("Blue-green", "Dinoflagellates", "Green", "Diatoms", "Stramenopiles", "Streptophyta", "Chrysophytes", "Euglenoids", "Haptophytes", "Cryptomonads", "Glaucophytes", "Ciliates", "Rotifers", "Copepods", "Ostracods", "Cladocerans"))


# Get mean masses ----
# calcuating mean of mean apposed to weighted mean
avg_mass <- bs %>% 
  
  # Calculate the mean of the means
  group_by(
    taxa.name
  ) %>% 
  
  summarise(
    avg.dw = mean(mean.dw),
    .groups = "drop"
  ) %>% 
  
  # add back in extra info
  left_join(
    taxonomy_list, by = "taxa.name"
  ) %>% 
  
  mutate(
    functional.group = case_when(
      is.na(functional.group) ~ NA,
      stri_detect_regex(functional.group, "\\/") ~ stri_extract_first_regex(functional.group, "//S+(?=\\/)"),
      !(stri_detect_regex(functional.group, "\\/")) ~ functional.group,
      TRUE ~ NA
    ),
    
    # # Rename groups so the fit on graph easier
    # taxonomic.group = case_when(
    #   taxonomic.group == "Blue-green" ~ "Blue-\ngreen",
    #   taxonomic.group == "Dinoflagellate" ~ "Dino-\nflagellate",
    #   TRUE ~ taxonomic.group
    # )
  )

# reorder groups for plotting
avg_mass$taxonomic.group <- factor(avg_mass$taxonomic.group, levels = c("Blue-green", "Dinoflagellates", "Green", "Diatoms", "Stramenopiles", "Streptophyta", "Chrysophytes", "Euglenoids", "Haptophytes", "Cryptomonads", "Glaucophytes", "Ciliates", "Rotifers", "Copepods", "Ostracods", "Cladocerans"))
avg_mass$functional.group <- factor(avg_mass$functional.group, levels = c("A", "D", "MP", "J", "X1", "LO", "Y", "X3", "H1", "S1", "K", "S2", "F", "X2", "W1", "SN", "N", "T", "TC", "H2", "Q", "P", "G", "B", "E", "1", "2", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "16"))

# Histogram ----

# facet by type
spread_type <- ggplot(avg_mass, aes(x = log10(avg.dw), fill = type))+
  geom_histogram(binwidth = 0.35) +
  scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#333333"))+
  facet_wrap(~ type, ncol = 1)+
  scale_y_log10()+
  guides(fill = "none")+
  ggtitle("a)")+
  theme(
    strip.text = element_text(size = 7),
    title = element_text(size = 5),
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5)
  )+
  labs(x = "Log mean body size (ug)")

spread_type # View

# save
ggsave("R/Data_outputs/plots/spread_type.png", plot = spread_type, width = 8, height = 5)

# Facet by group
spread_group <- ggplot(avg_mass, aes(x = log10(avg.dw), fill = type))+
  geom_histogram(binwidth = 0.7)+
  scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#333333"))+
  facet_wrap(~ taxonomic.group, ncol = 11)+
  scale_y_log10()+
  #guides(fill = "none")+
  ggtitle("b)")+
  theme(
    strip.text = element_text(size = 7),
    title = element_text(size = 5),
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )+
  labs(x = "Log mean body size (ug)")

spread_group

# save
ggsave("R/Data_outputs/plots/spread_group.png", plot = spread_group, width = 10, height = 5)

# Facet by fg
spread_fg <- ggplot(avg_mass, aes(x = log10(avg.dw), fill = type))+
  geom_histogram(binwidth = 0.5)+
  scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#333333"))+
  facet_wrap(~ functional.group, ncol = 25)+
  scale_y_log10()+
  guides(fill = "none")+
  ggtitle("c)")+
  theme(
    strip.text = element_text(size = 7),
    title = element_text(size = 5),
    axis.text.x = element_text(size = 5, angle = -45),
    axis.text.y = element_text(size = 5)
  )+
  labs(x = "Log mean body size (ug)")

spread_fg

# save
ggsave("R/Data_outputs/plots/spread_fg.png", plot = spread_fg)

# Join all together

all_spread <- (spread_type+spread_group)/spread_fg

all_spread

# save
ggsave("R/Data_outputs/plots/all_spread.png", plot = all_spread, width = 20, height = 13)

# box plot ----

# type
type_box <- ggplot(avg_mass, aes(x = type, y = log(avg.dw), fill = type)) +
  geom_boxplot()+
  scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#555555"))+
  labs(x = "Type",
       y = " Log mean body size (ug)")

# view
type_box

# group
group_box <- ggplot(bs, aes(x = taxonomic.group, y = log(mean.dw), fill = type)) +
  geom_boxplot()+
  scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#555555"))+
  labs(x = "Taxonomic group",
       y = " Log mean body size (ug)")+
  theme(
    strip.text = element_text(size = 7),
    title = element_text(size = 5),
    axis.text.x = element_text(size = 7, angle = -45),
    axis.text.y = element_text(size = 7),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5)
  )

# view
group_box

# save
ggsave("R/Data_outputs/plots/group_box.png", plot = group_box, height = 10, width = 9)

# Stats ----
## Format data ----
zoo_sub <- avg_mass %>% 
  filter(type == "Zooplankton")

phyto_sub <- avg_mass %>% 
  filter(type == "Phytoplankton")

## Anova ----
# groups
group_anova_zoo <- aov(avg.dw ~ taxonomic.group, data = zoo_sub)
summary(group_anova_zoo)

group_anova_phyto <- aov(avg.dw ~ taxonomic.group, data = phyto_sub)
summary(group_anova_phyto)

# fg
fg_anova_zoo <- aov(avg.dw ~ functional.group, data = zoo_sub)
summary(fg_anova_zoo)

fg_anova_phyto <- aov(fg_avg.dw ~ functional.group, data = phyto_sub)
summary(anova_phyto)


