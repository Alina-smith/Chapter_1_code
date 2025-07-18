# body size spread

# packages
library(tidyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(patchwork)
library(ggfortify)
library(agricolae)
library(multcomp)

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
    )
  )

# reorder groups for plotting anbd make as factor 
avg_mass$taxonomic.group <- factor(avg_mass$taxonomic.group, levels = c("Blue-green", "Dinoflagellates", "Green", "Diatoms", "Stramenopiles", "Charophytes", "Chrysophytes", "Euglenoids", "Haptophytes", "Cryptomonads", "Glaucophytes", "Ciliates", "Rotifers", "Copepods", "Ostracods", "Cladocerans"))
avg_mass$functional.group <- factor(avg_mass$functional.group, levels = c("A", "D", "MP", "J", "X1", "LO", "Y", "X3", "H1", "S1", "K", "S2", "F", "X2", "W1", "SN", "N", "T", "TC", "H2", "Q", "P", "G", "B", "E", "1", "2", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "16"))
avg_mass$type <- factor(avg_mass$type, levels = c("Phytoplankton", "Zooplankton"))

# Histogram ----

## Type ----
spread_type <- ggplot(avg_mass, aes(x = log10(avg.dw), fill = type))+
  geom_histogram(binwidth = 0.3) +
  #scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#555555"))+
  scale_fill_manual(values = c("Phytoplankton" = "#97B498", "Zooplankton" = "#A3C4DC"))+
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
  labs(
    x = "Log mean body size (ug)",
    y = "Count (log10)"
  )

spread_type # View

# find overlapping range
range_by_group <- avg_mass %>%
  group_by(type) %>%
  summarise(
    min_mass = min(avg.dw),
    max_mass = max(avg.dw)
  )

# save
ggsave("R/Data_outputs/plots/spread_type.png", plot = spread_type, width = 8, height = 5)

## Taxonomic group ----
spread_group <- ggplot(avg_mass, aes(x = log10(avg.dw), fill = type))+
  geom_histogram(binwidth = 0.7)+
  #scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#555555"))+
  scale_fill_manual(values = c("Phytoplankton" = "#97B498", "Zooplankton" = "#A3C4DC"))+
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
  labs(
    x = "Log mean body size (ug)",
    y = "Count (log10)"
    )

spread_group

# save
ggsave("R/Data_outputs/plots/spread_group.png", plot = spread_group, width = 10, height = 5)

## Functional group ----
spread_fg <- ggplot(avg_mass, aes(x = log10(avg.dw), fill = type))+
  geom_histogram(binwidth = 0.5)+
  #scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#555555"))+
  scale_fill_manual(values = c("Phytoplankton" = "#97B498", "Zooplankton" = "#A3C4DC"))+
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
  labs(
    x = "Log mean body size (ug)",
    y = "Count (log10)"
  )

spread_fg

# save
ggsave("R/Data_outputs/plots/spread_fg.png", plot = spread_fg, width = 20, height = 10)

## Join together ----

all_spread <- (spread_type+spread_group)/spread_fg

all_spread

# save
ggsave("R/Data_outputs/plots/all_spread.png", plot = all_spread, width = 20, height = 13)

# box plot ----

## Type ----
type_box <- ggplot(avg_mass, aes(x = type, y = log(avg.dw), fill = type)) +
  geom_boxplot()+
  scale_fill_manual(values = c("Phytoplankton" = "#97B498", "Zooplankton" = "#76B7B2"))+
  labs(x = "Type",
       y = " Log mean body size (ug)")

# view
type_box

## Taxonomic group ----
group_box <- ggplot(avg_mass, aes(x = taxonomic.group, y = log(avg.dw), fill = type)) +
  geom_boxplot()+
  #scale_fill_manual(values = c("Phytoplankton" = "#999999", "Zooplankton" = "#555555"))+
  scale_fill_manual(values = c("Phytoplankton" = "#97B498", "Zooplankton" = "#76B7B2"))+
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

# ANOVA ----
## Format data ----
zoo_sub <- avg_mass %>% 
  filter(type == "Zooplankton")

phyto_sub <- avg_mass %>% 
  filter(type == "Phytoplankton")

## Type ----

# Fit model
lm_type <- lm(log(avg.dw) ~ type, data = avg_mass)

# Check assumptions
autoplot(lm_type)

# F-test
an_type <- anova(lm_type)
summary(an_type)

# Post hoc test
## aov
type_aov <- aov(lm_type)
type_aov

## Tukey test
tukey_type <- glht(type_aov,linfct = mcp(type = "Tukey"))
summary(tukey_type) # Significant difference between zoo and phyto

## Taxonomic group ----

# Fit model
lm_zoo_tg <- lm(log(avg.dw) ~ taxonomic.group, data = zoo_sub)
lm_phyto_tg <- lm(log(avg.dw) ~ taxonomic.group, data = phyto_sub)

# Check assumptions
autoplot(lm_zoo_tg)
autoplot(lm_phyto_tg)

# F-test
an_zoo_tg <- anova(lm_zoo_tg)
an_phyto_tg <- anova(lm_phyto_tg)

summary(an_zoo_tg)
summary(an_phyto_tg)

# Post-hoc test
## aov
zoo_tg_aov <- aov(lm_zoo_tg)
phyto_tg_aov <- aov(lm_phyto_tg)

zoo_tg_aov
phyto_tg_aov

## Tukey test
tukey_zoo_tg <- glht(zoo_tg_aov,linfct = mcp(taxonomic.group = "Tukey"))
tukey_phyto_tg <- glht(phyto_tg_aov,linfct = mcp(taxonomic.group = "Tukey"))

summary(tukey_zoo_tg) # no significant difference within crustaceans and microzoo, significant difference between all the crustancean groups and microzoo groups
summary(tukey_phyto_tg)

## Plot
plot(tukey_zoo_tg)
plot(tukey_phyto_tg)

## Functional group ----

# Fit model
lm_zoo_fg <- lm(log(avg.dw) ~ functional.group, data = zoo_sub)
lm_phyto_fg <- lm(log(avg.dw) ~ functional.group, data = phyto_sub)

# Check assumptions
autoplot(lm_zoo_fg)
autoplot(lm_phyto_fg)

# F-test
an_zoo_fg <- anova(lm_zoo_fg)
an_zoo_fg <- anova(lm_phyto_fg)

summary(an_zoo_fg)
summary(an_phyto_fg)

# Post hoc test
## aov
zoo_fg_aov <- aov(lm_zoo_fg)
phyto_fg_aov <- aov(lm_phyto_fg)

zoo_fg_aov
phyto_fg_aov

## Tukey test
tukey_zoo_fg <- glht(zoo_fg_aov,linfct = mcp(functional.group = "Tukey"))
tukey_phyto_fg <- glht(phyto_fg_aov,linfct = mcp(functional.group = "Tukey"))

summary(tukey_zoo_fg)
summary(tukey_phyto_fg)

## Plot
plot(tukey_zoo_fg)
plot(tukey_phyto_fg)


