## Exploring the data from the aggregation simulations

## Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)

## Tian
## Read in data
tian_agg <- read_csv(here("Julia/Data_outputs", "tian_agg"))

## Get mean, standard deviation and standard error of data
tian_sum <- tian_agg %>% 
  group_by(aggregation_level) %>% 
  summarise(
    mean_stab = mean(FinalStability),
    sd_stab = sd(FinalStability),
    se_stab = sd(FinalStability)/sqrt(n())
  )

## Plot
ggplot(tian_sum, aes(x = factor(aggregation_level, levels = c("Genus", "Family", "Order")), y = mean_stab))+
  geom_point()+
  geom_errorbar(aes(ymin = mean_stab - sd_stab,
                    ymax = mean_stab + sd_stab),
                width = 0.1) + # Changes the width of the error bar hats
  ylab("Stability") +
  xlab("Agrregation level")








