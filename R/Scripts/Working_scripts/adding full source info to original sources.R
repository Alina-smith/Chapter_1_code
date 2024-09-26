## joining full source info to original sources

#### Packages
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(taxize)
library(ggplot2)
library(here)
library(data.table)

#### Data ---- 
sources_shortlist <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "sources_shortlist")
full_info <- read_xlsx(here("Raw_data","original_source_full_info.xlsx"), sheet = "original_source_full")
source_list_full <- read_xlsx(here("Raw_data","Master_WOS_data.xlsx"), sheet = "source_list")

source_list <- left_join(sources_shortlist, full_info, by = "doi") %>% 
  mutate(
    authors = ifelse(
      list.name == "original source" & !is.na(doi),
      authors.y,
      authors.x
      ),
    title = ifelse(
      list.name == "original source" & !is.na(doi),
      title.y,
      title.x
    ),
    publication = ifelse(
      list.name == "original source" & !is.na(doi),
      publication.y,
      publication.x
    ),
    year = ifelse(
      list.name == "original source" & !is.na(doi),
      year.y,
      year.x
    ),
    volume = ifelse(
      list.name == "original source" & !is.na(doi),
      volume.y,
      volume.x
    ),
    issue = ifelse(
      list.name == "original source" & !is.na(doi),
      issue.y,
      issue.x
    ),
    start.page.no = ifelse(
      list.name == "original source" & !is.na(doi),
      start.page.no.y,
      start.page.no.x
    ),
    end.page.no = ifelse(
      list.name == "original source" & !is.na(doi),
      end.page.no.y,
      end.page.no.x
    ),
    authors.full.name = ifelse(
      list.name == "original source" & !is.na(doi),
      authors.full.name.y,
      authors.full.name.x
    ),
    ISSN = ifelse(
      list.name == "original source" & !is.na(doi),
      ISSN.y,
      ISSN.x
    ),
    eISSN = ifelse(
      list.name == "original source" & !is.na(doi),
      eISSN.y,
      eISSN.x
    ),
    ISBN = ifelse(
      list.name == "original source" & !is.na(doi),
      ISBN.y,
      ISBN.x
    ),
    book.doi = ifelse(
      list.name == "original source" & !is.na(doi),
      book.doi.y,
      book.doi.x
    ),
    wos.uid = ifelse(
      list.name == "original source" & !is.na(doi),
      wos.uid.y,
      wos.uid.x
    ),
  ) %>% 
  select(paper.code, source.code, list.name, in.text.citation, authors, title, publication, year, volume, issue, start.page.no, end.page.no, doi, authors.full.name, ISSN, eISSN, ISBN, book.doi, wos.uid)

write_csv(source_list, "Raw_data/source_list.csv")

DOI <- together %>% 
  filter(list.name == "original source" & is.na(wos.uid))

