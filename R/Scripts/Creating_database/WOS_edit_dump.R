## not their data edits

# data
four <- read.csv("Raw_data/not_their_data/4.csv")
seventeen <- read.csv("Raw_data/not_their_data/17.csv")
two51 <- read.csv("Raw_data/not_their_data/251.csv")

# 4
# select unique papers and get doi from ones that have it
four_1 <- four %>% 
  distinct(paper) %>% 
  mutate(
    doi = stri_extract_first_regex(paper, "(?<=doi: )\\S+") # select any non white spaces after the regex "doi: "
  )
# select ones that have doi
four_doi <- four_1 %>% 
  filter(
    !is.na(doi)
  )
# select ones without doi
four_na <- four_1 %>% 
  filter(
    is.na(doi)
  )
# add them together
four_edit <- bind_rows(four_doi, four_na)
# save
write.csv(four_edit, "not_their_data/4_edit.csv")

# 17
# select unique papers and get doi from ones that have it
seventeen_1 <- seventeen %>% 
  distinct(References) %>% 
  mutate(
    doi = stri_extract_first_regex(References, "(?<=doi:)\\S+") # select any non white spaces after the regex "doi: "
  ) 
# select ones that have doi
seventeen_doi <- seventeen_1 %>% 
  filter(
    !is.na(doi)
  )
# select ones without doi
seventeen_na <- seventeen_1 %>% 
  filter(
    is.na(doi)
  )
# add them together
seventeen_edit <- bind_rows(seventeen_doi, seventeen_na)
# save
write.csv(seventeen_edit, "not_their_data/17_edit.csv")

## 251
two51_edit <- two51 %>% 
  filter(
    Habitat.Location %in% c("Yeniseher Lake", "Keban dam lake (Turkey)", "Southeastern Lake Michigan (MI, USA)", "Green Mountain Reservoir (Colorado)", "Lake Dillon (Colorado)", "Lake Aquatina, Italy")
  ) %>% 
  distinct(Source)
# save
write.csv(two51_edit, "not_their_data/251_edit.csv")



full_list <- read.csv("Raw_data/WOS_traits_list.csv")
shortlist <- read.csv("Raw_data/shortlist_list.csv") %>% 
  rename(
    Article.Title = title
  )

new_list <- left_join(shortlist, select(full_list, "Article.Title", "Author.Full.Names", "ISSN", "eISSN", "ISBN", "Book.DOI", "UT..Unique.WOS.ID."), by = "Article.Title")
# save
write.csv(new_list, "new_list.csv")





## getting reference list from secondar sources into same format as WOS
## Data
zotero_source_list <- read_xlsx(here("Raw_data", "zotero_original_source_list.xlsx"), sheet = "source_list")
zotero_source_list_full_name <- read_xlsx(here("Raw_data", "zotero_original_source_list.xlsx"), sheet = "full_name")

full_name_list <- zotero_source_list_full_name %>% 
  mutate(
    # author
    author = stri_extract_first_regex(source, ".*(?=, \\d{4}\\.)"),
    # doi
    doi = stri_extract_first_regex(source, "https://doi.org/(?<=https://doi.org/).*"),
  ) 


# creatng columns for each part of the reference
zotero_source_list_edit <- zotero_source_list %>% 
  mutate(
    source.edit = source,
    
    # year
    year = stri_extract_first_regex(source.edit, "\\(\\d{4}\\)|\\(\\d{4}\\w*\\)"),# select the date in the bracket
    year = stri_replace_all_regex(year, "\\(|\\)", ""), # remove bracket
    source.edit = stri_replace_first_regex(source.edit, "\\(\\d{4}\\)",""),
    
    doi = stri_extract_first_regex(source.edit, "https://doi.org/(?<=https://doi.org/).*"),
    source.edit = stri_replace_first_regex(source.edit, "Available at: https://doi.org/(?<=https://doi.org/).*",""),
   
    page = stri_extract_first_regex(source.edit, "(?<= pp. | p. )\\S+(–\\S+)*"),
    source.edit = stri_replace_first_regex(source.edit, " pp. \\S+(–\\S+)*| p. \\S+(–\\S+)*",""),
    
    issue = stri_extract_first_regex(source.edit, "")
    
    volume_issue = stri_extract_first_regex(source.edit, ", \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))*"),
     
  )

)
    
    # issue
    issue = stri_extract_first_regex(source.edit, "\\(\\d+(–\\d+)*\\), pp.( \\d+(–\\d+)*)*|\\(\\d+(–\\d+)*\\), p.( \\d+(–\\d+)*)*"))

# get issue by selecting digits that are followed by pp. so avoid getting things from title when issue isn't stated
    issue = stri_extract_first_regex(issue, "\\d+(–\\d+)*") # select just the digits
    
    
    )

pp. 91–98



,
    year = stri_extract_first_regex(year, "(\\d{4})"),
    source.edit = stri_replace_first_regex(source.edit, "(, \\d{4}\\.)",""),
    
    # doi
    doi = stri_extract_first_regex(source.edit, "(?<=https://doi.org/).*"),
    source.edit = stri_replace_first_regex(source.edit, "(?<=https://doi.org/).*",""),
    source.edit = stri_replace_first_regex(source.edit, "https://doi.org/","")
  ) 

%>% 
  select(source.edit, journal_volume)
  
. Hydrobiologia 232,

https://doi.org/
(?<=b)a

























