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

# creatng columns for each part of the reference
zotero_source_list_edit <- zotero_source_list %>% 
  mutate(
    source.code = row_number(),
    
    # author
    
    
    # year
    year = stri_extract_first_regex(source, "\\(\\d{4}\\)|\\(\\d{4}\\w*\\)"),# select the date in the bracket
    year = stri_replace_all_regex(year, "\\(|\\)", ""), # remove bracket
    
    # title
    title = case_when(
      source.code == "484" ~ "On the bicoecidae: a family of colourless flagellates",
      source.code == "563" ~ "SEASONAL VARIATION IN LENGTH OF COPEPODIDS AND ADULTS OF DIACYCLOPS THOMASI (FORBES) IN TWO COLORADO MONTANE RESERVOIRS (COPEPODA)",
      source.code == "393" ~ "Time series of phytoplankton biovolume at the depth of the vertical chlorophyll maximum in Falling Creek Reservoir, Vinton, VA, USA 2016-2019",
      TRUE ~ stri_extract_first_regex(source, "(?<=\\(\\d{4}\\) ‘|\\(\\d{4}\\w\\) ‘).*(?=’,)")
      ),
    
    #doi
    doi = stri_extract_first_regex(source, "https://doi.org/(?<=https://doi.org/).*"),
    doi = stri_replace_last_regex(doi, "\\.", ""), # get rid of full stop at the end
    doi = stri_replace_last_regex(doi, "https://doi.org/", ""),
    
    # page
    page = stri_extract_first_regex(source, "(?<= pp. | p. ).*(?=\\. Available)"),
    
    # volume and issue
    volume = stri_extract_first_regex(source, ", \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))*"), # easier to select volume and issue together with the regex and then separate them out
    issue = case_when(
      source.code == "508" ~ "308",
      source.code == "531" ~ "308",
      TRUE ~ stri_extract_first_regex(volume, "\\(\\d+(–\\d+)*\\)")
    ),
    volume = stri_replace_all_regex(volume, "\\(\\d+(–\\d+)*\\)", ""),
    volume = stri_replace_all_regex(volume, "\\, ", ""))

zotero_source_list

    
    publication = stri_extract_first_regex(source, "’, \\S+ \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))"))
                                           
                                           .*(?=, \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\)))(?<=’, ).*"))
    
    ", \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))*"
    
    ‘’, Limnology and Oceanography, 34(5), pp. 831–839.
    
    source.edit = stri_replace_first_regex(source,"(?<=\\(\\d{4}\\) ‘|\\(\\d{4}\\w\\) ‘).*(?=’,)", ""),
    source.edit = 
    
    "("
    
    Abdel-Aziz, F.A. et al. (2020) ‘’, 
    
    journal.book = 
    
508 531issue 
    
    
    
                                    \\S+(–\\S+)*.*),
    page = stri_replace_first_regex(source, ".", ""))


    source.edit = stri_replace_first_regex(source.edit, " pp. \\S+(–\\S+)*| p. \\S+(–\\S+)*",""),
    
    volume = stri_extract_first_regex(source.edit, ", \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))*"),
    issue = stri_extract_first_regex(volume, "\\(\\d+(–\\d+)*\\)"),
    volume = stri_replace_all_regex(volume, "\\(\\d+(–\\d+)*\\)", ""),
    volume = stri_replace_all_regex(volume, "\\, ", ""),
    source.edit = stri_replace_first_regex(source.edit, ", \\d+(–\\d+)*(\\(\\d+(–\\d+)*\\))*",""),
     
  )

)
title = case_when(
  source.code == "484" ~ "On the bicoecidae: a family of colourless flagellates",
  source.code == "563" ~ "SEASONAL VARIATION IN LENGTH OF COPEPODIDS AND ADULTS OF DIACYCLOPS THOMASI (FORBES) IN TWO COLORADO MONTANE RESERVOIRS (COPEPODA)",
  TRUE ~ stri_extract_first_regex(source.edit, "(?<=\\(\\d{4}\\) ').*"))
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


source.edit = case_when(
  source.code == "484" ~ "(1941) Philosophical Transactions of the Royal Society of London. Series B, Biological Sciences, 230(575), pp. 451–473. Available at: https://doi.org/10.1098/rstb.1941.0002.",
  source.code == "563" ~ "(1989) Journal of Crustacean Biology, 9(1), pp. 67–76. Available at: https://doi.org/10.1163/193724089X00214.",
  source.code == "393" ~ "Environmental Data Initiative. Available at: https://doi.org/10.6073/PASTA/2DE760E8B72E474C31E42526F5360F9A.",
  TRUE ~ stri_replace_first_regex(source.edit, "(?<=\\(\\d{4}\\) ‘|\\(\\d{4}\\w\\) ‘).*(?=’,)", "")
),
source.edit = stri_replace_first_regex(source.edit, ".*(?=\\(\\d{4}\\)|\\(\\d{4}\\w\\))", ""),
source.edit = stri_replace_first_regex(source.edit, "\\(\\d{4}\\)|\\(\\d{4}\\w\\)", "")






















