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





wander_raw <- read_xlsx("raw_data/master_wos_data.xlsx", sheet = "wander_raw", guess_max = 100000)
wander_names <- read_xlsx("raw_data/master_wos_data.xlsx", sheet = "wander_names")

# Weird ones ----
# ones i need to edit seperately and then add in as they were confusing to just add into the master excel sheet
wander_edit <- wander_raw %>% 
  
  # select(
  #   site_no,
  #   collect_date,
  #   TaxaID,
  #   Nauplius,
  #   ObjectiveMagnification,
  #   MarksInOcularMicrometer_No.,
  #   MarksInOcularMicrometer_Width_No.,
  #   MarksInOcularMicrometer_Height_No.
  # ) %>% 
  
  rename(
    join.location.1 = site_no,
    sample.start.date.full = collect_date,
    taxa.name = TaxaID,
    life.stage = Nauplius,
    mag = ObjectiveMagnification,
    length.marks = MarksInOcularMicrometer_No.,
    width.marks = MarksInOcularMicrometer_Width_No.,
    height.marks = MarksInOcularMicrometer_Height_No.
  ) %>% 
  
  # add full names for ones that have a .
  left_join(
    wander_names, by = "taxa.name"
  ) %>% 
  
  mutate(
    original.taxa.name = if_else(
      is.na(original.taxa.name),
      taxa.name,
      original.taxa.name
    )
  ) %>% 
  
  # remove ones with no data
  filter(
    !(stri_detect_regex(original.taxa.name, "\\."))
  ) %>% 
  
  mutate(
    # edit location codes
    join.location.1 = stri_extract_last_regex(join.location.1, "\\w+(?=_)"),
    join.location.1 = if_else(
      join.location.1 == "BVR_50",
      "BVR",
      join.location.1
    ),
    
    # date
    sample.start.date.full = stri_replace_all_regex(sample.start.date.full, "-", "."),
    
    `body length` = case_when(
      mag == "20x" ~ length.marks*0.4,
      mag == "25x" ~ length.marks*0.32,
      mag == "30x" ~ length.marks*0.27,
      mag == "35x" ~ length.marks*0.22,
      mag == "40x" ~ length.marks*0.2,
      mag == "50x" ~ length.marks*0.16,
      mag == "60x" ~ length.marks*0.13,
      mag == "70x" ~ length.marks*0.11,
      mag == "75x" ~ length.marks*0.11,
      
      TRUE ~ NA
    ),
    
    `body width` = case_when(
      mag == "20x" ~ width.marks*0.4,
      mag == "25x" ~ width.marks*0.32,
      mag == "30x" ~ width.marks*0.27,
      mag == "35x" ~ width.marks*0.22,
      mag == "40x" ~ width.marks*0.2,
      mag == "50x" ~ width.marks*0.16,
      mag == "60x" ~ width.marks*0.13,
      mag == "70x" ~ width.marks*0.11,
      mag == "75x" ~ width.marks*0.11,
      
      TRUE ~ NA
    ),
    
    `body height` = case_when(
      mag == "20x" ~ height.marks*0.4,
      mag == "25x" ~ height.marks*0.32,
      mag == "30x" ~ height.marks*0.27,
      mag == "35x" ~ height.marks*0.22,
      mag == "40x" ~ height.marks*0.2,
      mag == "50x" ~ height.marks*0.16,
      mag == "60x" ~ height.marks*0.13,
      mag == "70x" ~ height.marks*0.11,
      mag == "75x" ~ height.marks*0.11,
      
      TRUE ~ NA
    ),
    
    # add in extra info
    source.code = 1312,
    original.source.code.1 = "1312",
    experiemental.design = "in-situ",
    individual.uid = paste0("10.1093/plankt/fbae017-", row_number()),
    nu = "individual",
    units = "mm",
    measurement.type = "average",
    sample.size = "100",
    reps = "3",
    ind.per.nu = 1,
  ) %>% 
  
  pivot_longer(
    cols = c(`body length`, `body width`, `body height`),
    names_to = "bodysize.measurement",
    values_to = "body.size"
  ) %>% 
  
  mutate(
    body.size = as.character(body.size)
  ) %>% 
  
  # remove any without a body size
  filter(
    !is.na(body.size)
  ) %>% 
  
  # Reorder
  select(source.code, original.source.code.1, 
         sample.start.date.full,
         join.location.1,
         individual.uid, original.taxa.name, life.stage, nu, ind.per.nu,
         body.size,
         bodysize.measurement, units, measurement.type, sample.size, reps
  )
















