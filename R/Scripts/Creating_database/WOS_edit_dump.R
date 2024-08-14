## not their data edits

# data
four <- read.csv("not_their_data/4.csv")
seventeen <- read.csv("not_their_data/17.csv")
two51 <- read.csv("not_their_data/251.csv")

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
  distinct(Source)













