## Data
all_list <- read_xlsx("./Raw_data/wos_bodysize.xlsx", sheet = "complete_list")
extra_list <- read_xlsx("./Raw_data/wos_bodysize.xlsx", sheet = "extra_list")
new_list <- read_xlsx("./Raw_data/wos_bodysize.xlsx", sheet = "new_list")
old_list <- read_xlsx("./Raw_data/wos_bodysize.xlsx", sheet = "old_list")

al <- all_list %>% 
  select(Authors, `Article Title`, DOI)

el <- extra_list %>% 
  select(Authors, `Article Title`, DOI)

nl <- new_list %>% 
  select(Authors, `Article Title`, DOI)

ol <- old_list %>% 
  select(Authors, `Article Title`, DOI)


all_1 <- anti_join(nl, ol, by = "Article Title")

all_2 <- anti_join(all_1, al, by = "Article Title")

nw_unique <- nl %>% 
  distinct(DOI)

nl_distinct <- nl %>%
  group_by(`DOI`) %>%
  mutate(
    distinct = ifelse(n() == 1, "Y", "N")
  ) %>%
  ungroup()
  