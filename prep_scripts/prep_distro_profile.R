library(tidyverse)

acs_year <- 2018

leg2town <- read_csv("_utils/legislative_to_town_lookup.csv") %>%
  arrange(chamber, id, town) %>%
  group_by(chamber, id) %>%
  summarise(towns = paste(town, collapse = "; ")) %>%
  ungroup()

reps <- jsonlite::read_json("to_viz/legislators_meta.json", simplifyVector = TRUE) %>%
  bind_rows() %>%
  select(id, representative2020 = name, party2020 = party)

hdrs <- read_csv("_utils/indicator_headings.txt")
cws <- readRDS("output_data/cws_legislative_data_2018.rds") %>%
  mutate(level = ifelse(district == "Connecticut", "state", "district") %>% as_factor(),
         chamber = ifelse(district == "Connecticut", NA, chamber)) %>%
  mutate_at(vars(topic, question), as_factor) %>%
  mutate_if(is.factor, fct_drop) %>%
  mutate(topic = fct_relabel(topic, camiller::clean_titles),
         question = question %>% 
           fct_relabel(camiller::clean_titles) %>%
           fct_recode("Very good self-rated health" = "Very good self rate health", "Dental visit in past year" = "Dental visit past yr", "Local parks in good condition" = "Good cond of parks", "Feel local govt is responsive" = "Local govt responsive", "Think residents are able to obtain jobs" = "Suitable employment", "Good place to raise kids" = "Good to raise kids", "Adults with diabetes" = "Diabetes")) %>%
  select(level, chamber, name = district, indicator = question, value) %>%
  filter(!str_detect(name, "NA"))


prof <- readRDS("output_data/acs_legislative_profile_2018.rds") %>%
  mutate(chamber = na_if(chamber, "state")) %>%
  pivot_longer(estimate:share, names_to = "type", values_drop_na = TRUE) %>%
  unite(group, type, group, sep = " ") %>%
  left_join(hdrs, by = c("topic", "group" = "indicator")) %>%
  select(level, chamber, name, indicator = display, value)

# all_df <- bind_rows(prof, cws) %>%
#   distinct(name, indicator, .keep_all = TRUE) 
house_df <- prof %>%
  filter(chamber != "senate" | is.na(chamber)) %>%
  distinct(name, indicator, .keep_all = TRUE)

senate_df <- bind_rows(prof, cws) %>%
  filter(chamber != "house" | is.na(chamber)) %>%
  distinct(name, indicator, .keep_all = TRUE)

lst(house_df, senate_df) %>%
  iwalk(function(df, chmbr) {
    df %>%
      pivot_wider(names_from = indicator) %>%
      left_join(leg2town, by = c("name" = "id", "chamber")) %>%
      left_join(reps, by = c("name" = "id")) %>%
      select(level:name, towns, representative2020, party2020, everything()) %>%
      write_csv(str_glue("to_distro/{chmbr}_basic_profile_{acs_year}.csv"), na = "")
  })


