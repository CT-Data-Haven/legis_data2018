library(tidyverse)
library(jsonlite)

acs_year <- 2018

make_abbr <- function(x) {
  x %>%
    str_match_all("(?:_|\\b)([a-z])") %>%
    map(~.[,2]) %>%
    map_chr(paste, collapse = "")
}

types <- list(t = "estimate", m = "share")
hdrs <- read_csv("_utils/indicator_headings.txt")

prof <- readRDS(str_glue("output_data/acs_legislative_profile_{acs_year}.rds")) %>%
  rename(location = name)


# lookup of all indicators across all datasets to make codes
srvy_meta <- googlesheets4::sheets_find("surveyq") %>%
  pull(id) %>%
  googlesheets4::read_sheet() %>%
  mutate(display = coalesce(display, camiller::clean_titles(question))) %>%
  mutate(type2 = "m",
         indicator = paste(type2, question, sep = "X"),
         topic = fct_collapse(topic, health_and_wellbeing = c("health_risk_factors", "healthcare_access")))

cws <- readRDS("input_data/cws_indicators_by_senate.rds") %>%
  map_dfr(select, level, district, value, .id = "question") %>%
  left_join(srvy_meta, by = "question") %>%
  mutate(chamber = "senate",
         district = ifelse(level == "district", paste0("S", district), district)) 

cws_data <- cws %>%
  select(topic, level, chamber, location = district, indicator, value)

cws %>%
  select(topic, chamber, district, question, value) %>%
  saveRDS("output_data/cws_legislative_data_2018.rds")


prof_long <- bind_rows(
  (prof %>% filter(level == "state") %>% mutate(chamber = "house")),
  (prof %>% filter(level == "state") %>% mutate(chamber = "senate")),
  (prof %>% filter(level != "state"))
) %>%
  mutate(chamber = as_factor(chamber)) %>%
  pivot_longer(c(estimate, share), names_to = "type") %>%
  filter(!is.na(value)) %>%
  mutate(type2 = fct_recode(type, !!!types))

# add in cws
prof_wide <- prof_long %>%
  select(-type) %>%
  unite(indicator, type2, group, sep = "X") %>%
  bind_rows(cws_data) %>%
  mutate_at(vars(topic, level, chamber), as_factor) %>%
  split(.$chamber) %>%
  map(~split(., .$topic)) %>%
  map_depth(2, pivot_wider, names_from = indicator) %>%
  map_depth(2, select, -topic, -chamber)



# meta
# topics:
#   income:
#     display: Income
#     indicators: [tXpoverty, mXpoverty]
# indicators:
#   tXpoverty:
#     display: Population in poverty
#     format: ','


# meta by chamber

meta <- bind_rows(
  prof_long %>%
    distinct(chamber, topic, group, type, type2) %>%
    unite(indicator, type, group, sep = " ", remove = FALSE) %>%
    left_join(hdrs, by = c("topic", "indicator")) %>%
    select(-group, -type),
  cws %>%
    distinct(chamber, topic, indicator, display, type2) 
) %>%
  mutate(topic = as_factor(topic),
         indicator = str_replace(indicator, "^\\w+\\s", paste0(type2, "X")),
         format = if_else(topic == "life_expectancy", ".1f", recode(as.character(type2), "t" = ",", "m" = ".0%")),
         displayTopic = topic %>%
           fct_relabel(camiller::clean_titles) %>%
           fct_relabel(str_replace, "(?<=^Income) ", " by age: ") %>%
           fct_recode("Race and ethnicity" = "Race")) %>%
  split(.$chamber) %>%
  map(~split(., .$topic, drop = TRUE)) %>%
  map_depth(2, function(tdf) {
    idf <- tdf %>%
      select(indicator, type = type2, display, format)
    
    list(
      display = as.character(unique(tdf$displayTopic)),
      indicators = idf
    )
  })



write_json(prof_wide, str_glue("to_viz/legislative_wide_{acs_year}.json"), auto_unbox = TRUE)
write_json(meta, str_glue("to_viz/legislative_meta_{acs_year}.json"), auto_unbox = TRUE)


