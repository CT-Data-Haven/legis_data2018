library(tidyverse)
library(tidycensus)
library(cwi)

acs_year <- 2018

#####################################

house <- basic_table_nums %>%
  map(~get_acs("state legislative district (lower chamber)", table = ., state = "09", year = acs_year)) %>%
  map(mutate, level = "district")
senate <- basic_table_nums %>%
  map(~get_acs("state legislative district (upper chamber)", table = ., state = "09", year = acs_year)) %>%
  map(mutate, level = "district")
state <- basic_table_nums %>%
  map(~get_acs("state", table = ., year = acs_year)) %>%
  map(filter, NAME == "Connecticut") %>%
  map(mutate, level = "state")

BRRR::skrrrahh()

fetch <- transpose(lst(state, house, senate)) %>%
  map(bind_rows, .id = "chamber") %>%
  map(janitor::clean_names) %>%
  map(label_acs, year = acs_year) %>%
  map(mutate, level = as_factor(level))

saveRDS(fetch, file.path("output_data", str_glue("fetch_legislative_acs_{acs_year}.rds")))
