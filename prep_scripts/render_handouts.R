library(tidyverse)

acs_year <- 2018

ids <- read_csv("_utils/legislative_to_town_lookup.csv") %>%
  distinct(chamber, id) %>%
  split(.$chamber) %>%
  map(pull, id)

ids %>%
  imap(function(id_list, chamber) {
    walk(id_list, ~rmarkdown::render(input = "./prep_scripts/param_handout.Rmd",
                                     output_dir = file.path("handouts", chamber),
                                     output_file = str_glue("{acs_year}_profile_{.}.pdf"),
                                     params = list(geoid = .)))
  })

BRRR::skrrrahh(18)
