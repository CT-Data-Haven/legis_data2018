library(tidyverse)

acs_year <- 2018

ids <- read_csv("_utils/legislative_to_town_lookup.csv") %>%
  distinct(chamber, id) %>%
  split(.$chamber) %>%
  map(pull, id)

ids %>%
  imap(function(id_list, chamber) {
    walk(id_list, ~rmarkdown::render(input = "./prep_scripts/param_handout.Rmd",
                                     output_file = str_glue("handouts/{chamber}/{acs_year}_profile_{.}.pdf"),
                                     params = list(geoid = .)))
  })

# ids <- read_csv("../town_x_district.csv") %>%
#   select(-town) %>%
#   unique() %>%
#   mutate(num = stringr::str_sub(id, -3)) %>%
#   mutate(name = paste(chamber, num, sep = "_")) %>%
#   filter(chamber == "Senate")

# walk2(ids$id, ids$name,  ~rmarkdown::render(input = "./legislative_handout_v2.Rmd", output_file = sprintf("./output/2016_profile_%s.pdf", .y), params = list(this_id = .x)))

# walk2(ids$id, ids$name, function(id, name) {
#   rmarkdown::render(input = "./legislative_handout_v2.Rmd", output_file = sprintf("./output/2016_profile_%s.pdf", name), params = list(this_id = id))
#   dev.off()
#   print(id)
# })