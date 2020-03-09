library(tidyverse)
library(data.world)

dataset <- "camille86/legislativedata"

paths <- list.files("to_distro", full.names = TRUE)
dfs <- paths %>%
  set_names(str_extract, "\\w+(?=_df)") %>%
  map(read_csv)
descrs <- names(dfs) %>%
  set_names() %>%
  map(~str_glue("2018 ACS basic indicators by {.} district"))

descrs$senate <- str_glue("{descrs$senate}, plus CWS indicators")

url_base <- "https://github.com/CT-Data-Haven/legis_data2018/blob/master/to_distro"

walk2(descrs, paths, function(dscr, pth) {
  filename <- basename(pth)
  url <- file.path(url_base, filename)
  req <- file_create_or_update_request(file_name = filename,
                                description = dscr,
                                labels = list("clean data"),
                                url = url)
  update_dataset(dataset, dataset_update_request(files = list(req)))
})

# add license: cc sharealike
update_dataset(dataset, dataset_update_request(license_string = "CC-BY-SA"))

dwapi::sync(dataset)

names(dfs) %>%
  enframe(value = "file") %>%
  select(-name) %>%
  mutate(slug = c("mvhosefwoaopnufxkjcxss5pwxuyol", "jadxqcj3fwfb2bjikiai67zwo3jsqd")) %>%
  write_csv("_utils/dataworld_urls.csv")
