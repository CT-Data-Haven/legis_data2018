library(tidyverse)
library(rvest)

house_url <- "https://www.cga.ct.gov/asp/menu/hlist.asp"
senate_url <- "https://www.cga.ct.gov/asp/menu/slist.asp"

read_legis_table <- function(url) {
  base_url <- "https://www.cga.ct.gov"
  elem <- read_html(url) %>%
    html_node("table.footable")
  col_names <- elem %>% html_node("thead") %>% html_nodes("th") %>% html_text(trim = TRUE) %>% tolower()
  bill_col <- which(col_names == "bills")
  
  rows <- elem %>%
    html_node("tbody") %>%
    html_nodes("tr") %>%
    map(html_nodes, "td")
  
  bill_urls <- rows %>%
    map(bill_col) %>%
    map(html_node, "a") %>%
    map_chr(html_attr, "href")
  
  out <- rows %>%
    map(html_text, trim = TRUE) %>%
    map(set_names, col_names) %>%
    map(enframe) %>%
    map_dfr(pivot_wider) %>%
    select(-bills) %>%
    mutate(bill_url = paste0(base_url, bill_urls),
           district = str_extract(district, "\\d+$") %>% 
             str_pad(width = 3, side = "left", pad = "0") %>%
             paste0("09", .))
  out
}

read_legis_table(house_url)[["bill_url"]] %>%
  sample(10) %>%
  map_lgl(httr::http_error) %>%
  which()

read_legis_table(senate_url)[["bill_url"]] %>%
  sample(10) %>%
  map_lgl(httr::http_error) %>%
  which()

legis_meta <- list(house = house_url, senate = senate_url) %>%
  map_dfr(read_legis_table, .id = "chamber") %>%
  mutate(name = str_replace(name, "(^.+),\\s(.+$)", "\\2 \\1"),
         id = paste0(toupper(substr(chamber, 1, 1)), substr(district, 3, 5))) %>%
  select(chamber, id, name, party, website, bill_url) %>%
  split(.$chamber) %>%
  map(select, -chamber)

jsonlite::write_json(legis_meta, "to_viz/legislators_meta.json")