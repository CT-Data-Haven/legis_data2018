library(tidyverse)
library(sf)

house_sf <- tigris::state_legislative_districts("09", "lower", cb = TRUE, class = "sf") %>%
  janitor::clean_names() %>%
  select(geoid, id = sldlst, geometry)
senate_sf <- tigris::state_legislative_districts("09", "upper", cb = TRUE, class = "sf") %>%
  janitor::clean_names() %>%
  select(geoid, id = sldust, geometry)
town_sf <- tigris::county_subdivisions("09", cb = TRUE, class = "sf") %>%
  janitor::clean_names() %>%
  select(town = name, geometry)

# keep if at least 5% of district is in a town
thresh <- 0.05
house2town <- house_sf %>%
  st_intersection(town_sf) %>%
  mutate(cover_area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(id, town, cover_area) %>%
  left_join(
    house_sf %>%
      mutate(total_area = st_area(geometry)) %>%
      st_drop_geometry() %>%
      select(id, total_area),
    by = "id"
  ) %>%
  mutate(share_covered = as.numeric(cover_area / total_area)) %>%
  filter(share_covered > thresh)

senate2town <- senate_sf %>%
  st_intersection(town_sf) %>%
  mutate(cover_area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(id, town, cover_area) %>%
  left_join(
    senate_sf %>%
      mutate(total_area = st_area(geometry)) %>%
      st_drop_geometry() %>%
      select(id, total_area),
    by = "id"
  ) %>%
  mutate(share_covered = as.numeric(cover_area / total_area)) %>%
  filter(share_covered > thresh)

leg2town <- lst(house2town, senate2town) %>%
  map_dfr(as_tibble, .id = "chamber") %>%
  mutate(chamber = str_extract(chamber, "^[a-z]+"),
         id = paste0(toupper(substr(chamber, 1, 1)), id)) %>%
  arrange(chamber, id, town)

write_csv(leg2town, "_utils/legislative_to_town_lookup.csv")

leg_json <- leg2town %>%
  select(chamber, id, town) %>%
  split(.$chamber) %>%
  map(~split(., .$id)) %>%
  map_depth(2, pull, town)

jsonlite::write_json(leg_json, "to_viz/town_lookup.json", auto_unbox = F)



# topojson
shps <- lst(house_sf, senate_sf) %>%
  imap(function(df, chamber) {
    i <- toupper(substr(chamber, 1, 1))
    df %>%
      mutate(id = paste0(i, id)) %>%
      select(-geoid)
  }) %>%
  map(st_transform, 4326) %>%
  map(rmapshaper::ms_simplify, keep = 0.2) %>%
  set_names(str_remove, "_sf")

shps %>%
  iwalk(~geojsonio::topojson_write(.x, geometry = "polygon", file = str_glue("to_viz/shapes/{.y}_topo.json"), object_name = "shape"))

town_sf %>%
  st_transform(4326) %>%
  rmapshaper::ms_simplify(keep = 0.8) %>%
  geojsonio::topojson_write(geometry = "polygon", object_name = "shape", file = "to_viz/shapes/towns_topo.json")
