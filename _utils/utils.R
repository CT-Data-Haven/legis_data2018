library(tidyverse)
library(sf)
library(ggmap)
library(scales)
library(showtext)
library(camiller)
library(cwi)

# sysfonts::font_add("barlow", regular = "Barlow-Regular.ttf", bold = "Barlow-SemiBold.ttf")
# sysfonts::font_add("barcond", regular = "BarlowSemiCondensed-Regular.ttf", bold = "BarlowSemiCondensed-SemiBold.ttf")
font_add("din", regular = "D-DIN.otf", bold = "D-DIN-bold.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# colors
pastel <- c("gray70", rcartocolor::carto_pal(12, "Pastel"))
div <- rcartocolor::carto_pal(5, "Geyser")
div7 <- rcartocolor::carto_pal(7, "Geyser")
# greens <- c('#f0eeaa', '#c0d889', '#98c082', '#72a680', '#4d8b7e', '#277278', '#015969')

gnbu7 <- RColorBrewer::brewer.pal(7, "GnBu")
gnbu9 <- RColorBrewer::brewer.pal(9, "GnBu")
blue <- gnbu7[6]
bupu7 <- RColorBrewer::brewer.pal(7, "BuPu")


# ggplot
ggplot2::update_geom_defaults("text", list(family = "din", fontface = "bold", size = 3))

theme_din2 <- function(base_family = "din", base_size = 11, ...) {
  camiller::theme_din(base_family = base_family, base_size = base_size, ...) + 
    ggplot2::theme(legend.text = ggplot2::element_text(size = ggplot2::rel(0.75)), 
                   legend.key.size = ggplot2::unit(1.1, "lines"), 
                   legend.title = ggplot2::element_text(size = ggplot2::rel(0.9)),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(0.9), family = "din"),
                   plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8), family = "din"),
                   strip.background = ggplot2::element_blank())
}

theme_title <- function(text, ...) {
  themed_label(text,
                 element = "plot.title", x = 0.02, hjust = 0, vjust = 1, theme = ggplot2::theme_get(), ...) +
    theme(plot.title = element_text(hjust = 0, margin = margin(0, 0, 4, 0, "pt")),
          plot.margin = margin(0, 0, 0, 0, "pt"))
}

theme_subtitle <- function(text, ...) {
  themed_label(text,
                 element = "plot.subtitle", x = 0.02, hjust = 0, vjust = 0, theme = ggplot2::theme_get(), ...) +
    theme(plot.subtitle = element_text(hjust = 0, margin = margin(0, 0, 1.5, 0, "lines")),
          plot.margin = margin(0, 0, 0, 0, "lines"))
}

scale_y_barcontinuous <- function(...) {
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05)), ...)
}

logo <- cowplot::ggdraw() + 
  cowplot::draw_image(here::here("_utils", "DataHaven_Black.png"), x = 1, hjust = 1, width = 0.25) +
  theme(plot.margin = margin(0.05, 0.05, 0.05, 0.05, "in"))

outline <- function(col, amount = 0.2) {
  colorspace::darken(col, amount)
}




# senate_sf <- tigris::state_legislative_districts("CT", "upper", cb = TRUE, class = "sf") %>%
#   select(id = SLDUST, fips = AFFGEOID, geometry)
# house_sf <- tigris::state_legislative_districts("CT", "lower", cb = TRUE, class = "sf") %>%
#   select(id = SLDLST, fips = AFFGEOID, geometry)
# dists_sf <- lst(senate_sf, house_sf) %>% set_names(str_remove, "_sf")
# state_sf <- tigris::states(cb = TRUE, class = "sf", year = 2017) %>%
#   select(state = STUSPS, geometry) %>%
#   filter(state == "CT")
# ct_bbox <- state_sf %>%
#   st_buffer(5e-2) %>%
#   st_bbox() %>%
#   setNames(c("left", "bottom", "right", "top"))
# ct_basemap <- get_stamenmap(ct_bbox, zoom = 10, maptype = "toner-lite")
# 
# saveRDS(dists_sf, "_utils/districts_sf.rds")
# saveRDS(state_sf, "_utils/state_sf.rds")
# saveRDS(ct_basemap, "_utils/ct_basemap.rds")
util_path <- here::here("_utils")
dists_sf <- readRDS(file.path(util_path, "districts_sf.rds"))
state_sf <- readRDS(file.path(util_path, "state_sf.rds"))
ct_basemap <- readRDS(file.path(util_path, "ct_basemap.rds"))



id_lookup <- dists_sf %>%
  map_dfr(st_drop_geometry, .id = "chamber") %>%
  as_tibble() %>%
  mutate(id = substr(chamber, 1, 1) %>% toupper() %>% paste0(id))




clean_ages <- function(x) {
  x <- str_replace(x, "(?<=[A-Za-z])(\\B)(?=\\d)", " ")
  x <- str_replace(x, "plus", "+")
  x <- str_replace_all(x, "(?<=\\d)(_)(?=\\d)", "–")
  x <- str_replace_all(x, "_", " ")
  x <- str_to_sentence(x)
  x
}


round_sum100 <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}