library(tidyverse)
library(cwi)
library(camiller)

acs_year <- 2018

##################################

fetch <- readRDS(file.path("output_data", str_glue("fetch_legislative_acs_{acs_year}.rds"))) %>%
  map(mutate, name = ifelse(level == "district", paste0(toupper(substr(chamber, 1, 1)), substr(geoid, 3, 5)), name)) %>%
  map(select, level, chamber, name, variable, label, estimate) %>%
  map(group_by, level, chamber, name) %>%
  map(filter, !str_detect(name, "ZZZ"))

out <- list()

# AGE
# population under 18, 18+, 65+
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_pop = 1:23, ages0_17 = 1:4, ages18plus = 5:23, ages65plus = 18:23)) %>%
  calc_shares(digits = 2)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label) %>%
  calc_shares(digits = 2, group = label) %>%
  rename(group = label)


# FOREIGN-BORN
out$immigration <- fetch$foreign_born %>%
  separate(label, into = c("total", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_pop = 1:5, foreign_born = 4:5)) %>%
  calc_shares(digits = 2)


# TENURE
# owner-occupied households
tenure <- fetch$tenure %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label) %>%
  calc_shares(digits = 2, group = label, denom = "total_households") %>%
  rename(group = label)


# HOUSING COST
# cost-burdened, not by tenure
housing_cost <- fetch$housing_cost %>%
  separate(label, into = c("total", "tenure", "income", "group"), sep = "!!") %>%
  filter(!is.na(group)) %>%
  add_grps(list(total_households = 1:3, cost_burden = 3)) %>%
  calc_shares(digits = 2, denom = "total_households") %>%
  filter(group != "total_households")

out$housing <- bind_rows(tenure, housing_cost)


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$income <- fetch$poverty %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label) %>%
  calc_shares(digits = 2, group = label, denom = "poverty_status_determined") %>%
  rename(group = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
pov_age <- fetch$pov_age %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  filter(!is.na(ratio)) %>%
  mutate_at(vars(age, ratio), as_factor) %>%
  group_by(ratio, add = TRUE) %>%
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age) %>%
  group_by(level, chamber, name, age) %>%
  add_grps(list(poverty_status_determined = 1:12, poverty = 1:3, low_income = 1:8), group = ratio) %>%
  calc_shares(digits = 2, group = ratio, denom = "poverty_status_determined") %>%
  ungroup() %>%
  unite(group, age, ratio)
out$income_children <- pov_age %>% filter(str_detect(group, "^ages0_17"))
out$income_seniors <- pov_age %>% filter(str_detect(group, "^ages65plus"))


# BIND EVERYTHING

headings <- read_csv("./_utils/indicator_headings.txt") %>%
  mutate(topic = as_factor(topic))


out_df <- suppressWarnings(bind_rows(out, .id = "topic")) %>%
  ungroup() %>%
  mutate_at(vars(topic, chamber, group), as_factor)

out_wide <- out_df %>%
  pivot_longer(estimate:share, names_to = "type") %>%
  unite(indicator, type, group, sep = " ") %>%
  filter(!is.na(value)) %>%
  left_join(headings, by = c("topic", "indicator")) %>%
  distinct(level, chamber, name, indicator, .keep_all = TRUE) %>%
  select(-topic, -indicator) %>%
  pivot_wider(names_from = display)

########## write profile once I get CWS data
write_csv(out_wide, file.path("output_data", str_glue("acs_legislative_profile_{acs_year}_wide.csv")))
saveRDS(out_df, file.path("output_data", str_glue("acs_legislative_profile_{acs_year}.rds")))