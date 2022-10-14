library(tidyverse)
library(unpaywallR)
library(here)
library(vroom)

cali_dois <- vroom(here("prep", "cali_dois.csv"))

dois <- cali_dois$doi %>% unique()

email <- "vladislav.nachev@charite.de"


# Query Unpaywall
oa_results_raw <-
  unpaywallR::dois_OA_colors_fetch(
   dois,
    email = email,
    clusters = 2)

# Pick OA color based on hierarchy journal > repository (except bronze)  --------

hierarchy <-
  c("gold",
    "hybrid",
    "green",
    "bronze",
    "closed")

oa_results <-
  unpaywallR::dois_OA_pick_color(
    oa_results_raw,
    hierarchy
  ) %>%
  rename(color = OA_color, publication_date_unpaywall = date)

# 
# unpaywall_results <- unpaywallR::dois_OA_colors(publications$doi, email, clusters = 1)
# 
# unpaywall_results <- unpaywall_results %>% 
#   rename(publication_date_unpaywall = date)

# unpaywall_results %>% write.csv2(here("prep", "cali-data-oa.csv"), row.names = FALSE)


green_oa_hierarchy <-
  c("gold",
    "hybrid",
    "bronze",
    "green",
    "closed")

oa_results_green <-
  unpaywallR::dois_OA_pick_color(
    oa_results_raw,
    green_oa_hierarchy
  ) %>%
  select(doi, color_green_only = OA_color)

has_duplicates(oa_results_green, doi)

oa_unpaywall <- oa_results_green %>% 
  left_join(oa_results, by = "doi") %>%
  mutate(across(everything(), ~na_if(., "")))

oa_unpaywall %>% write.csv2(here("prep", "cali-data-oa.csv"), row.names = FALSE)
