library(tidyverse)
library(unpaywallR)
library(here)
library(vroom)

cali_dois <- vroom(here("prep", "cali_dois.csv"))

dois <- unique(cali_dois$doi)

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
  ) |>
  rename(color = OA_color, publication_date_unpaywall = date)

# 
# unpaywall_results <- unpaywallR::dois_OA_colors(publications$doi, email, clusters = 1)
# 
# unpaywall_results <- unpaywall_results %>% 
#   rename(publication_date_unpaywall = date)

# unpaywall_results %>% write.csv2(here("prep", "cali-data-oa.csv"), row.names = FALSE)


oa_unpaywall <- oa_results |> 
  mutate(across(everything(), ~na_if(., "")))

oa_unpaywall |> 
  write_excel_csv2(here("data", "processed", "cali_data_oa.csv"))
