library(tidyverse)
library(unpaywallR)
library(here)
library(vroom)
library(readxl)

cali_dois <- vroom(here("data", "processed", "cali_dois.csv"))

dois <- cali_dois$doi |>  unique()

email <- Sys.getenv("EMAIL")


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

oa_unpaywall <- oa_results |> 
  mutate(across(everything(), ~na_if(., "")))

oa_unpaywall |> 
  write_excel_csv2(here("data", "processed", "cali_data_oa.csv"))


manual_oa_data <- read_xlsx(here("data", "processed", "oa_manual.xlsx"))
