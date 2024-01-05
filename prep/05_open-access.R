library(tidyverse)
library(unpaywallR)
library(here)
library(vroom)
library(readxl)

cali_dois <- vroom(here("data", "processed", "cali_dois.csv"))

dois <- cali_dois$doi |> unique()
dois <- "10.1200/jco.2014.32.3_suppl.115"
email <- Sys.getenv("EMAIL")

dois <- c("10.1177/237946152100700104",
          "10.1093/ofid/ofy210.1973",
          "10.1016/j.cardfail.2015.06.309",
          "10.1016/j.jacc.2018.08.1381",
          "10.1200/jco.2016.34.15_suppl.e14110",
          "10.1097/dbp.0000000000000334",
          "10.1200/jco.2018.36.15_suppl.5554",
          "10.1200/jco.2014.32.3_suppl.115",
          "10.14283/jpad.2017.36")

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
