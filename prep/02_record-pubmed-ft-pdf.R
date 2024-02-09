# Create df of retrieved pubmed and pdfs (doi and pmid), with NA if no pmid
# Also add whether source of pdf if doi or pmid (TRUE/FALSE only)
# Use xml versions of pdfs

library(tidyverse)
library(vroom)
library(fs)
library(here)
library(janitor)
library(readxl)

cali <- read_xlsx(here("data", "California-trials_2014-2017_main.xlsx"))
cali_dois <- vroom(here("data", "processed", "cali_dois.csv"), delim = ";") |> 
  rename(nct_id = id)

# Prepare paths
dir_pubmed <- here("data", "raw", "pubmed")
dir_doi_xml <- here("data", "raw", "fulltext", "doi", "xml")
dir_nondoi_xml <- here("data", "raw", "fulltext", "nondoi", "xml")

dir_pubmed_processed <- dir_create(here("data", "processed", "pubmed"))

# List retrieved records
pubmed_retrieved <-
  dir_pubmed |>
  dir_ls() |>
  path_file() |>
  path_ext_remove()

ft_doi_retrieved <-
  dir_doi_xml |>
  dir_ls() |>
  path_file() |>
  str_remove(".tei.xml$") |>
  str_replace_all("\\+", "/") |> 
  tolower()

ft_nondoi_retrieved <- dir_nondoi_xml |>
  dir_ls() |>
  path_file() |>
  str_remove(".tei.xml$") |>
  str_replace_all("\\+", "/") |>
  tolower()

pubmed_ft_retrieved <-
  cali |>
  left_join(cali_dois) |>
  select(id = nct_id, doi, pmid) |>
  mutate(
    has_pubmed = case_when(
      is.na(pmid) ~ NA,
      pmid %in% pubmed_retrieved ~ TRUE,
      .default = FALSE
    ),

    has_ft = case_when(
      str_detect(pmid, "hilaris") ~ TRUE,
      (doi %in% ft_doi_retrieved) | (pmid %in% ft_nondoi_retrieved) ~ TRUE,
      is.na(doi) ~ NA,
      .default = FALSE
    ),

    ft_source = case_when(
      doi %in% ft_doi_retrieved ~ "doi",
      str_detect(pmid, "hilaris") ~ "manual",
      pmid %in% ft_nondoi_retrieved ~ "pmid",
      .default = NA
    ),
    ft_doi = if_else(doi %in% ft_doi_retrieved, TRUE, FALSE)
    # ft_pmid = if_else(pmid %in% ft_pmid_retrieved, TRUE, FALSE),
  ) |>

  # Remove duplicates due to intovalue versions
  distinct()

write_rds(pubmed_ft_retrieved, path(dir_pubmed_processed, "pubmed-ft-retrieved.rds"))

