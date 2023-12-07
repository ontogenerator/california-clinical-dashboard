library(dplyr)
library(readr)
library(fs)
library(here)
# remotes::install_github("maia-sh/ctregistries")
library(ctregistries)
# renv::install("maia-sh/tidypubmed")
# remotes::install_github("maia-sh/tidypubmed")
library(tidypubmed)
library(vroom)

source(here("prep", "functions", "extract_pubmed.R"))
source(here("prep", "functions", "get_grobid_ft_trn.R"))

cali <- read_xlsx(here("data", "California-trials_2014-2017_main.xlsx"))
cali_dois <- vroom(here("data", "processed", "cali_dois.csv"), delim = ";") |> 
  rename(nct_id = id)

pubmed_ft_retrieved <- read_rds(here("data", "processed", "pubmed", "pubmed-ft-retrieved.rds"))

# Prepare directory paths
dir_pubmed <- dir_create(here("data", "processed", "pubmed"))
dir_trn <- dir_create(here("data", "processed", "trn"))

# Get all unique pmid/doi combinations (for consistent pub ids as pmids across trns)
pmids_dois <-
  cali |> 
  left_join(cali_dois) |> 
  distinct(pmid, doi)  |> 
  tidyr::drop_na(pmid, doi)

# Extract TRNs from: PubMed secondary identifier, PubMed abstract, and PDF full-text
pubmed_xmls <- dir_ls(here("data", "raw", "pubmed"))


# Secondary identifier ----------------------------------------------------

si <-
  pubmed_xmls |>
  purrr::map_dfr(extract_pubmed, datatype = "databanks", quiet = FALSE) |>
  ctregistries::mutate_trn_registry(accession_number)

write_rds(si, path(dir_pubmed, "pubmed-si.rds"))
# si <- read_rds(path(dir_pubmed, "pubmed-si.rds"))

# Visually inspect mismatching trns and registries
# 1 accession numbers are figshare

si_trn_mismatches <-
  si |>
  filter(!accession_number %in% trn |
           !databank %in% registry)
# write_rds(si_trn_mismatches, path_wd("data", "processed", "si-trn-mismatches", ext = "rds"))

si <-
  si |>
  tidyr::drop_na(trn) |>
  select(pmid, registry, trn_detected = trn) |>
  distinct() |>
  group_by(pmid) |>
  mutate(n_detected = row_number()) |>
  ungroup() |>
  mutate(
    pmid = as.character(pmid),
    source = "secondary_id",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) |>
  left_join(pmids_dois, by  = "pmid")

write_rds(si, path(dir_trn, "trn-si.rds"))
# si <- read_rds(path(dir_trn, "trn-si.rds"))

# Abstract ----------------------------------------------------------------

abs <-
  pubmed_xmls |>
  purrr::map_dfr(extract_pubmed, datatype = "abstract", quiet = FALSE) |>
  ctregistries::mutate_trn_registry(abstract)

write_rds(abs, path(dir_pubmed, "pubmed-abstract.rds"))
# abs <- read_rds(path(dir_pubmed, "pubmed-abstract.rds"))



abs <-
  abs |>
  tidyr::drop_na(trn) |>
  distinct(pmid, registry, trn_detected = trn) |>
  group_by(pmid) |>
  mutate(n_detected = row_number()) |>
  ungroup() |>
  mutate(
    pmid = as.character(pmid),
    source = "abstract",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) |>
  left_join(pmids_dois, by  = "pmid")
# 
# manual_abs <- tribble(pmid = "",
#                       registry = "ClinicalTrials.gov",
#                       trn_detected = "NCT02320812",
#                       n_detected = 1,
#                       source = "abstract",
#                       trn_cleaned = "NCT02320812",
#                       doi = NA)

write_rds(abs, path(dir_trn, "trn-abstract.rds"))
# abs <- read_rds(path(dir_trn, "trn-abstract.rds"))

# Full-text DOI -----------------------------------------------------------

ft_doi_xmls <-
  dir_ls(here("data", "raw", "fulltext", "doi", "xml"))

ft_doi <-
  ft_doi_xmls |>
  purrr::map_dfr(get_grobid_ft_trn) |>
  select(-pmid) |>
  rename(trn_detected = trn, n_detected = n) |>
  mutate(
    doi = tolower(doi),
    source = "ft",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) |>
  left_join(pmids_dois, by = "doi")


# manual_nondoi <- tibble(doi = "hilarispublisher.com+fg3019+26153",
#                       n_detected = 1,
#                       trn_detected = "NCT01181245",
#                       registry = "ClinicalTrials.gov",
#                       source = "ft",
#                       trn_cleaned = "NCT01181245",
#                       pmid = "https://www.hilarispublisher.com/abstract/fg3019-a-human-monoclonal-antibody-to-connective-tissue-growth-factor-combined-with-chemotherapy-in-patients-with-locall-26153.html")
# 
# ft_doi2 <- ft_doi |> 
#   rows_append(manual_nondoi)


write_rds(ft_doi, path(dir_trn, "trn-ft-doi.rds"))
# ft_doi <- read_rds(path(dir_trn, "trn-ft-doi.rds"))

# Full-text pmid----------------------------------------------------------



# ft_nondoi_xmls <-
#   dir_ls(here("data", "raw", "fulltext", "nondoi", "xml"))
# 
# ft_nondoi <- ft_nondoi_xmls |>
#   purrr::map_dfr(get_grobid_ft_trn) |>
#   select(-doi) |>
#   rename(trn_detected = trn, n_detected = n) |>
#   mutate(
#     source = "ft",
#     trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
#   ) |>
#   left_join(pmids_dois, by  = "pmid")

# ft_pmid_xmls <-
#   dir_ls(here("data", "raw", "fulltext", "pmid", "xml"))
# no files there, so no need for further code
# ft_pmid <-
#   ft_pmid_xmls |>
#   purrr::map_dfr(get_grobid_ft_trn) |>
#   select(-doi) |>
#   rename(trn_detected = trn, n_detected = n) |>
#   mutate(
#     source = "ft",
#     trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
#   ) |>
#   left_join(pmids_dois, by  = "pmid")
# 
# write_rds(ft_pmid, path(dir_trn, "trn-ft-pmid.rds"))
# ft_pmid <- read_rds(path(dir_trn, "trn-ft-pmid.rds"))

# Note: Could add whether typo and dupe after cleaning
# mutate(has_trn_typo = if_else(trn_detected != trn_cleaned, TRUE, FALSE)) |>
# add_count(pmid, trn_cleaned, source)


# Combine reported TRNs (secondary id, abstract, full-text) ---------------

trn_combined <-
  bind_rows(si, abs,
            ft_doi
            # ft_pmid
            ) |>
  
  distinct(pmid, doi, trn = trn_cleaned, registry, source) |>
  
  # All records should have a trn
  assertr::assert(assertr::not_na, trn)

write_rds(trn_combined, path(dir_trn, "trn-reported-long.rds"))
# trn_combined <- read_rds(path(dir_trn, "trn-reported-long.rds"))

# Pivot wider to for one row per TRN with sources as columns --------------

trn_combined <-
  trn_combined |>
  
  # Note: `value_fill` is FALSE for all but some will be replaced with NA, e.g., because no full-text
  mutate(value = TRUE) |>
  tidyr::pivot_wider(
    names_from = source, names_prefix = "has_trn_",
    values_from = value, values_fill = FALSE
  )
# dupes <- trn_combined |> get_dupes(doi, pmid)
# Change "has_trn_SOURCE" from FALSE to NA if source *not* retrieved
trn_combined <-
  # Add info on retrieved pubmed and ft
  pubmed_ft_retrieved |>
  select(-id, -ft_source, -ft_doi) |>
  janitor::remove_empty("rows") |>
  distinct() |>
  (\(x) left_join(trn_combined, x, by = c("pmid", "doi")))() %>% 
    # Check that no rows added
  assertr::verify(nrow(.) == nrow(trn_combined)) |>
  
  mutate(
    
    # TRN in secondary id and abstract are NA if no pubmed record
    has_trn_secondary_id = if_else(!has_pubmed, NA, has_trn_secondary_id),
    has_trn_abstract = if_else(!has_pubmed, NA, has_trn_abstract),
    
    # TRN in full-text is NA no full-text
    has_trn_ft = if_else(!has_ft, NA, has_trn_ft)
  ) |>
  
  select(-has_pubmed, -has_ft)

write_rds(trn_combined, path(dir_trn, "trn-reported-wide.rds"))


# Combine all trns (intovalue and reported) -------------------------------

trn_all <-
  bind_rows(
    distinct(cali, trn = nct_id),
    distinct(trn_combined, trn)
  ) |>
  distinct()

write_rds(trn_all, path(dir_trn, "trn-all.rds"))

# from 11_extract-pubmed-metadata.R


source(here("prep", "functions", "extract_pubmed.R"))

dir <- here("data", "processed", "pubmed")

pubmed_xmls <- dir_ls(here("data", "raw", "pubmed"))

pubmed_main <-
  pubmed_xmls |>
  purrr::map_dfr(extract_pubmed, datatype = "main", quiet = FALSE) |>
  rename(pmcid = pmc) |>
  mutate(doi = tolower(doi))

write_rds(pubmed_main, path(dir, "pubmed-main", ext = "rds"))

