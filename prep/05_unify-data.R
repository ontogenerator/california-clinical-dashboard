library(here)
library(vroom)
library(lubridate)
library(rcrossref)
library(tidyverse)
library(easyRPubMed)
library(janitor)
library(readxl)

cali_trials_original <- vroom(here("data", "California-trials_2014-2017.csv"))
cali_trials <- read_xlsx(here("data", "California-trials_2014-2017_main.xlsx"))

incomplete_dates <- cali_trials_original |> 
  filter(str_detect(publication_date, " ")) |> 
  pull(nct_id)

cali_trials <- cali_trials |> 
  mutate(publication_type = case_when(
           any_paper == 1 ~ "journal publication",
          any_paper == 2 ~ "abstract",
           TRUE ~ "unknown"
         ),
         # publication_date = lubridate::dmy(publication_date),
         # last_update_submitted_date = lubridate::dmy(last_update_submitted_date),
         # registration_date = lubridate::dmy(registration_date),
         # summary_results_date = lubridate::dmy(summary_results_date),
         primary_completion_year = ifelse(is.na(primary_completion_year), completion_year, primary_completion_year),
         # is_prospective = lubridate::floor_date(registration_date, "month") <= 
         #   lubridate::floor_date(start_date, "month"),
         # has_pubmed = str_length(pmid) <  10 & !is.na(pmid),
         has_reg_pub_link = ifelse(publication_found == 1, TRUE, FALSE)) |> 
  select(-id, -id_2) |> 
  rename(id = nct_id)

cali_trials |> 
  filter(is.na(publication_date))

cali_trials |> count(publication_found, publication_type)
cali_trials |> count(publication_found)
cali_trials |>  count(publication_found)
get_dupes(cali_trials, id)

# with_progress({
#    cali_dois <- cali_trials |> 
#        filter(!is.na(pmid),
#               str_length(pmid) < 10) |>
#      select(pmid) |> 
#      get_metadata(pmid, chunksize = 100, api_key = NULL)
# })
# 
# 
# cali_dois_from_pmid <- cali_dois |> 
#   list_rbind() |>
#   mutate(pmid = as.character(pmid)) |> 
#   select(pmid, doi)

# cali_trials_doi <- cali_trials |> 
#   left_join(cali_dois_from_pmid, by = "pmid") |>
#   select(id, pmid, doi, everything()) 
# 
# cali_trials_doi <- cali_trials_doi |> 
#   mutate(doi = case_when(
#     str_detect(pmid, "10\\.") ~ str_extract(pmid, "10\\..*"),
#     .default = doi))
# 
# cali_trials_doi |> 
#   count(is.na(doi))
# 
# cali_dois <- cali_trials_doi |> 
#   drop_na(doi) |>
#   filter(doi != "") |> 
#   select(id, doi)
# 
# cali_dois |> 
#   extract_duplicates(doi)

# cali_dois |>
   # mutate(is_doi = str_detect(doi, "10.")) |>
   # count(is_doi)
# cali_dois <- cali_dois |> mutate(doi = tolower(doi)) 
# cali_dois |> write.csv2(here("data", "processed", "cali_dois.csv"), row.names = FALSE)

cali_dois <- vroom(here("data", "processed", "cali_dois.csv"))

## Remove non-extracted rows
cali_trials <- cali_trials |>
  left_join(cali_dois) |> # add doi from pubmed
    filter(!is.na(any_paper)) |> 
  select(id, pmid, doi, everything())


## Read OA data from Delwen
oa <- vroom(here("data", "processed", "cali_data_oa.csv")) |> 
  select(-color_green_only)
oa_manual <- read_xlsx(here("data", "processed", "oa_manual.xlsx")) |> 
  mutate(doi = tolower(doi))
## Combine extracted data and OA data
cali_trials <- cali_trials |>
  left_join(oa, by = "doi") |> 
  rows_upsert(oa_manual, by = "id")

## Determine the amount of follow-up
search_date <- as.Date("2022-10-01")

cali_trials$has_followup_2y <-
    as.Date(cali_trials$primary_completion_date) + 365*2 < search_date
cali_trials$has_followup_2y_pub <- cali_trials$has_followup_2y
cali_trials$has_followup_2y_sumres <- cali_trials$has_followup_2y

cali_trials$has_followup_5y <-
    as.Date(cali_trials$primary_completion_date) + 365*5 < search_date
cali_trials$has_followup_5y_pub <- cali_trials$has_followup_5y
cali_trials$has_followup_5y_sumres <- cali_trials$has_followup_5y

## Determine whether there are summary results within 2, 5 years
#which dates are floored????
  
cali_trials$is_summary_results_2y <-
  as.Date(cali_trials$summary_results_date) <=
  as.Date(cali_trials$primary_completion_date) + 365*2

cali_trials$is_summary_results_5y <-
    as.Date(cali_trials$summary_results_date) <=
    as.Date(cali_trials$primary_completion_date) + 365*5

cali_trials$is_publication_2y <-
    as.Date(cali_trials$publication_date) <=
    as.Date(cali_trials$primary_completion_date) + 365*2

cali_trials$is_publication_5y <-
    as.Date(cali_trials$publication_date) <=
    as.Date(cali_trials$primary_completion_date) + 365*5

cali_trials <- cali_trials |> 
  mutate(pub_date_incomplete = id %in% incomplete_dates)
# 
cali_trials <- cali_trials |>
  mutate(has_publication = ifelse(any_paper > 0, TRUE, FALSE)) 
# |> 
#   filter(start_year > 2004)

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")

# Add pubmed and full-text ------------------------------------------------

# Prepare pubmed variables, including citation: "author et al. (year) title"
pubmed_main <- read_rds(here(dir_processed, "pubmed", "pubmed-main.rds"))
# registry_studies <- read_rds(path(dir_processed, "registries", "registry-studies.rds"))
# registry_references <- read_rds(path(dir_processed, "registries", "registry-references.rds"))
# cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))
# n_cross_registrations <- read_rds(path(dir_processed, "trn", "n-cross-registrations.rds"))
trn_reported_wide <- read_rds(here(dir_processed, "trn", "trn-reported-wide.rds"))  |> 
  filter(registry == "ClinicalTrials.gov")
  
pubmed_ft_retrieved <- read_rds(here(dir_processed, "pubmed", "pubmed-ft-retrieved.rds")) |> 
  mutate(pmid = as.character(pmid))

pubmed <-
  pubmed_main |>
  
  # Some author lists start with or consist only of "NA NA" so remove "NA NA" and make NA if blank or use next author
  mutate(
    pmid = as.character(pmid),
    citation =
      str_remove(authors, "^NA NA(, )?") |> 
      na_if("")
  ) |>
  
  # Prepare author citations based on # authors
  mutate(citation = case_when(
    
    # 1 --> as is
    !str_detect(citation, ",") ~ citation,
    
    # 2 --> "&"
    !str_detect(citation, ",.*,") ~
      str_remove(citation, "(?<=\\w) [A-Z]*$") |> 
      str_replace("[A-Z]*,", "&"),
    
    # 3 --> "et al."
    TRUE ~ str_replace(citation, "(?<=\\s)[A-Z]*,.*$", "et al.")
  )) |>
  
  # Add year
  mutate(citation = glue::glue("{citation} ({year}) {title}")) |>
  
  select(
    pmid,
    pub_title = title, journal_pubmed = journal,
    ppub_date = ppub, epub_date = epub,
    citation
  )

cali_trials <- cali_trials |>
  
  # Add info about pubmed and ft (pdf) retrieval
  left_join(pubmed_ft_retrieved,
            by = c("id", "doi", "pmid")) |>
  
  # Add pubmed metadata
  left_join(pubmed, by = "pmid")

no_pmids <- trn_reported_wide |> filter(is.na(pmid))
# Add trns ------------------------------------------------------
# cali_trials |> count(has_trn_ft)
# ct <- cali_trials |> select(id, pmid, doi, contains("has"))

cali_trials <-
  cali_trials |> 
  
  # Add info about whether trn in secondary id, abstract, ft pdf
  left_join(trn_reported_wide, by = c("id" = "trn", "doi", "pmid")) |> 
                # select(has_trn_ft, everything())
  # Trials without trn reported anywhere are not in `trn_reported` and have NA for all `has_trn`
  # However, `has_trn` should be FALSE if source retrieved
  mutate(
    has_trn_abstract =
      if_else(has_pubmed & is.na(has_trn_abstract), FALSE, has_trn_abstract),
    has_trn_secondary_id =
      if_else(has_pubmed & is.na(has_trn_secondary_id), FALSE, has_trn_secondary_id),
    has_trn_ft = if_else(has_ft & is.na(has_trn_ft), FALSE, has_trn_ft)
  ) |> 
  # select(-starts_with("has_trn_")) |> 
  
  # If trial has pubmed, trn in abstract/si must not be NA
  pointblank::col_vals_not_null(
    vars(has_trn_secondary_id, has_trn_secondary_id),
    preconditions = \(x) filter(x, has_pubmed)
  ) |>
  
  # If trial has ft pdf, trn in ft pds must not be NA
  pointblank::col_vals_not_null(
    vars(has_trn_ft),
    preconditions = \(x) filter(x, has_pubmed & has_ft)
  )


# add trial_shared
cali_trials <- cali_trials |> 
  mutate(trial_shared = ifelse(str_detect(affiliation, ", "), 1, 0),
         multiple = str_count(affiliation, ", ") + 1) |> 
  # select(-color_green_only, -ft_source, -ft_doi)
  select(-ft_source, -ft_doi)

## Write to disk
cali_trials |>
  write_excel_csv2(here("data", "California-trials_2014-2017_exp.csv"))

cali_umc <- cali_trials |>
  mutate(umc = strsplit(as.character(affiliation), ", ")) |>
  tidyr::unnest(umc)
cali_umc |> 
  write_excel_csv2(here("data", "cali_dashboard_umc.csv"))

# cali_umc <- cali_umc |> 
#   mutate(pub_year = year(publication_date_unpaywall)) |> 
#   select(id, doi, pmid, umc, contains("color"), pub_year, contains("publication"))
# cali_trials_oa <- cali_trials |> 
#     mutate(pub_year = year(publication_date_unpaywall)) |>
#     select(id, doi, pmid, affiliation, contains("color"), pub_year, contains("publication"))


## Data from the prospective registration refresh
AACT_folder <- here("data", "raw", "AACT", "AACT_dataset_220929") #insert the AACT download folder here

#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values")

load_AACT_dataset_from_txt <- function(AACT_folder, AACT_dataset_names) {
  AACT_dataset_files <- paste0(AACT_folder, "/", AACT_dataset_names, ".txt")
  AACT_datasets <- AACT_dataset_files |>
    map(read_delim, delim = "|", guess_max = 10000)
  names(AACT_datasets) <- AACT_dataset_names
  
  return(AACT_datasets)
}

#
# load_AACT_dataset_from_csv <- function(AACT_folder, AACT_dataset_names) {
#   AACT_dataset_files <- paste0(AACT_folder, AACT_dataset_names, ".csv")
#   AACT_datasets <- AACT_dataset_files |>
#     map(read_csv, guess_max = 10000)
#   names(AACT_datasets) <- AACT_dataset_names
#
#   return(AACT_datasets)
# }

AACT_datasets <- load_AACT_dataset_from_txt(AACT_folder, AACT_dataset_names)

count(AACT_datasets$sponsors, agency_class)

#----------------------------------------------------------------------------------------------------------------------
# Load search terms for the affiliations/cities
#----------------------------------------------------------------------------------------------------------------------

acronyms <- c("Stanford", "UCD", "UCI", "UCLA", "UCSD", "UCSF", "USC")

cali_unis <- list(acronyms = as.list(acronyms),
                  search_terms = list("Stanford",
                                      c("UCD",
                                        "University of California, Davis",
                                        "University of California Davis"),
                                      c("UCI",
                                        "University of California, Irvine",
                                        "University of California Irvine"),
                                      c("UCLA",
                                        "University of California, Los Angeles",
                                        "University of California Los Angeles"),
                                      c("UCSD",
                                        "University of California, San Diego",
                                        "University of California San Diego"),
                                      c("UCSF",
                                        "University of California, San Francisco",
                                        "University of California San Francisco"),
                                      c("USC",
                                        "University of Southern California")))

cali_unis$search_terms <- cali_unis$search_terms  |> 
  map(\(x) paste0("\\b", x, "\\b", collapse = "|"))

#----------------------------------------------------------------------------------------------------------------------
#  search for studies with primary sponsor affiliations matching the search terms
#----------------------------------------------------------------------------------------------------------------------

grep_fast <- function(pattern, x)
{
  return(which(str_detect(x, pattern)))
}

get_nct <- function(affil_indices, dataset)
{
  ncts <- dataset |>
    slice(affil_indices) |>
    select(nct_id)
  return(ncts[[1]])
}

city_grep <- function(dataset, colname, grep_terms)
{
  indices <- map(tolower(grep_terms), grep_fast, x=tolower(dataset[[colname]]))
  city_ncts <- map(indices, get_nct, dataset=dataset)
  names(city_ncts) <- names(grep_terms)

  return(city_ncts)
}

#joining of the different grep results
affil_join <- function(affil_nct_list)
{
  affil_indices_joined <- affil_nct_list |>
    pmap(c) |>
    map(unique) |>
    map(sort)
}

#lead sponsor if affil found in fields (PI/sponsor/responsible_party)
search_NCTS_lead_sponsor <- function(AACT_datasets, city_search_terms)
{
  #search the different affilation datasets for the city search terms
  grep_PI <- city_grep(AACT_datasets$overall_officials, "affiliation", city_search_terms)
  grep_sponsor <- city_grep(AACT_datasets$sponsors |> filter(lead_or_collaborator == "lead"),
                            "name", city_search_terms)
  grep_resp_party_org <- city_grep(AACT_datasets$responsible_parties, "organization", city_search_terms)
  grep_resp_party_affil <- city_grep(AACT_datasets$responsible_parties, "affiliation", city_search_terms)

  #combine the results for the different fields
  grep_results_primary <- list(grep_PI, grep_sponsor, grep_resp_party_org, grep_resp_party_affil)
  affil_ncts_primary <- affil_join(grep_results_primary)

  return(affil_ncts_primary)
}

#create tibble with trial numbers and affiliations
institutions_ncts_primary <- cali_unis$search_terms |>  
  map(\(x) search_NCTS_lead_sponsor(AACT_datasets, x)[[1]]) |> 
  set_names(cali_unis$acronyms) |>  
  enframe() |> 
  unnest(value) |> 
  rename(nct_id = value, affiliation = name)

#collapse tibble to deduplicate and collapse affiliations in a single cell
institutions_ncts_primary <- institutions_ncts_primary |> 
  group_by(nct_id) |> 
  summarise(affiliation = paste(affiliation, collapse = ", "))

#----------------------------------------------------------------------------------------------------------------------
# reduce the CTgov dataset to those studies that are indeed affiliated
# and filter for primary completion years & study status
#----------------------------------------------------------------------------------------------------------------------

CTgov_sample_full <- AACT_datasets$studies |>
  left_join(AACT_datasets$calculated_values |>
              select(nct_id, were_results_reported), by = "nct_id")

#add calculated columns to dataset - time to summary results & prospective registration
CTgov_sample_full <- CTgov_sample_full |>
  mutate(days_compl_to_summary = results_first_submitted_date - completion_date,
         days_reg_to_start = start_date - study_first_submitted_date)

#calculate the metrics of interest
CTgov_sample_full <- CTgov_sample_full |>
  mutate(has_prospective_registration = floor_date(start_date, unit = "month") >=
           floor_date(study_first_submitted_date, unit = "month"),
         summary_result_12_month = days_compl_to_summary < 365,
         summary_result_24_month = days_compl_to_summary < 2*365,
         summary_result_12_month = replace_na(summary_result_12_month, FALSE),
         summary_result_24_month = replace_na(summary_result_24_month, FALSE))

CTgov_sample_Cali <- CTgov_sample_full |>
  filter(
    # nct_id %in% institutions_ncts_primary$nct_id,
    nct_id %in% cali_trials$id,
    study_type == "Interventional",
    # primary_completion_date >= "2014-01-01",
    # primary_completion_date < "2017-12-31",
    # start_date > "2007-01-01",
    # start_date < "2021-12-31"
    ) |> 
  select(nct_id, start_date, is_prospective = has_prospective_registration) |> 
  mutate(start_year = year(start_date)) 

# add affiliation information
cali_prosp_reg <- CTgov_sample_Cali |> 
  left_join(institutions_ncts_primary, by = "nct_id")

write_excel_csv2(cali_prosp_reg, here("data", "processed", "cali_prospective_registration.csv"))



