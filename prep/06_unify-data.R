library(here)
library(vroom)
library(rcrossref)
library(tidyverse)
library(easyRPubMed)
library(janitor)
library(readxl)

# cali_trials_original <- read_csv(here("data", "processed", "original_extractions", "California-trials_2014-2017.csv"))
# cali_trials_xlsx <- read_xlsx(here("data", "California-trials_2014-2017_main.xlsx"))
cali_trials <- read_csv(here("data", "processed", "original_extractions",  "California-trials_2014-2017_main.csv"))
# all.equal(cali_trials, cali_trials2)
# cali_trials <- cali_trials |>
#   mutate(primary_completion_date = as.POSIXct(primary_completion_date)) |>
#   rows_upsert(pub_dates, by = "nct_id") |>
#   rows_upsert(cali_trials_cdates, by = "nct_id") |>
#   write_csv(here("data", "California-trials_2014-2017_main.csv"))
# setdiff(names(cali_trials2), names(cali_trials))
# glimpse(cali_trials2)

# manually add incomplete publication_dates here!
incomplete_dates <- c("NCT02320812", "NCT00928564", "NCT02653456", "NCT01729416", "NCT01474746")

# pcy <- cali_trials |> 
#   select(id, primary_completion_year, primary_completion_date,
#          completion_year, completion_date)
# 
# pcy |> 
#   count(is.na(completion_date),
#         is.na(primary_completion_date))
  
cali_trials <- cali_trials |> 
  mutate(publication_type = case_when(
    any_paper == 1 ~ "journal publication",
    any_paper == 2 ~ "abstract",
    .default = "unknown"
    ),
    start_date = as.POSIXct(start_date, tz = "UCT"),
    # completion_date = as.POSIXct(completion_date),
    primary_completion_date = as.POSIXct(primary_completion_date, tz = "UCT"),
         # publication_date = lubridate::dmy(publication_date),
         # last_update_submitted_date = lubridate::dmy(last_update_submitted_date),
         # registration_date = lubridate::dmy(registration_date),
         # summary_results_date = lubridate::dmy(summary_results_date),
    primary_completion_year = year(primary_completion_date),
    # primary_completion_year = ifelse(is.na(primary_completion_year), completion_year, primary_completion_year),
         # has_pubmed = str_length(pmid) <  10 & !is.na(pmid),
    has_reg_pub_link = if_else(publication_found == 1, TRUE, FALSE)) |> 
  select(-id, -id_2) |> 
  rename(id = nct_id)

# cali_trials |> 
#   filter(is.na(publication_date))

cali_trials |> count(publication_found, publication_type)
cali_trials |> count(publication_found)


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
  # left_join(cali_dois_from_pmid, by = "pmid") |>
  # select(id, pmid, doi, everything())

# cali_trials_doi <- cali_trials |>
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
#   get_dupes(doi)

# cali_dois |>
   # mutate(is_doi = str_detect(doi, "10.")) |>
   # count(is_doi)
# cali_dois <- cali_dois |> mutate(doi = tolower(doi))

# cali_dois |> write_excel_csv2(here("data", "processed", "cali_dois.csv"))

cali_dois <- read_csv2(here("data", "processed", "cali_dois.csv"))
## Remove non-extracted rows
cali_trials <- cali_trials |>
  left_join(cali_dois) |> # add doi from pubmed
    filter(!is.na(any_paper)) |> 
  select(id, pmid, doi, everything())

## Read OA data from Delwen
oa <- vroom(here("data", "processed", "cali_data_oa.csv")) 
oa_manual <- read_xlsx(here("data", "processed", "oa_manual.xlsx")) |> 
  mutate(across(everything(), as.character))
## Combine extracted data and OA data
cali_trials <- cali_trials |>
  left_join(oa, by = "doi") |> 
  rows_upsert(oa_manual, by = "id") |> 
  mutate(any_open_access = case_when(
    is.na(color) ~ NA,
    str_detect(color, "bro|clo") ~ 2,
    .default = 1
  ))


# cali_trials$start_date |> class()
# cali_trials$registration_date |> class()
# cali_trials$completion_date |> class()

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


# cali_trials |> 
#   filter(id == "NCT01564381") |> 
#   select(id, doi, pmid)
# 
# pubmed_ft_retrieved |>
#   filter(id == "NCT01564381") |>
#   select(id, doi, pmid)

cali_trials <- cali_trials |>
  
  # Add info about pubmed and ft (pdf) retrieval
  left_join(pubmed_ft_retrieved,
            # by = c("id", "doi")
            by = c("id", "doi", "pmid")
            ) |>
  
  # Add pubmed metadata
  left_join(pubmed, by = "pmid")

no_pmids <- trn_reported_wide |> filter(is.na(pmid))

cali_trials <- cali_trials |> 
  mutate(
    is_prospective = floor_date(start_date, "month") >=
      floor_date(registration_date, "month"),
    is_prospective_usa = registration_date <= start_date + days(21),
    is_summary_results_1y = case_when(
      is.na(summary_results_date) ~ FALSE,
      .default = as.Date(summary_results_date) <= 
        as.Date(primary_completion_date) + 365
    ),  
    is_summary_results_2y = case_when(
      is.na(summary_results_date) ~ FALSE,
      .default = as.Date(summary_results_date) <=
        as.Date(primary_completion_date) + 365*2
    ),
    is_summary_results_5y = case_when(
      is.na(summary_results_date) ~ FALSE,
      .default = as.Date(summary_results_date) <=
        as.Date(primary_completion_date) + 365*5
    ),
    is_publication_2y = case_when(
      is.na(publication_date) ~ FALSE,
      .default = as.Date(publication_date) <=
        as.Date(primary_completion_date) + 365*2
    ),
    is_publication_5y = case_when(
      is.na(publication_date) ~ FALSE,
      .default = as.Date(publication_date) <=
        as.Date(primary_completion_date) + 365*5
    ),
    publication_date_type = case_when(
      id %in% incomplete_dates ~ "Estimated",
      .default = "Exact")
    ,
    has_publication = ifelse(any_paper > 0, TRUE, FALSE))

usa_check <- cali_trials |> 
  select(id, doi, contains("is_prosp"), start_date, registration_date)

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
    has_trn_ft = case_when(
      any_paper == 2 ~ NA,
      has_ft & is.na(has_trn_ft) ~ FALSE,
      .default = has_trn_ft
    )
      # if_else(has_ft & is.na(has_trn_ft), FALSE, has_trn_ft)
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
  select(-ft_source, -ft_doi, -registry)



# cali_umc <- cali_umc |> 
#   mutate(pub_year = year(publication_date_unpaywall)) |> 
#   select(id, doi, pmid, umc, contains("color"), pub_year, contains("publication"))
# cali_trials_oa <- cali_trials |> 
#     mutate(pub_year = year(publication_date_unpaywall)) |>
#     select(id, doi, pmid, affiliation, contains("color"), pub_year, contains("publication"))


## Update data from AACT to get which dates were estimated, plus Sponsor info
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

# #add calculated columns to dataset - time to summary results & prospective registration
# CTgov_sample_full <- CTgov_sample_full |>
#   mutate(days_compl_to_summary = results_first_submitted_date - primary_completion_date,
#          days_reg_to_start = start_date - study_first_submitted_date)

#calculate the metrics of interest
# CTgov_sample_full <- CTgov_sample_full |>
#   mutate(
    # is_prospective = floor_date(start_date, unit = "month") >=
    #        floor_date(study_first_submitted_date, unit = "month"),
    #      # is_prospective_type = case_when(
         #   !str_detect(start_month_year, ",")  ~ "Estimated",
         #   .default = "Exact"
         # ),
         # summary_result_12_month = days_compl_to_summary < 365,
         # summary_result_24_month = days_compl_to_summary < 2*365,
         # summary_result_12_month = replace_na(summary_result_12_month, FALSE),
         # summary_result_24_month = replace_na(summary_result_24_month, FALSE),
         # summary_result_type = case_when(
         #   !str_detect(primary_completion_date, ",")  ~ "Exact",
         #   .default = "Estimated"
         # )
         # )

CTgov_sample_Cali <- CTgov_sample_full |>
  filter(
    nct_id %in% cali_trials$id,
    ) |> 
  select(id = nct_id, contains("start_"),
         contains("completion_"),
         -contains("type")
         # summary_result_type,
         # results_first_submitted_date, # this was identical to summary_results_date
         # study_first_submitted_date, # this was identical to registration_date
         # is_prospective
         # is_prospective_type
         )

funding <- AACT_datasets$sponsors |> 
  select(id = nct_id,
         agency_class,
         name) |> 
  nest(.by = id)

CTgov_sample_Cali <- CTgov_sample_Cali |> 
  left_join(funding, by = "id") |> 
  mutate(agency_class = map_chr(data, \(ac) pull(ac, agency_class) |> paste(collapse = "; ")),
         sponsor_name = map_chr(data, \(sp) pull(sp, name) |> paste(collapse = "; "))) |> 
  select(-data)

CTgov_sample_Cali |> 
  count(agency_class, sort = TRUE)

affils <- cali_trials |>
  select(id, affiliation)

CTgov_sample_Cali <- CTgov_sample_Cali |> 
  left_join(affils, by = "id")

CTgov_sample_Cali <- CTgov_sample_Cali |> 
  select(id, contains("month"), agency_class, sponsor_name) |>
  mutate(start_date_type =
           if_else(str_detect(start_month_year, ", "), "Exact", "Estimated"),
         completion_date_type =
           if_else(str_detect(completion_month_year, ", "), "Exact", "Estimated"),
         primary_completion_date_type =
           if_else(str_detect(primary_completion_month_year, ", "),
                   "Exact", "Estimated")) |> 
  select(id, contains("start"), completion_month_year, completion_date_type,
         primary_completion_month_year, primary_completion_date_type,
         agency_class, sponsor_name) |> 
  filter(id != "")

CTgov_sample_Cali <- CTgov_sample_Cali |> 
  group_by(id) |> 
  summarise(across(everything(), first)) |> 
  ungroup()

cali_trials <- cali_trials |> 
  group_by(id) |> 
  summarise(across(everything(), first)) |> 
  ungroup() |> 
  left_join(CTgov_sample_Cali , by = "id")

## Write to disk
cali_trials |>
  write_excel_csv2(here("data", "California-trials_2014-2017_exp.csv"))

cali_umc <- cali_trials |>
  mutate(umc = strsplit(as.character(affiliation), ", ")) |>
  tidyr::unnest(umc)
cali_umc |> 
  write_excel_csv2(here("data", "cali_dashboard_umc.csv"))

cali_trials |> 
  distinct(doi, .keep_all = TRUE) |> 
  count(is.na(has_trn_ft))

# missing_pdfs <- cali_trials |> 
#   distinct(doi, .keep_all = TRUE) |> 
#   filter(is.na(has_trn_ft),
#          any_paper != 2,
#          str_detect(doi, "^10.")) |> 
#   select(doi, pmid)

pt <- cali_trials |> 
  select(publication_type, doi, has_trn_ft) |> 
  filter(!is.na(doi))

pt |> 
  filter(publication_type != "abstract") |> 
  count(has_trn_ft)

# 
# cali_prosp <- cali_trials |> 
#   left_join(CTgov_sample_Cali, by = "id") |> 
#   rowwise() |> 
#   mutate(start_date_type = case_when(
#     !str_detect(start_month_year, ",")  ~ "Estimated",
#     .default = "Exact"
#   )) |> 
#   select(id, is_prospective, is_prospective_type, study_first_submitted_date, results_first_submitted_date,
#          start_date_type, start_month_year, contains("start_date"), contains("primary_completion"),
#          everything()) 
# 
# cali_prosp |> count(start_date_type)

# 
# cali_prosp |> 
#   count(is_prospective,
#         is_prospective_type)

orig_dates <- cali_trials |> 
  select(id, contains("date"))


# rfs <- CTgov_sample_Cali |> 
#   left_join(orig_dates, by = "id") |> 
#   mutate(completion_date_match = case_when(
#     !str_detect(completion_month_year, ",") ~ floor_date(completion_date.x, "month") ==
#       completion_date.y,
#     .default = completion_date.x == completion_date.y,
#   )
#            ) |> 
#   select(id, contains("completion"), everything())

# proof that study_first_submitted_date = registration_date
rds <- cali_trials |> 
  select(id, registration_date)

rds_compare <- CTgov_sample_full |> 
  select(id = nct_id, study_first_submitted_date) |> 
  right_join(rds)


pub_dates <- cali_trials |> 
  select(id, pmid, doi, publication_date, epub_date, ppub_date, publication_date_unpaywall) |> 
  mutate(epub_date = as.POSIXct(epub_date, tz = "UTC"),
         publication_date_unpaywall = case_when(
           !str_detect(publication_date_unpaywall, "-01-01") ~ as.POSIXct(publication_date_unpaywall, tz = "UTC"),           .default = NA),
         ppub_date = case_when(
           str_count(ppub_date, "-") == 2 ~ as.POSIXct(ymd(ppub_date), tz = "UTC"),
           .default = NA
         )) |> 
  rowwise() |> 
  mutate(
    # min_date = min(epub_date, ppub_date, na.rm = TRUE),
         
         min_3_date = case_when(
           month(publication_date_unpaywall) == month(publication_date) & 
             year(publication_date_unpaywall) == year(publication_date) ~
             min(c(epub_date, ppub_date, publication_date), na.rm = TRUE),
           is.na(epub_date) & is.na(ppub_date) & is.na(publication_date_unpaywall) ~ publication_date,
           .default = min(c(epub_date, ppub_date, publication_date_unpaywall), na.rm = TRUE))
         )

pub_dates <- pub_dates |> 
  select(nct_id = id, publication_date = min_3_date)


fom <- pub_dates |> 
  filter(!is.na(publication_date),
         str_detect(publication_date_unpaywall, "-01$"),
         publication_date_unpaywall == min_3_date,
         publication_date_unpaywall != publication_date
  )

pub_dates |> 
  count(is.na(epub_date), is.na(ppub_date), is.na(publication_date_unpaywall),
        is.na(publication_date))

pdu <- pub_dates |> 
  filter(!is.na(epub_date)) |> 
  filter(publication_date != epub_date) |> 
  select(id, doi, contains("date")) 

pd <- pub_dates |> 
  filter(is.na(epub_date)) |> 
  filter(publication_date != publication_date_unpaywall) |> 
  select(id, doi, contains("date"))

pdmin <- pub_dates |> 
  filter(!is.na(epub_date)) |> 
  filter(publication_date != min_date) |> 
  select(id, doi, contains("date")) 


new_pd <- pub_dates |> 
  filter(min_3_date < Inf) |> 
  filter(publication_date != min_3_date) |> 
  select(id, doi, contains("date"))


cali_trials |> 
  select(id, contains("date"))

cali_trials |> 
  select(id, publication_date, doi, pmid, has_publication, has_pubmed, publication_type, has_trn_ft) |> 
  filter(id == "NCT01823458")

cali_qa <- cali_trials |> 
  select(id, publication_date, doi, pmid, any_paper, publication_type, contains("has_"), color)


cali_trials_cdates <- cali_trials |> 
  mutate(
    pc_date = case_when(
      primary_completion_date_type == "Estimated" ~ ceiling_date(primary_completion_date, unit = "month", change_on_boundary = TRUE) - days(1),
    .default = primary_completion_date
       ),
    is_sr_1y = case_when(
      is.na(summary_results_date) ~ FALSE,
      .default = as.Date(summary_results_date) <= 
        as.Date(pc_date) + 365
    ),  
    is_sr_2y = case_when(
      is.na(summary_results_date) ~ FALSE,
      .default = as.Date(summary_results_date) <=
        as.Date(pc_date) + 365*2
    ),
    is_sr_5y = case_when(
      is.na(summary_results_date) ~ FALSE,
      .default = as.Date(summary_results_date) <=
        as.Date(pc_date) + 365*5
    ),
    is_p_2y = case_when(
      is.na(publication_date) ~ FALSE,
      .default = as.Date(publication_date) <=
        as.Date(pc_date) + 365*2
    ),
    is_p_5y = case_when(
      is.na(publication_date) ~ FALSE,
      .default = as.Date(publication_date) <=
        as.Date(pc_date) + 365*5
    )) |> 
  select(id, publication_date, primary_completion_date, primary_completion_date_type,
         pc_date, contains("is_p"), summary_results_date, contains("is_s"))


cali_trials$primary_completion_date[1] 
ceiling_date(cali_trials$primary_completion_date[1], change_on_boundary = TRUE, unit = "month") - days(1)

x <- ymd_hms("2009-08-03 12:01:59.23")
ceiling_date(x, "month") - days(1)

cali_trials_cdates <- cali_trials_cdates |> 
  select(nct_id = id, primary_completion_date = pc_date)

cali_trials_cdates |> 
  filter(id == "NCT01154192") |> 
  mutate(
    d1 = as.Date(publication_date),
    d2 = as.Date(primary_completion_date) + 2*365,
    diff = d1 <= d2) |> 
  select(d1, d2, diff)
