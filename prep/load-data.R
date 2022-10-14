library(here)
library(vroom)
library(lubridate)
library(rcrossref)
library(tidyverse)

############## help functions
has_duplicates <- function(tib, col) {
  
  if (!rlang::as_name(enquo(col)) %in% names(tib)) {
    stop(paste(rlang::as_name(enquo(col)), "does not exist as a column name!")) 
    }
  !assertthat::are_equal(nrow(tib), nrow(tib %>% distinct({{col}})))
}

n_duplicates <- function(tib, col) {
  nrow(tib) -
    length(unique(tib %>%  pull({{col}})))
}

extract_duplicates <- function(tib, col) {
  tib %>% 
    count({{col}}) %>% 
    filter(n > 1) %>% 
    pull({{col}})
}
###############

cali_trials <- vroom(here("data", "California-trials_2014-2017.csv"))

cali_trials <- cali_trials %>% 
  mutate(publication_type = case_when(
           `any paper` == 1 ~ "journal publication",
          `any paper` == 2 ~ "abstract",
           TRUE ~ "unknown"
         ),
         publication_date = lubridate::dmy(publication_date),
         last_update_submitted_date = lubridate::dmy(last_update_submitted_date),
         registration_date = lubridate::dmy(registration_date),
         summary_results_date = lubridate::dmy(summary_results_date),
         is_prospective = lubridate::floor_date(registration_date, "month") <= 
           lubridate::floor_date(start_date, "month"),
         has_pubmed = str_length(pmid) <  10 & !is.na(pmid),
         has_reg_pub_link = ifelse(publication_found == 1, TRUE, FALSE)) %>% 
  select(-id, -id_2) %>% 
  rename(id = nct_id)

cali_trials %>% count(publication_found, publication_type)
# facilities <- vroom("C:/Datenablage/AACT/AACT_dataset_220920/facilities.txt") %>% 
#   rename(facility_name = name)

# cali_facilities <- facilities %>% # ony facilities from the German study subset
#   filter(nct_id %in% cali$nct_id,
#          state == "California") %>% 
#   select(nct_id, facility_name, city) 

# cali_umcs <- cali_facilities %>% 
#   group_by(nct_id) %>% 
#   summarise(cities = paste(city, collapse = ", "))
  
  # cali2 <- cali %>% 
  #   select(start_date, registration_date, is_prospective, everything())
# 
# cali_cities <- cali %>% 
#   left_join(cali_umcs) %>% 
#   select(-id, -id_2) %>% 
#   rename(id = nct_id)
  

# cali2 <- vroom(here("data", "California-trials_2014-2017_exp.csv")) 
has_duplicates(cali_trials, id)


# library(readxl)

## Read data from Daniel
# trials <- read_excel(
#     "prep/2022-03-08-Stanford version0_1 25_ cohort 14-17.xlsx",
#     sheet="2014-2017"
# )


pmid_to_doi <- function(pmid) {
  rcrossref::id_converter(pmid)$records$doi
}

# cali_dois <- cali_trials %>%
#   filter(!is.na(pmid), 
#          str_length(pmid) < 10) %>% 
#   select(1:6) %>%
#   slice(1:200) %>% 
#   mutate(doi = pmid_to_doi(pmid)) %>% 
#   filter(! is.na(doi)) %>% 
#   select(id, doi)
# 
# cali_dois_b <- cali_trials %>%
#   filter(!is.na(pmid), ! id %in% cali_dois$id,
#          str_length(pmid) < 10) %>%
#   select(1:6) %>%
#   slice(1:150) %>%
#   mutate(doi = pmid_to_doi(pmid)) %>%
#   filter(! is.na(doi)) %>%
#   select(id, doi)
# 
# cali_dois <- cali_dois %>%
#   bind_rows(cali_dois_b)
# 
# has_duplicates(cali_dois_b, id)


# cali_dois %>%
#   mutate(is_doi = str_detect(doi, "10.")) %>%
#   count(is_doi)

# cali_dois %>% write.csv2(here("prep", "cali_dois.csv"), row.names = FALSE)

cali_dois <- vroom(here("prep", "cali_dois.csv"))

## Remove non-extracted rows
cali_trials <- cali_trials %>%
  left_join(cali_dois) %>% # add doi from crossref
    filter(! is.na(`any paper`)) %>% 
  select(id, pmid, doi, everything())

## Read OA data from Delwen
oa <- vroom(here("prep", "cali-data-oa.csv"))

## Combine extracted data and OA data
cali_trials <- cali_trials %>%
    left_join(oa, by = "doi")

## Determine whether the trial is prospectively registered or not
# trials$start_month <- format(as.Date(trials$start_date), "%Y-%m-01")
# 
# trials$reg_month <- format(
#     as.Date(trials$registration_date),
#     "%Y-%m-01"
# )
# 
# trials$is_prospective <- trials$reg_month <= trials$start_month

## Determine the amount of follow-up
search_date <- as.Date("2022-10-01")

cali_trials$has_followup_2y <-
    as.Date(cali_trials$completion_date) + 365*2 < search_date
cali_trials$has_followup_2y_pub <- cali_trials$has_followup_2y
cali_trials$has_followup_2y_sumres <- cali_trials$has_followup_2y

cali_trials$has_followup_5y <-
    as.Date(cali_trials$completion_date) + 365*5 < search_date
cali_trials$has_followup_5y_pub <- cali_trials$has_followup_5y
cali_trials$has_followup_5y_sumres <- cali_trials$has_followup_5y

## Determine whether there are summary results within 2, 5 years

cali_trials$is_summary_results_5y <-
    as.Date(cali_trials$summary_results_date) <=
    as.Date(cali_trials$completion_date) + 365*5

cali_trials$is_summary_results_2y <-
    as.Date(cali_trials$summary_results_date) <=
    as.Date(cali_trials$completion_date) + 365*2

cali_trials$is_publication_2y <-
    as.Date(cali_trials$publication_date) <=
    as.Date(cali_trials$completion_date) + 365*2

cali_trials$is_publication_5y <-
    as.Date(cali_trials$publication_date) <=
    as.Date(cali_trials$completion_date) + 365*5

# 
cali_trials <- cali_trials %>%
    # rename(has_publication = `any paper`) %>%

  mutate(has_publication = ifelse(`any paper` > 0, TRUE, FALSE),
         ######### temp debug workaround
         has_iv_trn_abstract = TRUE,
         has_iv_trn_ft = TRUE)

## Write to disk
cali_trials %>% write.csv2("./data/California-trials_2014-2017_exp.csv", row.names = FALSE)

cali_trials %>%
  mutate(umc = strsplit(as.character(affiliation), ", ")) %>%
  tidyr::unnest(umc) %>% 
  write.csv2("./data/cali_dashboard_umc.csv", row.names = FALSE)

