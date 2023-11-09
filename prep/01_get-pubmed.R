library(dplyr)
library(fs)
library(here)
library(readxl)

source(here("prep", "functions", "download_pubmed.R"))

dir <- dir_create(here("data", "raw", "pubmed"))

# cali1 <- vroom::vroom(here("data", "California-trials_2014-2017.csv"))
cali <- read_xlsx(here("data", "California-trials_2014-2017_main.xlsx"))


pmids <-
  cali |>
  tidyr::drop_na(pmid) |>
  filter(str_length(pmid) < 10) |> 
  distinct(pmid) |>
  pull()



# If pmids already downloaded, remove those from list to download
if (dir_exists(dir)){
  
  pmids_downloaded <-
    dir_ls(dir) |>
    path_file() |>
    path_ext_remove() |>
    as.numeric()
  
  # Check whether pmids downloaded which aren't needed and manually review and remove
  pmids_downloaded_unused <- setdiff(pmids_downloaded, pmids)
  if (length(pmids_downloaded_unused) > 0) {
    rlang::warn(glue::glue("Unused pmid downloaded: {pmids_downloaded_unused}"))
  }
  
  pmids <- setdiff(pmids, pmids_downloaded)
}

# Download remaining pmids, if any
if (length(pmids) > 0) {

  pmids |>
    purrr::walk(download_pubmed,
                dir = dir,
                api_key = Sys.getenv("ENTREZ_KEY")
    )
  
  # Log query date
  loggit::set_logfile(here::here("queries.log"))
  loggit::loggit("INFO", "PubMed")
}

