library(tidyverse)
library(readxl)

## Read data from Daniel
trials <- read_excel(
    "prep/2022-04-06-1_Final_Stanford2014_2017_SW.xlsx",
    sheet="Finalized",
    na=c("NA", "NONE")
)

## Remove non-extracted rows
trials <- trials %>%
    filter(! is.na(`any paper`))

## Read OA data from Delwen
oa <- read_csv("prep/2022-04-07-stanford-data-oa.csv")

## Combine extracted data and OA data
trials <- trials %>%
    left_join(oa, by="doi")

## Determine the amount of follow-up
search_date <- as.Date("2022-04-07")

trials$has_followup_2y <-
    as.Date(trials$completion_date) + 365*2 < search_date
trials$has_followup_2y_pub <- trials$has_followup_2y
trials$has_followup_2y_sumres <- trials$has_followup_2y

trials$has_followup_5y <-
    as.Date(trials$completion_date) + 365*5 < search_date
trials$has_followup_5y_pub <- trials$has_followup_5y
trials$has_followup_5y_sumres <- trials$has_followup_5y

## Determine whether there are summary results within 2, 5 years

trials$is_summary_results_5y <-
    as.Date(trials$summary_results_date, format="%d.%m.%Y") <=
    as.Date(trials$completion_date) + 365*5

trials$is_summary_results_2y <-
    as.Date(trials$summary_results_date, format="%d.%m.%Y") <=
    as.Date(trials$completion_date) + 365*2

trials$is_publication_2y <-
    as.Date(trials$`publication date`, format="%d.%m.%Y") <=
    as.Date(trials$completion_date) + 365*2

trials$is_publication_5y <-
    as.Date(trials$`publication date`, format="%d.%m.%Y") <=
    as.Date(trials$completion_date) + 365*5

## Rename `any paper`

trials <- trials %>%
    rename(has_publication = `any paper`)

## Renames for linkage

trials <- trials %>%
    rename(has_reg_pub_link = `where: linked`)

trials <- trials %>%
    mutate(has_pubmed = ! is.na (pmid))

## Write to disk
trials %>%
    write_csv("data/2022-04-07-stanford-data.csv")

## Now do the Stanford Trials Affiliation thing for the prospectively
## registered metric

stanfaff <- read_csv(
    "prep/2022-04-08-stanford-trials-affiliations.csv"
)

## Include only 2000-2019

stanfaff <- stanfaff %>%
    filter(
        start_date >= as.Date("2000-01-01"),
        start_date <= as.Date("2019-12-31")
        )

## Determine whether the trial is prospectively registered or not
stanfaff$start_month <- format(
    as.Date(stanfaff$start_date),
    "%Y-%m-01"
)

stanfaff$reg_month <- format(
    as.Date(stanfaff$registration_date),
    "%Y-%m-01"
)

stanfaff$is_prospective <- stanfaff$reg_month <= stanfaff$start_month

## Write to disk
stanfaff %>%
    write_csv("data/2022-04-08-stanford-trials-affiliations.csv")
