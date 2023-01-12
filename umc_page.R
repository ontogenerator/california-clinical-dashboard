## Define the page layout

# iv_umc <- read_csv(
#     "data/ct-dashboard-intovalue-umc.csv"
# )
iv_umc <- vroom(here("data", "cali_dashboard_umc.csv"))

umc_page <- tabPanel(
    "One University", value = "tabUMC",
    wellPanel(
        br(),
        fluidRow(
            column(
                12,
                h1(
                    style = "margin-left: 0",
                    strong("Dashboard for clinical research transparency: Individual University/Medical School data"),
                    align = "left"
                ),
                h4(
                    style = "margin-left: 0",
                    "This dashboard displays the performance of Universities/Medical Schools in California on
                    established registration and reporting practices for clinical research transparency.
                    On this page, you can view the data for a University/Medical School of interest contextualized
                    to that across all included Universities/Medical Schools. Select the University/Medical School
                    of interest from the drop-down menu below."
                ),
                h4(style = "margin-left:0cm",
                   "The dashboard was developed as part of a scientific research project with the overall aim to
                   support the adoption of responsible research practices at Universities/Medical Schools.
                   The dashboard is a pilot and continues to be updated. More metrics may be added in the future."
                   ),
                h4("IN PROGRESS / CURRENT VERSION REFLECTS THE GERMAN DASHBOARDS: More detailed information on the
                   underlying methods can be found in the methods and limitations widgets next to each plot,
                   and in the Methods page."),
                br()
            )
        ),
        fluidRow(
            column(
                4,
                br(),
                br(),
                selectInput(
                    "selectUMC",
                    strong("Select a University/Medical School from the drop-down menu"),
                    choices = c(
                        "Select a University",
                        iv_umc %>%
                        arrange(umc) %>%
                        distinct(umc) %>%
                        pull()
                    ),
                    selected = NA
                )
            )
        )
    ),
    uiOutput("umc_registry_metrics"),
    uiOutput("umc_publication_metrics"),
    uiOutput("umc_openscience_metrics"),
    bsCollapsePanel(strong("Impressum"),
                    impressum_text,
                    style = "default"),
    bsCollapsePanel(strong("Datenschutz"),
                    datenschutz_text,
                    style = "default")
)
