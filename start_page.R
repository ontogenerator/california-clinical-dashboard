start_page <- tabPanel(
    "Start page",
    value = "tabStart",
    ## The following are defined in app.R in the Shiny server object
    uiOutput("startpage"),
    uiOutput("registry_metrics") %>% 
      shinycssloaders::withSpinner(),
    uiOutput("publication_metrics") %>% 
      shinycssloaders::withSpinner(),
    uiOutput("openscience_metrics") %>% 
      shinycssloaders::withSpinner(),
    bsCollapsePanel(strong("Impressum"),
                    impressum_text,
                    style = "default"),
    bsCollapsePanel(strong("Datenschutz"),
                    datenschutz_text,
                    style = "default")
)

