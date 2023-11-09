make_datatable <- function(dataset) {
    DT::datatable(
            data = dataset,
            rownames = FALSE,
            extensions = 'Buttons',
            filter = 'top',
            options = list(
                dom = 'Blfrtip',
                title = NULL,
                buttons =
                    list(
                      "csv",
                      list(extend = 'excel', title = NULL)
                        # list(
                        #     extend = "collection",
                        #     buttons = c("csv", "excel"),
                        #     title = NULL,
                        #     text = "Download"
                        # )
                    ),
                orderClasses = TRUE,
                pageLength = 20,
                lengthMenu = list(
                    c(10, 20, 50, 100, -1),
                    c(10, 20, 50, 100, "All")
                )
            )
        )
}

datasets_page <- tabPanel(
    "Datasets", value = "tabDatasets",
    h3("Datasets"),
    # bsCollapse(
    #     id = "datasetPanels_prospective_reg_data",
        bsCollapsePanel(
            strong("California data set"),
            DT::dataTableOutput("data_cali_data"),
            style = "default"
        # )
    ),
    # bsCollapse(
    #     id="datasetPanels_eutt_data",
    #     bsCollapsePanel(
    #         strong("EU Trials Tracker data set"),
    #         DT::dataTableOutput("data_table_eutt_data"),
    #         style="default"
    #     # )
    # ),
    # bsCollapse(
    #     id="datasetPanels_iv_data",
    #     bsCollapsePanel(
    #         strong("IntoValue"),
    #         DT::dataTableOutput("data_table_iv_data"),
    #         style="default"
    #     # )
    # ),
    bsCollapsePanel(strong("Impressum"),
                    impressum_text,
                    style = "default"),
    bsCollapsePanel(strong("Datenschutz"),
                    datenschutz_text,
                    style = "default")
)
