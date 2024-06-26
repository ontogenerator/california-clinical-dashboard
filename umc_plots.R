## Prospective registration
umc_plot_clinicaltrials_prereg <- function (dataset, dataset_all, umc_selected, color_palette) {
  # umc_plot_clinicaltrials_prereg <- function (dataset, dataset_all, dataset_umc, dataset_all, toggled_registry, umc, color_palette) {
    
    # if (toggled_registry == "ClinicalTrials.gov") {
        
        dataset <- dataset |>
            filter(!is.na(start_date))
        
        dataset_all <- dataset_all |>
            filter(!is.na(start_date))
        
        years <- seq(from=min(dataset$start_year, na.rm=TRUE), to=max(dataset$start_year, na.rm=TRUE))

        plot_data <- tribble(
            ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover
        )

        for (current_year in years) {

            numer_for_year <- dataset |>
                filter(
                    umc == umc_selected,
                    start_year == current_year,
                    is_prospective == TRUE
                ) |>
                nrow()

            denom_for_year <- dataset |>
                filter(
                    umc == umc_selected,
                    start_year == current_year
                ) |>
                nrow()

            all_numer_for_year <-  dataset_all |>
                filter(
                    start_year == current_year,
                    is_prospective == TRUE
                ) |>
                nrow()

            all_denom_for_year <- dataset_all |>
                filter(
                    start_year == current_year
                ) |>
                nrow()

            if (denom_for_year > 0) {
                percentage_for_year <- round(100*numer_for_year/denom_for_year, digits=1)
            } else {
                percentage_for_year <- NA
            }

            all_percentage_for_year <- round(100*all_numer_for_year/all_denom_for_year, digits=1)
            
            plot_data <- plot_data |>
                bind_rows(
                    tribble(
                        ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover,
                        current_year, all_percentage_for_year, percentage_for_year, paste0(all_numer_for_year, "/", all_denom_for_year), paste0(numer_for_year, "/", denom_for_year)
                    )
                )

        }

    plot_ly(
        data=plot_data,
        x = ~year,
        y = ~all_percentage,
        name = "All",
        text = ~all_mouseover,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
        add_trace(
            data=plot_data |> filter(!is.na(umc_percentage)),
            y=~umc_percentage,
            name=umc_selected,
            text=~umc_mouseover,
            marker = list(color = color_palette[2])
        ) |>
        layout(
            xaxis = list(
                title = '<b>Start year</b>',
                dtick = 1
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 105)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9],
            legend = list(xanchor= "left")
        )
    
}

## TRN

umc_plot_clinicaltrials_trn <- function (dataset, dataset_all, umc_selected, color_palette) {

    plot_data_abs <- dataset |>
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            has_pubmed == TRUE
        )
    
    plot_data_abs_all <- dataset_all |>
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            has_pubmed == TRUE
        )
    
    plot_data_ft <- dataset |>
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            has_ft == TRUE
        )
    
    plot_data_ft_all <- dataset_all |>
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            has_ft == TRUE
        )
    
    all_numer_abs <- sum(plot_data_abs_all$has_trn_abstract, na.rm=TRUE)
    abs_denom <- plot_data_abs_all |>
        filter(! is.na(has_trn_abstract)) |>
        nrow()
    
    all_numer_ft <- sum(plot_data_ft_all$has_trn_ft, na.rm=TRUE)
    ft_denom <- plot_data_ft_all |>
        filter(! is.na(has_trn_ft)) |>
        nrow()

    umc_abs_denom <- plot_data_abs |>
        filter(umc == umc_selected) |>
        filter(! is.na(has_trn_abstract)) |>
        nrow()

    umc_numer_abs <- plot_data_abs |>
        filter(umc == umc_selected) |>
        select(has_trn_abstract) |>
        filter(has_trn_abstract == TRUE) |>
        nrow()

    umc_numer_ft <- plot_data_ft |>
        filter(umc == umc_selected) |>
        select(has_trn_ft) |>
        filter(has_trn_ft == TRUE) |>
        nrow()

    umc_ft_denom <- plot_data_ft |>
        filter(umc == umc_selected) |>
        filter(! is.na(has_trn_ft)) |>
        nrow()

    plot_data <- tribble(
        ~x_label, ~colour, ~percentage, ~mouseover,
        "All", "In abstract", round(100*all_numer_abs/abs_denom, digits=1), paste0(all_numer_abs, "/", abs_denom),
        "All", "In full text", round(100*all_numer_ft/ft_denom, digits=1), paste0(all_numer_ft, "/", ft_denom),
        umc_selected, "In abstract", round(100*umc_numer_abs/umc_abs_denom, digits=1), paste0(umc_numer_abs, "/", umc_abs_denom),
        umc_selected, "In full text", round(100*umc_numer_ft/umc_ft_denom, digits=1), paste0(umc_numer_ft, "/", umc_ft_denom)
    )

    plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)

    plot_ly(
        plot_data,
        x = ~x_label,
        color = ~colour,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        colors = c(
            "#F1BA50",
            "#007265",
            "#634587"
        ),
        marker = list(
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Trials with publication (%)</b>',
                range = c(0, 105)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}
# cali_test <- cali_trials |> select(has_publication, publication_type, has_pubmed, has_reg_pub_link)
# Linkage

umc_plot_linkage <- function (dataset, dataset_all, umc_selected, color_palette) {
  # umc_plot_linkage <- function (dataset, dataset_all, chosenregistry, umc, color_palette) {
    dataset <- dataset |>
        filter(has_publication == TRUE,
               publication_type == "journal publication",
               has_pubmed == TRUE | ! is.na (doi),
               ! is.na (primary_completion_year))
    
    dataset_all <- dataset_all |>
        filter(has_publication == TRUE,
               publication_type == "journal publication",
               has_pubmed == TRUE | ! is.na (doi),
               ! is.na (primary_completion_year))

    
    years <- seq(from=min(dataset_all$primary_completion_year), to=max(dataset_all$primary_completion_year))
    
    umcdata <- dataset |>
        filter (umc == umc_selected)
    
    plot_data <- tribble(
        ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover
    )

    for (current_year in years) {

        umc_numer <- umcdata |>
            filter(has_reg_pub_link == TRUE) |>
            filter(primary_completion_year == current_year) |>
            nrow()

        umc_denom <- umcdata |>
            filter(primary_completion_year == current_year) |>
            nrow()

        all_numer <- dataset_all |>
            filter(has_reg_pub_link == TRUE) |>
            filter(primary_completion_year == current_year) |>
            nrow()

        all_denom <- dataset_all |>
            filter(primary_completion_year == current_year) |>
            nrow()

        if (umc_denom > 0) {
            percentage_for_year <- round(100*umc_numer/umc_denom, digits=1)
        } else {
            percentage_for_year <- NA
        }
        
        all_percentage_for_year <- round(100*all_numer/all_denom, digits=1)
        
        plot_data <- plot_data |>
            bind_rows(
                tribble(
                    ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover,
                    current_year, all_percentage_for_year, percentage_for_year, paste0(all_numer, "/", all_denom), paste0(umc_numer, "/", umc_denom)
                )
            )
        
    }

     plot_ly(
        data = plot_data,
        name = "All",
        x = ~year,
        y = ~all_percentage,
        text = ~all_mouseover,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
         add_trace(
             data=plot_data |> filter(!is.na(umc_percentage)),
             y=~umc_percentage,
             text=~umc_mouseover,
             name=umc_selected,
             marker = list(color = color_palette[2])
         ) |>
         layout(
             xaxis = list(
                 title = '<b>Completion year</b>',
                 dtick = 1
             ),
             yaxis = list(
                 title = '<b>Trials with publication (%)</b>',
                 range = c(0, 105)
             ),
             paper_bgcolor = color_palette[9],
             plot_bgcolor = color_palette[9],
             legend = list(xanchor= "left")
         )
}

## Summary results
umc_plot_clinicaltrials_sumres <- function (dataset, all_dataset, umc_selected, color_palette) {

        dataset <- dataset |> 
          filter(!is.na(primary_completion_year)) |>
            filter(
                umc == umc_selected
            )

        min_year <- dataset$primary_completion_year |>
            min(na.rm = TRUE)

        max_year <- dataset$primary_completion_year |>
            max(na.rm = TRUE)

        umc_data <- tribble(
            ~date, ~percent_reported, ~umc
        )

        for (currentyear in seq(from=min_year, to=max_year)) {

            currentyear_trials <- dataset |>
                filter(
                    primary_completion_year <= currentyear
                )

            currentyear_denom <- nrow(currentyear_trials)

            currentyear_numer <- currentyear_trials |>
                filter(is_summary_results_1y == TRUE) |>
                nrow()

            umc_data <- umc_data |>
                bind_rows(
                    tribble(
                        ~date, ~percent_reported, ~umc, ~mouseover,
                        currentyear, round(100*currentyear_numer/currentyear_denom, digits=1), umc_selected, paste0(currentyear_numer, "/", currentyear_denom)
                    )
                )
            
        }

        dataset <- all_dataset

        all_data <- tribble(
            ~date, ~percent_reported, ~umc
        )

        for (currentyear in seq(from=min_year, to=max_year)) {

            currentyear_trials <- dataset |>
                filter(
                    primary_completion_year <= currentyear
                )

            currentyear_denom <- nrow(currentyear_trials)

            currentyear_numer <- currentyear_trials |>
                filter(is_summary_results_1y == TRUE) |>
                nrow()

            all_data <- all_data |>
                bind_rows(
                    tribble(
                        ~date, ~percent_reported, ~umc, ~mouseover,
                        currentyear, round(100*currentyear_numer/currentyear_denom, digits=1), "All", paste0(currentyear_numer, "/", currentyear_denom)
                    )
                )
            
        }
        
    # }

    # plot_data <- rbind(all_data, umc_data)
    
    plot_ly(
      all_data,
      x = ~date,
      y = ~percent_reported,
      name = "All",
      text = ~mouseover,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(
        color = color_palette[3],
        line = list(
          color = 'rgb(0,0,0)',
          width = 1.5
        )
      )
    ) |>    
      add_trace(
        data=umc_data,
        text=~mouseover,
        name=~umc,
        marker = list(color = color_palette[2])
      ) |>
    # plot_ly(
    #     plot_data,
    #     x = ~date,
    #     y = ~percent_reported,
    #     name = ~umc,
    #     text = ~mouseover,
    #     type = 'scatter',
    #     mode = 'lines+markers',
    #     marker = list(
    #         color = color_palette[3],
    #         line = list(
    #             color = 'rgb(0,0,0)',
    #             width = 1.5
    #         )
    #     )
    # ) |>

        layout(
            xaxis = list(
                title = '<b>Date</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 105)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9],
            legend = list(xanchor= "left")
        )
    
}

# Timely publication within 2 years
umc_plot_clinicaltrials_timpub_2a <- function (dataset, dataset_all, umc_selected, rt, color_palette) {

    if (rt != "Summary results or manuscript publication") {

        if (rt == "Summary results only") {
            dataset$published_2a <- dataset$is_summary_results_2y
            
            dataset_all$published_2a <- dataset_all$is_summary_results_2y
        }
        
        if (rt == "Manuscript publication only") {
 
            dataset$published_2a <- dataset$is_publication_2y
            
            dataset_all$published_2a <- dataset_all$is_publication_2y
        }
        
    } else {

        dataset$published_2a <- dataset$is_summary_results_2y | dataset$is_publication_2y
        
        dataset_all$published_2a <- dataset_all$is_summary_results_2y | dataset_all$is_publication_2y
    }

    years <- seq(from=min(dataset$primary_completion_year), to=max(dataset$primary_completion_year))

    all_denom <- dataset_all |>
        nrow()
    
    all_numer <- dataset_all |>
        filter(published_2a) |>
        nrow()

    plot_data <- tribble(
        ~year, ~all_percentage, ~umc_percentage
    )

    for (current_year in years) {

        umc_numer <-  dataset |>
            filter(
                umc == umc_selected,
                primary_completion_year == current_year,
                published_2a
            ) |>
            nrow()

        umc_denom <-  dataset |>
            filter(
                umc == umc_selected,
                primary_completion_year == current_year
            ) |>
            nrow()

        all_numer <-  dataset_all |>
            filter(
                primary_completion_year == current_year,
                published_2a
            ) |>
            nrow()

        all_denom <-  dataset_all |>
            filter(
                primary_completion_year == current_year
            ) |>
            nrow()

        if (umc_denom > 0) {
            umc_percentage <- round(100*umc_numer/umc_denom, digits=1)
        } else {
            umc_percentage <- NA
        }
        
        all_percentage <- round(100*all_numer/all_denom, digits=1)

        plot_data <- plot_data |>
            bind_rows(
                tribble(
                    ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover,
                    current_year, all_percentage, umc_percentage, paste0(all_numer, "/", all_denom), paste0(umc_numer, "/", umc_denom)
                )
            )
        
    }

    plot_ly(
        data=plot_data,
        x = ~year,
        y = ~all_percentage,
        text = ~all_mouseover,
        name = "All",
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
        add_trace(
            data=plot_data |> filter(!is.na(umc_percentage)),
            y=~umc_percentage,
            name=umc_selected,
            text=~umc_mouseover,
            marker = list(color = color_palette[2])
        ) |>
        layout(
            xaxis = list(
                title = '<b>Completion year</b>',
                dtick = 1
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 105)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9],
            legend = list(xanchor= "left")
        )
    
}

# Timely publication within 5 years
umc_plot_clinicaltrials_timpub_5a <- function (dataset, dataset_all, umc_selected, rt, color_palette) {

    if (rt != "Summary results or manuscript publication") {

        if (rt == "Summary results only") {
            dataset$published_5a <- dataset$is_summary_results_5y
            
            dataset_all$published_5a <- dataset_all$is_summary_results_5y
        }
        
        if (rt == "Manuscript publication only") {
            dataset$published_5a <- dataset$is_publication_5y
  
            dataset_all$published_5a <- dataset_all$is_publication_5y
        }
        
    } else {

        dataset$published_5a <- dataset$is_summary_results_5y | dataset$is_publication_5y
        
        dataset_all$published_5a <- dataset_all$is_summary_results_5y | dataset_all$is_publication_5y
    }

    years <- seq(from=min(dataset$primary_completion_year), to=max(dataset$primary_completion_year))

    all_denom <- dataset_all |>
        nrow()
    
    all_numer <- dataset_all |>
        filter(published_5a) |>
        nrow()
    
    plot_data <- tribble(
        ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover
    )

    for (current_year in years) {

        umc_numer <-  dataset |>
            filter(
                umc == umc_selected,
                primary_completion_year == current_year,
                published_5a
            ) |>
            nrow()

        umc_denom <-  dataset |>
            filter(
                umc == umc_selected,
                primary_completion_year == current_year
            ) |>
            nrow()

        all_numer <-  dataset_all |>
            filter(
                primary_completion_year == current_year,
                published_5a
            ) |>
            nrow()

        all_denom <-  dataset_all |>
            filter(
                primary_completion_year == current_year
            ) |>
            nrow()
        
        if (umc_denom > 0) {
            umc_percentage <- round(100*umc_numer/umc_denom, digits=1)
        } else {
            umc_percentage <- NA
        }
        
        all_percentage <- round(100*all_numer/all_denom, digits=1)

        manuscript_denom <- dataset |>
            filter(
                primary_completion_year == current_year
            ) |>
            nrow()

        if (rt ==
            "Summary results only" |
            manuscript_denom >
            5) { ## This is because we only have 1
            ## data point in 2013 with 5 years of
            ## follow-up

            plot_data <- plot_data |>
                bind_rows(
                    tribble(
                        ~year, ~all_percentage, ~umc_percentage, ~all_mouseover, ~umc_mouseover,
                        current_year, all_percentage, umc_percentage, paste0(all_numer, "/", all_denom), paste0(umc_numer, "/", umc_denom)
                    )
                )
        }
        
    }

    plot_ly(
        data=plot_data,
        x = ~year,
        y = ~all_percentage,
        name = "All",
        text = ~all_mouseover,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
        add_trace(
            data=plot_data |> filter(!is.na(umc_percentage)),
            y=~umc_percentage,
            text=~umc_mouseover,
            name=umc_selected,
            marker = list(color = color_palette[2])
        ) |>
        layout(
            xaxis = list(
                title = '<b>Completion year</b>',
                dtick = 1
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 105)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9],
            legend = list(xanchor= "left")
        )
    
}

## Open access
umc_plot_opensci_oa <- function (dataset, dataset_all, umc_selected, absnum, color_palette) {
  # dataset_all <- cali_trials
  #dataset <- cali_umc
  # umc_selected <- "Stanford"
    ## Calculate the numerators and the denominator for the
    ## "all" bars

    plot_data <- dataset |>
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            umc == umc_selected,
            !is.na(doi),
            !is.na(publication_date_unpaywall)
        ) |>
        distinct(doi, .keep_all = TRUE)

    plot_data_all <- dataset_all |>
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            !is.na(doi),
            !is.na(publication_date_unpaywall)
        ) |>
        distinct(doi, .keep_all=TRUE)

    all_denom <- plot_data_all |>
        nrow()

    all_gold <- plot_data_all |>
        filter( color == "gold") |>
        nrow()

    all_green <- plot_data_all |>
        filter( color == "green") |>
        nrow()

    all_hybrid <- plot_data_all |>
        filter( color == "hybrid") |>
        nrow()

    all_na <- plot_data_all |>
        filter( is.na(color) ) |>
        nrow()

    all_closed <- plot_data_all |>
        filter( color == "closed") |>
        nrow()

    all_bronze <- plot_data_all |>
        filter( color == "bronze") |>
        nrow()


    umc_denom <- plot_data |>
        nrow()

    umc_gold <- plot_data |>
        filter(
          color == "gold"
        ) |>
        nrow()

    umc_green <- plot_data |>
        filter(
          color == "green"
        ) |>
        nrow()

    umc_hybrid <- plot_data |>
        filter(
          color == "hybrid"
        ) |>
        nrow()

    umc_na <- plot_data |>
        filter(
            is.na(color)
        ) |>
        nrow()

    umc_closed <- plot_data |>
        filter(
          color == "closed"
        ) |>
        nrow()

    umc_bronze <- plot_data |>
        filter(
          color == "bronze"
        ) |>
        nrow()

    if (absnum == "Show absolute numbers") {

        dataset <- dataset |>
            filter(
                has_publication == TRUE,
                publication_type == "journal publication",
                !is.na(doi),
                ! is.na (publication_date_unpaywall),
                umc == umc_selected
            ) |>
            distinct(doi, .keep_all = TRUE)

        dataset$oa_year <- dataset$publication_date_unpaywall |>
            format("%Y")

        plot_data <- tribble(
            ~x_label, ~gold,    ~green,    ~hybrid,    ~na,    ~closed,    ~bronze
        )

        upperlimit <- 0
        
        years <- seq(from=min(dataset$oa_year, na.rm=TRUE), to=max(dataset$oa_year, na.rm=TRUE))

        for (year in years) {
            
            gold_num <- dataset |>
                filter(
                    oa_year == year,
                    color == "gold"
                ) |>
                nrow()
            
            green_num <- dataset |>
                filter(
                    oa_year == year,
                    color == "green"
                ) |>
                nrow()

            hybrid_num <- dataset |>
                filter(
                    oa_year == year,
                    color == "hybrid"
                ) |>
                nrow()

            na_num <- dataset |>
                filter(
                    oa_year == year,
                    is.na(color)
                ) |>
                nrow()
            
            closed_num <- dataset |>
                filter(
                    oa_year == year,
                    color == "closed"
                ) |>
                nrow()

            bronze_num <- dataset |>
                filter(
                    oa_year == year,
                    color == "bronze"
                ) |>
                nrow()
            
            year_denom <- dataset |>
                filter(
                    oa_year == year
                ) |>
                nrow()

            if (year_denom > 0) {
                plot_data <- plot_data |>
                    bind_rows(
                        tribble(
                            ~x_label, ~gold,    ~green,    ~hybrid,  ~na,    ~closed,    ~bronze,
                            year, gold_num, green_num, hybrid_num, na_num, closed_num, bronze_num
                        )
                    )
            }

            year_upperlimit <- 1.1*year_denom
            upperlimit <- max(year_upperlimit, upperlimit)
            
        }

        ylabel <- "Number of publications"
        
    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~gold,
        name = "Gold",
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
        add_trace(
            y = ~green,
            name = "Green",
            marker = list(
                color = color_palette[8],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) |>
        add_trace(
            y = ~hybrid,
            name = "Hybrid",
            marker = list(
                color = color_palette[10],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) |>
        add_trace(
            y = ~bronze,
            name = "Bronze",
            marker = list(
                color = color_palette[4],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) |>
        add_trace(
            y = ~closed,
            name = "Closed",
            marker = list(
                color = color_palette[1],
                line = list(
                    color = 'rgb(0,0,0)',
                    width = 1.5
                )
            )
        ) |>
        
        # add_trace(
        #     y = ~na,
        #     name = "No data",
        #     marker = list(
        #         color = color_palette[6],
        #         line = list(
        #             color = 'rgb(0,0,0)',
        #             width = 1.5
        #         )
        #     )
        # ) |>
        layout(
            barmode = 'stack',
            xaxis = list(
                title = '<b>Year of publication</b>',
                spikemode = 'marker',
                spikethickness = 0
            ),
            yaxis = list(
                title = paste('<b>', ylabel, '</b>'),
                range = c(0, upperlimit)
            ),
            hovermode = "x unified",
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    

        
    } else {
        ## Not "absolute numbers"

        plot_data <- tribble(
            ~x_label, ~gold,                         ~green,                         ~hybrid,                         ~na,                         ~closed,                         ~bronze,     ~gold_numer, ~green_numer, ~hybrid_numer, ~bronze_numer, ~sum,
            "All",    round(100*all_gold/all_denom, digits=1), round(100*all_green/all_denom, digits=1), round(100*all_hybrid/all_denom, digits=1), round(100*all_na/all_denom, digits=1), round(100*all_closed/all_denom, digits=1), round(100*all_bronze/all_denom, digits=1), all_gold, all_green, all_hybrid, all_bronze, all_denom,
            umc_selected,      round(100*umc_gold/umc_denom, digits=1), round(100*umc_green/umc_denom, digits=1), round(100*umc_hybrid/umc_denom, digits=1), round(100*umc_na/umc_denom, digits=1), round(100*umc_closed/umc_denom, digits=1), round(100*umc_bronze/umc_denom, digits=1), umc_gold, umc_green, umc_hybrid, umc_bronze, umc_denom
        )

        plot_data$x_label <- fct_relevel(plot_data$x_label, "All", after= Inf)

        ylabel <- "Percentage Open Access (%)"

        
    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~gold,
        name = "Gold",
        text = ~paste0(gold_numer, " out of ", sum),
        textposition = "none",
        hovertemplate = paste0('<b>%{x} : </b>', '%{y}%',
                               ', %{text}'),
        type = 'bar',
        marker = list(
            color = color_palette[3],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) |>
        add_trace(
            y = ~green, name = "Green",
            text = ~paste0(green_numer, " out of ", sum),
            marker = list(
                color = color_palette[8],
                line = list(color = 'rgb(0,0,0)',
                            width = 1.5)
            )
        ) |>
        add_trace(
            y = ~hybrid, name = "Hybrid",
            text = ~paste0(hybrid_numer, " out of ", sum),
            marker = list(
                color = color_palette[10],
                line = list(color = 'rgb(0,0,0)',
                            width = 1.5)
            )
        ) |>
        add_trace(y = ~bronze, name = "Bronze",
                text = ~paste0(bronze_numer, " out of ", sum),
                marker = list(
                  color = "#cf9188",
                  line = list(color = 'rgb(0,0,0)',
                              width = 1.5)),
                visible = "legendonly"
      ) |>
      
        layout(
            barmode = 'stack',
            xaxis = list(
                title = '<b>UMC</b>',
                spikemode = 'marker',
                spikethickness = 0
            ),
            yaxis = list(
                title = paste('<b>', ylabel, '</b>'),
                range = c(0, 115)
            ),
            hovermode = "x unified",
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
   
        
    }

}
