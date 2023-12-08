# Prospective registration
plot_clinicaltrials_prereg <- function (dataset, color_palette) {
  
  dataset <- dataset |>
    filter(!is.na(start_date))
  
  years <- seq(from=min(dataset$start_year, na.rm=TRUE), to=max(dataset$start_year, na.rm=TRUE))
  
  plot_data <- tribble(
    ~year, ~percentage, ~mouseover
  )
  
  for (current_year in years) {
    
    numer_for_year <- dataset |>
      filter(
        start_year == current_year,
        is_prospective == TRUE
      ) |>
      nrow()
    
    denom_for_year <- dataset |>
      filter(
        start_year == current_year
      ) |>
      nrow()
    
    percentage_for_year <- round(100*numer_for_year/denom_for_year, digits=1)
    
    plot_data <- plot_data |>
      bind_rows(
        tribble(
          ~year, ~percentage, ~mouseover,
          current_year, percentage_for_year, paste0(numer_for_year, "/", denom_for_year)
        )
      )
    
  }
  
  plot_ly(
    plot_data,
    x = ~year,
    y = ~percentage,
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
    layout(
      yaxis = list(
        title = '<b>Percentage of trials (%)</b>',
        range = c(0, 105)
      ),
      xaxis = list(
        title = '<b>Start year</b>',
        dtick = 1
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9],
      legend = list(xanchor= "right")
    )
}

## TRN

plot_clinicaltrials_trn <- function (dataset, color_palette) {
  
  umc <- "All"
  
  all_numer_abs <- dataset |>
    filter(has_trn_abstract == TRUE) |>
    nrow()
  abs_denom <- dataset |>
    filter(
      has_publication == TRUE,
      publication_type == "journal publication",
      has_pubmed == TRUE
    ) |>
    nrow()
  
  all_numer_ft <- dataset |>
    filter(has_trn_ft == TRUE) |>
    nrow()
  ft_denom <- dataset |>
    filter(
      has_publication == TRUE,
      publication_type == "journal publication",
      has_ft == TRUE
    ) |>
    nrow()
  
  plot_data <- tribble(
    ~x_label, ~colour, ~percentage, ~mouseover,
    "All", "In abstract", round(100*all_numer_abs/abs_denom, digits=1), paste0(all_numer_abs, "/", abs_denom),
    "All", "In full text", round(100*all_numer_ft/ft_denom, digits=1), paste0(all_numer_ft, "/", ft_denom)
  )
  
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

## Linkage
plot_linkage <- function (dataset, color_palette) {
  # plot_linkage <- function (dataset, color_palette, chosenregistry) {
  dataset <- dataset |>
    filter(has_publication == TRUE,
           publication_type == "journal publication",
           has_pubmed == TRUE | ! is.na (doi),
           ! is.na(primary_completion_year))
  
  years <- seq(from=min(dataset$primary_completion_year), to=max(dataset$primary_completion_year))
  
  plot_data <- tribble(
    ~year, ~percentage, ~mouseover
  )
  
  for (current_year in years) {
    
    numer_for_year <- dataset |>
      filter(has_reg_pub_link == TRUE) |>
      filter(primary_completion_year == current_year) |>
      nrow()
    
    denom_for_year <- dataset |>
      filter(primary_completion_year == current_year) |>
      nrow()
    
    percentage_for_year <- round(100*numer_for_year/denom_for_year, digits=1)
    
    plot_data <- plot_data |>
      bind_rows(
        tribble(
          ~year, ~percentage, ~mouseover,
          current_year, percentage_for_year, paste0(numer_for_year, "/", denom_for_year)
        )
      )
  }
  
  ylabel <- "Trials with publication (%)"
  
  plot_ly(
    plot_data,
    x = ~year,
    y = ~percentage,
    name = 'All',
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
    layout(
      xaxis = list(
        title = '<b>Completion year</b>',
        dtick = 1
      ),
      yaxis = list(
        title = paste('<b>', ylabel, '</b>'),
        range = c(0, 105)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9]
    )
  
}

## Summary results
plot_clinicaltrials_sumres <- function (dataset, color_palette) {
  
  # dataset <- dataset |> 
  #   filter(!is.na(primary_completion_year))
  
  min_year <- dataset$primary_completion_year |>
    min()
  
  max_year <- dataset$primary_completion_year |>
    max()
  
  plot_data <- tribble(
    ~date, ~percent_reported
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
    
    plot_data <- plot_data |>
      bind_rows(
        tribble(
          ~date, ~percent_reported, ~mouseover,
          currentyear, round(100*currentyear_numer/currentyear_denom, digits=1),
          paste0(currentyear_numer, "/", currentyear_denom)
        )
      )
    
  }
  
  # }
  
  plot_ly(
    plot_data,
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
    layout(
      xaxis = list(
        title = '<b>Date</b>',
        dtick = 1
      ),
      yaxis = list(
        title = '<b>Percentage of trials (%)</b>',
        range = c(0, 105)
      ),
      paper_bgcolor = color_palette[9],
      plot_bgcolor = color_palette[9],
      legend = list(xanchor= "right")
    )
  
}

# Timely publication within 2 years
plot_clinicaltrials_timpub_2a <- function (dataset, rt, color_palette) {
  
  if (rt != "Summary results or manuscript publication") {
    
    if (rt == "Summary results only") {
      dataset$published_2a <- dataset$is_summary_results_2y
    }
    
    if (rt == "Manuscript publication only") {
      dataset$published_2a <- dataset$is_publication_2y
    }
    
  } else {
    
    dataset$published_2a <- dataset$is_summary_results_2y | dataset$is_publication_2y
  }
  
  umc <- "All"
  
  years <- seq(from=min(dataset$primary_completion_year), to=max(dataset$primary_completion_year))
  
  all_denom <- dataset |>
    nrow()
  
  all_numer <- dataset |>
    filter(published_2a) |>
    nrow()
  
  plot_data <- tribble(
    ~year, ~all_percentage, ~mouseover
  )
  
  for (current_year in years) {
    
    all_numer <-  dataset |>
      filter(
        primary_completion_year == current_year,
        published_2a
      ) |>
      nrow()
    
    all_denom <-  dataset |>
      filter(
        primary_completion_year == current_year
      ) |>
      nrow()
    all_percentage <- round(100*all_numer/all_denom, digits=1)
    
    plot_data <- plot_data |>
      bind_rows(
        tribble(
          ~year, ~all_percentage, ~mouseover,
          current_year, all_percentage, paste0(all_numer, "/", all_denom)
        )
      )
    
  }
  
  plot_ly(
    plot_data,
    x = ~year,
    y = ~all_percentage,
    name = umc,
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
      legend = list(xanchor= "right")
    )
  
}

# Timely publication within 5 years
plot_clinicaltrials_timpub_5a <- function (dataset, rt, color_palette) {
  
  if (rt != "Summary results or manuscript publication") {
    
    if (rt == "Summary results only") {
      
      dataset$published_5a <- dataset$is_summary_results_5y
    }
    
    if (rt == "Manuscript publication only") {
      
      dataset$published_5a <- dataset$is_publication_5y
    }
    
  } else {
    
    dataset$published_5a <- dataset$is_summary_results_5y | dataset$is_publication_5y
  }
  
  umc <- "All"
  
  years <- seq(from=min(dataset$primary_completion_year), to=max(dataset$primary_completion_year))
  
  all_denom <- dataset |>
    nrow()
  
  all_numer <- dataset |>
    filter(published_5a) |>
    nrow()
  
  plot_data <- tribble(
    ~year, ~all_percentage, ~mouseover
  )
  
  for (current_year in years) {
    
    all_numer <-  dataset |>
      filter(
        primary_completion_year == current_year,
        published_5a
      ) |>
      nrow()
    
    all_denom <-  dataset |>
      filter(
        primary_completion_year == current_year
      ) |>
      nrow()
    all_percentage <- round(100*all_numer/all_denom, digits=1)
    
    manuscript_denom <- dataset |>
      filter(
        primary_completion_year == current_year 
      ) |>
      nrow()
    
    if (rt == "Summary results only" |
        manuscript_denom > 5) { ## To remove years where there's
      ## too few data points
      
      plot_data <- plot_data |>
        bind_rows(
          tribble(
            ~year, ~all_percentage, ~mouseover,
            current_year, all_percentage, paste0(all_numer, "/", all_denom)
          )
        )
    }
    
  }
  
  plot_ly(
    plot_data,
    x = ~year,
    y = ~all_percentage,
    name = umc,
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
      legend = list(xanchor= "right")
    )
  
}

plot_opensci_oa <- function (dataset, absnum, color_palette) {
  
  umc <- "All"
  
  ## Calculate the numerators and the denominator for the
  ## "all" bars
  
  dataset <- dataset |>
    filter(
      has_publication == TRUE,
      publication_type == "journal publication",
      !is.na(doi),
      ! is.na (publication_date_unpaywall)
    ) |>
    distinct(doi, .keep_all=TRUE)
  
  dataset$oa_year <- dataset$publication_date_unpaywall |>
    format("%Y")
  
  if (absnum == "Show absolute numbers") {
    
    plot_data <- tribble(
      ~x_label, ~gold,    ~green,    ~hybrid,    ~na,    ~closed,    ~bronze
    )
    
    upperlimit <- 0
    
    for (year in unique(dataset$oa_year)) {
      
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
      
      if (year_denom > 20) {
        plot_data <- plot_data |>
          bind_rows(
            tribble(
              ~x_label, ~gold,    ~green,    ~hybrid,    ~na,    ~closed,    ~bronze,
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
      ~x_label, ~gold, ~green, ~hybrid, ~sum
    )
    
    for (year in unique(dataset$oa_year)) {
      
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
      
      if (year_denom > 20) {                
        plot_data <- plot_data |>
          bind_rows(
            tribble(
              ~x_label, ~gold, ~gold_num,                         ~green, ~green_num,                        ~hybrid, ~hybrid_num,        ~bronze, ~bronze_num, ~sum,
              year, round(100*gold_num/year_denom), gold_num, round(100*green_num/year_denom), green_num, round(100*hybrid_num/year_denom), hybrid_num, round(100*bronze_num/year_denom), bronze_num, year_denom
            )
          )
      }
    }
    
    ylabel <- "Percentage Open Access (%)"
    
    plot_ly(
      plot_data,
      x = ~x_label,
      y = ~gold,
      name = "Gold",
      text = ~paste0(gold_num, " out of ", sum),
      textposition = "none",
      hoverinfo = "text",
      hovertemplate = paste0('%{y}%, %{text}'),
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
        text = ~paste0(green_num, " out of ", sum),
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
        text = ~paste0(hybrid_num, " out of ", sum),
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
        text = ~paste0(bronze_num, " out of ", sum),
        marker = list(
          color = "#cf9188",
          line = list(
            color = 'rgb(0,0,0)',
            width = 1.5
          )),
        visible = "legendonly"
      ) |>
      
      layout(
        barmode = 'stack',
        xaxis = list(
          title = '<b>Year of publication</b>',
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
