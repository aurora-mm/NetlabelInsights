# Treetrunk Records version of Netlabel Insights

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(cowplot)
library(internetarchive)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Netlabel Insights: Treetrunk Records"),
  hr(),
  fluidRow(
    column(
      width = 3,
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: grey}")),
      sliderInput("num_artists",
        "Number of Artists:",
        min = 10,
        max = 14,
        value = 14
      ),
      actionButton("load_data", "Load/Refresh Data"),
      hr(),
      h6("The Netlabel Insights dashboard for Treetrunk Records provides an overview of download data from the Internet Archive across three historical periods. It allows users to explore the distribution of artist downloads and generate a hypothetical compilation using stratified sampling."),
      h6("The dashboard displays data on the artists with the highest mean download counts over specific time periods in the label’s history. It also shows the distribution of download counts for albums during these periods, as well as the trend in download counts with each subsequent album release."),
      h6("The Compilation Maker generates a list of 10-14 artists for a hypothetical Treetrunk Records compilation, based on historical data and stratified sampling. The compilation would feature artists whose albums have received fewer than 1000 downloads, between 1000 and 10000 downloads, and more than 10000 downloads. This ensures a mix of emerging and established artists from all time periods. Adjusting the Number of Artists slider generates a new compilation suggestion, drawn from Treetrunk Records artist pool."),
    ),
    column(
      width = 9,
      tabsetPanel(
        tabPanel("Period 1 (2005-2015)", plotOutput("plot_period1", height = "500px")),
        tabPanel("Period 2 (2015-2017)", plotOutput("plot_period2", height = "500px")),
        tabPanel("Period 3 (2019-Present)", plotOutput("plot_period3", height = "500px")),
        tabPanel("Compilation Maker", tableOutput("compilation"))
      )
    )
  )
)

server <- function(input, output, session) {
  treetrunk_data <- reactiveVal(NULL)
  cache_file <- "cache.rds"

  # Function for loading data
  load_treetrunk_data <- function() {
    withProgress(message = "Loading Data", value = 0, {
      incProgress(0.1, detail = "Checking cache...")

      # Load cached data if it exists
      cached_data <- if (file.exists(cache_file)) readRDS(cache_file) else tibble()
      cached_identifiers <- cached_data$Identifier %||% character(0)

      # Fetch new data
      incProgress(0.3, detail = "Fetching data from IA...")

      query <- list(
        collection = "treetrunk",
        date = "2024 TO 2099"
      )
      search_results <- ia_search(query, num_results = 9999)
      item_list <- search_results %>% ia_get_items()

      # Create data frame
      data <- tibble(
        Identifier = map_chr(item_list, ~ .x$metadata$identifier[[1]] %||% NA_character_),
        Title = map_chr(item_list, ~ .x$metadata$title[[1]] %||% NA_character_),
        Downloads = map_dbl(item_list, ~ as.numeric(.x$item$downloads %||% 0)),
        Date = map_chr(item_list, ~ .x$metadata$publicdate[[1]] %||% NA_character_) %>% as.Date(),
        Creator = map_chr(item_list, ~ {
          creator <- .x$metadata$creator[[1]] %||% NA_character_
          title <- .x$metadata$title[[1]] %||% NA_character_

          # If creator is available, use it
          if (!is.na(creator)) {
            return(str_to_lower(trimws(creator)))
          }

          # If creator is missing, derive from title
          if (!is.na(title) && str_detect(title, "-")) {
            parts <- str_split(title, "-")[[1]]
            if (length(parts) > 0) {
              return(str_to_lower(trimws(parts[1])))
            }
          }

          # Fallback if both creator and title are insufficient
          return("unknown")
        })
      )

      # Filter new items
      incProgress(0.3, detail = "Filtering new items...")
      new_data <- data %>% filter(!Identifier %in% cached_identifiers)
      combined_data <- bind_rows(cached_data, new_data)

      # Save to cache
      incProgress(0.2, detail = "Updating cache...")
      saveRDS(combined_data, cache_file)

      # Return combined data
      combined_data
    })

    # Define lookup table
    artist_lookup <- tribble(
      ~alias, ~canonical,
      "900piesek", "900 piesek",
      "900 piesak", "900 piesek",
      "ed powley", "edward j. powley",
      "evgenij v. kharitonov", "eugenekha",
      "christopher alvarado with mystified", "mystified with christopher alvarado",
      "the ghost between the strings, mystified", "the ghost between the strings and mystified",
      "thomas pak", "thomas park",
      "thomas park (with brian eno)", "thomas park with brian eno",
      "thomas park, mystified", "mystified"
    )
    lookup_vector <- deframe(artist_lookup)

    # Apply lookup table
    combined_data <- combined_data %>%
      mutate(
        Creator = ifelse(Creator %in% names(lookup_vector), lookup_vector[Creator], Creator)
      )

    # Add year, period and popularity
    combined_data <- combined_data %>%
      mutate(
        Year = year(Date),
        Period = case_when(
          Year >= 2019 ~ 3,
          Year > 2014 ~ 2,
          TRUE ~ 1
        ),
        Popularity = case_when(
          Downloads < 1000 ~ "low",
          Downloads < 10000 ~ "medium",
          TRUE ~ "high"
        )
      ) %>%
      filter(!is.na(Date))

    return(combined_data)
  }

  observeEvent(input$load_data, {
    treetrunk_data(load_treetrunk_data())
  })

  # Generate plots
  generate_period_plot <- function(period, data, color) {
    period_data <- data %>% filter(Period == period)
    top_artists <- period_data %>%
      group_by(Creator) %>%
      summarize(MeanDownloads = mean(Downloads, na.rm = TRUE)) %>%
      arrange(desc(MeanDownloads)) %>%
      slice_head(n = input$num_artists)
    p1 <- ggplot(top_artists, aes(x = reorder(Creator, MeanDownloads), y = MeanDownloads)) +
      geom_col(fill = color) +
      coord_flip() +
      labs(x = NULL, y = "Mean Downloads") +
      theme_minimal()
    p2 <- ggplot(period_data, aes(x = Downloads)) +
      geom_histogram(bins = 30, fill = color, alpha = 0.7) +
      labs(x = "Downloads", y = "Count") +
      theme_minimal()
    p3 <- ggplot(period_data, aes(x = Date, y = Downloads)) +
      geom_line(color = "black", alpha = 0.7) +
      geom_smooth(color = color, fill = color, method = "loess", se = TRUE) +
      labs(x = "Year", y = "Downloads") +
      theme_minimal()
    plot_grid(
      plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 1)),
      p3,
      ncol = 1, rel_heights = c(1, 0.8)
    )
  }

  # Render plots
  output$plot_period1 <- renderPlot({
    req(treetrunk_data())
    generate_period_plot(1, treetrunk_data(), "deepskyblue4")
  })
  output$plot_period2 <- renderPlot({
    req(treetrunk_data())
    generate_period_plot(2, treetrunk_data(), "thistle4")
  })
  output$plot_period3 <- renderPlot({
    req(treetrunk_data())
    generate_period_plot(3, treetrunk_data(), "wheat4")
  })

  # Generate compilation
  output$compilation <- renderTable({
    req(treetrunk_data())
    sampled_artists <- treetrunk_data() %>%
      group_by(Period, Popularity) %>%
      slice_sample(n = 4) %>%
      ungroup() %>%
      distinct(Creator) %>%
      slice_head(n = input$num_artists)
    sampled_artists %>% rename(`Artist Compilation` = Creator)
  })
}

shinyApp(ui = ui, server = server)
