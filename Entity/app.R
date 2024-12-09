# Entity version of Netlabel Insights

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(cowplot)
library(internetarchive)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Netlabel Insights: Entity"),
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
      h6("The Netlabel Insights dashboard for Entity provides an overview of download data from the Internet Archive. It allows users to explore the distribution of artist downloads and generate a hypothetical compilation using stratified sampling."),
      h6("The dashboard displays data on the artists with the highest mean download counts in the label's history. It also shows the distribution of download counts for albums, as well as the trend in download counts with each subsequent album release."),
      h6("The Compilation Maker generates a list of 10-14 artists for a hypothetical Entity compilation, based on historical data and stratified sampling. The compilation would feature artists whose albums have received fewer than 2000 downloads, between 2000 and 7000 downloads, and more than 7000 downloads. This ensures a mix of emerging and established artists. Adjusting the Number of Artists slider generates a new compilation suggestion, drawn from Entity artist pool."),
    ),
    column(
      width = 9,
      tabsetPanel(
        tabPanel("All-time (2004-Present)", plotOutput("plot_period1", height = "500px")),
        tabPanel("Compilation Maker", tableOutput("compilation"))
      )
    )
  )
)

server <- function(input, output, session) {
  entity_data <- reactiveVal(NULL)
  cache_file <- "cache.rds"

  # Function for loading data
  load_entity_data <- function() {
    withProgress(message = "Loading Data", value = 0, {
      incProgress(0.1, detail = "Checking cache...")

      # Load cached data if it exists
      cached_data <- if (file.exists(cache_file)) readRDS(cache_file) else tibble()
      cached_identifiers <- cached_data$Identifier %||% character(0)

      # Fetch new data
      incProgress(0.3, detail = "Fetching data from IA...")

      query <- list(
        collection = "entity",
        date = "2017 TO 2099"
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
      "danny kreutzfeldt, erratic, kaebin yield, ps, tirriddiliu", "xe phalanx",
      "element.act, kaebin yield, subskan, tirriddiliu, undacova, vteb, xanopticon", "xe phalanx",
      "9e phalanx", "xe phalanx",
      "dotkràz, m__, ps", "dotkràz",
      "chango feo, jen lilith, bryan glosemeyer", "chango feo",
      "elín", "elin",
      "bogdan dullsky, idle sunder", "idle sunder",
      "idle sunder, bogdan dullsky", "idle sunder",
      "fp, fabrice planquette", "fp",
      "nick nenov", "n.nenov",
      "v/a", "various artists",
      "xe phalanx, acta, chango feo, ellende, erratic, kaebin yield, no xivic, plagasul, sig~int", "xe phalanx",
      "sedarka, satka, atomhead", "sedarka",
      "kaebin yield, sedarka, yvan hinge, ronny ragtroll, atomhead", "kaebin yield",
      "a.s., repeater, rym hakiki, ***, jsld", "a.s."
      )
    lookup_vector <- deframe(artist_lookup)

    # Apply lookup table
    combined_data <- combined_data %>%
      mutate(
        Creator = ifelse(Creator %in% names(lookup_vector), lookup_vector[Creator], Creator)
      )

    # Add year, period (just one in this case) and popularity
    combined_data <- combined_data %>%
      mutate(
        Year = year(Date),
        Period = case_when(
          TRUE ~ 1
        ),
        Popularity = case_when(
          Downloads < 2000 ~ "low",
          Downloads < 7000 ~ "medium",
          TRUE ~ "high"
        )
      ) %>%
      filter(!is.na(Date))

    return(combined_data)
  }

  observeEvent(input$load_data, {
    entity_data(load_entity_data())
  })

  # Generate plots
  generate_period_plot <- function(period, data, color) {
    period_data <- data %>% filter(Period == period)
    top_artists <- period_data %>%
      group_by(Creator) %>%
      summarize(MeanDownloads = mean(Downloads, na.rm = TRUE)) %>%
      arrange(desc(MeanDownloads)) %>%
      slice_head(n = input$num_artists)
    p1 <- ggplot(top_artists, aes(x = reorder(enc2utf8(Creator), MeanDownloads), y = MeanDownloads)) +
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
    req(entity_data())
    generate_period_plot(1, entity_data(), "gray28")
  })

  # Generate compilation
  output$compilation <- renderTable({
    req(entity_data())
    sampled_artists <- entity_data() %>%
      filter(Creator != "various artists") %>% 
      group_by(Period, Popularity) %>%
      slice_sample(n = 4) %>%
      ungroup() %>%
      distinct(Creator) %>%
      slice_head(n = input$num_artists)
    sampled_artists %>% rename(`Artist Compilation` = Creator)
  })
}

shinyApp(ui = ui, server = server)
