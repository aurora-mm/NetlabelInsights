library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(cowplot)

# Define UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$style(HTML(
    paste0(
      ".shiny-input-container {background-color: #f5f5f5; border: 1px solid #e3e3e3;",
      "padding-left: 10px; padding-right: 10px; border-radius: 3px;}"
    )
  )),

  # Application title
  titlePanel("Treetrunk Records Insights"),
  hr(),
  fluidRow(
    column(
      width = 2,
      sliderInput("tracks",
        "Number of artists:",
        min = 10,
        max = 14,
        value = 14
      ),
      hr(),
      h6("The Treetrunk Records Insights dashboard presents data on artists with the highest mean download counts during specific time periods in the history of Treetrunk Records. It also shows the distribution of download counts for albums during these time periods, as well as how download counts change with each successive album release over time. The dataset includes all albums released on Treetrunk Records through June 6th, 2022."),
      h6("The Compilation Maker provides a selection of 10-14 artists for a hypothetical Treetrunk Records compilation based on historical data and stratified sampling. The proposed compilation would include artists  whose albums have received less than 1000 downloads, between 1000 and 10000 downloads, and more than 10000 downloads, respectively. As a result, both emerging and established artists from all three time periods would be provided with an opportunity to be featured on the compilation. When the slider position (Number of artists) is changed, a new suggestion is formed based on the existing artist pool of Treetrunk Records."),
    ),
    column(
      width = 10,
      tabsetPanel(
        tabPanel("Period 1 (2005-2015)", plotOutput("plot_1", height = "550px")),
        tabPanel("Period 2 (2015-2017)", plotOutput("plot_2", height = "550px")),
        tabPanel("Period 3 (2019-2022)", plotOutput("plot_3", height = "550px")),
        tabPanel("Compilation Maker", tableOutput("tracklist"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  theme_set(theme_bw())
  theme_update(
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 12),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(angle = 0, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.caption = element_text(hjust = -0.25, size = 10),
    axis.text.x.bottom = element_text(size = 12)
  )

  # Load and transform data
  treetrunk_df <- read.csv(file = "treetrunk_data.csv")
  treetrunk_df <- cbind(
    treetrunk_df$Creator, treetrunk_df$Downloads,
    treetrunk_df$Date
  ) %>% as.data.frame()
  colnames(treetrunk_df) <- c("Artist", "Downloads", "Date")

  treetrunk_ts <- treetrunk_df

  treetrunk_df$Downloads <- treetrunk_df$Downloads %>% as.numeric()
  treetrunk_df$Date <- treetrunk_df$Date %>%
    as.Date() %>%
    year()

  treetrunk_ts$Downloads <- treetrunk_ts$Downloads %>% as.numeric()
  treetrunk_ts$Date <- treetrunk_ts$Date %>% as.Date()

  treetrunk_df$Popular <- ifelse(treetrunk_df$Downloads < 1000, "Low", "Medium")
  treetrunk_df$Popular[treetrunk_df$Downloads > 10000] <- "High"

  treetrunk_df$Period <- ifelse(treetrunk_df$Date > 2014, 2, 1)
  treetrunk_df$Period[treetrunk_df$Date >= 2019] <- 3

  treetrunk_ts$Period <- ifelse(year(treetrunk_ts$Date) > 2014, 2, 1)
  treetrunk_ts$Period[year(treetrunk_ts$Date) >= 2019] <- 3
  treetrunk_ts <- treetrunk_ts %>% arrange(Date)

  # Show graphs
  output$plot_1 <- renderPlot({
    plot1_data <- treetrunk_df %>%
      filter(Period == 1) %>%
      arrange(desc(Downloads))
    plot1_data <- plot1_data %>%
      group_by(Artist) %>%
      summarize(mean = mean(Downloads)) %>%
      arrange(desc(mean))

    plot1_data$Artist <- factor(plot1_data$Artist,
      levels = plot1_data$Artist[order(plot1_data$mean)]
    )

    p1 <- ggplot(data = head(plot1_data, n = input$tracks), aes(x = Artist, y = mean)) +
      geom_bar(stat = "identity", fill = "deepskyblue4") +
      coord_flip() +
      xlab("") +
      ylab("Downloads")

    p2 <- ggplot(data = treetrunk_df %>% filter(Period == 1), aes(Downloads)) +
      geom_histogram(bins = 200, fill = "deepskyblue4") +
      xlab("Downloads") +
      ylab("Count") +
      scale_y_continuous(limits = c(0, 15))

    p3 <- ggplot(data = treetrunk_ts %>% filter(Period == 1), aes(x = Date, y = Downloads)) +
      geom_line(color = "black") +
      stat_smooth(
        color = "deepskyblue4", fill = "deepskyblue",
        method = "loess"
      ) +
      xlab("")

    p12 <- plot_grid(p1, p2, nrow = 1)

    plot_grid(p12, p3, ncol = 1)
  })

  # Show graphs
  output$plot_2 <- renderPlot({
    plot2_data <- treetrunk_df %>%
      filter(Period == 2) %>%
      arrange(desc(Downloads))
    plot2_data <- plot2_data %>%
      group_by(Artist) %>%
      summarize(mean = mean(Downloads)) %>%
      arrange(desc(mean))

    plot2_data$Artist <- factor(plot2_data$Artist,
      levels = plot2_data$Artist[order(plot2_data$mean)]
    )

    p1 <- ggplot(data = head(plot2_data, n = input$tracks), aes(x = Artist, y = mean)) +
      geom_bar(stat = "identity", fill = "thistle4") +
      coord_flip() +
      xlab("") +
      ylab("Downloads")

    p2 <- ggplot(data = treetrunk_df %>% filter(Period == 2), aes(Downloads)) +
      geom_histogram(bins = 200, fill = "thistle4") +
      xlab("Downloads") +
      ylab("Count") +
      scale_y_continuous(limits = c(0, 11))

    p3 <- ggplot(data = treetrunk_ts %>% filter(Period == 2), aes(x = Date, y = Downloads)) +
      geom_line(color = "black") +
      stat_smooth(
        color = "thistle4", fill = "thistle",
        method = "loess"
      ) +
      xlab("")

    p12 <- plot_grid(p1, p2, nrow = 1)

    plot_grid(p12, p3, ncol = 1)
  })

  # Show graphs
  output$plot_3 <- renderPlot({
    plot3_data <- treetrunk_df %>%
      filter(Period == 3) %>%
      arrange(desc(Downloads))
    plot3_data <- plot3_data %>%
      group_by(Artist) %>%
      summarize(mean = mean(Downloads)) %>%
      arrange(desc(mean))

    plot3_data$Artist <- factor(plot3_data$Artist,
      levels = plot3_data$Artist[order(plot3_data$mean)]
    )

    p1 <- ggplot(data = head(plot3_data, n = input$tracks), aes(x = Artist, y = mean)) +
      geom_bar(stat = "identity", fill = "wheat4") +
      coord_flip() +
      xlab("") +
      ylab("Downloads")

    p2 <- ggplot(data = treetrunk_df %>% filter(Period == 3), aes(Downloads)) +
      geom_histogram(bins = 200, fill = "wheat4") +
      xlab("Downloads") +
      ylab("Count") +
      scale_y_continuous(limits = c(0, 21))

    p3 <- ggplot(data = treetrunk_ts %>% filter(Period == 3), aes(x = Date, y = Downloads)) +
      geom_line(color = "black") +
      stat_smooth(
        color = "wheat4", fill = "wheat",
        method = "loess"
      ) +
      xlab("")

    p12 <- plot_grid(p1, p2, nrow = 1)

    plot_grid(p12, p3, ncol = 1)
  })

  # Sample and show the suggestion list
  output$tracklist <- renderTable({
    result <- treetrunk_df %>%
      group_by(Period, Popular) %>%
      slice_sample(n = 4, replace = FALSE)
    result <- result[sample(1:nrow(result)), ]
    tracklist <- head(unique(result$Artist), n = input$tracks) %>% as.data.frame()
    colnames(tracklist) <- ""
    tracklist
  })
}

# Run the application
shinyApp(ui = ui, server = server)
