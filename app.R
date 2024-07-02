# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(cluster)

# Define UI
ui <- fluidPage(
  titlePanel("Country GDP Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(".csv")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histograms", 
                 uiOutput("country_selector"),
                 plotOutput("gdp_histogram"),
                 plotOutput("gdp_line_chart")),
        tabPanel("K-Means Clustering",
                 plotOutput("kmeans_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read data from CSV file
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE)
    df$Country.Name <- as.character(df$Country.Name)  # Convert country names to character type
    return(df)
  })
  
  # Dynamically create country selector dropdown
  output$country_selector <- renderUI({
    req(data())
    selectInput("country", "Select Country", choices = unique(data()$Country.Name))
  })
  
  # Render histogram
  output$gdp_histogram <- renderPlot({
    req(data())
    req(input$country)
    country_data <- data() %>% filter(Country.Name == input$country)
    years <- substr(names(country_data)[3:ncol(country_data)], 5, 8)
    gdp_values <- unlist(country_data[,3:ncol(country_data)])
    df <- data.frame(Year = factor(years), GDP = as.numeric(gdp_values))
    ggplot(df, aes(x = Year, y = GDP, fill = Year)) +
      geom_bar(stat = "identity") +
      labs(x = "Year", y = "GDP", title = paste("GDP Histogram for", input$country)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render line chart
  output$gdp_line_chart <- renderPlot({
    req(data())
    req(input$country)
    country_data <- data() %>% filter(Country.Name == input$country)
    years <- substr(names(country_data)[3:ncol(country_data)], 5, 8)
    gdp_values <- unlist(country_data[,3:ncol(country_data)])
    df <- data.frame(Year = factor(years), GDP = as.numeric(gdp_values))
    ggplot(df, aes(x = Year, y = GDP, group = 1)) +
      geom_line(color = "blue") +
      labs(x = "Year", y = "GDP", title = paste("GDP Line Chart for", input$country)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render k-means clustering
  output$kmeans_plot <- renderPlot({
    req(data())
    df <- data()
    df <- df[,3:ncol(df)]
    df <- na.omit(df)
    kmeans_result <- kmeans(df, centers = 3)
    clusplot(df, kmeans_result$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0,
             main = paste("K-Means Clustering"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
