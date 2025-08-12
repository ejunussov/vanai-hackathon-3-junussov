library(shiny)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(viridis)
library(DT)
library(plotly)
library(shinydashboard)

# Load and prepare data
bc_survey <- read_csv("Hackathon round 3 with demos[48].csv")

# Define key questions for filtering
key_questions <- c(
  "Q1_Experience_with_AI",
  "Q2_Experience_with_AI_animal", 
  "AgeRollup_Broad",
  "Gender",
  "Education",
  "Income",
  "Location"
)

# Function to calculate exact intersections
calculate_exact_intersections <- function(data, filters) {
  if (length(filters) == 0) {
    return(list(
      count = nrow(data),
      percentage = 100,
      respondents = nrow(data)
    ))
  }
  
  # Apply all filters
  filtered_data <- data
  for (filter in filters) {
    filtered_data <- filtered_data %>%
      filter(!!sym(filter$question) == filter$value)
  }
  
  count <- nrow(filtered_data)
  percentage <- round(count / nrow(data) * 100, 1)
  
  return(list(
    count = count,
    percentage = percentage,
    respondents = count
  ))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "BC-AI Cohort Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Intersection Analysis", tabName = "intersection", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    # Filter Panel
    div(style = "padding: 15px;",
      h4("Filters"),
      
      # Q1: Experience with AI
      selectInput("q1_filter", "Experience with AI:",
                  choices = c("All", unique(bc_survey$Q1_Experience_with_AI)),
                  selected = "All"),
      
      # Q2: Animal Personality
      selectInput("q2_filter", "AI Animal Personality:",
                  choices = c("All", unique(bc_survey$Q2_Experience_with_AI_animal)),
                  selected = "All"),
      
      # Age
      selectInput("age_filter", "Age Group:",
                  choices = c("All", unique(bc_survey$AgeRollup_Broad)),
                  selected = "All"),
      
      # Gender
      selectInput("gender_filter", "Gender:",
                  choices = c("All", unique(bc_survey$Gender)),
                  selected = "All"),
      
      # Education
      selectInput("education_filter", "Education:",
                  choices = c("All", unique(bc_survey$Education)),
                  selected = "All"),
      
      # Income
      selectInput("income_filter", "Income:",
                  choices = c("All", unique(bc_survey$Income)),
                  selected = "All"),
      
      # Location
      selectInput("location_filter", "Location:",
                  choices = c("All", unique(bc_survey$Location)),
                  selected = "All"),
      
      # Clear filters button
      actionButton("clear_filters", "Clear All Filters", 
                   class = "btn-warning", style = "width: 100%; margin-top: 10px;")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
        fluidRow(
          # Summary Cards
          valueBoxOutput("total_respondents", width = 3),
          valueBoxOutput("filtered_count", width = 3),
          valueBoxOutput("match_percentage", width = 3),
          valueBoxOutput("match_strength", width = 3)
        ),
        
        fluidRow(
          # Population Circle Visualization
          box(
            title = "Population Representation",
            plotlyOutput("population_circle", height = 400),
            width = 6
          ),
          
          # Filter Summary
          box(
            title = "Active Filters",
            verbatimTextOutput("filter_summary"),
            width = 6
          )
        ),
        
        fluidRow(
          # Q1 Distribution
          box(
            title = "Experience with AI Distribution",
            plotlyOutput("q1_chart", height = 300),
            width = 6
          ),
          
          # Q2 Distribution
          box(
            title = "AI Animal Personality Distribution",
            plotlyOutput("q2_chart", height = 300),
            width = 6
          )
        )
      ),
      
      # Intersection Analysis Tab
      tabItem(tabName = "intersection",
        fluidRow(
          box(
            title = "Question Pair Intersections",
            plotlyOutput("intersection_heatmap", height = 500),
            width = 12
          )
        ),
        
        fluidRow(
          box(
            title = "Top Intersection Combinations",
            DTOutput("intersection_table"),
            width = 12
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Survey Data",
            DTOutput("data_table"),
            width = 12
          )
        )
      ),
      
      # About Tab
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About BC-AI Cohort Explorer",
            width = 12,
            div(style = "padding: 20px;",
              h3("Project Overview"),
              p("The BC-AI Cohort Explorer is an interactive data visualization tool designed to explore intersections and alignments within survey response data from British Columbia residents regarding their attitudes and experiences with AI."),
              
              h3("Key Features"),
              tags$ul(
                tags$li("Interactive filtering across multiple demographic and attitudinal variables"),
                tags$li("Real-time intersection calculations with exact respondent counts"),
                tags$li("Multiple visualization types including population circles, heatmaps, and charts"),
                tags$li("Comprehensive data exploration capabilities")
              ),
              
              h3("Data Source"),
              p("Survey data from 1,001 British Columbia residents collected to understand AI perceptions, experiences, and attitudes across different demographic groups."),
              
              h3("Technical Stack"),
              p("Built with R Shiny, tidyverse, plotly, and shinydashboard for an interactive web experience.")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filters
  active_filters <- reactive({
    filters <- list()
    
    if (input$q1_filter != "All") {
      filters <- c(filters, list(list(question = "Q1_Experience_with_AI", value = input$q1_filter)))
    }
    if (input$q2_filter != "All") {
      filters <- c(filters, list(list(question = "Q2_Experience_with_AI_animal", value = input$q2_filter)))
    }
    if (input$age_filter != "All") {
      filters <- c(filters, list(list(question = "AgeRollup_Broad", value = input$age_filter)))
    }
    if (input$gender_filter != "All") {
      filters <- c(filters, list(list(question = "Gender", value = input$gender_filter)))
    }
    if (input$education_filter != "All") {
      filters <- c(filters, list(list(question = "Education", value = input$education_filter)))
    }
    if (input$income_filter != "All") {
      filters <- c(filters, list(list(question = "Income", value = input$income_filter)))
    }
    if (input$location_filter != "All") {
      filters <- c(filters, list(list(question = "Location", value = input$location_filter)))
    }
    
    filters
  })
  
  # Filtered data
  filtered_data <- reactive({
    filters <- active_filters()
    
    if (length(filters) == 0) {
      return(bc_survey)
    }
    
    filtered <- bc_survey
    for (filter in filters) {
      filtered <- filtered %>%
        filter(!!sym(filter$question) == filter$value)
    }
    
    filtered
  })
  
  # Intersection results
  intersection_results <- reactive({
    calculate_exact_intersections(bc_survey, active_filters())
  })
  
  # Clear filters
  observeEvent(input$clear_filters, {
    updateSelectInput(session, "q1_filter", selected = "All")
    updateSelectInput(session, "q2_filter", selected = "All")
    updateSelectInput(session, "age_filter", selected = "All")
    updateSelectInput(session, "gender_filter", selected = "All")
    updateSelectInput(session, "education_filter", selected = "All")
    updateSelectInput(session, "income_filter", selected = "All")
    updateSelectInput(session, "location_filter", selected = "All")
  })
  
  # Outputs
  
  # Summary cards
  output$total_respondents <- renderValueBox({
    valueBox(
      nrow(bc_survey),
      "Total Respondents",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$filtered_count <- renderValueBox({
    results <- intersection_results()
    valueBox(
      results$count,
      "Matching Respondents",
      icon = icon("filter"),
      color = "green"
    )
  })
  
  output$match_percentage <- renderValueBox({
    results <- intersection_results()
    valueBox(
      paste0(results$percentage, "%"),
      "Match Percentage",
      icon = icon("percent"),
      color = "yellow"
    )
  })
  
  output$match_strength <- renderValueBox({
    results <- intersection_results()
    strength <- if (results$percentage >= 20) "Strong" else if (results$percentage >= 10) "Good" else "Weak"
    color <- if (results$percentage >= 20) "green" else if (results$percentage >= 10) "yellow" else "red"
    
    valueBox(
      strength,
      "Match Strength",
      icon = icon("star"),
      color = color
    )
  })
  
  # Population circle visualization
  output$population_circle <- renderPlotly({
    results <- intersection_results()
    total <- nrow(bc_survey)
    matched <- results$count
    
    # Create circle data
    circle_data <- data.frame(
      group = c("Matching", "Other"),
      value = c(matched, total - matched),
      color = c("#2E8B57", "#E6E6E6")
    )
    
    fig <- plot_ly(circle_data, labels = ~group, values = ~value, type = 'pie',
                   marker = list(colors = ~color),
                   textinfo = 'label+percent',
                   hole = 0.6) %>%
      layout(
        title = paste0("Population Representation: ", matched, " out of ", total, " respondents"),
        showlegend = FALSE
      )
    
    fig
  })
  
  # Filter summary
  output$filter_summary <- renderPrint({
    filters <- active_filters()
    
    if (length(filters) == 0) {
      cat("No active filters\n")
      cat("Showing all 1,001 respondents")
    } else {
      cat("Active Filters:\n")
      cat("===============\n")
      for (filter in filters) {
        cat(paste0("• ", filter$question, ": ", filter$value, "\n"))
      }
      cat("\n")
      results <- intersection_results()
      cat(paste0("Results: ", results$count, " respondents (", results$percentage, "%)"))
    }
  })
  
  # Q1 Distribution chart
  output$q1_chart <- renderPlotly({
    q1_data <- filtered_data() %>%
      count(Q1_Experience_with_AI) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    fig <- plot_ly(q1_data, x = ~Q1_Experience_with_AI, y = ~n, type = 'bar',
                   marker = list(color = '#2E8B57')) %>%
      layout(
        title = "Experience with AI Distribution",
        xaxis = list(title = "Experience Level"),
        yaxis = list(title = "Count")
      )
    
    fig
  })
  
  # Q2 Distribution chart
  output$q2_chart <- renderPlotly({
    q2_data <- filtered_data() %>%
      count(Q2_Experience_with_AI_animal) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    fig <- plot_ly(q2_data, x = ~Q2_Experience_with_AI_animal, y = ~n, type = 'bar',
                   marker = list(color = '#4682B4')) %>%
      layout(
        title = "AI Animal Personality Distribution",
        xaxis = list(title = "Animal Type"),
        yaxis = list(title = "Count")
      )
    
    fig
  })
  
  # Intersection heatmap
  output$intersection_heatmap <- renderPlotly({
    # Create Q1 x Q2 intersection data
    intersection_data <- bc_survey %>%
      group_by(Q1_Experience_with_AI, Q2_Experience_with_AI_animal) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round(count / nrow(bc_survey) * 100, 1))
    
    # Pivot for heatmap
    heatmap_data <- intersection_data %>%
      select(Q1_Experience_with_AI, Q2_Experience_with_AI_animal, percentage) %>%
      pivot_wider(
        names_from = Q2_Experience_with_AI_animal,
        values_from = percentage,
        values_fill = 0
      )
    
    # Convert to matrix for heatmap
    matrix_data <- as.matrix(heatmap_data[,-1])
    rownames(matrix_data) <- heatmap_data$Q1_Experience_with_AI
    
    fig <- plot_ly(
      z = matrix_data,
      x = colnames(matrix_data),
      y = rownames(matrix_data),
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Q1 x Q2 Intersection Heatmap",
        xaxis = list(title = "AI Animal Personality"),
        yaxis = list(title = "Experience with AI")
      )
    
    fig
  })
  
  # Intersection table
  output$intersection_table <- renderDT({
    # Create comprehensive intersection data
    intersection_data <- bc_survey %>%
      group_by(Q1_Experience_with_AI, Q2_Experience_with_AI_animal) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(percentage = round(count / nrow(bc_survey) * 100, 1)) %>%
      arrange(desc(count))
    
    datatable(
      intersection_data,
      options = list(
        pageLength = 10,
        order = list(list(3, 'desc'))
      ),
      colnames = c("Experience with AI", "AI Animal Personality", "Count", "Percentage (%)")
    )
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server) 