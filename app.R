library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)

source("helpers.R")
all_data <- read_csv('data/Data_Analysis_all.csv')
filter_data <- filter_data(all_data)
university_list <- read_csv('data/University_List.csv')

var_labels <- c(
  "Food Insecurity" = "pct_change_Food_Insecurity_Rate_2018_P",
  "Child Food Insecurity" = "pct_change_Child_Food_Insecurity_Rate_2018_P",
  "Cost Per Meal" = "pct_change_Cost_Per_Meal_2018_P",
  "Budget Shortfall" = "pct_change_Weighted_Annual_Food_Budget_Shortfall_2018_P",
  "Food Productivity" = "pct_change_nccpi3all_P",
  "FP: Small Grains" = "pct_change_nccpi3sg_P",
  "FP: Soybeans" = "pct_change_nccpi3soy_P",
  "FP: Corn" = "pct_change_nccpi3corn_P",
  "Temperature" = "pct_change_temp_mean_ann_P",
  "Precipitation" = "pct_change_precip_mean_ann_P"
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("uni", "Select a university", 
                  choices = university_list$Universities),
      uiOutput("page_select_input")
    ),
    mainPanel(
      tabsetPanel(id = "page",
                  tabPanel("Violin plots", 
                           plotlyOutput("violin_plot"), 
                           tableOutput("violin_plot_table")),
                  tabPanel("Scatterplots", 
                           plotlyOutput("prod_sec_scatterplot", height="45vh"), 
                           plotlyOutput("temp_precip_scatterplot"), height="40vh"),
                  tabPanel("By tribe", 
                           plotlyOutput("plot_temp_precip_for_tribe_gg"))
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  output$page_select_input <- renderUI({
    switch(input$page,
           "Violin plots" = selectInput("var", "Variable", 
                                        choices = var_labels),
           "By tribe" = selectInput("tribe", "Tribe", 
                                    choices = sort(unique(all_data$Tribe))),
           NULL
    )
  })

  selected_data <- reactive({
    if (!is.null(input$uni) && input$uni != "All 1862 Land Grant Institutions") {
      file <- paste0("data/university-data/Data_Analysis_", input$uni, ".csv")
      filter_data <- read_csv(file)
    }
    filter_data(filter_data)
  })
  observe({
    data_for_plot <- selected_data()
  })
  
  # Plots
  output$violin_plot <- renderPlotly({
    violin_plot(selected_data(), input$var, var_labels)
  })
  
  output$violin_plot_table <- renderTable({
    summary_table(selected_data(), var_labels)})
  
  output$prod_sec_scatterplot <- renderPlotly({
    prod_sec_scatterplot(selected_data())
  })
  
  output$temp_precip_scatterplot <- renderPlotly({
    temp_precip_scatterplot(selected_data())
  })
  
  output$plot_temp_precip_for_tribe_gg <- renderPlotly({
    req(input$tribe)
    plot_temp_precip_for_tribe_gg(all_data, input$tribe)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)