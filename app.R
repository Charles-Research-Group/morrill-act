library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)
library(leaflet)
library(sf)
library(RColorBrewer)

source("helpers.R")

# Load data ----
all_data <- read_csv('data/Data_Analysis_all.csv')
filter_data <- filter_data(all_data)
res_shapes <- st_read('data/reservation-shapes/AmericanIndianReservations.shp')
uni_list <- read_csv('data/university_list.csv')

uni_shapes <- st_read('data/landgrabu-data/shapes/University_Points.shp')
uni_info <- read_csv('data/landgrabu-data/csvs/Universities.csv')
uni_data <- uni_shapes %>%
  left_join(uni_info, by = c("Uni_Name" = "University")) %>%
  st_cast("POINT", warn = FALSE)

parcel_shapes <- st_read('data/landgrabu-data/shapes/Parcel_Polygons.shp')
parcel_info <- read_csv('data/landgrabu-data/csvs/Parcels.csv')
parcel_data <- parcel_shapes %>%
  left_join(parcel_info, by = "MTRSA_LG") %>%
  st_make_valid()

# Extract polygons
parcel_data <- parcel_data %>%
  filter(!st_is_empty(.)) %>%
  st_cast("MULTIPOLYGON", warn = FALSE) %>%
  filter(!st_is_empty(.))

# Make sure CRS matches other layers
if (st_crs(parcel_data)$input != "EPSG:4326") {
  parcel_data <- st_transform(parcel_data, 4326)
}

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

# UI layout ----
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput("uni", "Select a university", choices = uni_list$Universities),
    uiOutput("page_select_input")
  ),
  mainPanel(
    tabsetPanel(
      id = "page",
      tabPanel(
        "Violin plots",
        plotlyOutput("violin_plot"),
        tableOutput("violin_plot_table")
      ),
      tabPanel(
        "Scatterplots",
        plotlyOutput("prod_sec_scatterplot", height =
                       "45vh"),
        plotlyOutput("temp_precip_scatterplot"),
        height = "40vh"
      ),
      tabPanel("By tribe", plotlyOutput("plot_temp_precip_for_tribe_gg")),
      tabPanel("Map", div(style = "padding: 20px;", leafletOutput("map")))
    )
  )
))

# Define server logic ----
server <- function(input, output, session) {
  output$page_select_input <- renderUI({
    switch(
      input$page,
      "Violin plots" = selectInput("var", "Variable", choices = var_labels),
      "By tribe" = selectInput("tribe", "Tribe", choices = sort(unique(all_data$Tribe))),
      "Map" = selectInput("var", "Variable", choices = var_labels),
      NULL
    )
  })
  
  selected_data <- reactive({
    req(input$uni)
    if (input$uni == "All 1862 Land Grant Institutions") {
      filter_data(all_data)
    } else {
      file_path <- paste0("data/university-data/Data_Analysis_", input$uni, ".csv")
      filter_data(read_csv(file_path))
    }
  })
  
  
  selected_uni_data <- reactive({
    req(input$uni)
    if (input$uni == "All 1862 Land Grant Institutions") {
      uni_data
    } else {
      uni_data %>% filter(Uni_Name == input$uni)
    }
  })
  
  selected_parcel_data <- reactive({
    req(input$uni)
    if (input$uni == "All 1862 Land Grant Institutions") {
      parcel_data
    } else {
      parcel_data %>% filter(University == input$uni)
    }
  })
  
  # Plots
  output$violin_plot <- renderPlotly({
    req(input$var)
    violin_plot(selected_data(), input$var)
  })
  
  output$violin_plot_table <- renderTable({
    violin_plot_summary(selected_data(), var_labels)
  })
  
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
  
  output$map <- renderLeaflet({
    req(input$var)
    map(
      selected_data(),
      selected_uni_data(),
      selected_parcel_data(),
      res_shapes,
      input$var,
      var_labels
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)