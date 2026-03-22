library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)
library(leaflet)
library(sf)
library(leafgl)
library(RColorBrewer)
library(tidyverse)
library(lobstr)

source('helpers.R')

# Load data ----
all_data <- read_csv('data/Data_Analysis_all.csv', show_col_types = FALSE)
res_shapes  <- readRDS('data/preprocessed/reservations.rds')
uni_shapes  <- readRDS('data/preprocessed/universities.rds')
uni_list <- read_csv('data/University_List.csv', show_col_types = FALSE)
uni_info <- read_csv('data/landgrabu-data/csvs/Universities.csv',
                     show_col_types = FALSE)
uni_data <- uni_shapes %>%
  mutate(Uni_Name = gsub('/', ' & ', Uni_Name)) %>%
  mutate(Value = ifelse(Uni_Name == 'unidentified', 'Unidentified', Uni_Name)) %>%
  left_join(uni_info %>% mutate(University = gsub('/', ' & ', University)),
            by = c('Uni_Name' = 'University')) %>%
  st_cast('POINT', warn = FALSE)
parcel_data <- readRDS('data/preprocessed/parcel_data_small.rds')
parcel_by_university <- readRDS("data/preprocessed/parcel_by_university.rds")
parcel_by_tribe <- readRDS("data/preprocessed/parcel_by_tribe.rds")
parcel_by_university_tribe <- readRDS("data/preprocessed/parcel_by_university_tribe.rds")

var_labels <- c(
  'Food Insecurity' = 'pct_change_Food_Insecurity_Rate_2018_P',
  'Child Food Insecurity' = 'pct_change_Child_Food_Insecurity_Rate_2018_P',
  'Cost Per Meal' = 'pct_change_Cost_Per_Meal_2018_P',
  'Budget Shortfall' = 'pct_change_Weighted_Annual_Food_Budget_Shortfall_2018_P',
  'Food Productivity' = 'pct_change_nccpi3all_P',
  'FP: Small Grains' = 'pct_change_nccpi3sg_P',
  'FP: Soybeans' = 'pct_change_nccpi3soy_P',
  'FP: Corn' = 'pct_change_nccpi3corn_P',
  'Temperature' = 'pct_change_temp_mean_ann_P',
  'Precipitation' = 'pct_change_precip_mean_ann_P'
)

tribes_all <- sort(unique(all_data$Tribe))
tribes_shapes <- sort(unique(res_shapes$TRIBE_NAME))

print(setdiff(tribes_all, tribes_shapes))
print("---------------------------------")
print(setdiff(tribes_shapes, tribes_all))

# UI layout ----
ui <- page_sidebar(
  tags$head(tags$style(
    HTML(
      "
      .selectize-dropdown .option {
        color: black !important;
      }

      .selectize-dropdown .option:nth-child(odd), 
      .selectize-dropdown .option:nth-child(odd):hover {
        background-color: #ffffff !important;
      }

      .selectize-dropdown .option:nth-child(even),
      .selectize-dropdown .option:nth-child(even):hover {
        background-color: #e6e6e6 !important;
      }
      "
    )
  )),
  sidebar = sidebar(id = 'sidebar', uiOutput('page_select_input')),
  tabsetPanel(
    id = 'page',
    tabPanel('Home', div(
      style = 'margin: 20px;',
      p(
        'The Morrill Land-Grant Acts of 1862 and 1890 were federal laws that funded
        the creation of public colleges focused on agriculture and engineering.
        The 1862 Act granted states 30,000 acres of federal land to sell or develop
        for each of their representatives and senators in Congress. In total, nearly
        11 million acres, used to fund 52 land-grant universities, had been obtained
        through the violence-backed dispossession of Indigenous tribes.'
      ),
      p(
        'This app contains data visualizations exploring crop production, food
        insecurity, and climate trends in Indigenous tribes affected by the law.'
      )
    )),
    tabPanel(
      'Violin plots',
      plotlyOutput('violin_plot', width = '60vh', height = '80vh'),
      tableOutput('violin_plot_table')
    ),
    tabPanel(
      'Scatterplots',
      plotlyOutput('prod_sec_scatterplot', height = '40vh'),
      plotlyOutput('temp_precip_scatterplot', height = '40vh')
    ),
    tabPanel('By tribe', plotlyOutput('plot_temp_precip_for_tribe_gg')),
    tabPanel('Map', div(
      style = 'padding: 20px;',
      p(
        'We have chosen to omit the option to view all parcels for efficiency reasons, but',
        a('landgrabu.org', href = 'https://landgrabu.org', target = '_blank'),
        'provides this option.'
      ),
      leafletOutput('map', width = '120vh', height = '80vh')
    ))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  output$page_select_input <- renderUI({
    switch(
      input$page,
      'Home' = tagList(
        selectInput('uni', 'University', choices = uni_list$Universities),
        selectInput('tribe', 'Tribe', choices = c('All Tribes', sort(
          unique(all_data$Tribe)
        ))),
        selectInput('var', 'Variable', choices = var_labels)
      ),
      'Violin plots' = tagList(
        selectInput('uni', 'University', choices = uni_list$Universities),
        selectInput('var', 'Variable', choices = var_labels)
      ),
      'Scatterplots' = tagList(
        selectInput('uni', 'University', choices = uni_list$Universities),
      ),
      'By tribe' = tagList(selectInput('tribe', 'Tribe', choices = sort(
        unique(all_data$Tribe)
      ))),
      'Map' = tagList(
        selectInput('uni', 'University', choices = uni_list$Universities),
        selectInput('tribe', 'Tribe', choices = c('All Tribes', sort(
          unique(all_data$Tribe)
        ))),
        selectInput('var', 'Variable', choices = var_labels)
      ),
      NULL
    )
  })
  
  selected_data <- reactive({
    req(input$uni)
    if (input$uni == 'All 1862 Land Grant Institutions') {
      filter_data(all_data)
    } else {
      file_path <- paste0('data/university-data/Data_Analysis_',
                          input$uni,
                          '.csv')
      filter_data(read_csv(file_path, show_col_types = FALSE))
    }
  })
  
  selected_uni_data <- reactive({
    req(input$uni)
    if (input$uni == 'All 1862 Land Grant Institutions') {
      uni_data
    } else {
      uni_names <- strsplit(input$uni, ' & ')[[1]]
      uni_data %>%
        filter(Uni_Name %in% uni_names)
    }
  })
  
  selected_res_shapes <- reactive({
    req(input$tribe)
    if (input$tribe == 'All Tribes') {
      res_shapes
    } else {
      res_shapes %>%
        filter(TRIBE_NAME == input$tribe)
    }
  })
  
  selected_parcel_data <- reactive({
    req(input$uni, input$tribe)
    
    if (input$uni == "All 1862 Land Grant Institutions" &&
        input$tribe == "All Tribes") {
      parcel_data[0, ]
    } else if (input$uni == "All 1862 Land Grant Institutions") {
      parcel_by_tribe %>%
        filter(Present_Day_Tribes == input$tribe)
    } else if (input$tribe == "All Tribes") {
      parcel_by_university %>%
        filter(University.x == input$uni)
    } else {
      parcel_by_university_tribe %>%
        filter(University.x == input$uni,
               Present_Day_Tribes == input$tribe)
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
      selected_res_shapes(),
      input$var,
      var_labels
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)