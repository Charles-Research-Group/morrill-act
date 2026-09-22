library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)
library(sf)
library(RColorBrewer)

source('helpers.R')

# ============================================================
# LOAD DATA
# ============================================================
all_data <- read_csv('data/Data_Analysis_combined.csv', show_col_types = FALSE)
additional_data <- read_csv('data/Additional_Tribes.csv', show_col_types = FALSE)
res_shapes  <- readRDS('data/preprocessed/reservations.rds')
ok_shapes <- readRDS('data/preprocessed/ok_reservations.rds')
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

parcel_by_university_tribe <- readRDS(
  'data/preprocessed/parcel_by_university_tribe_small.rds'
) %>%
  mutate(University.x = gsub('/', ' & ', University.x))

parcel_by_university <- readRDS(
  'data/preprocessed/parcel_by_university_small.rds'
) %>%
  mutate(University.x = gsub('/', ' & ', University.x))

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

# cat("=== Oklahoma tribes missing from Data_Analysis_all.csv ===\n")
# 
# ok_tribes <- ok_shapes %>%
#   st_drop_geometry() %>%
#   pull(TRIBE_NAME) %>%
#   unique()
# 
# csv_tribes <- all_data %>%
#   pull(Tribe) %>%
#   unique()
# 
# print(setdiff(ok_tribes, csv_tribes))
# 
# cat("===============================================\n")

# ============================================================
# UI LAYOUT
# ============================================================
ui <- page_sidebar(
  tags$head(tags$style(
    HTML(
      '
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
      '
    )
  )),
  sidebar = sidebar(
    id = 'sidebar',
    open = list(desktop = 'open', mobile = 'closed'),
    uiOutput('page_select_input')
  ),
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
      tags$style(
        '
    @media (max-width: 768px) {
      #violin-wrap { width: 100% !important; }
    }
  '
      ),
      div(
        id = 'violin-wrap',
        style = 'width: 50%; padding: 10px;',
        plotlyOutput('violin_plot', width = '100%', height = '60vh')
      ),
      div(style = 'overflow-x: auto;', tableOutput('violin_plot_table'))
    ),
    tabPanel(
      'Scatterplots',
      plotlyOutput('prod_sec_scatterplot', width = '100%', height = '40vh'),
      plotlyOutput(
        'temp_precip_scatterplot',
        width = '100%',
        height = '40vh'
      )
    ),
    tabPanel(
      'By tribe',
      plotlyOutput(
        'plot_temp_precip_for_tribe_gg',
        width = '100%',
        height = '80vh'
      ),
      div(
        style = "overflow-x: auto;",
        tableOutput('tribe_summary_table')
      )
    ),
    tabPanel('Map', div(
      style = 'padding: 20px;',
      p(
        'We have chosen to omit the option to view all parcels for efficiency reasons, but',
        a('landgrabu.org', href = 'https://landgrabu.org', target = '_blank'),
        'provides this option.'
      ),
      leafletOutput('map', width = '100%', height = '65vh')
    ))
  )
)

# ============================================================
# SERVER LOGIC
# ============================================================
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
    req(input$uni, input$tribe)
    filtered_uni <- uni_data
    
    if (input$uni != 'All 1862 Land Grant Institutions') {
      uni_names <- strsplit(input$uni, ' & ')[[1]]
      filtered_uni <- filtered_uni %>%
        filter(Uni_Name %in% uni_names)
    }
    
    if (input$tribe != 'All Tribes') {
      valid_unis <- parcel_by_university_tribe %>%
        filter(Present_Day_Tribes == input$tribe) %>%
        pull(University.x) %>%
        unique() %>%
        strsplit(' & ') %>%
        unlist()
      
      filtered_uni <- filtered_uni %>%
        filter(Uni_Name %in% valid_unis)
    }
    
    filtered_uni
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
  
  selected_ok_shapes <- reactive({
    req(input$tribe)
    
    if (input$tribe == 'All Tribes') {
      ok_shapes
    } else {
      ok_shapes %>%
        filter(TRIBE_NAME == input$tribe)
    }
  })
  
  selected_parcel_data <- reactive({
    req(input$uni, input$tribe)
    
    result <- if (input$uni == 'All 1862 Land Grant Institutions' &&
                  input$tribe == 'All Tribes') {
      st_sf(geometry = st_sfc(), crs = 4326)
    } else if (input$uni == 'All 1862 Land Grant Institutions') {
      parcel_by_university_tribe %>%
        mutate(University.x = gsub('/', ' & ', University.x)) %>%
        filter(Present_Day_Tribes == input$tribe)
    } else if (input$tribe == 'All Tribes') {
      parcel_by_university %>%
        mutate(University.x = gsub('/', ' & ', University.x)) %>%
        filter(University.x == input$uni)
    } else {
      parcel_by_university_tribe %>%
        mutate(University.x = gsub('/', ' & ', University.x)) %>%
        filter(University.x == input$uni,
               Present_Day_Tribes == input$tribe)
    }
    
    st_transform(result, 4326)
  })
  
  # ============================================================
  # PLOTS
  # ============================================================
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
  
  output$tribe_summary_table <- renderTable({
    req(input$tribe)
    tribe_summary(all_data, input$tribe, var_labels)
  })
  
  output$map <- renderLeaflet({
    req(input$var)
    map(
      selected_data(),
      selected_uni_data(),
      selected_parcel_data(),
      selected_res_shapes(),
      selected_ok_shapes(),
      input$var,
      var_labels
    )
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)