filter_data <- function(data) {
  data_change <- data
  
  # Get the list of columns that have _H and _P suffixes
  columns_historic <- grep('_H$', names(data_change), value = TRUE)
  columns_present <- gsub('_H$', '_P', columns_historic)
  
  # Calculate the percentage change for each historic-present pair and add to data_change
  data_change <- data_change %>%
    rowwise() %>%
    mutate(across(
      all_of(columns_present),
      .fns = ~ (. - get(gsub(
        '_P$', '_H', cur_column()
      ))) /
        get(gsub('_P$', '_H', cur_column())) * 100,
      #Convert to percentage change
      .names = 'pct_change_{.col}'
    )) %>%
    ungroup()
  
  # Select only the desired columns for the output
  data_change %>%
    select(Tribe,
           grep('pct_change_', names(data_change), value = TRUE),
           Acres,
           Endow_Raised_Parcel)
}

violin_plot_summary <- function(data, var_labels) {
  data %>%
    summarise(across(all_of(var_labels), list(
      Mean = ~ mean(.x, na.rm = TRUE),
      Median = ~ median(.x, na.rm = TRUE)
    ))) %>%
    pivot_longer(
      everything(),
      names_to = c('Variable', 'Statistic'),
      names_sep = '_(?=[^_]+$)',
      values_to = 'Value'
    ) %>%
    pivot_wider(names_from = Statistic, values_from = Value)
}

violin_plot <- function(df, var, var_labels, title = NULL) {
  mean_value <- mean(df[[var]], na.rm = TRUE)
  y_limits <- range(df[[var]], na.rm = TRUE) * c(0.9, 1.1)
  
  p <- ggplot(df, aes(x = 1, y = .data[[var]])) +
    geom_violin(fill = 'lightblue',
                color = 'black',
                alpha = 0.7) +
    geom_hline(yintercept = 0,
               color = 'red',
               size = 1) +
    geom_jitter(
      aes(
        text = paste0(
          'Tribe: ',
          Tribe,
          '<br>Acres: ',
          round(Acres),
          '<br>Change: ',
          round(.data[[var]], 2),
          '%'
        )
      ),
      width = 0.2,
      alpha = 0.7,
      color = 'blue'
    ) +
    geom_point(
      aes(y = mean_value, text = paste0('Mean: ', round(mean_value, 2), '%')),
      color = 'red',
      shape = 18,
      size = 3
    ) +
    theme_minimal(base_size = 10) +
    labs(title = title, y = '% change') +
    theme(
      text = element_text(family = 'arial'),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 10, face = 'bold'),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = y_limits)
  
  ggplotly(p, tooltip = 'text') %>% style(hoverinfo = 'skip', traces = 0)
}

prod_sec_scatterplot <- function(df) {
  p <- ggplot(
    df,
    aes(
      x = pct_change_nccpi3all_P,
      y = pct_change_Food_Insecurity_Rate_2018_P,
      size = Acres,
      text = paste0(
        'Tribe: ',
        Tribe,
        '<br>Acres: ',
        round(Acres),
        '<br>Food production: ',
        round(pct_change_nccpi3all_P, 2),
        '%',
        '<br>Food insecurity: ',
        round(pct_change_Food_Insecurity_Rate_2018_P, 2),
        '%'
      )
    )
  ) +
    geom_point(
      shape = 21,
      color = 'black',
      fill = alpha('#ea801c', 0.4),
      stroke = 0.5
    ) +
    labs(x = '% Change in Food Productivity', y = '% Change in Food Insecurity', size = 'Acres') +
    geom_hline(yintercept = 0,
               color = 'black',
               size = 0.5) +
    geom_vline(xintercept = 0,
               color = 'black',
               size = 0.5) +
    theme_classic(base_size = 14) +
    theme(
      text = element_text(family = 'arial'),
      axis.title = element_text(size = 10, face = 'bold'),
      axis.text = element_text(size = 10),
      axis.line = element_line(color = 'black'),
      panel.grid = element_blank()
    ) +
    scale_size_continuous(
      range = c(3, 10),
      breaks = c(1e4, 1e5, 5e5, 1e6, 2e6),
      labels = c('10,000', '100,000', '500,000', '1,000,000', '2,000,000')
    )
  
  ggplotly(p, tooltip = 'text')
}

temp_precip_scatterplot <- function(df) {
  p <- ggplot(
    df,
    aes(
      x = pct_change_precip_mean_ann_P,
      y = pct_change_temp_mean_ann_P,
      size = Acres,
      text = paste0(
        'Tribe: ',
        Tribe,
        '<br>Acres: ',
        round(Acres),
        '<br>Temperature: ',
        round(pct_change_precip_mean_ann_P, 2),
        '%',
        '<br>Precipitation: ',
        round(pct_change_temp_mean_ann_P, 2),
        '%'
      )
    )
  ) +
    geom_point(
      shape = 21,
      color = 'black',
      fill = alpha('#1a80bb', 0.4),
      stroke = 0.5
    ) +
    labs(x = '% Change in Annual Precipitation', y = '% Change in Annual Temperature', size = 'Acres') +
    geom_hline(yintercept = 0,
               color = 'black',
               size = 0.5) +
    geom_vline(xintercept = 0,
               color = 'black',
               size = 0.5) +
    theme_classic(base_size = 14) +
    theme(
      text = element_text(family = 'arial'),
      axis.title = element_text(size = 10, face = 'bold'),
      axis.text = element_text(size = 10),
      axis.line = element_line(color = 'black'),
      panel.grid = element_blank()
    ) +
    scale_size_continuous(
      range = c(3, 10),
      breaks = c(1e4, 1e5, 5e5, 1e6, 2e6),
      labels = c('10,000', '100,000', '500,000', '1,000,000', '2,000,000')
    )
  
  ggplotly(p, tooltip = 'text')
  
}

plot_temp_precip_for_tribe_gg <- function(df, tribe_name) {
  tribe_data <- df %>% filter(Tribe == tribe_name)
  if (nrow(tribe_data) == 0) {
    cat('No data found for tribe:', tribe_name, '\n')
    return()
  }
  
  months <- 1:12
  
  temp_data <- data.frame(
    Month = rep(months, 2),
    Mean = c(
      sapply(months, function(i)
        tribe_data[[paste0('temp_mean_', i, '_P')]]),
      sapply(months, function(i)
        tribe_data[[paste0('temp_mean_', i, '_H')]])
    ),
    StdDev = c(
      sapply(months, function(i)
        tribe_data[[paste0('temp_std_', i, '_P')]]),
      sapply(months, function(i)
        tribe_data[[paste0('temp_std_', i, '_H')]])
    ),
    DataType = rep(c('Present', 'Historic'), each = 12)
  )
  
  precip_data <- data.frame(
    Month = rep(months, 2),
    Mean = c(
      sapply(months, function(i)
        tribe_data[[paste0('precip_mean_', i, '_P')]]),
      sapply(months, function(i)
        tribe_data[[paste0('precip_mean_', i, '_H')]])
    ),
    StdDev = c(
      sapply(months, function(i)
        tribe_data[[paste0('precip_std_', i, '_P')]]),
      sapply(months, function(i)
        tribe_data[[paste0('precip_std_', i, '_H')]])
    ),
    DataType = rep(c('Present', 'Historic'), each = 12)
  )
  
  custom_theme <- theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = 'arial'),
      axis.title = element_text(size = 10, face = 'bold'),
      axis.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.position = 'top',
    )
  
  # Temperature plot
  temp_plot <- ggplot(temp_data,
                      aes(
                        x = Month,
                        y = Mean,
                        color = DataType,
                        group = DataType
                      )) +
    geom_line(aes(text = paste0('Mean: ', round(Mean, 2), '°C')), size = 1) +
    geom_point(aes(text = paste0('Mean: ', round(Mean, 2), '°C')), size = 3) +
    geom_errorbar(aes(ymin = Mean - StdDev, ymax = Mean + StdDev), width = 0.2) +
    scale_color_manual(values = c('Present' = 'blue', 'Historic' = 'red')) +
    labs(y = 'Temperature (°C)', x = NULL) +
    custom_theme
  
  temp_plotly <- ggplotly(temp_plot, tooltip = 'text')
  
  # Precipitation plot
  precip_plot <- ggplot(precip_data,
                        aes(
                          x = Month,
                          y = Mean,
                          color = DataType,
                          group = DataType
                        )) +
    geom_line(aes(text = paste0('Mean: ', round(Mean, 2), 'mm')), size = 1) +
    geom_point(aes(text = paste0('Mean: ', round(Mean, 2), 'mm')), size = 3) +
    geom_errorbar(aes(ymin = Mean - StdDev, ymax = Mean + StdDev), width = 0.2) +
    scale_color_manual(values = c('Present' = 'blue', 'Historic' = 'red')) +
    labs(y = 'Precipitation (mm)', x = 'Month') +
    scale_x_continuous(breaks = seq(2, 12, by = 2)) +  # Set x-axis breaks every 2 months
    custom_theme
  
  precip_plotly <- ggplotly(precip_plot, tooltip = 'text')
  
  for (i in seq_along(precip_plotly$x$data)) {
    precip_plotly$x$data[[i]]$showlegend <- FALSE
  }
  
  
  combined_plt <- subplot(
    temp_plotly,
    precip_plotly,
    nrows = 2,
    shareX = TRUE,
    titleY = TRUE,
    heights = c(0.48, 0.48)
  ) %>%
    layout(
      title = list(
        text = tribe_name,
        x = 0.5,
        xanchor = 'center',
        font = list(size = 18, family = 'arial')
      ),
      margin = list(t = 80)
    )
  
  combined_plt
}

map <- function(df,
                uni_data,
                parcel_data,
                res_shapes,
                var,
                var_labels) {
  tribes <- df$Tribe
  tribe_shapes <- res_shapes %>% filter(TRIBE_NAME %in% tribes)
  tribe_data <- tribe_shapes %>%
    left_join(df, by = c('TRIBE_NAME' = 'Tribe'))
  
  uni_points <- uni_data %>% st_cast('POINT', warn = FALSE)
  
  pal <- colorNumeric(
    palette = brewer.pal(9, 'Reds'),
    domain = tribe_data[[var]],
    na.color = 'gray',
    reverse = FALSE
  )
  
  legend_var <- names(var_labels)[var_labels == var]
  
  show_parcels <- nrow(parcel_data) > 0 && nrow(parcel_data) < 10000
  
  m <- leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = tribe_shapes,
      color = 'red',
      weight = 2,
      fillColor = pal(tribe_data[[var]]),
      fillOpacity = 0.8,
      popup = paste0('Tribe: ', tribe_data[['TRIBE_NAME']], '<br>Change: ', round(tribe_data[[var]], 2), '%'),
      group = 'Tribes'
    )
  
  if (show_parcels) {
    parcel_display <- parcel_data %>%
      filter(st_geometry_type(.) %in% c('POLYGON', 'MULTIPOLYGON'))
    
    m <- m %>%
      addPolygons(
        data = parcel_display,
        color = 'mediumorchid',
        weight = 2,
        fillColor = 'orchid',
        fillOpacity = 0.6,
        popup = paste0('University: ', parcel_display[['University.x']]),
        group = 'Parcels'
      )
  } else {
    m <- m %>% hideGroup('Parcels')
  }
  
  m %>%
    addCircleMarkers(
      data = uni_points,
      radius = 4,
      color = '#27408B',
      fillColor = '#27408B',
      fillOpacity = 0.8,
      popup = paste0(
        uni_data[['Uni_Name']],
        '<br>Year founded: ',
        uni_data[['Yr_Uni_Founded']],
        '<br>Raised: ',
        uni_data[['Adjusted_ Total_Value_1914']]
      ),
      group = 'Universities'
    ) %>%
    addLegend(
      pal = pal,
      values = tribe_data[[var]],
      title = legend_var,
      position = 'bottomright'
    ) %>%
    addLegend(
      position = 'bottomleft',
      colors = c('#27408B', 'orchid', 'red'),
      labels = c('Universities', 'Parcels', 'Tribes'),
      title = 'Legend'
    ) %>%
    addLayersControl(
      overlayGroups = c('Universities', 'Tribes', 'Parcels'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    setView(lng = -97,
            lat = 38,
            zoom = 3)
}