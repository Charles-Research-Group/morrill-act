# ============================================================
# FILTER DATA
# ============================================================

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
      # Convert to percentage change
      .names = 'pct_change_{.col}'
    )) %>%
    ungroup()
  
  # Select only the desired columns for the output
  data_change %>%
    select(
      Tribe,
      contains('pct_change'),
      contains('_H'),
      contains('_P'),
      Acres,
      Endow_Raised_Parcel
    )
}

# ============================================================
# VIOLIN PLOTS
# ============================================================

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
    labs(title = title, y = '% Change') +
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

# ============================================================
# SCATTERPLOTS
# ============================================================

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
    ) +
    coord_cartesian(
      xlim = range(df$pct_change_nccpi3all_P, na.rm = TRUE) * 1.2,
      ylim = range(df$pct_change_Food_Insecurity_Rate_2018_P, na.rm = TRUE) * 1.2
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
    ) +
    coord_cartesian(
      xlim = range(df$pct_change_precip_mean_ann_P, na.rm = TRUE) * 1.2,
      ylim = range(df$pct_change_temp_mean_ann_P, na.rm = TRUE) * 1.2
    )
  
  ggplotly(p, tooltip = 'text')
  
}

# ============================================================
# TEMP-PRECIP LINE PLOTS & TABLE BY TRIBE
# ============================================================

tribe_summary <- function(df, tribe_name, var_labels) {
  tribe <- df %>%
    filter(Tribe == tribe_name)
  
  result <- data.frame(
    Variable = names(var_labels),
    Present = NA,
    Historic = NA,
    Percent_Change = NA
  )
  
  for (i in seq_along(var_labels)) {
    
    pct_col <- var_labels[i]
    present_col <- sub('pct_change_', '', pct_col)
    historic_col <- sub('_P$', '_H', present_col)
    
    has_present <- present_col %in% names(tribe) &&
      !is.na(tribe[[present_col]][1])
    has_historic <- historic_col %in% names(tribe) &&
      !is.na(tribe[[historic_col]][1])
    
    if (has_present) {
      result$Present[i] <- tribe[[present_col]][1]
    }
    if (has_historic) {
      result$Historic[i] <- tribe[[historic_col]][1]
    }
    
    if (has_present && has_historic) {
      if (result$Historic[i] != 0) {
        result$Percent_Change[i] <-
          (result$Present[i] - result$Historic[i]) /
          result$Historic[i] * 100
      }
    }
  }
  
  # Format for display
  names(result)[names(result) == 'Percent_Change'] <- '% Change'
  result$Present <- ifelse(is.na(result$Present), 'N/A', round(result$Present, 2))
  result$Historic <- ifelse(is.na(result$Historic), 'N/A', round(result$Historic, 2))
  result$`% Change` <- ifelse(is.na(result$`% Change`), 'N/A', round(result$`% Change`, 2))
  
  result
}

plot_temp_precip_for_tribe_gg <- function(df, tribe_name) {
  tribe_data <- df %>% filter(Tribe == tribe_name)
  if (nrow(tribe_data) == 0) {
    cat('No data found for tribe:', tribe_name, '\n')
    return()
  }
  
  months <- 1:12
  
  has_temp_history <- all(paste0('temp_mean_', 1:12, '_H') %in% names(tribe_data)) &&
    !all(is.na(tribe_data[paste0('temp_mean_', 1:12, '_H')]))
  
  temp_data <- data.frame(
    Month = rep(months, 2),
    Mean = c(sapply(months, function(i)
      tribe_data[[paste0('temp_mean_', i, '_P')]]), if (has_temp_history) {
        sapply(months, function(i)
          tribe_data[[paste0('temp_mean_', i, '_H')]])
      }),
    StdDev = c(sapply(months, function(i)
      tribe_data[[paste0('temp_std_', i, '_P')]]), if (has_temp_history) {
        sapply(months, function(i)
          tribe_data[[paste0('temp_std_', i, '_H')]])
      }),
    DataType = rep(c('Present', 'Historic'), each = 12)
  )
  
  has_precip_history <- all(paste0('precip_mean_', 1:12, '_H') %in% names(tribe_data)) &&
    !all(is.na(tribe_data[paste0('precip_mean_', 1:12, '_H')]))
  
  precip_data <- data.frame(
    Month = rep(months, 2),
    Mean = c(sapply(months, function(i)
      tribe_data[[paste0('precip_mean_', i, '_P')]]), if (has_precip_history) {
        sapply(months, function(i)
          tribe_data[[paste0('precip_mean_', i, '_H')]])
      }),
    StdDev = c(sapply(months, function(i)
      tribe_data[[paste0('precip_std_', i, '_P')]]), if (has_precip_history) {
        sapply(months, function(i)
          tribe_data[[paste0('precip_std_', i, '_H')]])
      }),
    DataType = rep(c('Present', 'Historic'), each = 12)
  )
  
  custom_theme <- theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = 'arial'),
      axis.title = element_text(size = 10, face = 'bold'),
      axis.text = element_text(size = 10),
      legend.title = element_blank()
    )
  
  temp_plot <- suppressWarnings(ggplot(temp_data,
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
  )
  
  temp_plotly <- ggplotly(temp_plot, tooltip = 'text')
  
  precip_plot <- suppressWarnings(ggplot(precip_data,
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
    scale_x_continuous(breaks = seq(2, 12, by = 2)) +
    custom_theme
  )
  
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
        text = paste(strwrap(tribe_name, width = 40), collapse = '<br>'),
        x = 0.5,
        xanchor = 'center',
        font = list(size = 16, family = 'arial')
      ),
      legend = list(
        title = list(text = ''),
        orientation = 'h',
        x = 0.5,
        xanchor = 'center',
        y = -0.30,
        yanchor = 'top'
      ),
      margin = list(t = 80, b = 60),
      
      modebar = list(
        remove = c(
          'zoom2d',
          'pan2d',
          'select2d',
          'lasso2d',
          'zoomIn2d',
          'zoomOut2d',
          'autoScale2d',
          'hoverClosestCartesian',
          'hoverCompareCartesian',
          'toggleSpikelines'
        )
      )
    )
  
  combined_plt
}

# ============================================================
# MAP
# ============================================================

tribe_popup <- function(data, var, var_labels) {
  
  var_name <- names(var_labels)[var_labels == var]
  
  popups <- character(nrow(data))
  
  for (i in seq_len(nrow(data))) {
    
    value <- data[[var]][i]
    
    tribe <- data[['TRIBE_NAME']][i]
    
    popup <- paste0(
      'Tribe: ', tribe
    )
    
    if (!is.na(value)) {
      popup <- paste0(
        popup,
        '<br>',
        var_name,
        ': ',
        round(value, 2),
        '%'
      )
    } else {
      
      pct_col <- var
      present_col <- sub('pct_change_', '', pct_col)
      historic_col <- sub('_P$', '_H', present_col)
      
      has_present <- present_col %in% names(data) &&
        !is.na(data[[present_col]][i])
      
      has_historic <- historic_col %in% names(data) &&
        !is.na(data[[historic_col]][i])
      
      if (has_present) {
        popup <- paste0(
          popup,
          '<br>',
          var_name,
          ' (Present): ',
          round(data[[present_col]][i], 2)
        )
      } else if (has_historic) {
        popup <- paste0(
          popup,
          '<br>',
          var_name,
          ' (Historic): ',
          round(data[[historic_col]][i], 2)
        )
      } else {
        popup <- paste0(
          popup,
          '<br>',
          var_name,
          ': Unavailable'
        )
      }
    }
    
    popup <- paste0(
      popup,
      '<br>Endowment Raised: N/A'
    )
    
    popups[i] <- popup
  }
  
  popups
}

map <- function(df,
                uni_data,
                parcel_data,
                res_shapes,
                var,
                tribe,
                var_labels) {
  tribes <- df$Tribe
  tribe_shapes <- res_shapes
  tribe_data <- tribe_shapes %>%
    left_join(df, by = c('TRIBE_NAME' = 'Tribe'))
  
  uni_points <- uni_data %>% st_cast('POINT', warn = FALSE)
  
  all_values <- tribe_data[[var]]
  all_values <- all_values[!is.na(all_values)]
  
  if (length(all_values) == 0) {
    max_abs <- 1
  } else {
    max_abs <- quantile(abs(all_values), 0.95, na.rm = TRUE)
    if (is.na(max_abs) || max_abs == 0) {
      max_abs <- max(abs(all_values), na.rm = TRUE)
    }
  }
  
  pal <- colorNumeric(
    palette = rev(brewer.pal(11, 'RdYlBu')),
    domain = c(-max_abs, max_abs),
    na.color = 'gray'
  )
  
  legend_values <- c(-max_abs, 0, max_abs)
  legend_var <- paste0(names(var_labels)[var_labels == var], ' (% Change)')
  
  uni_icon <- makeIcon(
    iconUrl = 'assets/uni.png',
    iconWidth = 18,
    iconHeight = 18,
    iconAnchorX = 9,
    iconAnchorY = 18,
    popupAnchorX = 0,
    popupAnchorY = -18,
  )
  
  show_parcels <- nrow(parcel_data) > 0 && nrow(parcel_data) < 90000
  
  m <- leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = tribe_data,
      color = 'brown',
      weight = 3,
      fillColor = pal(pmax(pmin(
        tribe_data[[var]], max_abs
      ), -max_abs)),
      fillOpacity = 0.8,
      popup = tribe_popup(tribe_data, var, var_labels),
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
  
  if (length(all_values) > 0) {
    m <- m %>%
      addLegend(
        position = 'bottomright',
        pal = pal,
        values = pmax(pmin(all_values, max_abs), -max_abs),
        title = legend_var
      )
  }
  
  m <- m %>%
    addMarkers(
      data = uni_points,
      icon = uni_icon,
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
      position = 'bottomleft',
      colors = c('dodgerblue', 'orchid', 'red'),
      labels = c('Universities', 'Parcels', 'Tribes'),
      title = 'Legend',
    ) %>%
    addLayersControl(
      overlayGroups = c('Universities', 'Tribes', 'Parcels'),
      options = layersControlOptions(collapsed = FALSE)
    )
  
    if (tribe != 'All Tribes' && nrow(tribe_data) > 0) {
      bbox <- st_bbox(st_transform(tribe_data, 4326))
      
      if (all(is.finite(bbox))) {
        m <- m %>%
          fitBounds(
            lng1 = as.numeric(bbox['xmin']),
            lat1 = as.numeric(bbox['ymin']),
            lng2 = as.numeric(bbox['xmax']),
            lat2 = as.numeric(bbox['ymax'])
          )
      } else {
        m <- m %>% setView(lng = -97, lat = 38, zoom = 3)
      }
    } else {
      m <- m %>% setView(lng = -97, lat = 38, zoom = 3)
    }
  
  m
}