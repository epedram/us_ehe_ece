library(scales)
library(ggsci)

    all_annual_summaries_sum <- all_annual_summaries %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      group_by(YYYY
      ) %>% #,
      dplyr::summarise(
        #num_obs = n(),

        c_95_percent = sum(completeness_above_95_percent, na.rm = T),
        #c_90_percent = sum(completeness_above_90_percent),
        c_85_percent = sum(completeness_above_85_percent, na.rm = T),
        #c_80_percent = sum(completeness_above_80_percent),
        c_75_percent = sum(completeness_above_75_percent, na.rm = T),
        c_all_stations = length(unique(station_id))
        , .groups = 'drop') %>%
      pivot_longer(cols= starts_with("c_"),
                   names_to = "Completeness",
                   values_to = "count") %>%
      filter(YYYY > start_year)

    available_stations_plot <- ggplot(all_annual_summaries_sum,
                                      aes(x = YYYY, 
                                          y = count,
                                          group = Completeness,
                                          color = Completeness)) +
      geom_point() +
      geom_line(linetype = "dashed") +

      #scale_color_paletteer_d("rtist::oldenburg") +
      #scale_color_brewer(
      #palette = "Spectral", direction = 1) +
      scale_color_locuszoom() +

      # scale_color_viridis_d(option = "viridis",
      #                       direction = 1) +

      #scale_color_paletteer_d("rtist::picasso") +
      #scale_color_manual(
      #values = wes_palette("Cavalcanti1", n = 5))+
      #values = wesanderson::wes_palette("Zissou1", n = 5)) +

      labs(x = "Year",
           y = "Number of available stations"
      ) +
      labs(title = paste0("Data Completeness Index for 3 Variables of Interest (combined)")) +
      labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)"
      )) +
      theme_bw() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 11)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position='bottom')


      #coord_cartesian(ylim=c(1600, 2600))

    #theme(axis.text.x = element_text(angle = 90, hjust = 1))

    #available_stations_plot

    available_stations_plot_fn <- paste0(plots_output_path, "/",
                                         "qc_available_stations_plot",
                                         ".jpg")

    ggsave(available_stations_plot_fn,
           plot = available_stations_plot, 
           dpi = 300, units = "cm",
           width = 24, height = 18)
    
    available_stations_plot_facet_fn <- paste0(plots_output_path, "/",
                                         "qc_available_stations_plot_bycode",
                                         ".jpg")
    
    ggsave(available_stations_plot_facet_fn,
           plot = available_stations_plot +
             facet_grid(Completeness ~ .), 
           dpi = 300, units = "cm",
           width = 24, height = 18)
    

    # 5: Vis plot all stations ------
    all_stations_plot <- ggplot(all_annual_summaries_sum %>%
                                  filter(Completeness == "c_all_stations"),
                                aes(x = YYYY, 
                                    y = count,
                                    group = Completeness)) +
      geom_point() +
      geom_line(linetype = "dashed") +
      scale_color_locuszoom() +

      labs(title = paste0("Total Number of Stations per Year")) +
      labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)")) + 
                             
      labs(x = "Year",
           y = "Number of available stations") +
        
      theme_bw() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 11)) +
        
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position='bottom')

      #scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      #coord_cartesian(ylim=c(2000,2800)) +
      #theme(legend.position = "none")

    #facet_grid(completeness_code) +
    #theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #theme(legend.position='bottom')

    all_stations_plot

    all_stations_plot_fn <- paste0(plots_output_path, "/",
                                   "noaa_all_stations_plot",
                                   ".jpg")

    ggsave(all_stations_plot_fn,
           plot = all_stations_plot, dpi = 300,
           width = 20, height = 16, units = "cm")

    stations_annual_summary <- all_annual_summaries %>% 
    mutate(YYYY = as.integer(YYYY)) 
    
    glimpse(stations_annual_summary)

    vfx_completenessPlotH(stations_annual_summary,
                          x_var = temperature_comp_ratio,
                          x_var_tag = "Temperature",
                          x_var_lower_bound = .7,
                          plots_output_path = plots_output_path)

    vfx_completenessPlotH(stations_annual_summary,
                          x_var = wind_speed_comp_ratio,
                          x_var_tag = "Wind Speed",
                          x_var_lower_bound = .7,
                          plots_output_path = plots_output_path)

    vfx_completenessPlotH(stations_annual_summary,
                          x_var = temperature_dewpoint_comp_ratio,
                          x_var_tag = "Dewpoint Temperature",
                          x_var_lower_bound = .7,
                          plots_output_path = plots_output_path)

ls(stations_annual_summary)

    vfx_distroPlotH(stations_annual_summary,
                    x_var = num_columns,
                    x_var_tag = "Number of Columns",
                    plots_output_path = plots_output_path)

    vfx_distroPlotH(stations_annual_summary,
                    x_var = avg_num_obs_per_day,
                    x_var_tag = "Average Number of Observations Per Day",
                    plots_output_path = plots_output_path)

    # Impute missing daily records
    ## Vis: missing values gapsize and number of stations ----
    # source(here::here("data_preparation",
    #                   "0504_vis_stations_lineplot_by_qc_threshold.R"), local=T)
    
    ggplot(all_daily_summaries_inf2na, aes(
      x = YYYY,
      group = YYYY,
      y = num_columns_day)) +
      geom_boxplot() +
      #geom_jitter() +
      labs(title = paste0("Climatological Data Table Structure - US (2008 - 2022)")) +
      labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)"
      )) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      #scale_y_continuous(#labels = percent,
      # breaks = scales::pretty_breaks(n = 8)) +
      #scale_x_continuous(breaks = scales::pretty_breaks(n = 12))
      theme_bw() 
    #labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)")) +
    #coord_flip()
    
    # ggplot(all_daily_summaries_inf2na, aes(y=num_columns_day)) +
    #   geom_histogram() +
    #   coord_flip()