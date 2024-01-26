# Latest update Oct 2023

## Completeness Plot ----
vfx_completenessPlotH <- function(segmented_df,
                                  x_var = Temperature,
                                  x_var_tag = "Temperature",
                                  title_text = "Distribution of Variable Completeness",
                                  subtitle_text = "NOAA Integrated Surface Database (ISD)",
                                  x_var_lower_bound = .1,
                                  plots_output_path = plots_output_path)
{
  #fmtExpLg10 <- function(x) paste(plyr::round_any(x * 10, 0.1) , "%", sep="")
  
  x_var_enquo <- enquo(x_var)
  
  completeness_plot <-
    ggplot(segmented_df) +
    #geom_boxplot(aes(
    #geom_violin(width=1.4) +
    geom_boxplot(aes(
      y = !!x_var_enquo,
      x = YYYY,
      group = YYYY
    ),
    ) +
    #coord_cartesian(ylim = c(x_var_lower_bound, 1)) +
    #coord_flip(ylim = c(x_var_lower_bound, 1)) +
    labs(x = "Year",
         y = "Completeness Percentage (exp)"
    ) +
    labs(title = paste0(title_text," - ", x_var_tag)) +
    labs(subtitle = paste0(subtitle_text, "")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_y_continuous(labels = percent,
                       trans = 'exp',
                       breaks = scales::pretty_breaks(n = 10)) +
    #scale_y_sqrt() + 
    #scale_y_log10() +
    
    scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
  
  completeness_plot
  
  completeness_plot_fn <- paste0(plots_output_path, "/",
                                 "completeness_plot_H_",
                                 x_var_tag,
                                 ".jpg")
  
  ggsave(completeness_plot_fn,
         plot = completeness_plot, dpi = 300,
         width = 22, height = 20, units = "cm")
}

vfx_distroPlotH <- function(segmented_df,
                            x_var = Temperature,
                            x_var_tag = "Temperature",
                            x_var_lower_bound = .7,
                            plots_output_path = plots_output_path)
{
  
  x_var_enquo <- enquo(x_var)
  
  completeness_plot <-
    ggplot(segmented_df) +
    #geom_boxplot(aes(
    geom_violin(aes(
      y = !!x_var_enquo,
      x = YYYY,
      group = YYYY
    ),
    ) +
    #ylim = c(x_var_lower_bound, 1) +
    #coord_cartesian(ylim = c(x_var_lower_bound, 1)) +
    #coord_flip() +
    labs(x = "Year",
         y = "Total (SQRT)"
    ) +
    labs(title = paste0("Climatological Data Table Structure - ", x_var_tag)) +
    labs(subtitle = paste0("NOAA Integrated Surface Database (ISD)"
    )) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_y_continuous(#labels = percent,
                       trans = 'sqrt',
                       breaks = scales::pretty_breaks(n = 8)) +
    #scale_y_sqrt() + 
    #scale_y_log10() +
    
    scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
  
  completeness_plot
  
  completeness_plot_fn <- paste0(plots_output_path, "/",
                                 "completeness_plot_VPlot_sqrt",
                                 x_var_tag,
                                 ".jpg")
  
  ggsave(completeness_plot_fn,
         plot = completeness_plot, dpi = 300,
         width = 22, height = 20, units = "cm")
}


## EHE/ECE Station Based Plots ----

fx_events_barplot <- function(df, output_path,
                              x_var,
                              x_var_label = "Year",
                              y_var,
                              y_var_label,
                              facet_formula = "~ cc_label") {
  x_var <- sym(x_var)
  y_var <- sym(y_var)
  x_axis_n <- length(unique(df[[x_var]]))
  # sym() is used to convert the column names to symbols, and !! is used to unquote them.sym() is used to convert the column names to symbols, and !! is used to unquote them.
  
  barplot <-
    ggplot(df,
           aes(
             x = !!x_var,
             y = !!y_var,
             #group = cc_label,
             group = Event_Type,
             fill = Event_Type
           )) +
    geom_bar(stat="identity",
             position=position_dodge()
    ) +
    scale_fill_manual(values = c("lightblue", "orange")) +
    # scale_color_sjplot(name = "Completeness Criteria",
    #                    palette = "system") +
    
    guides(x =  guide_axis(angle = 45)) +
    
    labs(title = paste0("Extreme Events Frequency")) +
    labs(subtitle = paste0("Contiguous U.S. (", time_period, ")")) +
    labs(caption = paste0("Data Source: NOAA Integrated Surface Database (ISD)")) +
    
    labs(
      x = x_var_label,
      y = y_var_label
    ) +
    #theme_classic() +
    theme_bw() +
    #theme(legend.position = "bottom") +
    facet_grid("Event_Type ~ EF_distribution_set") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = x_axis_n), expand = c(0.03, 0.03)) +
    scale_y_continuous(#labels = percent,
      #limits = c(0, 600),
      breaks = scales::pretty_breaks(n = 12))
  
  
  
  plot_file_name <- paste0(plots_output_path, "/",
                           params_suffix, "_",
                           y_var,
                           substitute(df),
                           x_var_label,
                           "_barplot",
                           ".jpg")
  ggsave(plot_file_name,
         plot = barplot, dpi = 300,
         width = 32, height = 18, units = "cm")
  
  return(barplot)
}


fx_lineplot <- function(df, output_path,
                        x_varo,
                        y_varo,
                        y_var_label,
                        facet_formula = "~ cc_label") {
  
  x_var <- sym(x_varo)
  y_var <- sym(y_varo)
  x_axis_n <- length(unique(df[[x_var]]))
  
  # sym() is used to convert the column names to symbols, and !! is used to unquote them.sym() is used to convert the column names to symbols, and !! is used to unquote them.
  
  lineplot <-
    ggplot(df,
           aes(
             x = !!x_var,
             y = !!y_var,
             group = Event_Type,
             color = Event_Type
           )
    ) +
    #geom_bar()+
    geom_point() +
    geom_line() +
    scale_color_manual(values = c("#069DAF", "#B42020")) +
    geom_smooth(method=lm, color = "darkgreen", linetype="dashed") +
    #annotate("text", #x = 5.5, y = 30,
    #        label = paste0("R^2 == ", round(summary(lm(x_varo ~ y_varo, data = df))$r.squared, 3))) +
    #scale_shape_manual(values = pshape_set) +
    
    #scale_color_carto_d(name = "Completeness Criteria",
    #                   palette = "Antique") +
    #scale_color_sjplot(name = "Completeness Criteria",
    #                  palette = "system") +
    
    guides(x =  guide_axis(angle = 45)) +
    
    labs(title = paste0("Extreme Events Characteristics: ", y_var_label)) +
    labs(subtitle = paste0("Contiguous U.S. (", time_period, ")")) +
    labs(caption = paste0("Data Source: NOAA Integrated Surface Database (ISD)")) +
    
    labs(
      x = "Year",
      y = y_var_label
    ) +
    #theme_classic() +
    theme_bw() +
    #theme(legend.position = "bottom") +
    facet_grid("Event_Type ~ EF_distribution_set") +
    scale_y_continuous(#labels = percent,
      breaks = scales::pretty_breaks(n = 20),
      limits = c(NA, NA)
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = x_axis_n), expand = c(0.03, 0.03))
  
  plot_file_name <- paste0(plots_output_path, "/",
                           params_suffix, "_",
                           y_var,
                           substitute(df),
                           x_var,
                           "_lineplot",
                           ".jpg")
  ggsave(plot_file_name,
         plot = lineplot, dpi = 300,
         width = 32, height = 18, units = "cm")
  
  return(lineplot)
}

fx_lineplot_errbar <- function(df, plots_output_path,
                               x_var,
                               y_var,
                               y_var_label,
                               y_var_error,
                               facet_formula) {
  
  x_var <- sym(x_var)
  x_axis_n <- length(unique(df[[x_var]]))
  
  y_var <- sym(y_var)
  y_var_error <- sym(y_var_error)
  # sym() is used to convert the column names to symbols, and !! is used to unquote them.sym() is used to convert the column names to symbols, and !! is used to unquote them.
  
  lineplot <-
    ggplot(df,
           aes(
             x = !!x_var,
             y = !!y_var,
             group = Event_Type,
             color = Event_Type
           )
    ) +
    #geom_bar()+
    geom_point() +
    geom_line() +
    scale_color_manual(values = c("#069DAF", "#B42020")) +
    geom_errorbar(aes(ymin=!!y_var - !!y_var_error,
                      ymax=!!y_var + !!y_var_error),
                  width=.2) +
    
    # scale_color_sjplot(name = "Completeness Criteria",
    #                    palette = "system") +
    
    guides(x =  guide_axis(angle = 45)) +
    
    labs(title = paste0("Extreme Events Characteristics: ", y_var_label)) +
    labs(subtitle = paste0("Contiguous U.S. (", time_period, ")")) +
    labs(caption = paste0("Data Source: NOAA Integrated Surface Database (ISD)")) +
    
    labs(
      x = x_var,
      y = y_var_label
    ) +
    #theme_classic() +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_grid("Event_Type ~ EF_distribution_set") +
    #facet_grid(facet_formula) +
    scale_y_continuous(#labels = percent,
      breaks = scales::pretty_breaks(n = 20),
      limits = c(NA, NA)
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = x_axis_n), expand = c(0.03, 0.03))
  
  plot_file_name <- paste0(plots_output_path, "/",
                           params_suffix, "_",
                           y_var,
                           substitute(df),
                           x_var,
                           "_line_errbar",
                           ".jpg")
  ggsave(plot_file_name,
         plot = lineplot, dpi = 300,
         width = 32, height = 22, units = "cm")
  
  return(lineplot)
}

fx_histboxplot <- function(df, #col_list, 
                              facet_col = NULL, 
                              output_path = plots_output_path,
                              params_suffix = params_suffix) {
  library(purrr)
  library(patchwork)
  library(ggthemes)
  library(ggplot2)
  
  subfolder_name <- paste(substitute(df), "variables_distribution_by", 
                          params_suffix, facet_col, sep = "_")

  dir.create(file.path(output_path, subfolder_name)) 
  distplot_outputs_path <- file.path(output_path, subfolder_name)
  
  print(distplot_outputs_path)

  fx_reportFieldsPar(
    df,
    
    prefix = params_suffix,
    suffix = substitute(df),
    output_path = distplot_outputs_path)
  
  df <- df %>% dplyr::select(matches(facet_col),
                             where(is.numeric))

  col_list <- names(select_if(df, is.numeric))
  print(col_list)
  
  # Calculate the count of observations per facet
  count_data <- df %>% 
    group_by(.data[[facet_col]]) %>%
    summarise(count = n())
  
  plots <- map(col_list, function(col_name) {
    binwidth <- 1
    
    # Create a ggplot histogram
    hist_plot <- ggplot(df, aes(x = .data[[col_name]])) +
      # geom_histogram(binwidth = binwidth, 
      #                fill = "gray90", color = "black") +
      labs(title = paste0("Variable: ", col_name),
           subtitle = paste0("N = ", nrow(df)), 
           x = col_name) +
      geom_density(aes(y=..scaled..), fill = "transparent", 
                   color = "blue", linewidth = 1) + # Add density curve
      scale_x_continuous(breaks = scales::pretty_breaks(n = 11), expand = c(0.03, 1)) +
      
      geom_vline(aes(xintercept = mean(.data[[col_name]], na.rm = TRUE)), 
                 color = "red3", linetype = "solid", linewidth = 1) + # Add a line for mean
      geom_vline(aes(xintercept = median(.data[[col_name]], na.rm = TRUE)), 
                 color = "green4", linetype = "dotdash", linewidth = 1) + # Add a line for median
      theme_bw()
    
    # Create a ggplot boxplot
    boxplot_plot <- ggplot(df, aes(y = "", 
                                   x = df[[col_name]])) +
      geom_boxplot(fill = "white", color = "black", width = 1.1) +
      geom_jitter(alpha = 0.01, color = "gray18") +
      geom_vline(aes(xintercept = mean(.data[[col_name]], na.rm = TRUE)), 
                 color = "red1", linetype = "solid", linewidth = 1) + 
      geom_vline(aes(xintercept = median(.data[[col_name]], na.rm = TRUE)), 
                 color = "green3", linetype = "dashed", linewidth = 1) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 11), expand = c(0.03, 2)) +
      geom_text(data = count_data, aes(label = paste0(" (N = ", count, ")")),
                color = "blue", x = Inf, y = Inf, hjust = 1, vjust = 1, linewidth = 4) +
      labs(y = facet_col, x = col_name) +
      theme_minimal()
      #theme_void()
      #theme_bw()
      #theme_tufte()
    
    # Combine the histogram and boxplot
    #combined_plot <- hist_plot + boxplot_plot + plot_layout(ncol = 1)
    # Facet the plot based on the specified column
    if (!is.null(facet_col)) {
      
      combined_plot <- hist_plot + boxplot_plot +
        facet_wrap(vars(.data[[facet_col]]), ncol = 1)
    } else {
      combined_plot <- hist_plot + boxplot_plot
    }
    
    # Return the combined plot
    return(combined_plot)
  })
  
  # Save the plots as JPEG files
  plot_file_names <- file.path(distplot_outputs_path,
                               paste0(
                                      col_list, "_",
                                      #substitute(df),
                                      "_histobox_",
                                      facet_col,
                                      ".jpg"))
  
  map2(plots, plot_file_names, function(p, file_name) {
    ggsave(file_name, plot = p, 
           width = 15, height = 9)
  })
}

vfx_histocomparison <- function(df_raw = stations_daily_means_compiled,
                                df = EHE_ECE_compiled,
                                col_list,
                                output_path = plots_output_path,
                                params_suffix = params_suffix) {
  library(purrr)
  library(patchwork)
  library(ggthemes)
  library(ggplot2)
  
  subfolder_name <- paste(substitute(df), params_suffix , sep = "_")
  
  dir.create(file.path(output_path, subfolder_name)) 
  distplot_outputs_path <- file.path(output_path, subfolder_name)
  
  print(distplot_outputs_path)
  
  df1 <- df %>% dplyr::filter(Event_Type == "Extreme Heat Event")
  df2 <- df %>% dplyr::filter(Event_Type == "Extreme Cold Event")
  
  df_n <- length(unique(df_raw$DATE))
  df1_n <- length(unique(df1$DATE))
  df2_n <- length(unique(df2$DATE))
  
  plots <- map(col_list, function(col_name) {
  distro_plot <- ggplot() +
    geom_density(data = df_raw, 
                 aes(x = .data[[col_name]], fill = "All days"), alpha = 0.8) +
    geom_density(data = df1, 
                 aes(x = .data[[col_name]], fill = "EHE days"), alpha = 0.4) +
    
    geom_density(data = df2, 
                 aes(x = .data[[col_name]], fill = "ECE days"), alpha = 0.4) +
    
    labs(title = paste0("Distribution of ", col_name)) +
    scale_fill_manual(values = c("All days" = "darkgreen", 
                                 "EHE days" = "red4",
                                 "ECE days" = "blue2"),
                      labels = c(paste0("All days, N: ", df_n), 
                                 paste0("ECE days, N: ", df2_n),
                                 paste0("EHE days, N: ", df1_n)),
                      name = "Distribution set")
  
  return(distro_plot)})
  
  # Save the plots as JPEG files
  plot_file_names <- file.path(distplot_outputs_path,
                               paste0(
                                 col_list, "_",
                                 params_suffix, 
                                 "_distroplot_",
                                 ".jpg"))
    map2(plots, plot_file_names, function(p, file_name) {
      ggsave(file_name, plot = p, 
             dpi = 300, units = "cm", 
             width = 32, height = 24)
    })
  
}

vfx_histocomparison_scales <- function(df_sc1 = conus_counties_events_catalog_2008_2022_dt,
                                       df_sc2 = conus_states_events_catalog_2008_2022_dt,
                                       sc1_label = "counties",
                                       sc2_label = "states",
                                col_list,
                                output_path = plots_output_path,
                                params_suffix = params_suffix) {
  library(purrr)
  library(patchwork)
  library(ggthemes)
  library(ggplot2)
  
  subfolder_name <- paste(sc1_label, "vs",
                          sc2_label, sep = "_")
  
  dir.create(file.path(output_path, subfolder_name)) 
  distplot_outputs_path <- file.path(output_path, subfolder_name)
  
  print(distplot_outputs_path)
  
  df_sc1_h <- df_sc1 %>% dplyr::filter(event_type == "Extreme Heat Event")
  df_sc1_c <- df_sc1 %>% dplyr::filter(event_type == "Extreme Cold Event")
  
  df_sc2_h <- df_sc2 %>% dplyr::filter(event_type == "Extreme Heat Event")
  df_sc2_c <- df_sc2 %>% dplyr::filter(event_type == "Extreme Cold Event")
  
  # df_n <- length(unique(df_raw$DATE))
  # df1_n <- length(unique(df1$df_sc1))
  # df2_n <- length(unique(df2$df_sc1))
  
  plots <- map(col_list, function(col_name) {
    distro_plot <- ggplot() +
      geom_density(data = df_sc1_h, 
                   aes(x = .data[[col_name]], y=..scaled..,
                       fill = "EHE days 1"), alpha = 0.4) +
      geom_density(data = df_sc1_c, 
                   aes(x = .data[[col_name]], y=..scaled..,
                       fill = "ECE days 1"), alpha = 0.4) +

      geom_density(data = df_sc2_h, 
                   aes(x = .data[[col_name]], y=..scaled..,
                       fill = "EHE days 2"), alpha = 0.4) +
      geom_density(data = df_sc2_c, 
                   aes(x = .data[[col_name]], y=..scaled..,
                       fill = "ECE days 2"), alpha = 0.4) +
      
      labs(title = paste0("Distribution of ", col_name)) +
      scale_fill_manual(values = c("EHE days 1" = "red4",
                                   "ECE days 1" = "blue2",
                                   "EHE days 2" = "orange2",
                                   "ECE days 2" = "cyan3"),
                         labels = c(paste0("ECE days, ", sc1_label),
                                    paste0("ECE days, ", sc2_label),
                                    paste0("EHE days, ", sc1_label),
                                    paste0("EHE days, ", sc2_label)),
                        name = "Distribution set")
    
    return(distro_plot)})
  
  # Save the plots as JPEG files
  plot_file_names <- file.path(distplot_outputs_path,
                               paste0(
                                 col_list, "_",
                                 params_suffix, 
                                 "_distroplot_",
                                 ".jpg"))
  map2(plots, plot_file_names, function(p, file_name) {
    ggsave(file_name, plot = p, 
           dpi = 300, units = "cm", 
           width = 32, height = 24)
  })
  
}

# EHE/ECE IDW Visualization----
fx_ehe_ece_iwd_plot <- function(output_path,
                                terralayer, 
                                idw_ehce_boundaries_sf,
                                map_divisions = map_divisions,
                                selected_day = selected_day,
                                idw_var_name = "idw_var_name", 
                                caption = "Data Source: NOAA Integrated Surface Database (ISD)") {
  library(wesanderson)
  library(tidyterra)
  
  pal <- wes_palette("Zissou1", 100, type = "continuous")
  
  
  # Define the colors
  red_to_brown <- colorRampPalette(c("orange", "brown"))
  
  green_to_blue <- colorRampPalette(c("lightblue", "blue"))
  
  interpolated_EHCE_plot <-  ggplot() +
    geom_spatraster(data = terralayer) +
    geom_spatraster_contour(data = terralayer, breaks = seq(-100, 100, 20), lwd = .1) +
    #idw_ehce_terra_contour
    #     scale_fill_gradient2(low = "blue", mid = "white", high = "red",
    #                        midpoint = 0, 
    #                       na.value = NA) +
    #    scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"), 
    #midpoint = 0,
    #                       values=rescale(c(-100,
    #                                       0-.Machine$double.eps,
    #                                      0,
    #                                     0+.Machine$double.eps,
    #                                    100))) +
  #scale_fill_gradientn(colors = pal,
  # EHMI/ECMI intensity colors + + + ----
  scale_fill_gradientn(colors=c("purple", "navy", "blue", "cyan", "lightblue",
                                "white", 
                                #"green3", "gray80", "darkgreen",
                                "khaki", "orange", "coral1", "red3", "brown4"), 
                       na.value = NA,
                       limits = c(-100, 100)) +
    labs(fill="Normalized Intensity Indicator (EHMI/ECMI)") +
    # Extreme Heat/Cold Events\n
    geom_sf(data = map_divisions,
            fill = NA,
            color = "Black",
            lwd = .1) +
    
    # geom_sf(data = idw_ehce_contours,
    #         color = "Black",
    #         lwd = .1) +
    
    labs(title = paste0("Extreme Event Date: ",  selected_day)) +
    #labs(subtitle = paste0(n, " station(s) impacted within Contiguous U.S.")) +# (", time_period, ")")) +
    labs(caption = caption) +
    
    geom_sf(data = idw_ehce_boundaries_sf,
            fill = NA,
            color = "green4",
            lwd = .1,
            linetype = "11", size = 1.5) +
    
    geom_sf(data = station_points_SF,
            color = "Black",
            alpha = 0.1,
            size = .05)  +
    # geom_text(data = station_points_SF_day, aes(x = lon, y = lat, label = EHCMI_normalized),
    #           nudge_x = 0.1, size = 2, color = "black", fontface = "bold") +
    # coord_sf(crs = st_crs(spatial_projection_lonlat)) +
    
    theme_bw() +
    theme(legend.position = "bottom")
  
  plot_file_name <- paste0(plots_output_path, "/",
                           "ehe_ece_iwd_",
                           #"_contours_overlay_",
                           selected_day,
                           ".jpg")
  
  plot_file_name_collector <- paste0(idw_outputs, "/",
                                     "ehe_ece_iwd_",
                                     idw_var_name, "_",
                                     #"_contours_overlay_",
                                     selected_day,
                                     ".jpg")
  
  tic()
  # ggsave(plot_file_name,
  #        plot = interpolated_EHCE_plot,
  #        dpi = 300,
  #        width = 32, height = 24, units = "cm")
  
  ggsave(plot_file_name_collector,
         plot = interpolated_EHCE_plot,
         dpi = 300,
         width = 32, height = 24, units = "cm")
  
  
  toc()
  #print(fx_toc(interpolated_EHCE_plot, 0, selected_day_label))
}

fx_ehe_ece_admin_geo_plot <- function(admin_geo_compiled_polygons, output_path,
                                      selected_day = selected_day,
                                      admin_geo_laebl = admin_geo_laebl,
                                      caption = "Data Source: NOAA Integrated Surface Database (ISD)") {
  library("wesanderson")
  
  pal <- wes_palette("Zissou1", 100, type = "continuous")
  
  
  # Define the colors
  red_to_brown <- colorRampPalette(c("orange", "brown"))
  
  green_to_blue <- colorRampPalette(c("lightblue", "blue"))
  
  interpolated_EHCE_plot <-  ggplot() +
    #geom_spatraster(data = terralayer) +
    #geom_spatraster_contours(data = terralayer, breaks = seq(-100, 100, 20)) +
    #idw_ehce_terra_contours
    #     scale_fill_gradient2(low = "blue", mid = "white", high = "red",
    #                        midpoint = 0, 
    #                       na.value = NA) +
    #    scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"), 
    #midpoint = 0,
    #                       values=rescale(c(-100,
    #                                       0-.Machine$double.eps,
    #                                      0,
  #                                     0+.Machine$double.eps,
  #                                    100))) +
  #scale_fill_gradientn(colors = pal,
  
  scale_fill_gradientn(colors=c(
    #"white", # "khaki", 
    "darkgreen", "lightgreen", 
    "orange2", "coral2"), 
    na.value = NA,
    limits = c(0, 1)) +
    labs(fill="Proportion of Impacted Area") +
    # Extreme Heat/Cold Events\n
    geom_sf(data = admin_geo_compiled_polygons,
            aes(fill = impacted_to_total_ratio),
            color = "Black",
            lwd = .1) +
    
    geom_sf(data = map_divisions,
            fill = NA,
            color = "Black",
            lwd = .2) +
    
    geom_sf(data = idw_ehce_boundaries_sf,
            fill = NA,
            color = "brown4",
            lwd = .2) +
    
    labs(title = paste0("Extreme Event Date: ",  selected_day)) +
    labs(subtitle = paste0(nrow(admin_geo_compiled_polygons),
                           " ", admin_geo_laebl,
                           " impacted within Contiguous U.S.")) +# (", time_period, ")")) +
    labs(caption = caption) +
    
    geom_sf(data = station_points_SF,
            color = "Black",
            alpha = 0.3,
            size = .12)  +
    theme_bw() + 
    theme(legend.position = "bottom")
  
  plot_file_name <- file.path(impacted_geo_plots_outputs, 
                              paste0("ehe_ece_impacted_",
                                     selected_day, "_",
                                     admin_geo_laebl,
                                     ".jpg"))
  
  # plot_file_name_collector <- paste0(idw_outputs, "/",
  #                                    "ehe_ece_iwd_",
  #                                    #"_contours_overlay_",
  #                                    selected_day,
  #                                    ".jpg")
  
  tic()
  ggsave(plot_file_name,
         plot = interpolated_EHCE_plot,
         dpi = 200,
         width = 28, height = 21, units = "cm")
  
  # ggsave(plot_file_name_collector,
  #        plot = interpolated_EHCE_plot,
  #        dpi = 300,
  #        width = 32, height = 24, units = "cm")
  
  #print(fx_toc(interpolated_EHCE_plot, 0, selected_day_label))
  toc()
}

fx_viz_agg_15years_plot <- function(data, 
                                    geo_label = "Spatial_Scale",
                                    colors_pal = colors_pal) {
  data_ehe <- data %>% 
    dplyr::filter(event_type == "Extreme Heat Event")
  
  data_ece <- data %>% 
    dplyr::filter(event_type == "Extreme Cold Event")
  
  max_days_ehe = max(data_ehe$total_event_days, na.rm = T)
  max_days_ece = max(data_ece$total_event_days, na.rm = T)
  
  colors_pal_ehe <- c(
    #"darkgreen","green3", #"lightgreen",
    "white", "yellow1",
    "yellow4", "orange",
    "tan4", "red3",
    "brown1", "brown4")

  colors_pal_ece <- c(
    "white", "lightblue", "cyan1", 
    "lightgreen", 
    "cyan4",
    "blue", "navy")

    # colors_pal <- c(
    # "white",
    # #"darkgreen","green3", "lightgreen",
    # "yellow2", "orange",
    # "tan2", "red3", "brown4")
  
  # Function to create breaks for color scales
  create_breaks <- function(max_days) {
    seq(0, max_days, by = 20)
  }
  
  agg_plot_ehe <- ggplot() +
    geom_sf(data = data_ehe,
            aes(fill = total_event_days),
            lwd = .1) + 
    # scale_fill_gradientn(colors = colors_pal_ehe,
    #                      na.value = NA,
    #                      limits = c(0, max_days_ehe)) +
    
    scale_fill_stepsn(
      colors = colors_pal_ehe,
      breaks = create_breaks(max_days_ehe),
      na.value = NA,
      limits = c(0, max_days_ehe)) +

    labs(fill = "Total Number of Days Impacted by Extreme Heat Events") +
    #coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
    
    theme_void() + 
    theme(
      legend.position = "bottom",
      legend.key.width = unit(0.08, "npc") # Legend keys span the map width
    )
  
  agg_plot_ece <- ggplot() +
    geom_sf(data = data_ece,
            aes(fill = total_event_days),
            lwd = .1) + 
    # scale_fill_gradientn(colors = colors_pal_ece,
    #                      na.value = NA,
    #                      limits = c(0, max_days_ece)) +
    scale_fill_stepsn(
      colors = colors_pal_ece,
      breaks = create_breaks(max_days_ece),
      na.value = NA,
      limits = c(0, max_days_ece)) +
    
    
    labs(fill = "Total Number of Days Impacted by Extreme Cold Events") +
    #coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
    theme_void() + 
    theme(
      legend.position = "bottom",
      legend.key.width = unit(0.08, "npc") # Legend keys span the entire width
    )
  
  plot_file_name_ehe <- file.path(runtime_path, 
                              paste0(geo_label, "_",
                                     nrow(data_ehe),
                                     "_sp_objects_(map_of_total_ehe_days",
                                     "_in_15_years)",
                                     ".jpg"))
  ggsave(plot_file_name_ehe,
         plot = agg_plot_ehe, dpi = 300,
         width = 22, height = 18, units = "cm")
  
  plot_file_name_ece <- file.path(runtime_path, 
                                  paste0(geo_label, "_",
                                         nrow(data_ece),
                                         "_sp_objects_(map_of_total_ece_days",
                                         "_in_15_years)",
                                         ".jpg"))
  ggsave(plot_file_name_ece,
         plot = agg_plot_ece, dpi = 300,
         width = 22, height = 18, units = "cm")
}


fx_cumulative_impact_plot <- function(output_path,
                                      cumulative_grid_terra, 
                                map_divisions = map_divisions,
                                year = start_year,
                                output_filename = output_filename,
                                cumulative_event_coverage = cumulative_event_coverage,
                                idw_var_name = "EHE / ECE",
                                plot_ranges = c(0, 40L, 5L),
                                caption = "Data Source: NOAA Integrated Surface Database (ISD)") {
  library(tidyterra)
  
  #lower_limit <- minmax(cumulative_grid_terra)[1,1]
  #upper_limit <- minmax(cumulative_grid_terra)[2,1]
  max_days <- round(minmax(cumulative_grid_terra)[2,1], 0)
  lower_limit <- plot_ranges[1]
  upper_limit <- plot_ranges[2]
  contour_seq <- plot_ranges[3]
  
  if (idw_var_name == "EHE") {
    colors_pal <- c(
      "orange", "coral1", "brown2", "brown4", "red4")
    
  } else if (idw_var_name == "ECE") { 
    colors_pal <- c(
      "cyan", "blue", "navy", "purple1", "purple4")
    
  } else if (idw_var_name == "EHCE") { 
    #upper_limit <- 55L
    colors_pal <- c(
 	    "tan", "yellow", "orange", "coral1",
       #"cyan4", "navy", 
       "brown2", "red4", "purple4", "black")
      #viridis(10, option = "magma")
    
  } else {
    #contour_seq <- 30L
    colors_pal <- c("navy", "blue", "cyan4", "lightgreen", 
                    #"white", 
                    "yellow", 
                    "tan1", "orange2", "brown2", "red4")
    
    lower_limit <- round(minmax(cumulative_grid_terra)[1,1], 1)
    
    upper_limit <- round(minmax(cumulative_grid_terra)[2,1], 1)
    
  }
  
  cumulative_impact_plot <-  ggplot() +
    geom_sf(data = map_divisions,
            fill = "gray85",
            color = NA) +
    
    geom_spatraster(data = cumulative_grid_terra) +
    geom_spatraster_contour(data = cumulative_grid_terra, 
                            breaks = seq(-1600, 1600, contour_seq), lwd = .1) +
    scale_fill_gradientn(colors = colors_pal,
                       na.value = NA,
                       limits = c(lower_limit, upper_limit)) +
    
    labs(fill="Number of impacted days ") +
    # Extreme Heat/Cold Events\n
    geom_sf(data = map_divisions,
            fill = NA,
            color = "Black",
            lwd = .1) +

    # labs(title = paste0("Percentage of Area Impacted by ", 
    #                     idw_var_name,
    #                     " ", 
    #                     cumulative_event_coverage, "%")) +
     labs(subtitle = paste0("Year: ",  year)) +
     labs(caption = paste0("Max impact: ", max_days)) +
    
    geom_sf(data = station_points_SF,
            color = "Black",
            alpha = 0.1,
            size = .05)  +
    
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(0.08, "npc") # Legend keys span the map width
    )
  
  plot_file_name <- paste0(plots_output_path, "/",
                           output_filename,
                           #idw_var_name,
                           "__", year, "  (", 
                           cumulative_event_coverage, "%)  [max ",
                           max_days, "]",
                           ".jpg")

  ggsave(plot_file_name,
         plot = cumulative_impact_plot,
         dpi = 300,
         width = 32, height = 24, units = "cm")
}


fx_delta_boxplot <- function(df = long_table_df, y_var = y_var,
                             subset_lebel, subset_value, 
                             i, plots_output_path) {
  
  y_var <- sym(y_var)
  
  dir.create(file.path(sample_output_path, paste0(y_var, "_boxplot")))
  y_var_output_path <- file.path(sample_output_path, paste0(y_var, "_boxplot"))
  
  delta_boxplot <- ggplot(df,
                                 aes(x = Method,
                                     y = !!y_var,
                                     fill = Method
                                 )) +
    geom_boxplot(lwd = 0.3,
                 fatten = 0.42,
                 outlier.size = 0.1) +
    guides(x = guide_axis(angle = 45)) +
    theme(legend.position = "none") +
    labs(y = "Estimated Ambient Temperature Error ") +
    labs(title = "Difference of the measured and estimated Ambient Temperature") +
    labs(subtitle = paste0(subset_lebel, ": ", subset_value))
    theme_classic()
  
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Estimated_T_Boxplot_Comparison_",
                           y_var, "_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = delta_boxplot,
         dpi = 300, units = "cm",
         width = 18, height = 14)
  
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Estimated_T_Boxplot_Comparison_byCRegion_",
                           y_var, "_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = delta_boxplot + facet_grid(. ~ Climate_Region),
         dpi = 300, units = "cm",
         width = 28, height = 16)
  
  plot_file_name <- paste0(plots_output_path, "/",
                           "Estimated_T_Boxplot_Comparison_byState_",
                           y_var, "_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = delta_boxplot + facet_grid(state ~ .),
         dpi = 300, units = "cm",
         width = 20, height = 125)
  
}


fx_delta_densityplot <- function(df = long_table_df, y_var = y_var,
                                 subset_lebel, subset_value, 
                                 i, plots_output_path) {
  
  y_var <- sym(y_var) # this corresponds to the Tavg, Tavg_NN in the long table
  
  dir.create(file.path(sample_output_path, paste0(y_var, "_densityplot")))
  y_var_output_path <- file.path(sample_output_path, paste0(y_var, "_densityplot"))
  
  delta_densityplot <- ggplot(df,
                                 aes(x=!!y_var)) + 
    geom_density(
      aes(group=Method,
          color=Method),
      alpha=0.3) +
    theme_light() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank()) +
    guides(x =  guide_axis(angle = 45)) +
    labs(title = "Difference of the measured and estimated Ambient Temperature") +
    labs(subtitle = paste0(subset_lebel, ": ", subset_value))
  
  
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Estimated_T_Distribution_Comparison_",
                           y_var, "_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = delta_densityplot,
         dpi = 300, units = "cm",
         width = 18, height = 14)
  
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Estimated_T_Distribution_Comparison_byCRegion_",
                           y_var, "_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = delta_densityplot + facet_grid(. ~ Climate_Region),
         dpi = 300, units = "cm",
         width = 28, height = 16)
  
  plot_file_name <- paste0(plots_output_path, "/",
                           "Estimated_T_Distribution_Comparison_byState_",
                           y_var, "_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = delta_densityplot + facet_grid(state ~ .),
         dpi = 300, units = "cm",
         width = 20, height = 125)
}

fx_r2_plot <- function(data = long_table_df, 
                       y_var = y_var, 
                       predicted_y_var = predicted_y_var,
                       subset_lebel, subset_value, 
                       i, plots_output_path) {
  
  dir.create(file.path(sample_output_path, paste0(predicted_y_var, "_scatterplot")))
  y_var_output_path <- file.path(sample_output_path, paste0(predicted_y_var, "_scatterplot"))
  
  # Calculate R-squared
   correlation <- cor(data[[y_var]], data[[predicted_y_var]])
   r_squared <- correlation^2
  
  # result <- data %>%
  #   group_by({{subset_value}}) %>%
  #   summarise(correlation = cor({{y_col}}, {{predicted_y_col}}),
  #             r_squared = cor({{y_col}}, {{predicted_y_col}})^2)
  
  # Create the scatterplot
  r2_plot <- ggplot(data, aes(x = .data[[y_var]], y = .data[[predicted_y_var]])) +
    geom_point(alpha = .2) +
    geom_smooth(method = "lm", color = "blue") +
    labs(x = y_var, y = predicted_y_var) +
    annotate("text", x = max(data[[y_var]]), y = min(data[[predicted_y_var]]),#
    #annotate("text", x = max(data[[y_var]], na.rm = TRUE), y = min(data[[predicted_y_var]], na.rm = TRUE),
             label = paste("R2 =", round(r_squared, 2)), hjust = 1) +
             #label = paste("R2 =", round(result$r_squared, 2)), hjust = 1) +
    labs(title = paste0("Scatterplot of Actual vs. Predicted Temperature")) +
    labs(subtitle = paste0(subset_lebel, ": ", subset_value)) +
    theme_minimal()
  
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Scatterplot_of_Tavg_vs__[",
                           predicted_y_var, "]_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = r2_plot,
         dpi = 300, units = "cm",
         width = 18, height = 14)
  
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Scatterplot_of_Tavg_vs__[",
                           predicted_y_var, "]_byCRegion_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = r2_plot + facet_grid(. ~ Climate_Region),
         dpi = 300, units = "cm",
         width = 28, height = 16)
  
  plot_file_name <- paste0(plots_output_path, "/",
                           "Scatterplot_of_Tavg_vs__[",
                           predicted_y_var, "]_byState_[",
                           subset_lebel, "]_(", subset_value,
                           ")_sample_", i,
                           ".jpg")
  ggsave(plot_file_name,
         plot = r2_plot + facet_grid(state ~ .),
         dpi = 300, units = "cm",
         width = 20, height = 125)
}



fx_r2_plot2 <- function(data, y_var, predicted_y_var, subset_label, subset_value, i, plots_output_path) {
  # Create output directory if it doesn't exist
  #dir.create(file.path(plots_output_path, paste0(predicted_y_var, "_scatterplot2")), recursive = TRUE, showWarnings = FALSE)
  #y_var_output_path <- file.path(plots_output_path, paste0(predicted_y_var, "_scatterplot2"))
  
  # Calculate R-squared for each facet based on 'Climate_Region' and 'state'
  r_squared_data <- data %>%
    group_by(data[[subset_label]], Climate_Region, state) %>%
    summarise(correlation = cor(.data[[y_var]], .data[[predicted_y_var]], use = "complete.obs"),
              r_squared = correlation^2) %>%
    ungroup()
  
  # Merge the R-squared values back into the original dataframe
  data_with_r2 <- data %>%
    left_join(r_squared_data, by = c(subset_label, "Climate_Region", "state"))
  
  # Create the scatterplot with best-fit lines and no overall R-squared annotation
  r2_plot_base <- ggplot(data_with_r2, aes(x = .data[[y_var]], y = .data[[predicted_y_var]])) +
    geom_point(alpha = .2) +
    geom_smooth(method = "lm", color = "blue") +
    labs(x = y_var, y = predicted_y_var,
         title = paste0("Scatterplot of ", y_var, " vs. Predicted ", predicted_y_var),
         subtitle = paste(subset_label, subset_value)) +
    theme_minimal()
  
  # Create plot by 'Climate_Region'
  plot_file_name <- paste0(y_var_output_path, "/",
                           "Scatterplot2_of_", y_var, "_vs_", predicted_y_var, "_byCRegion_",
                           subset_label, "_(", subset_value, ")_sample_", i, ".jpg")
  ggsave(plot_file_name,
         plot = r2_plot_base + facet_grid(. ~ Climate_Region) +
           geom_text(aes(label = paste0("R² = ", round(r_squared, 2)), x = Inf, y = Inf),
                     data = r_squared_data, hjust = 1, vjust = 1, size = 3, color = "red"),
         dpi = 300, units = "cm", width = 28, height = 16)
  
  # Create plot by 'state'
  plot_file_name <- paste0(plots_output_path, "/",
                           "Scatterplot2_of_", y_var, "_vs_", predicted_y_var, "_byState_",
                           subset_label, "_(", subset_value, ")_sample_", i, ".jpg")
  ggsave(plot_file_name,
         plot = r2_plot_base + facet_grid(state ~ .) +
           geom_text(aes(label = paste0("R² = ", round(r_squared, 2)), x = Inf, y = Inf),
                     data = r_squared_data, hjust = 1, vjust = 1, size = 3, color = "red"),
         dpi = 300, units = "cm", width = 20, height = 125)
}
