my_theme <- theme_classic() + 
  theme(legend.title=element_blank(), 
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.text = element_text(size=12),
        axis.text = element_text(color="black", size=12), 
        axis.title = element_text(color = "black", size=12))

my_theme2 <- theme_classic(base_size=12, base_family='Times New Roman') + 
  theme(legend.text = element_text(size=12),
        legend.position = "right",
        plot.title = element_blank(),
        axis.text = element_text(color="black", size=12), 
        axis.title.x = element_text(color = "black", size=12),
        axis.title.y = element_blank(),
        axis.line=element_line(),
        strip.background = element_blank(),
        strip.text = element_text(color="black", size=12),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines"))

my_theme3 <- theme_classic() + 
  theme(legend.title=element_blank(), 
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.text = element_text(size=12),
        axis.text = element_text(color="black", size=12), 
        axis.title = element_text(color = "black", size=12))

sensitivity_theme <- theme_classic() + 
  theme(legend.text = element_text(size=12),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color="black", size=12), 
        axis.title = element_text(color = "black", size=12),
        axis.line=element_line(),
        strip.background = element_blank(),
        strip.text = element_text(color="black", size=12),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines"))

make_sensitivity_plot2 <- function(data, param, range_vals, plot_labels, bar_labels, labels=TRUE){
  plot_label = plot_labels[[param]]
  bar_label=bar_labels[[param]]
  data %>% 
    mutate(variable = recode_factor(
      variable,
      f_frozen = "Change in frozen perm. (%)",
      Ca = "Change in atm. CO2 (%)",
      CH4 = "Change in atm. CH4 (%)",
      Tgav = "Change in temperature (%)"
    )) %>% 
    filter(param_name==param) ->
    param_data
  
  param_data %>% 
    ggplot() +
    aes(x = year, y = pct_diff, color = param_value, group = param_value) +
    geom_line() +
    facet_rep_wrap(~variable, scales = "fixed", ncol=1, strip.position = "left") +
    guides(color = guide_colorbar(title=bar_label, barwidth = 10, barheight = 1, title.vjust = 0.85)) +
    scale_color_viridis_c() +
    scale_x_continuous("Time (years)") +
    scale_y_continuous(data$variable) +
    sensitivity_theme +
    labs(title=plot_label) +
    ylim(range_vals$min, range_vals$max) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    ylab(NULL) +
    theme(axis.title.y=element_blank()) ->
    plot_out
  
  if (labels==FALSE){
    plot_out <- plot_out + 
      theme(axis.text.y=element_blank(),
            strip.text=element_blank())
  }
  
  #var_list <- unique(data$variable)
  
  #plots <- lapply(var_list, single_var_plot, data = param_data, range = range_vals, 
  #                plot_label = plot_labels[[param]], bar_label=bar_labels[[param]], labels=labels)
  
  #plot_out <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol=1)
  return(plot_out)
}

make_sensitivity_plot3 <- function(data, param, range_vals, plot_labels, bar_labels, labels=TRUE){
  plot_label = plot_labels[[param]]
  bar_label=bar_labels[[param]]
  data %>% 
    mutate(variable = recode_factor(
      variable,
      f_frozen = "Frozen permafrost fraction",
      Ca = "Atmospheric CO2 (ppm)",
      CH4 = "Atmospheric CH4 (ppbv)",
      Tgav = "Temperature anomaly (K)"
    )) %>% 
    filter(param_name==param) ->
    param_data
  
  param_data %>%
    filter(year==2100) %>% 
    ggplot() +
    aes(x = param_value, y = pct_diff, group = param_value) +
    geom_point() +
    facet_rep_wrap(~variable, scales = "fixed", ncol=1, strip.position = "left") +
    scale_color_viridis_c() +
    scale_x_continuous(plot_label) +
    scale_y_continuous(data$variable) +
    sensitivity_theme +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    ylab(NULL) +
    ylim(range_vals$min, range_vals$max) +
    theme(axis.title.y=element_blank()) ->
    plot_out
  
  if (labels==FALSE){
    plot_out <- plot_out + 
      theme(axis.text.y=element_blank(),
            strip.text=element_blank())
  }
  
  return(plot_out)
}