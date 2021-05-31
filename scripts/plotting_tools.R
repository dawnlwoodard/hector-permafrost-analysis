library(ggsci)
library(ggplot2)
library(comprehenr)
library(egg)

update_geom_defaults("line", list(size = 1.0))

#' @param xfrac The fraction over from the left side.
#' @param yfrac The fraction down from the top.
#' @param label The text to label with.
#' @param pos Position to pass to text()
#' @param ... Anything extra to pass to text(), e.g. cex, col.
add_label <- function(xfrac, yfrac, label, pos = 4, ...) {
  u <- par("usr")
  x <- u[1] + xfrac * (u[2] - u[1])
  y <- u[4] - yfrac * (u[4] - u[3])
  text(x, y, label, pos = pos, ...)
}


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
        legend.title = element_blank(),
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

single_plot <- theme_classic(base_size=12, base_family='Times New Roman') + 
  theme(legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = "right",
        plot.title = element_blank(),
        axis.text = element_text(color="black", size=12), 
        axis.title.x = element_text(color = "black", size=12),
        axis.title.y = element_text(color = "black", size=12),
        axis.line=element_line())

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

color_grad_list <- pal_uchicago("light")(9)[c(6,3,4,5)]

add_year_name <- function(data, year_col_name="year"){
  all_years <- data$year
  year_strings <- character(length(all_years))
  for (i in seq(1,length(all_years))){
    year_strings[i] <- toString(all_years[i])
  }
  
  data_out <- data %>% mutate(year_name = year_strings)
  return(data_out)
}

tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + xlim(2000-20,2300) +
    geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}


make_sensitivity_plot_2100 <- function(data, range_vals, plot_labels, labels=TRUE){
  # x_label = plot_labels[[param]]
  # y_label=bar_labels[[param]]
  data %>% 
    mutate(variable = recode_factor(
      variable,
      f_frozen = "Change in frozen perm. (%)",
      Ca = "Change in atm. CO2 (%)",
      CH4 = "Change in atm. CH4 (%)",
      Tgav = "Change in temperature (deg. C)"
    )) %>% 
    # mutate(param_pct=(param_value-param_defaults[[param]])/param_defaults[[param]]) %>% 
    filter(year==2100) ->
    param_data
  
  param_data %>% 
    ggplot() +
    aes(x = param_value, y = diff, color=param_name, group = param_value) +
    geom_line() +
    # facet_rep_wrap(~variable, scales = "fixed", ncol=1, strip.position = "left") +
    # guides(color = guide_colorbar(title=bar_label, barwidth = 10, barheight = 1, title.vjust = 0.85)) +
    # scale_colour_gradientn(colours=color_grad_list, values = NULL,
    #                        space = "Lab", na.value = "grey50") +
    # scale_x_continuous("Time (years)") +
    # scale_y_continuous(data$variable) +
    sensitivity_theme +
    # labs(title=plot_label) +
    # ylim(range_vals$min, range_vals$max) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    # xlab(x_label) +
    xlab("Change in parameter from default (%)") +
    ylab("Change in temperature (deg. C)") ->
    #legend() ->
    # theme(axis.title.y=element_blank()) ->
    plot_out
  
  if (labels==FALSE){
    plot_out <- plot_out + 
      theme(axis.text.y=element_blank(),
            strip.text=element_blank())
  }

  return(plot_out)
}

make_sensitivity_plot2 <- function(data, param, range_vals, plot_labels, bar_labels, labels=TRUE){
  plot_label = plot_labels[[param]]
  bar_label=bar_labels[[param]]
  data %>% 
    mutate(variable = recode_factor(
      variable,
      f_frozen = "Change in frozen perm. (%)",
      Ca = "Change in atm. CO2 (%)",
      CH4 = "Change in atm. CH4 (%)",
      Tgav = "Change in temperature (deg. C)"
    )) %>% 
    filter(param_name==param) ->
    param_data
  
  param_data %>% 
    ggplot() +
    aes(x = year, y = diff, color = param_value, group = param_value) +
    geom_line() +
    facet_rep_wrap(~variable, scales = "fixed", ncol=1, strip.position = "left") +
    guides(color = guide_colorbar(title=bar_label, barwidth = 10, barheight = 1, title.vjust = 0.85)) +
    #scale_color_viridis_c() +
    #scale_color_uchicago() +
    scale_colour_gradientn(colours=color_grad_list, values = NULL,
                           space = "Lab", na.value = "grey50") +
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

make_gt_table <- function(data){
  data %>% gt(rowname_col = "variable2", groupname_col = "param") %>%
    fmt_number(
      columns = c(3,4,5,6),
      decimals = 2
    ) %>%
    row_group_order(
      groups = names(constraint_names)
    ) %>%
    cols_label(Tgav=var_full_names[1], Ca=var_full_names[2],
               atm_ocean_flux=var_full_names[4],
               atm_land_flux=var_full_names[3]
    ) ->
    table_out
}

make_sensitivity_bar <- function(data, output_var, variable_full_name=output_var, label_strips=TRUE, logscale=FALSE) {
  data <- filter(all_results,output==output_var)
  if (logscale) {
    data <- mutate(data,value=log(value))
  }
  max_val <- max(data$value)
  ggplot(data, aes(x=variable2, y=value)) +
    #geom_segment( aes( x=variable2, xend=variable2, y=0, yend=value, size=0.5, colour=variable2), show.legend = FALSE) +
    geom_linerange( aes( ymin=0, ymax=value, colour=variable2), show.legend = FALSE, alpha=1) +
    geom_point( size=2, alpha=1, aes(colour=variable2) ) +
    #scale_colour_manual(values = bar_cols)+
    scale_colour_uchicago(alpha=0.7, guide = guide_legend(reverse = TRUE))+
    facet_grid(vars(param2), switch="y")+
    coord_flip() +
    xlab("Permafrost Parameter") +
    ylab(variable_full_name) +
    geom_hline(yintercept=0, show.legend = FALSE)+
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    theme_classic() + 
    theme(# legend.title = element_blank(), 
          axis.title.x = element_text(size=9, angle=0, face="bold"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size=9),
          axis.text.y = element_blank(),
          strip.background = element_blank(),
          strip.text.y.left = element_text(color="black", face="bold", size=9, angle=0),
          strip.placement = "inside",
          axis.line.y = element_blank(),
          legend.text = element_text(size=8),
          legend.position = "top",
          panel.spacing = unit(1, "lines")) +
    theme(legend.position = "none") ->
    out
  if (!label_strips){
    out + theme(strip.text.y.left=element_blank()) -> out
  }
  return(out)
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

# for results with a single scenario across multiple RCPs or vice versa
plot_by_rcp <- function(results, variables, plot_var="value", nrows=2, width=round(length(variables)/nrows)*2+5, height=nrows*2.5+2,
                        end_date=2300, split_by_date=NA, xlab = "Time (years)",
                        filename="test_plot.png", write=TRUE, labels=TRUE){
  #results <- arrange(transform(results,
  #                           variaria=factor(Species,levels=neworder)),Species)
  results <- filter(results, year>=2000)  # keep it to this century
  if (is.numeric(split_by_date)) {
    first_period <- filter(results, year<=split_by_date)
    second_period <- filter(results, year>split_by_date, year<=end_date)
    
    first_period <- mutate(first_period, period="Calibrated")
    second_period <- mutate(second_period, period="Uncalibrated")
    
    all_results <- rbind(first_period, second_period)
  } else{ all_results <- results }
  
  all_results %>%
    filter(year<=end_date, variable %in% variables) -> 
    plot_data
  
  # set ploting order based on order of user-input variables
  long_names <- unique(plot_data$var_full_name)
  names(long_names) <- unique(plot_data$variable)
  list_out <- to_vec(for (i in variables) long_names[[i]])
  plot_data$var_full_name <- factor(plot_data$var_full_name, levels=list_out)

  # plot_data$variable <- factor(plot_data$variable, levels=variables)

  ggplot(data=plot_data) +
    aes(x = year, y = .data[[plot_var]], color = RCP, linetype=period) +
    geom_line(size=1.5) +
    scale_x_continuous(xlab) +
    facet_wrap(vars(var_full_name), scales = "free_y", strip.position="left", nrow=nrows) +
    scale_color_manual(values=c("RCP26"="#003466", "RCP45"="#70A0CD", "RCP60"="#C47900", "RCP85"="#990002"))+
    #scale_color_uchicago() +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    my_theme2 + 
    #theme(legend.position = "none") ->
    theme(legend.position = "bottom", legend.text = element_text(size=12)) ->
    plot_out
  
  if (labels){
    # plot_out <- plot_out + annotate('text', label = LETTERS[1:length(variables)], x=min(plot_data$year), y=max(plot_data$value))
    plot_out <- tag_facet(plot_out, open="", close="", hjust=-0.8) #, hjust=1.5, x=Inf, y=Inf)
  }
  
  if (write){
    ggsave(plot_out, filename=paste0("paper_figures/",filename,".png"), width=width, height=height, dpi=300)
    ggsave(plot_out, filename=paste0("paper_figures/",filename,".eps"), device=cairo_ps, width=width, height=height, dpi=300)
  }
  return(plot_out)
}

plot_compare_scenarios <- function(results, variables, use_diff=FALSE, nrows=2, scenarios=unique(results$scenario),
                        end_date=2300, xlab = "Time (years)", filename="test_plot"){
  results %>%
    filter(year<=end_date, variable %in% variables, scenario %in% scenarios) -> 
    plot_data
  
  plot_data$variable <- factor(plot_data$variable, levels=variables)
  
  if (use_diff){
    ggplot(data=plot_data) +
      aes(x = year, y = diff, color = RCP, linetype=scenario) ->
      p
  } else{
    ggplot(data=plot_data) +
      aes(x = year, y = value, color = RCP, linetype=scenario) ->
      p
  }
  
  p + geom_line() +
    scale_x_continuous(xlab) +
    facet_wrap(vars(var_full_name), scales = "free_y", strip.position="left", nrow=nrows) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    my_theme2 + 
    theme(legend.position = "bottom", legend.text = element_text(size=12)) ->
    plot_out
  
  ggsave(plot_out, filename=paste0("figures/",filename,".png"), width=15, height=8, dpi=300)
  ggsave(plot_out, filename=paste0("figures/",filename,".eps"), device=cairo_ps, width=15, height=8, dpi=300)
  
}


