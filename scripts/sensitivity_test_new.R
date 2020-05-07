library(hector)
library(hectortools)
library(ggplot2)
library(lemon)
library(cowplot)
library(dplyr)
#library(rlist)

my_theme <- theme_classic() + 
  theme(legend.title=element_blank(), 
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.text = element_text(size=12),
        axis.text = element_text(color="black", size=12), 
        axis.title = element_text(color = "black", size=12))

my_theme2 <- theme_classic() + 
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

single_var_plot2 <- function(data, var, range, plot_label, bar_label, labels=TRUE){
  range = filter(range,variable==var)
  data %>% 
    ggplot() +
    aes(x = year, y = diff, color = param_value, group = param_value) +
    geom_line() +
    scale_color_viridis_c() +
    scale_x_continuous("Time (years)") +
    scale_y_continuous(data$variable) +
    ylim(range$min, range$max) +
    my_theme2 +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    ylab(NULL) ->
    plot_out
  
  if (var %in% c("f_frozen", "Ca", "CH4")){
    plot_out <- plot_out + 
      theme(axis.text.x=element_blank(),
            axis.title.x=element_blank())
  }
  if (labels==FALSE){
    plot_out <- plot_out + 
      theme(axis.text.y=element_blank(),
            axis.title.y=element_blank())
  }
  if (var=="f_frozen"){
    plot_out <- plot_out + 
      labs(title=plot_label)
  }
  if (var=="Tgav"){
    plot_out <- plot_out + 
      guides(color = guide_colorbar(title=bar_label, barwidth = 10, barheight = 1))
  }
}

single_var_plot <- function(data, var, range, plot_label, bar_label, labels=TRUE){
  range = filter(range,variable==var)
  data %>% 
    ggplot() +
      aes(x = year, y = diff, color = param_value, group = param_value) +
      facet_rep_wrap(~variable, scales = "free_y", ncol=1, strip.position = "left")
      geom_line() +
      scale_color_viridis_c() +
      scale_x_continuous("Time (years)") +
      scale_y_continuous(data$variable) +
      ylim(range$min, range$max) +
      my_theme2 +
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
      ylab(NULL) ->
      plot_out
  
  if (var %in% c("f_frozen", "Ca", "CH4")){
    plot_out <- plot_out + 
      theme(axis.text.x=element_blank(),
            axis.title.x=element_blank())
  }
  if (labels==FALSE){
    plot_out <- plot_out + 
      theme(axis.text.y=element_blank(),
            axis.title.y=element_blank())
  }
  if (var=="f_frozen"){
    plot_out <- plot_out + 
      labs(title=plot_label)
  }
  if (var=="Tgav"){
    plot_out <- plot_out + 
      guides(color = guide_colorbar(title=bar_label, barwidth = 10, barheight = 1))
  }
}

make_sensitivity_plot <- function(data, param, range_vals, plot_labels, bar_labels, labels=TRUE){
  
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
  
  var_list <- unique(data$variable)
  
  plots <- lapply(var_list, single_var_plot, data = param_data, range = range_vals, 
                 plot_label = plot_labels[[param]], bar_label=bar_labels[[param]], labels=labels)

  plot_out <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], ncol=1)
  return(plot_out)
}


make_sensitivity_plot2 <- function(data, param, range_vals, plot_labels, bar_labels, labels=TRUE){
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
    ggplot() +
    aes(x = year, y = pct_diff, color = param_value, group = param_value) +
    geom_line() +
    facet_rep_wrap(~variable, scales = "fixed", ncol=1, strip.position = "left") +
    guides(color = guide_colorbar(title=bar_label, barwidth = 10, barheight = 1)) +
    scale_color_viridis_c() +
    scale_x_continuous("Time (years)") +
    scale_y_continuous(data$variable) +
    my_theme2 +
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
    my_theme2 +
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


outvars <- c(F_FROZEN(), ATMOSPHERIC_CO2(), ATMOSPHERIC_CH4(), GLOBAL_TEMP())

run_with_param <- function(core, parameter, value) {
  old_value <- fetchvars(core, NA, parameter)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, parameter, value, unit)
  reset(core)
  run(core)
  result <- fetchvars(core, 2000:2100, outvars)
  result[["param_value"]] <- value
  result[["param_name"]] <- parameter
  result
}

#' Run Hector with a range of parameter values
run_with_param_range <- function(core, parameter, values, default) {
  default_run <- run_with_param(core, parameter, default)
  invisible(reset(core))
  mapped <- Map(function(x) run_with_param(core, parameter, x), values)
  mapped_default <- Map(function(x) mutate(x, default_val=default_run$value, diff=value-default_val, 
                                           pct_diff=100*(value-default_val)/default_val), mapped)
  Reduce(rbind, mapped_default)
}

run_with_ini <- function(value) {
  rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
  perm_ini <- modifyList(rcp45, list(simpleNbox = list(
    permafrost_c = value
  )))
  write_ini(perm_ini, "./hector_rcp45_perm.ini")
  perm45 <- "./hector_rcp45_perm.ini"
  core <- newcore(perm45, suppresslogging = TRUE)
  run(core)
  result <- fetchvars(core, 2000:2100, outvars)
  result[["param_value"]] <- value
  result[["param_name"]] <- "permafrost_c0"
  return(result)
}

#' Run Hector with a range of parameter values
run_with_ini_range <- function(values, default) {
  default_run <- run_with_ini(default)
  mapped <- Map(function(x) run_with_ini(x), values)
  mapped_default <- Map(function(x) mutate(x, default_val=default_run$value, diff=value-default_val, 
                                           pct_diff=100*(value-default_val)/default_val), mapped)
  Reduce(rbind, mapped_default)
}

n_pts = 50

# need to do permafrost c by changing ini file
# range from Hugelius et al 2014  1,035 ± 150 Gt of carbon
sensitivity_permafrost_c0 <- run_with_ini_range(seq(885, 1185, length.out = n_pts), 1035)

# set permafrost in ini file
rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
perm_ini <- modifyList(rcp45, list(simpleNbox = list(
  permafrost_c = 1035
)))
write_ini(perm_ini, "./hector_rcp45_perm.ini")
perm45 <- "./hector_rcp45_perm.ini"

# range from Kessler: According to Burke et al. (2012; 2013), the size of this passive pool is 
# considered uncertain and could range between 15% and 60%
core <- newcore(perm45, suppresslogging = TRUE)
sensitivity_fpf_static <- run_with_param_range(core, FPF_STATIC(), seq(0.15, 0.60, length.out = n_pts), default=0.4)

# kessler fit range: 0.172 +/- 0.0261
# resultant fitted sigma range: 0.53 - 0.71
core <- newcore(perm45, suppresslogging = TRUE)
sensitivity_pf_sigma <- run_with_param_range(core, PF_SIGMA(), seq(0.53, 0.71, length.out = n_pts), 0.618)

# kessler fit range: 0.172 +/- 0.0261
# resultant fitted mu range: 1.14 - 1.41
core <- newcore(perm45, suppresslogging = TRUE)
sensitivity_pf_mu <- run_with_param_range(core, PF_MU(), seq(1.14, 1.41, length.out = n_pts), 1.25)

# need to do rh_ch4_frac with biomes
core <- newcore(perm45, suppresslogging = TRUE)
split_biome(core, "global", c("non-permafrost", "permafrost"),
            fveg_c = c(0.98, 0.02),
            fdetritus_c = c(0.98, 0.02),
            fsoil_c = c(0.89, 0.11),
            fpermafrost_c = c(0.0000001,0.9999999))
# range from paper for rh_ch4_frac: 0.07 +/- 0.1387. From Kessler (pg. 7), 2.3% (1-3%)
sensitivity_rh_ch4_frac <- run_with_param_range(core, RH_CH4_FRAC(biome="permafrost"), seq(0.063, 0.077, length.out = n_pts), 0.07)

all_data <- rbind(sensitivity_pf_mu, sensitivity_pf_sigma, sensitivity_fpf_static, sensitivity_permafrost_c0, sensitivity_rh_ch4_frac)
all_data %>%
  # group_by(variable) %>% 
  summarise(max=max(pct_diff), min=min(pct_diff)) ->
  range_vals

plot_labels <- c("pf_mu"=expression(mu), "pf_sigma"=expression(sigma), "fpf_static"="Non-labile fraction", 
            "permafrost_c0"="Initial permafrost carbon", 
            "permafrost.rh_ch4_frac"=expression(paste("R"["H"], " CH"[4], " Fraction")))

bar_labels <- c("pf_mu"=expression(mu), "pf_sigma"=expression(sigma), "fpf_static"="", 
                 "permafrost_c0"="Pg C", 
                 "permafrost.rh_ch4_frac"="")

first_plot <- make_sensitivity_plot3(sensitivity_pf_mu, "pf_mu", range_vals, plot_labels, bar_labels, labels=TRUE)

all_data %>% 
  filter(param_name!="pf_mu") ->
  plot_data

param_list <- unique(plot_data$param_name)

plots <- lapply(param_list, make_sensitivity_plot3, data=plot_data, range_vals = range_vals, 
                plot_labels = plot_labels, bar_labels = bar_labels, labels=FALSE)
#all_plots <- list.prepend(plots, first_plot)

setEPS(bg = "white", family = "Times")
postscript("figures/sensitivity_analysis_2.eps", width=18, height=10)
plot_grid(first_plot, plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=1)
dev.off()


first_plot <- make_sensitivity_plot2(sensitivity_pf_mu, "pf_mu", range_vals, plot_labels, bar_labels, labels=TRUE)

all_data %>% 
  filter(param_name!="pf_mu") ->
  plot_data

param_list <- unique(plot_data$param_name)

plots <- lapply(param_list, make_sensitivity_plot2, data=plot_data, range_vals = range_vals, 
                plot_labels = plot_labels, bar_labels = bar_labels, labels=FALSE)
#all_plots <- list.prepend(plots, first_plot)

setEPS(bg = "white", family = "Times")
postscript("figures/sensitivity_analysis_all_years.eps", width=18, height=10)
plot_grid(first_plot, plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=1)
dev.off()



plot_data %>% 
  group_by(variable) %>% 
  summarise(max=max(diff), min=min(diff)) ->
  range_vals

all_data %>% 
  ggplot() +
  aes(x = year, y = value, linetype = scenario, color = param_value, group=param_value) +
  geom_line() +
  facet_grid(vars(variable), scales = "free_y") +
  theme_bw() +
  theme(axis.title = element_blank())


plot_data2 <- 
  mutate(plot_data, variable = recode_factor(
    variable,
    f_frozen = "Frozen permafrost fraction",
    Ca = "Atmospheric CO2 (ppm)",
    CH4 = "Atmospheric CH4 (ppbv)",
    Tgav = "Global mean temperature anomaly (K)"
  )) 
  
all_data %>% 
  filter(param_name=="pf_mu") %>% 
  ggplot() +
  aes(x = year, y = pct_diff, color = param_value, group = param_value) +
  geom_line() +
  facet_rep_wrap(~variable, scales = "free_y", ncol=1, strip.position = "left") +
  guides(color = guide_colorbar(title = "pf_mu", barwidth = 15, barheight = 1)) +
  scale_color_viridis_c() +
  scale_x_continuous("Time (years)") +
  labs(title="pf_mu")+
  my_theme2 +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  ylab(NULL) 

all_data %>% 
  filter(param_name=="pf_sigma") %>% 
  ggplot() +
  aes(x = year, y = value, color = param_value, group = param_value) +
  geom_line() +
  facet_rep_wrap(~variable, scales = "free_y", ncol=1, strip.position = "left") +
  guides(color = guide_colorbar(title = "pf_mu", barwidth = 15, barheight = 1)) +
  scale_color_viridis_c() +
  scale_x_continuous("Time (years)") +
  labs(title="pf_mu")+
  my_theme2 +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  ylab(NULL) 



p1 <- make_sensitivity_plot(plot_data, "pf_mu", "pf_mu")





# or (better)

plots2 <- Map(function(x) make_sensitivity_plot(plot_data, x), param_list)

mapped <- Map(function(x) run_with_param(core, FPF_STATIC(), x), seq(0.15, 0.60, length.out = n_pts), 0.4)
invisible(reset(core))
default_fpf_static <- run_with_param(core, FPF_STATIC(), 0.4)

mutate(mapped[[1]], default_val=default_fpf_static$value, diff=value-default_val)
mapped_default <- Map(function(x) mutate(x, default_val=default_fpf_static$value, diff=value-default_val), mapped)


invisible(reset(core))
default_fpf_static <- run_with_param(core, FPF_STATIC(), 0.4)


library(rlang)
exec(plot_grid, !!!c(plots, nrow=1))

all_data %>% 
  ggplot() +
  aes(x = year, y = value, color = param_value, group = param_value) +
  geom_line() +
  facet_grid(variable~param_name, scales = "free") +
  guides(color = guide_colorbar(title = "pf_mu")) +
  scale_color_viridis_c() +
  my_theme2



pf_mu_plot <- ggplot(sensitivity_pf_mu) +
  aes(x = year, y = value, color = param_value, group = param_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol=1) +
  guides(color = guide_colorbar(title = "pf_mu")) +
  scale_color_viridis_c() +
  theme_bw() +
  theme(panel.grid=element_blank())













setEPS(bg = "white", family = "Times")
postscript("figures/pf_mu_sensitivity.eps", width=7, height=4)
ggplot(sensitivity_pf_mu) +
  aes(x = year, y = value, color = parameter_value, group = parameter_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = "pf_mu")) +
  scale_color_viridis_c() +
  theme_bw()+
  theme(panel.grid=element_blank())
dev.off()

setEPS(bg = "white", family = "Times")
postscript("figures/pf_sigma_sensitivity.eps", width=7, height=4)
ggplot(sensitivity_pf_sigma) +
  aes(x = year, y = value, color = parameter_value, group = parameter_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = "pf_sigma")) +
  scale_color_viridis_c() +
  theme_bw()+
  theme(panel.grid=element_blank())
dev.off()

setEPS(bg = "white", family = "Times")
postscript("figures/fpf_static_sensitivity.eps", width=7, height=4)
ggplot(sensitivity_fpf_static) +
  aes(x = year, y = diff, color = param_value, group = param_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = "fpf_static")) +
  scale_color_viridis_c() +
  theme_bw()+
  theme(panel.grid=element_blank())
dev.off()

setEPS(bg = "white", family = "Times")
postscript("figures/permafrost_c0_sensitivity.eps", width=7, height=4)
ggplot(sensitivity_permafrost_c0) +
  aes(x = year, y = value, color = parameter_value, group = parameter_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = "permafrost_c0")) +
  scale_color_viridis_c() +
  theme_bw()+
  theme(panel.grid=element_blank())
dev.off()

setEPS(bg = "white", family = "Times")
postscript("figures/rh_ch4_frac_sensitivity.eps", width=7, height=4)
ggplot(sensitivity_rh_ch4_frac) +
  aes(x = year, y = diff, color = param_value, group = param_value) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  guides(color = guide_colorbar(title = "rh_ch4_frac")) +
  scale_color_viridis_c() +
  theme_bw()+
  theme(panel.grid=element_blank())
dev.off()

