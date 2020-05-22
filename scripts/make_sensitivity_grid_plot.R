library(hector)
library(hectortools)
library(ggplot2)
library(lemon)
library(cowplot)
library(dplyr)

source("scripts/plotting_tools.R")

outvars <- c(F_FROZEN(), ATMOSPHERIC_CO2(), ATMOSPHERIC_CH4(), GLOBAL_TEMP())

run_with_param <- function(core, parameter, value) {
  old_value <- fetchvars(core, NA, parameter)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, parameter, value, unit)
  reset(core)
  run(core)
  result <- fetchvars(core, 2000:2100, outvars)
  result[["param_value"]] <- value
  result[["param_name"]] <- unlist(strsplit(parameter, ".", fixed=TRUE))[2]
  result
}

#' Run Hector with a range of parameter values
run_with_param_range <- function(core, parameter, values, default) {
  split_biome(core, "global", c("non-permafrost", "permafrost"),
              fveg_c = c(0.98, 0.02),
              fdetritus_c = c(0.98, 0.02),
              fsoil_c = c(0.89, 0.11),
              fpermafrost_c = c(0.0000001,0.9999999),
              rh_ch4_frac = c(0, 0.023))
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
  split_biome(core, "global", c("non-permafrost", "permafrost"),
              fveg_c = c(0.98, 0.02),
              fdetritus_c = c(0.98, 0.02),
              fsoil_c = c(0.89, 0.11),
              fpermafrost_c = c(0.0000001,0.9999999),
              rh_ch4_frac = c(0,0.023))
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
sensitivity_fpf_static <- run_with_param_range(core, FPF_STATIC(biome="permafrost"), seq(0.15, 0.60, length.out = n_pts), default=0.4)

# kessler fit range: 0.172 +/- 0.0261
# resultant fitted sigma range: 0.53 - 0.71
core <- newcore(perm45, suppresslogging = TRUE)
sensitivity_pf_sigma <- run_with_param_range(core, PF_SIGMA(biome="permafrost"), seq(0.53, 0.71, length.out = n_pts), 0.618)

# kessler fit range: 0.172 +/- 0.0261
# resultant fitted mu range: 1.14 - 1.41
core <- newcore(perm45, suppresslogging = TRUE)
sensitivity_pf_mu <- run_with_param_range(core, PF_MU(biome="permafrost"), seq(1.14, 1.41, length.out = n_pts), 1.25)

# need to do rh_ch4_frac with biomes
core <- newcore(perm45, suppresslogging = TRUE)
# range from paper for rh_ch4_frac: 0.07 +/- 0.1387. From Kessler (pg. 7), 2.3% (1-3%)
sensitivity_rh_ch4_frac <- run_with_param_range(core, RH_CH4_FRAC(biome="permafrost"), seq(0.063, 0.077, length.out = n_pts), 0.07)

all_data <- rbind(sensitivity_pf_mu, sensitivity_pf_sigma, sensitivity_fpf_static, sensitivity_permafrost_c0, sensitivity_rh_ch4_frac)
all_data %>%
  # group_by(variable) %>% 
  summarise(max=max(pct_diff), min=min(pct_diff)) ->
  range_vals

plot_labels <- c("pf_mu"=expression(paste("Permafrost Thaw Parameter ",mu)), "pf_sigma"=expression(paste("Permafrost Thaw Parameter ",sigma)), "fpf_static"="Non-labile fraction", 
            "permafrost_c0"="Initial permafrost carbon", 
            "rh_ch4_frac"=expression(paste("R"["H"], " CH"[4], " Fraction")))


bar_labels <- c("pf_mu"="", "pf_sigma"="", "fpf_static"="", 
                 "permafrost_c0"="(Pg C)", 
                 "rh_ch4_frac"="")

# make var vs time plot
first_plot <- make_sensitivity_plot2(sensitivity_pf_mu, "pf_mu", range_vals, plot_labels, bar_labels, labels=TRUE)

all_data %>% 
  filter(param_name!="pf_mu") ->
  plot_data

param_list <- unique(plot_data$param_name)

plots <- lapply(param_list, make_sensitivity_plot2, data=plot_data, range_vals = range_vals, 
                plot_labels = plot_labels, bar_labels = bar_labels, labels=FALSE)

grid_plot <- plot_grid(first_plot, plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=1, rel_widths = c(1.15,1,1,1,1))
ggsave(grid_plot, filename="figures/sensitivity_analysis_all_years.png", width=18, height=12, dpi=300)

# make param vs var plot
first_plot <- make_sensitivity_plot3(sensitivity_pf_mu, "pf_mu", range_vals, plot_labels, bar_labels, labels=TRUE)

all_data %>% 
  filter(param_name!="pf_mu") ->
  plot_data

param_list <- unique(plot_data$param_name)

plots <- lapply(param_list, make_sensitivity_plot3, data=plot_data, range_vals = range_vals, 
                plot_labels = plot_labels, bar_labels = bar_labels, labels=FALSE)

setEPS(bg = "white", family = "Times")
postscript("figures/sensitivity_analysis_2.eps", width=18, height=10)
plot_grid(first_plot, plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=1)
dev.off()

