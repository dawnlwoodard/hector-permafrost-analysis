library(ggpubr)
library(ggplot2)
library(purrr)
library(hector)
library(hectortools)
library(viridis)
source("scripts/plotting_tools.R")
source("scripts/analysis_tools.R")
source("scripts/nomenclature.R")
source("scripts/parameter_derivation.R")
source("scripts/sensitivity_functions.R")

retune <- FALSE
rerun <- FALSE

# set all model parameters here

param_data <- get_permafrost_parameters(retune)

param_default <- param_data[["defaults"]]
param_bounds <- param_data[["bounds"]]

param_names <- names(param_default)
names(param_names) <- names(param_default)  # setting up named list here for use in lapply later

# control which RCP scenarios are run
rcps <- c("26", "45", "60", "85")
names(rcps) <- paste0("RCP", rcps)

# accompanying external data needed: input/cmip6_tas_HL_45.csv, input/cmip6_pffrac_45.csv (along with script to generate)

# ************************************************ Figure Themes ************************************************* #

fig2_theme <- theme_classic() +
  theme(legend.title=element_blank(),
        legend.position = c(0.02, 0.01),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.text = element_text(size=12),
        legend.background = element_rect(fill="transparent"),
        axis.text = element_text(color="black", size=12),
        axis.title = element_text(color = "black", size=12))


simple_theme <- theme_classic() +
  theme(legend.title=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        axis.text = element_text(color="black", size=12),
        axis.title = element_text(color = "black", size=12))

# ************************************************ Figure 2 ************************************************* #

# generate panel a
# requires some functions from parameter_derivation.R

k_sig <- 0.0261

x <- seq(0.01, 10, 0.1)
kessler_m <- 0.172
kessler <- kessler_line(x, kessler_m)
kessler_upper = kessler_line(x, kessler_m+k_sig*2)
kessler_lower = kessler_line(x, kessler_m-k_sig*2)

curves_optimized <- tibble(x = x, kessler_lower = kessler_lower, kessler_upper=kessler_upper, kessler=kessler,
                           funline = cdfun(x, param_default[[PF_MU()]], param_default[[PF_SIGMA()]]))


kessler_colors <- pal_uchicago("default")(5)

ggplot(data=curves_optimized) +
  geom_ribbon(aes(x=x, ymin = kessler_lower, ymax = kessler_upper, fill="Hector"),
              show.legend=FALSE, alpha = 0.15) +
  geom_line(aes(x=x, y = funline, colour="Hector"), size=1) +
  geom_line(aes(x=x, y=kessler, colour = "Kessler (2017)"), size=1, linetype="dashed") +
  ylim(0, max(curves_optimized$funline)+0.12) +
  xlim(0, max(curves_optimized$x)) +

  geom_point(aes(x=1.3, y=0.94), colour=kessler_colors[1]) +
  geom_label(aes(x=1.3, y=0.94), label="Crichton et al. (2016)", nudge_x = 1.55, nudge_y = 0.07, colour=kessler_colors[1], label.size=0)+

  geom_point(aes(x=2, y=0.94), colour=kessler_colors[2]) +
  geom_label(aes(x=2, y=0.94), label="Burke et al. (2017)", nudge_x = 1.6, nudge_y = -0.01, colour=kessler_colors[2], label.size=0)+

  geom_point(aes(x=5.4, y=0.69), colour=kessler_colors[3]) +
  geom_label(aes(x=5.4, y=0.69), label="Koven et al. (2013)", nudge_x = 1.7, nudge_y = -0.05, colour=kessler_colors[3], label.size=0)+

  geom_point(aes(x=5.8, y=0.72), colour=kessler_colors[2]) +
  geom_label(aes(x=5.8, y=0.72), label="Burke et al. (2017)", nudge_x = 1.4, nudge_y = 0.065, colour=kessler_colors[2], label.size=0)+

  geom_point(aes(x=6, y=0.71), colour=kessler_colors[4]) +
  geom_label(aes(x=6, y=0.71), label="McGuire et al. (2018)", nudge_x = 1.75, nudge_y = 0.005, colour=kessler_colors[4], label.size=0)+

  geom_point(aes(x=4.6, y=0.49), colour=kessler_colors[5]) +
  geom_label(aes(x=4.6, y=0.49), label="MacDougall and Knutti (2016)", nudge_x = -2.3, nudge_y = 0.0, colour=kessler_colors[5], label.size=0)+

  geom_point(aes(x=7.3, y=0.36), colour=kessler_colors[1]) +
  geom_label(aes(x=7.3, y=0.36), label="Crichton et al. (2016)", nudge_x = -1.79, nudge_y = -0.036, colour=kessler_colors[1], label.size=0)+

  geom_point(aes(x=9, y=0.37), colour=kessler_colors[5]) +
  geom_label(aes(x=9, y=0.37), label="MacDougall and Knutti (2016)", nudge_x = -2.1, nudge_y = -0.15, colour=kessler_colors[5], label.size=0)+

  fig2_theme +
  theme(legend.justification = "center", legend.position = c(0.15,0.10), legend.text = element_text(size=12), legend.background = element_blank()) +
  ylab("Frozen Permafrost Fraction") +
  xlab(expression(paste("High Latitude Temperature Change (", degree*C, ")"))) +
  scale_colour_manual(NA, values = c("firebrick3", "black")) +
  scale_fill_manual(values=c("black")) ->
  kessler_fit_plot

# generate panel b)
cmip6_tas_HL <- read_delim("input/cmip6_tas_HL_45.csv", delim=',')
cmip6_pf_frac <- read_delim("input/cmip6_pffrac_45.csv", delim=',')

cmip_pf_frac <- cmip6_pf_frac %>% mutate(year=X1) %>%
  select(-c(X1)) %>% pivot_longer(!year, names_to="model", values_to="value") %>% mutate(variable="pf_frac")

cmip_tas_HL <- cmip6_tas_HL %>% mutate(year=X1) %>%
  select(-c(X1)) %>% pivot_longer(!year, names_to="model", values_to="value") %>%
  mutate(variable="tas")

cmip6_data <- bind_rows(cmip_pf_frac, cmip_tas_HL)
cmip6_to_plot <- cmip6_data %>% filter(model!="model_mean") %>% pivot_wider(names_from = variable, values_from="value")
cmip6_to_plot <- cmip6_to_plot %>% filter(tas > (-2))
model_mean <- cmip6_data %>% filter(model=="model_mean") %>% pivot_wider(names_from = variable, values_from="value") %>% filter(tas > (-2))

model_colors <- gray.colors(length(unique(cmip6_to_plot$model)))
names(model_colors) <- unique(cmip6_to_plot$model)

plot_colors <- c("Hector"="firebrick3", "CMIP6 Mean"="dodgerblue4", model_colors)

ggplot() +
  geom_point(data=cmip6_to_plot, show.legend = FALSE, aes(x = tas, y = pf_frac, color = model)) +
  geom_point(data=model_mean, aes(x = tas, y = pf_frac, color = "CMIP6 Mean")) +
  geom_line(data=curves_optimized, aes(x=x, y = funline, color="Hector"), size=1) +
  scale_color_manual("", values=plot_colors) +
  ylim(0, max(curves_optimized$funline)+0.12) +
  xlim(0, max(curves_optimized$x)) +
  ylab("") +
  xlab(expression(paste("High Latitude Temperature Change (", degree*C, ")"))) +
  annotate("text", x = 9.4, y = 0.17, label = 'atop(bold("Hector"))', parse = TRUE, colour="firebrick3") +
  annotate("text", x = 4.2, y = 0.43, label = 'atop(bold("CMIP6 Mean"))', parse = TRUE, colour="dodgerblue4") +
  fig2_theme +
  theme(legend.position = "none") ->
  plot_out

twopanel_compare <- ggpubr::ggarrange(kessler_fit_plot, plot_out, ncol=2, labels=c("a", "b"), hjust=-8.0, font.label=list(size=12, face="plain"))

ggsave(twopanel_compare, filename="paper_figures/figure2.png", width=12, height=4, dpi=300)
ggsave(twopanel_compare, filename="paper_figures/figure2.pdf", width=12, height=4, dpi=300)


# ************************************************ Figure 3 ************************************************* #

if (rerun){
  # rcps defined at top
  results <- purrr::map_dfr(rcps, run_rcp, start_date=2000, end_date=2300, param_inputs=param_default, .id = "RCP")
  plot_results <- process_results(results)
  save(plot_results, file="paper_data/plot_results.RData")
} else {
  # loads variable plot_results
  load("paper_data/plot_results.RData")
}

default <- filter(plot_results, scenario==NO_PERMAFROST_SCENARIO()) %>% mutate(value=case_when(variable==PERMAFROST_C()~0.0, TRUE~value))

plot_results %>%
  filter(scenario==PERMAFROST_FULL_SCENARIO()) %>%
  select(-scenario) %>%
  mutate(diff=value-default$value, default=default$value) ->
  diff_output

variables <- c("permafrost_c", "thawedp_c", "annual_c_flux", "CH4", "Ca", "Tgav")

# function from plotting_tools.R - saves figure in "paper_figures" folder
plot_by_rcp(diff_output, variables, plot_var="diff", end_date=2300, split_by_date=2100,
            width=13, height=8.5, xlab = "Time (years)", filename="figure3")


# ************************************************ Figure 4 ************************************************* #

if (rerun){
  # need to run for separate years because we don't have time series output for the ocean carbon stock in this version of Hector
  c_stocks_2050 <- purrr::map_dfr(rcps, run_rcp, end_date=2050, get_c_stocks=TRUE, param_inputs=param_default, .id = "RCP")
  c_stocks_2100 <- purrr::map_dfr(rcps, run_rcp, end_date=2100, get_c_stocks=TRUE, param_inputs=param_default, .id = "RCP")
  c_stocks_2300 <- purrr::map_dfr(rcps, run_rcp, end_date=2300, get_c_stocks=TRUE, param_inputs=param_default, .id = "RCP")

  c_stocks_2050 <- mutate(c_stocks_2050, year=2050)
  c_stocks_2100 <- mutate(c_stocks_2100, year=2100)
  c_stocks_2300 <- mutate(c_stocks_2300, year=2300)

  c_stocks_all_years <- rbind(c_stocks_2050, c_stocks_2100, c_stocks_2300)
  save(c_stocks_all_years, file="paper_data/c_stocks_all_years.RData")
} else {
  load("paper_data/c_stocks_all_years.RData")
}


pf_c0 <- param_default[[PF_C0()]] # Pg C
c_stocks_all_years %>%
  mutate(value=case_when(variable==PERMAFROST_C() & scenario==NO_PERMAFROST_SCENARIO()~pf_c0, TRUE~value)) %>%
  mutate(var_full_name = recode_factor(
    variable,
    atmos_c = "Atmospheric C pool",
    ocean_c = "Ocean C pool",
    veg_c = "Vegetation C pool",
    detritus_c = "Detritus C pool",
    soil_c = "Active soil C pool",
    thawedp_c = "Thawed Permafrost C pool",
    permafrost_c = "Frozen Permafrost C pool"
  )) ->
  plot_c_stocks_all_years

plot_c_stocks_all_years %>%
  filter(scenario==NO_PERMAFROST_SCENARIO(), variable!=PERMAFROST_C()) ->
  baseline_stocks_all_years

plot_c_stocks_all_years %>%
  filter(scenario!=NO_PERMAFROST_SCENARIO(), variable!=PERMAFROST_C()) %>%
  group_by(scenario) %>%
  mutate(value = value - baseline_stocks_all_years$value) %>%
  group_by(scenario, RCP, year) %>%
  mutate(pct = round(100*value/sum(value), digits=1)) ->
  diff_c_stocks_all_years

c_stock_cols <- c("#5D5F8AFF", "#235A98FF", "#3A866DFF", "#616F59FF", "#675D52FF", "#564544")

diff_c_stocks_all_years$year <- factor(diff_c_stocks_all_years$year, levels=c("2050", "2100", "2300"))

diff_c_stocks_all_years %>%
  filter(variable!=PERMAFROST_C(), scenario==PERMAFROST_FULL_SCENARIO()) %>%
  ggplot(aes(x = year, y = value, fill = var_full_name)) +
  geom_bar(stat = 'identity', position = 'stack', width=0.7) +
  scale_fill_manual(values = c_stock_cols) +
  facet_grid(~ RCP, switch="both")+
  xlab("Scenario") +
  ylab("Change in Carbon Storage from Baseline (Pg C)") +
  geom_hline(yintercept=0)+
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_text(color="black", face="bold", size=18),
        strip.placement = "outside",
        legend.text = element_text(size=16),
        legend.position = "top",
        panel.spacing = unit(1, "lines")) ->
  c_stocks_byRCP_all_yrs

ggsave(c_stocks_byRCP_all_yrs, filename="paper_figures/figure4.png", width=11, height=7, dpi=300)
ggsave(c_stocks_byRCP_all_yrs, filename="paper_figures/figure4.eps", device=cairo_ps, width=11, height=7, dpi=300)

# ************************************************ Figure 5 ************************************************* #

# use param_defaults from top
if (rerun){
  library(Runuran)
  n_pts <- 50
  n_runs <- 500

  # calculate standard deviations for each parameter based on mean difference between default value and upper and lower range values
  calc_sd <- function(param_name){
    return(mean(abs(param_bounds[[param_name]] - param_default[[param_name]])))
  }
  param_sd <- lapply(param_names, calc_sd)

  # Permafrost parameter priors. Currently assuming normal distribution with upper and lower bounds set for some based on real-world constraints.
  priors <- tibble(fpf_static = urnorm(n_pts, param_default[[FPF_STATIC()]], param_sd[[FPF_STATIC()]], lb=0, ub=1),
                   pf_mu = rnorm(n_pts, param_default[[PF_MU()]], param_sd[[PF_MU()]]),
                   pf_sigma = rnorm(n_pts, param_default[[PF_SIGMA()]], param_sd[[PF_SIGMA()]]),
                   warmingfactor = urnorm(n_pts, param_default[[WARMINGFACTOR()]], param_sd[[WARMINGFACTOR()]], lb=1),
                   rh_ch4_frac = urnorm(n_pts, param_default[[RH_CH4_FRAC()]], param_sd[[RH_CH4_FRAC()]], lb=0, ub=1),
                   permafrost_c = urnorm(n_pts, param_default[[PF_C0()]], param_sd[[PF_C0()]], lb=0),
                   nonpf_c = urnorm(n_pts, param_default[[NONPF_C()]], param_sd[[NONPF_C()]], lb=min(param_bounds[[NONPF_C()]]), ub=max(param_bounds[[NONPF_C()]]))
  )

  # generate hector results from n_runs random samples from the priors and store in tibble
  hector_output_co2 <- run_all_param_sets(priors, n_runs, ATMOSPHERIC_CO2(), end_date=2100)
  save(hector_output_co2, file="hector_sensitivity_runs_co2.RData")

  # generate hector results from n_runs random samples from the priors and store in tibble
  hector_output_ch4 <- run_all_param_sets(priors, n_runs, ATMOSPHERIC_CH4(), end_date=2100)
  save(hector_output_ch4, file="hector_sensitivity_runs_ch4.RData")

  # generate hector results from n_runs random samples from the priors and store in tibble
  hector_output_tgav <- run_all_param_sets(priors, n_runs, GLOBAL_TEMP(), end_date=2100)
  save(hector_output_tgav, file="hector_sensitivity_runs_tgav.RData")

  # run Pecan-like sensitivity analysis over Hector results
  sensitivity_results_co2 <- run_pecan_analysis(hector_output_co2, names(hector_output_co2)[1], names(hector_output_co2)[-1])

  # run Pecan-like sensitivity analysis over Hector results
  sensitivity_results_ch4 <- run_pecan_analysis(hector_output_ch4, names(hector_output_ch4)[1], names(hector_output_ch4)[-1])

  # run Pecan-like sensitivity analysis over Hector results
  sensitivity_results_tgav <- run_pecan_analysis(hector_output_tgav, names(hector_output_tgav)[1], names(hector_output_tgav)[-1])

  save(sensitivity_results_co2, sensitivity_results_ch4, sensitivity_results_tgav, file="paper_data/sensitivity_results_all.RData")

} else {
  load("paper_data/sensitivity_results_all.RData")
}

all_sensitivity_results <- process_sensitivity_results(sensitivity_results_co2, sensitivity_results_ch4, sensitivity_results_tgav)

# these two can be plotted because they are all on similar scales
pv <- make_sensitivity_bar(all_sensitivity_results, "partial_var", "Partial Variance", label_strips = FALSE)
el <- make_sensitivity_bar(all_sensitivity_results, "elasticity", "Elasticity", label_strips = FALSE)

figure <- ggpubr::ggarrange(el, pv, heights = c(1,1), common.legend = FALSE)

ggsave(figure, filename="paper_figures/figure5_part1.png", device="png", width=4.5, height=4, dpi=300)
ggsave(figure, filename="paper_figures/figure5_part1.pdf", device="pdf", width=4.5, height=4, dpi=300)

# CV values are manually entered in table section of figure (separate PowerPoint file) to allow better formatting and alignment
all_sensitivity_results %>% select(-c(param, output, variable)) %>%
  mutate(value=formatC(signif(value,digits=3), digits=3,format="fg", flag="#")) %>%
  arrange(param2) %>%
  pivot_wider(names_from=output2,values_from=value) %>% select(c(variable2, param2,.data[["Coefficient of Variation"]]))->
  sensitivity_table_results


# ************************************************ Figure 6 ************************************************* #

if (rerun){

  n_pts = 50
  outvars <- c(F_FROZEN(), ATMOSPHERIC_CO2(), ATMOSPHERIC_CH4(), GLOBAL_TEMP())

  full_sensitivity_output_45 <- run_simple_sensitivity(param_names, param_default, outvars, n_pts, bounds=param_bounds, model_list=get_nonpfc_modellist())

  full_sensitivity_output_85 <- run_simple_sensitivity(param_names, param_default, outvars, n_pts, bounds=param_bounds, model_list=get_nonpfc_modellist(), rcp="85")

  full_sensitivity_output_26 <- run_simple_sensitivity(param_names, param_default, outvars, n_pts, bounds=param_bounds, model_list=get_nonpfc_modellist(), rcp="26")

  full_sensitivity_output_60 <- run_simple_sensitivity(param_names, param_default, outvars, n_pts, bounds=param_bounds, model_list=get_nonpfc_modellist(), rcp="60")

  full_results_list <- map2(list(full_sensitivity_output_26, full_sensitivity_output_45, full_sensitivity_output_60, full_sensitivity_output_85), c("26", "45", "60", "85"), add_column, column="rcp")
  full_sensitivity_all_rcps <- tibble(rbindlist(full_results_list))

  save(full_sensitivity_all_rcps, file="paper_data/full_sensitivity_all_rcps.RData")
} else {
  load("paper_data/full_sensitivity_all_rcps.RData")
}

# calculate slopes
slope_plot_data <- full_sensitivity_all_rcps %>%
  filter(variable=="Tgav") %>%
  mutate(full_param_name = recode_factor(
    param_name,
    warmingfactor = "Permafrost Region Warming Factor",
    nonpf_c = "Non-Permafrost C in the Permafrost Region (Pg C)",
    fpf_static = "Non-Labile Fraction of Thawed Permafrost C",
    permafrost_c0 = "Initial Size of Permafrost Pool (Pg C)",
    rh_ch4_frac = "Methane Fraction of Thawed Permafrost Emissions",
    pf_sigma = "Permafrost Thaw Parameter Standard Deviation",
    pf_mu = "Permafrost Thaw Parameter Mean"
  ), var_full_name = recode_factor(
    variable,
    f_frozen = "Change in frozen perm. (%)",
    Ca = "Change in atm. CO2 (%)",
    CH4 = "Change in atm. CH4 (%)",
    Tgav = "Change in temperature (deg. C)"
  ))

get_slope <- function(y, x){
  data = tibble(y=y,x=x)
  lm_model <- lm(y~x, data=data)
  slope <- lm_model$coefficients[[2]]
  return(slope)
}

slope_out_data <- slope_plot_data %>% select(c(year, full_param_name, param_diff, diff, rcp)) %>%
  mutate(param_diff=param_diff*100) %>% group_by(full_param_name, rcp, year) %>% summarise(slope=10*get_slope(diff,param_diff)) %>% ungroup()

slope_data_wide <- slope_out_data %>%
  pivot_wider(id_cols=c(full_param_name, rcp, year), names_from=rcp, values_from=slope) %>%
  mutate(rcp26=`26`,rcp45=`45`, rcp85=`85`, mid=apply(cbind(rcp26, rcp45, rcp85),1,median),
         upper=pmax(rcp26,rcp45, rcp85), lower=pmin(rcp26,rcp45,rcp85)) %>%
  select(-c(`26`, `45`, `85`))

param_colors <-viridis(length(unique(slope_data_wide$full_param_name)))
names(param_colors) <- levels(slope_data_wide$full_param_name)

ggplot(data=slope_data_wide) +
  geom_line(aes(x=year, y=mid, color=full_param_name)) +
  geom_ribbon(aes(x=year, ymin = lower, ymax = upper, fill=full_param_name), alpha = 0.4) +
  geom_hline(yintercept=0) +
  ylim(min(slope_data_wide$lower)-0.002, max(slope_data_wide$upper)+0.002)+
  xlim(1999,2100)+
  simple_theme +
  scale_color_manual(values=param_colors) +
  scale_fill_manual(values=param_colors) +
  ylab("Change in temperature per 10% change in parameter (deg. C / 10%)") +
  xlab("Time (years)") ->
  slope_plot

# calculate maximum and minimum temperature effects
param_full_names <- c("Non-Permafrost C in the Permafrost Region (Pg C)",
                      "Permafrost Thaw Parameter Standard Deviation",
                      "Permafrost Thaw Parameter Mean",
                      "Initial Size of Permafrost Pool (Pg C)",
                      "Permafrost Region Warming Factor",
                      "Methane Fraction of Thawed Permafrost Emissions",
                      "Non-Labile Fraction of Thawed Permafrost C")

bar_plot_data <- full_sensitivity_all_rcps %>% filter(variable=="Tgav", year %in% c(2030, 2050, 2100, 2300))
bar_plot_data <- add_year_name(bar_plot_data)

bar_plot_all_maxmin <- bar_plot_data %>% filter(year_name=="2100") %>% group_by(param_name) %>%
  summarise(ymax = max(diff), ymin=min(diff)) %>% ungroup()

bar_plot_all_maxmin$param_name <- factor(x=bar_plot_all_maxmin$param_name,
                                         levels=c("nonpf_c", "pf_sigma", "pf_mu", "permafrost_c0",
                                                  "warmingfactor", "rh_ch4_frac", "fpf_static"),
                                         labels=param_full_names, ordered=TRUE)

slope_2100 <- filter(slope_data_wide, year==2100) %>% mutate(slope=mid) %>% select(c(full_param_name, slope))
slope_2100$full_param_name <- unlist(lapply(slope_2100$full_param_name, as.character))
slope_2100$full_param_name <- factor(x=slope_2100$full_param_name,
                                     levels=param_full_names,
                                     labels=param_full_names, ordered=TRUE)

all_maxmin_data <- full_join(bar_plot_all_maxmin, slope_2100, by=c("param_name"="full_param_name"))

ggplot(data=all_maxmin_data) +
  geom_errorbar(aes(x=slope, ymin = ymin, ymax = ymax, color=param_name), width=0.004, size=1.0) +
  geom_hline(yintercept=0) +
  scale_x_continuous(limits=c(min(slope_data_wide$lower)-0.002, max(slope_data_wide$upper)+0.002), position = "top")+
  simple_theme +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_color_manual(values=param_colors) +
  ylab("Change in temperature at 2100 (deg. C)") +
  geom_vline(xintercept=0) +
  xlab("") +
  coord_flip() ->
  hbar_test

twopanel_sensitivity <- ggpubr::ggarrange(slope_plot, NULL, hbar_test, widths=c(2.5, -.01, 1),
                                  ncol=3, nrow=1, labels = c("a","", "b"), label.x = 0.1,
                                  common.legend = TRUE, legend="bottom")

ggsave(twopanel_sensitivity, filename="paper_figures/figure6.png", width=13, height=7, dpi=300)
ggsave(twopanel_sensitivity, filename="paper_figures/figure6.pdf", width=13, height=7, dpi=300)


# ************************************************ Table 1 ************************************************** #

# see parameter_derivation.R



# ************************************************ Table 2 ************************************************** #
library(xtable)
booktabs()
if (rerun){
  full_results <- purrr::map_dfr(rcps, run_rcp, scenarios=c("default", "pf_full", "pf_wf"),
                                 scenario_names=c(NO_PERMAFROST_SCENARIO(), PERMAFROST_FULL_SCENARIO(), WARMING_ONLY_SCENARIO()), start_date=1745,
                                 end_date=2300, param_inputs=param_default, .id = "RCP")
  full_plot_results <- process_results(full_results)
  save(full_plot_results, file="paper_data/full_results.RData")
} else {
  load(file="paper_data/full_results.RData")
}

data <- full_plot_results %>% filter(scenario==PERMAFROST_FULL_SCENARIO(),
                        variable %in% c(PERMAFROST_C(), F_FROZEN()),
                        RCP %in% c("RCP45", "RCP85"), year %in% c(1850, 2005, 2010, 2014, 2100))

pf_remaining <- filter(data, variable==F_FROZEN(), year %in% c(1850, 2005, 2100)) %>% select(-c(variable, scenario))
pf_remaining <- pf_remaining %>% pivot_wider(names_from=year, values_from="value") %>%
  mutate("1850-2005"=`2005`/`1850`, "2005-2100"=`2100`/`2005`) %>%
  select(-c(`1850`, `2005`, `2100`))

# total permafrost C in 2010 (approximation of Hugelius data time period)
modern_pf_c <- filter(data, year==2010, scenario==PERMAFROST_FULL_SCENARIO(), variable==PERMAFROST_C(),
                      RCP == "RCP45") %>% select(-c(scenario, variable, RCP))  # RCP doesn't matter for historical time period

scenario <- c("---", "RCP4.5", "RCP4.5", "RCP8.5")
source <- c("Hugelius et al. (2014)", rep("Koven et al. (2013)",3))
variable <- c("Modern Permafrost Carbon 0-3m (Pg C)", "Remaining Permafrost Area 1850-2005 (%)",
              "Remaining Permafrost Area 2005-2100 (%)", "Remaining Permafrost Area 2005-2100 (%)")
value <- c(727, 84, 58, 29)  # 727 is all carbon in permafrost soils from Hugelius et al. (2014)
hector <- c(modern_pf_c$value, 100*pf_remaining$`1850-2005`[1], 100*pf_remaining$`2005-2100`)
table2_values <- tibble(Scenario=scenario, Source=source, Variable=variable, Value=value, Hector=hector)

out_table <- xtable(table2_values, digits=c(NA, NA,NA,NA,0,0))
print(out_table,floating=FALSE,latex.environments=NULL,booktabs=TRUE, include.rownames=FALSE, file="paper_tables/table2.tex")


# ************************************************ Table 3 ************************************************** #

library(tables)
library(Hmisc)
booktabs()

# generate table for paper
table_vars <- c("Tgav", "Ca", "CH4", "rh_thawedp_cum", "rh_ch4_cum", "permafrost_c", "soil_c", "detritus_c", "veg_c")

# need to have generated full_plot_results above

default <- filter(full_plot_results, scenario==NO_PERMAFROST_SCENARIO())

full_plot_results %>%
  filter(scenario==PERMAFROST_FULL_SCENARIO()) %>%
  mutate(value_type="Permafrost", default = default$value, diff=value-default,
         Change = ifelse((default-0)<0.0001, 100, round(100*(value-default)/default, digits=1)), Total=round(value, digits=1)) %>%
  filter(year=="2100", variable %in% table_vars) %>%
  select(var_full_name, variable, Total, Change, RCP) ->
  totals_2100

table_tex <- tabular((Output=var_full_name) ~ Heading(Scenario)*identity*Heading()*factor(RCP)*(Total + Change), data=droplevels(totals_2100))
latex.tabular(table_tex)  # copy and paste this into manuscript or latex file


# ************************************************ Table 4 ************************************************** #

# Permafrost remaining 2005-2100 already calculated for Table 2
pf_remaining_85 <- filter(pf_remaining, RCP=="RCP85")$`2005-2100`

# Permafrost Lost 2010-2299 (x10^6 km^2) RCP4.5, RCP8.5
# Initial area in McGuire et al is 14.1x10^6 km^2 in 2010
pf_lost <- full_plot_results %>% filter(variable=="f_frozen", scenario==PERMAFROST_FULL_SCENARIO(),
                        RCP %in% c("RCP45", "RCP85"), year %in% c(2010, 2299)) %>%
  select(-c(variable, scenario)) %>% pivot_wider(names_from = year, values_from=value) %>%
  mutate(`2299`=14.1*`2299`/`2010` ,`2010`=14.1, `2299-2010`=`2010`-`2299`, var_full_name="Permafrost Lost (x10^6 km^2)")


# Cumulative Permafrost CO2 Emissions 2010-2100 (Pg C) RCP8.5
pf_cum_co2 <- full_plot_results %>% filter(variable=="rh_thawedp_cum", year %in% c(2010,2100), scenario==PERMAFROST_FULL_SCENARIO(), RCP=="RCP85") %>%
  select(-c(scenario)) %>% pivot_wider(names_from=year, values_from=value) %>% mutate(`2100-2010`=`2100`-`2010`)
pf_cum_co2_all <- full_plot_results %>% filter(variable=="rh_thawedp_cum", year %in% c(2010,2100, 2200, 2300), scenario==PERMAFROST_FULL_SCENARIO()) %>%
  select(-c(scenario)) %>% pivot_wider(names_from=year, values_from=value) %>% mutate(`2100-2010`=`2100`-`2010`, `2200-2010`=`2200`-`2010`)
# pf_cum_co2_all
pf_cum_co2_2100 <- pf_cum_co2_all$`2100`

# Permafrost CH4 Flux Change 2010-2100 (Pg C yr−1) RCP8.5
pf_ch4_flux <- full_plot_results %>% filter(variable=="rh_ch4", year %in% c(2010,2100), scenario==PERMAFROST_FULL_SCENARIO(), RCP=="RCP85") %>%
  select(-c(scenario)) %>% pivot_wider(names_from=year, values_from=value) %>% mutate(`2100-2010`=1000*(16.04/12.01)*`2100`-`2010`, current_ch4=(16.04/12.01)*`2010`*1000)
#pf_ch4_flux

# relative mineralization of permafrost carbon compared to Knoblauch 2018 (22 g CH4 / kg C +/- 13 - Table S5)
pf_c_2010 <- full_plot_results %>% filter(variable==PERMAFROST_C(),year==2010, scenario==PERMAFROST_FULL_SCENARIO(), RCP=="RCP85")
pf_ch4_cum <- full_plot_results %>% filter(variable=="rh_ch4_cum", year %in% c(2010,2100), scenario==PERMAFROST_FULL_SCENARIO(), RCP=="RCP85") %>%
  select(-c(scenario)) %>% pivot_wider(names_from=year, values_from=value) %>%
  mutate(`2100-2010`=`2100`-`2010`, rel_min_2010_2100=`2100-2010`*1e15*(16.04/12.01)/(1e12*pf_c_2010$value)) %>%
  select(-var_full_name)

# Permafrost-Driven Temperature Change by 2100 RCP8.5
default_tgav <- full_plot_results %>% filter(variable=="Tgav", year %in% c(2100), scenario==NO_PERMAFROST_SCENARIO()) %>%
  select(-c(scenario, year))
pf_tgav <- full_plot_results %>% filter(variable=="Tgav", year %in% c(2100), scenario==PERMAFROST_FULL_SCENARIO()) %>%
  select(-c(scenario)) %>% mutate(diff=value-default_tgav$value, pct_diff=100*(value-default_tgav$value)/default_tgav$value)
tgav_range <- range(pf_tgav$diff)
tgav_pct_range <- range(pf_tgav$pct_diff)

default_tgav_2300 <- full_plot_results %>% filter(variable=="Tgav", year %in% c(2300), scenario==NO_PERMAFROST_SCENARIO()) %>%
  select(-c(scenario, year))
pf_tgav_2300 <- full_plot_results %>% filter(variable=="Tgav", year %in% c(2300), scenario==PERMAFROST_FULL_SCENARIO()) %>%
  select(-c(scenario)) %>% mutate(diff=value-default_tgav$value, pct_diff=100*(value-default_tgav$value)/default_tgav$value)

scenario <- c("RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", rep("RCP8.5", 3), "---", rep("RCP8.5", 4))
source <- c("Burke et al. (2020)", rep("McGuire et al. (2018)", 2), rep("MacDougall and Knutti (2016)",2), "Schuur et al. (2015)", "Koven et al. (2015)", "Kirschke et al. (2013)", "Koven et al. (2015)", "Knoblauch et al. (2018)", "Crichton et al. (2016)", "MacDougall et al. (2012)")
variable <- c("Permafrost Remaining 2005-2100 (%)",
              rep("Permafrost Lost 2010-2299 (x10$^6$ km$^2$)", 2),
              rep("Cumulative Permafrost CO2 Emissions 1850-2100 (Pg C)", 2),
              rep("Cumulative Permafrost CO2 Emissions 2010-2100 (Pg C)", 2),
              "Permafrost CH4 Flux 2010 (Tg C yr^{-1})",
              "Permafrost CH4 Flux Change 2010-2100 (Tg C yr^{-1})",
              "Relative Mineralization of Permafrost C 2010-2100 (g CH4 kg C^{-1})",
              "Permafrost-Driven Temperature Change by 2100 (%)",
              "Permafrost-Driven Temperature Change by 2100 (deg. C)")

value <- c("37", "4.1", "12.7", "71", "101", "92", "27.9-112.6", "30", "3.97-10.48", "22", "10-40%", "0.27 deg. C")
hector <- c(round(pf_remaining_85,2)*100, round(pf_lost$`2299-2010`,1), round(pf_cum_co2_2100[2]), round(pf_cum_co2_2100[4]), rep(round(pf_cum_co2$`2100-2010`,2),2),
            round(pf_ch4_flux$current_ch4,1), round(pf_ch4_flux$`2100-2010`,0), round(pf_ch4_cum$rel_min_2010_2100,1),
            paste0(format(round(tgav_pct_range[1], 2)), "-", format(round(tgav_pct_range[2], 1))),
            paste0(format(round(pf_tgav$diff[4],2))))


table4_values <- tibble(Scenario=scenario, Source=source, Variable=variable, Value=value, Hector=hector)

out_table4 <- xtable(table4_values, digits=c(NA, NA,NA,NA,0,0))
print(out_table4,floating=FALSE,latex.environments=NULL,booktabs=TRUE, include.rownames=FALSE, file="paper_tables/table4.tex")


# **************************************** Calculations of Numbers in Text **************************************** #

# full_plot_results variable used in many calculations below is stored in "paper_data/full_results.RData"

# Abstract

# 2100 % increase in CH4 concentration range across RCPs
default_ch4 <- full_plot_results %>% filter(variable=="CH4", year==2100, scenario== NO_PERMAFROST_SCENARIO())
pf_ch4 <- full_plot_results %>% filter(variable=="CH4", year==2100, scenario==PERMAFROST_FULL_SCENARIO()) %>%
  mutate(pct_diff = 100*(value-default_ch4$value)/default_ch4$value)
pf_ch4

# 2100 % increase in CO2 concentration range across RCPs
default_co2 <- full_plot_results %>% filter(variable=="Ca", year %in% c(2100, 2300), scenario== NO_PERMAFROST_SCENARIO())
pf_co2 <- full_plot_results %>% filter(variable=="Ca", year %in% c(2100, 2300), scenario==PERMAFROST_FULL_SCENARIO()) %>%
  mutate(diff = value-default_co2$value, pct_diff = 100*(value-default_co2$value)/default_co2$value)
pf_co2

pct_ranges <- round(c(range(pf_co2$pct_diff)[1], range(pf_co2$pct_diff)[2], range(pf_ch4$pct_diff)[1],
                range(pf_ch4$pct_diff)[2]),0)

sprintf("We found that by 2100 thawed permafrost carbon emissions increased Hector’s
        atmospheric CO\textsubscript{2} concentration by %.0f-%.0f%% and the atmospheric
        CH\textsubscript{4} concentration by %.0f-%.0f%%, depending on the future scenario.",
        pct_ranges[1], pct_ranges[2], pct_ranges[3], pct_ranges[4])


# 2100 temperature change range across RCPs
default_tas <- full_plot_results %>% filter(variable=="Tgav", year %in% c(2100, 2300), scenario== NO_PERMAFROST_SCENARIO())
pf_tas <- full_plot_results %>% filter(variable=="Tgav", year %in% c(2100, 2300), scenario==PERMAFROST_FULL_SCENARIO()) %>%
  mutate(diff = value-default_tas$value)
range(pf_tas$diff)
mean(pf_tas$diff)

# most significant parameters for temperature and atmospheric co2 by 2100 and 2300

all_sensitivity_results %>% select(-c(param2, output2, variable2)) %>%
  mutate(value=formatC(signif(value,digits=3), digits=3,format="fg", flag="#")) %>%  #format(value,digits=1,scientific=FALSE)) %>%
  filter(variable %in% c("Tgav", "CO2")) %>%
  pivot_wider(names_from=output,values_from=value) %>% select(c(variable, param,.data[["partial_var"]])) %>%
  arrange(param, variable) ->
  # arrange(desc(`Partial Variance`)) ->
  sensitivity_table_results

load("paper_data/sensitivity_results_all_2300.RData")
all_sensitivity_results_2300 <- process_sensitivity_results(sensitivity_results_co2_2300, sensitivity_results_ch4_2300, sensitivity_results_tgav_2300)

all_sensitivity_results_2300 %>% select(-c(param2, output2, variable2)) %>%
  mutate(value=formatC(signif(value,digits=3), digits=3,format="fg", flag="#")) %>%  #format(value,digits=1,scientific=FALSE)) %>%
  filter(variable %in% c("Tgav", "CO2")) %>%
  pivot_wider(names_from=output,values_from=value) %>% select(c(variable, param,.data[["partial_var"]])) %>%
  arrange(param, variable) ->
  sensitivity_table_results_2300

sensitivity_table_results_comb <- full_join(sensitivity_table_results, sensitivity_table_results_2300, by=c("param","variable"))

sensitivity_table_results_comb %>%
  mutate(pv_2100_col=round(as.numeric(partial_var.x),2), pv_2300_col=round(as.numeric(partial_var.y),2), diff=round(pv_2300_col-pv_2100_col,2)) %>%
  select(-c(partial_var.x, partial_var.y)) -> pv_compare_table

pv_compare_table

unique(sensitivity_table_results_2300$param2)


# Methods

# Comparison of tuning to CMIP6 vs CMIP5
tuned_thaw_params_cmip6 <- optim(c(1.953, 0.9619), run_for_optim, data=c(0.17, 0.54, 0.28), method="L-BFGS-B", lower=c(1.67, 0.84), upper=c(2.35, 1.11))
thaw_param_pct_diff <- 100*(tuned_thaw_params_cmip6$par-tuned_thaw_params$par)/tuned_thaw_params$par
print(paste0("tuning to these instead affected our permafrost thaw parameter values by less than ",round(max(thaw_param_pct_diff),1),"%"))

# Results

# permafrost lost from model start (1745) to 2100 across all RCPs
pf_c <- full_plot_results %>% filter(scenario==PERMAFROST_FULL_SCENARIO(), variable=="permafrost_c", year %in% c(1745, 2100)) %>%
  pivot_wider(names_from=year, values_from=value) %>% mutate(diff=`2100`-`1745`)
range(pf_c$diff[2:4])

# thawed permafrost pool peak timing
thawed_pc <- full_plot_results %>% filter(scenario==PERMAFROST_FULL_SCENARIO(), variable=="thawedp_c",
                                     year %in% seq(2000, 2100)) %>%
  pivot_wider(names_from="RCP", values_from=value) %>% select(-c(scenario, variable))

thawed_pc$year[thawed_pc$RCP26==max(thawed_pc$RCP26)]
thawed_pc$year[thawed_pc$RCP45==max(thawed_pc$RCP45)]
thawed_pc$year[thawed_pc$RCP60==max(thawed_pc$RCP60)]
thawed_pc$year[thawed_pc$RCP85==max(thawed_pc$RCP85)]

# absolute net land-atmosphere carbon flux in default, pf

default <- filter(full_plot_results, scenario==NO_PERMAFROST_SCENARIO())

default %>% filter(variable=="annual_c_flux") -> default_netLA_flux

pf_netLA_flux <- full_plot_results %>% filter(scenario==PERMAFROST_FULL_SCENARIO(), variable=="annual_c_flux") %>% mutate(diff=value-default_netLA_flux$value)

ggplot(data=default_netLA_flux, aes(x=year,y=value,colour=RCP)) +
  geom_line() +
  xlim(2000,2300) +
  simple_theme

ggplot(data=NULL) +
  geom_line(data=pf_netLA_flux, aes(x=year,y=value,linetype=RCP), color="red") +
  geom_line(data=pf_netLA_flux, aes(x=year,y=diff,linetype=RCP), color="gray") +
  geom_line(data=default_netLA_flux, aes(x=year,y=value,linetype=RCP), color="black") +
  xlim(2000,2300) +
  ylab("Net Land-Atmosphere Flux (Pg C/yr)") +
  simple_theme

# max difference & value at 2300 in net land-atmosphere c flux pf-default
max(pf_netLA_flux$diff)
pf_netLA_flux$diff[length(pf_netLA_flux$diff)]


# On the other hand, the changes in the non-permafrost land-atmosphere flux due to permafrost in Hector
# increased until the end of the century, after which they declined and resulted in net losses by 2300,
# driven by higher temperatures and thus increasing losses of soil carbon from heterotrophic respiration

# non-permafrost land-atmosphere flux (soil rh + detritus rh + npp) difference (pf-default) values 2000-2300
default %>% filter(variable=="nonpf_c_flux") -> default_nonpfLA_flux

pf_nonpfLA_flux <- full_plot_results %>% filter(scenario==PERMAFROST_FULL_SCENARIO(), variable=="nonpf_c_flux") %>% mutate(diff=value-default_nonpfLA_flux$value)
ggplot(data=NULL) +
  geom_line(data=pf_nonpfLA_flux, aes(x=year,y=value,linetype=RCP), color="red") +
  geom_line(data=pf_nonpfLA_flux, aes(x=year,y=diff,linetype=RCP), color="gray") +
  geom_line(data=default_nonpfLA_flux, aes(x=year,y=value,linetype=RCP), color="black") +
  xlim(2000,2300) +
  simple_theme


# methane effect on temperature
# % diff between temperature in run with and without methane (% diff of pf-default vs pf_noch4-default), diff in 2100
# use default_tas from above
noch4_tas <- full_plot_results %>% filter(variable=="Tgav", year==2100, scenario==WARMING_ONLY_SCENARIO())
pf_tas <- pf_tas %>% mutate(diff_ch4 = value-noch4_tas$value,
                            diff_co2 = noch4_tas$value-default_tas$value,
                            pct_diff_ch4 = 100*(diff_ch4)/diff) %>%
  select(-c(var_full_name,year,scenario))

# diff_ch4 = increase in temp from including methane in equation (full run - run with no ch4)
# diff_co2 = increase in temp from just co2, no methane (co2-only run - default run)
# diff = total difference in full pf run from default
pf_tas

# carbon emissions from the thawed pool - when do they drop to zero?

full_plot_results %>% filter(variable=="pf_c_flux", scenario==PERMAFROST_FULL_SCENARIO(), year>2000) -> pf_c_flux

ggplot(data=pf_c_flux, aes(x=year, y=value, color=RCP)) +
  geom_line() +
  simple_theme


# saturation of temperature impact of additional co2/ch4 over time
data_rcp85 <- run_rcp(rcp="85", scenarios="pf_full", scenario_names=PERMAFROST_FULL_SCENARIO(),
                      start_date=1745, end_date=2300, param_inputs=param_default, outvars_input=c(RF_TOTAL(), RF_CH4(), RF_CO2()))
data_rcp85_wide <- filter(data_rcp85,
                          variable %in% c(RF_TOTAL(),
                                          GLOBAL_TEMP(),
                                          ATMOSPHERIC_C(), RF_CH4(),
                                          RF_CO2())) %>%
  pivot_wider(id_cols=c(year, variable), names_from = variable, values_from=value) %>%
  mutate(frac_rf=(FCO2+FCH4)/Ftot, FCtot=FCO2+FCH4, FC_slope=1000*FCtot/atmos_c)

# FC_slope = W/m^2 per Eg C

data_rcp85_wide
#filter(data_rcp85_wide, year>2100)

ggplot(data=data_rcp85_wide, aes(x=atmos_c, y=FCtot))+
  geom_point() +
  simple_theme ->
  rf_vs_atmos_c
rf_vs_atmos_c

ggplot(data=data_rcp85_wide, aes(x=atmos_c, y=FC_slope))+
  geom_point() +
  simple_theme ->
  rf_slope
rf_slope

# total thawed permafrost carbon (thawed + emissions) by 2100

# fraction of permafrost carbon released into atmosphere by 2100 (diff bt thawed and pf lost)

# fraction of carbon taken up by ocean in 2100 and 2300 across all scenarios

# fraction of pf carbon remaining in atmosphere by 2100, 2300 across all scenarios

diff_c_stocks_all_years %>% filter(year %in% c(2050, 2100, 2300), scenario==PERMAFROST_FULL_SCENARIO()) %>% ungroup() ->
  c_stocks_out

full_plot_results %>% filter(year %in% c(2050, 2100, 2300), scenario==PERMAFROST_FULL_SCENARIO(),
                        variable %in% c("pf_c_cum")) ->
  pf_emissions


get_c_stock_vals("RCP26", 2050, c_stocks_out)
rcp_26_2100 <- get_c_stock_vals("RCP26", 2100, c_stocks_out)
rcp_26_2300 <- get_c_stock_vals("RCP26", 2300, c_stocks_out)

get_c_stock_vals("RCP45", 2050, c_stocks_out)
get_c_stock_vals("RCP45", 2100, c_stocks_out)
get_c_stock_vals("RCP45", 2300, c_stocks_out)

rcp_85_2050 <- get_c_stock_vals("RCP85", 2050, c_stocks_out)
rcp_85_2100 <- get_c_stock_vals("RCP85", 2100, c_stocks_out)
rcp_85_2300 <- get_c_stock_vals("RCP85", 2300, c_stocks_out)

rcp_26_2100
rcp_85_2100
rcp_26_2300
rcp_85_2300

c(rcp_26_2100$thawed[1], rcp_85_2100$thawed[1])  # all these values are the same across the rows

# percent emitted to atmosphere - pf_lost_frac, total emitted to atmosphere - pf_lost
c(rcp_85_2100$pf_lost[1], rcp_85_2100$pf_lost_frac[1])

c(rcp_26_2300$thawed[1], rcp_85_2300$thawed[1])

# percent emitted to atmosphere - pf_lost_frac, total emitted to atmosphere - pf_lost
c(rcp_85_2300$pf_lost[1], rcp_85_2300$pf_lost_frac[1])

c(rcp_26_2100$pf_lost[1], rcp_26_2100$pf_lost_frac[1])
c(rcp_26_2300$pf_lost[1], rcp_26_2300$pf_lost_frac[1])
rcp_26_2100
rcp_26_2300


# sensitivity analysis

all_sensitivity_results %>% select(-c(param2, output2, variable2)) %>%
  mutate(value=formatC(signif(value,digits=3), digits=3,format="fg", flag="#")) %>%  #format(value,digits=1,scientific=FALSE)) %>%
  filter(variable %in% c("Tgav", "CO2", "CH4")) %>%
  pivot_wider(names_from=output,values_from=value) %>% select(c(variable, param,.data[["partial_var"]])) %>%
  arrange(param, variable) ->
  # arrange(desc(`Partial Variance`)) ->
  sensitivity_table_results

load("paper_data/sensitivity_results_all_2300.RData")
all_sensitivity_results_2300 <- process_sensitivity_results(sensitivity_results_co2_2300, sensitivity_results_ch4_2300, sensitivity_results_tgav_2300)

all_sensitivity_results_2300 %>% select(-c(param2, output2, variable2)) %>%
  mutate(value=formatC(signif(value,digits=3), digits=3,format="fg", flag="#")) %>%  #format(value,digits=1,scientific=FALSE)) %>%
  filter(variable %in% c("Tgav", "CO2", "CH4")) %>%
  pivot_wider(names_from=output,values_from=value) %>% select(c(variable, param,.data[["partial_var"]])) %>%
  arrange(param, variable) ->
  # arrange(desc(`Partial Variance`)) ->
  sensitivity_table_results_2300

sensitivity_table_results_comb <- full_join(sensitivity_table_results, sensitivity_table_results_2300, by=c("param","variable"))

sensitivity_table_results_comb %>%
  mutate(pv_2100_col=round(as.numeric(partial_var.x),2), pv_2300_col=round(as.numeric(partial_var.y),2), diff=round(pv_2300_col-pv_2100_col,2)) %>%
  select(-c(partial_var.x, partial_var.y)) -> pv_compare_table

pv_compare_table

unique(sensitivity_table_results_2300$param2)


all_maxmin_data %>% mutate(yrange=abs(ymax-ymin))

fstatic <- filter(full_sensitivity_all_rcps, variable=="Tgav", rcp=="45", year==2100, param_name == "fpf_static")
range(fstatic$diff)
range(100*fstatic$diff/0.325)

rhch4 <- filter(full_sensitivity_all_rcps, variable=="Tgav", rcp=="45", year==2100, param_name == "rh_ch4_frac")
range(rhch4$diff)
range(100*rhch4$diff/0.325)

wf <- filter(full_sensitivity_all_rcps, variable=="Tgav", rcp=="45", year==2100, param_name == "warmingfactor")
range(wf$diff)
range(100*wf$diff/0.325)

pfc0 <- filter(full_sensitivity_all_rcps, variable=="Tgav", rcp=="45", year==2100, param_name == "permafrost_c0")
range(pfc0$diff)
range(100*pfc0$diff/0.325)

