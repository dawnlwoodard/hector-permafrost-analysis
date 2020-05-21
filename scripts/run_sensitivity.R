library(hector)
library(stats)
library(tidyverse)

source("scripts/analysis_tools.R")

n_pts <- 50
n_runs <- 100
outvar <- GLOBAL_TEMP()

# Permafrost parameter priors. Currently assuming normal distribution. Should look into making more accurate.
# Should add warming factor to this
priors <- tibble(fpf_static = urnorm(n_pts, 0.4, 0.2, lb=0, ub=1), 
                 pf_mu = rnorm(n_pts, 1.25, 0.13), 
                 pf_sigma = rnorm(n_pts, 0.618, 0.09),
                 permafrost_c = urnorm(n_pts, 1035, 150, lb=0),
                 rh_ch4_frac = urnorm(n_pts, 0.023, 0.01, lb=0, ub=1))

# generate hector results from n_runs random samples from the priors and store in tibble
hector_output <- run_all_param_sets(priors, n_runs, outvar)

# run Pecan-like sensitivity analysis over Hector results
sensitivity_results <- run_sensitivity_analysis(out, names(out)[1], names(out)[-1])

# make table of results