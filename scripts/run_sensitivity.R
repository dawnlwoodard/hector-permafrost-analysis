library(hector)
library(hectortools)
library(stats)
library(tidyverse)
library(Runuran)

source("scripts/analysis_tools.R")

n_pts <- 100
n_runs <- 1000
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
save(hector_output, file="hector_sensitivity_1000_runs.RData")

# run Pecan-like sensitivity analysis over Hector results
sensitivity_results <- run_sensitivity_analysis(hector_output, names(hector_output)[1], names(hector_output)[-1])
save(sensitivity_results, file="sensitivity_results.RData")

# make table of results
library(xtable)
sensitivity_results %>% 
  mutate(param=recode_factor(param, fpf_static = "Non-labile Fraction", 
                             pf_mu = "Frozen Permafrost Fraction Mean", 
                             pf_sigma = "Frozen Permafrost Fraction Stdev", 
                             permafrost_c = "Initial Permafrost Pool Size",
                             rh_ch4_frac = "Methane Fraction from Thaw Decomp")) ->
  sensitivity_results

out_table <- xtable(sensitivity_results, digits=c(0, NA,2,2,4,2))
names(out_table) <- c("Parameter", "Coefficient of Variation", "Elasticity", "Prediction Variance", "Partial Variance")
print(out_table,floating=FALSE,latex.environments=NULL,booktabs=TRUE, include.rownames=FALSE, file="table.tex")

