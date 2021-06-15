# Derive model parameters
library(stats)
library(hector)

fit_to_kessler <- function(kessler_m){
  fit <- optim(c(3.5, 2), pfit, kessler_m=kessler_m)
}

pfit <- function(pars, kessler_m) {
  mu <- pars[1]
  sig <- pars[2]
  x <- seq(0.01, 6, 0.1)
  yline <- kessler_line(x, kessler_m)
  yfun <- cdfun(x, mu, sig)
  sum((yline - yfun) ^ 2)
}

cdfun <- function(x, mu, sig) plnorm(x, mu, sig, lower.tail = FALSE)

kessler_line <- function(x, kessler_m){
  kessler_b <- 0.8268 * kessler_m*0.5 + 1
  pmin(kessler_b - kessler_m * x*0.5, 1)
}

get_permafrost_parameters <- function(retune=FALSE){
  param_default <- list()
  param_bounds <- list()

  # TODO saved last tuned parameter state so it can check if anything is different and retune automatically. or ask if it should.

  # ************************************************ PERMAFROST_C0 ************************************************* #

  hist_frac_lost <- 0.16  # Koven et al 2013

  pf_modern <- 727  # Pg C, Hugelius et al 2014
  pf_c0 <- pf_modern/(1-hist_frac_lost)
  param_default[PF_C0()] <- pf_c0

  pf_cv <- 150.0/1035  # range / best estimate from Hugelius 2014
  param_bounds[[PF_C0()]] <- c(pf_c0-pf_cv*pf_c0, pf_c0+pf_cv*pf_c0)


  # ************************************************ PF MU and SIGMA ************************************************* #

  if (retune){
    tuned_thaw_params <- optim(c(1.953, 0.9619), run_for_optim, data=c(0.16, 0.58, 0.29), method="L-BFGS-B", lower=c(1.67, 0.84), upper=c(2.35, 1.11))
  } else { tuned_thaw_params <- c(1.67, 0.9855)}

  kessler_m <- 0.172  # Kessler 2017
  kessler_sig <- 0.0261

  fit_default <- fit_to_kessler(kessler_m)
  fit_upper <- fit_to_kessler(kessler_m+kessler_sig*2)
  fit_lower <- fit_to_kessler(kessler_m-kessler_sig*2)

  fitted_upper_diff <- abs(fit_default$par - fit_upper$par)/fit_default$par
  fitted_lower_diff <- abs(fit_default$par - fit_lower$par)/fit_default$par

  kessler_mu_cv <- mean(fitted_upper_diff[1], fitted_lower_diff[1])
  kessler_sig_cv <- mean(fitted_upper_diff[2], fitted_lower_diff[2])

  tuned_thaw_mu_bounds <- c(tuned_thaw_params[1]*(1-kessler_mu_cv), tuned_thaw_params[1]*(1+kessler_mu_cv))
  tuned_thaw_sig_bounds <- c(tuned_thaw_params[2]*(1-kessler_sig_cv), tuned_thaw_params[2]*(1+kessler_sig_cv))

  # store thaw parameter default, upper, and lower bound values in output variables
  param_default[PF_MU()] <- tuned_thaw_params[1]
  param_default[PF_SIGMA()] <- tuned_thaw_params[2]
  param_bounds[[PF_MU()]] <- tuned_thaw_mu_bounds
  param_bounds[[PF_SIGMA()]] <- tuned_thaw_sig_bounds

  # CMIP6 fraction calculations shown in Python code in input folder


  # ************************************************ FPF_STATIC ************************************************* #

  fpf_static_default <- 0.74  # derived from Schaedel et al 2014
  fpf_static_sd <- 0.2336  # derived from Schaedel et al 2014

  param_default[FPF_STATIC()] <- fpf_static_default
  param_bounds[[FPF_STATIC()]] <- c(0.4, fpf_static_default+fpf_static_sd)  # using lower bound from Burke et al 2012, 2013

  # ************************************************ HL WARMING FACTOR ************************************************* #

  param_default[WARMINGFACTOR()]<-2.0
  param_bounds[[WARMINGFACTOR()]]<-c(1.75,2.25)
  param_default["default_wf"] <- 2.0

  # ************************************************ RH_CH4_FRAC ************************************************* #

  param_default[RH_CH4_FRAC()] <- 0.023
  param_bounds[[RH_CH4_FRAC()]] <- c(0.006, 0.04)

  # ************************************************ NONPF_C ***************************************************** #

  # TODO need to get nonpf_c separated values stored so they can be read by the get_nonpfc_modellist function
  param_default[NONPF_C()] <- 329.6
  param_bounds[[NONPF_C()]] <- c(266.8, 392.2)

  # calculate cmip6 values and non-pf c
  # veg & det values from CMIP6, soil from hugelius
  # f_veg = 0.030, f_veg_cv = 0.8077, tot veg = 550, lb = 3.17 ((f_veg - f_veg*f_veg_cv)*550), ub = 29.8, default = 16.5 (f_veg*550)
  # f_litter = 0.11, ub = 10.9, lb = 1.24, mean = 6.06
  # soil_c_pf_region = 307 Pg C, soil_cv = 150/1035 = 0.145, ub = 351.5, lb = 262.5
  # all non-pf C in pf biome: 262.5+1.24+3.17 = 266.8, 351.5+29.8+10.9 = 392.2, mean = 307+6.06+16.50 = 329.6

  param_default[["pf_soil"]] <- 307
  param_default[["pf_veg"]] <- 16.5
  param_default[["pf_det"]] <- 6.06

  return(list("defaults"=param_default, "bounds"=param_bounds))
}

