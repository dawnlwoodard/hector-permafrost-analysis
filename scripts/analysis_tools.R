library(hectortools)
library(tidyverse)
library(data.table)


# TUNING FUNCTIONS

# data is fraction of permafrost lost 1850-2005, fraction of permafrost carbon lost 2005-2100 in RCP4.5 and RCP8.5
run_for_optim <- function(values=c(1.953, 0.9619), data=c(0.16, 0.58, 0.29)){
  core45 <- init_rcp(rcp="45") #pf run with default pf parameters
  core85 <- init_rcp(rcp="85")

  parameters <- c("pf_mu", "pf_sigma")

  for (i in seq(1,length(parameters))){
    parameters[i] = paste0("permafrost.",parameters[i])
  }
  print(parameters)
  print(values)
  run_with_params(core45, parameters, values)
  run_with_params(core85, parameters, values)

  historical <- fetchvars(core45, 1850:2005, F_FROZEN())
  rcp45 <- fetchvars(core45, 2005:2100, F_FROZEN())
  rcp85 <- fetchvars(core85, 2005:2100, F_FROZEN())

  pf_frac_lost_1850_2005 <- (historical$value[length(historical$value)] - historical$value[1])/historical$value[1]
  pf_frac_left_rcp45 <- rcp45$value[length(rcp45$value)]/rcp45$value[1]
  pf_frac_left_rcp85 <- rcp85$value[length(rcp85$value)]/rcp85$value[1]

  hector = c(-pf_frac_lost_1850_2005, pf_frac_left_rcp45, pf_frac_left_rcp85)

  frac_diff <- (hector-data)/data

  err <- sum((frac_diff)^2)^0.5

  return(err)
}

run_with_params <- function(core, parameters, values){
  for (i in seq(1,length(values))){
    old_value <- fetchvars(core, NA, parameters[i])
    unit <- as.character(old_value[["units"]])
    setvar(core, NA, parameters[i], values[i], unit)
  }
  reset(core)
  invisible(run(core))
}

# FUNCTIONS FOR RUNNING MODEL

set_param <- function(core, param, value, biome=NA) {
  if (!is.na(biome)) param <- paste(biome, param, sep=".")
  old_value <- fetchvars(core, NA, param)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, param, value, unit)
  reset(core)
}

run_with_param <- function(core, parameter, value) {

  old_value <- fetchvars(core, NA, parameter)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, parameter, value, unit)
  reset(core)
  run(core)
  result <- fetchvars(core, 2000:2100, outvars)
  result[["param_value"]] <- value
  if (grepl(".", parameter, fixed=TRUE)){
    result[["param_name"]] <- unlist(strsplit(parameter, ".", fixed=TRUE))[2]
  }
  else{
    result[["param_name"]] <- parameter
  }
  result
}

#' Run Hector with a range of parameter values
run_with_param_range <- function(core, parameter, values, default, outvars=NULL) {
  default_run <- run_with_param(core, parameter, default)
  invisible(reset(core))
  mapped <- Map(function(x) run_with_param(core, parameter, x), values)
  mapped_default <- Map(function(x) mutate(x, param_diff=(param_value-default)/default, default_val=default_run$value, diff=value-default_val,
                                           pct_diff=100*(value-default_val)/default_val), mapped)
  Reduce(rbind, mapped_default)
}

# model_list needed if target_param = nonpf_c
run_with_ini <- function(value, target_param, params, model_list=NULL, rcp="45", start_year=2000, end_year=2100) {
  # this function should not be accessed externally - only from run_with_ini_range so that params input is correct
  if (target_param=="nonpf_c"){
    veg_val <- value*get_lm_prediction(model_list[["veg"]], value)
    soil_val <- value*get_lm_prediction(model_list[["soil"]], value)
    litter_val <- value*get_lm_prediction(model_list[["litter"]], value)
    tot_c <- veg_val+soil_val+litter_val
    extra_c <- value-tot_c
    params[["pf_soil"]] <- max(soil_val + (soil_val/tot_c)*extra_c,0)  # using tot_c not value so that proportions add to 1
    params[["pf_veg"]] <- max(veg_val + (veg_val/tot_c)*extra_c,0)
    params[["pf_det"]] <- max(litter_val + (litter_val/tot_c)*extra_c,0)
    print(params[c("pf_soil", "pf_det", "pf_veg")])
  } else {
    params[target_param] <- value
  }

  core <- init_rcp(rcp=rcp, param_list = params)

  run(core)
  result <- fetchvars(core, start_year:end_year, outvars)
  result[["param_value"]] <- value
  result[["param_name"]] <- target_param
  return(result)
}


# Run Hector with a range of parameter values
run_with_ini_range <- function(values, default, target_param, model_list=NULL, params_input=list(), outvars=NULL, rcp="45") {

  print("starting run_with_ini_range")
  params <- list("permafrost_c0"=865, "warmingfactor"=2.0, "rh_ch4_frac"=0.023, "default_wf"=2.0, "fpf_static"=0.74, "pf_soil"=307, "pf_det"=6.06, "pf_veg"=16.5)
  params[names(params_input)] <- params_input  # overwrite with any values given in params_input
  print("starting default run")
  default_run <- run_with_ini(default, target_param, model_list=model_list, params=params, rcp=rcp)
  mapped <- Map(function(x) run_with_ini(x, target_param=target_param, params=params, model_list = model_list, rcp=rcp), values)
  mapped_default <- Map(function(x) mutate(x, param_diff=(param_value-default)/default, default_val=default_run$value, diff=value-default_val,
                                           pct_diff=100*(value-default_val)/default_val), mapped)
  Reduce(rbind, mapped_default)
}


get_lm_prediction <- function(lm_model, value){
  output <- lm_model$coefficients[[2]]*value + lm_model$coefficients[[1]]
  return(output)
}

get_slope <- function(y, x){
  data = tibble(y=y,x=x)
  lm_model <- lm(y~x, data=data)
  slope <- lm_model$coefficients[[2]]
  return(slope)
}


# This is all hard coded based on values derived from CMIP6 and Hugelius et al 2014
get_nonpfc_modellist <- function(){
  litter_fracs <- c(0.0047, 0.0184, 0.0278)  # fraction of total non_pf c in pf region (lb, default, ub)
  soil_fracs <- c(0.9835, 0.9314, 0.8962)
  veg_fracs <- c(0.0119, 0.0501, 0.0760)
  tot_non_pf_c <- c(266.8, 329.6, 392.2)
  nonpf_data <- tibble(tot_non_pf_c, litter_fracs, soil_fracs, veg_fracs)
  model_list <- list("litter"=lm(litter_fracs~tot_non_pf_c,data=nonpf_data),"veg"=lm(veg_fracs~tot_non_pf_c,data=nonpf_data), "soil"=lm(soil_fracs~tot_non_pf_c,data=nonpf_data))
  return(model_list)
}

add_column <- function(df, values, column){
  df %>% mutate({{column}}:=values) -> output
  return(output)
  #df %>% mutate({{column}}:=values)
}


get_c_stock_vals <- function(rcp, year_input, data){
  emissions_data <- full_plot_results %>% filter(year %in% c(2050, 2100, 2300), scenario==PERMAFROST_FULL_SCENARIO(),
                                                 variable %in% c("pf_c_cum"))
  data %>% filter(RCP=={{rcp}}, year=={{year_input}}, variable!='permafrost_c') -> data_rcp
  total <- sum(data_rcp$value)
  neg_vals <- data_rcp[data_rcp$value<0, "value"]
  thawed_p_val <- data_rcp[data_rcp$variable=="thawedp_c", "value"]
  pf_emissions <- emissions_data %>% filter(RCP=={{rcp}}, year=={{year_input}})

  if(nrow(neg_vals)>0){
    total_w_other <- abs(sum(data_rcp[data_rcp$value<0, "value"])) + pf_emissions$value
  } else { total_w_other <- pf_emissions$value}
  data_rcp <- mutate(data_rcp, pct = 100*value/pf_emissions$value, total=total, thawed=pf_emissions$value + thawed_p_val$value,
                     total_w_other=total_w_other, frac_other=100*value/total_w_other, pf_lost=pf_emissions$value, pf_lost_frac = pf_lost/thawed) %>%
    select(-c(scenario, var_full_name))
  return(data_rcp)
}


# vars to set is named list of param=value
init_rcp <- function(scenario="", rcp="45", param_list) {  # vars_to_set=list(), pf_c0=865, wf=2.0, rh_ch4_frac=0.023, fpf_static=0.74, default_wf=1.0, pf_soil=307, pf_det=13.4, pf_veg=21.5){

  params_to_set <- c(PF_MU(), PF_SIGMA())

  # TODO should read these in from ini file
  default_soil_c <- 2358  # updated from Hector's previous default value
  default_veg_c <- 550
  default_det_c <- 55
  # calc pf veg, det, soil fracs
  fpf_soil <- param_list[["pf_soil"]]/default_soil_c
  fpf_veg <- param_list[["pf_veg"]]/default_veg_c
  fpf_det <- param_list[["pf_det"]]/default_det_c

  if ( scenario=="default" ){
    param_list[[WARMINGFACTOR()]] <- param_list[["default_wf"]]
    param_list[[PF_C0()]] <- 0.0; param_list[[RH_CH4_FRAC()]] <- 0.0 }
  else if ( scenario=="pf" ){ param_list[[WARMINGFACTOR()]] <- param_list[["default_wf"]]; param_list[[RH_CH4_FRAC()]] <- 0.0 }
  else if ( scenario=="pf_ch4" ){ param_list[[WARMINGFACTOR()]] <- param_list[["default_wf"]]}
  else if ( scenario=="pf_wf" ){ param_list[[RH_CH4_FRAC()]] <- 0.0 }
  else if ( scenario=="pf_full") {
    # nothing to change
  }
  else {
    print("ERROR: Scenario not recognized. Exiting.")
    exit()
  }
  # not including pf_ch4_wf because all default values already correspond to this scenario

  rcp_ini <- read_ini(system.file("input", paste0("hector_rcp",rcp,".ini"), package = "hector"))
  perm_ini <- modifyList(rcp_ini, list(simpleNbox = list(
    permafrost_c = param_list[[PF_C0()]], # default for permafrost run is 825: 727/0.881
    soil_c = 2358
  )))
  write_ini(perm_ini, paste0("./hector_rcp",rcp,"_perm.ini"))

  core <- newcore(paste0("./hector_rcp",rcp,"_perm.ini"), suppresslogging = FALSE)
  split_biome(core, "global", c("non-permafrost", "permafrost"),
              fveg_c = c(1-fpf_veg, fpf_veg),
              fdetritus_c = c(1-fpf_det, fpf_det),
              fsoil_c = c(1-fpf_soil, fpf_soil),
              fpermafrost_c = c(0,1),
              warmingfactor = c(1.0,param_list[[WARMINGFACTOR()]]),
              fpf_static = c(param_list[[FPF_STATIC()]], param_list[[FPF_STATIC()]]),
              rh_ch4_frac = c(0,param_list[[RH_CH4_FRAC()]]))

  settable_param_vals <- param_list[params_to_set]
  if (length(settable_param_vals)>0){
    for (i in range(1,length(settable_param_vals))){
      core <- set_param(core, names(settable_param_vals)[i],settable_param_vals[[i]], biome="permafrost")
    }
  }

  return(core)
}

# Runs Hector with a given RCP over the specified permafrost-related scenarios under different modes, depending on desired output
# setting biome will get results from *only* that biome. Leaving it NA will return global results
# additional_outvars should be a list of strings of output variables to include in the results in addition to the default ones obtained based on the mode
run_rcp <- function(rcp, scenarios=NA, scenario_names=NA, start_date=2000, end_date=2100, get_c_stocks=FALSE, get_fluxes=FALSE, biome=NA,
                    param_inputs=list(), additional_outvars=NA) {

  params=list("fpf_static"=0.74, "warmingfactor"=2.0, "permafrost_c0"=865, "rh_ch4_frac"=0.023, "pf_veg"=16.5, "pf_det"=6.06, "pf_soil"=307)
  params[names(param_inputs)] = param_inputs
  print(params)
  if (anyNA(scenarios)) {
    scenarios <- c("default", "pf", "pf_ch4", "pf_wf", "pf_full")
    scenario_names <- c(NO_PERMAFROST_SCENARIO(), PERMAFROST_ONLY_SCENARIO(), METHANE_ONLY_SCENARIO(), WARMING_ONLY_SCENARIO(), PERMAFROST_FULL_SCENARIO())
  }

  if (get_c_stocks){
    outvars <- c(VEG_C(), DETRITUS_C(), SOIL_C(), OCEAN_C(), PERMAFROST_C(), THAWEDP_C(), ATMOSPHERIC_C())
    dates <- NA
  } else if (get_fluxes){
    outvars <- c(RH_DETRITUS(), RH_SOIL(), NPP(), OCEAN_CFLUX(), RH_CH4(), RH_THAWEDP())
    dates <- NA
  } else {
    outvars <- c(ATMOSPHERIC_C(), ATMOSPHERIC_CO2(), ATMOSPHERIC_CH4(), GLOBAL_TEMP(), F_FROZEN(), LUC_EMISSIONS(), FFI_EMISSIONS(),
                 RH_CH4(), RH_THAWEDP(), RH_SOIL(), RH_DETRITUS(), NPP(), VEG_C(),
                 DETRITUS_C(), SOIL_C(), PERMAFROST_C(), THAWEDP_C())

    if (is.character(biome)){
      outvars <- paste(biome, outvars, sep=".")
    }
    dates <- seq(start_date, end_date)
  }

  if (!is.na(outvars_input)){
    outvars <-c(outvars, outvars_input)
  }

  cores <- lapply(scenarios, init_rcp, rcp=rcp, param_list=params)

  lapply(cores, run, runtodate=end_date)
  results <- lapply(cores, fetchvars, dates=dates, vars=outvars)
  results_list <- map2(results, scenario_names, add_column, column="scenario")
  all_results <- tibble(rbindlist(results_list))

  if (get_c_stocks | get_fluxes){
    all_results <- mutate(all_results, year=end_date)
  }

  return(all_results)
}

# Adds full variable names for output and calculate additional quantities of interest
process_results <- function(results, pf_c0=825){
  # assume if any have a biome, then all do because this is not set up to distinguish biomes
  if (grepl(".",results$variable[1],fixed=TRUE)){
    split_biome <- strsplit(results$variable, ".", fixed=TRUE)
    results$variable <- unlist(split_biome)[seq(2,length(test),2)]
  }

  results %>%
    mutate(value=case_when(variable==PERMAFROST_C() & scenario==NO_PERMAFROST_SCENARIO()~pf_c0, TRUE~value)) %>%
    select(-units) %>%
    spread(variable, value) %>%
    group_by(scenario, RCP) %>%
    mutate(pf_carbon_lost = pf_c0-permafrost_c, pct_pf_lost = 100*pf_carbon_lost/pf_c0,
           ffi_emissions_sum = cumsum(ffi_emissions), luc_cum = cumsum(luc_emissions),
           rh_thawedp_cum = cumsum(rh_thawedp), rh_ch4_cum = cumsum(rh_ch4), soil_c = soil_c,
           rh_soil_cum = cumsum(rh_soil), rh_det_cum = cumsum(rh_det), npp_cum = cumsum(npp),
           net_carbon_flux = (rh_thawedp_cum + rh_ch4_cum - npp_cum + rh_soil_cum + rh_det_cum + luc_cum),
           all_rh = rh_thawedp+rh_ch4+rh_soil+rh_det, nonpf_rh = rh_soil+rh_det,
           nonpf_rh_cum = rh_soil_cum + rh_det_cum, nonpf_c_flux = (nonpf_rh - npp + luc_emissions), pf_c_flux = (rh_ch4 + rh_thawedp),
           nonpf_c_cum = nonpf_rh_cum - npp_cum + luc_cum, pf_c_cum = rh_ch4_cum+rh_thawedp_cum,
           annual_c_flux = (rh_thawedp+rh_ch4+rh_soil+rh_det-npp+luc_emissions),
           net_carbon_storage = soil_c+veg_c+detritus_c) %>%
    ungroup() %>%
    pivot_longer(-c(RCP, scenario, year), names_to = "variable", values_to = "value") %>%
    mutate(var_full_name = recode_factor(
      variable,
      pf_carbon_lost = "Permafrost Carbon Lost (Pg C)",
      ffi_emissions_sum = "Cumulative Fossil Fuel Emissions (Pg C)",
      ffi_emissions = "Fossil Fuel Emissions (Pg C/yr)",
      f_frozen = "Frozen Permafrost Fraction",
      permafrost_c = "Permafrost Carbon (Pg C)",
      pct_pf_lost = "Fraction Permafrost Lost (%)",
      thawedp_c = "Thawed Permafrost Carbon (Pg C)",
      rh_thawedp = "Permafrost CO2 Emissions (Pg C/yr)",
      rh_thawedp_cum = "Net Permafrost CO2 Emissions (Pg C)",
      rh_soil = "Non-Permafrost Soil Flux (Pg C/yr)",
      rh_soil_cum = "Net Non-Permafrost Soil Flux (Pg C)",
      rh_det = "Detritus Flux (Pg C/yr)",
      rh_det_cum = "Net Detritus Flux (Pg C)",
      nonpf_rh = "Non-Permafrost Respiration (Pg C/yr)",
      nonpf_rh_cum = "Cumulative Non-Permafrost Respiration (Pg C)",
      npp = "Net Primary Productivity (Pg C/yr)",
      npp_cum = "Cumulative NPP (Pg C)",
      luc_emissions = "LUC Emissions (Pg C/yr)",
      luc_cum = "Cumulative LUC Emissions (Pg C)",
      nonpf_c_flux = "Non-Permafrost Carbon Flux (Pg C/yr)",
      pf_c_flux = "Thawed Permafrost Carbon Flux (Pg C/yr)",
      net_carbon_storage = "Total Land Carbon Storage (Pg C)",
      net_carbon_flux = "Cumulative Land Carbon Flux (Pg C)",
      annual_c_flux = "Change in Net Land Carbon Flux (Pg C/yr)",
      Ca = "Change in Atmospheric CO2 (ppm)",
      atmos_c = "Atmospheric Carbon (Pg C)",
      rh_ch4 = "Permafrost CH4 Emissions (Pg C/yr)",
      rh_ch4_cum = "Net Permafrost CH4 Emissions (Pg C)",
      CH4 = "Change in Atmospheric CH4 (ppbv)",
      soil_c = "Non-Permafrost Soil Carbon (Pg C)",
      detritus_c = "Detritus Carbon (Pg C)",
      veg_c = "Vegetation Carbon (Pg C)",
      Tgav = "Change in Temperature (K)"
    )) ->
    results_out

  return(results_out)
}
