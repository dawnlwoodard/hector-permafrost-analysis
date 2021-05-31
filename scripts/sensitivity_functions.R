#' Parameter sensitivity analysis
#'
#' Perform a PEcAn-like parameter sensitivity analysis to analyze the
#' contribution of parameters to predictive uncertainty.
#'
#' This analysis produces three key metrics:
#'
#' - "Coefficient of variation (CV)" describes the relative
#' uncertainty of the input parameter. It is calculated as the ratio
#' between the input parameter variance and its median value.
#'
#' - "Elasticity" is the normalized sensitivity of the model to a
#' change in one parameter.
#'
#' - "Partial variance" is the fraction of variance in the model
#' output that is explained by the given parameter. In essence, it
#' integrates the information provided by the CV and elasticity.
#'
#' The theory and implementation are based on the sensitivity analysis
#' described by [LeBauer et al.
#' (2013)](https://doi.org/10.1890/12-0137.1), but with several key
#' differences:
#'
#' - LeBauer et al. use a cubic spline interpolation through each
#' point in the model output. This function uses a generalized
#' additive model regression ([mgcv::gam()]).
#' - LeBauer et al. fit individual splines to each parameter-output
#' combination where other parameters are held constant at their
#' median. This function fits a multivariate generalized additive
#' regression model, and then uses that fit to calculate the partial
#' derivatives.
#'
#' @param df `data.frame` of Hector results.
#' @param xcols Character vector of parameter column names
#' @param ycol Name of column containing response variable
#' @param .type Whether the multivariate fit is "additive" (`Y ~ a + b +
#'   c`, default) or "interactive" (`Y ~ a * b * c`). This argument
#'   supports partial matching via [base::match.arg()].
#' @return `data.frame` of sensitivity analysis results. See Details.
#' @author Alexey Shiklomanov
#' @export
run_pecan_analysis <- function(dat, ycol, xcols, .type = "additive"){
  modtype <- match.arg(.type, c("additive", "interactive"))
  .collapse <- switch(modtype, additive = " + ", interactive = " * ")
  rhs <- paste(sprintf("s(%s)", xcols), collapse = .collapse)
  form <- paste(ycol, rhs, sep = " ~ ")
  print(c(rhs,ycol,form))
  fit <- mgcv::gam(as.formula(form), data = dat)
  xmed <- apply(dat[, xcols], 2, median)
  xcv <- (apply(dat[, xcols], 2, var) / xmed) %>%
    tibble::enframe("param", "cv")
  pred <- apply(dat[, xcols], 2, quantile, probs = c(0.49, 0.51))
  dpred <- apply(pred, 2, diff)
  ymed <- median(dat[[ycol]])
  
  # Elasticity
  el_inputs <- lapply(
    xcols,
    function(x) {
      as.data.frame(c(rlang::list2(
        !!x := pred[, x],
        !!!xmed[xcols != x]
      )))
    }
  )
  
  names(el_inputs) <- xcols
  el_results <- lapply(el_inputs, predict, object = fit)
  print
  ("el_results")
  print(el_results)
  print("dpred,ymed,xmed")
  print(c(dpred,ymed,xmed))
  el_slope <- ((vapply(el_results, diff, numeric(1)) / dpred) /
                 (ymed / xmed)) %>%
    tibble::enframe("param", "elasticity")
  
  # Prediction variance
  pred_inputs <- lapply(
    xcols,
    function(x) {
      as.data.frame(c(rlang::list2(
        !!x := dat[[x]],
        !!!xmed[xcols != x]
      )))
    }
  )
  names(pred_inputs) <- xcols
  pred_results <- lapply(pred_inputs, predict, object = fit)
  pred_var <- vapply(pred_results, var, numeric(1)) %>%
    tibble::enframe("param", "pred_var") %>%
    dplyr::mutate(partial_var = pred_var / sum(pred_var))
  
  purrr::reduce(list(xcv, el_slope, pred_var), dplyr::full_join, by = "param")
}

pick_params <- function(param_set) {
  rows <- sample(1:nrow(param_set), ncol(param_set))
  out <- param_set[1,]
  for (i in 1:ncol(param_set)){
    out[1,i] <- param_set[rows[i],i]
  }
  return(out)
}

process_sensitivity_results <- function(co2_results, ch4_results, tas_results){
  param_full_names <- c(fpf_static="Static Fraction", 
                        pf_mu="Frozen Permafrost\nFraction Mean", 
                        pf_sigma = "Frozen Permafrost\nFraction Stdev", 
                        permafrost_c = "Initial Permafrost\nPool Size (Pg C)", 
                        rh_ch4_frac = "Methane Fraction\nfrom Thaw Decomp.",
                        warmingfactor = "High Latitutude\nWarming Factor",
                        nonpf_c = "Non-Permafrost C in\nPermafrost Region (Pg C)"
  )
  var_full_names <- c(Tgav="Temperature Anomaly", CO2="Atmospheric CO2", CH4="Atmospheric CH4")
  output_full_names <- c(cv="Coefficient of Variation", elasticity="Elasticity", 
                         pred_var="Prediction Variance", partial_var="Partial Variance")
  
  co2_results %>% pivot_longer(
    cols = c("cv", "elasticity", "pred_var", "partial_var"),
    names_to = "output",
    values_to = "value") %>%
    mutate(variable = "CO2",
           param2=recode(param,!!!param_full_names),
           variable2=recode(variable,!!!var_full_names),
           output2=recode(output,!!!output_full_names)) %>% 
    replace_na(list(value=0)) -> co2_results
  
  ch4_results %>% pivot_longer(
    cols = c("cv", "elasticity", "pred_var", "partial_var"),
    names_to = "output",
    values_to = "value"
  ) %>% mutate(variable = "CH4",
               param2=recode(param,!!!param_full_names),
               variable2=recode(variable,!!!var_full_names),
               output2=recode(output,!!!output_full_names)) %>% 
    replace_na(list(value=0)) -> ch4_results
  
  tas_results %>% pivot_longer(
    cols = c("cv", "elasticity", "pred_var", "partial_var"),
    names_to = "output",
    values_to = "value"
  ) %>% mutate(variable = GLOBAL_TEMP(),
               param2=recode(param,!!!param_full_names),
               variable2=recode(variable,!!!var_full_names),
               output2=recode(output,!!!output_full_names)) %>% 
    replace_na(list(value=0)) -> tgav_results
  
  all_sensitivity_results <- bind_rows(tgav_results, co2_results, ch4_results)
  
  all_sensitivity_results$variable2 <- factor(x=all_sensitivity_results$variable2, levels=unique(all_sensitivity_results$variable2), ordered=TRUE)
  return(all_sensitivity_results)
}

# bounds are required by only one type, either/or
run_simple_sensitivity <- function(params, default_values, outvars, n_pts = 50, bound_frac=NULL, bounds=NULL, model_list=NULL, rcp="45"){
  all_data <- NULL
  # loop over params
  for (param in params){
    default <- default_values[[param]]
    print(paste0("starting param: ", param, "= ", default))

    if (!is.null(bound_frac)) {
      param_range <- seq(default-bound_frac*default, default+bound_frac*default, length.out = n_pts) 
    } else {
      param_range <- seq(bounds[[param]][[1]], bounds[[param]][[2]], length.out = n_pts)
    }

    if (param == "permafrost_c0"){
      sensitivity_data <- run_with_ini_range(param_range, default, param, outvars, params_input=default_values, rcp=rcp)
    } else if (param == "nonpf_c"){
      sensitivity_data <- run_with_ini_range(param_range, default, param, outvars, params_input=default_values, model_list=model_list, rcp=rcp)
    
    } else if (param %in% c("warmingfactor", "rh_ch4_frac", "pf_mu", "pf_sigma", "fpf_static")){
      core <- init_rcp(rcp=rcp, param_list=default_values)  # TODO want to update this to use input defaults to be safe
      # param name must be exact here
      sensitivity_data <- run_with_param_range(core, paste0("permafrost.",param), param_range, default, outvars)
    } else {
      print("Unrecognized parameters")
      exit()
    }
    all_data <- rbind(all_data, sensitivity_data)
    
  }
  return(all_data)

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
  return(result)
}

run_with_param_set <- function(param_set, outvar=GLOBAL_TEMP(), end_date=2100, rcp="45", run_only=FALSE, model_list=get_nonpfc_modellist()) {
  print("starting new param set")
  value <- param_set$nonpf_c
  veg_val <- value*get_lm_prediction(model_list[["veg"]], value)
  soil_val <- value*get_lm_prediction(model_list[["soil"]], value)
  litter_val <- value*get_lm_prediction(model_list[["litter"]], value)
  tot_c <- veg_val+soil_val+litter_val
  extra_c <- value-tot_c
  pf_soil <- max(soil_val + (soil_val/tot_c)*extra_c,0,01)  # using tot_c not value so that proportions add to 1
  pf_veg <- max(veg_val + (veg_val/tot_c)*extra_c,0.01)
  pf_det <- max(litter_val + (litter_val/tot_c)*extra_c,0.01)
  
  # core <- init_rcp(rcp, pf_c0 = param_set$permafrost_c, rh_ch4_frac = param_set$rh_ch4_frac, wf = param_set$warmingfactor, pf_soil = pf_soil, pf_veg=pf_veg, pf_det=pf_det)
  core <- init_rcp(rcp=rcp, param_list=param_set)
  
  # TODO update 1:3 so order can change
  Map(function(x, y) set_param(core, y, x, biome="permafrost"), unlist(param_set[,1:3], use.names=FALSE), names(param_set[,1:3]))
  run(core)
  if (!run_only){
    fetchvars(core, 2000:end_date, outvar) %>% 
      filter(year==end_date) %>%  
      select(value) ->
      results
    
    output <- cbind(results, param_set)
    return(output)
  }
}

run_all_param_sets <- function(param_priors, n_runs, outvar=GLOBAL_TEMP(), end_date=2100){
  model_list <- get_nonpfc_modellist()
  out <- do.call("rbind", replicate(n_runs, run_with_param_set(pick_params(param_priors), outvar, end_date, model_list=model_list), simplify = FALSE))
  return(out)
}