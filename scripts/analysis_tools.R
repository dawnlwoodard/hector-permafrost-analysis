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
sensitivity_analysis <- function(dat, xcols, ycol, .type = "additive") {
  modtype <- match.arg(.type, c("additive", "interactive"))
  .collapse <- switch(modtype, additive = " + ", interactive = " * ")
  rhs <- paste(sprintf("s(%s)", xcols), collapse = .collapse)
  form <- paste(ycol, rhs, sep = " ~ ")
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


run_sensitivity_analysis <- function(dat, ycol, xcols, .type = "additive"){
  modtype <- match.arg(.type, c("additive", "interactive"))
  .collapse <- switch(modtype, additive = " + ", interactive = " * ")
  rhs <- paste(sprintf("s(%s)", xcols), collapse = .collapse)
  form <- paste(ycol, rhs, sep = " ~ ")
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

run_all_param_sets <- function(param_priors, n_runs, outvar=GLOBAL_TEMP()){
  out <- do.call("rbind", replicate(n_runs, run_with_param_set(pick_params(param_priors), outvar), simplify = FALSE))
  return(out)
}

set_param <- function(core, param, value, biome=NA) {
  if (!is.na(biome)) param <- paste(biome, param, sep=".")
  old_value <- fetchvars(core, NA, param)
  unit <- as.character(old_value[["units"]])
  setvar(core, NA, param, value, unit)
  reset(core)
}

pick_params <- function(param_set) {
  rows <- sample(1:nrow(param_set), ncol(param_set))
  out <- param_set[1,]
  for (i in 1:ncol(param_set)){
    out[1,i] <- param_set[rows[i],i]
  }
  return(out)
}

run_with_param_set <- function(param_set, outvar=GLOBAL_TEMP()) {
  rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
  perm_ini <- modifyList(rcp45, list(simpleNbox = list(
    permafrost_c = param_set$permafrost_c
  )))
  write_ini(perm_ini, "./hector_rcp45_perm.ini")
  perm45 <- "./hector_rcp45_perm.ini"
  core <- newcore(perm45, suppresslogging = FALSE)
  split_biome(core, "global", c("non-permafrost", "permafrost"),
              fveg_c = c(0.98, 0.02),
              fdetritus_c = c(0.98, 0.02),
              fsoil_c = c(0.89, 0.11),
              fpermafrost_c = c(0.0000001,0.9999999),
              rh_ch4_frac = c(0,param_set$rh_ch4_frac))
  Map(function(x, y) set_param(core, y, x, biome="permafrost"), unlist(param_set[,1:3], use.names=FALSE), names(param_set[,1:3]))
  run(core)
  fetchvars(core, 2000:2100, outvar) %>% 
    filter(year==2100) %>%  
    mutate(temperature=value) %>% 
    select(temperature) ->
    tgav_res
  
  output <- cbind(tgav_res, param_set)
  #mapply(set_param, names(params), unlist(params[1,], use.names=FALSE), core=core)
  return(output)
}



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
