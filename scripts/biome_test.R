library(hector)
library(hectortools)
library(devtools)


get_biome_inits <- function(core, biome) {
  # `fetchvars` requires date to be between start and end date, so
  # we need to call the lower-level `sendmessage` method here.
  current_data_1 <- rbind.data.frame(
    sendmessage(core, GETDATA(), VEG_C(biome), 0, NA, ""),
    sendmessage(core, GETDATA(), DETRITUS_C(biome), 0, NA, ""),
    sendmessage(core, GETDATA(), SOIL_C(biome), 0, NA, ""),
    sendmessage(core, GETDATA(), PERMAFROST_C(biome), 0, NA, "")
  )
  current_data_2 <- fetchvars(core, NA, c(NPP_FLUX0(biome),
                                          BETA(biome),
                                          WARMINGFACTOR(biome)))[, -1]
  current_data <- rbind.data.frame(current_data_1, current_data_2)
  current_values <- current_data[["value"]]
  names(current_values) <- gsub(paste0(biome, "."), "",
                                current_data[["variable"]], fixed = TRUE)
  current_values
}

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging = TRUE)
invisible(run(core, 2100))
result_vars <- c(ATMOSPHERIC_CO2(), RF_TOTAL(), GLOBAL_TEMP(),
                 VEG_C(), SOIL_C(), DETRITUS_C(), ATMOSPHERIC_CH4())
reference_results <- fetchvars(core, 2000:2100, result_vars, scenario = "reference")

#rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
#core <- newcore(rcp45, suppresslogging = FALSE)
invisible(reset(core))
setvar(core, 0, PERMAFROST_C(), 1035, "PgC")
invisible(run(core, 2100))
perm_results <- fetchvars(core, 2000:2100, result_vars, scenario = "permafrost")




#permafrost run
rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
perm_ini <- modifyList(rcp45, list(simpleNbox = list(
  permafrost_c = 1035
)))
#write_ini(perm_ini, system.file("input", "hector_rcp45_perm.ini", package = "hector"))
write_ini(perm_ini, "./hector_rcp45_perm.ini")
perm_45 <- "./hector_rcp45_perm.ini"


#rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(perm_45, suppresslogging = FALSE)
#invisible(reset(core))
get_biome_list(core)
split_biome(core, "global", c("non-permafrost", "permafrost"),
            fveg_c = c(0.98, 0.02),
            fdetritus_c = c(0.98, 0.02),
            fsoil_c = c(0.89, 0.11),
            fpermafrost_c = c(0.0000001,0.9999999),
            rh_ch4_frac = c(0, 0.01))
invisible(run(core, 2100))
warming_results <- fetchvars(core, 2000:2100, result_vars, scenario = "permafrost")






# run scenario with permafrost but no biome split

#rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(perm_45, suppresslogging = FALSE)
#setvar(core, 0, PERMAFROST_C(), 1035, "PgC")
invisible(run(core, 2100))
perm_results <- fetchvars(core, 2000:2100, result_vars, scenario = "permafrost only")

invisible(reset(core))
# run scenario with permafrost split and active




plot_data <- rbind(reference_results, warming_results)
plot_data$variable <- factor(plot_data$variable, result_vars)
ggplot(plot_data) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y") +
  theme_bw()




