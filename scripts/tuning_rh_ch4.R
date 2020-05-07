library(hector)
library(hectortools)
library(devtools)
library(ggplot2)


result_vars <- c(ATMOSPHERIC_CO2(), EMISSIONS_CH4(), GLOBAL_TEMP(),
                 VEG_C(), SOIL_C(), DETRITUS_C())

# run global without permafrost 
rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging = TRUE)
invisible(run(core, 2100))
reference_results <- fetchvars(core, 2000:2100, result_vars, scenario = "reference")


# set permafrost in ini file
rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
perm_ini <- modifyList(rcp45, list(simpleNbox = list(
  permafrost_c = 1035
)))
write_ini(perm_ini, "./hector_rcp45_perm.ini")
perm45 <- "./hector_rcp45_perm.ini"


# run global with permafrost
core <- newcore(perm45, suppresslogging = FALSE)
invisible(run(core, 2100))
perm_results <- fetchvars(core, 2000:2100, result_vars, scenario = "permafrost global")


# split into permafrost and non-permafrost biomes
#core <- newcore(perm_45, suppresslogging = FALSE)
invisible(reset(core))
split_biome(core, "global", c("non-permafrost", "permafrost"),
            fveg_c = c(0.98, 0.02),
            fdetritus_c = c(0.98, 0.02),
            fsoil_c = c(0.89, 0.11),
            fpermafrost_c = c(0.0000001,0.9999999),
            rh_ch4_frac = c(0,0.07))
invisible(run(core, 2100))
biome_results <- fetchvars(core, 2000:2100, result_vars, scenario = "permafrost biomes")



plot_data <- rbind(reference_results, perm_results, biome_results)
plot_data$variable <- factor(plot_data$variable, result_vars)
ggplot(plot_data) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y") +
  theme_bw()
