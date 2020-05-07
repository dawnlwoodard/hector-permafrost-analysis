library(hector)
library(hectortools)

result_vars <- c(ATMOSPHERIC_CO2(), RF_TOTAL(), GLOBAL_TEMP(),
                 VEG_C(), SOIL_C(), DETRITUS_C(), ATMOSPHERIC_CH4(), THAWEDP_C())

# set permafrost in ini file
rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
perm_ini <- modifyList(rcp45, list(simpleNbox = list(
  permafrost_c = 1035
)))
write_ini(perm_ini, "./hector_rcp45_perm.ini")
perm45 <- "./hector_rcp45_perm.ini"
core <- newcore(perm45, suppresslogging = TRUE)

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging = FALSE)
run(core)
results <- fetchvars(core, 2000:2100, result_vars)
head(results) 
