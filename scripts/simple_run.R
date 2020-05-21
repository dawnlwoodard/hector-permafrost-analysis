library(hector)
library(hectortools)

outvars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), F_FROZEN(), SOIL_C())

rcp45 <- system.file("input", "hector_rcp45.ini", package = "hector")
core <- newcore(rcp45, suppresslogging = FALSE)
run(core)
no_pf_res <- fetchvars(core, 2000:2100, outvars)


rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
perm_ini <- modifyList(rcp45, list(simpleNbox = list(
  permafrost_c = 1035
)))
write_ini(perm_ini, "./hector_rcp45_perm.ini")
perm45 <- "./hector_rcp45_perm.ini"
core <- newcore(perm45, suppresslogging = TRUE)
run(core)
pf_res <- fetchvars(core, 2000:2100, outvars)
