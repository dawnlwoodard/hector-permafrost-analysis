library(drake)
library(knitr)
library(here)
library(dplyr)
library(ggplot2)
library(hector)

# Permafrost exponential model
pfa <- 2.371
pfb <- -0.676
pfc <- -3.685
# Kessler linear model
kessler_m <- 0.172
kessler_b <- 0.8 * kessler_m + 1

sigmoid_cap <- paste(
    "Fraction of permafrost thaw as a function of change in",
    "global annual mean air temperature since pre-industrial (1750)."
)

setEPS(bg = "white", family = "Times")
postscript("../figures/hector_vs_kessler.eps", width=7, height=4)
plot(0, 0, type = "n", xlim = c(0, 8), ylim = c(0, 1),
     xlab = expression(Delta * T[air] ~ (K)),
     ylab = "Frac. permafrost remaining")
curve(1 - (1 + pfa * exp(pfb * x)) ^ pfc, 0, 10, col = 1, add = TRUE)
abline(a = kessler_b, b = -kessler_m, lty = "dashed")
abline(h = 1, lty = "dotted")
legend("topright", c("Hector", "Kessler (2015)"), lty = c("solid","dashed"),
       bg = "white")
dev.off()
permafrost_c_cap <- paste(
    "Effect of permafrost C emissions on scenarios."
)

run_rcp <- function(rcp) {
    inidir <- file.path("~", "Documents", "GitHub",
                        "hector_perm", "inst", "input")
    inifile <- normalizePath(file.path(inidir, paste0("hector_rcp", rcp, ".ini")))
    stopifnot(file.exists(inifile))
    hc <- newcore(inifile)
    dates <- seq(1750, 2100)
    run(hc, max(dates))
    outvars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP(), F_FROZEN(), SOIL_C())
    no_pf <- fetchvars(hc, dates, outvars, scenario = "Original")
    reset(hc)
    setvar(hc, 0, PERMAFROST_C(), 1035, "PgC")
    run(hc, max(dates))
    yes_pf <- fetchvars(hc, dates, outvars, scenario = "Permafrost")
    return(tibble::as_tibble(dplyr::bind_rows(no_pf, yes_pf)))
}

rcps <- c("26", "45", "60", "85")
names(rcps) <- paste0("RCP", rcps)
results <- purrr::map_dfr(rcps, run_rcp, .id = "RCP")

setEPS(bg = "white", family = "Times")
postscript("../figures/hector_4panel_results.eps", width=7, height=4)
results %>%
    mutate(variable = recode_factor(
        variable,
        Tgav = "Global mean temperature anomaly (K)",
        Ca = "Atmospheric CO2 (ppm)",
        soil_c = "Active soil C pool (PgC)",
        f_frozen = "Frozen permafrost fraction"
    )) %>%
    ggplot() +
    aes(x = year, y = value, linetype = scenario, color = RCP) +
    geom_line() +
    facet_wrap(vars(variable), scales = "free_y") +
    theme_bw() +
    theme(axis.title = element_blank())
dev.off()

