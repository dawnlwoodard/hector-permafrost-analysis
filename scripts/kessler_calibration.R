cdfun <- function(x, mu, sig) plnorm(x, mu, sig, lower.tail = FALSE)
kessler_line <- function(x){
  kessler_b = 0.8 * kessler_m + 1
  pmin(kessler_b - kessler_m * x, 1)
}

fit_to_kessler <- function(kessler_m){
  fit <- optim(c(3.5, 2), pfit)
}

pfit <- function(pars) {
  mu <- pars[1]
  sig <- pars[2]
  x <- seq(0.01, 6, 0.1)
  yline <- kessler_line(x)
  yfun <- cdfun(x, mu, sig)
  ## sum((yline - yfun) ^ 2 * (1 / x^1.8))
  sum((yline - yfun) ^ 2)
}


kessler_m <- 0.172
fit <- fit_to_kessler(kessler_m)
kessler_m <- 0.1981
fit_upper <- fit_to_kessler(kessler_m)
kessler_m <- 0.1459
fit_lower <- fit_to_kessler(kessler_m)

kessler_m <- 0.172

library(ggplot2)

my_theme <- theme_classic() + 
  theme(legend.title=element_blank(), 
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.text = element_text(size=12),
        axis.text = element_text(color="black", size=12), 
        axis.title = element_text(color = "black", size=12))

setEPS(bg = "white", family = "Times")
postscript("figures/kessler_calibration.eps", width=7, height=4)
ggplot(data.frame(x=c(0,6)), aes(x=x)) +
  stat_function(fun = kessler_line, aes(colour = "Kessler (2017)"), size=1) + 
  stat_function(fun = cdfun, args=list(fit$par[1], fit$par[2]), aes(colour="Hector"), size=1) + 
  stat_function(fun = cdfun, args=list(fit_upper$par[1], fit_upper$par[2]), linetype="dashed", aes(colour="Hector")) +
  stat_function(fun = cdfun, args=list(fit_lower$par[1], fit_lower$par[2]), linetype="dashed", aes(colour="Hector")) +
  my_theme +
  scale_y_continuous("Frozen Permafrost Fraction") +
  scale_x_continuous(expression(paste("Temperature Change (", degree*C, ")"))) +
  scale_colour_manual(NA, values = c("red", "black"))
dev.off()

fit$par[1]
fit_upper$par[1]
fit_lower$par[1]

fit$par[2]
fit_upper$par[2]
fit_lower$par[2]


curve(kessler_line(x), 0, 6, col = "blue", ylim = c(0, 1))
curve(cdfun(x, fit$par[1], fit$par[2]), 0, 6, add = TRUE)
curve(cdfun(x, fit_upper$par[1], fit_upper$par[2]), 0, 6, col='red', add = TRUE)
curve(cdfun(x, fit_lower$par[1], fit_lower$par[2]), 0, 6, col='violet', add = TRUE)
abline(h = c(0, 1), lty = 2)







kessler_b_upper <- 0.8 * kessler_m + 1
kessler_line_upper <- function(x) pmin(kessler_b - kessler_m * x, 1)
fit_upper <- optim(c(3.5, 2), pfit)

kessler_m_lower <- 0.1459
kessler_b <- 0.8 * kessler_m + 1
kessler_line_lower <- function(x) pmin(kessler_b - kessler_m * x, 1)
fit_lower <- optim(c(3.5, 2), pfit)



curve(kessler_line(x), 0, 6, col = "blue", ylim = c(0, 1))
curve(cdfun(x, fit$par[1], fit$par[2]), 0, 6, add = TRUE)
curve(cdfun(x, fit_upper$par[1], fit_upper$par[2]), 0, 6, col='red', add = TRUE)
curve(cdfun(x, fit_lower$par[1], fit_lower$par[2]), 0, 6, col='violet', add = TRUE)
abline(h = c(0, 1), lty = 2)
fit$par[1]
fit$par[2]

fit_upper$par[1]
fit_upper$par[2]

fit_lower$par[1]
fit_lower$par[2]
