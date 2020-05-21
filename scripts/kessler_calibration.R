library(dplyr)
library(ggplot2)

my_theme <- theme_classic() + 
  theme(legend.title=element_blank(), 
        legend.position = c(1, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.text = element_text(size=12),
        axis.text = element_text(color="black", size=12), 
        axis.title = element_text(color = "black", size=12))

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
  sum((yline - yfun) ^ 2)
}

kessler_m <- 0.172
fit <- fit_to_kessler(kessler_m)
kessler_m <- 0.1981
fit_upper <- fit_to_kessler(kessler_m)
kessler_m <- 0.1459
fit_lower <- fit_to_kessler(kessler_m)

# Create a data frame to hold the curves data
x <- seq(0.01, 6, 0.1)
kessler_m <- 0.172
kessler <- kessler_line(x)
kessler_m <- 0.1981
kessler_upper = kessler_line(x)
kessler_m <- 0.1459
kessler_lower = kessler_line(x)

curves <- tibble(x = x, kessler_lower = kessler_lower, kessler_upper=kessler_upper, kessler=kessler, funline = cdfun(x, fit$par[1], fit$par[2]), 
                 funline_upper = cdfun(x, fit_upper$par[1], fit_upper$par[2]), 
                 funline_lower=cdfun(x, fit_lower$par[1], fit_lower$par[2]))

ggplot(data=curves) +
  geom_line(aes(x=x, y=kessler_upper, colour = "Kessler (2017)"), show.legend=FALSE, linetype="dashed") +
  geom_line(aes(x=x, y=kessler_lower, colour = "Kessler (2017)"), show.legend=FALSE, linetype="dashed") + 
  geom_line(aes(x=x, y=kessler, colour = "Kessler (2017)"), size=1) +
  geom_line(aes(x=x, y = funline, colour="Hector"), size=1) + 
  geom_ribbon(aes(x=x, ymin = funline_lower, ymax = funline_upper, fill="Hector"), show.legend=FALSE, alpha = 0.3)+
  my_theme +
  scale_y_continuous("Frozen Permafrost Fraction") +
  scale_x_continuous(expression(paste("Temperature Change (", degree*C, ")"))) +
  scale_colour_manual(NA, values = c("red", "black")) ->
  kessler_fit_plot
ggsave(kessler_fit_plot, filename="figures/kessler_calibration.eps", device=cairo_ps, width=6, height=3, dpi=300)
