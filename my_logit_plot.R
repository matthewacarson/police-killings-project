library(openintro)
library(splines)
library(dplyr)

d <- all_tracts$income_population_quintiles_2020
m <- logit_1
p  <- predict(m, type = "response")
p. <- p


noise <- rnorm(nrow(d), sd = 0.08)

xlimit <- 0.18
ylimit <- 0.16

plot(p, d$luof_boolean + noise / 5,
     type = "n",
     xlim = c(0, xlimit),
     ylim = c(0, ylimit),
     axes = FALSE,
     xlab = "",
     ylab = "")
par(las = 1)
rect(0, 0, 1, 1,
     border = COL[6],
     col = "#00000000",
     lwd = 1.5)
lines(0:1, 0:1,
      lty = 2,
      col = COL[6],
      lwd = 1.5)
at <- seq(0, 1, length.out = length(seq(0, ylimit, 0.002)))
axis(2, at)
# axis(1, at)
title(xlab = "Predicted Probability")
axis(1, at, )
eps <- 1e-4
bucket_breaks <- quantile(p, seq(0, 1, 0.02))
bucket_breaks[1] <- bucket_breaks[1] - eps
n_buckets <- length(bucket_breaks) - 1
bucket_breaks[n_buckets] <- bucket_breaks[n_buckets] + 1e3 * eps
bucket_breaks. <- bucket_breaks

n_buckets <- length(bucket_breaks)
xp <- rep(NA, n_buckets)
yp <- rep(NA, n_buckets)
yp_lower <- rep(NA, n_buckets)
yp_upper <- rep(NA, n_buckets)
zs <- qnorm(0.975)
for (i in 1:n_buckets) {
  these <- bucket_breaks[i] < p & p <= bucket_breaks[i + 1]
  xp[i] <- mean(p[these])
  y <- d$luof_boolean[these]
  yp[i] <- mean(y)
  yp_lower[i] <- yp[i] - zs * sqrt(yp[i] * (1 - yp[i]) / length(y))
  yp_upper[i] <- yp[i] + zs * sqrt(yp[i] * (1 - yp[i]) / length(y))
}
points(xp, yp, pch = 19, cex = 0.7)
segments(xp, yp_lower, xp, yp_upper)
