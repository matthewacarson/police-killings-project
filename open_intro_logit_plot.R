library(openintro)
library(splines)
library(dplyr)
a <- resume
d <- data.frame(
  callback = a$received_callback,
  job_city = a$job_city,
  college_degree = a$college_degree,
  years_experience = a$years_experience,
  honors = a$honors,
  military = a$military,
  email_address = a$has_email_address,
  race = a$race,
  gender = ifelse(a$gender == "m", "male", "female"))

d <- all_tracts$income_population_quintiles_2020
m <- logit_1
summary(m)
p  <- predict(m, type = "response")
p. <- p


noise <- rnorm(nrow(d), sd = 0.08)

plot(p, d$luof_boolean + noise / 5,
     type = "n",
     xlim = 0:1,
     ylim = c(-0.07, 1.07),
     axes = FALSE,
     xlab = "Predicted Probability",
     ylab = "")
par(las = 0)
mtext("Truth", 2, 5.5)
par(las = 1)
rect(0, 0, 1, 1,
     # border = COL[6],
     col = "#00000000",
     lwd = 1.5)
lines(0:1, 0:1,
      lty = 2,
      # col = COL[6],
      lwd = 1.5)
points(p, d$luof_boolean + noise / 5,
       col = fadeColor(COL[1], "18"),
       pch = 20)
axis(1)
at <- seq(0, 1, length.out = 6)
labels <- c("0 (No LUOF)",
            "0.2  ",
            "0.4  ",
            "0.6  ",
            "0.8  ",
            "1 (LUOF)")
axis(2, at, labels)
eps <- 1e-4
bucket_breaks <- quantile(p, seq(0, 1, 0.01))
bucket_breaks[1] <- bucket_breaks[1] - eps
n_buckets <- length(bucket_breaks) - 1
bucket_breaks[n_buckets] <- bucket_breaks[n_buckets] + 1e3 * eps
bucket_breaks. <- bucket_breaks
k <- 1
for (i in 1:n_buckets) {
  if (abs(bucket_breaks.[i] - bucket_breaks[k]) >= 0.01) {
    k <- k + 1
    bucket_breaks[k] <- bucket_breaks.[i]
  }
}
bucket_breaks <- bucket_breaks[1:k]

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
arrows(0.3, 0.17,
       0.24, 0.22,
       length = 0.07)
text(0.3, 0.15,
     paste("Observations are bucketed,",
           "then we compute the observed probability in each bucket (y)",
           "against the average predicted probability (x)",
           "for each of the buckets with 95% confidence intervals.",
           sep = "\n"),
     cex = 0.85, pos = 4)
