# count number of killings in tracts
source('summary_tables.R')
# Distribution of counts at the tract level

# Count of the number of tracts with "n" LUOFs.

table(table(fatal_enc$joined$GEOID))

# 1     2     3     4    5    9 
# 5796  479   54    6    2    1

# Total count of LUOFs.
table(table(fatal_enc$joined$GEOID)) |> sum()

# [1] 6338

# Number of tracts without a LUOF

nrow(all_tracts$income_population_quintiles_2020) - 
  table(table(fatal_enc$joined$GEOID)) |> sum()

# This also works
nrow(all_tracts$income_population_quintiles_2020[all_tracts$income_population_quintiles_2020$luof_boolean == FALSE,])

# [1] 76569

summary(as.numeric(table(fatal_enc$joined$GEOID)))

var(as.numeric(table(fatal_enc$joined$GEOID)))



logit_income_only <- all_tracts$income_population_quintiles_2020 |> 
  glm(formula = luof_boolean ~ Income_1k, data = _, family = 'binomial')

# reshape data

income_population_longer <-
  all_tracts$income_population_quintiles_2020 |>
  select(GEOID,
         Income_1k,
         NH_BlackP,
         NH_WhiteP,
         Hisp_LatinoP,
         luof_boolean) |>
  pivot_longer(
    cols = c(NH_BlackP, NH_WhiteP, Hisp_LatinoP),
    names_to = 'Race',
    values_to = 'Proportion'
  ) |>
  mutate(Race = factor(Race, labels = c('NH_WhiteP', 'Hisp_LatinoP', 'NH_BlackP')))

logit_race <- income_population_longer |>
  glm(formula = luof_boolean ~ Race * Proportion,
      data = _,
      family = 'binomial')



# Open a PNG graphics device for each plot and save it to a separate file
# png("plot1.png", width = 10.4, height = 6.08, units = 'in', res = 320)
# plot(logit_income_only, which = 1)
# dev.off()
# 
# png("plot2.png", width = 10.4, height = 6.08, units = 'in', res = 320)
# plot(logit_income_only, which = 2)
# dev.off()
# 
# png("plot3.png", width = 10.4, height = 6.08, units = 'in', res = 320)
# plot(logit_income_only, which = 3)
# dev.off()
# 
# png("plot4.png", width = 10.4, height = 6.08, units = 'in', res = 320)
# plot(logit_income_only, which = 4)
# dev.off()
# 
# png("plot5.png", width = 10.4, height = 6.08, units = 'in', res = 320)
# plot(logit_income_only, which = 5)
# dev.off()
# 
# png("plot6.png", width = 10.4, height = 6.08, units = 'in', res = 320)
# plot(logit_income_only, which = 6)
# dev.off()


################################# #
# model fit with 100 quantiles ####
################################# #


num_bins <- 100

income_percentile_observed_proportions <- 
  aggregate(luof_boolean ~ income_bins_100, data = all_tracts$income_population_quintiles_2020 |> mutate(income_bins_100 = as.numeric(income_bins_100)), FUN = mean) |> rename(proportion_observed = luof_boolean)

medians_of_income_percentiles <- 
  data.frame(
    income_bins_100 = 1:num_bins,
    Income_1k = quantile(
      all_tracts$income_population_quintiles_2020$IncomeE,
      probs = seq(0.005, 0.995, by = 1/num_bins), na.rm = TRUE) / 1e3)

observed_prop_by_inc_percent <- left_join(
  x = medians_of_income_percentiles,
  y = income_percentile_observed_proportions
) |> select(-income_bins_100)

pred_df <- data.frame(Income_1k = c(seq(6, 14, by = 4), observed_prop_by_inc_percent$Income_1k, 220))

# Make predictions using the fitted model
pred_df$Predictions <- predict(logit_income_only, newdata = pred_df['Income_1k'], type = "response")

observed_prop_by_inc_percent$Predictions <- predict(logit_income_only, newdata = observed_prop_by_inc_percent['Income_1k'], type = "response")

# residuals
observed_prop_by_inc_percent$residuals <- observed_prop_by_inc_percent$Predictions - observed_prop_by_inc_percent$proportion_observed

png("logit_income_residuals_v_fitted.png", width = 10.4, height = 6.08, units = 'in', res = 320)

plot(
  x = observed_prop_by_inc_percent$Predictions,
  y = observed_prop_by_inc_percent$residuals,
  pch = 19,
  main = 'Residuals vs Fitted',
  ylab = 'Residuals',
  xlab = 'Predicted values',
  las = 1
); abline(h = 0, col = 'red', lwd = 2)

mtext("Each dot represents an income percentile", side = 3, font = 1)
dev.off()

png("logit_income_qqnorm.png", width = 10.4, height = 6.08, units = 'in', res = 320)

qqnorm(observed_prop_by_inc_percent$residuals, pch = 19,
       main = 'Q-Q Residuals', ylab = 'Observed Quantiles',
       las = 1)
qqline(observed_prop_by_inc_percent$residuals, col = 'red', lwd = 2)
dev.off()


################## #
# Logistic plot ####
################## #

png("logit_income_model_loess.png", width = 10.4, height = 6.08, units = 'in', res = 320)

plot(
  pred_df$Income_1k,
  pred_df$Predictions,
  type = 'l',
  ylab = expression("Pr("*LUOF ~ "|" ~ IncomePercentile[i]*")"),
  xlab = "Household Income (USD; thousands)",
  xlim = c(0, 215),
  ylim = c(0, 0.15),
  xaxt = 'n',
  yaxt = 'n',
  col = '#1a85ff',
  lwd = 2
)

title(main = "Predicted Probability vs. Observed Proportion")
mtext("Each dot represents an income percentile", side = 3, font = 1) # line = 1)
axis(1, at = seq(0, 220, by = 20), las = 1)
axis(2, at = seq(0, 0.15, by = 0.03), las = 1)


# Assuming you have loaded your data and plotted the points as mentioned
with(observed_prop_by_inc_percent, {
  # Plot the points
  points(Income_1k, proportion_observed, pch = 19)

  # Fit a loess line
  loess_fit <- loess(proportion_observed ~ Income_1k, data = observed_prop_by_inc_percent)

  # Predict values from the loess fit for smoother curve
  loess_predicted <- predict(loess_fit, newdata = data.frame(Income_1k = sort(Income_1k)))

  # Plot the loess line
  lines(sort(Income_1k), loess_predicted, col = "red", lwd = 2)
})

legend('topright', c('Logit', 'LOESS'),
  col = c('#1a85ff', 'red'),
  lwd = 2
)
dev.off()
################################# #
# try with 200 quantiles ####
################################# #
# num_bins <- 200
# 
# income_percentile_observed_proportions_200 <- 
#   aggregate(luof_boolean ~ income_bins_200, data = all_tracts$income_population_quintiles_2020 |> mutate(income_bins_200 = as.numeric(income_bins_200)), FUN = mean) |> rename(proportion_observed = luof_boolean)
# 
# 
# medians_of_income_percentiles_200 <- 
#   data.frame(
#     income_bins_200 = 1:num_bins,
#     Income_1k = quantile(
#       all_tracts$income_population_quintiles_2020$IncomeE,
#       probs = seq(1/200/2, 1 - 1/200/2, by = 1/num_bins), na.rm = TRUE) / 1e3)
# 
# observed_prop_by_inc_percent_200 <- left_join(
#   x = medians_of_income_percentiles_200,
#   y = income_percentile_observed_proportions_200
# ) |> select(-income_bins_200)
# 
# pred_df_200 <- data.frame(Income_1k = c(seq(6, 14, by = 4), observed_prop_by_inc_percent_200$Income_1k, 220))
# 
# # Make predictions using the fitted model
# pred_df_200$Predictions <- predict(logit_income_only, newdata = pred_df_200['Income_1k'], type = "response")
# 
# observed_prop_by_inc_percent_200$Predictions <- predict(logit_income_only, newdata = observed_prop_by_inc_percent_200['Income_1k'], type = "response")
# 
# # residuals
# observed_prop_by_inc_percent_200$residuals <- observed_prop_by_inc_percent_200$Predictions - observed_prop_by_inc_percent_200$proportion_observed
# 
# plot(
#   x = observed_prop_by_inc_percent_200$Predictions,
#   y = observed_prop_by_inc_percent_200$residuals,
#   pch = 19,
#   main = 'Residuals vs Fitted',
#   ylab = 'Residuals',
#   xlab = 'Predicted probability of a LUOF by income percentiles'
# ); abline(h = 0, col = 'red', lwd = 2)
# 
# qqnorm(observed_prop_by_inc_percent_200$residuals, pch = 19)
# qqline(observed_prop_by_inc_percent_200$residuals, col = 'red', lwd = 2)
# 
# plot(
#   pred_df_200$Income_1k,
#   pred_df_200$Predictions,
#   type = 'l',
#   ylab = 'Probability of a LUOF',
#   xlab = "Household Income (USD; thousands)",
#   xlim = c(0, 215),
#   ylim = c(0, 0.15),
#   xaxt = 'n',
#   yaxt = 'n'
# )
# title(main = "Predicted Probability vs. Observed Proportion")
# mtext("Each dot represents an income percentile", side = 3, font = 1) # line = 1)
# axis(1, at = seq(0, 220, by = 20), las = 1)
# axis(2, at = seq(0, 0.15, by = 0.03), las = 1)
# 
# # Assuming you have loaded your data and plotted the points as mentioned
# with(observed_prop_by_inc_percent_200, {
#   # Plot the points
#   points(Income_1k, proportion_observed, pch = 19)
#   
#   # Fit a loess line
#   loess_fit <- loess(proportion_observed ~ Income_1k, data = observed_prop_by_inc_percent_200)
#   
#   # Predict values from the loess fit for smoother curve
#   loess_predicted <- predict(loess_fit, newdata = data.frame(Income_1k = sort(Income_1k)))
#   
#   # Plot the loess line
#   lines(sort(Income_1k), loess_predicted, col = "red")
# })

# hist(all_tracts$income_population_quintiles_2020$IncomeE[all_tracts$income_population_quintiles_2020$income_bins_100 == 1])


############################# #
# Other stuff ####
############################# #
# 
# library(car)
# 
# # Create partial residual plots
# 
# car::crPlot(logit_income_only, variable = 'Income_1k')
# 
# # Create functional form plots
# 
# library(effects)
# plot(allEffects(logit_income_only))





# plotting fitted values against income


all_tracts$income_population_quintiles_2020$fitted_values <- 
  log(logit_income_only$fitted.values / (1 - logit_income_only$fitted.values))

with(all_tracts$income_population_quintiles_2020[sample(1:82907, 500),],
  plot(
    x = Income_1k,
    y = fitted_values,
    type = 'l'
  )
)
lm_1 <- lm(fitted_values ~ Income_1k, 
           data = all_tracts$income_population_quintiles_2020)
abline(lm_1, col = 'red')
           