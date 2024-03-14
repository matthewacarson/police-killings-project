# count number of killings in tracts
source('summary_tables.R')
# Distribution of counts at the tract level

table(table(fatal_enc$joined$GEOID))

# 1     2     3     4    5    9 
# 5796  479   54    6    2    1 
table(table(fatal_enc$joined$GEOID)) |> sum()

# [1] 6338

nrow(
  all_tracts$income_population_quintiles_2020) - 
  table(table(fatal_enc$joined$GEOID)) |> sum()

# [1] 76569

summary(as.numeric(table(fatal_enc$joined$GEOID)))

sd(as.numeric(table(fatal_enc$joined$GEOID)))


all_tracts$income_population_quintiles_2020 <- 
all_tracts$income_population_quintiles_2020 |> 
  mutate(Income_1k = IncomeE / 1e3)

logit_1 <- all_tracts$income_population_quintiles_2020 |> 
  glm(formula = luof_boolean ~ Income_1k, data = _, family = 'binomial')

# plot(logit_1, which = 1)
# plot(logit_1, which = 2)
# plot(logit_1, which = 3)
# plot(logit_1, which = 4)
# plot(logit_1, which = 5)
# plot(logit_1, which = 6)


income_percentile_observed_proportions <- 
table(all_tracts$income_population_quintiles_2020$luof_boolean, all_tracts$income_population_quintiles_2020$income_bins_100)[2,] / table(all_tracts$income_population_quintiles_2020$income_bins_100)


income_percentile_observed_proportions <- data.frame(
    income_bins_100 = 1:100,
    proportion_observed = as.numeric(income_percentile_observed_proportions))



medians_of_income_percentiles <- 
  data.frame(
    income_bins_100 = 1:100,
    Income_1k = quantile(
      all_tracts$income_population_quintiles_2020$IncomeE,
      probs = seq(0.005, 0.995, by = 1/100), na.rm = TRUE) / 1e3)

observed_prop_by_inc_percent <- left_join(
  x = medians_of_income_percentiles,
  y = income_percentile_observed_proportions
) |> select(-income_bins_100)

pred_df <- data.frame(Income_1k = c(seq(6, 14, by = 4), observed_prop_by_inc_percent$Income_1k, 220))

# Make predictions using the fitted model
pred_df$Predictions <- predict(logit_1, newdata = pred_df['Income_1k'], type = "response")

observed_prop_by_inc_percent$Predictions <- predict(logit_1, newdata = observed_prop_by_inc_percent['Income_1k'], type = "response")

# residuals
observed_prop_by_inc_percent$residuals <- observed_prop_by_inc_percent$Predictions - observed_prop_by_inc_percent$proportion_observed

plot(
  x = observed_prop_by_inc_percent$Predictions,
  y = observed_prop_by_inc_percent$residuals 
); abline(h = 0)

qqnorm(observed_prop_by_inc_percent$residuals, pch = 19)
qqline(observed_prop_by_inc_percent$residuals, col = 'red')



plot(
  pred_df$Income_1k,
  pred_df$Predictions,
  type = 'l',
  ylab = 'Probability of a LUOF',
  xlab = "Household Income (USD; thousands)",
  xlim = c(0, 215),
  ylim = c(0, 0.15),
  xaxt = 'n',
  yaxt = 'n'
)
with(observed_prop_by_inc_percent,
     points(Income_1k, proportion_observed, pch = 19))
title(main = "Predicted Probability vs. Observed Proportion")
mtext("Each dot represents an income percentile", side = 3, font = 1) # line = 1)
axis(1, at = seq(0, 220, by = 20), las = 1)
axis(2, at = seq(0, 0.15, by = 0.03), las = 1)




# hist(all_tracts$income_population_quintiles_2020$IncomeE[all_tracts$income_population_quintiles_2020$income_bins_100 == 1])


# library(car)

# Create partial residual plots
car::crPlot(logit_1, variable = 'Income_1k', )

# Create functional form plots
library(effects)
plot(allEffects(model))


# Extract predicted log odds
all_tracts$income_population_quintiles_2020$predicted_log_odds <- predict(logit_1, type = "link")

# Plot predicted log odds against each independent variable
plot(all_tracts$income_population_quintiles_2020$Income_1k, predicted_log_odds, 
     xlab = "Independent Variable 1", ylab = "Predicted Log Odds",
     main = "Predicted Log Odds vs. Independent Variable 1",
     type = 'l')

ggplot(data = all_tracts$income_population_quintiles_2020,
       aes(x = Income_1k, y = predicted_log_odds)) +
  # ggplot2::geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

