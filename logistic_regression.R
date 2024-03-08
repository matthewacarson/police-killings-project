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



logit_1 <- all_tracts$income_population_quintiles_2020 |> 
  glm(formula = luof_boolean ~ Income_10k, data = _, family = 'binomial')

pred_df <- data.frame(Income_10k = seq(1, 20, .1))

# Make predictions using the fitted model
pred_df$Predictions <- predict(logit_1, newdata = pred_df['Income_10k'], type = "response")

# Plot the predicted probabilities against NH_BlackP values
# with(pred_df, plot(Income_10k, Predictions, type =  'l'))



library(car)

# Create partial residual plots
crPlot(logit_1, variable = 'Income_10k')

# Create functional form plots
library(effects)
plot(allEffects(model))


# Extract predicted log odds
all_tracts$income_population_quintiles_2020$predicted_log_odds <- predict(logit_1, type = "link")

# Plot predicted log odds against each independent variable
plot(all_tracts$income_population_quintiles_2020$Income_10k, predicted_log_odds, 
     xlab = "Independent Variable 1", ylab = "Predicted Log Odds",
     main = "Predicted Log Odds vs. Independent Variable 1",
     type = 'l')

ggplot(data = all_tracts$income_population_quintiles_2020,
       aes(x = Income_10k, y = predicted_log_odds)) +
  # ggplot2::geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)

