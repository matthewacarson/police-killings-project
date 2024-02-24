# count number of killings in tracts
source('summary_tables.R')
LUOF_binary <- all_tracts$income_population_quintiles_2020
LUOF_binary$LUOF_binary <- all_tracts$income_population_quintiles_2020$GEOID %in% fatal_enc$joined$GEOID


  


# Logistic regression

logit_1 <- 
  glm(
    formula = LUOF_binary ~ IncomeE,
    data = LUOF_binary,
    family = 'binomial'
  )


# Generate a sequence of values from 0.01 through 1
nh_values <- seq(10000, 250000, 10000)

# Create a dataframe with NH_BlackP values
nh_df <- data.frame(IncomeE = nh_values)

# Make predictions using the fitted model
predictions <- predict(logit_1, newdata = nh_df, type = "response")

# Plot the predicted probabilities against NH_BlackP values
plot(nh_values, predictions, 
     # ylim = c(0, 1),
     type = "l", 
     xlab = "NH_BlackP", 
     ylab = "Predicted Probability", 
     main = "Predicted Probability vs NH_BlackP")


logit_2 <- 
  glm(
    formula = as.logical(LUOFs) ~ IncomeE,
    data = LUOF_counts,
    family = 'binomial'
  )


# Generate a sequence of values from 0.01 through 1
nh_values <- seq(1000, 250000, by = 1000)

# Create a dataframe with NH_BlackP values
nh_df <- data.frame(IncomeE = nh_values)

# Make predictions using the fitted model
predictions <- predict(logit_2, newdata = nh_df, type = "response")

# Plot the predicted probabilities against NH_BlackP values
plot(nh_values, predictions, 
     # ylim = c(0, 1),
     xlim = c(2500, 25000),
     type = "l", 
     xlab = "IncomeE", 
     ylab = "Predicted Probability", 
     main = "Predicted Probability vs IncomeE")


logit_3 <- 
  glm(
    formula = as.logical(LUOFs) ~ IncomeE + NH_BlackP,
    data = LUOF_counts,
    family = 'binomial'
  )

