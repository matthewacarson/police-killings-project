# count number of killings in tracts
source('summary_tables.R')
LUOF_counts <-
  left_join(
    x = all_tracts$income_population_quintiles_2020,
    y = fatal_enc$joined |>
      count(GEOID)) |> 
  rename(LUOFs = n) |> 
  mutate(LUOFs = case_when(is.na(LUOFs) ~ 0, TRUE ~ LUOFs))
      # arrange(desc(n)))
  # mutate()



# Logistic regression

logit_1 <- 
  glm(
    formula = as.logical(LUOFs) ~ IncomeE,
    data = LUOF_counts,
    family = 'binomial'
  )


# Generate a sequence of values from 0.01 through 1
nh_values <- seq(0.01, 1, by = 0.01)

# Create a dataframe with NH_BlackP values
nh_df <- data.frame(NH_BlackP = nh_values)

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

