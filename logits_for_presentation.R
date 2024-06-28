fatal_enc_stata <- read.csv("C:/Users/madou/OneDrive - UCLA IT Services/1)_PS-Honors/HP_PC/police_killings_github_HP/stata/dta/logit_10k.csv")


logit_fatal_enc_income_only <- 
  fatal_enc_sata |> 
  glm(
    family = binomial,
    data = _,
    fatal_enc_binary ~ IncomeE_10k
  )

fatal_enc_sata |> 
  ggplot(data = _, aes(x = IncomeE_10k, y = fatal_enc_binary)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + theme_minimal()



# Example: Logistic Regression in R
model_NH_BlackP <- glm(fatal_enc_binary ~ NH_BlackP, data = fatal_enc_sata, family = binomial)
model_NH_WhiteP <- glm(fatal_enc_binary ~ NH_WhiteP, data = fatal_enc_sata, family = binomial)
model_Hisp_LatinoP <- glm(fatal_enc_binary ~ Hisp_LatinoP, data = fatal_enc_sata, family = binomial)
# Define values of NH_BlackP for which to compute margins
NH_BlackP_values <- seq(0, 1, by = 0.05)

# Compute predicted probabilities (margins)
predicted_probs <- predict(model, newdata = data.frame(NH_BlackP = NH_BlackP_values), type = "response")

# Combine NH_BlackP values and predicted probabilities into a data frame
margins_data <- data.frame(NH_BlackP = NH_BlackP_values, Predicted_Probability = predicted_probs)


library(ggplot2)

# Plot using ggplot2
ggplot(margins_data, aes(x = NH_BlackP, y = Predicted_Probability)) +
  geom_line(color = "black", size = 1) +
  labs(
    title = "Predicted Probability of a LUOF, 2015-2020",
    x = "Proportion black in the census tract",
    y = "Probability of a LUOF"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 0.18), breaks = seq(0, 0.18, by = 0.02)) +
  theme_minimal()

# ###################### #
# DOing them all at once
# ###################### #

# Load necessary libraries
library(ggplot2)

# Assuming you have fitted logistic regression models and stored them as `model_NH_BlackP`, `model_NH_WhiteP`, `model_Hisp_LatinoP`

# Define sequence of values for NH_BlackP, NH_WhiteP, and Hisp_LatinoP
NH_BlackP_values <- seq(0, 1, by = 0.05)
NH_WhiteP_values <- seq(0, 1, by = 0.05)
Hisp_LatinoP_values <- seq(0, 1, by = 0.05)

# Function to compute predicted probabilities
compute_predicted_probs <- function(model, values) {
  predicted_probs <- predict(model, newdata = data.frame(Hisp_LatinoP = values), type = "response")
  return(predicted_probs)
}

# Compute predicted probabilities for each predictor
predicted_probs_NH_BlackP <- compute_predicted_probs(model_NH_BlackP, NH_BlackP_values)
predicted_probs_NH_WhiteP <- compute_predicted_probs(model_NH_WhiteP, NH_WhiteP_values)
predicted_probs_Hisp_LatinoP <- compute_predicted_probs(model_Hisp_LatinoP, Hisp_LatinoP_values)

# Create data frames for each predictor
margins_data_NH_BlackP <- data.frame(Proportion = NH_BlackP_values, Predicted_Probability = predicted_probs_NH_BlackP, Group = "Black")
margins_data_NH_WhiteP <- data.frame(Proportion = NH_WhiteP_values, Predicted_Probability = predicted_probs_NH_WhiteP, Group = "White")
margins_data_Hisp_LatinoP <- data.frame(Proportion = Hisp_LatinoP_values, Predicted_Probability = predicted_probs_Hisp_LatinoP, Group = "Latino")

# Combine data frames
margins_data <- rbind(margins_data_NH_BlackP, margins_data_NH_WhiteP, margins_data_Hisp_LatinoP)

# Plot using ggplot2
ggplot(margins_data, aes(x = Proportion, y = Predicted_Probability, linetype = Group, color = Group)) +
  geom_line(size = 1) +
  labs(
    title = "Predicted Probability of a LUOF, 2015-2020",
    x = "Proportion in the census tract",
    y = "Probability of a LUOF"
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 0.18), breaks = seq(0, 0.18, by = 0.02)) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom')

ggsave(
  filename = "C:/Users/madou/1_GitHub_Repositories/ps_thesis_latex/ur_week_slides/images/race_logit.png", 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4*.7,
  height = 4.81)



# Fit logistic regression models with interaction terms
model_black <- glm(fatal_enc_binary ~ IncomeE_10k * NH_BlackP, data = fatal_enc_stata, family = binomial)
model_white <- glm(fatal_enc_binary ~ IncomeE_10k * NH_WhiteP, data = fatal_enc_stata, family = binomial)
model_latino <- glm(fatal_enc_binary ~ IncomeE_10k * Hisp_LatinoP, data = fatal_enc_stata, family = binomial)


# Calculate marginal effects using the effects package
eff_black <- effect("IncomeE_10k:NH_BlackP", model_black, xlevels = list(NH_BlackP = seq(0, 1, by = 0.05), IncomeE_10k = 1))
eff_white <- effect("IncomeE_10k:NH_WhiteP", model_white, xlevels = list(NH_WhiteP = seq(0, 1, by = 0.05), IncomeE_10k = 1))
eff_latino <- effect("IncomeE_10k:Hisp_LatinoP", model_latino, xlevels = list(Hisp_LatinoP = seq(0, 1, by = 0.05), IncomeE_10k = 1))



# Convert effects to data frames
eff_black_df <- as.data.frame(eff_black)
eff_white_df <- as.data.frame(eff_white)
eff_latino_df <- as.data.frame(eff_latino)

# Add Group column to distinguish the effects
eff_black_df$Group <- "Black"
eff_white_df$Group <- "White"
eff_latino_df$Group <- "Latino"

# Combine all marginal effects into one data frame
margins_df <- bind_rows(eff_black_df, eff_white_df, eff_latino_df)


# Plot using ggplot2
ggplot(margins_df, aes(x = NH_BlackP, y = fit, linetype = Group)) +
  geom_line(size = 1) +
  labs(title = "Average Marginal Effects of Income",
       x = "Proportion in Census Tract",
       y = "Effects of $10k Income on Pr(LUOF)") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  facet_wrap(~ Group, scales = "free_x", labeller = labeller(Group = c(Black = "Proportion Black", White = "Proportion White", Latino = "Proportion Latino")))




# exponentiating
# 
#Black
round(exp(0.655957 * 0.1) - 1, 4) * 100

# White
round(exp(-0.926671 * 0.1) - 1, 4) * 100

# Latino
round(exp(1.015783 * 0.1) - 1, 4) * 100



# Not using these
# 


ggpredict(model_black, terms = c("IncomeE_10k [20:80, by=20]", "education [High school, Bachelor's degree]")) |> plot()


ggplot(fatal_enc_sata) + 
  geom_smooth(
    aes(x = NH_BlackP, y = fatal_enc_binary),
    method = "glm", method.args = list(family = "binomial"), se = FALSE, col = 'red') +
  geom_smooth(
    aes(x = Hisp_LatinoP, y = fatal_enc_binary),
    method = "glm", method.args = list(family = "binomial"), se = FALSE, col = 'green') +
  geom_smooth(
    aes(x = NH_WhiteP, y = fatal_enc_binary),
    method = "glm", method.args = list(family = "binomial"), se = FALSE, col = 'blue')
