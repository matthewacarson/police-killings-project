# #################################
# Grouped by Race/Ethnicity
# #################################
library(grid)
library(ggplot2)
# fatal_enc_plots <- list()
fatal_enc_plots$by_race <- ggplot(
  summary_tables$race_and_quintiles_incl_overall_rate %>%
    filter(Majority_Race != "Asian")
  # summary_tables$race_and_quintiles, # excludes "All Groups" category 
  ,aes(x = Majority_Race, y = Per10MillionPopPerYr, fill = Quintile)) +
  geom_hline(yintercept = seq(0,70, by = 10), color = "gray", linetype = "dashed", linewidth = 0.5) +
  geom_bar(
    stat = "identity", position = "dodge", color = 'black', linewidth = 0.01) +
  geom_text(aes(label = round(Per10MillionPopPerYr, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, color = "black") +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Majority (> 50%)",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Median Household\nIncome in\nCensus Tract") +
  theme_minimal() + theme(axis.text.x = element_text(color = "black"))
# ,plot.margin = margin(10, 10, 20, 10, "mm"))  # Add extra margin at the bottom for mtext

fatal_enc_plots$by_race

pushViewport(viewport(layout = grid.layout(1, 1)))
grid.text("race_quintiles_ggplot.R", 
          gp = gpar(fontsize = 5),
          vp = viewport(
            x = 1.02, 
            y = -0.035, 
            width = 0.1, 
            height = 0.1, 
            just = c("right", "bottom")))

# #################################
# Grouped by Income Quintile
# #################################
fatal_enc_plots$by_quintile <- ggplot(
  summary_tables$race_and_quintiles_incl_overall_rate %>% 
    filter(Majority_Race != "Asian")
  # summary_tables$race_and_quintiles, # excludes "All Groups" category
  ,aes(x = Quintile, y = Per10MillionPopPerYr, fill = Majority_Race)) +
  geom_hline(yintercept = seq(0,70, by = 10), color = "gray", linetype = "dashed", linewidth = 0.5) +
  geom_bar(
    stat = "identity", position = "dodge", color = 'black', linewidth = 0.01
  ) +
  geom_text(aes(label = round(Per10MillionPopPerYr, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, color = "black") +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Median Household Income in Census Tract",
       fill = "Majority\n(> 50%)") +
  theme_minimal() + theme(axis.text.x = element_text(color = "black"))

fatal_enc_plots$by_quintile

pushViewport(viewport(layout = grid.layout(1, 1)))
grid.text("race_quintiles_ggplot.R", 
          gp = gpar(fontsize = 5),
          vp = viewport(
            x = 1.02, 
            y = -0.035, 
            width = 0.1, 
            height = 0.1, 
            just = c("right", "bottom")))
