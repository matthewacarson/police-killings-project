# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
################################################ #
# Begin Summary Tables  ########################
################################################ #



# ############################## #
# Grouped by Race/Ethnicity ####
# ############################## #

# table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles) |> 
  rename(Killings = n)

summary_tables$race_and_income_pop <- 
  all_tracts$income_population_quintiles_2020 |> 
  aggregate(Total_popE ~ Majority + income_quintiles, FUN = sum) |> 
  rename(Population = Total_popE)

summary_tables$race_and_income_summary <- 
  left_join(
    x = summary_tables$race_and_income,
    y = summary_tables$race_and_income_pop,
    by = c("Majority", "income_quintiles")
  ) |> 
  mutate(
    Annualized_Per_10_M =
      Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Income = income_quintiles,
    Population,
    Killings,
    Annualized_Per_10_M
  ) |> add_row(
    summary_tables$summary_1 |>
      select(-Killings_Per_Yr)
  ) |> na.omit() |> 
  mutate(
    Income = factor(Income, ordered = TRUE),
    Majority = factor(Majority, ordered = TRUE))

# plots$quintile_by_race
ggplot(
  summary_tables$race_and_income_summary
  ,aes(x = Majority, y = Annualized_Per_10_M, fill = Income)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5,
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    width = 0.75
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Majority (> 50%)",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Median Household\nIncome in\nCensus Tract") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()
  )
ggsave(
  'plots/by_quintile/original_quintile_denominators_by_race.png', 
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
# ggsave(
#   plot = plots$quintile_by_race,
#   filename = 'plots/quintile_by_race.png', 
#   dpi = 'retina', 
#   bg = 'white',
#   width = 10.4,
#   height = 4.81)
# ############################### #
# Grouped by Income Quintile ####
# ############################### #
# plots$race_by_quintile <- 
ggplot(
  summary_tables$race_and_income_summary, 
  aes(x = Income, y = Annualized_Per_10_M, fill = Majority)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    # linewidth = 0.01,
    width = 0.75
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Median Household Income in Census Tract",
       fill = "Majority\n(> 50%)") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
    # panel.grid.major.x = element_blank()
  )
ggsave(
  'plots/by_quintile/original_quintile_denominators_by_quintile.png', 
  dpi = 'retina',
  width = 10.4,
  height = 4.81)

# ggsave(
#   plot = plots$race_by_quintile,
#   filename = 'plots/race_by_quintile.png', 
#   dpi = 'retina', 
#   bg = 'white',
#   width = 10.4,
#   height = 4.81)

# table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

####################################### #
# These plots use different colors. 
# They are the original plots I made.
# The difference is that I used total 
# population of that tract rather than
# total population of each particular race.
####################################### #


ggplot(
    summary_tables$race_and_income_summary
    ,aes(x = Majority, y = Annualized_Per_10_M, fill = Income)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5,
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    width = 0.75
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Majority (> 50%)",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Median Household\nIncome in\nCensus Tract") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()
  )

ggsave(
  # plot = plot$quintile_by_race,
  filename = 'plots/by_quintile/quintile_by_race.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)
# ############################### #
# Grouped by Income Quintile ####
# ############################### #
ggplot(
    summary_tables$race_and_income_summary, 
    aes(x = Income, y = Annualized_Per_10_M, fill = Majority)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    # linewidth = 0.01,
    width = 0.75
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Median Household Income in Census Tract",
       fill = "Majority\n(> 50%)") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
    # panel.grid.major.x = element_blank()
  )

ggsave(
  # plot = plot$race_by_quintile,
  filename = 'plots/by_quintile/race_by_quintile.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)