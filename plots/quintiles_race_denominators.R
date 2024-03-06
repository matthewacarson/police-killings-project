# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = 'summary_tables.R')

##################### #
# GGplot by race ####
##################### #

# save to CSV
race_income_race_denom_out <- 
  summary_tables$race_income_summary_race_denom |> 
  select(-Population) |> 
  mutate(out_value = paste(Killings, round(Annualized_Per_10_M, 2), sep = " | "))

race_income_race_denom_out |> 
  pivot_wider(id_cols = Quintile,
              names_from = Majority,
              values_from = out_value) |> 
  write_csv(file = "race_income_race_denom_out.csv")

ggplot(
    summary_tables$race_income_summary_race_denom
    ,aes(x = Majority, y = Annualized_Per_10_M, fill = Quintile)) +
  geom_hline(
    yintercept = seq(0,100, by = 12.5), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5
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
       x = "Majority (> 50%)",# "Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Median Household\nIncome in\nCensus Tract") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()
  )
ggsave(
  'plots/quintiles_race_denominators/race_only_denom_race.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
# ############################### #
# Grouped by Income Quintile ####
# ############################### #
ggplot(
  summary_tables$race_income_summary_race_denom, 
    aes(x = Income, y = Annualized_Per_10_M, fill = Majority)) +
  geom_hline(
    yintercept = seq(0,100, by = 10), 
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
  'plots/quintiles_race_denominators/race_only_denom_quint.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
