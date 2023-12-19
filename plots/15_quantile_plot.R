# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = "summary_tables.R")
################################################ #
# Begin Summary Tables  ########################
################################################ #

# ############################## #
# Grouped by Race/Ethnicity ####
# ############################## #

# Note: this script uses 15 tract income quantiles.
# The reasons for choosing 15 quantiles is that it lets
# us divide quintiles into tertiles of each quintile. 
# So by looking at the three lowest of 15 quantiles, 
# we are looking at the tertiles of the lowest quintile.

# plot$quant15_by_race
  ggplot(
    summary_tables$quant15_summary |> filter(Income <= 3)
    ,aes(x = Majority, y = Annualized_Per_10_M, fill = Income)) +
  geom_hline(
    yintercept = seq(0,110, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5,
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    width = 0.82
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population (Annualized)",
       x = "Majority (> 50%)",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Tertiles of the\nLowest Quintile") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()
  )
ggsave(filename = "plots/15_quantile_plot/15_quantile_plot_by_race.png", dpi = 'retina', width = 10.4, height = 4.81)

################################################# #
# By Income
################################################# #

ggplot(
  summary_tables$quant15_summary |> filter(Income <= 3),
  aes(x = Income, y = Annualized_Per_10_M, fill = Majority)) +
  geom_hline(
    yintercept = seq(0,110, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5,
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    width = 0.82
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Lowest three tract income quantiles of 15 quantiles (tertiles of the lowest tract income quintile). Years: [2015-2020]",
       y = "Per 10 Million Population (Annualized)",
       x = "Tertiles of the Lowest Tract Income Quintile",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Racial or\nEthnic Group") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank()) 

ggsave(filename = "plots/15_quantile_plot/15_quantile_plot_by_tert.png", 
       dpi = 'retina',   
       width = 10.4,
       height = 4.81)

