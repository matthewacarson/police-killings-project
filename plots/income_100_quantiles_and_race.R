# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = "summary_tables.R")

# There are some issues with this plot.
# Since there are such small underlying population numbers when dividing into 
# percentiles, the rates sometimes are dramatically high. It is possible that 


ggplot(summary_tables$decile_race_denom, aes(x = as.numeric(Income), y = Annualized_Per_10_M, color = Majority)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 1) + 
  labs(
    x = "Median Household Income in Census Tracts (Deciles)", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + 
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  theme_light() +
  scale_x_continuous(breaks = 0:10) +
  theme(
    axis.text.x = element_text(color = 'black', vjust = 0.5),
    axis.text.y = element_text(color = 'black')
  )

ggsave(
  'plots/income_deciles_and_race/income_deciles_and_race.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
