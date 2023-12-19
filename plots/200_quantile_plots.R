# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = "summay_tables.R")

############################################## #
## geom_point plot - 200 quantiles ##########
############################################## #

ggplot(
  summary_tables$bins200_summary_1, 
  aes(x = Income, y = Annualized_Per_10_M)) +
  geom_point() +  
  geom_smooth(
    method = "loess", formula = y ~ x, color = "blue", se = TRUE) + 
  labs(
    x = "Median Household Income in Census Tracts\n200 Quantiles", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + scale_x_continuous(breaks = seq(0, 200 , by = 10)) + 
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  theme_light() +
  theme(
    axis.text.x = element_text(color = 'black', vjust = 0.5),
    axis.text.y = element_text(color = 'black')
  )
ggsave(
  filename = 'plots/200_quantile_plots/all_200.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)

############################################## #
## geom_bar plot - 200 quantiles #############
############################################## #

ggplot(
  summary_tables$bins200_summary_1, 
  aes(x = Income, y = Annualized_Per_10_M)) +
  geom_bar(
    stat = 'identity', fill = 'lightblue3', width = 1, color = 'black') +
  geom_smooth(
    method = "loess", formula = y ~ x, color = "blue", se = TRUE) +
  labs(
    x = "Median Household Income in Census Tracts", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + theme_light() +
  theme(
    axis.text.x = element_blank()
  )

ggsave(
  filename = 'plots/200_quantile_plots/bar_200_all.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)
