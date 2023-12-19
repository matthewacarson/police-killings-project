# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = "summary_tables.R")


############### #
# Plot ####
############### #
ggplot(data = summary_tables$race_quint_proportions,
       aes(x = Race, 
           y = Proportion, 
           fill = Quintile)) +
  # geom_hline(
  #   yintercept = seq(0,100, by = 12.5), 
  #   color = "gray", 
  #   linetype = "dashed", 
  #   linewidth = 0.5
  # ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85),
    color = 'black', 
    # linewidth = 0.01,
    width = 0.8
  ) + 
  geom_text(
    aes(
      label = 
        paste0(round(Proportion, 2) * 100, "%")),
    position = position_dodge(width = 0.85),
    vjust = -0.4, color = "black", size = 3.5) +
  labs(
    title = "Income Quintile Distribution of Racial or Ethnic Groups",
    subtitle = "Proportion of Each Racial or Ethnic Group Living in Each Census Tract Income Quintile (2020)",
    x = "Racial or Ethnic Group",
    y = "Percentage of Group"
  ) +
  theme_light() +
  # scale_y_continuous(
    # breaks = seq(0,0.35,0.1), 
    # labels = function(x) paste0(x * 100, '%')) + #, name = "Percentage") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"))
# scale_color_brewer(palette = 'Dark3')

ggsave(
  filename = "plots/total_pop_quintiles_race.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
