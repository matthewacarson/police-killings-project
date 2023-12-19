# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = "summary_tables.R")

############################## #
## Create quintile bar plot ####
############################## #
# This needs to be assigned to an object for cowplot
income_quintiles_only <-
  summary_tables$summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Income)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "Annual Rate Per 10 Million Population", 
    y = "Tract Median Household Income Quintiles"
    # title = "Police Lethal Uses of Force",
    # subtitle = "Years: [2015-2020]"
  ) + 
  # coord_flip() +
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.5, color = "black"
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(angle = 90, hjust = 0.5, color = 'black')
  ) +
  scale_y_discrete(
    labels = c(
      "1st", 
      "2nd", 
      "3rd", 
      "4th", 
      "5th"
    )
  ) + scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70))
# axis.title = element_text(size = 20),
# plot.title = element_text(size = 30),
# plot.subtitle = element_text(size = 20)


####################### #
# Majority Race Plot ####
####################### #

majority_race_only <- summary_tables$majority_summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Majority)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "",
    # x = "Annual Rate Per 10 Million Population", 
    y = "Tracts With Majority One Race (>50%)", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.5, color = "black"
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(angle = 90, hjust = 0.5, color = 'black')) +
  scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  scale_y_discrete(labels = c("Black", "Latino", "White"))
# coord_flip()
# scale_x_continuous(breaks = seq(0,70,10))

##################### #
## Combining plots ####
##################### #

# library(patchwork)
# plots$income_quintiles_only + plots$majority_race_only

library(cowplot)

plot_grid(
  majority_race_only, 
  income_quintiles_only, 
  # labels = c("A", "B"), 
  nrow = 2,
  rel_heights = c(1, 1.25))

ggsave(
  filename = 'plots/inc_quint_majority_race_same_plot/combined.png',
  dpi = 'retina',
  bg = 'white',
  width = 10.4,
  height = 4.81)

################################################################# #
# Alternatively, this produces the income quintiles only with the 
# title and other labels. 
################################################################# #

########################################### #
# Individual plot: Income Quintiles only ####
########################################### #

summary_tables$summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Income)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "Annual Rate Per 10 Million Population", 
    y = "Tract Median Household Income Quintiles", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  # coord_flip() +
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.3, color = "black", size = 5
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  scale_y_discrete(
    labels = c(
      "1st", 
      "2nd", 
      "3rd", 
      "4th", 
      "5th"
    )
  ) + scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70))
# plot.subtitle = element_text(size = 20)
# axis.title = element_text(size = 20),
# plot.title = element_text(size = 30),
ggsave(
  filename = 
    'plots/inc_quint_majority_race_same_plot/income_quintiles_only_ind.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)


#################################### #
# Majority Race Plot (individual) ####
#################################### #

summary_tables$majority_summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Majority)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "",
    # x = "Annual Rate Per 10 Million Population", 
    y = "Tracts With Majority One Race (>50%)", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.3, color = "black", size = 5
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  scale_y_discrete(labels = c("Black", "Latino", "White"))
# coord_flip()
# scale_x_continuous(breaks = seq(0,70,10))
# plots$majority_race_only

ggsave(
  filename = 
    'plots/inc_quint_majority_race_same_plot/majority_race_only_ind.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)



# filename = paste(
  # 'plots/inc_quint_majority_race_same_plot/quintiles_only_',
  # # format(Sys.time(), "%Y-%m-%d_%H-%M-%OS3"), '.png',
  # sep = '')