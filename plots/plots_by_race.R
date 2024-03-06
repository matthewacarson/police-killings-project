# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "summary_tables.R")


# ################################### #
# Plots using race of the victim 
# ################################### #

ggplot(
  summary_tables$bin_table_race_proportion, 
  aes(x = Percentile, y = Proportion, color = race_imputed)
) +
  geom_point() +  
  geom_smooth(method = "loess", formula = y ~ x, se = F) + 
  labs(
    x = "Median Household Income in Census Tracts\n100 Quantiles", 
    y = "Proportion of Persons Killed Within Each Racial Group", 
    title = "Lethal Uses of Force",
    subtitle = "Distribution within racial groups",
    color = "Race of Victim"
  ) + scale_x_continuous(breaks = seq(0, 100 , by = 5)) + 
  theme_light() +
  scale_color_brewer(palette = "Dark2") +
  theme()

ggsave(
  filename = 'plots/plots_by_race/race_100_proportion.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)


################################################# #
# Another plot ####
################################################# #

## Race only plot ####
##
ggplot() +
  geom_bar(
    data = summary_tables$race_freq,
    aes(
      x = Race,
      y = Prop,
    ),
    stat = 'identity',
    width = 0.5,
    fill = 'lightblue',
    color = 'black'
  ) + 
  labs(
    x = "", 
    y = "Proportion of Total LUOFs",
    title = "Police Lethal Uses of Force",
    subtitle = "Race of Victim. Years: [2015-2020]"
  ) + 
  coord_flip() +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14),
    legend.key.size = unit(8, "mm"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    # plot.margin = unit(c(1, 1, 1, 0.5), "cm"),
    # aspect.ratio = 0.75/2.5
  ) + scale_y_continuous(breaks = seq(0, 0.5, 0.05))

ggsave(
  filename = 'plots/plots_by_race/race_proportion_of_total_bar.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)

#################################################### #
# Distribution by quintile within race ####
#################################################### #



ggplot(
  data = summary_tables$quiniles_race_victim,
  aes(x = Race, y = Prop, fill = Quintile)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.9), 
    color = 'black', 
    width = 0.8,
    linewidth = 0.5) +
  labs(title = "Police Lethal Uses of Force",
       subtitle = "The proportion of each racial group killed in each US Census median household income quintile.",
       y = "Proportion",
       x = "Race of Victim") +
  geom_text(aes(label = paste0(round(Prop, 2) * 100, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.4, color = "black", size = 3) +
  theme_light() + 
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5, color = 'black'),
    # axis.title.x = element_text(size = 15),
    # axis.title.y = element_text(size = 15),
    # plot.title = element_text(size = 18),
    # plot.subtitle = element_text(size = 14),
    # legend.key.size = unit(8, "mm"),
    # legend.title = element_text(size = 15),
    # legend.text = element_text(size = 12)
  ) + 
  guides(fill = guide_legend(title = "Median\nHousehold\nIncome"))

ggsave(
  filename = 'plots/plots_by_race/inc_and_race_victim.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)

################################### #
# Cross Tabulations Table ####
################################### #

summary_tables$quiniles_race_victim[1:20,] |> 
  mutate(Prop = round(Prop,digits = 3) * 100) |> 
  pivot_wider(names_from = Race, values_from = Prop) |> write_csv(file = "quintiles_race_victim.csv")



############################################ #
# LUOF and race population distribution ####
############################################ #


############################## #
# Line interaction plot ####
############################## #

## Using shapes ####
ggplot(
  summary_tables$IncRace_pop_and_IncRace_LUOF, 
  aes(
    x = Quintile, 
    y = Proportion, 
    color = Race, 
    shape = Type, 
    group = interaction(Race, Type))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  # scale_color_manual(
    # values = c(
      # "Black" = "red", 
      # "Hispanic/Latino" = "blue", 
      # "White" = "green2")) +
  scale_shape_manual(values = c("LUOFs" = 16, "Population" = 17)) +
  guides(
    color = guide_legend(override.aes = list(shape = NA))
  ) +
  labs(
    title = "Distributions Within Each Racial Group",
    subtitle = "Proportion of LUOFs in each quintile vs. proportion of the racial group living in that quintile",
    x = "Quintile",
    y = "Proportion",
    shape = "Proportion of",
    color = "Victim") +
  theme_light() +
  theme(legend.position = "right")

ggsave(
  filename = 'plots/plots_by_race/inc_and_race_victim_shape.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)

### Using line type ####
ggplot(
  summary_tables$population_vs_LUOF_proportions, 
  aes(
    x = Quintile, 
    y = Proportion, 
    color = Race, 
    linetype = Type, 
    group = interaction(Race, Type))) +
  # geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_shape_manual(values = c("LUOFs" = 16, "Population" = 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  guides(
    # color = guide_legend(override.aes = list(shape = NA)),
    linetype = guide_legend(
      title = "Proportion of", override.aes = list(size = 10)),
    color = guide_legend(
      # title = "Proportion of", override.aes = list(linetype = "solid", size = 10),
      title = "Victim", override.aes = list(linetype = "solid", size = 10, linewidth = 10)
  )) +
  # scale_linetype_manual(values = c("solid", "dashed"),
  #                       name = "Proportion of") + 
  labs(title = "Distributions of LUOFs vs. Distribution of General Population",
       subtitle = "Proportion of LUOFs in each income quintile by race vs. proportion of that race living in that income quintile",
       x = "Quintile",
       y = "Proportion",
       # shape = "Proportion of"
       ) +
  theme_light() +
  theme(legend.position = "right")

ggsave(
  filename = 'plots/plots_by_race/inc_and_race_victim_linetype.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)

# Just showing the differences


ggplot(
  summary_tables$prop_difference, 
  aes(
    x = Quintile, 
    y = Difference, 
    color = Race,
    # shape = Type, 
    group = Race
  )) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  # scale_color_manual(
  # values = c(
  # "Black" = "red", 
  # "Hispanic/Latino" = "blue", 
  # "White" = "green2")) +
  scale_shape_manual(values = c("LUOFs" = 16, "Population" = 17)) +
  # guides(
    # color = guide_legend(override.aes = list(shape = NA))
  # ) +
  labs(
    title = "Distributions Within Each Racial Group",
    subtitle = "Difference between the proportion of LUOFs in each quintile vs. the proportion of each racial group living in that quintile",
    x = "Census Tract Income Quintile",
    y = "LUOF Proportion - Proportion Living There",
    # shape = "Proportion of",
    # color = "Victim"
    ) +
  theme_light() #+
  # theme(legend.position = "right")

ggsave(
  filename = 'plots/plots_by_race/difference_in_proportions.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)



# Example from another script
ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual,
            aes(
              x = Income_Quintile, y = Annual_10_M, 
              color = Victim, linetype = Majority, 
              group = interaction(Victim, Majority)), linewidth = 1) +
  labs(
    title = "Lethal Use of Force Interactions",
    subtitle = "Victim Race x Majority-race in Tract x Census Tract Household Income Quintile",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population"
  ) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed"),
                        name = "Majority") + 
  guides(
    linetype = guide_legend(
      title = "Majority", override.aes = list(size = 10)),
    color = guide_legend(
      title = "Victim", override.aes = list(linetype = "solid", size = 10))) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)) +
  theme_light()
############## #
# Bar plot ####
############## #

library(ggpattern)

ggplot(
  summary_tables$IncRace_pop_and_IncRace_LUOF,
  aes(x = Quintile, y = Proportion)) +
  geom_bar_pattern(
    aes(fill = Race, pattern_density = Type),
    position = "dodge",
    stat = "identity",
    width = 0.7,
    show.legend = TRUE) +
  labs(title = "Proportion by Quintile, Race",
       x = "Quintile",
       y = "Proportion",
       pattern_density = "Proportion of") +
  scale_pattern_density_manual(
    values = c("LUOFs" = 0, "Population" = 0.2)) +
  guides(
    fill = guide_legend(override.aes = list(pattern_density = 0))
  ) +
  theme_minimal() #+
  # theme(legend.position = "right")

ggsave(
  filename = 'plots/plots_by_race/population_vs_LUOF_distribution.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)
