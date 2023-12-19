# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")
source(file = "summary_tables.R")


# ################################### #
# Plots using race of the victim 
# ################################### #

ggplot(
  summary_tables$bin_table_race_proportion, 
  aes(x = Income, y = Proportion, color = race_imputed)
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


## Race of the victim only plot ####
summary_tables$race_freq <- 
  table(fatal_enc$joined$race_imputed) |> 
  as.data.frame() |> 
  rename(Race = Var1) |> 
  filter(Race %in% c(
    "Black",
    "White",
    "Hispanic/Latino")
  ) |> mutate(
    Prop = Freq / nrow(fatal_enc$joined),
    Race =
      case_when(
        Race == "Black" ~ "Black",
        Race == "White" ~ "White",
        Race == "Hispanic/Latino" ~ "Latino"
      )
  )

summary_tables$race_freq$Race <- factor(
  x = summary_tables$race_freq$Race,
  levels = c(
    "White",
    "Black",
    "Latino"
  )
)

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

summary_tables$quiniles_race_victim <- 
  left_join(
    x = fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles_nolab) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |>
      count(Income = income_quintiles_nolab,Race = race_imputed) |>
      rename(Killings_race_inc = n),
    y =   fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles_nolab) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(Race = race_imputed) |> 
      rename(Killed_race = n),
    by = "Race"
  ) |> mutate(
    Prop = Killings_race_inc / Killed_race
  ) |> select(Income, Race, Prop)

summary_tables$quiniles_race_victim <- 
  summary_tables$quiniles_race_victim |> 
  add_row(
    summary_tables$summary_1 |> 
      select(Income, Race = Majority,Killings_quintile = Killings) |> 
      mutate(Total_Killed = sum(Killings_quintile)) |> 
      mutate(Prop = Killings_quintile / Total_Killed) |> 
      select(Income, Race, Prop) |> 
      mutate(Income = factor(Income, ordered = T))
  )
# summary_tables$race_and_income_summary

### Reordering Race factors ####
###
summary_tables$quiniles_race_victim$Race <- 
  factor(
    summary_tables$quiniles_race_victim$Race, 
    levels = c("All", "Black",
               "White", "Hispanic/Latino"))

ggplot(
  data = summary_tables$quiniles_race_victim,
  aes(x = Race, y = Prop, fill = Income)) +
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
  pivot_wider(names_from = Race, values_from = Prop) #|> write_csv(file = "quiniles_race_victim.csv")



############################################ #
# LUOF and race population distribution ####
############################################ #

all_tracts$total_pop_by_race <- 
  data.frame(
    Race = c('Black', 'Hispanic/Latino', 'White'),
    race_total_pop = c(
      sum(all_tracts$income_population_quintiles_2020$NH_BlackE),
      sum(all_tracts$income_population_quintiles_2020$Hisp_LatinoE),
      sum(all_tracts$income_population_quintiles_2020$NH_WhiteE)))

# Calculating the total population by Race in each income quintile
all_tracts$race_quint_xtab <- 
  all_tracts$income_population_quintiles_2020 |> 
  select(
    "White" = NH_WhiteE, 
    "Black" = NH_BlackE, 
    "Hispanic/Latino" = Hisp_LatinoE, 
    Quintile = income_quintiles_nolab,
    NAME) |> 
  pivot_longer(
    cols = c('White', 'Black', 'Hispanic/Latino'),
    names_to = 'Race',
    values_to = "population") |> 
  aggregate(population ~ Quintile + Race, FUN = sum)

# Calculate the proportion of each racial group living in each income
# quintile
all_tracts$race_quint_proportions <- 
  left_join(
    x = all_tracts$race_quint_xtab,
    y = all_tracts$total_pop_by_race,
    by = join_by(Race)) |> 
  mutate(Population = population / race_total_pop) |> 
  select(-race_total_pop)


# making changes
right_join(
  x = summary_tables$quiniles_race_victim |> 
    rename(Quintile = Income, LUOFs = Prop),
  
  y = all_tracts$race_quint_proportions |> 
    select(-population),
  
  by = join_by(Quintile, Race)) |> 
  pivot_longer(
    cols = c("LUOFs", "Population"),
    names_to = "Type",
    values_to = "Proportion"
  ) |> 
assign(
  "IncRace_pop_and_IncRace_LUOF",
  value = _,
  envir = summary_tables)

### Two-way Table ####

summary_tables$prop_difference <- 
summary_tables$IncRace_pop_and_IncRace_LUOF |> 
  pivot_wider(
    names_from = 'Type',# c('Race', 'Type'),
    values_from = Proportion
  ) |> 
  mutate(Difference = LUOFs - Population)

summary_tables$prop_difference |> 
  write_csv(
    file = 'LUOF-proportions.csv'
  )

# reshape(
#   data = summary_tables$IncRace_pop_and_IncRace_LUOF,
#   idvar = "Quintile",
#   timevar = "Interaction",
#   direction = "wide"
# )

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
  summary_tables$IncRace_pop_and_IncRace_LUOF, 
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
  labs(title = "Distributions Within Each Racial Group",
       subtitle = "Proportion of LUOFs in each quintile vs. proportion of the racial group living in that quintile",
       x = "Quintile",
       y = "Proportion of Race Living in That Quintile",
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
