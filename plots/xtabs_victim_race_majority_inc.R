# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)
library(patchwork)
################################################# #
# Run setup file to bring in data to summarize ####
################################################# #
source(file = "police-killings-setup.R")
source(file = "summary_tables.R")


# race_victim_majority_and_quintile <- with(
#   fatal_enc$joined |>
#     filter(race_imputed != "Other/Unknown") |>
#     select(
#       `Victim Race` = race_imputed,
#       `Income Quintile` = income_quintiles_nolab,
#       Majority),
#   table(
#     `Victim Race`, Majority, `Income Quintile`
#   )
# )
# 
# 
# summary_tables$race_victim_majority_and_quintile <- 
#   fatal_enc$joined |> 
#   filter(race_imputed != "Other/Unknown") |> 
#   select(
#     Victim = race_imputed,
#     Income_Quintile = income_quintiles_nolab,
#     Majority) |> 
#   count(Victim, Majority, Income_Quintile)
# 
# 
# # summary_tables$race_and_income_pop <- 
#   # all_tracts$income_population_quintiles_2020 |> 
#   # select(
#   #   `White Pop` = NH_WhiteE,
#   #   `Black Pop` = NH_BlackE,
#   #   `Hispanic/Latino Pop` = Hisp_LatinoE,
#   #   `Income Quintile` = income_quintiles_nolab,
#   #   Majority) |> 
#   #   aggregate(`White Pop` + `Black Pop` + `Hispanic/Latino Pop` ~ `Income Quintile` + Majority, FUN = sum)
# 
# summary_tables$race_and_income_pop <- 
#   all_tracts$income_population_quintiles_2020 %>%
#   select(
#     `White Pop` = NH_WhiteE,
#     `Black Pop` = NH_BlackE,
#     `Hispanic/Latino Pop` = Hisp_LatinoE,
#     Income_Quintile = income_quintiles_nolab,
#     Majority
#   ) %>%
#   group_by(Income_Quintile, Majority) %>%
#   summarize(
#     White = sum(`White Pop`),
#     Black = sum(`Black Pop`),
#     Latino = sum(`Hispanic/Latino Pop`)
#   ) |> na.omit()
# 
# 
# summary_tables$victim_race_majority_quint <-
#   left_join(
#     x = summary_tables$race_victim_majority_and_quintile,
#     y = summary_tables$race_and_income_pop
#   ) |> na.omit() # |> write_csv(file = "xtabs_race_majority_inc/victim_race_majority_quint.csv")
# 
# 
# summary_tables$victim_race_majority_quint_annual <-
# summary_tables$victim_race_majority_quint |> 
#   mutate(
#     Killed_Per_Yr =
#       case_when(
#         Victim == 'White' ~ n / 6 ,
#         Victim == 'Black' ~ n / 6,
#         Victim == 'Hispanic/Latino' ~ n /  6
#       ),
#     Annual_10_M = 
#       case_when(
#         Victim == 'White' ~ n / White / 6 * 10000000,
#         Victim == 'Black' ~ n / Black / 6 * 10000000,
#         Victim == 'Hispanic/Latino' ~ n / Latino / 6 * 10000000
#       ),
#     Income_Quintile = as.factor(Income_Quintile),
#     Victim = factor(Victim)
#   ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_all_data.csv")
# 
# ##################################################### #
# # Annualized -- NOT PER CAPITA! ####
# ##################################################### #
# 
# summary_tables$victim_race_majority_quint_annual |> # print(n = 99)
#   select(
#     Victim,
#     Majority,
#     Income_Quintile,
#     Killed_Per_Yr) |> 
#   # filter(Victim == 'Black') |> 
#   pivot_wider(
#     names_from = Victim,
#     names_prefix = "Victim_",
#     values_from = Killed_Per_Yr
#   )
# 
# 
# ###################################################### #
# # Annualized, Per capita rates ####
# ###################################################### #
# 
# summary_tables$victim_race_majority_quint_annual |> # print(n = 99)
#   select(
#     Victim,
#     Majority,
#     Income_Quintile,
#     Annual_10_M) |> 
#   # filter(Victim == 'Black') |> 
#   pivot_wider(
#     names_from = Victim,
#     names_prefix = "Victim_",
#     values_from = Annual_10_M
#   ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_victim_race_majority_inc.csv")
# 

################################################################ #
# Plots using Annualized, per capita rates ####
################################################################ #

############################################################## #
# Majority Black Tracts by Victim Race and Income Quintile ####
############################################################## #
ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual |> 
                filter(Majority == 'Black'),
              aes(
                x = Income_Quintile, y = Annual_10_M, 
                color = Victim, group = Victim), lwd = 1) +
  labs(
    title = "Majority Black Tracts",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  theme_light()

ggsave(
  filename = "plots/xtabs_race_majority_inc/Majority Black Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

############################################################## #
# Majority White Tracts by Victim Race and Income Quintile ####
############################################################## #

ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual |> 
                filter(Majority == 'White'),
              aes(x = Income_Quintile, y = Annual_10_M, 
                color = Victim, group = Victim), lwd = 1) +
  labs(
    title = "Majority White Tracts",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  theme_light()

ggsave(
  filename = "plots/xtabs_race_majority_inc/Majority White Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

####################################################################### #
# Majority Hispanic/Latino Tracts by Victim Race and Income Quintile ####
####################################################################### #

ggplot() + 
  geom_line(  data = summary_tables$victim_race_majority_quint_annual |> 
                filter(Majority == 'Hispanic/Latino'),
              aes(
                x = Income_Quintile, y = Annual_10_M, 
                color = Victim, group = Victim), lwd = 1) +
  labs(
    title = "Majority Hispanic/Latino Tracts",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  theme_light()


ggsave(
  filename = "plots/xtabs_race_majority_inc/Majority Hispanic_Latino Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

################################################# #
# plot all interactions ####
# Victim Race x Majority x Income Quintile
################################################# #

ggplot(data = summary_tables$victim_race_majority_quint_annual,
       aes(
         x = Income_Quintile, y = Annual_10_M, 
         color = Victim, linetype = Majority, 
         group = interaction(Victim, Majority))) + 
  geom_line(lwd = 1) +
  labs(
    title = "Lethal Use of Force Interactions",
    subtitle = "Victim Race x Majority-race in Tract x Census Tract Household Income Quintile",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population"
  ) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash"),
                        name = "Majority") + 
  guides(
    linetype = guide_legend(
      title = "Majority", override.aes = list(size = 10)),
    color = guide_legend(
      title = "Victim", 
      override.aes = list(
        size = 10, linewidth = 10))) +
  theme(
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)) +
  theme_light()


ggsave(
  filename = "plots/xtabs_race_majority_inc/all interactions no points.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

######################### #
# Trying to add points
######################### #

# ggplot() + 
#   geom_line(data = summary_tables$victim_race_majority_quint_annual,
#             aes(
#               x = Income_Quintile, y = Annual_10_M, 
#               color = Victim, linetype = Majority, group = interaction(Victim, Majority)), lwd = 1) + 
#   geom_point(data = summary_tables$victim_race_majority_quint_annual,
#              aes(
#                x = Income_Quintile, y = Annual_10_M, 
#                color = Victim, shape = Majority, group = interaction(Victim, Majority)), size = 3) +
#   labs(
#     title = "Lethal Use of Force Interactions",
#     subtitle = "Victim Race x Majority-race Tract x Census Tract Median Household Income Quintile",
#     x = "Census Tract Income Quintile",
#     y = "Annualized Rate Per 10 Million Population",
#   ) +
#   scale_linetype_manual(
#     values = c("solid", "dotdash", "longdash"), # rep("solid", 3),
#     name = "Majority") + 
#   guides(color = guide_legend(title = "Victim", override.aes = list(linetype = "solid")))

# Using code below instead (12/4)
ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual,
            aes(
              x = Income_Quintile, y = Annual_10_M, 
              color = Victim, linetype = Majority, 
              group = interaction(Victim, Majority)), lwd = 1) + 
  geom_point(data = summary_tables$victim_race_majority_quint_annual,
             aes(
               x = Income_Quintile, y = Annual_10_M, 
               color = Victim, shape = Majority, 
               group = interaction(Victim, Majority)), size = 3) +
  labs(
    title = "Lethal Use of Force Interactions",
    subtitle = "Victim Race x Majority-race Tract x Census Tract Median Household Income Quintile",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) +
  scale_linetype_manual(
    values = c("solid", "dotdash", "longdash"),
    name = "Majority") + 
  guides(
    color = guide_legend(
      title = "Victim", override.aes = list(shape = c(NA, NA, NA)))
  ) + theme_light()


ggsave(
  filename = "plots/xtabs_race_majority_inc/all interactions with points.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)




################################################################ #
# Plots using Annualized but NOT PER CAPITA rates ####
################################################################ #


############################################################## #
# Majority Black Tracts by Victim Race and Income Quintile ####
############################################################## #
ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual |> 
              filter(Majority == 'Black'),
            aes(
              x = Income_Quintile, y = Killed_Per_Yr, 
              color = Victim, group = Victim), lwd = 1) +
  labs(
    title = "Majority Black Tracts",
    x = "Census Tract Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25),
    limits = c(0, 130)) +
  theme_light()

ggsave(
  filename = "plots/xtabs_race_majority_inc/not_per_capita/Majority Black Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

############################################################## #
# Majority White Tracts by Victim Race and Income Quintile ####
############################################################## #

ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual |> 
              filter(Majority == 'White'),
            aes(
              x = Income_Quintile, y = Killed_Per_Yr, 
              color = Victim, group = Victim), lwd = 1) +
  labs(
    title = "Majority White Tracts",
    x = "Census Tract Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25),
    limits = c(0, 130)) +
  theme_light()

ggsave(
  filename = "plots/xtabs_race_majority_inc/not_per_capita/Majority White Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)


####################################################################### #
# Majority Hispanic/Latino Tracts by Victim Race and Income Quintile ####
####################################################################### #

ggplot() + 
  geom_line(  data = summary_tables$victim_race_majority_quint_annual |> 
                filter(Majority == 'Hispanic/Latino'),
              aes(
                x = Income_Quintile, y = Killed_Per_Yr, 
                color = Victim, group = Victim), lwd = 1) +
  labs(
    title = "Majority Hispanic/Latino Tracts",
    x = "Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25),
    limits = c(0, 130)) +
  theme_light()

ggsave(
  filename = "plots/xtabs_race_majority_inc/not_per_capita/Majority Hispanic_Latino Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)


######################################################### #
# Plot everything together ####
######################################################### #
(
ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual |> 
              filter(Majority == 'Black'),
            aes(
              x = Income_Quintile, y = Killed_Per_Yr, 
              color = Victim, group = Victim, linetype = Victim), 
            lwd = 1, show.legend = FALSE) +
  labs(
    subtitle = "Majority Black Tracts",
    x = NULL,
    # x = "Census Tract Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race")) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25),
    limits = c(0, 130)) +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")) +
  theme_light() + 
  theme(
    axis.text.x = element_text(color = 'black'),
    axis.text.y = element_text(color = 'black')) +
  
  # Next plot
  ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual |> 
              filter(Majority == 'White'),
            aes(
              x = Income_Quintile, y = Killed_Per_Yr, 
              color = Victim, group = Victim, linetype = Victim), 
            lwd = 1, show.legend = TRUE) +
  labs(
    subtitle = "Majority White Tracts",
    x = NULL,
    # x = "Census Tract Income Quintile",
    y = NULL,
    color = "Victim's Race",
    linetype = "Victim's Race"
  ) + guides(color = guide_legend(
    title = "Victim's Race")) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25),
    limits = c(0, 130)) +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")) +
  theme_light() + 
  theme(
    legend.key.size = unit(3, 'lines'),
    legend.position = 'bottom',
    axis.text.x = element_text(color = 'black'),
    axis.text.y = element_blank()) +
  
  # Add last plot
  ggplot() + 
  geom_line(  data = summary_tables$victim_race_majority_quint_annual |> 
                filter(Majority == 'Hispanic/Latino'),
              aes(
                x = Income_Quintile, y = Killed_Per_Yr, 
                color = Victim, group = Victim, linetype = Victim), 
              lwd = 1, show.legend = FALSE) +
  labs(
    subtitle = "Majority Hispanic/Latino Tracts",
    # x = "Census Tract Income Quintile",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25), limits = c(0, 130), position = "right") +
  scale_x_discrete(labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")) +
  theme_light() + 
  theme(
    axis.text.x = element_text(color = 'black'),
    # axis.text.y = element_blank()
    )

) +
  plot_annotation(
    title = "Majority Race in Tract by Median Household Income Quintile",
    # theme = theme(plot.title = element_text(hjust = 0.5))
  )


ggsave(
  filename = "plots/xtabs_race_majority_inc/not_per_capita/all_plots.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)
