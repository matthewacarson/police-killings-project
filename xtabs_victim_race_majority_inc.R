# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

################################################# #
# Run setup file to bring in data to summarize ####
################################################# #
source(file = "police-killings-setup.R")


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


summary_tables$race_victim_majority_and_quintile <- 
  fatal_enc$joined |> 
  filter(race_imputed != "Other/Unknown") |> 
  select(
    Victim_Race = race_imputed,
    Income_Quintile = income_quintiles_nolab,
    Majority) |> 
  count(Victim_Race, Majority, Income_Quintile)


# summary_tables$race_and_income_pop <- 
  # all_tracts$income_population_quintiles_2020 |> 
  # select(
  #   `White Pop` = NH_WhiteE,
  #   `Black Pop` = NH_BlackE,
  #   `Hispanic/Latino Pop` = Hisp_LatinoE,
  #   `Income Quintile` = income_quintiles_nolab,
  #   Majority) |> 
  #   aggregate(`White Pop` + `Black Pop` + `Hispanic/Latino Pop` ~ `Income Quintile` + Majority, FUN = sum)

summary_tables$race_and_income_pop <- all_tracts$income_population_quintiles_2020 %>%
  select(
    `White Pop` = NH_WhiteE,
    `Black Pop` = NH_BlackE,
    `Hispanic/Latino Pop` = Hisp_LatinoE,
    Income_Quintile = income_quintiles_nolab,
    Majority
  ) %>%
  group_by(Income_Quintile, Majority) %>%
  summarize(
    White = sum(`White Pop`),
    Black = sum(`Black Pop`),
    Latino = sum(`Hispanic/Latino Pop`)
  ) |> na.omit()


summary_tables$victim_race_majority_quint <-
  left_join(
    x = summary_tables$race_victim_majority_and_quintile,
    y = summary_tables$race_and_income_pop
  ) |> na.omit() # |> write_csv(file = "xtabs_race_majority_inc/victim_race_majority_quint.csv")


summary_tables$victim_race_majority_quint_annual <-
summary_tables$victim_race_majority_quint |> 
  mutate(
    Killed_Per_Yr =
      case_when(
        Victim_Race == 'White' ~ n / 6 ,
        Victim_Race == 'Black' ~ n / 6,
        Victim_Race == 'Hispanic/Latino' ~ n /  6
      ),
    Annual_10_M = 
      case_when(
        Victim_Race == 'White' ~ n / White / 6 * 10000000,
        Victim_Race == 'Black' ~ n / Black / 6 * 10000000,
        Victim_Race == 'Hispanic/Latino' ~ n / Latino / 6 * 10000000
      ),
    Income_Quintile = as.factor(Income_Quintile)
  ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_all_data.csv")

##################################################### #
# Annualized -- NOT PER CAPITA! ####
##################################################### #

summary_tables$victim_race_majority_quint_annual |> # print(n = 99)
  select(
    Victim_Race,
    Majority,
    Income_Quintile,
    Killed_Per_Yr) |> 
  # filter(Victim_Race == 'Black') |> 
  pivot_wider(
    names_from = Victim_Race,
    names_prefix = "Victim_",
    values_from = Killed_Per_Yr
  )


###################################################### #
# Annualized, Per capita rates ####
###################################################### #

summary_tables$victim_race_majority_quint_annual |> # print(n = 99)
  select(
    Victim_Race,
    Majority,
    Income_Quintile,
    Annual_10_M) |> 
  # filter(Victim_Race == 'Black') |> 
  pivot_wider(
    names_from = Victim_Race,
    names_prefix = "Victim_",
    values_from = Annual_10_M
  ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_victim_race_majority_inc.csv")


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
                color = Victim_Race, group = Victim_Race), lwd = 1) +
  labs(
    title = "Majority Black Tracts",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) + guides(color = guide_legend(title = "Victim's Race"))

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
              aes(
                x = Income_Quintile, y = Annual_10_M, 
                color = Victim_Race, group = Victim_Race), lwd = 1) +
  labs(
    title = "Majority White Tracts",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) + guides(color = guide_legend(title = "Victim's Race"))

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
                color = Victim_Race, group = Victim_Race), lwd = 1) +
  labs(
    title = "Majority Hispanic/Latino Tracts",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) + guides(color = guide_legend(title = "Victim's Race"))


ggsave(
  filename = "plots/xtabs_race_majority_inc/Majority Hispanic_Latino Tracts.png",
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
              color = Victim_Race, group = Victim_Race), lwd = 1) +
  labs(
    title = "Majority Black Tracts",
    x = "Census Tract Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race"))

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
              color = Victim_Race, group = Victim_Race), lwd = 1) +
  labs(
    title = "Majority White Tracts",
    x = "Census Tract Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race"))

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
                color = Victim_Race, group = Victim_Race), lwd = 1) +
  labs(
    title = "Majority Hispanic/Latino Tracts",
    x = "Income Quintile",
    y = "Number of Persons Killed",
  ) + guides(color = guide_legend(title = "Victim's Race"))


ggsave(
  filename = "plots/xtabs_race_majority_inc/not_per_capita/Majority Hispanic_Latino Tracts.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

################################################# #
# plot all interactions ####
# Victim Race x Majority x Income Quintile
################################################# #

ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual,
            aes(
              x = Income_Quintile, y = Annual_10_M, 
              color = Victim_Race, linetype = Majority, group = interaction(Victim_Race, Majority)), lwd = 1) +
  labs(
    title = "Lethal Use of Force Interactions",
    subtitle = "Victim Race x Majority-race in Tract x Census Tract Household Income Quintile",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population"
  ) +
  scale_linetype_manual(values = c("solid", "dotdash", "longdash"),
                        name = "Majority") + 
  guides(color = guide_legend(title = "Victim", override.aes = list(linetype = "solid")))

ggsave(
  filename = "plots/xtabs_race_majority_inc/all interactions no points.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

######################### #
# Trying to add points
######################### #

ggplot() + 
  geom_line(data = summary_tables$victim_race_majority_quint_annual,
            aes(
              x = Income_Quintile, y = Annual_10_M, 
              color = Victim_Race, linetype = Majority, group = interaction(Victim_Race, Majority)), lwd = 1) + 
  geom_point(data = summary_tables$victim_race_majority_quint_annual,
             aes(
               x = Income_Quintile, y = Annual_10_M, 
               color = Victim_Race, shape = Majority, group = interaction(Victim_Race, Majority)), size = 3) +
  labs(
    title = "Lethal Use of Force Interactions",
    subtitle = "Victim Race x Majority-race Tract x Census Tract Median Household Income Quintile",
    x = "Census Tract Income Quintile",
    y = "Annualized Rate Per 10 Million Population",
  ) +
  scale_linetype_manual(
    values = c("solid", "dotdash", "longdash"), # rep("solid", 3),
    name = "Majority") + 
  guides(color = guide_legend(title = "Victim", override.aes = list(linetype = "solid")))

ggsave(
  filename = "plots/xtabs_race_majority_inc/all interactions with points.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

