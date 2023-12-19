######################################################### #
# This is not a complete script of all summary tables ####
######################################################### #

# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

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
    Victim = race_imputed,
    Income_Quintile = income_quintiles_nolab,
    Majority) |> 
  count(Victim, Majority, Income_Quintile) |> 
  full_join(
    x = _,
    y =   fatal_enc$joined |>
      select(
        Income_Quintile = income_quintiles_nolab) |> 
      count(Income_Quintile) |> 
      mutate(Majority = "All", Victim = "All") |> 
      na.omit()
  )

# summary_tables$race_and_income_pop <- 
# all_tracts$income_population_quintiles_2020 |> 
# select(
#   `White Pop` = NH_WhiteE,
#   `Black Pop` = NH_BlackE,
#   `Hispanic/Latino Pop` = Hisp_LatinoE,
#   `Income Quintile` = income_quintiles_nolab,
#   Majority) |> 
#   aggregate(`White Pop` + `Black Pop` + `Hispanic/Latino Pop` ~ `Income Quintile` + Majority, FUN = sum)

summary_tables$race_and_income_pop <- 
  all_tracts$income_population_quintiles_2020 %>%
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
    Killed_Per_Yr = n / 6,
    Annual_10_M = 
      case_when(
        Victim == 'White' ~ n / White / 6 * 10000000,
        Victim == 'Black' ~ n / Black / 6 * 10000000,
        Victim == 'Hispanic/Latino' ~ n / Latino / 6 * 10000000
      ),
    Income_Quintile = as.factor(Income_Quintile),
    Victim = factor(Victim)
  ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_all_data.csv")

##################################################### #
# Annualized -- NOT PER CAPITA! ####
##################################################### #

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

###################################################### #
# Annualized, Per capita rates ####
###################################################### #

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

########################################################## #
# total_pop_quintiles_race.R ####
########################################################## #


# Total population by race 2020
summary_tables$total_pop_by_race <- 
  data.frame(
    Race = c('Black', 'Hispanic/Latino', 'White'),
    race_total_pop = c(
      sum(all_tracts$income_population_quintiles_2020$NH_BlackE),
      sum(all_tracts$income_population_quintiles_2020$Hisp_LatinoE),
      sum(all_tracts$income_population_quintiles_2020$NH_WhiteE)))

# Calculating the total population by Race in each income quintile
summary_tables$race_quint_xtab <- 
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
summary_tables$race_quint_proportions <- 
  left_join(
    x = summary_tables$race_quint_xtab,
    y = summary_tables$total_pop_by_race,
    by = join_by(Race)) |> 
  mutate(Proportion = population / race_total_pop) |> 
  select(-race_total_pop)

################### #
### Save as CSV ####
################### #

# summary_tables$race_quint_proportions |> select(-population) |>  
#   mutate(Proportion = Proportion * 100) |>
#   pivot_wider(names_from = Race, values_from = Proportion) |> 
#   write_csv(file = "race_quint_proportions.csv")

