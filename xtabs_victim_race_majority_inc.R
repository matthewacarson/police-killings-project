# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
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
  ) |> na.omit()

summary_tables$victim_race_majority_quint_annual <- 
summary_tables$victim_race_majority_quint |> 
  mutate(
    Annual_10_M = 
      case_when(
        Victim_Race == 'White' ~ n / White / 6 * 10000000,
        Victim_Race == 'Black' ~ n / Black / 6 * 10000000,
        Victim_Race == 'Hispanic/Latino' ~ n / Latino / 6 * 10000000
      ),
    Income_Quintile = as.factor(Income_Quintile)
  )

summary_tables$victim_race_majority_quint_annual |> 
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
  ) |> write_csv(file = "xtabs_victim_race_majority_inc.csv")


