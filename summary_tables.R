
# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

####################### #
# Run setup script ####
####################### #
source(file = 'setup.R')

if (!exists("summary_tables")) {summary_tables <- new.env()}

######################################################### #
# This is not a complete script of all summary tables
######################################################### #

######################################################### #
# Creating basic summary table ####
######################################################### #

######################################################### #
## killings per year -BY- quintile ####
######################################################### #
summary_tables$fatal_enc_table_1 <-  fatal_enc$joined %>%
  count(Quintile = income_quintiles_nolab) %>% rename(Killings = n) %>% 
  filter(!is.na(Quintile)) %>% mutate(Killings_Per_Yr = Killings / 6)

# col dictionary
# Income =                tract income quintile
# Killings =              number killed in quintile
# Killings_Per_Yr =       number of LUOF per year

######################################################### #
## total population in quintile ####
######################################################### #
summary_tables$pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_quintiles_nolab,
  sum, na.rm = TRUE) %>% 
  data.frame(Quintile = rownames(.), Population = .)
######################################################### #
## annualized: OVERALL rate for quintiles ####
######################################################### #
summary_tables$summary_1 <- 
  left_join(
    x = summary_tables$fatal_enc_table_1,
    y = summary_tables$pop_table_1,
    by = "Quintile")

summary_tables$summary_1$Majority <- "All"

summary_tables$summary_1 <- summary_tables$summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000)
######################################################### #
## killings: majority -BY- quintile ####
######################################################### #
summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles_nolab) |> 
  rename(Killings = n, Quintile = income_quintiles_nolab)

# print(summary_tables$race_and_income, n = 100)

######################################################### #
## population: majoirty - BY- quintile ####
## For example: black people living in majority black
## tracts in quintile 1.
######################################################### #
summary_tables$race_and_income_pop <- all_tracts$income_population_quintiles_2020 |> 
  aggregate(Total_popE ~ Majority + income_quintiles_nolab, FUN = sum) |> 
  rename(Population = Total_popE, Quintile = income_quintiles_nolab)
######################################################### #
## annualized rate: majority -BY- quintile  ####
######################################################### #
summary_tables$race_and_income_summary <- 
  left_join(
    x = summary_tables$race_and_income,
    y = summary_tables$race_and_income_pop,
    by = c("Majority", "Quintile")
  )

summary_tables$race_and_income_summary <- 
  summary_tables$race_and_income_summary |> 
  mutate(
    Annualized_Per_10_M =
      Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Quintile,
    Population,
    Killings,
    Annualized_Per_10_M
  ) |> add_row(
    summary_tables$summary_1 |>
      select(-Killings_Per_Yr)
  ) |> na.omit()

######################################################### #
## killings: victim -BY- majority -BY- quintile ####
######################################################### #

summary_tables$race_victim_majority_and_quintile <- 
  fatal_enc$joined |> 
  filter(race_imputed != "Other/Unknown") |> 
  select(
    Victim = race_imputed,
    Quintile = income_quintiles_nolab,
    Majority) |> 
  count(Victim, Majority, Quintile) |> 
  rename(Killings = n) |> 
  full_join(
    x = _,
    y =   fatal_enc$joined |>
      select(
        Quintile = income_quintiles_nolab) |> 
      count(Quintile) |> 
      rename(Killings = n) |> 
      mutate(Majority = "All", Victim = "All") |> 
      na.omit(), 
    by = join_by(Victim, Majority, Quintile, Killings)
  )

# print(summary_tables$race_victim_majority_and_quintile, n = 30)

################################################################### #
## population: racial population within majority -BY- quintile   ####
################################################################### #

summary_tables$race_and_income_pop <- 
  all_tracts$income_population_quintiles_2020 %>%
  select(
    `White Pop` = NH_WhiteE,
    `Black Pop` = NH_BlackE,
    `Hispanic/Latino Pop` = Hisp_LatinoE,
    Quintile = income_quintiles_nolab,
    Majority
  ) %>%
  group_by(Quintile, Majority) %>%
  summarize(
    White = sum(`White Pop`),
    Black = sum(`Black Pop`),
    Latino = sum(`Hispanic/Latino Pop`)
  ) |> na.omit()
######################################################### #
## Victim -BY- Majority -BY- Quintile -BY- Killings  ####
######################################################### #

summary_tables$victim_race_majority_quint <-
  left_join(
    x = summary_tables$race_victim_majority_and_quintile,
    y = summary_tables$race_and_income_pop,
    by = join_by(Majority, Quintile)
  ) |> na.omit() # |> write_csv(file = "xtabs_race_majority_inc/victim_race_majority_quint.csv")
######################################################### #
## victim_race_majority_quint: annual rate ####
######################################################### #

summary_tables$victim_race_majority_quint_annual <-
  summary_tables$victim_race_majority_quint |> 
  mutate(
    Killed_Per_Yr = Killings / 6,
    Annual_10_M = 
      case_when(
        Victim == 'White' ~ Killings / White / 6 * 10000000,
        Victim == 'Black' ~ Killings / Black / 6 * 10000000,
        Victim == 'Hispanic/Latino' ~ Killings / Latino / 6 * 10000000
      ),
    Quintile = as.factor(Quintile),
    Victim = factor(Victim)
  )
##################################################### #
## Annualized Rates ####
##################################################### #
### NOT PER CAPITA! ####
##################################################### #

# summary_tables$victim_race_majority_quint_annual |> # print(n = 99)
#   select(
#     Victim,
#     Majority,
#     Quintile,
#     Killed_Per_Yr) |> 
#   # filter(Victim == 'Black') |> 
#   pivot_wider(
#     names_from = Victim,
#     names_prefix = "Victim_",
#     values_from = Killed_Per_Yr
#   )

###################################################### #
### Per capita rates ####
###################################################### #

# summary_tables$victim_race_majority_quint_annual |> # print(n = 99)
#   select(
#     Victim,
#     Majority,
#     Quintile,
#     Annual_10_M) |> 
#   # filter(Victim == 'Black') |> 
#   pivot_wider(
#     names_from = Victim,
#     names_prefix = "Victim_",
#     values_from = Annual_10_M
#   ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_victim_race_majority_inc.csv")

############################################### #
############################################### #
# SCRIPT: total_pop_quintiles_race.R ####
############################################### #
############################################### #

######################################################### #
## Total population by race 2020 ####
######################################################### #

summary_tables$total_pop_by_race <- 
  data.frame(
    Majority = c('Black', 'Hispanic/Latino', 'White'),
    race_total_pop = c(
      sum(all_tracts$income_population_quintiles_2020$NH_BlackE),
      sum(all_tracts$income_population_quintiles_2020$Hisp_LatinoE),
      sum(all_tracts$income_population_quintiles_2020$NH_WhiteE)))

############################################################ #
## total population -BY- Majority -BY- income quintile ####
############################################################ #

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
    names_to = 'Majority',
    values_to = "Population_in_tracts") |> 
  aggregate(Population_in_tracts ~ Quintile + Majority, FUN = sum)

################################### #
# Race of Victim ####
# PLOT SCRIPT: race_of_victim.R #
################################### #
source('race_of_victim_summary.R')

############################################### #
############################################### #
# SCRIPT: 15_quantile_plot.R ####
############################################### #
############################################### #

# ############################## #
## Grouped by Race/Ethnicity ####
# ############################## #

# Note: this script uses 15 tract income quantiles.
# The reasons for choosing 15 quantiles is that it lets
# us divide quintiles into tertiles of each quintile. 
# So by looking at the three lowest of 15 quantiles, 
# we are looking at the tertiles of the lowest quintile.

summary_tables$quant15_table <- 
  fatal_enc$joined |> 
  count(Majority, income_15_quant) |> 
  rename(Killings = n) |> 
  na.omit() |> 
  add_row(
    fatal_enc$joined |> 
      count(income_15_quant) |> 
      rename(Killings = n) |> 
      na.omit() |> 
      mutate(Majority = 'All')
  )

summary_tables$quant15_pop <- 
  # Black
  all_tracts$income_population_quintiles_2020 |> 
  filter(Majority == 'Black') |> 
  aggregate(NH_BlackE ~ Majority + income_15_quant, FUN = sum) |> 
  rename(Population = NH_BlackE) |> 
  add_row(
    # White
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'White') |> 
      aggregate(NH_WhiteE ~ Majority + income_15_quant, FUN = sum) |> 
      rename(Population = NH_WhiteE)) |> 
  add_row(
    # Latino
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'Hispanic/Latino') |> 
      aggregate(Hisp_LatinoE ~ Majority + income_15_quant, FUN = sum) |> 
      rename(Population = Hisp_LatinoE)) |> 
  add_row(
    all_tracts$income_population_quintiles_2020 |> 
      aggregate(Total_popE ~ income_15_quant, FUN = sum) |> 
      rename(Population = Total_popE) |> mutate(Majority = 'All')
  )

summary_tables$quant15_joined <- 
  left_join(
    x = summary_tables$quant15_table,
    y = summary_tables$quant15_pop,
    by = join_by(Majority, income_15_quant))

summary_tables$quant15_summary <- 
  summary_tables$quant15_joined |> 
  mutate(
    Annualized_Per_10_M = 
      Killings / Population * 10000000 / 6,
    Majority = factor(Majority, ordered = TRUE)
  ) |> 
  rename(Quintile = income_15_quant)

############################################### #
############################################### #
# SCRIPT: 200_quantile_plots.R ####
############################################### #
############################################### #

######################## #
## Tables for 200 bins ####
######################## #

summary_tables$bins200_table_1 <-  fatal_enc$joined |> 
  filter(!is.na(income_bins_200)) |> 
  count(quantile_200 = income_bins_200) |> 
  rename(Killings = n) |> 
  mutate(Killings_Per_Yr = Killings / 6)

summary_tables$bins200_pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_bins_200,
  sum, na.rm = TRUE) %>%
  data.frame(quantile_200 = rownames(.), Population = .)

# Median income of each bin
# fatal_enc$joined |>  aggregate(QuintileE ~ Majority + income_bins, FUN = median)

summary_tables$bins200_summary_1 <-
  left_join(
    x = summary_tables$bins200_table_1,
    y = summary_tables$bins200_pop_table_1,
    by = "quantile_200"
  ) |> mutate(Annualized_Per_10_M = Killings_Per_Yr / Population * 10000000)

summary_tables$bins200_summary_1$quantile_200 <- as.numeric(summary_tables$bins200_summary_1$quantile_200)

############################################### #
############################################### #
# SCRIPT: by_quintile.R ####
############################################### #
############################################### #

# ############################## #
## Grouped by majority race/ethnicity ####
# ############################## #

# table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles_nolab) |> 
  rename(Killings = n, Quintile = income_quintiles_nolab)

summary_tables$race_and_income_pop <- 
  all_tracts$income_population_quintiles_2020 |> 
  aggregate(Total_popE ~ Majority + income_quintiles_nolab, FUN = sum) |> 
  rename(Population = Total_popE, Quintile = income_quintiles_nolab)

summary_tables$race_and_income_summary <- 
  left_join(
    x = summary_tables$race_and_income,
    y = summary_tables$race_and_income_pop,
    by = join_by(Majority, Quintile)
  ) |> 
  mutate(
    Annualized_Per_10_M =
      Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Quintile,
    Population,
    Killings,
    Annualized_Per_10_M
  ) |> add_row(
    summary_tables$summary_1 |>
      select(-Killings_Per_Yr)
  ) |> na.omit() |> 
  mutate(
    Quintile = factor(Quintile, ordered = TRUE),
    Majority = factor(Majority, ordered = TRUE))

################################################ #
################################################ #
# SCRIPT: inc_quint_majority_race_same_plot.R ####
################################################ #
################################################ #

################## #
## Majority Race ####
################## #
# This needs to be assigned to an object for cowplot
summary_tables$majority_table_1 <-  fatal_enc$joined %>%
  count(Majority = Majority) %>% rename(Killings = n) %>% 
  filter(!is.na(Majority)) %>% mutate(Killings_Per_Yr = Killings / 6)

summary_tables$majority_pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$Majority,
  sum, na.rm = TRUE) %>% 
  data.frame(Majority = rownames(.), Population = .)

summary_tables$majority_summary_1 <- 
  left_join(
    x = summary_tables$majority_table_1,
    y = summary_tables$majority_pop_table_1,
    by = "Majority"
  )

summary_tables$majority_summary_1 <- 
  summary_tables$majority_summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000
  )

############################################# #
############################################# #
# SCRIPT: income_100_quantiles_and_race.R ####
############################################# #
############################################# #

# all_tracts$income_population_quintiles_2020 |> 
# aggregate(Total_popE ~ Majority + income_decile, FUN = sum)

######################################### #

summary_tables$decile_race_denom <- 
  fatal_enc$joined |> 
  count(Majority, income_decile) |> 
  rename(Killings = n) |> 
  na.omit() |> 
  add_row(
    fatal_enc$joined |> 
      count(income_decile) |> 
      rename(Killings = n) |> 
      na.omit() |> 
      mutate(Majority = 'All')
  )

summary_tables$decile_pop_race_denom <- 
  # Black
  all_tracts$income_population_quintiles_2020 |> 
  filter(Majority == 'Black') |> 
  aggregate(NH_BlackE ~ Majority + income_decile, FUN = sum) |> 
  rename(Population = NH_BlackE) |> 
  add_row(
    # White
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'White') |> 
      aggregate(NH_WhiteE ~ Majority + income_decile, FUN = sum) |> 
      rename(Population = NH_WhiteE)) |> 
  add_row(
    # Latino
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'Hispanic/Latino') |> 
      aggregate(Hisp_LatinoE ~ Majority + income_decile, FUN = sum) |> 
      rename(Population = Hisp_LatinoE)) |> 
  add_row(
    all_tracts$income_population_quintiles_2020 |> 
      aggregate(Total_popE ~ income_decile, FUN = sum) |> 
      rename(Population = Total_popE) |> mutate(Majority = 'All')
  )

summary_tables$decile_race_denom <- 
  left_join(
    x = summary_tables$decile_pop_race_denom,
    y = summary_tables$decile_race_denom,
    by = join_by(Majority, income_decile)
  ) |> 
  mutate(Annualized_Per_10_M = 
           # coalesce replaces NAs with 0
           coalesce(Killings / Population * 10000000 / 6, 0)) |>   
  rename(Income = income_decile)

########################################## #
########################################## #
# SCRIPT: income_deciles_and_race.R ####
########################################## #
########################################## #

# all_tracts$income_population_quintiles_2020 |> 
# aggregate(Total_popE ~ Majority + income_decile, FUN = sum)
######################################### #

summary_tables$decile_race_denom <- 
  fatal_enc$joined |> 
  count(Majority, income_decile) |> 
  rename(Killings = n) |> 
  na.omit() |> 
  add_row(
    fatal_enc$joined |> 
      count(income_decile) |> 
      rename(Killings = n) |> 
      na.omit() |> 
      mutate(Majority = 'All')
  )

summary_tables$decile_pop_race_denom <- 
  # Black
  all_tracts$income_population_quintiles_2020 |> 
  filter(Majority == 'Black') |> 
  aggregate(NH_BlackE ~ Majority + income_decile, FUN = sum) |> 
  rename(Population = NH_BlackE) |> 
  add_row(
    # White
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'White') |> 
      aggregate(NH_WhiteE ~ Majority + income_decile, FUN = sum) |> 
      rename(Population = NH_WhiteE)) |> 
  add_row(
    # Latino
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'Hispanic/Latino') |> 
      aggregate(Hisp_LatinoE ~ Majority + income_decile, FUN = sum) |> 
      rename(Population = Hisp_LatinoE)) |> 
  add_row(
    all_tracts$income_population_quintiles_2020 |> 
      aggregate(Total_popE ~ income_decile, FUN = sum) |> 
      rename(Population = Total_popE) |> mutate(Majority = 'All')
  )

summary_tables$decile_race_denom <- 
  left_join(
    x = summary_tables$decile_pop_race_denom,
    y = summary_tables$decile_race_denom,
    by = join_by(Majority, income_decile)
  ) |> 
  mutate(Annualized_Per_10_M = 
           # coalesce replaces NAs with 0
           coalesce(Killings / Population * 10000000 / 6, 0)) |>   
  rename(Income = income_decile)

############################################# #
############################################# #
# SCRIPT: quintiles_race_denominators.R ####
############################################# #
############################################# #

# ############################## #
## Grouped by Race/Ethnicity ####
# ############################## #

# table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

summary_tables$race_income_luof_race_denom <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles) |> 
  rename(Killings = n) |> 
  na.omit() |> 
  add_row(
    fatal_enc$joined |> 
      count(income_quintiles) |> 
      rename(Killings = n) |> 
      na.omit() |> 
      mutate(Majority = 'All')
  )

summary_tables$race_and_income_pop_race_denom <- 
  # Black
  all_tracts$income_population_quintiles_2020 |> 
  filter(Majority == 'Black') |> 
  aggregate(NH_BlackE ~ Majority + income_quintiles, FUN = sum) |> 
  rename(Population = NH_BlackE) |> 
  add_row(
    # White
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'White') |> 
      aggregate(NH_WhiteE ~ Majority + income_quintiles, FUN = sum) |> 
      rename(Population = NH_WhiteE)) |> 
  add_row(
    # Latino
    all_tracts$income_population_quintiles_2020 |> 
      filter(Majority == 'Hispanic/Latino') |> 
      aggregate(Hisp_LatinoE ~ Majority + income_quintiles, FUN = sum) |> 
      rename(Population = Hisp_LatinoE)) |> 
  add_row(
    all_tracts$income_population_quintiles_2020 |> 
      aggregate(Total_popE ~ income_quintiles, FUN = sum) |> 
      rename(Population = Total_popE) |> mutate(Majority = 'All')
  )


summary_tables$race_income_summary_race_denom <- 
  left_join(
    x = summary_tables$race_and_income_pop_race_denom,
    y = summary_tables$race_income_luof_race_denom,
    by = join_by(Majority, income_quintiles)
  ) |> 
  mutate(Annualized_Per_10_M = 
           Killings / Population * 10000000 / 6,
         Majority = factor(Majority, ordered = TRUE)) |> 
  rename(Income = income_quintiles)

