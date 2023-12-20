# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

####################### #
####################### #
# Run setup script ####
####################### #
####################### #
source(file = 'setup.R')

if (!exists("summary_tables")) {summary_tables <- new.env()}

######################################################### #
# This is not a complete script of all summary tables
######################################################### #

############################################# #
# Creating basic summary table ####
############################################# #

summary_tables$fatal_enc_table_1 <-  fatal_enc$joined %>%
  count(Income = income_quintiles_nolab) %>% rename(Killings = n) %>% 
  filter(!is.na(Income)) %>% mutate(Killings_Per_Yr = Killings / 6)

summary_tables$pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_quintiles_nolab,
  sum, na.rm = TRUE) %>% 
  data.frame(Income = rownames(.), Population = .)

summary_tables$summary_1 <- 
  left_join(
    x = summary_tables$fatal_enc_table_1,
    y = summary_tables$pop_table_1,
    by = "Income")

summary_tables$summary_1$Majority <- "All"

summary_tables$summary_1 <- summary_tables$summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000)


summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles_nolab) |> 
  rename(Killings = n)

summary_tables$race_and_income_pop <- all_tracts$income_population_quintiles_2020 |> 
  aggregate(Total_popE ~ Majority + income_quintiles_nolab, FUN = sum) |> 
  rename(Population = Total_popE)

summary_tables$race_and_income_summary <- 
  left_join(
    x = summary_tables$race_and_income,
    y = summary_tables$race_and_income_pop,
    by = c("Majority", "income_quintiles_nolab")
  )

summary_tables$race_and_income_summary <- 
  summary_tables$race_and_income_summary |> 
  mutate(
    Annualized_Per_10_M =
      Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Income = income_quintiles_nolab,
    Population,
    Killings,
    Annualized_Per_10_M
  ) |> add_row(
    summary_tables$summary_1 |>
      select(-Killings_Per_Yr)
  ) |> na.omit()

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
    Quintile = income_quintiles_nolab,
    Majority) |> 
  count(Victim, Majority, Quintile) |> 
  full_join(
    x = _,
    y =   fatal_enc$joined |>
      select(
        Quintile = income_quintiles_nolab) |> 
      count(Quintile) |> 
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
    Quintile = income_quintiles_nolab,
    Majority
  ) %>%
  group_by(Quintile, Majority) %>%
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
    Quintile = as.factor(Quintile),
    Victim = factor(Victim)
  ) # |> write_csv(file = "xtabs_race_majority_inc/xtabs_all_data.csv")


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

# Total population by race 2020
summary_tables$total_pop_by_race <- 
  data.frame(
    Majority = c('Black', 'Hispanic/Latino', 'White'),
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
    names_to = 'Majority',
    values_to = "population") |> 
  aggregate(population ~ Quintile + Majority, FUN = sum)

# Calculate the proportion of each racial group living in each income
# quintile
summary_tables$race_quint_proportions <- 
  left_join(
    x = summary_tables$race_quint_xtab,
    y = summary_tables$total_pop_by_race,
    by = join_by(Majority)) |> 
  mutate(Proportion = population / race_total_pop) #|> 
  # select(-race_total_pop)

################### #
### Save as CSV ####
################### #

# summary_tables$race_quint_proportions |> select(-population) |>  
#   mutate(Proportion = Proportion * 100) |>
#   pivot_wider(names_from = Race, values_from = Proportion) |> 
#   write_csv(file = "race_quint_proportions.csv")

############################################### #
############################################### #
# SCRIPT: race_of_victim.R ####
############################################### #
############################################### #

# Plot quintiles of number of race of victim and quintile of census tract
# they were killed in
summary_tables$quiniles_race_victim <-  
  fatal_enc$joined |> 
  filter(
    !is.na(income_quintiles_nolab) &
      race_imputed %in% c(
        "Black", 
        "White", 
        "Hispanic/Latino")) |> 
  count(Income = income_quintiles_nolab, race_imputed) |> 
  rename(Killings = n) |>
  mutate(Killings_Per_Yr = Killings / 6) # |> 
  # select(-Killings)

### plots$race_100 ####
summary_tables$quintile_race_proportion <- 
  left_join(
    x = fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles_nolab) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(Quintile = income_quintiles_nolab, race_imputed) |> 
      rename(Killings_by_Quintile_and_Race = n),
    y = fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles_nolab) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(race_imputed) |> 
      rename(Killings_Race_Total = n),
    by = "race_imputed"
  ) |> 
  mutate(
    Proportion_LUOF = Killings_by_Quintile_and_Race / Killings_Race_Total
  ) |> 
  rename(Race = race_imputed)

summary_tables$bin_table_race_proportion <- 
  left_join(
    x = fatal_enc$joined |> 
      filter(
        !is.na(income_bins_100) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(Income = income_bins_100, race_imputed) |> 
      rename(Killings_inc_race = n),
    y = fatal_enc$joined |> 
      filter(
        !is.na(income_bins_100) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(race_imputed) |> 
      rename(Killings_race_only = n),
    by = "race_imputed"
  ) |> 
  mutate(
    Proportion = Killings_inc_race / Killings_race_only
  )

summary_tables$bin_table_race_proportion$Income <- as.numeric(summary_tables$bin_table_race_proportion$Income)

## ######################## #
## Using 100 quantiles/percentiles ####
## ######################## #

# this used to be called summary_tables$bin_table_race
summary_tables$race_percentiles <-  
  fatal_enc$joined |> 
  filter(
    !is.na(income_bins_100) &
      race_imputed %in% c(
        "Black", 
        "White", 
        "Hispanic/Latino")) |> 
  count(Income = income_bins_100, race_imputed) |> 
  rename(Killings = n) |> 
  mutate(Killings_Per_Yr = Killings / 6)

summary_tables$race_percentiles$Income <- 
  as.numeric(summary_tables$race_percentiles$Income)

################################################# #
## Another plot ####
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

#################################################### #
## Distribution by quintile within race ####
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

############################################ #
## LUOF and race population distribution ####
############################################ #

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
  mutate(Prop_living_in_tract = population / race_total_pop) |> 
  rename(Population_in_tracts = population)


# making changes
right_join(
  x = summary_tables$quiniles_race_victim |> 
    rename(Quintile = Income, LUOFs = Prop),
  
  y = summary_tables$race_quint_proportions,
  
  by = join_by(Quintile, Race)) |> 
  pivot_longer(
    cols = c("LUOFs", "race_total_pop"),
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
  mutate(Difference = LUOFs - race_total_pop)

# summary_tables$prop_difference |> 
#   write_csv(
#     file = 'LUOF-proportions.csv'
#   )

# reshape(
#   data = summary_tables$IncRace_pop_and_IncRace_LUOF,
#   idvar = "Quintile",
#   timevar = "Interaction",
#   direction = "wide"
# )

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
  rename(Income = income_15_quant)

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
  count(Income = income_bins_200) |> 
  rename(Killings = n) |> 
  mutate(Killings_Per_Yr = Killings / 6)

summary_tables$bins200_pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_bins_200,
  sum, na.rm = TRUE) %>%
  data.frame(Income = rownames(.), Population = .)

# Median income of each bin
# fatal_enc$joined |>  aggregate(IncomeE ~ Majority + income_bins, FUN = median)

summary_tables$bins200_summary_1 <- 
  left_join(
    x = summary_tables$bins200_table_1,
    y = summary_tables$bins200_pop_table_1,
    by = "Income"
  )

summary_tables$bins200_summary_1 <- summary_tables$bins200_summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000
  )

summary_tables$bins200_summary_1$Income <- as.numeric(summary_tables$bins200_summary_1$Income)

############################################### #
############################################### #
# SCRIPT: by_quintile.R ####
############################################### #
############################################### #

# ############################## #
## Grouped by Race/Ethnicity ####
# ############################## #

# table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles_nolab) |> 
  rename(Killings = n)

summary_tables$race_and_income_pop <- 
  all_tracts$income_population_quintiles_2020 |> 
  aggregate(Total_popE ~ Majority + income_quintiles_nolab, FUN = sum) |> 
  rename(Population = Total_popE)

summary_tables$race_and_income_summary <- 
  left_join(
    x = summary_tables$race_and_income,
    y = summary_tables$race_and_income_pop,
    by = c("Majority", "income_quintiles_nolab")
  ) |> 
  mutate(
    Annualized_Per_10_M =
      Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Income = income_quintiles_nolab,
    Population,
    Killings,
    Annualized_Per_10_M
  ) |> add_row(
    summary_tables$summary_1 |>
      select(-Killings_Per_Yr)
  ) |> na.omit() |> 
  mutate(
    Income = factor(Income, ordered = TRUE),
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

######################################### #
######################################### #
# SCRIPT: rescaling.R ####
######################################### #
######################################### #

summary_tables$race_LUOF_Rescale <-
  full_join(
    x = summary_tables$race_quint_proportions,
    y = summary_tables$quintile_race_proportion,
    by = join_by(Race, Quintile)
  )


summary_tables$race_LUOF_Rescale_s <-
  summary_tables$race_LUOF_Rescale |> 
  arrange(Quintile) |> 
  rename(
    pop_quint_race = Population_in_tracts,
    LUOF_Quint_Race = Killings_by_Quintile_and_Race,
    LUOF_RaceTotal = Killings_Race_Total
  )


summary_tables$race_LUOF_Rescale_s$Rate <- 
summary_tables$race_LUOF_Rescale_s$LUOF_Quint_Race / 
  summary_tables$race_LUOF_Rescale_s$pop_quint_race *
  10000000 / 6

summary_tables$race_LUOF_Rescale_White <-
  summary_tables$race_LUOF_Rescale_s[
    summary_tables$race_LUOF_Rescale_s$Race == "White",]

summary_tables$race_LUOF_Rescale_filter <-
  summary_tables$race_LUOF_Rescale_s[
    summary_tables$race_LUOF_Rescale_s$Race != "White",]

# merge(
  # summary_tables$race_LUOF_Rescale_White[,c('Quintile', 'Prop_living_in_tract')],
  # summary_tables$race_LUOF_Rescale_s,
  # by = "Quintile"
# )

summary_tables$race_LUOF_Rescale_filter$white_prop <-
  summary_tables$race_LUOF_Rescale_White$Prop_living_in_tract[match(
    summary_tables$race_LUOF_Rescale_filter$Quintile,
    summary_tables$race_LUOF_Rescale_White$Quintile
  )]

summary_tables$race_LUOF_Rescale_filter$prop_x_rate <-
  summary_tables$race_LUOF_Rescale_filter$Prop_living_in_tract *
  summary_tables$race_LUOF_Rescale_filter$Rate

summary_tables$race_LUOF_Rescale_rounded <-
  apply(
    summary_tables$race_LUOF_Rescale_filter[, 3:11],
    2,
    FUN = function(x)
      round(x = x, digits = 2)
  ) |> as.data.frame()

summary_tables$race_LUOF_Rescale_rounded <- cbind(
  summary_tables$race_LUOF_Rescale_filter[, 1:2],
  summary_tables$race_LUOF_Rescale_rounded
)


write_csv(
  x = summary_tables$race_LUOF_Rescale_rounded, 
  file = "reweighting.csv")

# summary_tables$race_LUOF_Rescale_filter$rescaled <- 
#   summary_tables$race_LUOF_Rescale_filter$Rate * 
#   (summary_tables$race_LUOF_Rescale_filter$white_prop /
#      summary_tables$race_LUOF_Rescale_filter$Prop_living_in_tract)

rownames(summary_tables$race_LUOF_Rescale_White) <- NULL
summary_tables$race_LUOF_Rescale_White

# rownames(summary_tables$race_LUOF_Rescale_filter) <- NULL
# summary_tables$race_LUOF_Rescale_filter |> 
#   select(-LUOF_Quint_Race, -Killings_Race_Total)



summary_tables$race_LUOF_Rescale_s$rescale <-
summary_tables$race_LUOF_Rescale_s$Rate *
summary_tables$race_LUOF_Rescale_s$Prop_living_in_tract

summary_tables$race_LUOF_Rescale_s

# tapply(summary_tables$race_LUOF_Rescale_s$Prop_living_in_tract, summary_tables$race_LUOF_Rescale_s$Race, sum)


