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
    Race = c('Black', 'Hispanic/Latino', 'White'),
    race_total_pop = c(
      sum(all_tracts$income_population_quintiles_2020$NH_BlackE),
      sum(all_tracts$income_population_quintiles_2020$Hisp_LatinoE),
      sum(all_tracts$income_population_quintiles_2020$NH_WhiteE)))

############################################### #
############################################### #
# SCRIPT: race_of_victim.R ####
############################################### #
############################################### #

############################################### #
## killings annual -- victim proportion ####
# Proportion of racial group victims killed
# in that income quintile
# BY: quintile
# BY: race of victim
############################################### #


# This calcualtes killings by race of victim and income quintile ONLY
## plots$race_100 ####
summary_tables$quintile_race_proportion <- 
  left_join(
    x = 
      fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles_nolab) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(Quintile = income_quintiles_nolab, Race = race_imputed) |> 
      rename(Killings_by_Quintile_and_Race = n) |>
      mutate(Killings_Per_Yr = Killings_by_Quintile_and_Race / 6),
    y = 
      fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles_nolab) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(race_imputed) |> 
      rename(Killings_Race_Total = n, Race = race_imputed),
    
    by = "Race") |> 
  mutate(
    Proportion_LUOF = Killings_by_Quintile_and_Race / Killings_Race_Total)


######################################################## #
## PERCENTILES: killings annual -- victim proportion ####
# Proportion of racial group victims killed
# in that income quintile
# BY: centiles
# BY: race of victim
######################################################## #

summary_tables$bin_table_race_proportion <- 
  left_join(
    x = fatal_enc$joined |> 
      filter(
        !is.na(income_bins_100) &
          race_imputed %in% c(
            "Black", 
            "White", 
            "Hispanic/Latino")) |> 
      count(Percentile = income_bins_100, race_imputed) |> 
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
    Proportion = Killings_inc_race / Killings_race_only,
    Killings_Per_Yr = Killings_inc_race / 6
  )

summary_tables$bin_table_race_proportion$Percentile <- as.integer(summary_tables$bin_table_race_proportion$Percentile)

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
      count(Quintile = income_quintiles_nolab,Race = race_imputed) |>
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
  ) |> select(Quintile, Race, Prop)

summary_tables$quiniles_race_victim <- 
  summary_tables$quiniles_race_victim |> 
  add_row(
    summary_tables$quintiles_only |> 
      select(Quintile, Race = Majority,Killings_quintile = Killings) |> 
      mutate(Total_Killed = sum(Killings_quintile)) |> 
      mutate(Prop = Killings_quintile / Total_Killed) |> 
      select(Quintile, Race, Prop) |> 
      mutate(Quintile = factor(Quintile, ordered = T))
  )
# summary_tables$race_and_income_summary

### Reordering Race factors ####
###
summary_tables$quiniles_race_victim$Race <- 
  factor(
    summary_tables$quiniles_race_victim$Race, 
    levels = c("All", "Black",
               "White", "Hispanic/Latino"))

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
    values_to = "Population_in_tracts") |> 
  aggregate(Population_in_tracts ~ Quintile + Race, FUN = sum)

############################################################### #
## proportion racial group population ####
## BY: income quintile
## BY: Majority
############################################################### #
summary_tables$race_quint_proportions <- 
  left_join(
    x = summary_tables$race_quint_xtab,
    y = summary_tables$total_pop_by_race,
    by = join_by(Race)) |> 
  mutate(Race_proportion_quintile = Population_in_tracts / race_total_pop)


######################################################## #
# Slicing by victim-race / quintile / majority ####
######################################################## #
# I'm not going to use this because it slices the numbers too small

# fatal_enc$joined |> 
#   filter(
#     !is.na(income_quintiles_nolab) &
#       race_imputed %in% c(
#         "Black", 
#         "White", 
#         "Hispanic/Latino") &
#       !is.na(Majority)
#     ) |> 
#   count(
#     Quintile = income_quintiles_nolab, 
#     Race = race_imputed,
#     Majority) |> 
#   rename(Killings_by_Quintile_and_Race = n) |>
#   mutate(Killings_Per_Yr = Killings_by_Quintile_and_Race / 6)

summary_tables$race_LUOF_counterfactual <-
  full_join(
    x = summary_tables$race_quint_proportions,
    y = summary_tables$quintile_race_proportion,
    by = join_by(Race, Quintile)
  ) |>   rename(
    # income quintile of tract
    quintile = Quintile, 
    
    # race of victim
    race = Race,
    
    # count of that race living in that income quintile
    pop_quint_race = Population_in_tracts, 
    
    # total US population of that race
    total_US_pop_race = race_total_pop,
    
    # proportion of that racial group that lives in that income quintile
    prop_of_race_in_quintile = Race_proportion_quintile,
    
    # count of LUOF of that race occurring within that income quintile
    LUOF_count_quint_race = Killings_by_Quintile_and_Race,
    
    # total count of persons of that race killed
    LUOF_race_total_US = Killings_Race_Total,
    
    # proportion of all persons killed of that race within that income quintile
    prop_of_LUOF_by_race = Proportion_LUOF
  )
