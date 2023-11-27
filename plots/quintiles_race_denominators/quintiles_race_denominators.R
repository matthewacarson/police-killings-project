# ############################## #
# Grouped by Race/Ethnicity ####
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

##################### #
# GGplot by race ####
##################### #

ggplot(
    summary_tables$race_income_summary_race_denom
    ,aes(x = Majority, y = Annualized_Per_10_M, fill = Income)) +
  geom_hline(
    yintercept = seq(0,100, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5,
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    width = 0.75
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Majority (> 50%)",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Median Household\nIncome in\nCensus Tract") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()
  )
ggsave(
  'plots/quintiles_race_denominators/race_only_denom_race.png',
  dpi = 'retina')
# ############################### #
# Grouped by Income Quintile ####
# ############################### #
ggplot(
  summary_tables$race_income_summary_race_denom, 
    aes(x = Income, y = Annualized_Per_10_M, fill = Majority)) +
  geom_hline(
    yintercept = seq(0,100, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85), 
    color = 'black', 
    # linewidth = 0.01,
    width = 0.75
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.85),
            vjust = -0.4, color = "black", size = 3) +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Median Household Income in Census Tract",
       fill = "Majority\n(> 50%)") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
    # panel.grid.major.x = element_blank()
  )
ggsave(
  'plots/quintiles_race_denominators/race_only_denom_quint.png',
  dpi = 'retina')
