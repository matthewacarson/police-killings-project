# Plot quintiles of number of race of victim and quintile of census tract ####
# they were killed in
# 

summary_tables$quiniles_race_victim <-  
  fatal_enc$joined |> 
  filter(
    !is.na(income_quintiles) &
      race_imputed %in% c(
        "African-American/Black", 
        "European-American/White", 
        "Hispanic/Latino")) |> 
  count(Income = income_quintiles, race_imputed) |> 
  rename(Killings = n) |>
  mutate(Killings_Per_Yr = Killings / 6) |> 
  select(-Killings)


summary_tables$quintile_race_proportion <- 
  left_join(
    x = fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles) &
          race_imputed %in% c(
            "African-American/Black", 
            "European-American/White", 
            "Hispanic/Latino")) |> 
      count(Income = income_quintiles, race_imputed) |> 
      rename(Killings_by_Quintile_and_Race = n),
    y = fatal_enc$joined |> 
      filter(
        !is.na(income_quintiles) &
          race_imputed %in% c(
            "African-American/Black", 
            "European-American/White", 
            "Hispanic/Latino")) |> 
      count(race_imputed) |> 
      rename(Killings_Race_Total = n),
    by = "race_imputed"
  ) |> 
  mutate(
    Proportion = Killings_by_Quintile_and_Race / Killings_Race_Total
  )

# SUmmary table for perentile bins by race ####

summary_tables$bin_table_race <-  
  fatal_enc$joined |> 
  filter(
    !is.na(income_bins_100) &
      race_imputed %in% c(
        "African-American/Black", 
        "European-American/White", 
        "Hispanic/Latino")) |> 
  count(Income = income_bins_100, race_imputed) |> 
  rename(Killings = n) |>
  mutate(Killings_Per_Yr = Killings / 6)

# summary_tables$bin_pop_table_race <- tapply(
#   all_tracts$income_population_quintiles_2020$Total_popE, 
#   all_tracts$income_population_quintiles_2020$income_bins,
#   sum, na.rm = TRUE) %>% 
#   data.frame(Income = rownames(.), Population = .)

# summary_tables$bin_summary_race <- 
#   left_join(
#     x = summary_tables$bin_table_race,
#     y = summary_tables$bin_pop_table_race,
#     by = "Income"
#   )

# summary_tables$bin_summary_1 <- summary_tables$bin_summary_1 |> 
#   mutate(
#     Annualized_Per_10_M =
#       Killings_Per_Yr / Population * 10000000
#   )

summary_tables$bin_table_race$Income <- as.numeric(summary_tables$bin_table_race$Income)

plot_race_100 <- ggplot(
  summary_tables$bin_table_race, 
  aes(x = Income, y = Killings_Per_Yr, color = race_imputed)
) +
  geom_point() +  
  geom_smooth(method = "loess", formula = y ~ x, se = F) + 
  labs(
    x = "Median Household Income in Census Tracts\n100 Quantiles", 
    y = "Number of Persons Killed", 
    title = "Lethal Uses of Force",
    color = "Race of Victim"
  ) + scale_x_continuous(breaks = seq(0, 100 , by = 5)) + 
  theme_light() +
  scale_color_brewer(palette = "Dark2") +
  theme()

plot_race_100


summary_tables$bin_table_race_proportion <- 
left_join(
  x = fatal_enc$joined |> 
        filter(
          !is.na(income_bins_100) &
            race_imputed %in% c(
              "African-American/Black", 
              "European-American/White", 
              "Hispanic/Latino")) |> 
        count(Income = income_bins_100, race_imputed) |> 
        rename(Killings = n) |> 
        mutate(Killings_Per_Yr = Killings / 6) |> 
        select(-Killings),
  y = fatal_enc$joined |> 
        filter(
          !is.na(income_bins_100) &
            race_imputed %in% c(
              "African-American/Black", 
              "European-American/White", 
              "Hispanic/Latino")) |> 
        count(race_imputed) |> 
        rename(Killings = n) |> 
        mutate(Killings_Per_Yr = Killings / 6) |> 
          select(-Killings),
  by = "race_imputed"
) |> 
  mutate(
    Proportion = Killings_Per_Yr.x / Killings_Per_Yr.y
  )

summary_tables$bin_table_race_proportion$Income <- as.numeric(summary_tables$bin_table_race_proportion$Income)

# Make plot
#
plot_race_100_proportion <- ggplot(
  summary_tables$bin_table_race_proportion, 
  aes(x = Income, y = Proportion, color = race_imputed)
) +
  geom_point() +  
  geom_smooth(method = "loess", formula = y ~ x, se = F) + 
  labs(
    x = "Median Household Income in Census Tracts\n100 Quantiles", 
    y = "Proportion of Persons Killed", 
    title = "Lethal Uses of Force",
    subtitle = "Distribution within racial groups",
    color = "Race of Victim"
  ) + scale_x_continuous(breaks = seq(0, 100 , by = 5)) + 
  theme_light() +
  scale_color_brewer(palette = "Dark2") +
  theme()

plot_race_100_proportion

# Overall histogram
#
fatal_enc_hist <- 
fatal_enc$joined |> 
  filter(!is.na(IncomeE)) |> 
  ggplot(
    data = _,
    aes(x = IncomeE)) +
  geom_histogram(fill = 'red', col = 'black')

fatal_enc_hist

hist_all_tracts <- 
all_tracts$population_income2020 |> 
  filter(!is.na(IncomeE)) |> 
  ggplot(
    data = _,
    aes(x = IncomeE)) +
  geom_histogram(fill = 'blue', col = 'black')

hist_all_tracts

# income_list <- 
#   list(
#     all_income = all_tracts$population_income2020$IncomeE[
#       !is.na(all_tracts$population_income2020$IncomeE)
#     ],
#     fatal_enc_income = fatal_enc$joined$IncomeE[
#       !is.na(fatal_enc$joined$IncomeE)
#     ]) 
# ########################################################## #
## Creating a data frame for tracts without fatal encounters ####
# ########################################################## #
fatal_enc$no_fatal_enc <- 
  all_tracts$population_income2020[
  !all_tracts$population_income2020$GEOID %in% fatal_enc$joined$GEOID,
]

fatal_enc$no_fuof_median_income <- median(
  fatal_enc$no_fatal_enc$IncomeE, 
  na.rm = T
)

fatal_enc$median_income <- median(
  fatal_enc$joined$IncomeE, 
  na.rm = T
)

fatal_enc$median_no_dupes <- 
  median(
    fatal_enc$fatal_enc_unique_id$IncomeE,
    na.rm = T
  )

# Fatal encounters: remove duplicated GEOIDs
# This is create a data frame that will include every tract that has had
# at least one incident, rather than every incident.
# This will make a better comparison between tracts with any incidents 
# (regardless of how many) vs. tracts without any incidents
fatal_enc$fatal_enc_unique_id <- 
fatal_enc$joined[
  !is.na(fatal_enc$joined$IncomeE) & !duplicated(fatal_enc$joined$GEOID),
]
alpha <- 0.5

hist_unique <- 
  ggplot() +
  geom_histogram(
    data = fatal_enc$fatal_enc_unique_id,
    aes(
      x = IncomeE,
      y = after_stat(density),
      fill = "Lethal UOF"
    ),
    alpha = alpha,
    bins = 30
  ) +
  geom_histogram(
    data = fatal_enc$no_fatal_enc |> 
      filter(!is.na(IncomeE)),
    aes(
      x = IncomeE, 
      y = after_stat(density),
      fill = "No Lethal UOF"
    ),
    alpha = alpha,
    bins = 30
  ) + 
  geom_vline(
    aes(
      xintercept = fatal_enc$no_fuof_median_income, 
      color = "No Lethal UOF"
    ), 
    linetype = "dashed", linewidth = 1
  ) +
  geom_vline(
    aes(
      xintercept = fatal_enc$median_no_dupes, 
      color = "Lethal UOF"
    ), 
    linetype = "solid", linewidth = 1) +
  labs(
    # title = "Income",
    x = "Income",
    y = "Density",
    fill = "Distributions") +
  scale_color_manual(
    name = "Medians", 
    values = c("No Lethal UOF" = "blue3", "Lethal UOF" = "red3"),
    guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))
  ) +
  theme_light() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## ######################## #
## Not using plots below ####
## ####################### #

# Overall histogram
#
# fatal_enc_hist <- 
#   fatal_enc$joined |> 
#   filter(!is.na(IncomeE)) |> 
#   ggplot(
#     data = _,
#     aes(x = IncomeE)) +
#   geom_histogram(fill = 'red', col = 'black')

# Histogram for all tracts

# hist_all_tracts <- 
#   all_tracts$population_income2020 |> 
#   filter(!is.na(IncomeE)) |> 
#   ggplot(
#     data = _,
#     aes(x = IncomeE)) +
#   geom_histogram(fill = 'blue', col = 'black')

# hist_all_fatal <- 
#   ggplot() +
#   geom_histogram(
#     data = all_tracts$population_income2020 |> 
#       filter(!is.na(IncomeE)),
#     aes(
#       x = IncomeE, 
#       y = after_stat(density),
#       fill = "All Tracts"
#     ),
#     alpha = alpha,
#     bins = 30
#   ) + 
#   geom_histogram(
#     data = fatal_enc$joined,
#     aes(
#       x = IncomeE,
#       y = after_stat(density),
#       fill = "Lethal UOF"
#     ),
#     alpha = alpha,
#     bins = 30
#   ) +
#   geom_vline(
#     aes(
#       xintercept = all_tracts$median_income, 
#       color = "All Tracts"
#     ), 
#     linetype = "dashed", linewidth = 1
#   ) +
#   geom_vline(
#     aes(
#       xintercept = fatal_enc$median_income, 
#       color = "Lethal UOF"
#     ), 
#     linetype = "solid", linewidth = 1) +
#   labs(
#     # title = "Income",
#     x = "Income",
#     y = "Density",
#     fill = "Distributions") +
#   scale_color_manual(
#     name = "Medians", 
#     values = c("Lethal UOF" = "blue3", "All Tracts" = "red3"),
#     guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))
#   ) +
#   theme_light() +
#   scale_fill_brewer(palette = "Set1") +
#   scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))





