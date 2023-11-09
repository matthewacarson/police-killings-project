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

race_100_plot <- ggplot(
  summary_tables$bin_table_race, 
  aes(x = Income, y = Killings_Per_Yr, color = race_imputed)
) +
  geom_point() +  
  geom_smooth(method = "loess", formula = y ~ x, se = F) + 
  labs(
    x = "Median Household Income in Census Tracts\n100 Quantiles", 
    y = "Annualized Rate", 
    title = "Lethal Uses of Force",
    color = "Race"
  ) + scale_x_continuous(breaks = seq(0, 100 , by = 5)) + 
  theme_light() +
  scale_color_brewer(palette = "Dark2") +
  theme()

race_100_plot

