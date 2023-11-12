# There are some issues with this plot.
# Since there are such small underlying population numbers when dividing into 
# percentiles, the rates sometimes are dramatically high. It is possible that 
# reducing to 50 quantiles would resolve this issue

summary_tables$income_100_majority <- 
  left_join(
    x = fatal_enc$joined |>
          count(Majority, income_bins_100) |>
          rename(
            Killings = n,
            Income = income_bins_100
          ) |> filter() |> 
          na.omit(),
    y = all_tracts$income_population_quintiles_2020 |>
          aggregate(Total_popE ~ Majority + income_bins_100, FUN = sum) |>
          rename(
            Population = Total_popE,
            Income = income_bins_100
          ),
    by = c("Majority", "Income")
  ) |> mutate(
      LUOF_Annualized_Per_10_M =
        Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Income,
    Population,
    Killings,
    LUOF_Annualized_Per_10_M) |> 
  add_row(
    left_join(
      x = all_tracts$income_population_quintiles_2020 |> 
        aggregate(Total_popE ~ income_bins_100, FUN = sum) |> 
        rename(
          Income = income_bins_100,
          Population = Total_popE), 
      y = all_tracts$income_population_quintiles_2020$income_bins_100 |> 
        table() |> as.data.frame() |> 
        rename(
          Income = Var1,
          Killings = Freq),
      by = "Income"
    ) |> mutate(
      Majority = "All"
    ) |> mutate(
      LUOF_Annualized_Per_10_M = Killings / 6 / Population * 1000000
    )
  )

plot$majority_race_100
  ggplot(summary_tables$income_100_majority, aes(x = Income, y = LUOF_Annualized_Per_10_M, color = Majority)) +
  geom_point()
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = TRUE) + 
  labs(
    x = "Median Household Income in Census Tracts\n200 Quantiles", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + scale_x_continuous(breaks = seq(0, 200 , by = 10)) + 
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  theme_light() +
  theme(
    axis.text.x = element_text(color = 'black', vjust = 0.5),
    axis.text.y = element_text(color = 'black')
  )


  
