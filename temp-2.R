# Plot quintiles of number of race of victim and quintile of census tract ####
# they were killed in
# 
  fatal_enc$joined |> 
  filter(
    !is.na(income_quintiles) &
      race_imputed %in% c(
        "African-American/Black", 
        "European-American/White", 
        "Hispanic/Latino")) |>
  count(Income = income_quintiles,Race = race_imputed) |> 
  rename(Killings = n) |>
  mutate(Killings_Per_Yr = Killings / 6) |> 
  select(-Killings)

summary_tables$quiniles_race_victim <-  
left_join(
  x = fatal_enc$joined |> 
    filter(
      !is.na(income_quintiles) &
        race_imputed %in% c(
          "African-American/Black", 
          "European-American/White", 
          "Hispanic/Latino")) |>
    count(Income = income_quintiles,Race = race_imputed) |> 
    rename(Killings = n) |>
    mutate(Killings_Per_Yr = Killings / 6) |> 
    select(-Killings),
  y =   fatal_enc$joined |> 
    filter(
      !is.na(income_quintiles) &
        race_imputed %in% c(
          "African-American/Black", 
          "European-American/White", 
          "Hispanic/Latino")) |> 
    count(Race = race_imputed) |> 
    rename(Total_Killed = n) |> 
    mutate(Total_Killed_Per_Yr = Total_Killed / 6),
  by = "Race"
) |> mutate(Prop = Killings_Per_Yr / Total_Killed_Per_Yr)

# NExt step is to join summary_tables$summary_1 with the df above
# Than fix the ggplot below so that it references the correct columns/objects
# in the df above

ggplot(
  data = summary_tables$quiniles_race_victim,
  aes(x = Race, y = Killings_Per_Yr, fill = Income)) +
  geom_hline(
    yintercept = seq(0,150, by = 10), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
  geom_bar(
    stat = "identity", 
    position = "dodge", 
    color = 'black', 
    linewidth = 0.01
  ) +
  geom_text(aes(label = round(Annualized_Per_10_M, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, color = "black") +
  labs(title = "Rate of Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Per 10 Million Population Per Year",
       x = "Majority (> 50%)",#"Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Median Household\nIncome in\nCensus Tract") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()
  )
