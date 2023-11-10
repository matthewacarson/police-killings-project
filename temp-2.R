# Plot quintiles of number of race of victim and quintile of census tract ####
# they were killed in
# 
  # fatal_enc$joined |> 
  # filter(
  #   !is.na(income_quintiles) &
  #     race_imputed %in% c(
  #       "African-American/Black", 
  #       "European-American/White", 
  #       "Hispanic/Latino")) |>
  # count(Income = income_quintiles,Race = race_imputed) |> 
  # rename(Killings = n) |>
  # mutate(Killings_Per_Yr = Killings / 6) |> 
  # select(-Killings)

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
    rename(Killings_race_inc = n),
  y =   fatal_enc$joined |> 
    filter(
      !is.na(income_quintiles) &
        race_imputed %in% c(
          "African-American/Black", 
          "European-American/White", 
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
      select(Income, Race, Prop)
  )
# summary_tables$race_and_income_summary

### Reordering Race factors ####
###
summary_tables$quiniles_race_victim$Race <- 
  factor(
    summary_tables$quiniles_race_victim$Race, 
    levels = c("All", "African-American/Black",
               "European-American/White", "Hispanic/Latino"))

ggplot(
  data = summary_tables$quiniles_race_victim,
  aes(x = Race, y = Prop, fill = Income)) +
  geom_bar(
    stat = "identity", 
    position = "dodge", 
    color = 'black', 
    linewidth = 0.01) +
  labs(title = "Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Proportion of Racial Group") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(color = "black"),
    panel.grid.major.x = element_blank())
