# Total population by race 2020
all_tracts$total_pop_by_race <- 
  data.frame(
    Race = c('Black', 'Hispanic/Latino', 'White'),
    race_total_pop = c(
      sum(all_tracts$income_population_quintiles_2020$NH_BlackE),
      sum(all_tracts$income_population_quintiles_2020$Hisp_LatinoE),
      sum(all_tracts$income_population_quintiles_2020$NH_WhiteE)))

# Calculating the total population by Race in each income quintile
all_tracts$race_quint_xtab <- 
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
all_tracts$race_quint_proportions <- 
  left_join(
    x = all_tracts$race_quint_xtab,
    y = all_tracts$total_pop_by_race,
    by = join_by(Race)) |> 
  mutate(Proportion = population / race_total_pop) |> 
  select(-race_total_pop)

################### #
### Save as CSV ####
################### #

all_tracts$race_quint_proportions |> select(-population) |>  
  mutate(Proportion = Proportion * 100) |>
  pivot_wider(names_from = Race, values_from = Proportion) |> 
  write_csv(file = "race_quint_proportions.csv")


############### #
# Plot ####
############### #
ggplot(data = all_tracts$race_quint_proportions,
       aes(x = Race, 
           y = Proportion, 
           fill = Quintile)) +
  # geom_hline(
  #   yintercept = seq(0,100, by = 12.5), 
  #   color = "gray", 
  #   linetype = "dashed", 
  #   linewidth = 0.5
  # ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.85),
    color = 'black', 
    # linewidth = 0.01,
    width = 0.8
  ) + 
  geom_text(
    aes(
      label = 
        paste0(round(Proportion, 2) * 100, "%")),
    position = position_dodge(width = 0.85),
    vjust = -0.4, color = "black", size = 3.5) +
  labs(
    title = "Income Quintile Distribution of Racial or Ethnic Groups",
    subtitle = "Proportion of Each Racial or Ethnic Group Living in Each Census Tract Income Quintile (2020)",
    x = "Racial or Ethnic Group",
    y = "Percentage of Group"
  ) +
  theme_light() +
  # scale_y_continuous(
    # breaks = seq(0,0.35,0.1), 
    # labels = function(x) paste0(x * 100, '%')) + #, name = "Percentage") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"))
# scale_color_brewer(palette = 'Dark3')

ggsave(
  filename = "plots/total_pop_quintiles_race.png",
  dpi = 'retina',
  width = 10.4,
  height = 4.81)
