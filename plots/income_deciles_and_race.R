# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run setup file to bring in data to summarize
source(file = "police-killings-setup.R")


# all_tracts$income_population_quintiles_2020 |> 
# aggregate(Total_popE ~ Majority + income_decile, FUN = sum)

######################################### #
######################################### 

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


# There are some issues with this plot.
# Since there are such small underlying population numbers when dividing into 
# percentiles, the rates sometimes are dramatically high. It is possible that 


ggplot(summary_tables$decile_race_denom, aes(x = as.numeric(Income), y = Annualized_Per_10_M, color = Majority)) +
  geom_point() + 
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) + 
  labs(
    x = "Median Household Income in Census Tracts Deciles", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + 
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  theme_light() +
  scale_x_continuous(breaks = 0:10) +
  theme(
    axis.text.x = element_text(color = 'black', vjust = 0.5),
    axis.text.y = element_text(color = 'black')
  )

ggsave(
  'plots/quintiles_race_denominators/race_only_denom_quint.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)