
# ################################### #
# Plots using race of the victim ####
# ################################### #

## ######################## #
## Using 100 quantiles/percentiles ####
## ######################## #

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

summary_tables$bin_table_race$Income <- as.numeric(summary_tables$bin_table_race$Income)

# Plot quintiles of number of race of victim and quintile of census tract ####
# they were killed in
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

### plots$race_100 ####
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
      rename(Killings_inc_race = n),
    y = fatal_enc$joined |> 
      filter(
        !is.na(income_bins_100) &
          race_imputed %in% c(
            "African-American/Black", 
            "European-American/White", 
            "Hispanic/Latino")) |> 
      count(race_imputed) |> 
      rename(Killings_race_only = n),
    by = "race_imputed"
  ) |> 
  mutate(
    Proportion = Killings_inc_race / Killings_race_only
  )

summary_tables$bin_table_race_proportion$Income <- as.numeric(summary_tables$bin_table_race_proportion$Income)

# Make plot
#
plots$race_100_proportion
ggplot(
  summary_tables$bin_table_race_proportion, 
  aes(x = Income, y = Proportion, color = race_imputed)
) +
  geom_point() +  
  geom_smooth(method = "loess", formula = y ~ x, se = F) + 
  labs(
    x = "Median Household Income in Census Tracts\n100 Quantiles", 
    y = "Proportion of Persons Killed Within Each Racial Group", 
    title = "Lethal Uses of Force",
    subtitle = "Distribution within racial groups",
    color = "Race of Victim"
  ) + scale_x_continuous(breaks = seq(0, 100 , by = 5)) + 
  theme_light() +
  scale_color_brewer(palette = "Dark2") +
  theme()

ggsave(
  plot = plots$race_100_proportion,
  filename = 'plots/race_100_proportion.png', 
  dpi = 'retina', 
  bg = 'white',
  width = 10.4,
  height = 4.81)

# ########################################################## #
## Creating a data frame for tracts without fatal encounters ####
# ########################################################## #
fatal_enc$no_fatal_enc <- 
  all_tracts$population_income2020[
    !all_tracts$population_income2020$GEOID %in% fatal_enc$joined$GEOID,]

fatal_enc$no_fuof_median_income <- median(
  fatal_enc$no_fatal_enc$IncomeE, 
  na.rm = T)

fatal_enc$median_income <- median(
  fatal_enc$joined$IncomeE, 
  na.rm = T)

fatal_enc$median_no_dupes <- 
  median(
    fatal_enc$fatal_enc_unique_id$IncomeE,
    na.rm = T)

## Race of the victim only plot ####
summary_tables$race_freq <- 
  table(fatal_enc$joined$race_imputed) |> 
    as.data.frame() |> 
    rename(Race = Var1) |> 
    filter(Race %in% c(
        "African-American/Black",
        "European-American/White",
        "Hispanic/Latino")
      ) |> mutate(
        Prop = Freq / nrow(fatal_enc$joined),
        Race =
          case_when(
            Race == "African-American/Black" ~ "Black",
            Race == "European-American/White" ~ "White",
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

# write_csv(
#   x = summary_tables$race_freq,
#   file = "race_freq_table.csv")

## Race only plot ####
##
plots$race_proportion_of_total
ggplot() +
  geom_bar(
    data = summary_tables$race_freq,
    aes(
      x = Race,
      y = Prop,
    ),
    stat = 'identity',
    width = 0.5,
    fill = 'lightblue',
    color = 'black'
  ) + 
  labs(
    x = "", 
    y = "Proportion of Total LUOFs",
    title = "Police Lethal Uses of Force",
    subtitle = "Race of Victim. Years: [2015-2020]"
  ) + 
  coord_flip() +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14),
    legend.key.size = unit(8, "mm"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    # plot.margin = unit(c(1, 1, 1, 0.5), "cm"),
    # aspect.ratio = 0.75/2.5
  ) + scale_y_continuous(breaks = seq(0, 0.5, 0.05))

ggsave(plot = plots$race_proportion_of_total,
       filename = 'plots/race_proportion_of_total.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)

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
plots$inc_and_race_victim
ggplot(
  data = summary_tables$quiniles_race_victim,
  aes(x = Race, y = Prop, fill = Income)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.9), 
    color = 'black', 
    width = 0.8,
    linewidth = 0.5) +
  labs(title = "Police Lethal Uses of Force",
       subtitle = "Years: [2015-2020]",
       y = "Proportion LUOF Within Each Racial Group",
       x = "Race of Victim") +
  theme_classic() + 
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14),
    legend.key.size = unit(8, "mm"),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12))
ggsave(plot = plots$inc_and_race_victim,
       filename = 'plots/inc_and_race_victim.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)

summary_tables$quiniles_race_victim[1:20,] |> 
  mutate(Prop = round(Prop,digits = 3) * 100) |> 
  pivot_wider(names_from = Race, values_from = Prop) |> 
  mutate(
    Income =
      c("1st Quintile", "2nd Quintile", "3rd Quintile", 
        "4th Quintile", "5th Quintile")
    ) |> write_csv(file = "quiniles_race_victim.csv")

# Histograms ####
## Histogram prep #### 
### 
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

# Summary table for perentile bins by race ####
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

summary_tables$bin_table_race$Income <- as.numeric(summary_tables$bin_table_race$Income)

## Plot Histograms ####
plots$hist_all_fatat
ggplot() +
  geom_histogram(
    data = all_tracts$population_income2020 |>
      filter(!is.na(IncomeE)),
    aes(x = IncomeE, y = after_stat(density), fill = "All Tracts"),
    alpha = alpha,
    bins = 30
  ) +
  geom_histogram(
    data = fatal_enc$joined,
    aes(x = IncomeE, y = after_stat(density), fill = "Lethal UOF"),
    alpha = alpha,
    bins = 30
  ) +
  geom_vline(
    xintercept = all_tracts$median_income, color = "All Tracts",
    linetype = "dashed", linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = fatal_enc$median_income, color = "Lethal UOF"),
    linetype = "solid", linewidth = 1) +
  labs(
    title = "Census Tract Median Household Income",
    subtitle = "Lethal Uses of Force vs. All Tracts in the US",
    x = "Income",
    y = "Density",
    fill = "Distributions") +
  scale_color_manual(
    name = "Medians",
    values = c("Lethal UOF" = "blue3", "All Tracts" = "red3"),
    guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))
  ) +
  theme_light() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(plot = plots$hist_all_fatal,
       filename = 'plots/hist_all_fatal.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)

### Race Specific Histograms ####
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

summary_tables$quiniles_race_victim <- 
  summary_tables$quiniles_race_victim |> 
  add_row(
    summary_tables$summary_1 |> 
      select(Income, Race = Majority,Killings_quintile = Killings) |> 
      mutate(Total_Killed = sum(Killings_quintile)) |> 
      mutate(Prop = Killings_quintile / Total_Killed) |> 
      select(Income, Race, Prop)
  )

### Plot: proportion of income quintile  ####
summary_tables$quiniles_proportions <- 
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
      count(Income = income_quintiles) |>
      rename(Killed_income = n),
    by = "Income"
  ) |> mutate(
    Prop = Killings_race_inc / Killed_income
  ) |> select(Income, Race, Prop)

summary_tables$quiniles_proportions_2 <- 
  summary_tables$quiniles_proportions |> 
  add_row(
    summary_tables$summary_1 |> 
      select(Income, Race = Majority,Killings_quintile = Killings) |> 
      mutate(Total_Killed = sum(Killings_quintile)) |> 
      mutate(Prop = Killings_quintile / Total_Killed) |> 
      select(Income, Race, Prop)
  )

all_tracts$income_population_LUOF_count <- 
left_join(
  x = all_tracts$income_population_quintiles_2020,
  y = fatal_enc$joined |> 
    count(GEOID) |> 
    rename(LUOF_count = n),
  by = "GEOID"
) |> mutate(LUOF_count =
  case_when(
    is.na(LUOF_count) ~ 0,
    TRUE ~ LUOF_count
  )
)
all_tracts$income_population_LUOF_count$Income10k <- 
  all_tracts$income_population_LUOF_count$IncomeE / 10000

all_tracts$income_population_LUOF_count$Majority <-
  ifelse(
    is.na(all_tracts$income_population_LUOF_count$Majority),
    "No Majority",
    all_tracts$income_population_LUOF_count$Majority)

all_tracts$income_population_LUOF_count$Majority <- 
  factor(all_tracts$income_population_LUOF_count$Majority, 
         levels = c("White", "Hispanic/Latino", "Black", "No Majority"))

################# #
# Checkpoint 4 ####
################# #
# save.image(file = "RData/checkpoint_4.RData")
load(file = "RData/checkpoint_4.RData")
# 5_Regression Models ####
#
## LUOF Frequency counts of each tract ####
# Adding frequency of lethal use of force in each tract to
# all_tracts

all_tracts$income_population_LUOF_count$LUOF_logical <- as.logical(all_tracts$income_population_LUOF_count$LUOF_count)

inc_in_LUOF_tracts <- 
  all_tracts$income_population_LUOF_count$IncomeE[
    all_tracts$income_population_LUOF_count$LUOF_logical]

inc_in_nonLUOF_tracts <- 
  all_tracts$income_population_LUOF_count$IncomeE[
    !all_tracts$income_population_LUOF_count$LUOF_logical]

# qqnorm(inc_in_LUOF_tracts, pch = 16, col = 'dodgerblue')
# qqline(inc_in_LUOF_tracts, col = 'red', lwd = 2)
# 
# qqnorm(inc_in_nonLUOF_tracts, pch = 16, col = 'green4')
# qqline(inc_in_nonLUOF_tracts, col = 'blue', lwd = 2)

# t_test_inc <- 
# t.test(
  # inc_in_LUOF_tracts,
  # inc_in_nonLUOF_tracts)

# wilcox.test(
  # inc_in_LUOF_tracts,
  # inc_in_nonLUOF_tracts)

# write_csv(x = all_tracts$income_population_LUOF_count, file = "income_population_LUOF_count.csv")

lm_income <- 
  lm(IncomeE ~ LUOF_logical + LUOF_logical:Majority,
     data = all_tracts$income_population_LUOF_count)
# plot(x = all_tracts$income_population_LUOF_count$IncomeE,
#      y = all_tracts$income_population_LUOF_count$LUOF_logical)

# abline(lm_income)
# summary(lm_income)
# plot(lm_income)


means_binary <- aggregate(IncomeE ~ LUOF_logical + Majority, data = all_tracts$income_population_LUOF_count, FUN = median) |> 
  pivot_wider(names_from = Majority, values_from = IncomeE)

means_binary_diff <- sapply(means_binary, function(x) x[2] - x[1])

# Overall difference in medians
overall_means_binary <- aggregate(IncomeE ~ LUOF_logical, data = all_tracts$income_population_LUOF_count, FUN = median)

overall_means_binary_diff <- sapply(aggregate(IncomeE ~ LUOF_logical, data = all_tracts$income_population_LUOF_count, FUN = median), FUN = function(x) x[2] - x[1])


# Example assuming all_tracts$income_population_LUOF_count is your dataset
poisson_model <- 
  glm(LUOF_count ~ Income10k:Majority, 
      data = all_tracts$income_population_LUOF_count, 
      family = "poisson")

# Display summary
# summary(poisson_model)

logit_model <- 
  glm(LUOF_logical ~ Majority:Income10k, 
      data = all_tracts$income_population_LUOF_count, 
      family = "binomial")

# summary(logit_model)
# plot(logit_model)

# predictions
logit_prediction_df <- data.frame(
  Majority = rep(levels(all_tracts$income_population_LUOF_count$Majority), each = 4),
  income_quartile = as.factor(rep(seq(1, 4, by = 1), times = 4))
  # Term = names(logit_model$coefficients)[-1]
)

logit_predictions <- data.frame(
  logit_prediction_df,
  Predicted_Probability = predict(
    logit_model, 
    newdata = logit_prediction_df,
    type = 'response'
  )
)

# Predicted probabilities

pivot_wider(logit_predictions, names_from = income_quartile, values_from = Predicted_Probability, names_prefix = "Income_Quartile")

pivot_wider(logit_predictions, names_from = Majority, values_from = Predicted_Probability)

odds_predictions <- data.frame(
  logit_prediction_df,
  Predicted_Odds = exp(predict(logit_model, newdata = logit_prediction_df, type = 'link'))
)

### Probability to odds ####

prob_to_odds <- function(prob, log = FALSE) {
  if (log) {
    log(prob / (1 - prob))
  } else {
    prob / (1 - prob) 
  }
}

odds_to_prob <- function(odds, log) {
  if (!is.logical(log)) {
    stop("Argument 'log' must be TRUE or FALSE")
  }
  if (log) {
    odds <- exp(odds)
    odds / (1 + odds)
  } else {
    odds / (1 + odds)
  }
}

# Frequency/proportion tables ####
prop.table(
table(
  all_tracts$income_population_LUOF_count$income_quartile,
  all_tracts$income_population_LUOF_count$Majority,
  all_tracts$income_population_LUOF_count$LUOF_logical
))

table(
  all_tracts$income_population_LUOF_count$income_quartile,
  all_tracts$income_population_LUOF_count$Majority
)
table(
  all_tracts$income_population_LUOF_count$income_quartile,
  all_tracts$income_population_LUOF_count$Majority,
  all_tracts$income_population_LUOF_count$LUOF_logical, exclude = c(NA, FALSE)
)

all_tracts$income_population_LUOF_count |> 
  group_by(Majority, income_quartile) |> 
  count(LUOF_logical) |> 
  mutate(Proportion = n / sum(n)) |> 
  filter(LUOF_logical) |>
  select(-LUOF_logical, -n) |> 
  na.omit() |> 
  pivot_wider(names_from = Majority, 
              values_from = Proportion)

# pivot_wider(odds_predictions, names_from = Income10k, values_from = Predicted_Odds, names_prefix = "Inc10k*")

# pivot_wider(odds_predictions, names_from = Majority, values_from = Predicted_Odds)

income_population_LUOF_deciles <- read_csv("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github-project/income_population_LUOF_deciles.txt")


# Proportion of tracts that have experienced at least 1 LUOF
income_population_LUOF_deciles |> 
  group_by(Decile, Majority) |> 
  summarise(prop = mean(LUOF_logical)) |> 
  na.omit() |> 
  pivot_wider(names_from = Majority, values_from = prop)



## Boxplots ####
# ggplot() + 
#   geom_boxplot(
#     data = all_tracts$income_population_LUOF_count,
#     aes(x = LUOF_logical, y = IncomeE, color = Majority)) +
#   coord_flip()

plots$boxplot_by_race <- 
  ggplot(
    data = all_tracts$income_population_LUOF_count,
    aes(
      x = Majority,
      y = IncomeE,
      # color = LUOF_logical,
      fill = LUOF_logical
    )) +
    geom_boxplot() +
    coord_flip() +
    theme_cowplot() +
    labs(
      fill = "Lethal Use\nof Force",
      y = "Median Household Income",
      title = "Median Household Income in Census Tracts",
      subtitle = "Tracts with a LUOF vs. Tracts without a LUOF.")

ggsave(plot = plots$boxplot_by_race,
       filename = 'plots/boxplot_by_race.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)

## Density plot by race and LUOF ####
## (un-weighted income)
plots$density_by_race <-
  ggplot(
    data = all_tracts$income_population_LUOF_count, 
    aes(x = IncomeE, fill = LUOF_logical, color = LUOF_logical)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~Majority, ) +
    # theme_light() +
    # scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "Census Tract Median Household Income",
    subtitle = "Tracts with a lethal use of force vs. Tracts without a lethal use of force [2015-2020].",
    # caption = "[2015-2020]",
    # tag = "dgh",
    x = "Median Household Income",
    y = "Density") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1", name = "Lethal Use\nof Force") +
  scale_color_brewer(palette = "Set1", name = "Lethal Use\nof Force") +
  # scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
  theme(
    axis.text.x = element_text(
      angle = 90, vjust = 0.5, color = 'black'),
    axis.text.y = element_text(
      angle = 90, hjust = 0.5, color = 'black'),
    # plot.caption = element_text(
    #   vjust = 100, color = 'black'),
    plot.title = element_text(color = 'black', face = "bold"),
    plot.subtitle = element_text(color = 'black'),
    axis.title.x = element_text(color = 'black'),
    axis.title.y = element_text(color = 'black'),
    legend.text = element_text(color = 'black'),
    legend.title = element_text(color = 'black'),
    strip.text = element_text(color = 'black', size = 12)
  )

ggsave(plot = plots$density_by_race,
       filename = 'plots/density_by_race.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)
## Histogram by race and LUOF ####
## (un-weighted income)
plots$hist_unweighted_by_race <- 
  ggplot(
    data = all_tracts$income_population_LUOF_count, 
    aes(x = IncomeE, y = after_stat(density), fill = LUOF_logical)) +
    geom_histogram(
      alpha = 0.5,
      position = "identity") +
    facet_wrap(~Majority) +
    theme_light() +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(plot = plots$hist_unweighted_by_race,
       filename = 'plots/hist_unweighted_by_race.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)

## Histogram using "un-weighted" Income for LUOF ####
median_no_LUOF <- median(
    all_tracts$income_population_LUOF_count$IncomeE[
      !all_tracts$income_population_LUOF_count$LUOF_logical], na.rm = T)
median_LUOF <- median(
    all_tracts$income_population_LUOF_count$IncomeE[
      all_tracts$income_population_LUOF_count$LUOF_logical], na.rm = T)


plots$hist_unweighted <- 
  ggplot() +
    geom_histogram(
      data = all_tracts$income_population_LUOF_count, 
      aes(x = IncomeE, y = after_stat(density), fill = LUOF_logical),
      alpha = 0.4,
      position = "identity") +
    geom_vline(
      aes(xintercept = median_LUOF, color = TRUE),
      linetype = "solid", linewidth = 1, alpha = 1) +
    geom_vline(
      aes(xintercept = median_no_LUOF, color = FALSE),
      linetype = "solid", linewidth = 1, alpha = 1) +
    labs(
      title = "Census Tract Median Household Income",
      subtitle = "Tracts with a lethal use of force vs. Tracts without a lethal use of force [2015-2020].",
      # caption = "[2015-2020]",
      # tag = "dgh",
      x = "Income",
      y = "Density") +
    theme_light() +
    scale_fill_brewer(palette = "Set1", name = "Lethal Use\nof Force") +
    scale_color_brewer(palette = "Set1", name = "Lethal Use\nof Force") +
    scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 0.5, color = 'black'),
      axis.text.y = element_text(
        angle = 90, hjust = 0.5, color = 'black'),
      # plot.caption = element_text(
      #   vjust = 100, color = 'black'),
      plot.title = element_text(color = 'black', face = "bold"),
      plot.subtitle = element_text(color = 'black'),
      axis.title.x = element_text(color = 'black'),
      axis.title.y = element_text(color = 'black'),
      legend.text = element_text(color = 'black'),
      legend.title = element_text(color = 'black')
      )
ggsave(plot = plots$hist_unweighted,
       filename = 'plots/hist_unweighted.png', 
       dpi = 'retina', 
       bg = 'white',
       width = 10.4,
       height = 4.81)

################# #
# Checkpoint 5 ####
################# #
# save.image(file = "RData/checkpoint_5.RData")

################# #
# Checkpoint 5 ####
################# #

load(file = "RData/checkpoint_5.RData")

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

# Calculate the proportion of each racial group liiving in each income
# quintile
all_tracts$race_quint_proportions <- 
  left_join(
    x = all_tracts$race_quint_xtab,
    y = all_tracts$total_pop_by_race,
    by = join_by(Race)) |> 
  mutate(Proportion = population / race_total_pop) |> 
  select(-race_total_pop)

# Plot
ggplot(data = all_tracts$race_quint_proportions,
       aes(x = Race, 
           y = Proportion, 
           fill = Quintile)) +
  geom_hline(
    yintercept = seq(0,100, by = 12.5), 
    color = "gray", 
    linetype = "dashed", 
    linewidth = 0.5
  ) +
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
  scale_y_continuous(
    breaks = seq(0,0.35,0.1), 
    labels = function(x) paste0(x * 100, '%')) + #, name = "Percentage") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"))
  # scale_color_brewer(palette = 'Dark3')

ggsave(
  filename = "total_pop_race_income_quintile_distribution.png",
  dpi = 'retina')

