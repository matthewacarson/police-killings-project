

# Note: Instead of running from the top (many operations take too long, start at 3_Summary Tables)

# setwd(
#   "C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")
# 1_all_tracts ####

# Load Libraries ####

# library(tidycensus)
# library(sf)
# library(tidyverse)

# Download and clean tract income ####

# all_tracts <- list()

## Download 2020 tract data ####

# all_tracts$population_income2020 <- get_acs(
#   geography = "tract",
#   variables = c(
#     Income = "B19013_001",
#     Total_pop = "B03002_001",
#     NH_White = "B03002_003",
#     NH_Black = "B03002_004",
#     NH_Asian = "B03002_006",
#     Hisp_Latino = "B03002_012"
#   ),
#   state = c(01, 02, 04, 05, 06, 08, 09, 10:13, 15:42, 44:51, 53:56),
#   survey = "acs5",
#   year = 2020,
#   geometry = TRUE,
#   output = "wide",
#   cache_table = TRUE
# )

## Download 2019 data ####

# all_tracts$population_income2019 <- get_acs(
#   geography = "tract",
#   variables = c(
#     Income = "B19013_001",
#     Total_pop = "B03002_001",
#     NH_White = "B03002_003",
#     NH_Black = "B03002_004",
#     NH_Asian = "B03002_006",
#     Hisp_Latino = "B03002_012"
#   ),
#   state = c(01, 02, 04, 05, 06, 08, 09, 10:13, 15:42, 44:51, 53:56),
#   survey = "acs5",
#   year = 2019,
#   geometry = TRUE,
#   output = "wide",
#   cache_table = TRUE
# )

# Filter tracts with NA income values ####

# all_tracts$income_population_quintiles_2020 <- st_drop_geometry(all_tracts$population_income2020)

# all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020[!is.na(all_tracts$income_population_quintiles_2020$IncomeE),]

## Add quantile data to all_tracts ####

### Quintiles ####
all_tracts$quintiles_2020 <- quantile(
  all_tracts$population_income2020$IncomeE, 
  probs = seq(0, 1, by = 0.2), na.rm = TRUE
)

all_tracts$quintile_text <- c(
  "1st Quintile",
  "2nd Quintile",
  "3rd Quintile",
  "4th Quintile",
  "5th Quintile"
)

all_tracts$income_population_quintiles_2020 <- 
  all_tracts$income_population_quintiles_2020 |> 
  mutate(income_quintiles = cut(
    IncomeE,
    labels = all_tracts$quintile_text,
    breaks = all_tracts$quintiles_2020, 
    include.lowest = TRUE,
    # labels = FALSE # Setting labels to FALSE gives numeric labels to bins
  ))

### Divide into 200 quantiles ####
all_tracts$two_hundred <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE, 
  probs = seq(0, 1, by = 1/200), na.rm = TRUE
)

all_tracts$income_population_quintiles_2020 <- 
  all_tracts$income_population_quintiles_2020 |> 
  mutate(income_bins = cut(
    IncomeE,
    breaks = all_tracts$two_hundred, # 201 points create 200 bins
    include.lowest = TRUE,
    labels = seq(1,200)
    # labels = FALSE # Setting labels to FALSE gives numeric labels to bins
  ))

# Divide into 100 quantiles
all_tracts$one_hundred <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE, 
  probs = seq(0, 1, by = 1/100), na.rm = TRUE
)

all_tracts$income_population_quintiles_2020 <- 
  all_tracts$income_population_quintiles_2020 |> 
  mutate(income_bins_100 = cut(
    IncomeE,
    breaks = all_tracts$one_hundred, 
    include.lowest = TRUE,
    labels = seq(1,100)
    # labels = FALSE # Setting labels to FALSE gives numeric labels to bins
  ))

## Designate tracts as racial majorities ####

all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020 %>%
  mutate(
    NH_WhiteP = NH_WhiteE / Total_popE,
    NH_BlackP = NH_BlackE / Total_popE,
    NH_AsianP = NH_AsianE / Total_popE,
    Hisp_LatinoP = Hisp_LatinoE / Total_popE,
    Majority = case_when(
      NH_WhiteP > 0.5 ~ 'White',
      NH_BlackP > 0.5 ~ 'Black',
      # NH_AsianP > 0.5 ~ 'A',
      Hisp_LatinoP > 0.5 ~ 'Hispanic/Latino'))

## Identifying majority race/ethnicity with 2019 data. ####

all_tracts$income_population_quintiles_2019 <- all_tracts$population_income2019 %>% 
  mutate(
    NH_WhiteP = NH_WhiteE / Total_popE,
    NH_BlackP = NH_BlackE / Total_popE,
    NH_AsianP = NH_AsianE / Total_popE,
    Hisp_LatinoP = Hisp_LatinoE / Total_popE,
    Majority = case_when(
      NH_WhiteP > 0.5 ~ 'White',
      NH_BlackP > 0.5 ~ 'Black',
      # NH_AsianP > 0.5 ~ 'A',
      Hisp_LatinoP > 0.5 ~ 'Hispanic/Latino'))

all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020 %>% 
  mutate(
    Majority_Quintile = paste(Majority, income_quintiles, sep = "_")
  )

# 2_fatal_enc ####

# fatal_enc <- list()
# fatal_enc$backup_not_clean <- read_csv(
#   "https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/export?format=csv", 
#   col_types = cols(
#     `Date of injury resulting in death (month/day/year)` = col_date(format = "%m/%d/%Y"),
#     Name = col_skip(),
#     Age = col_skip(),
#     Gender = col_skip(),
#     `URL of image (PLS NO HOTLINKS)` = col_skip(),
#     `Location of injury (address)` = col_skip(),
#     `Full Address` = col_skip(),
#     `UID Temporary` = col_skip(),
#     `Name Temporary` = col_skip(),
#     `Description Temp` = col_skip(),
#     `URL Temp` = col_skip(),
#     `Brief description` = col_skip(),
#     `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS` = col_skip(),
#     `Supporting document link` = col_skip(),
#     `Foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS` = col_skip(),
#     ...33 = col_skip(),
#     ...34 = col_skip(),
#     `Unique ID formula` = col_skip(),
#     `Unique identifier (redundant)` = col_skip()
#   )
# )

## Initial Cleaning ####
fatal_enc$initial_clean <-
  fatal_enc$backup_not_clean %>%
  select(
    # Select columns to keep and rename
    c(unique_id = `Unique ID`,
      race = `Race`,
      race_imputed = `Race with imputations`,
      imputation_probability = `Imputation probability`,
      date = `Date of injury resulting in death (month/day/year)`,
      city = `Location of death (city)`,
      state = `State`,
      zip = `Location of death (zip code)`,
      county = `Location of death (county)`,
      latitude = `Latitude`,
      longitude = `Longitude`,
      agencies = `Agency or agencies involved`,
      highest_force = `Highest level of force`,
      armed = `Armed/Unarmed`,
      weapon = `Alleged weapon`,
      aggressive = `Aggressive physical movement`,
      fleeing = `Fleeing/Not fleeing`,
      intended_use_of_force = `Intended use of force (Developing)`,
    )
  ) |>    ### Remove spacer (this wasn't an observation) ####
# filter(date != "12/31/1999") |> 
### Remove non-numeric characters from the latitude column ####
mutate(latitude = as.numeric(gsub("[^0-9.-]", "", latitude))) |> 
  filter(
    highest_force %in% c(
      "Gunshot", 
      "Tasered", 
      "Beaten/Bludgeoned with instrument",
      "Asphyxiated/Restrained", 
      "Asphyxiation/Restrained",
      "Asphyxiation/Restrain", 
      "Less-than-lethal force",
      "Stabbed", 
      "Chemical agent/Pepper spray", 
      "Restrain/Asphyxiation"
    )
  ) |>  filter(intended_use_of_force != "Suicide")

fatal_enc$initial_clean$date <- as.Date(fatal_enc$initial_clean$date, format = "%m/%d/%Y")

fatal_enc$initial_clean <- fatal_enc$initial_clean |> 
  filter(year(date) >= 2015 & year(date) <= 2020)

# Fix Hispanic/Latino (uppercase "I")
# 
fatal_enc$initial_clean$race_imputed <- ifelse(
  fatal_enc$initial_clean$race_imputed == "HIspanic/Latino",
  "Hispanic/Latino", 
  fatal_enc$initial_clean$race_imputed
)

## Adding sf object to fatal_enc (combined lat/long) ####
fatal_enc$initial_clean <- 
  fatal_enc$initial_clean %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = "NAD83")

## Join the fatal encounters data set with census data
### Add GEOID to the fatal encounters data set

# r Add GEOID to the fatal encounters data set
fatal_enc$initial_clean_geoid <- 
  st_join(
    x = fatal_enc$initial_clean,
    y = all_tracts$population_income2020 %>% select(GEOID),
    join = st_within
  )

fatal_enc$initial_clean_geoid <- st_drop_geometry(fatal_enc$initial_clean_geoid)

# Remove duplicated rows (unique ids that appear twice)
fatal_enc$initial_clean_geoid <- 
  fatal_enc$initial_clean_geoid[
    !duplicated(fatal_enc$initial_clean_geoid$unique_id) | 
      duplicated(fatal_enc$initial_clean_geoid, fromLast = TRUE),
  ]

fatal_enc$initial_clean_geoid$imputation_probability <- 
  as.numeric(fatal_enc$initial_clean_geoid$imputation_probability)

fatal_enc$joined <- 
  left_join(
    x = fatal_enc$initial_clean_geoid,
    y = all_tracts$income_population_quintiles_2020,
    by = "GEOID"
  )

# 3_Summary Tables ####
summary_tables <- list()

summary_tables$fatal_enc_table_1 <-  fatal_enc$joined %>%
  count(Income = income_quintiles) %>% rename(Killings = n) %>% 
  filter(!is.na(Income)) %>% mutate(Killings_Per_Yr = Killings / 6)

summary_tables$pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_quintiles,
  sum, na.rm = TRUE) %>% 
  data.frame(Income = rownames(.), Population = .)

summary_tables$summary_1 <- 
  left_join(
    x = summary_tables$fatal_enc_table_1,
    y = summary_tables$pop_table_1,
    by = "Income"
  )

summary_tables$summary_1$Majority <- "All"

summary_tables$summary_1 <- summary_tables$summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000
  )

# ######################################################################### #
# 4_Begin ggplot ####
# ######################################################################### #

plot <- list()
# Income Quintiles only ####
## Create quintile bar plot ####
plot$income_quintiles_only <- 
summary_tables$summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Income)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "Annual Rate Per 10 Million Population", 
    y = "Tract Median Household Income Quintiles", 
    # title = "Police Lethal Uses of Force",
    # subtitle = "Years: [2015-2020]"
  ) + 
  # coord_flip() +
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.5, color = "black"
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(angle = 90, hjust = 0.5, color = 'black')
  ) +
  scale_y_discrete(
    labels = c(
      "1st", 
      "2nd", 
      "3rd", 
      "4th", 
      "5th"
    )
  ) + scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70))
    # axis.title = element_text(size = 20),
    # plot.title = element_text(size = 30),
    # plot.subtitle = element_text(size = 20)



# Majority Race ####

summary_tables$majority_table_1 <-  fatal_enc$joined %>%
  count(Majority = Majority) %>% rename(Killings = n) %>% 
  filter(!is.na(Majority)) %>% mutate(Killings_Per_Yr = Killings / 6)

summary_tables$majority_pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$Majority,
  sum, na.rm = TRUE) %>% 
  data.frame(Majority = rownames(.), Population = .)

summary_tables$majority_summary_1 <- 
  left_join(
    x = summary_tables$majority_table_1,
    y = summary_tables$majority_pop_table_1,
    by = "Majority"
  )

summary_tables$majority_summary_1 <- 
  summary_tables$majority_summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000
  )

# Majority Race Plot ####
plot$majority_race_only <- 
summary_tables$majority_summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Majority)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "",
    # x = "Annual Rate Per 10 Million Population", 
    y = "Tracts With Majority One Race (>50%)", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.5, color = "black"
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(angle = 90, hjust = 0.5, color = 'black')) +
  scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  scale_y_discrete(labels = c("Black", "Latino", "White"))
  # coord_flip()
  # scale_x_continuous(breaks = seq(0,70,10))
# plot$majority_race_only

## Combining plots ####

# library(patchwork)
# plot$income_quintiles_only + plot$majority_race_only

library(cowplot)

plot$cp_race_income_separate <- 
plot_grid(
  plot$majority_race_only, 
  plot$income_quintiles_only, 
  # labels = c("A", "B"), 
  nrow = 2,
  rel_heights = c(1, 1.25)
)

ggsave(
  filename = "cp_race_income_separate.png",
  plot = plot$cp_race_income_separate,
  width = 2300*1.5,
  height = 2000*1.5,
  units = "px",
  dpi = 320,
  # scale = 1.5
)

ggsave(
  filename = "cp_race_income_separate.pdf",
  plot = plot$cp_race_income_separate,
  width = 2300*1.5,
  height = 2000*1.5,
  units = "px",
  # dpi = 320,
  # scale = 1.5
)

# Individual plot: Income Quintiles only ####
plot$income_quintiles_only_ind <- 
  summary_tables$summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Income)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "Annual Rate Per 10 Million Population", 
    y = "Tract Median Household Income Quintiles", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  # coord_flip() +
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.3, color = "black", size = 5
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  scale_y_discrete(
    labels = c(
      "1st", 
      "2nd", 
      "3rd", 
      "4th", 
      "5th"
    )
  ) + scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70))
  # plot.subtitle = element_text(size = 20)
# axis.title = element_text(size = 20),
# plot.title = element_text(size = 30),


ggsave(
  filename = "income_quintiles_only_ind.png",
  plot = plot$income_quintiles_only_ind,
  width = 2300*1.5,
  height = 2000*1.5,
  units = "px",
  dpi = 320,
  # scale = 1.5
)

ggsave(
  filename = "income_quintiles_only_ind.pdf",
  plot = plot$income_quintiles_only_ind,
  width = 2300*1.5,
  height = 2000*1.5,
  units = "px",
  # dpi = 320,
  # scale = 1.5
)

# Majority Race Plot (individual) ####
plot$majority_race_only_ind <- 
summary_tables$majority_summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Majority)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue", color = 'black'
  ) +
  labs(
    x = "",
    # x = "Annual Rate Per 10 Million Population", 
    y = "Tracts With Majority One Race (>50%)", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    hjust = -0.3, color = "black", size = 5
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, angle = 90, hjust = 0.5, color = 'black'),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0, 70)) +
  scale_y_discrete(labels = c("Black", "Latino", "White"))
  # coord_flip()
  # scale_x_continuous(breaks = seq(0,70,10))
  # plot$majority_race_only


ggsave(
  filename = "majority_race_only_ind.png",
  plot = plot$majority_race_only_ind,
  width = 2300*1.5,
  height = 2000*1.5,
  units = "px",
  dpi = 320,
  # scale = 1.5
)

ggsave(
  filename = "majority_race_only_ind.pdf",
  plot = plot$majority_race_only_ind,
  width = 2300*1.5,
  height = 2000*1.5,
  units = "px",
  # dpi = 320,
  # scale = 1.5
)



# Tables for 200 bins ####
summary_tables$bin_table_1 <-  fatal_enc$joined |> 
  filter(!is.na(income_bins)) |> 
  count(Income = income_bins) |> 
  rename(Killings = n) |> 
  mutate(Killings_Per_Yr = Killings / 6)

summary_tables$bin_pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_bins,
  sum, na.rm = TRUE) %>% 
  data.frame(Income = rownames(.), Population = .)


# Median income of each bin
# fatal_enc$joined |>  aggregate(IncomeE ~ Majority + income_bins, FUN = median)

summary_tables$bin_summary_1 <- 
  left_join(
    x = summary_tables$bin_table_1,
    y = summary_tables$bin_pop_table_1,
    by = "Income"
  )

summary_tables$bin_summary_1 <- summary_tables$bin_summary_1 |> 
  mutate(
    Annualized_Per_10_M =
      Killings_Per_Yr / Population * 10000000
  )

summary_tables$bin_summary_1$Income <- as.numeric(summary_tables$bin_summary_1$Income)

### Regression with binned data ####

# lm_200 <- summary_tables$bin_summary_1 |> lm(
#   Annualized_Per_10_M ~ Income,
#   data = _
# )

# summary(lm_200)
# cor(x = summary_tables$bin_summary_1$Income, y = summary_tables$bin_summary_1$Annualized_Per_10_M)
## Plot for 200 quantiles ####
plot$all_200 <- 
ggplot(summary_tables$bin_summary_1, aes(x = Income, y = Annualized_Per_10_M)) +
  geom_point() +  
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

ggsave(
  filename = "all_200.png",
  plot = plot$all_200,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "all_200.pdf",
  plot = plot$all_200,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)

plot$bar_200_all <- 
ggplot(summary_tables$bin_summary_1, aes(x = Income, y = Annualized_Per_10_M)) +
  geom_bar(stat = 'identity', fill = 'lightblue3', width = .7, color = 'black') +
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = TRUE) +
  labs(
    x = "Median Household Income in Census Tracts", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + theme_light() +
  theme(
    axis.text.x = element_blank()
  )

ggsave(
  filename = "bar_200_all.png",
  plot = plot$bar_200_all,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "bar_200_all.pdf",
  plot = plot$bar_200_all,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)

# ############################## #
# Grouped by Race/Ethnicity ####
# ############################## #

# table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  count(Majority, income_quintiles) |> 
  rename(Killings = n)

summary_tables$race_and_income_pop <- all_tracts$income_population_quintiles_2020 |> 
  aggregate(Total_popE ~ Majority + income_quintiles, FUN = sum) |> 
  rename(Population = Total_popE)

summary_tables$race_and_income_summary <- 
  left_join(
    x = summary_tables$race_and_income,
    y = summary_tables$race_and_income_pop,
    by = c("Majority", "income_quintiles")
  )

summary_tables$race_and_income_summary <- 
  summary_tables$race_and_income_summary |> 
  mutate(
    Annualized_Per_10_M =
      Killings / Population * 10000000 / 6
  ) |> 
  select(
    Majority, 
    Income = income_quintiles,
    Population,
    Killings,
    Annualized_Per_10_M
  ) |> add_row(
    summary_tables$summary_1 |>
      select(-Killings_Per_Yr)
  ) |> na.omit()

plot$quintile_by_race <- 
ggplot(
  summary_tables$race_and_income_summary
  ,aes(x = Majority, y = Annualized_Per_10_M, fill = Income)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
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
  filename = "quintile_by_race.png",
  plot = plot$quintile_by_race,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "quintile_by_race.pdf",
  plot = plot$quintile_by_race,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)

# ############################### #
# Grouped by Income Quintile ####
# ############################### #
plot$race_by_quintile <- 
  ggplot(
  summary_tables$race_and_income_summary, 
  aes(x = Income, y = Annualized_Per_10_M, fill = Majority)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
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
  filename = "race_by_quintile.png",
  plot = plot$race_by_quintile,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "race_by_quintile.pdf",
  plot = plot$race_by_quintile,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)



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

# Summary table for perentile bins by race ####
# Not using this plot for now
# plot$race_100 <- ggplot(
#   summary_tables$bin_table_race, 
#   aes(x = Income, y = Killings_Per_Yr, color = race_imputed)
# ) +
#   geom_point() +  
#   geom_smooth(method = "loess", formula = y ~ x, se = F) + 
#   labs(
#     x = "Median Household Income in Census Tracts\n100 Quantiles", 
#     y = "Number of Persons Killed", 
#     title = "Lethal Uses of Force",
#     color = "Race of Victim"
#   ) + scale_x_continuous(breaks = seq(0, 100 , by = 5)) + 
#   theme_light() +
#   scale_color_brewer(palette = "Dark2") +
#   theme()

### plot$race_100 ####

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
plot$race_100_proportion <- 
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
  filename = "race_100_proportion.png",
  plot = plot$race_100_proportion,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "race_100_proportion.pdf",
  plot = plot$race_100_proportion,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)

# ########################################################## #
## Creating a data frame for tracts without fatal encounters ####
# ########################################################## #
fatal_enc$no_fatal_enc <- 
  all_tracts$population_income2020[
    !all_tracts$population_income2020$GEOID %in% fatal_enc$joined$GEOID,
  ]

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

# Not using the unique histogram

# Fatal encounters: remove duplicated GEOIDs
# This is create a data frame that will include every tract that has had
# at least one incident, rather than every incident.
# This will make a better comparison between tracts with any incidents 
# (regardless of how many) vs. tracts without any incidents
# fatal_enc$fatal_enc_unique_id <- 
#   fatal_enc$joined[
#     !is.na(fatal_enc$joined$IncomeE) & !duplicated(fatal_enc$joined$GEOID),
#   ]
# alpha <- 0.5
# 
# hist_unique <- 
#   ggplot() +
#   geom_histogram(
#     data = fatal_enc$fatal_enc_unique_id,
#     aes(
#       x = IncomeE,
#       y = after_stat(density),
#       fill = "Lethal UOF"
#     ),
#     alpha = alpha,
#     bins = 30
#   ) +
#   geom_histogram(
#     data = fatal_enc$no_fatal_enc |> 
#       filter(!is.na(IncomeE)),
#     aes(
#       x = IncomeE, 
#       y = after_stat(density),
#       fill = "No Lethal UOF"
#     ),
#     alpha = alpha,
#     bins = 30
#   ) + 
#   geom_vline(
#     aes(
#       xintercept = fatal_enc$no_fuof_median_income, 
#       color = "No Lethal UOF"
#     ), 
#     linetype = "dashed", linewidth = 1
#   ) +
#   geom_vline(
#     aes(
#       xintercept = fatal_enc$median_no_dupes, 
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
#     values = c("No Lethal UOF" = "blue3", "Lethal UOF" = "red3"),
#     guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))
#   ) +
#   theme_light() +
#   scale_fill_brewer(palette = "Set1") +
#   scale_x_continuous(breaks = seq(0, 250000, by = 25000)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = 'black'))
# 
# ggsave(
#   filename = "hist_unique.png",
#   plot = plot$hist_unique,
#   width = 1650 * 2,
#   height = 780 * 2,
#   units = "px",
#   dpi = 320,
#   # scale = 1
# )
# 
# ggsave(
#   filename = "hist_unique.pdf",
#   plot = plot$hist_unique,
#   # width = 2300,
#   # height = 2000,
#   # units = "px",
#   # dpi = 320,
#   scale = 1.84
# )

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

write_csv(
  x = summary_tables$race_freq,
  file = "race_freq_table.csv")

## Race only plot ####
##
plot$race_proportion_of_total <- 
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



ggsave(
  filename = "race_proportion_of_total.png",
  plot = plot$race_proportion_of_total,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 360,
  # scale = 1
)

ggsave(
  filename = "race_proportion_of_total.pdf",
  plot = plot$race_proportion_of_total,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)

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
plot$inc_and_race_victim <- 
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
    legend.text = element_text(size = 12)
  )
ggsave(
  filename = "inc_and_race_victim.png",
  plot = plot$inc_and_race_victim,
  width = 1296 * 1.46,
  height = 519 * 1.46,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "inc_and_race_victim.pdf",
  plot = plot$inc_and_race_victim,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  scale = 1.84
)

summary_tables$quiniles_race_victim[1:20,] |> 
  mutate(Prop = round(Prop,digits = 3) * 100) |> 
  pivot_wider(names_from = Race, values_from = Prop) |> 
  mutate(
    Income =
      c("1st Quintile", "2nd Quintile", "3rd Quintile", 
        "4th Quintile", "5th Quintile")
    ) |> write_csv(file = "quiniles_race_victim.csv")
###
### Histograms #### 
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

plot$hist_all_fatal <-
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
    aes(xintercept = all_tracts$median_income, color = "All Tracts"),
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

ggsave(
  filename = "hist_all_fatal.png",
  plot = plot$hist_all_fatal,
  width = 1650 * 2,
  height = 780 * 2,
  units = "px",
  dpi = 320,
  # scale = 1
)

ggsave(
  filename = "hist_all_fatal.pdf",
  plot = plot$hist_all_fatal,
  # width = 2300,
  # height = 2000,
  # units = "px",
  # dpi = 320,
  # scale = 1.84
)

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
# summary_tables$quiniles_race_victim$Race <- 
#   factor(
#     summary_tables$quiniles_race_victim$Race, 
#     levels = c("All", "African-American/Black",
#                "European-American/White", "Hispanic/Latino"))
# 
# ggplot(
#   data = summary_tables$quiniles_race_victim,
#   aes(x = Race, y = Prop, fill = Income)) +
#   geom_bar(
#     stat = "identity", 
#     position = "dodge", 
#     color = 'black', 
#     linewidth = 0.01) +
#   labs(title = "Police Lethal Uses of Force",
#        subtitle = "Race of Victim. Years: [2015-2020]",
#        y = "Proportion of Racial Group",
#        x = "Race of Victim") +
#   theme_light() + 
#   theme(
#     axis.text.x = element_text(color = "black"),
#     panel.grid.major.x = element_blank()
#   )

### Plot: proportion of income quintile  ####
# ggplot(
#   data = summary_tables$quiniles_race_victim,
#   aes(x = Income, y = Prop, fill = Race)) +
#   geom_bar(
#     stat = "identity", 
#     position = "dodge", 
#     color = 'black', 
#     linewidth = 0.01) +
#   labs(title = "Police Lethal Uses of Force",
#        subtitle = "Race of Victim. Years: [2015-2020]",
#        # y = "Proportion of Racial Group",
#        x = "Median Household Income") +
#   theme_classic() + 
#   theme(
#     axis.text.x = element_text(color = "black"),
#     panel.grid.major.x = element_blank())

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

all_tracts$income_population_LUOF_count$Income10k <- 
  all_tracts$income_population_LUOF_count$IncomeE / 10000

all_tracts$income_population_LUOF_count$Majority <-
  ifelse(
    is.na(all_tracts$income_population_LUOF_count$Majority),
    "No Majority",
    all_tracts$income_population_LUOF_count$Majority
  )

all_tracts$income_population_LUOF_count$Majority <- 
  factor(all_tracts$income_population_LUOF_count$Majority, levels = c("White", "Hispanic/Latino", "Black", "No Majority"))
# ggplot(
#   data = summary_tables$quiniles_proportions_2,
#   aes(x = Income, y = Prop, fill = Race)) +
#   geom_bar(
#     stat = "identity", 
#     position = "dodge", 
#     color = 'black', 
#     linewidth = 0.01) +
#   labs(title = "Police Lethal Uses of Force",
#        subtitle = "Race of Victim. Years: [2015-2020]",
#        # y = "Proportion of Racial Group",
#        x = "Median Household Income") +
#   theme_classic() + 
#   theme(
#     axis.text.x = element_text(color = "black"),
# #     panel.grid.major.x = element_blank())
# 
# summary_tables$quiniles_race_victim <- 
#   summary_tables$quiniles_race_victim |> 
#   add_row(
#     summary_tables$summary_1 |> 
#       select(Income, Race = Majority,Killings_quintile = Killings) |> 
#       mutate(Total_Killed = sum(Killings_quintile)) |> 
#       mutate(Prop = Killings_quintile / Total_Killed) |> 
#       select(Income, Race, Prop)
#   )


# 5_Regression Models ####
#


## LUOF Frequency counts of each tract ####
# Adding frequency of lethal use of force in each tract to
# all_tracts

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

all_tracts$income_population_LUOF_count$LUOF_logical <- as.logical(all_tracts$income_population_LUOF_count$LUOF_count)

inc_in_LUOF_tracts <- 
  all_tracts$income_population_LUOF_count$IncomeE[
    all_tracts$income_population_LUOF_count$LUOF_logical
]

inc_in_nonLUOF_tracts <- 
  all_tracts$income_population_LUOF_count$IncomeE[
    !all_tracts$income_population_LUOF_count$LUOF_logical
  ]

qqnorm(inc_in_LUOF_tracts, pch = 16, col = 'dodgerblue')
qqline(inc_in_LUOF_tracts, col = 'red', lwd = 2)

qqnorm(inc_in_nonLUOF_tracts, pch = 16, col = 'green4')
qqline(inc_in_nonLUOF_tracts, col = 'blue', lwd = 2)

# t_test_inc <- 
t.test(
  inc_in_LUOF_tracts,
  inc_in_nonLUOF_tracts
)

wilcox.test(
  inc_in_LUOF_tracts,
  inc_in_nonLUOF_tracts
)

# write_csv(x = all_tracts$income_population_LUOF_count, file = "income_population_LUOF_count.csv")

lm_income <- 
  lm(IncomeE ~ LUOF_logical,
     data = all_tracts$income_population_LUOF_count)

# plot(x = all_tracts$income_population_LUOF_count$IncomeE,
#      y = all_tracts$income_population_LUOF_count$LUOF_logical)

# abline(lm_income)
summary(lm_income)
# plot(lm_income)

# Example assuming all_tracts$income_population_LUOF_count is your dataset
poisson_model <- 
  glm(LUOF_count ~ Income10k + Majority, 
      data = all_tracts$income_population_LUOF_count, 
      family = "poisson")

# Display summary
summary(poisson_model)

logit_model <- 
  glm(LUOF_logical ~ Income10k + Majority, 
      data = all_tracts$income_population_LUOF_count, 
      family = "binomial")

summary(logit_model)
# plot(logit_model)

income_population_LUOF_deciles <- read_csv("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github-project/income_population_LUOF_deciles.txt")
income_population_LUOF_deciles |> 
  group_by(Decile, Majority) |> 
  summarise(prop = mean(LUOF_logical)) |> 
  na.omit() |> 
  pivot_wider(names_from = Majority, values_from = prop)




