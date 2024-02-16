
setwd(
  "C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")
# 1_all_tracts ####

# Load Libraries ####

library(tidycensus)
library(sf)
library(tidyverse)

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

all_tracts$income_population_quintiles_2020 <- st_drop_geometry(all_tracts$population_income2020)

all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020[!is.na(all_tracts$income_population_quintiles_2020$IncomeE),]

## Add quantile data to all_tracts ####

### Quintiles ####
all_tracts$quintiles_2020 <- quantile(
  all_tracts$population_income2020$IncomeE, 
  probs = seq(0, 1, by = 0.2), na.rm = TRUE
)

all_tracts$quintile_text <- c(
  "1st Quintile\n(Lowest Income)",
  "2nd Quintile",
  "3rd Quintile",
  "4th Quintile",
  "5th Quintile\n(Highest Income)"
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

### Divide into 200 groups ####
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

## Designate tracts as racial majorities ####

all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020 %>%
  mutate(
    NH_WhiteP = NH_WhiteE / Total_popE,
    NH_BlackP = NH_BlackE / Total_popE,
    NH_AsianP = NH_AsianE / Total_popE,
    Hisp_LatinoP = Hisp_LatinoE / Total_popE,
    Majority = case_when(
      NH_WhiteP > 0.5 ~ 'W',
      NH_BlackP > 0.5 ~ 'B',
      # NH_AsianP > 0.5 ~ 'A',
      Hisp_LatinoP > 0.5 ~ 'H'))

## Identifying majority race/ethnicity with 2019 data. ####

all_tracts$income_population_quintiles_2019 <- all_tracts$population_income2019 %>% 
  mutate(
    NH_WhiteP = NH_WhiteE / Total_popE,
    NH_BlackP = NH_BlackE / Total_popE,
    NH_AsianP = NH_AsianE / Total_popE,
    Hisp_LatinoP = Hisp_LatinoE / Total_popE,
    Majority = case_when(
      NH_WhiteP > 0.5 ~ 'W',
      NH_BlackP > 0.5 ~ 'B',
      # NH_AsianP > 0.5 ~ 'A',
      Hisp_LatinoP > 0.5 ~ 'H'))

all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020 %>% 
  mutate(
    Majority_Quintile = paste(Majority, income_quintiles, sep = "_")
  )

# 2_fatal_enc ####

# fatal_enc <- list()
fatal_enc$backup_not_clean <- read_csv(
  "https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/export?format=csv", 
  col_types = cols(
    `Date of injury resulting in death (month/day/year)` = col_date(format = "%m/%d/%Y"),
    Name = col_skip(),
    Age = col_skip(),
    Gender = col_skip(),
    `URL of image (PLS NO HOTLINKS)` = col_skip(),
    `Location of injury (address)` = col_skip(),
    `Full Address` = col_skip(),
    `UID Temporary` = col_skip(),
    `Name Temporary` = col_skip(),
    `Description Temp` = col_skip(),
    `URL Temp` = col_skip(),
    `Brief description` = col_skip(),
    `Dispositions/Exclusions INTERNAL USE, NOT FOR ANALYSIS` = col_skip(),
    `Supporting document link` = col_skip(),
    `Foreknowledge of mental illness? INTERNAL USE, NOT FOR ANALYSIS` = col_skip(),
    ...33 = col_skip(),
    ...34 = col_skip(),
    `Unique ID formula` = col_skip(),
    `Unique identifier (redundant)` = col_skip()
  )
)

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


## Adding sf object to fatal_enc (combined lat/long) ####
library(sf)
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

# Remove duplicated rows
fatal_enc$initial_clean_geoid <- 
  fatal_enc$initial_clean_geoid[
    !duplicated(fatal_enc$initial_clean_geoid$unique_id) | 
      duplicated(fatal_enc$initial_clean_geoid, fromLast = TRUE),
  ]

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

# Income Quintiles only

# Assuming summary_tables$summary_1$Income and summary_tables$summary_1$Annualized_Per_10_M are vectors in your data frame

# First, install and load necessary packages if you haven't already
# install.packages("ggplot2")
# library(ggplot2)

# Create quintile bar plot ####
summary_tables$summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Income)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue"
  ) +
  labs(
    x = "Annual Rate Per 10 Million Population", 
    y = "Median Household Income in Census Tract", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  coord_flip() +
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, color = "black"
  ) + theme_light() +
  theme(
    axis.text.x = element_text(size = 15, color = "black"),
    # axis.title = element_text(size = 20),
    # plot.title = element_text(size = 30),
    # plot.subtitle = element_text(size = 20)
  )

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

summary_tables$majority_summary_1 |> 
  ggplot(
    data = _, 
    aes(x = Annualized_Per_10_M, y = Majority)
  ) +
  geom_bar(
    stat = "identity", fill = "skyblue"
  ) +
  labs(
    x = "Annual Rate Per 10 Million Population", 
    y = "Tracts With Majority One Race (>50%)", 
    title = "Police Lethal Uses of Force",
    subtitle = "Years: [2015-2020]"
  ) + 
  geom_text(
    aes(label = round(Annualized_Per_10_M, 1)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, color = "black"
  ) + theme_light() +
  theme(axis.text.x = element_text(size = 12, color = "black")) +
  scale_y_discrete(
    labels = c("Black", "Hispanic/Latino", "White"),
  ) +
  coord_flip()

# Trying to add race of victim as another factor

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

fatal_enc$joined |>  aggregate(IncomeE ~ Majority + income_bins, FUN = median)



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

# Regression with binned data ####

lm_200 <- summary_tables$bin_summary_1 |> lm(
  Annualized_Per_10_M ~ Income,
  data = _
)

summary(lm_200)
cor(x = summary_tables$bin_summary_1$Income, y = summary_tables$bin_summary_1$Annualized_Per_10_M)

ggplot(summary_tables$bin_summary_1, aes(x = Income, y = Annualized_Per_10_M)) +
  geom_point() +  
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = TRUE, interval = 'prediction') + 
  labs(
    x = "Median Household Income in Census Tracts\n200 Quantiles", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + scale_x_continuous(breaks = seq(0, 200 , by = 5)) + 
  theme_light() +
  theme(
    # axis.text.x = element_blank()
  )

ggplot(summary_tables$bin_summary_1, aes(x = Income, y = Annualized_Per_10_M)) +
  geom_bar(stat = 'identity', fill = 'lightblue3', width = .7) +
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = TRUE) +
  labs(
    x = "Median Household Income in Census Tracts", 
    y = "Per 10 Million Population (Annualized)", 
    title = "Lethal Uses of Force"
  ) + theme_light() +
  theme(
    axis.text.x = element_blank()
  )

# GGplot race and income
#

table(fatal_enc$joined$Majority, fatal_enc$joined$income_quintiles)

summary_tables$race_and_income <- 
  fatal_enc$joined |> 
  na.omit() |> 
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
  )

ggplot(
  summary_tables$race_and_income_summary
  ,aes(x = Majority, y = Annualized_Per_10_M, fill = Income)) +
  geom_hline(
    yintercept = seq(0,70, by = 10), 
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
  theme_minimal() + theme(axis.text.x = element_text(color = "black"))

summary(object = all_tracts$income_population_quintiles_2019)

