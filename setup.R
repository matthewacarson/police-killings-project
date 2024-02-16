# 1_all_tracts ####

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Creating environments

if (!exists("all_tracts")) {all_tracts <- new.env()}

if (!exists("fatal_enc")) {fatal_enc <- new.env()}

# Download and clean tract income ####
## Download 2019 data ####

# population_income2019 <- get_acs(
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
# save(
#   population_income2019, 
#   file = "acs_2019.RData"); rm(population_income2019)

## Download 2020 tract data ####
# population_income2020 <- get_acs(
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
#   cache_table = TRUE)
# save(
#   population_income2020, 
#   file = "acs_2020.RData"); rm(population_income2020)

# Load data from tidycensus ACS
load("git_ignore/RData/acs_2019_raw.Rdata", envir = all_tracts)
load("git_ignore/RData/acs_2020_raw.Rdata", envir = all_tracts)

# Filter tracts with NA income values ####
all_tracts$income_population_quintiles_2020 <- st_drop_geometry(all_tracts$population_income2020)
all_tracts$income_population_quintiles_2020 <- all_tracts$income_population_quintiles_2020[!is.na(all_tracts$income_population_quintiles_2020$IncomeE),]

## Add quantile data to all_tracts ####

### Tertiles ####
all_tracts$tertiles_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/3)), na.rm = TRUE
)
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_tertile = cut(
    IncomeE,
    labels = 1:3,
    breaks = all_tracts$tertiles_2020,
    include.lowest = TRUE, ordered_result = TRUE
    # labels = FALSE 
    # Setting labels to FALSE gives numeric labels to bins
  ))

### Quartiles ####
all_tracts$quartiles_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/4)), na.rm = TRUE
)

all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_quartile = cut(
    IncomeE,
    labels = 1:4,
    breaks = all_tracts$quartiles_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### Quintiles ####
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_quintiles = cut(
    IncomeE,
    labels = c("1st Quintile",
               "2nd Quintile",
               "3rd Quintile",
               "4th Quintile",
               "5th Quintile"),
    breaks = quantile(IncomeE, 
                      probs = seq(0, 1, by = (1/5)), 
                      na.rm = TRUE),
    include.lowest = TRUE, ordered_result = TRUE))


# Without quintile labeling
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_quintiles_nolab = cut(
    IncomeE,
    labels = 1:5,
    breaks = quantile(IncomeE, 
                      probs = seq(0, 1, by = (1/5)), 
                      na.rm = TRUE),
    include.lowest = TRUE, ordered_result = TRUE))

### Sextiles ####
all_tracts$sextile_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/6)), na.rm = TRUE)

all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_sextile = cut(
    IncomeE,
    labels = 1:6,
    breaks = all_tracts$sextile_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### Septiles ####
all_tracts$septile_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/7)), na.rm = TRUE
)

all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_septile = cut(
    IncomeE,
    labels = 1:7,
    breaks = all_tracts$septile_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### Octiles ####
all_tracts$octile_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/8)), na.rm = TRUE
)
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_octile = cut(
    IncomeE,
    labels = 1:8,
    breaks = all_tracts$octile_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### Noniles ####
all_tracts$nonile_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/9)), na.rm = TRUE
)
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_nonile = cut(
    IncomeE,
    labels = 1:9,
    breaks = all_tracts$nonile_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### Deciles ####
all_tracts$decile_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/10)), na.rm = TRUE
)
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_decile = cut(
    IncomeE,
    labels = 1:10,
    breaks = all_tracts$decile_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### Break into 15 quantiles ####
all_tracts$income_population_quintiles_2020 <- 
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_15_quant = cut(
    IncomeE,
    labels = 1:15,
    breaks = quantile(IncomeE, 
                      probs = seq(0, 1, by = (1/15)), 
                      na.rm = TRUE), 
    include.lowest = TRUE, ordered_result = TRUE))

### Sub-Octile ####
all_tracts$income_population_quintiles_2020 <- 
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_suboctile = cut(
    IncomeE,
    labels = 1:16,
    breaks = quantile(IncomeE, 
                      probs = seq(0, 1, by = (1/16)), 
                      na.rm = TRUE), 
    include.lowest = TRUE, ordered_result = TRUE))

### Ventiles ####
all_tracts$ventile_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/20)), na.rm = TRUE
)
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_ventile = cut(
    IncomeE,
    labels = 1:20,
    breaks = all_tracts$ventile_2020,
    include.lowest = TRUE, ordered_result = TRUE))

### 50 Quantiles ####
all_tracts$quantiles_50_2020 <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = (1/50)), na.rm = TRUE
)
all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_50_quantiles = cut(
    IncomeE,
    labels = 1:50,
    breaks = all_tracts$quantiles_50_2020,
    include.lowest = TRUE, ordered_result = TRUE))

# Divide into 100  (percentiles)
all_tracts$one_hundred <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = 1/100), na.rm = TRUE)

all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_bins_100 = cut(
    IncomeE,
    labels = seq(1,100),
    breaks = all_tracts$one_hundred,
    include.lowest = TRUE,ordered_result = TRUE))

### Sub-Percentiles (200 quantiles) ####
all_tracts$two_hundred <- quantile(
  all_tracts$income_population_quintiles_2020$IncomeE,
  probs = seq(0, 1, by = 1/200), na.rm = TRUE)

all_tracts$income_population_quintiles_2020 <-
  all_tracts$income_population_quintiles_2020 |>
  mutate(income_bins_200 = cut(
    IncomeE,
    labels = seq(1,200),
    breaks = all_tracts$two_hundred, # 201 points create 200 bins
    include.lowest = TRUE, ordered_result = TRUE))


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

# fatal_enc$initial_clean <-
#   fatal_enc$backup_not_clean %>%
#   select(
#     # Select columns to keep and rename
#     c(unique_id = `Unique ID`,
#       race = `Race`,
#       race_imputed = `Race with imputations`,
#       imputation_probability = `Imputation probability`,
#       date = `Date of injury resulting in death (month/day/year)`,
#       city = `Location of death (city)`,
#       state = `State`,
#       zip = `Location of death (zip code)`,
#       county = `Location of death (county)`,
#       latitude = `Latitude`,
#       longitude = `Longitude`,
#       agencies = `Agency or agencies involved`,
#       highest_force = `Highest level of force`,
#       armed = `Armed/Unarmed`,
#       weapon = `Alleged weapon`,
#       aggressive = `Aggressive physical movement`,
#       fleeing = `Fleeing/Not fleeing`,
#       intended_use_of_force = `Intended use of force (Developing)`,
#     )
#   ) |>    ### Remove spacer (this wasn't an observation) ####
# filter(date != "1999-12-31") |>
#   ### Remove non-numeric characters from the latitude column ####
# mutate(latitude = as.numeric(gsub("[^0-9.-]", "", latitude))) |> 
#   filter(
#     highest_force %in% c(
#       "Gunshot", 
#       "Tasered", 
#       "Beaten/Bludgeoned with instrument",
#       "Asphyxiated/Restrained", 
#       "Asphyxiation/Restrained",
#       "Asphyxiation/Restrain", 
#       "Less-than-lethal force",
#       "Stabbed", 
#       "Chemical agent/Pepper spray", 
#       "Restrain/Asphyxiation")) |>  
#   filter(intended_use_of_force != "Suicide")
# 
# fatal_enc$initial_clean <- fatal_enc$initial_clean |> 
#   filter(year(date) >= 2015 & year(date) <= 2020)
# 
# # Fix Hispanic/Latino (uppercase "I")
# # 
# fatal_enc$initial_clean$race_imputed <- ifelse(
#   fatal_enc$initial_clean$race_imputed == "HIspanic/Latino",
#   "Hispanic/Latino", 
#   fatal_enc$initial_clean$race_imputed
# )
# 
# ## Adding sf object to fatal_enc (combined lat/long) ####
# fatal_enc$initial_clean <- 
#   fatal_enc$initial_clean %>% 
#   st_as_sf(coords = c('longitude', 'latitude'), crs = "NAD83")
# 
# ## Join the fatal encounters data set with census data
# ### Add GEOID to the fatal encounters data set
# 
# # r Add GEOID to the fatal encounters data set
# fatal_enc$initial_clean_geoid <- 
#   st_join(
#     x = fatal_enc$initial_clean,
#     y = all_tracts$population_income2020 %>% select(GEOID),
#     join = st_within)
# 
# fatal_enc$initial_clean_geoid <- st_drop_geometry(fatal_enc$initial_clean_geoid)
# 
# # Remove duplicated rows (unique ids that appear twice)
# fatal_enc$initial_clean_geoid <- 
#   fatal_enc$initial_clean_geoid[
#     !duplicated(fatal_enc$initial_clean_geoid$unique_id) | 
#       duplicated(fatal_enc$initial_clean_geoid, fromLast = TRUE),]
# 
# fatal_enc$initial_clean_geoid$imputation_probability <-
#   as.numeric(fatal_enc$initial_clean_geoid$imputation_probability)

############################################ #
# Joining with all_tracts data from above ####
############################################ #
load(
  file = "git_ignore/RData/fatal_enc_initial_clean_geoid.RData",
  envir = fatal_enc)

fatal_enc$joined_backup <- 
  left_join(
    x = fatal_enc$initial_clean_geoid,
    y = all_tracts$income_population_quintiles_2020,
    by = "GEOID")

############################################# #
# Rename race and race_imputed variables
############################################# #

fatal_enc$joined <- 
  fatal_enc$joined_backup |> 
  mutate(
    race =
      case_when(
        race == "European-American/White" ~ "White",
        race == "African-American/Black" ~ "Black",
        race == "Hispanic/Latino" ~ race,
        TRUE ~ "Other/Unknown"
      ),
    race_imputed =
      case_when(
        race_imputed == "European-American/White" ~ "White",
        race_imputed == "African-American/Black" ~ "Black",
        race_imputed == "Hispanic/Latino" ~ race_imputed,
        TRUE ~ "Other/Unknown"
      )
  )



