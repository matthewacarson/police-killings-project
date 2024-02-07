# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run summary file to bring in data to summarize
source(file = "summary_tables.R")

######################################### #
######################################### #
# SCRIPT: rescaling.R ####
######################################### #
######################################### #

# These tables use the race of the victim.

summary_tables$race_LUOF_Rescale <-
  full_join(
    x = summary_tables$race_quint_proportions,
    y = summary_tables$quintile_race_proportion,
    by = join_by(Race, Quintile)
  ) |>   rename(
# income quintile of tract
    quintile = Quintile, 
    
# race of victim
    race = Race,

# count of that race living in that income quintile
    pop_quint_race = Population_in_tracts, 

# total US population of that race
    total_US_pop_race = race_total_pop,

# proportion of that racial group that lives in that income quintile
    prop_of_race_in_quintile = Prop_living_in_tract,

# count of LUOF of that race occurring within that income quintile
    LUOF_count_quint_race = Killings_by_Quintile_and_Race,

# total count of persons of that race killed
    LUOF_race_total_US = Killings_Race_Total,

# proportion of all persons killed of that race within that income quintile
    prop_of_LUOF_by_race = Proportion_LUOF
  )

# summary_tables$race_LUOF_Rescale # inspect

# Calculate the annual rate per 10 million population in tract type
summary_tables$race_LUOF_Rescale$rate <-
  summary_tables$race_LUOF_Rescale$LUOF_count_quint_race /
  summary_tables$race_LUOF_Rescale$pop_quint_race * 10000000 / 6

# Subset tracts where whites were killed -- quintiles
summary_tables$race_LUOF_Rescale_White <-
  summary_tables$race_LUOF_Rescale[summary_tables$race_LUOF_Rescale$race == "White",]

# Subset tracts where whites were killed -- quintiles
summary_tables$race_LUOF_Rescale_POC <-
  summary_tables$race_LUOF_Rescale[summary_tables$race_LUOF_Rescale$race != "White",]

# Add the proportion of whites in tracts to POC df
summary_tables$race_LUOF_Rescale_POC$white_prop_in_tract <-
  summary_tables$race_LUOF_Rescale_White$prop_of_race_in_quintile[match(
    summary_tables$race_LUOF_Rescale_POC$quintile,
    summary_tables$race_LUOF_Rescale_White$quintile
  )]

# Proportion of the race of victim in that tract income quintile
summary_tables$race_LUOF_Rescale_POC$`prop_of_race_in_quintile*rate` <-
  summary_tables$race_LUOF_Rescale_POC$prop_of_race_in_quintile * 
  summary_tables$race_LUOF_Rescale_POC$rate

# Rescaling as if the Black or Latino proportion living in "x" income tract
# were the same as the proportion of whites living in "x" income tract.
summary_tables$race_LUOF_Rescale_POC$rescaled <-
  summary_tables$race_LUOF_Rescale_POC$rate *
  summary_tables$race_LUOF_Rescale_POC$white_prop_in_tract

# Rounding the results for display (excluding non-numeric columns)
summary_tables$race_LUOF_Rescale_rounded <-
  apply(summary_tables$race_LUOF_Rescale_POC[, 3:12], MARGIN = 2,
        FUN = function(x) round(x = x, digits = 2)) |> as.data.frame()

# Rejoining non-numeric columns
summary_tables$race_LUOF_Rescale_rounded <-
  cbind(summary_tables$race_LUOF_Rescale_POC[, 1:2],
        summary_tables$race_LUOF_Rescale_rounded
)


# summary_tables$race_LUOF_Rescale_rounded

# Calculating overall white rate

mean(summary_tables$race_LUOF_Rescale_White$rate)

# ############ #
# Calculating overall black counterfactual rate 

summary_tables$race_LUOF_Rescale_POC
