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

summary_tables$race_LUOF_Rescale

summary_tables$race_LUOF_Rescale$rate <-
  summary_tables$race_LUOF_Rescale$LUOF_count_quint_race /
  summary_tables$race_LUOF_Rescale$pop_quint_race * 10000000 / 6

summary_tables$race_LUOF_Rescale_White <-
  summary_tables$race_LUOF_Rescale_s[summary_tables$race_LUOF_Rescale_s$Race == "White",]

summary_tables$race_LUOF_Rescale_filter <-
  summary_tables$race_LUOF_Rescale_s[summary_tables$race_LUOF_Rescale_s$Race != "White",]

summary_tables$race_LUOF_Rescale_filter$white_prop <-
  summary_tables$race_LUOF_Rescale_White$Prop_living_in_tract[match(
    summary_tables$race_LUOF_Rescale_filter$Quintile,
    summary_tables$race_LUOF_Rescale_White$Quintile
  )]

summary_tables$race_LUOF_Rescale_filter$prop_x_rate <-
  summary_tables$race_LUOF_Rescale_filter$Prop_living_in_tract *
  summary_tables$race_LUOF_Rescale_filter$Rate

summary_tables$race_LUOF_Rescale_rounded <-
  apply(
    summary_tables$race_LUOF_Rescale_filter[, 3:11], MARGIN = 2,
    FUN = function(x) round(x = x, digits = 2)) |> as.data.frame()

summary_tables$race_LUOF_Rescale_rounded <- cbind(
  summary_tables$race_LUOF_Rescale_filter[, 1:2],
  summary_tables$race_LUOF_Rescale_rounded
)

summary_tables$race_LUOF_Rescale_filter$rescaled <-
  summary_tables$race_LUOF_Rescale_filter$Rate * 
  summary_tables$race_LUOF_Rescale_filter$white_prop

summary_tables$race_LUOF_Rescale_filter
