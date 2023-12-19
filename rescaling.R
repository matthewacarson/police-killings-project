# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run summary file to bring in data to summarize
source(file = "summary_tables.R")

# summary_tables$quiniles_race_victim
# summary_tables$quiniles_race_victim


summary_tables$race_LUOF_Rescale <-
  full_join(
    x = summary_tables$race_quint_proportions,
    y = summary_tables$quintile_race_proportion,
    by = join_by(Quintile, Race)
  )


summary_tables$race_LUOF_Rescale_s <-
  summary_tables$race_LUOF_Rescale |>
  select(-Killings_Race_Total,
         -Proportion_LUOF,
         -race_total_pop)


summary_tables$race_LUOF_Rescale_s$Rate <- 
summary_tables$race_LUOF_Rescale_s$Killings_by_Quintile_and_Race / 
  summary_tables$race_LUOF_Rescale_s$Population_in_tracts *
  10000000 / 6

summary_tables$race_LUOF_Rescale_White <-
  summary_tables$race_LUOF_Rescale_s[
    summary_tables$race_LUOF_Rescale_s$Race == "White",]

summary_tables$race_LUOF_Rescale_filter <-
  summary_tables$race_LUOF_Rescale_s[
    summary_tables$race_LUOF_Rescale_s$Race != "White",]

summary_tables$race_LUOF_Rescale_filter$white_prop <-
  summary_tables$race_LUOF_Rescale_White$Prop_living_in_tract[
    match(
    summary_tables$race_LUOF_Rescale_White$Quintile,
    summary_tables$race_LUOF_Rescale_s$Quintile
    )]

summary_tables$race_LUOF_Rescale_filter$rescaled <-
  summary_tables$race_LUOF_Rescale_filter$Rate *
  (summary_tables$race_LUOF_Rescale_filter$white_prop /
     summary_tables$race_LUOF_Rescale_filter$Prop_living_in_tract)

summary_tables$race_LUOF_Rescale_White
summary_tables$race_LUOF_Rescale_filter



summary_tables$race_LUOF_Rescale_s$rescale <-
summary_tables$race_LUOF_Rescale_s$Rate *
summary_tables$race_LUOF_Rescale_s$Prop_living_in_tract

summary_tables$race_LUOF_Rescale_s

# tapply(summary_tables$race_LUOF_Rescale_s$Prop_living_in_tract, summary_tables$race_LUOF_Rescale_s$Race, sum)
