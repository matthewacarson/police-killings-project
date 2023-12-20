# setwd("C:/Users/madou/OneDrive - UCLA IT Services/PS-Honors/police-killings-github")

# Load Libraries ####
# library(tidycensus)
# library(sf)
# library(tidyverse)

# Run summary file to bring in data to summarize
source(file = "summary_tables.R")

# Assuming your data frame is named summary_tables$race_LUOF_Rescale_s
total_population_by_quintile <- tapply(summary_tables$race_LUOF_Rescale_s$Population_in_tracts, summary_tables$race_LUOF_Rescale_s$Quintile, sum)

# Create a new data frame for counterfactual calculations
counterfactual_data <- summary_tables$race_LUOF_Rescale_s

# Calculate counterfactual Prop_living_in_tract
for (quintile in unique(summary_tables$race_LUOF_Rescale_s$Quintile)) {
  whites_prop <- subset(summary_tables$race_LUOF_Rescale_s, Quintile == quintile & Race == "White")$Prop_living_in_tract
  counterfactual_data$Counterfactual_Prop_living_in_tract[summary_tables$race_LUOF_Rescale_s$Quintile == quintile] <-
    (summary_tables$race_LUOF_Rescale_s$Population_in_tracts[summary_tables$race_LUOF_Rescale_s$Quintile == quintile] / total_population_by_quintile[quintile]) * whites_prop
}

# Calculate counterfactual Rate
counterfactual_data$Counterfactual_Rate <- (counterfactual_data$Counterfactual_Prop_living_in_tract * counterfactual_data$Killings_by_Quintile_and_Race / counterfactual_data$Population_in_tracts) * 100000

# Print the result
print(counterfactual_data[, c("Quintile", "Race", "Counterfactual_Rate")])
