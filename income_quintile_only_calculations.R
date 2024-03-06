# Income quintile only calculations
# 

######################################################### #
## killings per year -BY- quintile ####
######################################################### #
summary_tables$fatal_enc_table_1 <-  fatal_enc$joined |>
  count(Quintile = income_quintiles_nolab) |> rename(Killings = n) |> 
  filter(!is.na(Quintile)) |> mutate(Killings_Per_Yr = Killings / 6)

# col dictionary
# Income =                tract income quintile
# Killings =              number killed in quintile
# Killings_Per_Yr =       number of LUOF per year

######################################################### #
## total population in quintile ####
######################################################### #
summary_tables$pop_table_1 <- tapply(
  all_tracts$income_population_quintiles_2020$Total_popE, 
  all_tracts$income_population_quintiles_2020$income_quintiles_nolab,
  sum, na.rm = TRUE) %>%
  data.frame(Quintile = rownames(.), Population = .)
######################################################### #
## annualized: OVERALL rate for quintiles ####
######################################################### #
summary_tables$quintiles_only <- 
  left_join(
    x = summary_tables$fatal_enc_table_1,
    y = summary_tables$pop_table_1,
    by = "Quintile")

summary_tables$quintiles_only$Majority <- "All"

summary_tables$quintiles_only <- summary_tables$quintiles_only |> 
  mutate(
    Annual_10_M =
      Killings_Per_Yr / Population * 10000000)
