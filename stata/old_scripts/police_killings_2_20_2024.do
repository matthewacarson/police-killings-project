
clear

// Change working directory
cd "\\Client\C$\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\\police_killings_github"

// import all census tracts (2020)
use all_tracts_population_income2020

// inspecting
// inspect  

// how many missing values
// tabulate IncomeE if missing(IncomeE), missing

drop if missing(IncomeE)

// income quintiles
xtile income_quintile=IncomeE, n(5)

// inspecting if quintiles are roughly equal
// tabulate income_quintile

// income percentiles
xtile income_percentile=IncomeE, n(100)

// income into 200 quantiles
xtile income_200_quant=IncomeE, n(200)

// calculate proportions
generate NH_WhiteP = NH_WhiteE / Total_popE
generate NH_BlackP = NH_BlackE / Total_popE
generate NH_AsianP = NH_AsianE / Total_popE
generate Hisp_LatinoP = Hisp_LatinoE / Total_popE

// determine majority ethnicity
generate Majority = ""

replace Majority = "White" if NH_WhiteP > 0.5
replace Majority = "Black" if NH_BlackP > 0.5
replace Majority = "Hispanic/Latino" if Hisp_LatinoP > 0.5

// sorting by GEOID before merge
sort GEOID
save all_tracts_population_income2020_modified, replace

// read in fatal encounters data
clear
use fatal_encounters_initial_clean_geoid
sort GEOID

// adding census data to the list of police fatal encounters
merge m:1 GEOID using all_tracts_population_income2020_modified
keep if _merge == 3

// inspecting
// describe
// list in 1/20

// rename 'race_imputed' variable
replace race_imputed = "White" if race_imputed == "European-American/White"
replace race_imputed = "Black" if race_imputed == "African-American/Black"
replace race_imputed = "Other/Unknown" if race_imputed != "White" & race_imputed != "Black" & race_imputed != "Hispanic/Latino"

tabulate race_imputed

















