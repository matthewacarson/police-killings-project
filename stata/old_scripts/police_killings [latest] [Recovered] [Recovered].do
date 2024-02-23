// install packages
net install mdesc // package for counting missinge values
search nmissing
net install dm0085_2
// Change working directory

cd "\\Client\C$\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\\police_killings_github\\stata\\dta\\"

// cd "\\tsclient\C\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\police_killings_github\stata\dta\\"

// import all census tracts (2020)
// use income_population_2020.dta
import delimited income_population_2020.csv, case(preserve) clear
// insheet using income_population_2020.csv //, comma clear

// how many missing values
tabulate IncomeE if missing(IncomeE), missing
misstable summarize

egen missing_count = rowmiss(IncomeE)
total missing_count
drop missing_count
mdesc // IncomeE

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








// began running into issues here




// read in fatal encounters data
use "fatal_encounters_initial_clean_geoid - Copy.dta"
sort GEOID

// adding census data to the list of police fatal encounters
merge m:1 GEOID using all_tracts_population_income2020_modified
keep if _merge == 3
drop _merge
// inspecting
// describe
// list in 1/20

// rename 'race_imputed' variable
replace race_imputed = "White" if race_imputed == "European-American/White"
replace race_imputed = "Black" if race_imputed == "African-American/Black"
replace race_imputed = "Other/Unknown" if race_imputed != "White" & race_imputed != "Black" & race_imputed != "Hispanic/Latino"

// backup
save fatal_encounters_joined, replace

// reload other data set
use all_tracts_population_income2020_modified

// identify tracts (GEOID) where a fatal encounter occurred
merge 1:m GEOID using fatal_encounters_joined

// backup
save all_tracts_binary, replace

// create a binary variable for GEOIDs
// where a fatal encounter occurred
gen fatal_enc_binary = _merge == 3

// logistic regression
logit fatal_enc_binary IncomeE