// install packages
// search mdesc
// net install "mdesc.pkg" // package for counting missing values
// search nmissing
// net install dm0085_2.pkg
// Change working directory

// cd "\\Client\C$\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\\police_killings_github\\stata\\dta\\"

cd "\\tsclient\C\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\HP_PC\police_killings_github_HP\stata\dta\\"

// read in fatal encounters data
// stringcols() specifies which columns are strings
// import delimited "fatal_enc_clean_geoid.csv", stringcols(1 17) case(preserve)clear

// convert CSV to .dta
// save fatal_enc_clean_geoid, replace

// all census tracts (2020)
// import delimited "all_tracts_2020.csv", stringcols(1) case(preserve) clear

// convert CSV to .dta
// save all_tracts_2020, replace

// load income_population_2020
// use all_tracts_2020, clear
use all_tracts_2020_foreign, clear
// use all_tracts_2020_haven, clear
// how many missing values

// check which cols have missing values
misstable summarize

// tabulate IncomeE if missing(IncomeE), missing
// egen missing_count = rowmiss(IncomeE)
// total missing_count
// drop missing_count
// mdesc // IncomeE

drop if missing(IncomeE)

// income quintiles
xtile income_quintile=IncomeE, n(5)

// income declines
xtile income_decile=IncomeE, n(10)

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
generate non_whiteP = 1 - NH_WhiteP

// determine majority ethnicity
generate Majority = ""

replace Majority = "White" if NH_WhiteP > 0.5
replace Majority = "Black" if NH_BlackP > 0.5
replace Majority = "Hispanic/Latino" if Hisp_LatinoP > 0.5

// sorting by GEOID before merge
sort GEOID
// save all_tracts_2020_modified, replace
save all_tracts_2020_modified_foreign, replace
// save all_tracts_2020_modified_haven, replace


//////////////////////////////////////
// Begin working with fatal enc data
//////////////////////////////////////

// read in fatal encounters data
// use fatal_enc_clean_geoid, clear
use fatal_enc_clean_geoid_foreign, clear
// use fatal_enc_clean_geoid_haven, clear

////////////////////////////////////////
// add census data to fatal encounters
////////////////////////////////////////

// adding census data to the list of police fatal encounters
merge m:1 GEOID using all_tracts_2020_modified_foreign
// figure out which GEOIDs aren't merging
keep if _merge == 3
drop _merge
duplicates report unique_id
// inspecting
// describe
// list in 1/20

// rename 'race_imputed' variable
replace race_imputed = "White" if race_imputed == "European-American/White"
replace race_imputed = "Black" if race_imputed == "African-American/Black"
replace race_imputed = "Other/Unknown" if race_imputed != "White" & race_imputed != "Black" & race_imputed != "Hispanic/Latino"

// backup
save fatal_encounters_joined, replace

// fatal encounters data set is complete for later analysis

///////////////////////////////////////////////////////
// Working on binary for logistic regression
///////////////////////////////////////////////////////
use fatal_encounters_joined, clear
// drop duplicates for GEOID
// (I am only making a binary variable)
duplicates drop GEOID, force
gen LUOF_binary = 1
sort GEOID

save fatal_encounters_joined, replace

// Reload all tracts to create binary variable for where fatal
// encounters occurred
use all_tracts_2020_modified_foreign, clear


// identify tracts (GEOID) where a fatal encounter occurred
merge m:1 GEOID using fatal_encounters_joined

// checking for duplicates
duplicates report GEOID
misstable summarize

// create a binary variable for GEOIDs
// where a fatal encounter occurred
gen fatal_enc_binary = _merge == 3
total fatal_enc_binary

// backup
save all_tracts_binary, replace


///////////////////////
// load all_tracts_binary
/////////////////////////
use all_tracts_binary, clear

// convert IncomeE to thousands place
gen IncomeE_1k = IncomeE / 1000

// logistic regression -- income only
logit fatal_enc_binary IncomeE_1k

// Store estimation results
estimates store my_model

// Export results to Excel using putexcel
putexcel set logit_results, replace // specify Excel file name 
putexcel A1 = etable // upper-lefthand corner of where data should go

// predictions based on income at 10,000 intervals
margins, at(IncomeE_1k = (10(10)250)) noatleg

// plot logit regression IncomeE
marginsplot, title("Predicted Probability of a LUOF, 2015-2020") ///
              xtitle("Median household income in census tract") ///
              ytitle("Probability of a LUOF") ///
			  name(income_only, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")

graph export "..\LUOF_logit_income_only.png", width(7680) height(4608) replace



// logistic regression NH_BlackP
logit fatal_enc_binary NH_BlackP

// Store estimation results
estimates store my_model
putexcel A10 = etable

margins, at(NH_BlackP = (0(.05)1)) noatleg

marginsplot, title("Predicted Probability of a LUOF, 2015-2020") ///
              xtitle("Proportion black in the census tract") ///
              ytitle("Probability of a LUOF") ///
			  name(NH_BlackP, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")

graph export "..\LUOF_logit_NH_black_only.png", width(7680) height(4608) replace

// logistic regression NH_WhiteP
logit fatal_enc_binary NH_WhiteP

// Store estimation results
estimates store my_model
putexcel A20 = etable

margins, at(NH_WhiteP = (0(.05)1)) noatleg

marginsplot, title("Predicted Probability of a LUOF, 2015-2020") ///
              xtitle("Proportion white in census tract") ///
              ytitle("Probability of a LUOF") ///
			  name(NH_WhiteP, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")

graph export "..\LUOF_logit_NH_white_only.png", width(7680) height(4608) replace

// logistic regression Hisp_LatinoP
logit fatal_enc_binary Hisp_LatinoP

// Store estimation results
estimates store my_model
putexcel A30 = etable

margins, at(Hisp_LatinoP = (0(.05)1)) noatleg

marginsplot, title("Predicted Probability of a LUOF, 2015-2020") ///
              xtitle("Proportion Hispanic/Latino in census tract") ///
              ytitle("Probability of a LUOF") ///
			  name(Hisp_LatinoP, replace) ///
			  yscale(range(0 0.18)) ///
			  ylabel(0 "0" 0.02 "0.02" 0.04 "0.04" 0.06 "0.06" 0.08 "0.08" 0.10 "0.10" 0.12 "0.12" 0.14 "0.14" 0.16 "0.16" 0.18 "0.18")

graph export "..\LUOF_logit_Hisp_Latino_only.png", width(7680) height(4608) replace

// Combine plots into quadrants
graph combine NH_BlackP Hisp_LatinoP NH_WhiteP, xcommon ycommon ///
name(bivariate, replace)

graph export "..\LUOF_logit_bivariate.png", width(7680) height(4608) replace

///////////////////////////
// Create interaction terms
///////////////////////////

// check for missing values
count if missing(IncomeE_1k)

// Generate interaction terms
gen IncomeE_1k_NH_BlackP = IncomeE_1k * NH_BlackP
list IncomeE_1k_NH_BlackP in 1/10
gen IncomeE_1k_NH_WhiteP = IncomeE_1k * NH_WhiteP
list IncomeE_1k_NH_WhiteP in 1/10
gen IncomeE_1k_NH_Latino = IncomeE_1k * Hisp_LatinoP
list IncomeE_1k_NH_Latino in 1/10

// backup data
save all_tracts_2020_interaction_terms, replace
// export delimited all_tracts_2020_interaction_terms, replace
// use all_tracts_2020_interaction_terms, clear

//////////////////////////
// logits: INTERACTIONS
//////////////////////////

// Run logistic regression with interaction term

logit fatal_enc_binary IncomeE_1k NH_BlackP IncomeE_1k_NH_BlackP

// Store estimation results
estimates store my_model
putexcel A40 = etable

margins, dydx(IncomeE_1k) at(NH_BlackP = (0.00(0.05)1)) post

// Plot the results
marginsplot, title("Average marginal effects of Income (95% CIs)") ///
			xtitle("Proportion black in census tract") ///
			ytitle("Effects of $1k income on Pr(LUOF)") ///
			name(IncomeE_1k_black, replace) ///
			ylabel(-.0013 -.0012 -.0011 -.001 -.0009 -.0008 -.0007 -.0006 -.0005 -.0004)

graph export "..\LUOF_logit_IncomeE_1k_NH_BlackP.png", width(7680) height(4608) replace

// // Run logistic regression with interaction term
// logit fatal_enc_binary IncomeE_1k NH_WhiteP IncomeE_1k_NH_WhiteP
//
// Calculate margins for NH_BlackP, varying IncomeE
// margins, dydx(NH_WhiteP) at(IncomeE_1k = (5(10)250)) post
//
// marginsplot

logit fatal_enc_binary IncomeE_1k NH_WhiteP IncomeE_1k_NH_WhiteP

// Store estimation results
estimates store my_model
putexcel A50 = etable

margins, dydx(IncomeE_1k) at(NH_WhiteP = (0.00(0.05)1)) post

marginsplot, title("Average marginal effects of Income (95% CIs)") ///
			xtitle("Proportion white in census tract") ///
			ytitle("Effects of $1k income on Pr(LUOF)") ///
			name(IncomeE_1k_white, replace) ///
			ylabel(-.0013 -.0012 -.0011 -.001 -.0009 -.0008 -.0007 -.0006 -.0005 -.0004)

			
graph export "..\LUOF_logit_IncomeE_1k_NH_WhiteP.png", width(7680) height(4608) replace
			
// Latino logit
logit fatal_enc_binary IncomeE_1k Hisp_LatinoP IncomeE_1k_NH_Latino

// Store estimation results
estimates store my_model
putexcel A60 = etable

// Close/save Excel file
putexcel save

margins, dydx(IncomeE_1k) at(Hisp_LatinoP = (0.00(0.05)1)) post

marginsplot, title("Average marginal effects of Income (95% CIs)") ///
			xtitle("Proportion Hispanic/Latino in census tract") ///
			ytitle("Effects of $1k income on Pr(LUOF)") ///
			name(IncomeE_1k_hispLatino, replace) ///
			ylabel(-.0013 -.0012 -.0011 -.001 -.0009 -.0008 -.0007 -.0006 -.0005 -.0004)

graph export "..\LUOF_logit_IncomeE_1k_NH_Latino.png", width(7680) height(4608) replace
			
// Combine plots into quadrants
graph combine IncomeE_1k_black IncomeE_1k_white IncomeE_1k_hispLatino, xcommon ycommon ///
name(combined_effects, replace)

graph export "..\LUOF_logit_combined_effects.png", width(7680) height(4608) replace