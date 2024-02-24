// create no do file:
// doedit police_killings

// Ctrl + Shift + E to run one line
// logical negation: ~

// summary statistics by income quintile
sort income_quintile
by income_quintile: summarize IncomeE

by income_quintile: median IncomeE

// open data editor ?
browse

// removes a variable
drop


// inspecting data
ds
inspect
describe
codebook IncomeE
summarize IncomeE


// current working directory
pwd

//head
list in 1/10

// tail
list in -10/l 

// matrix list victim_race_quintile

// net install http://www.stata.com/users/kcrow/tab2xl, replace

// tab2xl race_imputed income_quintile using testfile row(1)