* Encoding: UTF-8.
show directory
    
CD '\\tsclient\C\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\police_killings_github\stata\dta\'.

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="\\tsclient\C\Users\madou\OneDrive - UCLA IT "+
    "Services\1)_PS-Honors\police_killings_github\stata\dta\all_tracts_2020_interaction_terms.csv"
  /ENCODING='UTF8'
  /DELCASE=LINE
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  GEOID AUTO
  NAME AUTO
  IncomeE AUTO
  IncomeM AUTO
  Total_popE AUTO
  Total_popM AUTO
  NH_WhiteE AUTO
  NH_WhiteM AUTO
  NH_BlackE AUTO
  NH_BlackM AUTO
  NH_AsianE AUTO
  NH_AsianM AUTO
  Hisp_LatinoE AUTO
  Hisp_LatinoM AUTO
  income_quintile AUTO
  income_decile AUTO
  income_percentile AUTO
  income_200_quant AUTO
  NH_WhiteP AUTO
  NH_BlackP AUTO
  NH_AsianP AUTO
  Hisp_LatinoP AUTO
  non_whiteP AUTO
  Majority AUTO
  unique_id AUTO
  race AUTO
  race_imputed AUTO
  imputation_probability AUTO
  date AUTO
  city AUTO
  state AUTO
  zip AUTO
  county AUTO
  agencies AUTO
  highest_force AUTO
  armed AUTO
  weapon AUTO
  aggressive AUTO
  fleeing AUTO
  intended_use_of_force AUTO
  LUOF_binary AUTO
  @_merge AUTO
  fatal_enc_binary AUTO
  IncomeE_1k AUTO
  IncomeE_1k_NH_BlackP AUTO
  IncomeE_1k_NH_WhiteP AUTO
  IncomeE_1k_NH_Latino AUTO
  /MAP.
RESTORE.

CACHE.
EXECUTE.
DATASET NAME DataSet2 WINDOW=FRONT.
