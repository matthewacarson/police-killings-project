/* Change directory */
%let path=\\Client\C$\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\HP_PC\police_killings_github_HP\stata\dta;
libname mydata "&path";

/* Import fatal encounters data */
proc import datafile="\\Client\C$\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\HP_PC\police_killings_github_HP\stata\dta\fatal_enc_clean_geoid.csv"
    out=mydata.fatal_enc_clean_geoid
    dbms=csv
    replace;
    getnames=yes;
    guessingrows=max;
run;

/* Import all census tracts (2020) */
proc import datafile="\\Client\C$\Users\madou\OneDrive - UCLA IT Services\1)_PS-Honors\\police_killings_github\\stata\\dta\\all_tracts_2020.csv"
    out=mydata.all_tracts_2020
    dbms=csv
    replace;
    getnames=yes;
    guessingrows=max;
run;
