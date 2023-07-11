// Read in PISA 2018 student questionnaire (main file)
import spss "CY07_MSU_STU_QQQ.sav", clear

// Select variables of interest
keep CNTRYID OECD ST004D01T AGE ESCS ST019AQ01T ST019BQ01T ST019CQ01T ST021Q01TA ICTHOME ICTSCH ENTUSE HOMESCH USESCH ICTCLASS ICTOUTSIDE COMPICT INTICT SOIAICT AUTICT METASPAM starts_with(PV) ends_with(MATH) ends_with(SCIE) ends_with(READ)

// Rename columns
rename ST004D01T GNDR ///
       CNTRY CNT ///
       CNTSCHID SCHOOLID ///
       CNTSTUID STUDENTID ///
       ST019AQ01T Origin_cntry_you ///
       ST019BQ01T Origin_cntry_mother ///
       ST019CQ01T Origin_cntry_father ///
       ST021Q01TA Age_arrival_cntry

// Create variable immback indicating immigrant background
egen immback = group(Origin_cntry_mother Origin_cntry_father Origin_cntry_you Age_arrival_cntry), label

// Calculate achievement scores
egen MATH = rowmean(PV1MATH to PV10MATH)
egen SCIE = rowmean(PV1SCIE to PV10SCIE)
egen READ = rowmean(PV1READ to PV10READ)

// Create dummy variable for gender
gen male = GNDR
replace male = 0 if GNDR == 1
replace male = 1 if GNDR == 2
replace male = . if GNDR >= 5

// Remove missing values
replace GNDR = . if GNDR >= 5
replace AGE = . if AGE >= 9995
replace ESCS = . if ESCS >= 95
replace ICTHOME = . if ICTHOME >= 95

// Subset dataset for immigrants only
keep if immback == 1

// Descriptive statistics
summarize ICTHOME ICTSCH ENTUSE HOMESCH USESCH ICTCLASS ICTOUTSIDE COMPICT INTICT SOIAICT AUTICT METASPAM MATH SCIE READ GNDR AGE ESCS, detail

// OLS Regression: ICT availability on academic achievement
regress MATH ICTHOME male AGE ESCS
regress READ ICTHOME male AGE ESCS
regress SCIE ICTHOME male AGE ESCS

// OLS Regression: ICT general use on academic achievement
regress MATH ENTUSE HOMESCH USESCH male AGE ESCS
regress READ ENTUSE HOMESCH USESCH male AGE ESCS
regress SCIE ENTUSE HOMESCH USESCH male AGE ESCS

// OLS Regression: ICT subject related use on academic achievement
regress MATH ICTCLASS ICTOUTSIDE male AGE ESCS
regress READ ICTCLASS ICTOUTSIDE male AGE ESCS
regress SCIE ICTCLASS ICTOUTSIDE male AGE ESCS

// OLS Regression: ICT engagement on academic achievement
regress MATH COMPICT INTICT SOIAICT AUTICT male AGE ESCS
regress READ COMPICT INTICT SOIAICT AUTICT male AGE ESCS
regress SCIE COMPICT INTICT SOIAICT AUTICT male AGE ESCS
