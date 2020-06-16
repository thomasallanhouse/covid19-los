
//  This file uses CHESS data, 26th May data file, FOR COMPARISON PAPER.

////////////////////////////////////////////////////////////////////////////////
//
//                         Preparation
//
////////////////////////////////////////////////////////////////////////////////

// age categories and dummies
gen age1_49  = ageyear4==1 if ageyear4 !=.
gen age50_64 = ageyear4==2 if ageyear4 !=.
gen age65_74 = ageyear4==3 if ageyear4 !=.
gen age75up  = ageyear4==4 if ageyear4 !=.      // This is the omitted reference category

//// dummy for female sex
gen female = sex == 1

//// Date of admission dummies
drop if weekofadmission == 21
// for the model
gen wk12 	= cond(weekofadmission == 12, 1, 0) // This is the omitted reference category
gen wk13 	= cond(weekofadmission == 13, 1, 0)
gen wk14 	= cond(weekofadmission == 14, 1, 0)
gen wk15 	= cond(weekofadmission == 15, 1, 0)
gen wk16 	= cond(weekofadmission == 16, 1, 0)
gen wk17 	= cond(weekofadmission == 17, 1, 0)
gen wk18 	= cond(weekofadmission == 18, 1, 0)
gen wk19_20 = cond(weekofadmission == 19 | weekofadmission == 20, 1, 0)
// for the results table
gen wk12_14 = cond(weekofadmission >= 12 & weekofadmission <= 14, 1, 0)
gen wk15_20 = cond(weekofadmission >= 15 & weekofadmission <= 20, 1, 0)
tab weekofadmission wk15_20, miss

//// Binary icu entry
gen icu = wasthepatientadmittedtoicu == 3

//// Check covariates for missingness
gen miss = cond(female !=. & ageyear4 !=. & wk12 !=. & icu !=., 0, 1)
tab miss // no non-missing


//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//
//                			models
//
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

// 1. LoS hospital admission -> ICU entry, 	  icu patients
// 2. LoS ICU entry -> ICU exit, 			  icu patients
// 3. LoS hospital admission -> finaloutcome, icu patients
// 4. Los hospital admission -> finaloutcome, NON-icu patients


//////////////////////////////////////////////////////////////////////////////
// 1. Hospital admission -> ICU entry, icu patients
//////////////////////////////////////////////////////////////////////////////

stset HOSPTOICU, failure(dummy_durationhospitalicu = 1) if(icu==1)
stdescribe
//N = 2983 observations, of which N = 0 are censored
// Mean 2.1, Med 0.67 

// Proportion of duration variance associated with predictors
reg HOSPTOICU wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female ///
 age1_49 age50_64 age65_74 if _st == 1
// r2 = 0.0052

// Weibull
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(weibull) time vce(cluster trustcode)
//Log pseudolikelihood =   -5220.1151
// shape parameter p = -0.26, i.e basehazard is declining over time.
predict los1w, mean

// Log-log
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(loglog) time vce(cluster trustcode)
// -4922.7935 - much better
predict los1ll, mean

sum los1w los1ll, det 

//////////////////////////////////////////////////////////////////////////////
// 2. ICU entry -> ICU exit, icu patients
//////////////////////////////////////////////////////////////////////////////

stset ICUduration, failure(dummy_outcome_icu = 1) if(icu==1)
stdescribe
//N = 1809 observations, of which N = 108 are censored
// Mean 11.6, Med 8.0

// Proportion of duration variance associated with predictors
reg  ICUduration wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female ///
 age1_49 age50_64 age65_74 if _st == 1
// r2 = 0.0574

// Weibull
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(weibull) time vce(cluster trustcode)
// Log pseudolikelihood =   -2772.4061
// shape parameter p is ns, i.e. p = 1, basehazard is constant, i.e. exponential model
predict los2w, mean

// Log-log
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(loglog) time vce(cluster trustcode)
// -2849.0924 - much worse
predict los2ll, mean

sum los2w los2ll, det
 

//////////////////////////////////////////////////////////////////////////////
// 3. Hospital admission -> finaloutcome, icu patients
//////////////////////////////////////////////////////////////////////////////

stset HOSPtoOUTCOME, failure(dummy_hospitaloutcome = 1) if(icu==1)
stdescribe
//N = 2555 observations, of which N = 43 are censored
// Mean 16.0, Med 13

// Proportion of duration variance associated with predictors
reg HOSPtoOUTCOME wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female ///
 age1_49 age50_64 age65_74 if _st == 1
// r2 = 0.0510

// Weibull
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(weibull) time vce(cluster trustcode)
//p = 1.43, LL -3104.0964
predict los3w, mean

// Log-log
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(loglog) time vce(cluster trustcode)
// LL -3220.7425 - much worse
predict los3ll, mean

sum los3w los3ll, det

//////////////////////////////////////////////////////////////////////////////
// 4. Hospital admission -> finaloutcome, NON-icu patients
//////////////////////////////////////////////////////////////////////////////

stset HOSPtoOUTCOME, failure(dummy_hospitaloutcome = 1) if(icu==0)
stdescribe
//N = 2805 observations, of which N = 14 are censored
// Mean 8.4, Med 6

// Proportion of duration variance associated with predictors
reg HOSPtoOUTCOME wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female ///
 age1_49 age50_64 age65_74 if _st == 1
// r2 = 0.1023

// Weibull
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(weibull) time vce(cluster trustcode)
// LL  -4279.4595, p = 1.05, only just sig. pos.
predict los4w, mean

// Log-log
streg wk13 wk14 wk15 wk16 wk17 wk18 wk19_20 female age1_49 age50_64 age65_74 ///
 , distribution(loglog) time vce(cluster trustcode)
// LL  -4459.6439 - much worse
predict los4ll, mean

sum los4w los4ll


///////////////////////////////////////////////////////////////////////////////
///
///                    LOS predictions
///
///////////////////////////////////////////////////////////////////////////////

// For 1., Log-log model was actually a better-fitting model.
// A quick peek at the different (Weibull vs. log-log) estimates for Hosp->ICU
graph twoway (kdensity los1w, bwidth(0.25))(kdensity los1ll, bwidth(0.25))
// Distribution is shifted by about half a day.
// We will use Weibull estimates anyway, for the purposes of comparison with 
//  the other models (which used Weibull).


// 1. Hosp->ICU
table ageyear4 wk15_20 , ///
 c(mean los1w sd los1w n los1w) col row
// 2. IUC->ICU
table ageyear4 wk15_20 , ///
 c(mean los2w sd los2w n los2w) col row
// 3. HOSP->FINOUTCOME ICU == 1
table ageyear4 wk15_20 , ///
 c(mean los3w sd los3w n los3w) col row
// 4. HOSP->FINOUTCOME ICU == 0
table ageyear4 wk15_20 , ///
 c(mean los4w sd los4w n los4w) col row

 

///////////////////////////////////////////////////////////////////////////////
///
///                    Observed data
///
///////////////////////////////////////////////////////////////////////////////

//1. 
stset HOSPTOICU, failure(dummy_durationhospitalicu = 1) if(icu==1)
stdescribe
table ageyear4 wk15_20 if _st==1, ///
 c(mean HOSPTOICU sd HOSPTOICU n HOSPTOICU) col row

//2.
stset ICUduration, failure(dummy_outcome_icu = 1) if(icu==1)
stdescribe
table ageyear4 wk15_20 if _st==1, ///
 c(mean ICUduration sd ICUduration n ICUduration) col row

 //3.
stset HOSPtoOUTCOME, failure(dummy_hospitaloutcome = 1) if(icu==1)
stdescribe
table ageyear4 wk15_20 if _st==1, ///
 c(mean HOSPtoOUTCOME sd HOSPtoOUTCOME n HOSPtoOUTCOME) col row

 //4. 
stset HOSPtoOUTCOME, failure(dummy_hospitaloutcome = 1) if(icu==0)
stdescribe
table ageyear4 wk15_20 if _st==1, ///
 c(mean HOSPtoOUTCOME sd HOSPtoOUTCOME n HOSPtoOUTCOME) col row

