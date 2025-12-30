clear all
set more off
capture log close
cd "C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models"
log using "PubertyOrg.log", replace

***************************************************************************
* ORGANIZE FINAL FILES FOR IMPORT
***************************************************************************

*
* Clean girls log models
*

***child***
***PDS***
clear
use "Working data\girls_childPDS_logisticmodels_251230.dta"
summ p_a p_lambda
rename p_lambda yfPDStiming
rename p_a yfPDStempo
order ID yfPDStiming yfPDStempo
save "Working data\yfPDS_251230.dta", replace

***parent***
***PDS***
clear
use "Working data\girls_parentPDS_logisticmodels_251230.dta"
summ p_a p_lambda
rename p_lambda pfPDStiming
rename p_a pfPDStempo
order ID pfPDStiming pfPDStempo
save "Working data\pfPDS_251230.dta", replace

***merge within gender***
clear
use "Working data\yfPDS_251230.dta"
merge 1:1 ID using "Working data\pfPDS_251230.dta", gen(mpfPDS)

save "Final data\fPDS_251230.dta", replace

*
* Clean boy's log model!
*

***child***
***PDS***
clear
use "Working data\boys_childPDS_logisticmodels_251230.dta"
summ p_a p_lambda
rename p_lambda ymPDStiming
rename p_a ymPDStempo
order ID ymPDStiming ymPDStempo
save "Working data\ymPDS_251230.dta", replace

***parent***
***PDS***
clear
use "Working data\boys_parentPDS_logisticmodels_251230.dta"
summ p_a p_lambda
rename p_lambda pmPDStiming
rename p_a pmPDStempo
order ID pmPDStiming pmPDStempo
save "Working data\pmPDS_251230.dta", replace

***merge within gender***
clear
use "Working data\ymPDS_251230.dta"
merge 1:1 ID using "Working data\pmPDS_251230.dta", gen(mpmPDS)

save "Final data\mPDS_251230.dta", replace

*
*Merge estimates
*
clear
use "Final data\fPDS_251230.dta"
append using "Final data\mPDS_251230.dta"

save "Final data\PDS_251230.dta", replace





















