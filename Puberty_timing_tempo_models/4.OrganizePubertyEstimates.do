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

summ yfPDStiming yfPDStempo pfPDStiming pfPDStempo

*    Variable |        Obs        Mean    Std. dev.       Min        Max
*-------------+---------------------------------------------------------
* yfPDStiming |      5,470    12.21195    .9647618   9.311014   15.13782
*  yfPDStempo |      5,470    .5786892    .1340796  -.0170147    .948642
* pfPDStiming |      5,486    11.92854    1.100897   8.470568   15.37158
*  pfPDStempo |      5,486    .5478074    .1495698   -.160673   .9838472

gen yfPDStiming_trim = yfPDStiming 
gen yfPDStempo_trim = yfPDStempo 
gen pfPDStiming_trim = pfPDStiming 
gen pfPDStempo_trim = pfPDStempo

replace yfPDStiming_trim =. if (yfPDStiming > 12.21195 + 3*.9647618) | (yfPDStiming < 12.21195 - 3*.9647618)
replace yfPDStempo_trim  =. if (yfPDStempo  > .5786892 + 3*.1340796) | (yfPDStempo  < .5786892 - 3*.1340796)
replace pfPDStiming_trim =. if (pfPDStiming > 11.92854 + 3*1.100897) | (pfPDStiming < 11.92854 - 3*1.100897)
replace pfPDStempo_trim  =. if (pfPDStempo  > .5478074 + 3*.1495698)   | (pfPDStempo  < .5478074 - 3*.1495698)
list ID yfPDStiming if (yfPDStiming_trim==. & yfPDStiming !=.)
list ID yfPDStempo if (yfPDStempo_trim==. & yfPDStempo !=.)

summ yfPDStiming_trim yfPDStempo_trim pfPDStiming_trim pfPDStempo_trim

summ ymPDStiming ymPDStempo pmPDStiming pmPDStempo

*    Variable |        Obs        Mean    Std. dev.       Min        Max
*-------------+---------------------------------------------------------
* ymPDStiming |      6,024    14.02491    1.036901   10.62326   17.98074
*  ymPDStempo |      6,024    .3858667     .096863  -.0269149   .6845728
* pmPDStiming |      6,024    14.01258     1.05301    9.82162   18.22447
*  pmPDStempo |      6,024    .4716762    .1223303  -.3119603   .8396984

gen ymPDStiming_trim = ymPDStiming 
gen ymPDStempo_trim = ymPDStempo 
gen pmPDStiming_trim = pmPDStiming 
gen pmPDStempo_trim = pmPDStempo

replace ymPDStiming_trim =. if (ymPDStiming > 14.02491 + 3*1.036901)  | (ymPDStiming < 14.02491 - 3*1.036901)
replace ymPDStempo_trim  =. if (ymPDStempo  > .3858667  + 3*.096863) | (ymPDStempo  < .3858667  - 3*.096863)
replace pmPDStiming_trim =. if (pmPDStiming > 14.01258 + 3*1.05301)  | (pmPDStiming < 14.01258 - 3*1.05301)
replace pmPDStempo_trim  =. if (pmPDStempo  > .4716762 + 3*.1223303)  | (pmPDStempo  < .4716762 - 3*.1223303)
list ymPDStiming if (ymPDStiming_trim==. & ymPDStiming !=.)
list ID ymPDStempo if (ymPDStempo_trim==. & ymPDStempo !=.)

summ ymPDStiming_trim ymPDStempo_trim pmPDStiming_trim pmPDStempo_trim

gen male = .  
replace male=1 if ymPDStiming !=. | ymPDStempo !=. | pmPDStiming !=. | pmPDStempo !=.
replace male=0 if yfPDStiming !=. | yfPDStempo !=. | pfPDStiming !=. | pfPDStempo !=.

gen yPDStiming=.
replace yPDStiming=yfPDStiming_trim if male==0
replace yPDStiming=ymPDStiming_trim if male==1

gen pPDStiming=.
replace pPDStiming=pfPDStiming_trim if male==0
replace pPDStiming=pmPDStiming_trim if male==1

gen yPDStempo=.
replace yPDStempo=yfPDStempo_trim if male==0
replace yPDStempo=ymPDStempo_trim if male==1

gen pPDStempo=.
replace pPDStempo=pfPDStempo_trim if male==0
replace pPDStempo=pmPDStempo_trim if male==1

drop yfPDStiming yfPDStempo pfPDStiming pfPDStempo mpfPDS ymPDStiming ymPDStempo pmPDStiming pmPDStempo mpmPDS

rename yfPDStiming_trim yfPDStiming 
rename yfPDStempo_trim yfPDStempo 
rename pfPDStiming_trim pfPDStiming 
rename pfPDStempo_trim pfPDStempo
rename ymPDStiming_trim ymPDStiming
rename ymPDStempo_trim ymPDStempo 
rename pmPDStiming_trim pmPDStiming 
rename pmPDStempo_trim pmPDStempo

order ID male yPDStiming pPDStiming yPDStempo pPDStempo yfPDStiming yfPDStempo pfPDStiming pfPDStempo ymPDStiming ymPDStempo pmPDStiming pmPDStempo

save "Final data\PDS_251230.dta", replace





















