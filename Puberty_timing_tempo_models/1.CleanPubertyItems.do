clear all
set more off
capture log close
cd "C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models"
log using "PubertyCleaning.log", replace

***************************************************************************
* Created by NC on 12/30/25
*
* Last revised by NC on 12/30/25
* 
* Function:
* This do file will be to code core variables for the ABCD puberty papers
*  
* Source ABCD Release:
* 6.0 
*
* Source Datasets:
* ph_y_pds.parquet
* ph_p_pds.parquet
* ab_p_demo.parquet
*
* Updated Dataset:
* 
* Data Notes:
* 12/30/25: Removed folks who were missing more than 2 items on the PDS. 
*  
***************************************************************************
* IMPORT ALL DATA AND SAVE AS STATA FILE
***************************************************************************

***Parent PDS***
clear
import delimited "Raw data\ph_p_pds.tsv", delimiter("\t") clear
save "Working data\ph_p_pds.dta", replace

***Youth PDS***
clear
import delimited "Raw data\ph_y_pds.tsv", delimiter("\t") clear
save "Working data\ph_y_pds.dta", replace

***age***
clear
import delimited "Raw data\ab_p_demo.tsv", delimiter("\t") clear
save "Working data\ab_p_demo.dta", replace

***sex***
clear
import delimited "Raw data\ab_g_stc.tsv", delimiter("\t") clear
save "Working data\ab_g_stc.dta", replace

***************************************************************************
* CLEAN DATA
***************************************************************************

*
* Clean age
*

clear
use "Working data\ab_p_demo.dta"

rename participant_id ID /**rename for readability*/

tab session_id, m
codebook session_id

gen year = .
replace year = 0 if session_id == "ses-00A"
replace year = 1 if session_id == "ses-01A"
replace year = 2 if session_id == "ses-02A"
replace year = 3 if session_id == "ses-03A"
replace year = 4 if session_id == "ses-04A"
replace year = 5 if session_id == "ses-05A"
replace year = 6 if session_id == "ses-06A"

tab1 session_id year, m
drop session_id

keep ID year ab_p_demo_age ab_p_demo_dtt
rename ab_p_demo_age age
rename ab_p_demo_dtt intdatefromage

order ID year age intdatefromage

drop intdatefromage

save "Working data\demosage.dta", replace

*
*Clean static variables (sex, family relationships)
*

clear
use "Working data\ab_g_stc.dta"

rename participant_id ID /**rename for readability*/

keep ID ab_g_stc__design_id__fam ab_g_stc__cohort_sex

rename ab_g_stc__design_id__fam FAMID
rename ab_g_stc__cohort_sex male

tab male, m
*
*        male|      Freq.     Percent        Cum.
*------------+-----------------------------------
*          1 |      6,190       52.16       52.16
*          2 |      5,678       47.84      100.00
*------------+-----------------------------------
*      Total |     11,868      100.00

recode male (1=1) (2=0) (.=.)
tab male, m

save "Working data\demossex.dta", replace

*
* Clean parent data
*

***open parent puberty dataset***
clear
use "Working data\ph_p_pds.dta"

keep participant_id session_id ph_p_pds_dtt ph_p_pds_age ph_p_pds_001 ph_p_pds_002 ph_p_pds_003 ph_p_pds__f_001 ph_p_pds__f_002 ph_p_pds__m_001 ph_p_pds__m_002

rename participant_id ID /**rename for readability*/
rename ph_p_pds_dtt DATE

tab session_id, m
codebook session_id

gen year = .
replace year = 0 if session_id == "ses-00A"
replace year = 1 if session_id == "ses-01A"
replace year = 2 if session_id == "ses-02A"
replace year = 3 if session_id == "ses-03A"
replace year = 4 if session_id == "ses-04A"
replace year = 5 if session_id == "ses-05A"
replace year = 6 if session_id == "ses-06A"

tab1 session_id year, m
drop session_id

tab ph_p_pds_age, m
rename ph_p_pds_age pubage

tab1 ph_p_pds_001 ph_p_pds_002 ph_p_pds_003 ph_p_pds__f_001 ph_p_pds__f_002 ph_p_pds__m_001 ph_p_pds__m_002, m

rename ph_p_pds_001    ppds1
rename ph_p_pds_002    ppds2
rename ph_p_pds_003    ppds3
rename ph_p_pds__m_001 ppdsm5
rename ph_p_pds__m_002 ppdsm4
rename ph_p_pds__f_001 ppdsf4
rename ph_p_pds__f_002 ppdsf5 

label variable ppds1 "growth in height"
label variable ppds2 "body hair"
label variable ppds3 "skin changes"
label variable ppdsm4 "voice deepening"
label variable ppdsm5 "facial hair"
label variable ppdsf4 "breast development"
label variable ppdsf5 "menstruate"

tab1 ppds*, m

foreach v of varlist ppds* {
    // If string: map text codes to missing, then destring
    capture confirm string variable `v'
    if !_rc {
        // normalize common text codes to empty (string-missing)
        replace `v' = "" if inlist(strtrim(strlower(`v')), "n/a", "na", ".")
        replace `v' = "" if inlist(strtrim(`v'), "99", "999")
        destring `v', replace  // converts to numeric; empties become .
    }

    // If numeric: set 99/999 to numeric missing
    capture confirm numeric variable `v'
    if !_rc {
        replace `v' = . if inlist(`v', 99, 999)
    }
}

tab1 ppds*, m

***recode menarche correctly***
recode ppdsf5 (0=1)(1=4)

label define pds_labels 1 "has not yet started" 2 "have barely started" 3 "definitely underway" 4 "seems complete"

label values ppds* pds_labels

tab1 ppds*, m

order ID DATE year pubage ppds1 ppds2 ppds3 ppdsf4 ppdsf5 ppdsm5 ppdsm4 

***merge in sex***
merge m:1 ID using "Working data\demossex.dta", gen(mergesex)
tab mergesex, m
order ID FAMID DATE male year pubage ppds1 ppds2 ppds3 ppdsf4 ppdsf5 ppdsm5 ppdsm4
list ID FAMID DATE male year pubage ppds1 ppds2 ppds3 ppdsf4 ppdsf5 ppdsm5 ppdsm4 if mergesex==2

***merge age***
merge 1:1 ID year using "Working data\demosage.dta", gen(mergeage)
order ID FAMID DATE male year age pubage ppds1 ppds2 ppds3 ppdsf4 ppdsf5 ppdsm5 ppdsm4
list ID FAMID DATE male year age pubage ppds1 ppds2 ppds3 ppdsf4 ppdsf5 ppdsm5 ppdsm4 if mergeage==2

***organize data***
drop age mergesex mergeage
rename pubage age

tab male, m

drop if male ==.
drop if year==. 

order ID FAMID DATE male year age ppds1 ppds2 ppds3 ppdsf4 ppdsf5 ppdsm5 ppdsm4

***save out initial parent puberty estimates***
save "Working data\parentpuberty_long251230.dta", replace

***save out all parent puberty estimates for girls***
clear 
use "Working data\parentpuberty_long251230.dta"

drop if male == 1
drop ppdsm4 ppdsm5 
sort ID year FAMID 

***create overall status***
egen pfPDSm=rowmean(ppds1 ppds2 ppds3 ppdsf4 ppdsf5)
egen misspub = rowmiss(ppds1 ppds2 ppds3 ppdsf4 ppdsf5)
tab1 misspub, m
replace pfPDSm=    . if misspub==3 | misspub==4 | misspub==5

drop misspub

save "Working data\parentpubertygirls_long251230.dta", replace

***save out all parent puberty estimates for boys***
clear 
use "Working data\parentpuberty_long251230.dta"

drop if male == 0
drop ppdsf4 ppdsf5
sort ID year

***create overall status***
egen pmPDSm=rowmean(ppds1 ppds2 ppds3 ppdsm4 ppdsm5)
egen misspub = rowmiss(ppds1 ppds2 ppds3 ppdsm4 ppdsm5)
tab1 misspub, m
replace pmPDSm=    . if misspub==3 | misspub==4 | misspub==5

drop misspub

save "Working data\parentpubertyboys_long251230.dta", replace

*
* Clean youth data below
*

***open parent puberty dataset***
clear
use "Working data\ph_y_pds.dta"

keep participant_id session_id ph_y_pds_dtt ph_y_pds_age ph_y_pds_001 ph_y_pds_002 ph_y_pds_003 ph_y_pds__f_001 ph_y_pds__f_002 ph_y_pds__m_001 ph_y_pds__m_002

rename participant_id ID /**rename for readability*/
rename ph_y_pds_dtt DATE

tab session_id, m
codebook session_id

gen year = .
replace year = 0 if session_id == "ses-00A"
replace year = 1 if session_id == "ses-01A"
replace year = 2 if session_id == "ses-02A"
replace year = 3 if session_id == "ses-03A"
replace year = 4 if session_id == "ses-04A"
replace year = 5 if session_id == "ses-05A"
replace year = 6 if session_id == "ses-06A"

tab1 session_id year, m
drop session_id

tab ph_y_pds_age, m
rename ph_y_pds_age pubage

tab1 ph_y_pds_001 ph_y_pds_002 ph_y_pds_003 ph_y_pds__f_001 ph_y_pds__f_002 ph_y_pds__m_001 ph_y_pds__m_002, m

rename ph_y_pds_001    ypds1
rename ph_y_pds_002    ypds2
rename ph_y_pds_003    ypds3
rename ph_y_pds__m_001 ypdsm5
rename ph_y_pds__m_002 ypdsm4
rename ph_y_pds__f_001 ypdsf4
rename ph_y_pds__f_002 ypdsf5 

label variable ypds1  "growth in height"
label variable ypds2  "body hair"
label variable ypds3  "skin changes"
label variable ypdsm4 "voice deepening"
label variable ypdsm5 "facial hair"
label variable ypdsf4 "breast development"
label variable ypdsf5 "menstruate"

tab1 ypds*, m

foreach v of varlist ypds* {
    // If string: map text codes to missing, then destring
    capture confirm string variable `v'
    if !_rc {
        // normalize common text codes to empty (string-missing)
        replace `v' = "" if inlist(strtrim(strlower(`v')), "n/a", "na", ".")
        replace `v' = "" if inlist(strtrim(`v'), "99", "999", "777")
        destring `v', replace  // converts to numeric; empties become .
    }

    // If numeric: set 99/999 to numeric missing
    capture confirm numeric variable `v'
    if !_rc {
        replace `v' = . if inlist(`v', 99, 777, 999)
    }
}

tab1 ypds*, m

***recode menarche correctly***
recode ypdsf5 (0=1)(1=4)

label define pds_labels 1 "has not yet started" 2 "have barely started" 3 "definitely underway" 4 "seems complete"

label values ypds* pds_labels

tab1 ypds*, m

order ID year pubage ypds1 ypds2 ypds3 ypdsf4 ypdsf5 ypdsm5 ypdsm4 

***merge in sex***
merge m:1 ID using "Working data\demossex.dta", gen(mergesex)
tab mergesex, m
order ID FAMID male year pubage ypds1 ypds2 ypds3 ypdsf4 ypdsf5 ypdsm4 ypdsm5 
list ID FAMID male year pubage ypds1 ypds2 ypds3 ypdsf4 ypdsf5 ypdsm5 ypdsm4 if mergesex==2

***merge age***
merge 1:1 ID year using "Working data\demosage.dta", gen(mergeage)
order ID FAMID male year age pubage ypds1 ypds2 ypds3 ypdsf4 ypdsf5 ypdsm4 ypdsm5 
list ID FAMID male year age pubage ypds1 ypds2 ypds3 ypdsf4 ypdsf5 ypdsm4 ypdsm5 if mergeage==2

***organize data***
drop age mergesex mergeage
rename pubage age

tab male, m

drop if male ==.
drop if year==.

order ID FAMID DATE male year age ypds1 ypds2 ypds3 ypdsf4 ypdsf5 ypdsm4 ypdsm5 

***save out initial youth puberty estimates***
save "Working data\youthpuberty_long251230.dta", replace

***save out all youth puberty estimates for girls***
clear 
use "Working data\youthpuberty_long251230.dta"

drop if male == 1
drop ypdsm4 ypdsm5 
sort ID year FAMID 

***create overall status***
egen yfPDSm=rowmean(ypds1 ypds2 ypds3 ypdsf4 ypdsf5)
egen misspub = rowmiss(ypds1 ypds2 ypds3 ypdsf4 ypdsf5)
tab1 misspub, m
replace yfPDSm=    . if misspub==3 | misspub==4 | misspub==5

drop misspub

save "Working data\youthpubertygirls_long251230.dta", replace

***save out all youth puberty estimates for boys***
clear 
use "Working data\youthpuberty_long251230.dta"

drop if male == 0
drop ypdsf4 ypdsf5
sort ID year

***create overall status***
egen ymPDSm=rowmean(ypds1 ypds2 ypds3 ypdsm4 ypdsm5)
egen misspub = rowmiss(ypds1 ypds2 ypds3 ypdsm4 ypdsm5)
tab1 misspub, m
replace ymPDSm=    . if misspub==3 | misspub==4 | misspub==5

drop misspub

save "Working data\youthpubertyboys_long251230.dta", replace











