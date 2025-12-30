clear all
set more off
capture log close
cd "C:\Users\nchaku\Documents\GitHub\pubertybrain\Working_memory"
log using "WMCleaning.log", replace

***************************************************************************
* Created by NC on 12/30/25
*
* Last revised by NC on 12/30/25
*
* Data Notes:
* 12/30/25: 
*  
***************************************************************************
* CLEAN DATA
***************************************************************************

***open data***
use "dataset.dta"

rename participant_id ID /**rename for readability*/

tab session_id, m
codebook session_id

rename session_id year
rename ab_g_dyn_0002 age
rename ab_g_dyn_0013 SITE
rename ab_g_stc_0001 FAMID

order ID FAMID SITE year age

drop nc_y_nihtb_0067 nc_y_nihtb_0066 nc_y_nihtb_0078 nc_y_nihtb_0080 nc_y_nihtb_0081 nc_y_nihtb_0083 nc_y_nihtb_0068 nc_y_nihtb_0069 nc_y_nihtb_0070 nc_y_nihtb_0071 nc_y_nihtb_0072 nc_y_nihtb_0073 nc_y_nihtb_0074 nc_y_nihtb_0075 nc_y_nihtb_0076

rename nc_y_nihtb_0082 WRKMEM
rename nc_y_nihtb_0077 REMOTE
recode REMOTE (1=1) (2=0) (3=.) (4=.)
tab REMOTE, m

save "WRKMEM_251230.dta", replace











