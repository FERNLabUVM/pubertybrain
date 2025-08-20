clear all
set more off
capture log close
cd "Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels"
cd "/Volumes/bl-pbs-chakulab/Projects/Data/ABCD/Syntax/Puberty/6.0"
log using "PubertyOrg.log", replace

***************************************************************************
* Created by NC on 8/20/2025
*
* Last revised by NC on 8/20/25
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
*
* 
***************************************************************************
* ORGANIZE FINAL FILES FOR IMPORT
***************************************************************************

*
* Clean girl's sitar model!
*

clear
use "ygirlsSITARmod3.dta"

tab ID, m
tab b, m
tab c, m

tab APV_years, m
tab pv, m

tab APV_flag, m
tab APV_years if APV_flag=="high"
tab APV_years if APV_flag=="low"

drop if APV_years < 7 | APV_years > 20 

rename APV_years gtiming
rename pv gtempo

drop b c APV_flag

save "girlpub.dta", replace

*
* Clean boy's log model!
*

clear
use "boys_childPDS_logisticmodels_250826.dta"

tab p_a, m
tab p_lambda, m

summ p_a p_lambda

rename p_lambda btiming
rename p_a btempo

save "boypub.dta", replace























