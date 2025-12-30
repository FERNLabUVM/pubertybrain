
cd "C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models"

*
*Girls (child)
*

***PDS***
clear
use "Working data\ygirlsPDSclean_251230.dta"
decode ID, gen(ID_str)
drop ID
rename ID_str ID
save "Working data\ygirlsPDSclean_251230.dta", replace

*
*Girls (parent)
*

***PDS***
clear
use "Working data\pgirlsPDSclean_251230.dta"
decode ID, gen(ID_str)
drop ID
rename ID_str ID
save "Working data\pgirlsPDSclean_251230.dta", replace

*
*Boys (child)
*

***PDS***
clear
use "Working data\yboysPDSclean_251230.dta"
decode ID, gen(ID_str)
drop ID
rename ID_str ID
save "Working data\yboysPDSclean_251230.dta", replace

*
*Boys (parent)
*

***PDS***
clear
use "Working data\pboysPDSclean_251230.dta"
decode ID, gen(ID_str)
drop ID
rename ID_str ID
save "Working data\pboysPDSclean_251230.dta", replace
