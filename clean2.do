* ================================================================
* This program merges school-level data with a separate survey
* data sets used - KAPgirls.dta.dta
*                - KAPboys.dta
*                - school_info
* intermediate   - KAPall.dta
*                - religion_byschool.dta
* output         - school_religmerge.dta
* ================================================================

use KAPgirls.dta, replace
append using KAPboys.dta

save KAPall.dta, replace

use KAPall.dta, replace
rename schoolID schoolid

gen religiosity = 1 if q52_church >= 75 & q52_church != .
replace religiosity = 0 if q52_church < 75
drop if religiosity == . 

gen religiosity_2 = 1 if q52_church == 100 
replace religiosity_2 = 0 if q52_church < 100
drop if religiosity_2 == . 

gen religiosity_3 = 1 if q52_church >0 & q52_church != .
replace religiosity_3 = 0 if q52_church == 0
drop if religiosity_3 == . 

/*
label define religval ///
1  "protestant" ///
2  "catholic" ///
3  "muslim" ///
4  "hindu"

label values q12_religion religval

decode q12_religion, gen(relig_str) 

for any protestant catholic muslim hindu: gen X=.

local varlist "protestant catholic muslim hindu"
foreach var of varlist protestant catholic muslim hindu{
 replace `religion' = 1 if relig_str == "`religion'"
 replace `religion' = 0 if relig_str != "`religion'"
 }
 */

gen protestant = 1 if q12_religion ==1
replace protestant = 0 if q12_religion !=1
gen catholic = 1 if q12_religion ==2
replace catholic = 0 if q12_religion !=2
gen muslim = 1 if q12_religion ==3
replace muslim = 0 if q12_religion !=3
gen hindu = 1 if q12_religion ==4
replace hindu = 0 if q12_religion !=4

collapse protestant catholic muslim hindu ///
religiosity religiosity_2 religiosity_3, ///
by(schoolid)

save religion_byschool, replace

use school_info.dta, replace
merge m:1 schoolid using religion_byschool.dta

save school_religmerge, replace
