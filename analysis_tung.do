*================================================================
*  Replication of Duflo, E., Dupas, P., \& Kremer, M. (2015)  
*  Master do file 
*================================================================

* version 12

clear all
set more off
cd "\ADM\Replication"

global treatmentdummies="Uonly Honly UH"
global treatmentdummiesCT2="Uonly HnoCT UHnoCT HwithCT UHwithCT"
global controlsR="yrbirth_all yrbirth_missing date05v3 date07v2 schsize i.stratum"
global controls="yrbirth_all yrbirth_missing schsize i.stratum"
global controlsKAP="age agemissing schsize i.stratum"
global controlsB="yrbirth_all yrbirth_missing"

global varlist "presence evmar05v3 evpreg05v3 evpregunmar05v3 evunpregmar05v3 dropout07v2 evmar07v2 evpreg07v2 evpregunmar07v2 evunpregmar07v2"

* Merge school data and individual data. This is the same as the original do file    
do clean.do

* Merge school data with data from separate in-class survey. This is an extension. 
do clean2.do

* Incorporate religion into balance check
do extension_tab1.do

* Regression: Table 2A girls only
do replication_tab2a.do

* Regression: Table 2B girls only
do replication_tab2b.do

* Regression: Table A2 Attrition in short run and medium run surveys
do replication_taba2.do

* Regression: Table A3 Attrition in long run survey
do replication_taba3.do

/*
********************;
****FIGURE 1************;
********************;

use studysample_allmerged2, clear
collapse evpreg07v2 hsv2_positive, by(sch03v1 Uonly Honly UH sex) 
//sch03v1: primary school ID

gen group=1 if Uonly==1
replace group=2 if UH==1

foreach outcome in evpreg07v2 hsv2_positive  {
	local txt "Share of girls who dropped out within 5 years"
	if "`outcome'"=="evpreg07v2" local txt "Share of girls ever pregnant within 5 years"
	if "`outcome'"=="evpreg05v3" local txt "Share of girls ever pregnant within 3 years"
	if "`outcome'"=="child_by_16" local txt "Share of girls who had a child by age 16"
	if "`outcome'"=="hsv2_positive" local txt "Share of girls HSV2 positive after 7 years"
	if "`outcome'"=="reached8" local txt "Share of girls who reached 8th grade"
	twoway (kdensity `outcome' if sex==2 & Uonly==0 & Honly==0 & UH==0, lpat(longdash)) || ///
		(kdensity `outcome' if sex==2 & Uonly==0 & Honly==1 & UH==0, lpat(dash_dot) ) ||  ///
		(kdensity `outcome' if sex==2 & Uonly==0 & Honly==0 & UH==1, lpat(solid) lwidth(medthick)) ||  ///
		(kdensity `outcome' if sex==2 & Uonly==1 & Honly==0 & UH==0, lpat(dash) lwidth(medthick)), ///
		legend(col(2) row(2) label(1 "Control") label(4 "Stand-Alone Education Subsidy") ///
		 label(3 "Joint Program")  label(2 "Stand-Alone HIV education") size(small)) ///
		name(`outcome', replace) graphregion(color(white) fcolor(white)) nodraw /// 
		xtit(`txt', margin(medium)) ytit("Density") tit(" ")
	}	

ksmirnov evpreg07v2  if sex==2, by(group)
local p1=round(r(p_cor),0.001)

ksmirnov hsv2_positive  if sex==2, by(group)
local p2=round(r(p_cor), 0.001)
	
gr combine evpreg07v2 hsv2_positive, ///
col(1) rows(2) xcommon ysize(9) xsize(5.5) ///
graphregion(color(white) fcolor(white)) /// 
note("Notes: School-level averages." ///
"Two-sample Kolmogorov-Smirnov tests for equality of distribution between Stand-alone Education" "Subsidy and Joint Program:" "   p-value for share ever pregnant (top panel): `p1'**" "   p-value for share HSV2 positive (bottom panel): `p2'**", ///
size(vsmall)) ///
saving("Fig1.gph", replace)

*/

**************;
*TABLE 1 PANEL A;
****************;
/*
use school_info.dta, clear

gen var=""
for any  mean_U sd_U mean_H sd_H  mean_UH sd_UH  mean_control sd_control p_Uonly p_Honly p_UH p_UUH p_HUH  N: gen X=.
gen urban=0
replace urban=1 if situation<3

gen sexratio_teachers=Nfemale/(TOTteachers-Nfemale)

gen Honly=(HIVtreat==1) & (Utreat==0)
gen Uonly=(HIVtreat==0) & (Utreat==1)
gen UH=(HIVtreat==1) & (Utreat==1)

#delimit;
local vars=10;
for any  kcpe2003  schsize ratio02  latrine_2004 urban total_2km  
TOTteachers  meanage sexratio_teachers HIVtreat

 \ num 1/`vars':
 replace var="X" if _n==Y \
 reg X Uonly \
 replace N=e(N) if _n==Y \
 test Uonly=0 \
 replace p_Uonly=r(p) if _n==Y \
 reg X Honly \
 test Honly=0 \
 replace p_Honly=r(p) if _n==Y \
 reg X UH \
 test UH=0 \
 replace p_UH=r(p) if _n==Y \
 reg X Uonly Honly UH \
 test UH=Uonly \
 replace p_UUH=r(p) if _n==Y \
 test UH=Honly \
 replace p_HUH=r(p) if _n==Y \
 sum X if Utreat==1&HIVtreat==0 \
 replace mean_U=r(mean) if _n==Y \
 replace sd_U=r(sd) if _n==Y \
 sum X if Utreat==0&HIVtreat==1 \
 replace mean_H=r(mean) if _n==Y \
 replace sd_H=r(sd) if _n==Y \
 sum X if Utreat==1&HIVtreat==1 \
 replace mean_UH=r(mean) if _n==Y \
 replace sd_UH=r(sd) if _n==Y \
 sum X if Utreat==0 \
 replace mean_control=r(mean) if _n==Y \
 replace sd_control=r(sd) if _n==Y ;

#delimit cr
 
for var p_Uonly p_Honly p_UH p_UUH p_HUH  mean* sd*: replace X=round(X, 0.001)

//outsheet command does not work in Stata 15.0
keep var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control ///
p_Uonly p_Honly p_UH p_UUH p_HUH  N

drop if mean_U==.

dataout , save(table1a.tex) replace tex


outsheet var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control ///
p_Uonly p_Honly p_UH p_UUH p_HUH  N if _n<=`vars' ///
using "table1a.xls",replace
*/


*****************;
*TABLE 1 PANEL B;
******************;
use sampling_frame.dta, clear
gen age03_female=age03 if female==1&std03v1==6
gen age03_male=age03 if female==0&std03v1==6

gen femalecount=1 if female==1&std03v1==6
gen malecount=1 if female==0&std03v1==6

collapse  Utreat03v1 HIVtreat03v1 age03_female age03_male (sum) femalecount malecount , by(sch03v1)
gen sexratio_grade6=femalecount/malecount

gen Honly=(HIVtreat03v1==1) & (Utreat03v1==0)
gen Uonly=(HIVtreat03v1==0) & (Utreat03v1==1)
gen UH=(HIVtreat03v1==1) & (Utreat03v1==1)

gen var=""
for any  mean_U sd_U mean_H sd_H  mean_UH sd_UH  mean_control sd_control p_Uonly p_Honly p_UH p_UUH p_HUH  N: gen X=.

#delimit;
local vars=5;
for any femalecount malecount sexratio_grade6 age03_female age03_male 

 \ num 1/`vars':
 replace var="X" if _n==Y \
 reg X Uonly \
 replace N=e(N) if _n==Y \
 test Uonly=0 \
 replace p_Uonly=r(p) if _n==Y \
 reg X Honly \
 test Honly=0 \
 replace p_Honly=r(p) if _n==Y \
 reg X UH \
 test UH=0 \
 replace p_UH=r(p) if _n==Y \
 reg X Uonly Honly UH \
 test UH=Uonly \
 replace p_UUH=r(p) if _n==Y \
 test UH=Honly \
 replace p_HUH=r(p) if _n==Y \
 sum X if Utreat03v1==1&HIVtreat03v1==0 \
 replace mean_U=r(mean) if _n==Y \
 replace sd_U=r(sd) if _n==Y \
 sum X if Utreat03v1==0&HIVtreat03v1==1 \
 replace mean_H=r(mean) if _n==Y \
 replace sd_H=r(sd) if _n==Y \
 sum X if Utreat03v1==1&HIVtreat03v1==1 \
 replace mean_UH=r(mean) if _n==Y \
 replace sd_UH=r(sd) if _n==Y \
 sum X if Utreat03v1==0 \
 replace mean_control=r(mean) if _n==Y \
 replace sd_control=r(sd) if _n==Y ;
 
for var p_Uonly p_Honly p_UH p_UUH p_HUH mean* sd*: replace X=round(X, 0.001)

keep var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control /// 
p_Uonly p_Honly p_UH p_UUH p_HUH  N 

drop if mean_U ==.

dataout , save(table1b_tung.tex) replace tex

// outsheet does not work
/*
outsheet var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control 
p_Uonly p_Honly p_UH p_UUH p_HUH  N if _n<=`vars' using "table1b.xls",replace;
*/

*cannot link religion with pupilID

*************;
***TABLE 2: ROLL CALL RESULTS;
*****************;
/*

use studysample_allmerged2.dta, clear


gen HnoCT=Honly*(1-sampleCT103v1)
gen UHnoCT=UH*(1-sampleCT103v1)
gen HwithCT=Honly*(sampleCT103v1)
gen UHwithCT=UH*(sampleCT103v1)



foreach visit in 04v1 04v2 05v1 05v2 05v3 {
	gen presence`visit'=pres`visit'
	replace presence`visit'=0 if pres`visit'==2
	replace presence`visit'=0.5 if pres`visit'==3
	replace presence`visit'=. if pres`visit'>3
}

replace dropout05v3=0 if presence05v3==1
replace dropout05v3=. if evdead05v3==1

egen presence=rmean(presence04v1 presence04v2 presence05v1 presence05v2 presence05v3)

foreach date in 05v3 07v2 {
	replace evmar`date'=0 if evmar`date'==. & evpreg`date'==1
	gen evunpregmar`date'=(1-evpreg`date')*evmar`date'
	gen evpregunmar`date'=evpreg`date'*(1-evmar`date')
	gen marifchild`date'=evmar`date' if evpreg`date'==1
}

*/

use studysample_allmerged2.dta, clear

** WITHOUT CT;
#delimit;
local i 2;
while `i'>0 { ;
		sum  dropout05v3 if sex==`i' & group03v1=="C";
		gen mean=r(mean);
		xi: reg dropout05v3 ${treatmentdummies} ${controlsR} , cluster(sch03v1), if sex==`i';
		test Uonly=UH;
			local pval=r(p);
		test Honly=UH;
			local p2=r(p);
		test Honly=Uonly;
			local p3=r(p);
		test UH=Uonly+Honly;
			local p4=r(p);
		outreg2 ${treatmentdummies} using table2_`i'.xls, nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `pval', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;

		foreach var in ${varlist} { ;
			sum  `var' if sex==`i' & group03v1=="C";
			gen mean=r(mean);
			xi: reg `var' ${treatmentdummies} ${controlsR} , cluster(sch03v1), if sex==`i';
			if mean!=0 {;
		test Uonly=UH;
			local pval=r(p);
		test Honly=UH;
			local p2=r(p);
		test Honly=Uonly;
			local p3=r(p);
		test UH=Uonly+Honly;
			local p4=r(p);
					};
			if mean==0 {; 
				local pval=-99;
				local p2=-99;
				local p3=-99;
				local p4=-99;

			};
			cap outreg2 ${treatmentdummies} using table2_`i'.xls, nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean,  "U=UH", `pval', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		} ;
	
local i=`i'-1;
} ;
#delimit cr

*results confirmed**

/*
** WITH ALL CT TREATMENTS (Table 5 cols 4-5-6);
#delimit ;
local i 2;
while `i'>0 { ;
		sum  dropout05v3 if sex==`i' & group03v1=="C";
		gen mean=r(mean);
		xi: reg dropout05v3 ${treatmentdummiesCT2} ${controlsR} , cluster(sch03v1), if sex==`i';
		test Uonly=UHnoCT;
			local pval=r(p);
		test HnoCT=UHnoCT;
			local p2=r(p);
		test HnoCT=Uonly;
			local p3=r(p);
		test UHnoCT=Uonly+HnoCT;
			local p4=r(p);
		outreg2 ${treatmentdummiesCT2} using table2CT2_`i'.xls, nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `pval', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;

		foreach var in ${varlist} { ;
			sum  `var' if sex==`i' & group03v1=="C";
			gen mean=r(mean);
			xi: reg `var' ${treatmentdummiesCT2} ${controlsR} , cluster(sch03v1), if sex==`i';
			if mean!=0 {;
		test Uonly=UHnoCT;
			local pval=r(p);
		test HnoCT=UHnoCT;
			local p2=r(p);
		test HnoCT=Uonly;
			local p3=r(p);
		test UHnoCT=Uonly+HnoCT;
			local p4=r(p);
					};
			if mean==0 {; 
				local pval=-99;
				local p2=-99;
				local p3=-99;
				local p4=-99;

			};
			cap outreg2 ${treatmentdummiesCT2} using table2CT2_`i'.xls, nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean,  "U=UH", `pval', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		} ;
	
local i=`i'-1;
} ;

#delimit cr

*/

* LR impacts ?

#delimit ;

use studysample_allmerged2.dta, clear;
	
local depvars 17;
for any 
	reached8	
	an_grades_completed		
	an_ever_mar 
	an_ever_fert 		
	an_ever_fert_unmar 	 
	an_ever_unfert_mar 
	child_by_16		
	hiv_positive 		
	hsv2_positive 			
	an_everhadsex 
	an_reports_abst
	an_reports_faith
	an_reports_cond
	an_want_1stpreg 		 		
	an_age_old_part 	 			  
	an_age_firstsex 	        	
	an_condomlastsex 		

 \ num 1/`depvars':
	global depvarY="X" ;

foreach num of numlist 1/8 10/17 {;
	gen date`num'=Q_realdate;
	gen year`num'=Q_year;
};
foreach num in 9 {;
	gen date`num'=LOG_bd_BDrealdate;
	gen year`num'=BD_year;

	};

 
local j 1;
while `j' <= `depvars' {;

	if `j'==1  {;
		local append_replace="replace";
		};
	if `j' !=1  {;
		local append_replace="append";
		};


	reg ${depvar`j'} [pw=weight_sample] if group03v1=="C" & sex==2;
	local meanf=_b[_cons];

	reg ${depvar`j'} [pw=weight_sample] if group03v1=="C" & sex==1;
	local meanm=_b[_cons];

	*** NO CT;
			*females;
			xi: reg ${depvar`j'} ${treatmentdummies} date`j' year`j' ${controls} [pw=weight_sample],cluster(schoolid), if sex==2 ;
				test Uonly=UH;
					local p1=r(p);
				test Honly=UH;
					local p2=r(p);
				test Honly=Uonly;
					local p3=r(p);
				test UH=Uonly+Honly;
					local p4=r(p);
				quietly outreg2 ${treatmentdummies}  using LRimpact_2.xls, nonote se /*sigsymb(***,**,*)*/ `append_replace'  nolabel 
					 addstat("control mean", `meanf', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3) bdec(3);


			*males;
			xi: reg ${depvar`j'} ${treatmentdummies} date`j' year`j' ${controls} [pw=weight_sample] ,cluster(schoolid), if sex==1;
				test Uonly=UH;
					local p1=r(p);
				test Honly=UH;
					local p2=r(p);
				test Honly=Uonly;
					local p3=r(p);
				test UH=Uonly+Honly;
					local p4=r(p);
				quietly outreg2 ${treatmentdummies}  using LRimpact_1.xls, nonote se /*sigsymb(***,**,*)*/ `append_replace'  nolabel 
					 addstat("control mean", `meanm', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3) bdec(3);

/*
		*** WITH ALL CT TREATMENTS (Table 5 cols 4-5-6);
			*females;
			xi: reg ${depvar`j'} ${treatmentdummiesCT2} date`j' year`j' ${controls} [pw=weight_sample],cluster(schoolid), if sex==2 ;
				test Uonly=UHnoCT;
					local p1=r(p);
				test HnoCT=UHnoCT;
					local p2=r(p);
				test HnoCT=Uonly;
					local p3=r(p);
				test UHnoCT=Uonly+HnoCT;
					local p4=r(p);
				quietly outreg2 ${treatmentdummiesCT2}  using LRimpactCT2_2.xls, nonote se /*sigsymb(***,**,*)*/ `append_replace' nolabel 
					 addstat("control mean", `meanf', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3) bdec(3);


			*males;
			xi: reg ${depvar`j'} ${treatmentdummiesCT2} date`j' year`j' ${controls} [pw=weight_sample] ,cluster(schoolid), if sex==1;
			test Uonly=UHnoCT;
				local p1=r(p);
			test HnoCT=UHnoCT;
				local p2=r(p);
			test HnoCT=Uonly;
				local p3=r(p);
			test UHnoCT=Uonly+HnoCT;
				local p4=r(p);
				quietly outreg2 ${treatmentdummiesCT2}  using LRimpactCT2_1.xls, nonote se /*sigsymb(***,**,*)*/ `append_replace' nolabel 
					 addstat("control mean", `meanm', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3) bdec(3);
*/		 
	local j=`j'+1;
	};

	
*/;	
*/

***************************;
*** TABLE 7 ***************;
****************************;

/*
#delimit;
gen var=.;
gen group="";

for any N mean mean_U mean_H mean_UH mean_control: gen X=.;

gen inschool=(Q_b1_10==2|Q_b1_10==3) if Q_b1_10!=.;
*no schooling no spouse: nosonospo;
	gen nosonospo=(1-inschool)*(1-an_ever_mar) if inschool!=. & an_ever_mar!=.;

foreach var of varlist an_ever_mar inschool an_everhadsex {;
	gen `var'_no=1-`var';
	};

local group=7;
replace var=hsv2_positive;

preserve;
drop if sex==1;

for any an_ever_mar an_ever_mar_no inschool inschool_no nosonospo an_everhadsex an_everhadsex_no  
 \ num 1/`group':
 replace group="X" if _n==Y \
 sum var if X==1 \
	replace mean=r(mean) if _n==Y \
	replace N=r(N) if _n==Y \	
 sum var if Utreat==1&HIVtreat==0 & X==1\	
	 replace mean_U=r(mean) if _n==Y \
 sum var if Utreat==0&HIVtreat==1 & X==1\
	 replace mean_H=r(mean) if _n==Y \
 sum var if Utreat==1&HIVtreat==1 & X==1\
	 replace mean_UH=r(mean) if _n==Y \
 sum var if Utreat==0 & X==1\
	 replace mean_control=r(mean) if _n==Y ;

for var mean* : replace X=round(X, 0.001);
outsheet group N mean mean_U mean_H mean_UH mean_control if _n<=`group' using hsv2_bytype2.xls,replace;

restore;


*/	

*******************************************************************;
*******************************************************************;
*******************************************************************;
********              APPENDIX TABLES                **************;
*******************************************************************;
*******************************************************************;
*******************************************************************;


*************************************************************;
**** APPENDIX TABLE A1: ACCURACY OF ROLL CALL METHOD;
*****************************************************************;
/*
#delimit;
use hometracking.dta, clear;
	//rename q5 pupilid;
	keep if sex=="2";
	keep if strpos(q9_whofound, "3")==0;
	*drop if q8!="1";

	gen gavebirth=q17_birth;
		destring gavebirth, force replace;
		replace gavebirth=. if gavebirth==99;
		replace gavebirth=0 if gavebirth==2;

	gen everpregnant=gavebirth;
		replace everpregnant=1 if q25_miscarriage=="1";
		replace everpregnant=1 if q27_preginterview=="1";

	gen everpregchild=everpregnant;
		replace everpregnant=. if sex=="1";

	split date, gen(bub) p(/) l(3) destring force;
		drop bub3;
		gen bub3=2006;
		gen datesurvey=mdy(bub1, bub2, bub3);
		format datesurvey %d;

	keep pupilid datesurvey everpregnant everpregchild gavebirth;
	sort pupilid;
	save tp1, replace;

use sampling_frame.dta, clear;
	keep if sex==2 &std03v1<8;
	gen evpreg06v1=evpreg05v3;
		replace evpreg06v1=1 if preg06v1==1|evchild06v1==1;
	keep pupilid evpreg05v3 evpreg06v1 evchild05v3  evchild06v1 HIVtreat03v1 Utreat03v1 sch03v1 std03v1;
	sort pupilid;
	merge pupilid using tp1;
	drop if _merge!=3;
	tab _merge;
	drop _merge;

	gen lastpreg=evpreg05v3 if datesurvey<d(12apr2006);
	gen lastfert=evchild05v3 if datesurvey<d(12apr2006);
	gen gap=datesurvey-d(01nov2005) if datesurvey<d(12apr2006);
		replace lastpreg=evpreg06v1 if datesurvey>d(11apr2005);
		replace lastfert=evchild06v1 if datesurvey>d(11apr2005);
		replace gap=datesurvey-d(15mar2006) if datesurvey>d(11apr2005);

	gen accurate=(lastpreg==everpregchild) if lastpreg!=. & everpregchild!=. ;
	gen caccurate=(lastfert==gavebirth) if lastfert!=.&gavebirth!=. ;

		gen fert1_acc= accurate if lastpreg==1;
		gen fert0_acc= accurate if lastpreg==0;	
		gen child1_acc= caccurate if lastfert==1;
		gen child0_acc= caccurate if lastfert==0;

****ACCURACY TABLE;
gen Honly=(HIVtreat03v1==1) & (Utreat03v1==0);
gen Uonly=(HIVtreat03v1==0) & (Utreat03v1==1);
gen UH=(HIVtreat03v1==1) & (Utreat03v1==1);

		sum  fert1_acc;
		gen mean=r(mean);
		xi: reg fert1_acc ${treatmentdummies}, cluster(sch03v1);
		test Uonly=UH;
			local p1=r(p);
		test Honly=UH;
			local p2=r(p);
		test Honly=Uonly;
			local p3=r(p);
		test UH=Uonly+Honly;
			local p4=r(p);
		outreg2 ${treatmentdummies} using "tableACC.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean,  "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;

		foreach var in fert0_acc child1_acc child0_acc { ;
			sum  `var';
			gen mean=r(mean);
			xi: reg `var' ${treatmentdummies}, cluster(sch03v1) ;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);
			outreg2 ${treatmentdummies} using "tableACC.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean,  "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		} ;


erase tp1.dta;
*/

*****************************************************;
***APPENDIX TABLE A3: ATTRITION IN LR DATA;
*********************************************************;


*erase temp.dta;

*****************************************************;
***APPENDIX TABLE A4: QUALITY OF LR DATA;
*********************************************************;
*checking for attrition across different arms
/*

#delimit;
use studysample_allmerged2.dta, clear;

foreach date in 05v3 07v2 {;
	replace evmar`date'=0 if evmar`date'==. & evpreg`date'==1;
	gen evpregmar`date'=evpreg`date'*evmar`date';
	gen evpregunmar`date'=evpreg`date'*(1-evmar`date');
	gen marifchild`date'=evmar`date' if evpreg`date'==1;
};


local i 2;
while `i'>0 { ;
		
	global var="evpreg05v3"; 
		sum  ${var} if sex==`i' & group03v1=="C";
		gen mean=r(mean);
		xi: reg ${var} ${treatmentdummies} ${controlsR} , cluster(sch03v1), if sex==`i';
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;


		reg  ${var} if sex==`i' & group03v1=="C" & dead==0 & weight_sample!=. & LOG_surveyed==1;
		gen mean=_b[_cons];
		xi: reg ${var} ${treatmentdummies} ${controlsR} , cluster(sch03v1), if sex==`i' & dead==0 & weight_sample!=. & LOG_surveyed==1;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		reg  ${var} [pw=weight_sample] if sex==`i' & group03v1=="C" & dead==0 & weight_sample!=. & LOG_surveyed==1;
		gen mean=_b[_cons];
		xi: reg ${var} ${treatmentdummies} ${controlsR} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0 & weight_sample!=. & LOG_surveyed==1;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		reg  ${var} [pw=weight_sample] if sex==`i' & group03v1=="C" & dead==0 & weight_sample!=. & LOG_surveyed==1 & sampledIT==0;
		gen mean=_b[_cons];
		xi: reg ${var} ${treatmentdummies} ${controlsR} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0 & weight_sample!=. & LOG_surveyed==1 & sampledIT==0;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

	global var="evpreg07v2"; //use this as dummy for attrition
		sum  ${var} if sex==`i' & group03v1=="C";
		gen mean=r(mean);
		xi: reg ${var} ${treatmentdummies} ${controlsR} , cluster(sch03v1), if sex==`i';
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		reg  ${var}  if  sex==`i' & group03v1=="C" & dead==0 & weight_sample!=. & LOG_surveyed==1;
		gen mean=_b[_cons];
		xi: reg ${var} ${treatmentdummies} ${controlsR} , cluster(sch03v1), if sex==`i' & dead==0 & weight_sample!=. & LOG_surveyed==1;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		reg  ${var} [pw=weight_sample] if sex==`i' & group03v1=="C" & dead==0 & weight_sample!=. & LOG_surveyed==1;
		gen mean=_b[_cons];
		xi: reg ${var} ${treatmentdummies} ${controlsR} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0 & weight_sample!=. & LOG_surveyed==1; 
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;
	
		reg  ${var} [pw=weight_sample] if  sex==`i' & group03v1=="C" & dead==0 & weight_sample!=. & LOG_surveyed==1 & sampledIT==0;
		gen mean=_b[_cons];
		xi: reg ${var} ${treatmentdummies} ${controlsR} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0 & weight_sample!=. & LOG_surveyed==1 & sampledIT==0; 
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA4_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

local i=`i'-1;
} ;
*/


***********************************************;
***APPENDIX TABLE A5: KAP data analysis;
************************************************;


******************
**********girls;
********************;
/*
#delimit;
use KAPgirls, clear;
//rename q3 schoolid;
rename schoolID schoolid;
sort schoolid;
merge schoolid using school_info.dta;
drop if _merge==2;
drop _merge;
gen HandUtreat=HIVtreat*Utreat;
rename age q4;
rename grade q5;
rename q6_school q6;
rename q6_schoolother q6other;
rename q7_gradelastyr q7;
gen age=q4;
destring age, force replace;
replace age=12 if q4=="KUMI NA MBILI";
replace age=14 if q4=="KUMI NA MINNE";
replace age=11 if q4== "KUMI NA MOJA";
replace age=18 if q4== "KUMI NA NANE";
replace age=14 if q4==  "KUMI NA NNE";
replace age=17 if q4== "KUMI NA SABA";
replace age=16 if q4== "KUMI NA SITA";
replace age=15 if q4== "KUMI NA TANO";
replace age=13 if q4== "KUMI NA TATU";

replace age=12 if age==0.12;
replace age=11 if q4=="11'";
replace age=14 if age==114;
replace age=12 if age==122;
replace age=14 if age==147;
replace age=15 if age==156|age==157;
replace age=1987 if age==1887;
replace age=2005-age if age>1900&age<2000;
replace age=19 if age==193;

#delimit cr

*fix entry errors
gen p1=1 if (age==6|age==7|age==8)&q5==1
gen p2=1 if q6==.&(q6other=="5"|q6other=="6"|q6other=="7"|q6other=="8")&q7==.
gen p3=1 if q5>9&q5<17
gen p4=1 if q5==81
gen p5=1 if q5==1&q6==.&(q6other=="5"|q6other=="6"|q6other=="7"|q6other=="8")&(q7==5|q7==6|q7==7|q7==8)
gen p6=1 if q6==.&q6other=="1"

replace q6=1 if p6==1
replace q6other="" if p6==1

replace q6=q5 if p1==1&q6==.
replace q5=age if p1==1
replace age=. if p1==1

gen q6o=q6other
destring q6o, force replace
replace q7=q6o if p2==1
replace q6=q5 if p2==1
replace q5=. if p2==1
replace q6other="" if p2==1

replace age=q5 if p3==1
replace q5=. if p3==1

replace q5=8 if p4==1
replace q6=1 if p4==1

replace q5=q6o if p5==1
replace q6other="" if p5==1
replace q6=1 if p5==1

replace q5=6 if q6other=="STD 6"
replace q6other="" if q6other=="STD 6"

gen class=6 if q5==6
replace class=7 if q5==7
replace class=8 if q5==8
replace q6=2 if q6other!=""&q6==.

replace age=18 if q4==""&q6other=="18"
replace q7=q6o if q6==1&q7==.&(q6other=="5"|q6other=="6"|q6other=="7"|q6other=="8")
replace age=. if age>21|age<9

gen agemissing =  (age==.)
sum age
replace age= r(mean) if agemissing==1

drop q6o

*CONTROLS
gen transferred=0
	replace transferred=1 if q6==2
gen repeater=0
	replace repeater=1 if  q7==q5
foreach X of varlist q8 q9 q10 q11 q13 q14 {
	replace `X'=0 if `X'==2
	}
rename q8 ironroof
rename q9 radio
rename q10 bednet
rename q11 HCmember
gen std6=0
	replace std6=1 if class==6
gen std7=0
	replace std7=1 if class==7
gen std8=0
	replace std8=1 if class==8

*INFO RECEIVED
gen HIVmentionedlast4weeks=0 if q16!=.
	replace HIVmentionedlast4weeks=1 if q16==1|q16==2
gen HIVmentionedinclass=1 if q16!=.
	replace HIVmentionedinclass=0 if q16==5

*KNOWLEDGE
gen HIVkills=0  if q19!=.
	replace HIVkills=1 if q19==1
gen healthylook=0 if q18!=.
	replace healthylook=1 if q18==1
gen condompreventpreg=0  if q22!=.
	replace condompreventpreg=1 if q22==1
gen condompreventHIV=0 if q23!=.
	replace condompreventHIV=1 if q23==1
gen mentionabstinence=0 if q17_hivmethod1!=""|q17_hivmethod2!=""|q17_hivmethod3!=""|q17_hivmethod4!=""|q17_hivmethod5!=""
	replace mentionabstinence=1 if q17_1_code_1==1| q17_1_code_2==1| q17_2_code_1==1| q17_2_code_2==1| q17_3_code_1==1|q17_3_code_2==1| q17_4_code_1==1|q17_4_code_2==1|q17_1_code_1==1
gen mentioncondoms=0 if q17_hivmethod1!=""|q17_hivmethod2!=""|q17_hivmethod3!=""|q17_hivmethod4!=""|q17_hivmethod5!=""
	replace mentioncondoms=1 if q17_1_code_1==3| q17_1_code_2==3| q17_2_code_1==3| q17_2_code_2==3| q17_3_code_1==3|q17_3_code_2==3| q17_4_code_1==3|q17_4_code_2==3|q17_1_code_1==3
gen mentionfaithfulness=0 if q17_hivmethod1!=""|q17_hivmethod2!=""|q17_hivmethod3!=""|q17_hivmethod4!=""|q17_hivmethod5!=""
	replace mentionfaithfulness=1 if q17_1_code_1==2| q17_1_code_2==2| q17_2_code_1==2| q17_2_code_2==2| q17_3_code_1==2|q17_3_code_2==2| q17_4_code_1==2|q17_4_code_2==2|q17_1_code_1==2

#delimit;
keep if class==7|class==8;
keep if q6==1;
keep if q7!=5&q7!=8;


* TREATMENT DUMMIES;
gen Uonly=(HIVtreat==0)&(Utreat==1);
gen Honly=(Utreat==0)&(HIVtreat==1);
gen UH=Utreat*HIVtreat;
gen HnoCT=Honly*(1-sampleCT103v1);
gen UHnoCT=UH*(1-sampleCT103v1);
gen HwithCT=Honly*(sampleCT103v1);
gen UHwithCT=UH*(sampleCT103v1);


sum HIVmentionedlast4weeks if HIVtreat==0&Utreat==0;
gen mean=r(mean);
xi: reg HIVmentionedlast4weeks  ${treatmentdummies} ${controlsKAP}, clust(schoolid);
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA5_2.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;
	
foreach X of varlist  HIVmentionedinclass- mentionfaithfulness
  {;
	sum `X'  if HIVtreat==0&Utreat==0;
	gen mean_`X'=r(mean);
	xi: reg `X'  ${treatmentdummies} ${controlsKAP}, clust(schoolid);
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA5_2.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean_`X', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
	drop mean_`X';
	}; 


** ALL CT TREATMENTS (Table 5 cols 1-3);
#delimit;
sum HIVmentionedlast4weeks if HIVtreat==0&Utreat==0;
gen mean=r(mean);
xi: reg HIVmentionedlast4weeks  ${treatmentdummiesCT2}  ${controlsKAP}, clust(schoolid);
			test Uonly=UHnoCT;
				local pval=r(p);
			test HnoCT=UHnoCT;
				local p2=r(p);
			test HnoCT=Uonly;
				local p3=r(p);
			test UHnoCT=Uonly+HnoCT;
				local p4=r(p);

		outreg2 ${treatmentdummiesCT2} using "tableA5CT2_2.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;
		
foreach X of varlist  HIVmentionedinclass- mentionfaithfulness  {;
	sum `X'  if HIVtreat==0&Utreat==0;
	gen mean_`X'=r(mean);
	xi: reg `X'  ${treatmentdummiesCT2} ${controlsKAP}, clust(schoolid);
			test Uonly=UHnoCT;
				local pval=r(p);
			test HnoCT=UHnoCT;
				local p2=r(p);
			test HnoCT=Uonly;
				local p3=r(p);
			test UHnoCT=Uonly+HnoCT;
				local p4=r(p);

		outreg2 ${treatmentdummiesCT2} using "tableA5CT2_2.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean_`X', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
	drop mean_`X';
	}; 

******************;
**********boys;
********************;
#delimit;
use KAPboys, clear;
rename schoolID schoolid;
//rename q3 schoolid;
sort schoolid;
merge schoolid using school_info.dta;
drop if _merge==2;
drop _merge;
gen HandUtreat=HIVtreat*Utreat;
#delimit cr

****fix stuff
rename age q4
rename grade q5
rename q6_school q6
rename q6_schoolother q6other
rename q7_gradelastyr q7
gen age=q4
	destring age, force replace
	replace age=12 if q4=="KUMI NA MBILI"|q4== "KUMI NA MIWILI"
	replace age=14 if q4=="KUMI NA MINNE"| q4=="KUMI NA MIINE"|q4=="KUMI NA NNE"
	replace age=11 if q4== "KUMI NA MOJA"
	replace age=18 if q4== "KUMI NA NANE"
	replace age=14 if q4==  "KUMI NA NNE"
	replace age=17 if q4== "KUMI NA SABA"|q4== "KUMI7"
	replace age=16 if q4== "KUMI NA SITA"
	replace age=15 if q4== "KUMI NA TANO"|q4== "KUMI NA MITANO"
	replace age=13 if q4== "KUMI NA TATU"|q4== "KUMI NA MITATU"
	
	replace age=12 if age==0.12
	replace age=11 if q4=="11'"
	replace age=14 if age==114
	replace age=17 if age==117
	replace age=15 if age==115|age==156
	replace age=16 if age==116|q4=="16 YEARS"
	replace age=12.5 if q4=="121/2"
	replace age=12 if age==122
	replace age=13 if age==123|age==133|q4=="13 YEARS"
	replace age=16 if age==126|age==167
	replace age=14 if age==147|q4=="14YRS"
	replace age=15 if age==156|age==157|q4=="15 YEARS"
	replace age=1987 if age==1887
	replace age=2005-age if age>1900&age<2000
	replace age=19 if age==193
	replace age=18 if age==185|q4=="18YEARS"

gen p1=1 if (age==6|age==7|age==8)&q5==1
gen p2=1 if q6==.&(q6other=="5"|q6other=="6"|q6other=="7"|q6other=="8")&q7==.
gen p3=1 if q5>9&q5<17
gen p4=1 if q5==81
gen p5=1 if q5==1&q6==.&(q6other=="5"|q6other=="6"|q6other=="7"|q6other=="8")&(q7==5|q7==6|q7==7|q7==8)
gen p6=1 if q6==.&q6other=="1"

replace q6=1 if p6==1
replace q6other="" if p6==1
replace q6=q5 if p1==1&q6==.
replace q5=age if p1==1
replace age=. if p1==1

gen q6o=q6other
destring q6o, force replace
replace q7=q6o if p2==1
replace q6=q5 if p2==1
replace q5=. if p2==1
replace q6other="" if p2==1

replace age=q5 if p3==1
replace q5=. if p3==1

replace q5=8 if p4==1
replace q6=1 if p4==1

replace q5=q6o if p5==1
replace q6other="" if p5==1
replace q6=1 if p5==1

replace q5=6 if q6other=="STD 6"
replace q6other="" if q6other=="STD 6"

gen class=6 if q5==6
replace class=7 if q5==7
replace class=8 if q5==8
replace q6=2 if q6other!=""&q6==.

replace age=18 if q4==""&q6other=="18"
replace q7=q6o if q6==1&q7==.&(q6other=="5"|q6other=="6"|q6other=="7"|q6other=="8")

replace age=. if age>21|age<9

replace class=6 if class==.&q4=="6"
replace class=7 if class==.&q4=="7"
replace class=8 if class==.&q4=="8"
replace class=7 if class==.&q4=="167"

drop q6o
gen agemissing =  (age==.)
sum age
replace age= r(mean) if agemissing==1


****CONTROLS
gen transferred=0
	replace transferred=1 if q6==2
gen repeater=0
	replace repeater=1 if  q7==q5
foreach X of varlist q8 q9 q10 q11 q13 q14 {
	replace `X'=0 if `X'==2
	}
rename q8 ironroof
rename q9 radio
rename q10 bednet
rename q11 HCmember

gen std6=0
	replace std6=1 if class==6
gen std7=0
	replace std7=1 if class==7
gen std8=0
	replace std8=1 if class==8

*INFO RECEIVED
gen HIVmentionedlast4weeks=0 if q16!=.
	replace HIVmentionedlast4weeks=1 if q16==1|q16==2
gen HIVmentionedinclass=1 if q16!=.
	replace HIVmentionedinclass=0 if q16==5


*KNOWLEDGE
gen healthylook=0 if q18!=.
	replace healthylook=1 if q18==1
gen HIVkills=0  if q19!=.
	replace HIVkills=1 if q19==1
gen condompreventpreg=0  if q22!=.
	replace condompreventpreg=1 if q22==1
gen condompreventHIV=0 if q23!=.
	replace condompreventHIV=1 if q23==1
gen mentionabstinence=0 if q17_hivmethod1!=""|q17_hivmethod2!=""|q17_hivmethod3!=""|q17_hivmethod4!=""|q17_hivmethod5!=""
	replace mentionabstinence=1 if q17_1_code_1==1| q17_1_code_2==1| q17_2_code_1==1| q17_2_code_2==1| q17_3_code_1==1|q17_3_code_2==1| q17_4_code_1==1|q17_4_code_2==1|q17_5_code_1==1
gen mentioncondoms=0 if q17_hivmethod1!=""|q17_hivmethod2!=""|q17_hivmethod3!=""|q17_hivmethod4!=""|q17_hivmethod5!=""
	replace mentioncondoms=1 if q17_1_code_1==3| q17_1_code_2==3| q17_2_code_1==3| q17_2_code_2==3| q17_3_code_1==3|q17_3_code_2==3| q17_4_code_1==3|q17_4_code_2==3|q17_5_code_1==3
gen mentionfaithfulness=0 if q17_hivmethod1!=""|q17_hivmethod2!=""|q17_hivmethod3!=""|q17_hivmethod4!=""|q17_hivmethod5!=""
	replace mentionfaithfulness=1 if q17_1_code_1==2| q17_1_code_2==2| q17_2_code_1==2| q17_2_code_2==2| q17_3_code_1==2|q17_3_code_2==2| q17_4_code_1==2|q17_4_code_2==2|q17_5_code_1==2

#delimit;
keep if class==7|class==8;
keep if q6==1;
keep if q7!=5&q7!=8;

* TREATMENT DUMMIES;
gen Uonly=(HIVtreat==0)&(Utreat==1);
gen Honly=(Utreat==0)&(HIVtreat==1);
gen UH=Utreat*HIVtreat;
gen HnoCT=Honly*(1-sampleCT103v1);
gen UHnoCT=UH*(1-sampleCT103v1);
gen HwithCT=Honly*(sampleCT103v1);
gen UHwithCT=UH*(sampleCT103v1);


sum HIVmentionedlast4weeks if HIVtreat==0&Utreat==0;
gen mean=r(mean);
xi: reg HIVmentionedlast4weeks ${treatmentdummies} ${controlsKAP}, clust(schoolid);
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA5_1.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
	drop mean;
foreach X of varlist  HIVmentionedinclass- mentionfaithfulness  {;
	sum `X'  if HIVtreat==0&Utreat==0;
	gen mean_`X'=r(mean);
	xi: reg `X'  ${treatmentdummies} ${controlsKAP}, clust(schoolid);
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "tableA5_1.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean_`X', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4', "UH=U+H", `p4') adec(3)  append;
	drop mean_`X';
	}; 

	
*** ALL CT TREATMENTS (TABLE 5 COLS 1-3);	
sum HIVmentionedlast4weeks if HIVtreat==0&Utreat==0;
gen mean=r(mean);
xi: reg HIVmentionedlast4weeks ${treatmentdummiesCT2} ${controlsKAP}, clust(schoolid);
			test Uonly=UHnoCT;
				local pval=r(p);
			test HnoCT=UHnoCT;
				local p2=r(p);
			test HnoCT=Uonly;
				local p3=r(p);
			test UHnoCT=Uonly+HnoCT;
				local p4=r(p);

		outreg2 ${treatmentdummiesCT2} using "tableA5CT2_1.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;
		
foreach X of varlist  HIVmentionedinclass- mentionfaithfulness  {;
	sum `X'  if HIVtreat==0&Utreat==0;
	gen mean_`X'=r(mean);
	xi: reg `X'  ${treatmentdummiesCT2} ${controlsKAP}, clust(schoolid);
			test Uonly=UHnoCT;
				local pval=r(p);
			test HnoCT=UHnoCT;
				local p2=r(p);
			test HnoCT=Uonly;
				local p3=r(p);
			test UHnoCT=Uonly+HnoCT;
				local p4=r(p);

		outreg2 ${treatmentdummiesCT2} using "tableA5CT2_1.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean_`X', "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4', "UH=U+H", `p4') adec(3)  append;
	drop mean_`X';
	}; 
	

**Deleting redundant txt files;
local txt LRimpact_1 LRimpact_2 LRimpactCT2_1 LRimpactCT2_2 table2_1 table2_2 table2A_1 table2A_2 table2CT2_1 table2CT2_2 table3A_1 table3A_2 tableA4_1 tableA4_2 tableA5_1 tableA5_2 tableA5CT2_1 tableA5CT2_2 tableACC;
foreach file in `txt' {;
	erase `file'.txt;
};
*/

****************************
*** Heckman 2-step model ***
****************************
version 15
*limit sample to only for girls
* treatmentdummiesCT2="Uonly HnoCT UHnoCT HwithCT UHwithCT"
* controlsR="yrbirth_all yrbirth_missing date05v3 date07v2 schsize i.stratum"
* varlist "presence evmar05v3 evpreg05v3 evpregunmar05v3 evunpregmar05v3 dropout07v2 evmar07v2 evpreg07v2 evpregunmar07v2 evunpregmar07v2"
* treatmentdummies="Uonly Honly UH"

// use attrition.dta, replace //This is LR attrition data

use attri_SMR.dta, replace
drop if sex !=2 //keep only females
sort pupilid

*clean as in the original dofile
foreach visit in 04v1 04v2 05v1 05v2 05v3 {
	gen presence`visit'=pres`visit'
	replace presence`visit'=0 if pres`visit'==2
	replace presence`visit'=0.5 if pres`visit'==3
	replace presence`visit'=. if pres`visit'>3
}

replace dropout05v3=0 if presence05v3==1
replace dropout05v3=. if evdead05v3==1

foreach date in 05v3 07v2 {
	replace evmar`date'=0 if evmar`date'==. & evpreg`date'==1
	gen evunpregmar`date'=(1-evpreg`date')*evmar`date'
	gen evpregunmar`date'=evpreg`date'*(1-evmar`date')
	gen marifchild`date'=evmar`date' if evpreg`date'==1
}

*if available for some still considered found
*some is 0 not all
* sum of missing max=4 : miss all
* sum of missing min=0 : available all

gen found05 = 0 
replace found05 = 1 ///
if dropout05v3_missing + presence_missing + evmar05v3_missing + evpreg05v3_missing <=3

gen found05_strict = 0 
replace found05_strict = 1 ///
if dropout05v3_missing + presence_missing + evmar05v3_missing + evpreg05v3_missing == 0

summarize found05 found05_strict

xi: reg found05 ${treatmentdummies} ${controlsR}, ///
cluster(sch03v1)
test Uonly=UH 
	estadd local p1=r(p)
	test Honly=UH //significant
	estadd local p2=r(p)
	test Honly=Uonly 
	estadd local p3=r(p)

xi: reg found05_strict ${treatmentdummies} ${controlsR}, ///
cluster(sch03v1)
test Uonly=UH //significant
	estadd local p1=r(p)
	test Honly=UH
	estadd local p2=r(p)
	test Honly=Uonly
	estadd local p3=r(p)

xi: heckman dropout05v3 ${treatmentdummies} ${controlsR}, ///
twostep select(found05_strict ${treatmentdummies} ${controlsR}) 
//similar magnitude

// any other instrument we can find? 
// an exclusion restriction is required to generate credible estimates:
// there must be at least one variable which appears with 
// a non-zero coefficient in the selection equation but does not appear 
// in the equation of interest, essentially an instrument. 
// If no such variable is available, it may be difficult to correct for 
// sampling selectivity.

*IV is limited
xi: heckman dropout05v3 ${treatmentdummies} ${controlsR}, ///
twostep select(found05_strict ${treatmentdummies} ${controlsR} i.Q_b1_20) 

* Lee's bound estimator //results should be similar for Honly and UH
xi: reg dropout05v3 Uonly // -.0214642 sig
xi: reg dropout05v3 Uonly ${controlsR} //-.0270405 sig 

xi: heckman dropout05v3 ${treatmentdummies} ${controlsR}, ///
twostep select(found05_strict ${treatmentdummies} ${controlsR}) 

xi: leebounds dropout05v3 Uonly // -.0246442 to -.0208772 sig

xi: leebounds dropout05v3 Uonly, ///
tight(${controlsR})

gen H_dummy = 0
replace H_dummy = 1 if Honly == 1 | UH == 1

xi: reg dropout05v3 H_dummy // -.0002798  insig
xi: reg dropout05v3 H_dummy ${controlsR} //.0091512  insig 

xi: leebounds dropout05v3 H_dummy //  -.0007494  to .0019819 insig



