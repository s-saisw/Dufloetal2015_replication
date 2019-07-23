* ================================================================
* This program cleans data according to the original do-file
* data sets used - school_info.dta
*                - studysample_allmerged.dta
* output         - school_small.dta
*                - studysample_allmerged2.dta
*                - temp.dta
*                - attri_SMR.dta
* ================================================================

use school_info, clear
keep schoolid schsize HIVtreat Utreat
gen Honly=(HIVtreat==1) & (Utreat==0)
gen Uonly=(HIVtreat==0) & (Utreat==1)
gen UH=(HIVtreat==1) & (Utreat==1)
sort schoolid
save school_small, replace

use studysample_allmerged, clear
sort schoolid
merge m:1 schoolid using school_small //change syntax to newer version of merge
drop if _merge==2
drop _merge

******************;
* FIX AGE, YR OF BIRTH;	
* note 1: we need to use yrbirth (collected at baseline), rather than the yr of birth
* information collected in the survey, because people tend to lie about their year of
* birth if they have already started a family;

* note 2: since kids were in grade 6 in 2003, it's not really possible for them to be
* born after 1992 (they couldn't have been in grade 6 before age 10)
* or born before 1987 (they couldn't be older than 16 and be in grade 6);

replace yrbirth=Q_a3_8_date_of_birth  if yrbirth>1992&Q_a3_8_date_of_birth<yrbirth &  LOG_surveyed==1 & Q_a3_8_date_of_birth  !=.
replace yrbirth=Q_a3_8_date_of_birth  if yrbirth<1987&Q_a3_8_date_of_birth>yrbirth & LOG_surveyed==1 & Q_a3_8_date_of_birth  !=.
replace yrbirth=Q_a3_8_date_of_birth if yrbirth==. & Q_a3_8_date_of_birth!=.

replace yrbirth=1985 if yrbirth<1985
replace yrbirth=1992 if yrbirth>1992 & yrbirth!=.

**** new age variables;
gen age2009=2009-yrbirth
replace age2009=2009-Q_a3_8_date_of_birth if (age2009<16 | age2009>22) ///
		& Q_a3_8_date_of_birth>1986 & Q_a3_8_date_of_birth<1993 ///
		& Q_a3_8_date_of_birth!=.

gen age2003=age2009-6

gen age_survey=Q_year-yrbirth
label var age_survey "age at time of survey"

gen age_survey_all=age_survey
gen age_survey_missing=age_survey==. if LOG_surveyed==1 & dead==0
gen a=age_survey if LOG_surveyed==1 & dead==0
bysort sex: egen r=mean(a)
replace age_survey_all=r if LOG_surveyed==1 & dead==0 & age_survey==. 
drop a r
label var age_survey_all "Age at time of survey, missing replaced with mean by gender"

gen yrbirth_missing=(yrbirth==.)
gen yrbirth_all=yrbirth
bys sex: egen mean=mean(yrbirth)
replace mean=round(mean,1)
replace yrbirth_all=mean if yrbirth_missing==1
drop mean
label var yrbirth_all "Year of Birth, missing replaced with mean by gender"

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

save studysample_allmerged2, replace

***********************************************;
***TABLES 3, 4 and 6: Long-run follow-up results;
***********************************************;

#delimit;
use studysample_allmerged2.dta, clear;
gen HnoCT=Honly*(1-sampleCT103v1);
gen UHnoCT=UH*(1-sampleCT103v1);
gen HwithCT=Honly*(sampleCT103v1);
gen UHwithCT=UH*(sampleCT103v1);


gen BD_year=year(LOG_bd_BDrealdate);

			/* CREATE VARS OF INTEREST FROM SURVEY RESPONSES */;
			
					***********;
					** grades of school completed;
					gen an_grades_completed=Q_b1_12_edu_level;
					destring an_grades_completed, force replace;
					replace an_grades_completed=9 if Q_b1_12_edu_level=="F1";
					replace an_grades_completed=10 if Q_b1_12_edu_level=="F2";
					replace an_grades_completed=11 if Q_b1_12_edu_level=="F3";
					replace an_grades_completed=12 if Q_b1_12_edu_level=="F4";

					replace an_grades_completed=11 if  Q_b1_10==3 & an_grades_completed==.;
					replace an_grades_completed=13 if  strpos(Q_b1_12_other_des,"COLL") & an_grades_completed==.;
								
			
					#delimit cr
					*****************************************
					**** SECTION B3 preparation
					*****************************************

					** we prepare the variables

					*******
					
					*******
					** Q79
					gen an_Q_b3_79=Q_b3_79_prevent_hiv 
						recode an_Q_b3_79 (2=0) (3=0)

					*******
					** Q80

					** fill in the var and the number of possible answers (t)
					foreach var of varlist Q_b3_80_how_prevent_hiv {
					local t=19

					forvalues i=1/`t' {
						gen an_`var'_`i'=0 if `var'!=""
						label var an_`var'_`i' "`var' reponse `i'"
					}

					gen tp1=`var'
					replace tp1=subinstr(tp1,".",",",.)
					replace tp1=subinstr(tp1,"]",",",.)
					replace tp1=subinstr(tp1,","," ",.)
					replace tp1=subinstr(tp1,"  "," ",.)

					gen tp2=wordcount(tp1)
					sum tp2
					local j=r(max)
					forvalues i=1/`j' {
						gen temp`i'=""
						qui replace temp`i'=word(tp1,`i') if temp`i'==""
						destring temp`i' ,replace
					forvalues k=1/`t'{
						replace an_`var'_`k'=1 if temp`i'==`k'
					}

					drop temp`i'
					}
					drop tp1 tp2
					}


					*******
					** Q81 Q83 Q84 Q87 Q88 Q90 Q92 Q93 Q94 Q95 Q96 Q102

					** correction: 
					replace Q_b3_94=. if pupilid==92260090

					#delimit ;
					foreach var of varlist Q_b3_81 Q_b3_83 Q_b3_84 Q_b3_87
									 Q_b3_88 Q_b3_90 Q_b3_92 Q_b3_93 
									Q_b3_94 Q_b3_95 Q_b3_96 Q_b3_102_get_condoms {;
					gen an_`var'=`var' if LOG_surveyed==1 & dead==0;
						recode an_`var' (2=0) (3=0);
					};

					#delimit cr

					*******
					** Q85 Q86 Q98 Q99 Q100 Q101 

					foreach var of varlist Q_b3_85 Q_b3_86 Q_b3_98 Q_b3_99 Q_b3_100 Q_b3_101 {
					gen an_`var'=`var' if LOG_surveyed==1 & dead==0
					}


					**************
					** Knowledge of of HIV/AIDS

					
					****************
					*** No missing

					gen an_tot_miss_b3=0
					#delimit ;
					foreach var of varlist Q_b3_81 Q_b3_83 Q_b3_84 Q_b3_87 Q_b3_88 
					Q_b3_90 Q_b3_92 Q_b3_93 Q_b3_94 Q_b3_95 
					Q_b3_96 Q_b3_102_get_condoms Q_b3_85 Q_b3_86 Q_b3_98 Q_b3_76_discuss_aids
					Q_b3_99 Q_b3_100 Q_b3_101 {;
					qui replace an_tot_miss_b3=an_tot_miss_b3+1 if `var'==. & LOG_surveyed==1 & dead==0;
					};  
						
					foreach var of varlist  Q_b3_78 {;
					replace an_tot_miss_b3=an_tot_miss_b3+1 if `var'=="" & LOG_surveyed==1 & dead==0;
					};  

					** corrections;
					replace Q_b3_110_agree_with_statement=. if Q_b3_110_agree_with_statement<1;
			
					* *******************************************;
					* MARITAL AND FERTILITY HISTORY;
					* *******************************************;
					
					gen an_ever_mar=(Q_b4_114_marital_status!=4) if Q_b4_114_marital_status!=. & LOG_surveyed==1;

					gen an_ever_fert=. ;
						*girls;
						replace an_ever_fert=1 if Q_b6_139_given_birth==1; 
						replace an_ever_fert=0 if Q_b6_139_given_birth==2;
						replace an_ever_fert=1 if Q_b6_144_child_died==1;
						replace an_ever_fert=1 if Q_b6_161_pregnant==1;
						replace an_ever_fert=1 if Q_b6_164==1;
						* boys;
						replace an_ever_fert=1 if Q_b6_174_have_kids==1;
						replace an_ever_fert=0 if Q_b6_174_have_kids==2;
						replace an_ever_fert=1 if  Q_b6_179_child_died==1;


					** number of children ever conceived;

						** women;
							gen an_num_child=0 if sex==2;
							foreach var of varlist Q_b6_141_boys_with_you Q_b6_141_girls_with_you
								Q_b6_143_boys_not_with_you Q_b6_143_girls_not_with_you 
								Q_b6_145_boys_died Q_b6_145_girls_died Q_b6_165 {;
							qui replace an_num_child= an_num_child+ `var' if `var'!=.;
							};
							qui replace an_num_child=an_num_child+1 if Q_b6_161_pregnant==1;
						
						** men;
							gen tp1=0 if sex==1;
							foreach var of varlist Q_b6_176_boys_with_you Q_b6_176_girls_with_you 
								Q_b6_178_boys_not_with_you Q_b6_178_girls_not_with_you 
								Q_b6_180_boys_died Q_b6_180_girls_died {;
							qui replace tp1= tp1+`var' if `var'!=. & `var'!=99;
						};
						qui replace tp1=tp1+1 if Q_b6_196_spouse_pregnant ==1;

					replace an_num_child=tp1 if tp1!=.;
					replace an_num_child=. if LOG_surveyed!=1| dead==1;

					drop tp1;



					****************;
					** age when 1st child is born;
					** correction;

						replace Q_b6_154b_born_yr=2007 if Q_b6_154b_born_yr==207;
						replace  Q_b6_189a_born_yr=1999 if  Q_b6_189a_born_yr==9999;
						replace Q_b6_154a_born_yr=. if Q_b6_154a_born_yr==2;
						replace Q_b6_154a_born_yr=. if Q_b6_154a_born_yr==9;
						replace Q_b6_154c_born_yr=. if Q_b6_154c_born_yr==8;
						replace  Q_b6_189a_born_yr=. if  Q_b6_189a_born_yr==8;
						
					** girls;
						gen tp1=Q_b6_154a_born_yr;
						replace tp1=Q_b6_154b_born_yr if Q_b6_154b_born_yr<Q_b6_154a_born_yr 
							& Q_b6_154b_born_yr!=. ;
						replace tp1=Q_b6_154c_born_yr if Q_b6_154c_born_yr<Q_b6_154b_born_yr 
							& Q_b6_154c_born_yr!=.;
						replace tp1=Q_b6_154d_born_yr if Q_b6_154d_born_yr<Q_b6_154c_born_yr 
							& Q_b6_154d_born_yr!=.;
						
					** boys;
						gen tp2=Q_b6_189a_born_yr;
						replace tp2=Q_b6_189b_born_yr if Q_b6_189b_born_yr<Q_b6_189a_born_yr 
							& Q_b6_189b_born_yr!=. ;
						replace tp2=Q_b6_189c_born_yr if Q_b6_189c_born_yr<Q_b6_189b_born_yr 
							& Q_b6_189c_born_yr!=.;
						replace tp2=Q_b6_189d_born_yr if Q_b6_189d_born_yr<Q_b6_189c_born_yr 
							& Q_b6_189d_born_yr!=.;

					gen an_age_1st_child_yr=tp1-yrbirth if sex==2;
						replace an_age_1st_child_yr=tp2-yrbirth if sex==1;
						replace an_age_1st_child_yr=. if an_age_1st_child_yr<0;
						replace an_age_1st_child_yr=age2003 if an_age_1st_child_yr<age2003 & age2003<15;
						gen tag=1 	if an_age_1st_child_yr<age2003 & age2003>=15;
							replace age2003=an_age_1st_child_yr if tag==1;
							replace yrbirth=2003-age2003 if tag==1;

					** we add the month of birth date of the 1st child;
					gen an_age_1st_child_month=an_age_1st_child_yr+(Q_b6_154a_born_months/120)
								if an_age_1st_child_yr!=. & sex==2;
					replace an_age_1st_child_month=an_age_1st_child_yr+(Q_b6_189a_born_months/120)
								if an_age_1st_child_yr!=. & sex==1;

					drop tp1 tp2;

					******;
					*** if married: age when married;
						gen tp1=month(Q_realdate);
						gen tp2=year(Q_realdate);
						gen tp3=ym(tp2,tp1);
						gen tp4=tp3-Q_b4_115_months_married;
						gen tp5=dofy(yrbirth);
						replace tp5=mofd(tp5);
						
						gen an_age_marriage_month=(tp4-tp5)/12;
						gen an_age_marriage_yr=round(an_age_marriage_month);
						drop tp1 tp2 tp3 tp4 tp5;

					
					*************;
					** are you pregnant now?;
						gen an_pregnant=.;
						qui replace an_pregnant=0 if Q_b6_161_pregnant==2 & sex==2;
						qui replace an_pregnant=1 if Q_b6_161_pregnant==1 & sex==2;
						replace an_pregnant=0 if sex==1 & LOG_surveyed==1 & dead==0;

					**************;
					** wanted first pregnancy when it happened;
						gen an_want_1stpreg=0 if (Q_b6_163_wanted_pregnancy!=. & Q_b6_163_wanted_pregnancy!=4) & an_ever_fert==1 & sex==2 ;
						qui replace an_want_1stpreg=0 if (Q_b6_198_wanted_pregnancy!=. & an_ever_fert==1  & Q_b6_198_wanted_pregnancy!=4) & sex==1;
						qui replace an_want_1stpreg=1 if Q_b6_163_wanted_pregnancy==1 & an_ever_fert==1 & sex==2;
						qui replace an_want_1stpreg=1 if Q_b6_198_wanted_pregnancy==1 & an_ever_fert==1 & sex==1;


					* *******************************************;
					* REPORTED SEXUAL BEHAVIOR;
					* *******************************************;
					gen an_everhadsex=Q_b4_117_had_sex;
						recode an_everhadsex (2=0);
					gen an_age_firstsex= Q_b4_119_age_1st_sex;

					*** condom use;
					gen an_everusedcondom=Q_b4_126_used_condom;
						recode an_everusedcondom (2=0);
					gen an_condomlastsex= Q_b4_127_last_sex_use_condom;
						recode an_condomlastsex (2=0) (4=.);
						replace an_condomlastsex=0 if an_everusedcondom==0;

					gen an_multiplepartners=Q_b4_118_sex_one_person;
						recode an_multiplepartners (1=0) (2=1);

					*********************************;
					*AGE OF PARTNERS ***************;
					*********************************;
					*** age of spouse Q116;
					gen an_spouse_age=Q_b4_116_spouse_age;
						replace an_spouse_age=19 if an_spouse_age==119;
						replace an_spouse_age=. if an_spouse_age==99 | an_spouse_age<4;
						
					*** age of current partner;
					gen an_age_cur_part=Q_b4_122_age_you;
					replace an_age_cur_part=. if an_age_cur_part==99 | an_age_cur_part<2;
	
					*** age of oldest partner;
					gen an_age_old_part=Q_b4_124_oldest_sex_patner;
						replace an_age_old_part=. if an_age_old_part==99 | an_age_old_part<10;

					*** Reported ways protects oneself;	
					gen an_Q_b5_135=Q_b5_135_change_behavior;
					recode an_Q_b5_135 2=0; 
					** Q136 ;
					foreach var of varlist Q_b5_136 {;
					local t=8;

					forvalues i=1/`t' {;
						gen an_`var'_`i'=0 if `var'!="";
						label var an_`var'_`i' "`var' reponse `i'";
					};

					gen tp1=`var';
					qui replace tp1=subinstr(tp1,".",",",.);
					qui replace tp1=subinstr(tp1,"]",",",.);
					qui replace tp1=subinstr(tp1,","," ",.);
					qui replace tp1=subinstr(tp1,"  "," ",.);

					gen tp2=wordcount(tp1);
					sum tp2;
					local j=r(max);

					forvalues i=1/`j' {;
						gen temp`i'="";
						qui replace temp`i'=word(tp1,`i') if temp`i'=="";
						destring temp`i' ,replace;
					forvalues k=1/`t'{;
						qui replace an_`var'_`k'=1 if temp`i'==`k';
					};
					drop temp`i';
					};
					drop tp1 tp2;
					};
					
					*CLEANING BASED ON CONSISTENCY CHECKS;		
					** ever_fert;
					replace an_ever_fert=1 if an_num_child>0 & an_num_child!=.;
					replace an_ever_fert=0 if an_num_child==0 & LOG_surveyed==1;

					** an_want_1stpreg;
					replace  an_want_1stpreg=. if an_ever_fert==0 & sex==2;


					** everhadsex;
					replace an_everhadsex=1 if an_everusedcondom==1
						 | an_multiplepartners==1 | an_age_firstsex!=. | 
						an_ever_fert==1 | an_condomlastsex==1 | an_age_cur_part!=.
						| an_age_old_part!=. | (an_num_child!=0 & an_num_child!=.);

					** age at first sex;
						replace an_age_firstsex=. if  Q_b4_119_age_1st_sex>30;
						replace an_age_firstsex=. if  Q_b4_119_age_1st_sex<8;
						replace an_age_firstsex=. if  an_age_firstsex>age_survey;


					** marriage;
					replace an_ever_mar=1 if an_spouse_age!=. | an_age_marriage_month!=.;

					
				
			/* CREATE FINAL VARS OF INTEREST FOR TABLES */;
			gen an_ever_fert_unmar=(an_ever_mar==0)*(an_ever_fert==1) if an_ever_fert!=. & an_ever_mar!=.;
			gen an_ever_unfert_mar=(an_ever_mar==1)*(an_ever_fert==0) if an_ever_fert!=. & an_ever_mar!=.;
			gen an_num_preg_ifany=an_num_child if an_ever_fert==1;
				replace an_multiplepartners=0 if an_multiplepartners==. & an_everhadsex==0;
			gen child_by_16=0 if an_ever_fert!=.;
				replace child_by_16=1 if   an_age_1st_child_yr<=16;
			gen reached8=(an_grades_completed>7) if an_grades_completed!=.;

			replace an_multiplepartners=. if an_everhadsex==0;
			gen an_mentions_abst=an_Q_b3_80_how_prevent_hiv_1;
			egen an_mentions_faith=rmax(an_Q_b3_80_how_prevent_hiv_2 an_Q_b3_80_how_prevent_hiv_17);
			egen an_mentions_cond=rmax(an_Q_b3_80_how_prevent_hiv_3 an_Q_b3_80_how_prevent_hiv_4);
			gen an_reports_abst=(an_Q_b5_136_1==1) if (an_Q_b5_135!=. | an_Q_b5_136_1!=.) ;
			egen an_reports_faith=rmax(an_Q_b5_136_3 an_Q_b5_136_4) ;
			replace an_reports_faith=0 if an_reports_faith==. & an_Q_b5_135!=.;
			gen an_reports_cond=(an_Q_b5_136_2==1) if (an_Q_b5_135!=. | an_Q_b5_136_2!=.);


* ignore VCT results for those not randomly sampled for VCT;
	replace hiv_positive=. if sampledVCT!=1;

	save temp, replace;

#delimit cr

* missing outcome data
*****************************************************;
***APPENDIX TABLE A2: ATTRITION IN ROLL CALL DATA;
*********************************************************;

use studysample_allmerged2.dta, clear

#delimit;

/*
foreach visit in 04v1 04v2 05v1 05v2 05v3 {;
	gen presence`visit'=pres`visit';
	replace presence`visit'=0 if pres`visit'==2;
	replace presence`visit'=0.5 if pres`visit'==3;
	replace presence`visit'=. if pres`visit'>3;
};
*/

replace dropout05v3=0 if presence05v3==1;
replace dropout05v3=. if evdead05v3==1;

//egen presence=rmean(presence04v1 presence04v2 presence05v1 presence05v2 presence05v3);

foreach date in 05v3 07v2 {;
	replace evmar`date'=0 if evmar`date'==. & evpreg`date'==1;
};

foreach var in dropout05v3 presence evmar05v3 evpreg05v3 { ; //outcome missing in 2005
			gen `var'_missing=0;
			replace `var'_missing=1 if `var'==. & evdead05v3!=1;
			} ;

foreach var in dropout07v2 evmar07v2 evpreg07v2  { ;
			gen `var'_missing=0;
			replace `var'_missing=1 if `var'==. & evdead07v2!=1;
			} ;

#delimit cr

save attri_SMR.dta, replace 

* attrition in LR survey

use temp.dta, clear

#delimit;
foreach var in an_grades_completed an_ever_fert hsv2_positive { ;
					gen `var'_notmissing=1;
					replace `var'_notmissing=0 if `var'==. & dead!=1;
					} ;

gen found_RT=(eligible_ITsampling==0)*(LOG_surveyed==1);
gen found_IT=(LOG_surveyed==1) if sampledIT==1;
gen found=(found_IT==1) |(found_RT ==1) if dead==0;
#delimit cr

save attri_LR.dta, replace 
