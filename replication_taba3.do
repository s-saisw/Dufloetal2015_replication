* ==================================================================================
* This program replicates results from Table A2 for girls only
* data sets used - attri_LR.dta
* output         - table3A_1.xls
*                - table3A_2.xls
* ==================================================================================

use attri_LR.dta, replace 

#delimit;
local i 2;
while `i'>0 { ;
		sum dead if sex==`i' & group03v1=="C";
		gen mean=r(mean);
		xi: reg dead ${treatmentdummies} ${controls}, cluster(sch03v1), if sex==`i';
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;


		sum found_RT if sex==`i' & group03v1=="C"  & dead==0;
		gen mean=r(mean);
		xi: reg found_RT ${treatmentdummies} ${controls} , cluster(sch03v1), if sex==`i' & dead==0;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
		outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		foreach var in  sampledIT found_IT { ;
			sum  `var' if sex==`i' & group03v1=="C"  & dead==0;
			gen mean=r(mean);
			xi: reg `var' ${treatmentdummies} ${controls}, cluster(sch03v1), if sex==`i' & found_RT==0 & dead==0;
			if mean!=0 {;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

					};
			if mean==0 {; 
				local p1=-99;
				local p2=-99;
				local p3=-99;
				local p4=-99;
			};
			cap outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		};
		

		sum found if sex==`i' & group03v1=="C" & dead==0;
		gen mean=r(mean);
		xi: reg found ${treatmentdummies} ${controls}, cluster(sch03v1), if sex==`i' & dead==0;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		sum found if sex==`i' & group03v1=="C" & weight_sample!=. & dead==0;
		gen mean=r(mean);
		xi: reg found ${treatmentdummies} ${controls} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
		drop mean;

		foreach var in  an_grades_completed an_ever_fert hsv2_positive  { ;
			sum  `var'_notmissing if sex==`i' & group03v1=="C" & weight_sample!=.;
			gen mean=r(mean);
			xi: reg `var'_notmissing ${treatmentdummies} ${controls} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0;
			if mean!=0 {;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

					};
			if mean==0 {; 
				local p1=-99;
				local p2=-99;
				local p3=-99;
				local p4=-99;

			};
			cap outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		} ;
	
		foreach var in  Q_realdate LOG_bd_BDrealdate  { ;
			sum  `var' if sex==`i' & group03v1=="C" & weight_sample!=.;
			gen mean=r(mean);
			xi: reg `var' ${treatmentdummies} ${controls} [pw=weight_sample], cluster(sch03v1), if sex==`i' & dead==0;
			if mean!=0 {;
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

					};
			if mean==0 {; 
				local p1=-99;
				local p2=-99;
				local p3=-99;
				local p4=-99;

			};
			cap outreg2 ${treatmentdummies} using "table3A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		} ;
	
		
local i=`i'-1;
} ;

#delimit cr
