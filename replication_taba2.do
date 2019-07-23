* ==================================================================================
* This program replicates results from Table A2 for girls only
* data sets used - attri_SMR.dta
* output         - table2a_1.xls
*                - table2a_2.xls
* ==================================================================================

use attri_SMR.dta, replace

#delimit;
local i 2;
while `i'>0 { ;
		sum  dropout05v3_missing if sex==`i' & group03v1=="C"; //outcome missing in 2007
		gen mean=r(mean);
		xi: reg dropout05v3_missing ${treatmentdummies} ${controlsR}, cluster(sch03v1), if sex==`i';
			test Uonly=UH;
				local p1=r(p);
			test Honly=UH;
				local p2=r(p);
			test Honly=Uonly;
				local p3=r(p);
			test UH=Uonly+Honly;
				local p4=r(p);

		outreg2 ${treatmentdummies} using table2A_`i'.xls, nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  replace;
		drop mean;

		foreach var in presence evmar05v3 evpreg05v3 dropout07v2 evmar07v2 evpreg07v2 { ;
			sum  `var'_missing if sex==`i' & group03v1=="C";
			gen mean=r(mean);
			xi: reg `var'_missing ${treatmentdummies} ${controlsR}, cluster(sch03v1), if sex==`i';
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
			cap outreg2 ${treatmentdummies} using "table2A_`i'.xls", nor2 bdec(3) se nonotes  /*sigsymb(***,**,*)*/ 10pct addstat("Mean C", mean, "U=UH", `p1', "H=UH", `p2',"U=H", `p3', "UH=U+H", `p4') adec(3)  append;
			drop mean;
		} ;
	
local i=`i'-1;
} ;
