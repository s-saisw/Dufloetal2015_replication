* ==================================================================================
* This program replicates results from Table A2 column 1-4 for girls only
* data sets used - attri_LR.dta
* output         - taba3_rep.tex
* ==================================================================================

use attri_LR.dta, replace 
drop if sex !=2

* no sampling weight
foreach x in dead { //m1
	xi: reg `x' ${treatmentdummies} ${controls}, cluster(sch03v1)
	eststo
	summarize `x' if group03v1=="C"
	estadd scalar mean=r(mean)
	test Uonly=UH
	estadd scalar p1=r(p)
	test Honly=UH
	estadd scalar p2=r(p)
	test Honly=Uonly
	estadd scalar p3=r(p)
}

foreach x in found_RT found { //m2, m4
	xi: reg `x' ${treatmentdummies} ${controls} ///
	if dead==0, ///
	cluster(sch03v1)
	eststo
	summarize `x' if group03v1=="C" & dead==0 
	estadd scalar mean=r(mean)
	test Uonly=UH
	estadd scalar p1=r(p)
	test Honly=UH
	estadd scalar p2=r(p)
	test Honly=Uonly
	estadd scalar p3=r(p)
}

foreach x in found_IT { //m3
	xi: reg `x' ${treatmentdummies} ${controls} ///
	if dead==0 & found_RT==0, ///
	cluster(sch03v1)
	eststo
	summarize `x' if group03v1=="C" & dead==0 & found_RT==0
	estadd scalar mean=r(mean)
	test Uonly=UH
	estadd scalar p1=r(p)
	test Honly=UH
	estadd scalar p2=r(p)
	test Honly=Uonly
	estadd scalar p3=r(p)
}

estout est1 est2 est4 est3 ///
using taba3_rep.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH) ///
stats(N mean p1 p2 p3, ///
labels("Observations" "Control attrition" "U=UH" "H=UH" "U=H") ///
fmt(%15.0fc %9.3f %9.3f %9.3f %9.3f )) ///
mlabels("Dead" "Found RT" "Found IT" "Found") ///
collabels(none) ///
label wrap ///
numbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) ///
style(tex) varlabels(_cons Constant) ///
prehead(\begin{center} \begin{threeparttable} ///
\begin{tabular}{l*{@M}{r}} \hline \hline ) ///
posthead(\hline ) ///
prefoot(\hline ) ///
postfoot(\hline \end{tabular} \begin{tablenotes} \small ///
\item Standard errors in parentheses, ///
clustered by school. $@starlegend$  ///
\end{tablenotes} \end{threeparttable} \end{center})

eststo clear

*original do-file
/*
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
*/
