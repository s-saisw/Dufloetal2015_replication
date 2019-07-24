* ==================================================================================
* This program replicates results from Table A2 for girls only
* data sets used - attri_SMR.dta
* output         - taba2_rep.tex
* ==================================================================================

use attri_SMR.dta, replace
drop if sex !=2

foreach x in dropout05v3 presence evmar05v3 evpreg05v3 ///
dropout07v2 evmar07v2 evpreg07v2 {
	xi: reg `x'_missing ${treatmentdummies} ${controlsR}, cluster(sch03v1)
	eststo
	summarize `x'_missing if group03v1=="C"
	estadd scalar mean = r(mean)
	test Uonly=UH
	estadd scalar p1=r(p)
	test Honly=UH
	estadd scalar p2=r(p)
	test Honly=Uonly
	estadd scalar p3=r(p)
}

estout est1 est2 est3 est4 est5 est6 est7 ///
using taba2_rep.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH) ///
stats(N mean p1 p2 p3, ///
labels("Observations" "Control attrition" "U=UH" "H=UH" "U=H") ///
fmt(%15.0fc %9.3f %9.3f %9.3f %9.3f )) ///
mlabels("Dropout" "Presence" "Married" "Pregnant" "Dropout" "Married" "Pregnant") ///
mgroups("After 3 years" "After 5 years", pattern(1 0 0 0 1 0 0)) ///
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

//the original code computes p values for  UH=Uonly+Honly as well. This is meaningless

/*
eststo clear
xi: reg `x'_missing ${treatmentdummies} ${controlsR}, cluster(sch03v1)
eststo
sum  dropout05v3_missing if group03v1=="C"
gen mean=r(mean)
test Uonly=UH
test Honly=UH
test Honly=Uonly

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

*/


