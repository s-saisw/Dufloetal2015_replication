* ==============================================================================
* This program replicates Table 3 PanelA: LR impacts on girls
* data sets used - temp.dta
* output         - tab3_rep.tex
* ==============================================================================


use temp.dta, replace
drop if sex!=2


#delimit ;
	
local depvars 7;
for any 
	reached8	
	an_grades_completed		
	an_ever_mar 
	an_ever_fert 		
	an_ever_fert_unmar 	 
	an_ever_unfert_mar 
	child_by_16			

 \ num 1/`depvars':
	global depvarY="X" ;
#delimit cr

foreach num of numlist 1/7 {
	gen date`num'=Q_realdate
	gen year`num'=Q_year
}

local j 1
while `j' <= `depvars' {

	xi: reg ${depvar`j'} ${treatmentdummies} date`j' year`j' ${controls} ///
	[pw=weight_sample],cluster(schoolid)
	eststo
	
	estadd local SW = "Yes"
	
	test Uonly=UH
	estadd scalar p1 = r(p)
	test Honly=UH
	estadd scalar p2 = r(p)
	test Honly=Uonly
	estadd scalar p3 = r(p)
	test UH=Uonly+Honly
	estadd scalar p4 = r(p)
	
	reg ${depvar`j'} [pw=weight_sample] if group03v1=="C"
	estadd scalar meanf=_b[_cons]
				
	local j=`j'+1
	}

estout  ///
using tab3_rep.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH) ///
stats(SW N p1 p2 p3 p4, ///
labels("Sampling weights" "Observations" "U=UH" "H=UH" "U=H" "UH=U+H") ///
fmt(%9.3f %15.0fc %9.3f %9.3f %9.3f %9.3f)) ///
mlabels("Reach G8" "Grades comp" "Married" "Pregnant" "Preg Unmar" "Mar unpreg" "Child bf 16") ///
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
no clustering. $@starlegend$  ///
\end{tablenotes} \end{threeparttable} \end{center})
	
eststo clear


