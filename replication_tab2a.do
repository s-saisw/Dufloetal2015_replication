* ==================================================================================
* This program replicates results from Table 2A col 1, 3, 4, 5, and 6 for girls only
* data sets used - studysample_allmerged2.dta
* output         - tab2a_rep.tex
* ==================================================================================

use studysample_allmerged2.dta, replace
drop if sex !=2 //keep only females

eststo clear

foreach x in dropout05v3 evmar05v3 evpreg05v3 evpregunmar05v3 evunpregmar05v3 { 
    xi:regress `x' $treatmentdummies $controlsR , cluster(sch03v1)
	eststo
	sum `x' if group03v1=="C"
	estadd scalar mean = r(mean)
	test Uonly=UH
	estadd scalar p1=r(p)
	test Honly=UH
	estadd scalar p2=r(p)
	test Honly=Uonly
	estadd scalar p3=r(p)
	test UH=Uonly+Honly
	estadd scalar p4=r(p)
}

estout est1 est2 est3 est4 est5 ///
using tab2a_rep.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH) ///
stats(N mean p1 p2 p3 p4, ///
labels("Observations" "Control mean" "U=UH" "H=UH" "U=H" "UH=U+H") ///
fmt(%15.0fc %9.3f %9.3f %9.3f %9.3f %9.3f )) ///
mlabels("Dropout" "married" "pregnant" "preg unmar." "mar unpreg.") ///
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
