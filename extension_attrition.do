* ==============================================================================
* This program addresses selection bias: Heckman 2step, Lee's bound estimator
* data sets used - attri_SMR.dta
*                - temp.dta
* output         - tab_hec_SR.tex
*                - tab_lee_SR.tex
*                - tab_lee_ols.tex
* ==============================================================================

use attri_SMR.dta, replace
drop if sex !=2

****************************
*** Heckman 2-step model ***
****************************

gen distance = Q_b1_21

foreach x in dropout05v3 presence {
	xi: heckman `x' ${treatmentdummies} ${controlsR}, ///
	twostep select(${treatmentdummies} ${controlsR}) 
	eststo

	xi: heckman `x' ${treatmentdummies} ${controlsR}, ///
	twostep select(${treatmentdummies} ${controlsR} distance)
	eststo
	estadd local IV = "Yes"
}

estout est1 est2 est3 est4 ///
using tab_hec_SR.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH distance) ///
stats(N IV, ///
labels("Observations" "Instrument") ///
fmt(%15.0fc %9.3f )) ///
mlabels(none) ///
mgroups("Dropout" "Presence", pattern(1 0 1 0)) ///
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

/*
The following lines of code test the strength of 'distance' as instrument.
*/

foreach x in dropout05v3 presence evmar05v3 evpreg05v3{
	gen `x'_found = 0
	replace `x'_found = 1 if `x'_missing==0
}

xi: reg dropout05v3_found distance ${treatmentdummies} ${controlsR}
test distance

*****************************
*** Lee's bound estimator ***
*****************************

/*
Since group03v1 (group indicator in the original data) is a string variable,
making it hard to operate, we generate new numeric categorical variable here:
*/

gen treatgr = .
replace treatgr = 0 if Utreat == 0 & HIVtreat == 0
replace treatgr = 1 if Utreat == 1 & HIVtreat == 0
replace treatgr = 2 if Utreat == 0 & HIVtreat == 1
replace treatgr = 3 if Utreat == 1 & HIVtreat == 1

/*
leebounds command only allows for binary treatment variable and does not
allow continuous covariates. To make the results comparable to ols, we first use 
ols to separately estimate the difference between each treatment and control 
group without including any covariates.
*/

foreach x in dropout05v3 presence {
	leebounds `x' Uonly if treatgr == 0 | treatgr == 1 
	eststo
	leebounds `x' Honly if treatgr == 0 | treatgr == 2
	eststo
	leebounds `x' UH if treatgr == 0 | treatgr == 3
	eststo
}

foreach x in dropout05v3 presence {
	reg `x' Uonly if treatgr == 0 | treatgr == 1 
	eststo
	reg `x' Honly if treatgr == 0 | treatgr == 2
	eststo
	reg `x' UH if treatgr == 0 | treatgr == 3
	eststo
}

estout est1 est2 est3 est4 est5 est6 ///
using tab_lee_SR.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
stats(N trim, ///
labels("Observations" "Trimming proportion") ///
fmt(%15.0fc %9.3f )) ///
mlabels("Uonly" "Honly" "UH" "Uonly" "Honly" "UH") ///
mgroups("Dropout" "Presence", pattern(1 0 0 1 0 0)) ///
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

estout est7 est8 est9 est10 est11 est12 ///
using tab_lee_ols.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH) ///
stats(N , ///
labels("Observations" ) ///
fmt(%15.0fc )) ///
mlabels(none) ///
mgroups("Dropout" "Presence", pattern(1 0 0 1 0 0)) ///
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

********************************
*** Lee's bound with LR data ***
********************************
use temp.dta, replace
drop if sex!=2

gen treatgr = .
replace treatgr = 0 if Utreat == 0 & HIVtreat == 0
replace treatgr = 1 if Utreat == 1 & HIVtreat == 0
replace treatgr = 2 if Utreat == 0 & HIVtreat == 1
replace treatgr = 3 if Utreat == 1 & HIVtreat == 1


foreach x in reached8 an_grades_completed {
	leebounds `x' Uonly if treatgr == 0 | treatgr == 1 [pw=weight_sample]
	eststo
	estadd local SW = "Yes"
	leebounds `x' Honly if treatgr == 0 | treatgr == 2 [pw=weight_sample]
	eststo
	estadd local SW = "Yes"
	leebounds `x' UH if treatgr == 0 | treatgr == 3 [pw=weight_sample]
	eststo
	estadd local SW = "Yes"
}

estout ///
using tab3_lee.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
stats(SW N trim, ///
labels("Sampling weights" "Observations" "Trimming proportion") ///
fmt(%9.3f %15.0fc %9.3f)) ///
mlabels("Uonly" "Honly" "UH" "Uonly" "Honly" "UH") ///
mgroups("Reach G8" "Grades completed", pattern(1 0 0 1 0 0)) ///
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

foreach x in reached8 an_grades_completed{
	reg `x' Uonly if treatgr == 0 | treatgr == 1 [pw=weight_sample]
	eststo
	estadd local SW = "Yes"
	reg `x' Honly if treatgr == 0 | treatgr == 2 [pw=weight_sample]
	eststo
	estadd local SW = "Yes"
	reg `x' UH if treatgr == 0 | treatgr == 3 [pw=weight_sample]
	eststo
	estadd local SW = "Yes"
}

estout ///
using tab3_lee_OLS.tex, replace ///
cells(b(star fmt(3)) se(par)) ///
keep(Uonly Honly UH) ///
stats(SW N, ///
labels("Sampling weights" "Observations" ) ///
fmt(%9.3f %15.0fc %9.3f)) ///
mlabels(none) ///
mgroups("Reach G8" "Grades completed", pattern(1 0 0 1 0 0)) ///
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

