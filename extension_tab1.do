* ================================================================
* This program extends the balance check to incorporate religion
* data sets used - school_religmerge.dta
* output         - table1a_extension.tex
* ================================================================

use school_religmerge.dta, replace

gen var=""
for any  mean_U sd_U mean_H sd_H  mean_UH sd_UH  mean_control sd_control p_Uonly p_Honly p_UH p_UUH p_HUH  N: gen X=.
gen urban=0
replace urban=1 if situation<3

gen sexratio_teachers=Nfemale/(TOTteachers-Nfemale)

gen Honly=(HIVtreat==1) & (Utreat==0)
gen Uonly=(HIVtreat==0) & (Utreat==1)
gen UH=(HIVtreat==1) & (Utreat==1)

#delimit;
local vars=17;
for any  kcpe2003  schsize ratio02  latrine_2004 urban total_2km  
TOTteachers  meanage sexratio_teachers HIVtreat
protestant catholic muslim hindu
religiosity religiosity_2 religiosity_3

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

keep var mean_U sd_U mean_H sd_H mean_UH sd_UH  mean_control sd_control ///
p_Uonly p_Honly p_UH p_UUH p_HUH  N

drop if mean_U==.

//no significant difference in religiosity score among students

dataout , save(table1a_extension.tex) replace tex
