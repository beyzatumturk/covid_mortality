
***************************************************************** Figure 1: Mortality Trends
use "4finaldata/STATA_data/deaths_by_age.dta", clear

keep if age != 90
collapse (sum) deaths_imp, by(year cohort)
keep if cohort > 1933 & cohort < 1970


twoway (line deaths_imp cohort if year == 2009, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2010, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2011, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2012, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2013, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2014, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2015, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2016, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2017, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2018, lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2019, lcolor(black)) ///
       (line deaths_imp cohort if year == 2020, lcolor(red)) ///
       (line deaths_imp cohort if year == 2021, lcolor(midblue)) ///
       (line deaths_imp cohort if year == 2022, lcolor(midgreen)) ///
       (line deaths_imp cohort if year == 2023, lcolor(magenta)) ///
       (scatter deaths_imp cohort if year == 2019, mcolor(black) msize(vsmall)) ///
	   (scatter deaths_imp cohort if year == 2020, mcolor(red) msize(vsmall)) ///
	   (scatter deaths_imp cohort if year == 2021, mcolor(midblue) msize(vsmall)) ///
	   (scatter deaths_imp cohort if year == 2022, mcolor(midgreen) msize(vsmall)) ///
	   (scatter deaths_imp cohort if year == 2023, mcolor(magenta) msize(vsmall)) ///
       , xline(1955.5) ///
	   xtitle("birth year") ytitle("Nb of Deaths") ///
       xlabel(1930(10)1970, grid) xscale(reverse) yscale(range(0 .)) ///
       xsize(15) ysize(10) scale(0.75) ///
       plotregion(margin(5 5 5 5)) ///
       legend(size(small) order(1 "2009-2018: Pre-COVID, No Curfew" 11 "2019: Pre-COVID, No Curfew" 12 "2020: COVID, Curfew" 13 "2021: COVID, No Curfew" 14 "2022: COVID, No Curfew" 15 "2023: Post-COVID, No Curfew") ///
              pos(6) col(2))
	  graph export "6graphs/level/figure1_trends.eps", as(eps) name("Graph") preview(off) fontface("Georgia")replace

	
	
***************************************************************** Figure 2: Causes of Mortality
use "4finaldata/STATA_data/cause_of_death.dta", clear
destring(deaths_imp), replace
drop if age=="'90+"
destring(age), replace

replace cause_of_death="nervous" if cause_of_death=="Sinir sistemi ve duyu organları hastalıkları" 
replace cause_of_death="digestive" if cause_of_death=="Sindirim sistemi hastalıkları"
replace cause_of_death="respiratory" if cause_of_death=="Solunum sistemi hastalıkları" 
replace cause_of_death="other" if cause_of_death!="nervous" & cause_of_death!="digestive" &  cause_of_death!="respiratory"
collapse (sum) deaths_imp, by(year cause_of_death age)
gen cohort= year-age
keep if cohort > 1933 & cohort < 1970

local vars= "nervous digestive respiratory"

foreach var in `vars' {			  
	   twoway (line deaths_imp cohort if year == 2009 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2010 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2011 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2012 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2013 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2014 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2015 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2016 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2017 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2018 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2019 & cause_of_death== "`var'", lcolor(black)) ///
       (line deaths_imp cohort if year == 2020 & cause_of_death== "`var'", lcolor(red)) ///
       (line deaths_imp cohort if year == 2021 & cause_of_death== "`var'", lcolor(midblue)) ///
       (line deaths_imp cohort if year == 2022 & cause_of_death== "`var'", lcolor(midgreen)) ///
       (line deaths_imp cohort if year == 2023 & cause_of_death== "`var'", lcolor(magenta)) ///
       , xline(1955.5) ///
	   xtitle("birth year") ytitle("`var' system conditions") ///
       xlabel(1940(10)1970, grid) ylabel(, grid)xscale(reverse) yscale(range(0 .)) ///
       xsize(15) ysize(10) scale(0.75) ///
       plotregion(margin(5 5 5 5)) /// Increase margins to move axis titles outside the plot
       legend(size(small) order(1 "2009-2018: Pre-COVID, No Curfew" 11 "2019: Pre-COVID, No Curfew" 12 "2020: COVID, Curfew" 13 "2021: COVID, No Curfew" 14 "2022: COVID, No Curfew" 15 "2023: Post-COVID, No Curfew") pos(6) col(2))
	   
	   graph save "Graph" "6graphs/level/09_`var'_deaths.gph", replace		  
			  
}

local var="other" 
	   twoway (line deaths_imp cohort if year == 2009 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2010 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2011 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2012 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2013 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2014 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2015 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2016 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2017 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2018 & cause_of_death== "`var'", lcolor(gs13)) ///
       (line deaths_imp cohort if year == 2019 & cause_of_death== "`var'", lcolor(black)) ///
       (line deaths_imp cohort if year == 2020 & cause_of_death== "`var'", lcolor(red)) ///
       (line deaths_imp cohort if year == 2021 & cause_of_death== "`var'", lcolor(midblue)) ///
       (line deaths_imp cohort if year == 2022 & cause_of_death== "`var'", lcolor(midgreen)) ///
       (line deaths_imp cohort if year == 2023 & cause_of_death== "`var'", lcolor(magenta)) ///
       , xline(1955.5) ///
	   xtitle("birth year") ytitle("other reasons") ///
       xlabel(1940(10)1970, grid) ylabel(, grid)xscale(reverse) yscale(range(0 .)) ///
       xsize(15) ysize(10) scale(0.75) ///
       plotregion(margin(5 5 5 5)) /// Increase margins to move axis titles outside the plot
       legend(size(small) order(1 "2009-2018: Pre-COVID, No Curfew" 11 "2019: Pre-COVID, No Curfew" 12 "2020: COVID, Curfew" 13 "2021: COVID, No Curfew" 14 "2022: COVID, No Curfew" 15 "2023: Post-COVID, No Curfew") ///
              pos(6) col(2))	  
	   graph save "Graph" "6graphs/level/09_`var'_deaths.gph", replace	  	  


		
graph combine "6graphs/level/09_respiratory_deaths.gph" ///
"6graphs/level/09_digestive_deaths.gph" ///
"6graphs/level/09_nervous_deaths.gph" ///
"6graphs/level/09_other_deaths.gph", col(2) ///
imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(5) xsize(7.5)

graph export "6graphs/level/09_death_cause.eps", as(eps) name("Graph") preview(off) fontface("Georgia")replace
	
	  
	  
***************************************************************** Figure 3: Excess mortality Trends	
 set seed 6683 
 use "4finaldata/STATA_data/deaths_by_age.dta", clear
 keep if age != 90
replace cohort=year-age
gen tdata= year<2020

gen year_n= year - 2008
gen year_n_sq= year_n^2

tabulate age, generate(age_t)
tabulate cohort, generate(cohort_t)
tabulate pcode, generate(pcode_t)

drop age_t1
drop cohort_t1
drop pcode_t1

gen training=0
replace training=1 if year<2019

egen year_n_sq_std= std(year_n_sq)
egen year_n_std= std(year_n)

lasso linear deaths_imp age_t* cohort_t* pcode_t* year_n_sq year_n if training==1, selection(cv)
predict deaths_imp_pred if training==0


collapse (mean) deaths_imp deaths_imp_pred, by (cohort year)

keep if year>2018
keep if cohort < 1979 & cohort > 1933
sort year cohort

******** seperate graphs
preserve
twoway (line deaths_imp cohort if cohort < 1979 & cohort > 1933, lcolor(black) lwidth(medium) lpattern(solid)) ///
       (line deaths_imp_pred cohort if cohort < 1979 & cohort > 1933, lcolor(red) lwidth(medium) lpattern(solid)) ///
       (scatter deaths_imp cohort if cohort < 1979 & cohort > 1933, mcolor(black) msize(vsmall) msymbol(O)) ///
       (scatter deaths_imp_pred cohort if cohort < 1979 & cohort > 1933, mcolor(red) msize(vsmall) msymbol(O)), ///
       xlabel(1980(10)1940, grid) ylabel(, grid) legend(order(1 "Actual" 2 "Predicted") cols(2) size(small) region(lstyle(none))) ///
	   xline(1955, lpattern(dash) lcolor(black)) xsc(reverse) ///
       xtitle("Birth Year", size(small)) ytitle("Nb of Deaths", size(small)) ///
	    graphregion(fcolor(white)) ///
		xsize(16) ysize(10) ///
       by(year, note("")) 	
	   
	   graph export "6graphs/level/02_Fig2_ExcessMortality.eps", as(eps) name("Graph") preview(off) fontface("Georgia") replace
restore


********* combined graphs
gen excess= deaths_imp-deaths_imp_pred
collapse (sum) excess, by(year cohort)
keep if cohort > 1933 & cohort < 1970


twoway (line excess cohort if year == 2019, lcolor(black)) ///
       (line excess cohort if year == 2020, lcolor(red)) ///
       (line excess cohort if year == 2021, lcolor(midblue)) ///
       (line excess cohort if year == 2022, lcolor(midgreen)) ///
       (line excess cohort if year == 2023, lcolor(magenta)) ///
       (scatter excess cohort if year == 2019, mcolor(black) msize(vsmall)) ///
	   (scatter excess cohort if year == 2020, mcolor(red) msize(vsmall)) ///
	   (scatter excess cohort if year == 2021, mcolor(midblue) msize(vsmall)) ///
	   (scatter excess cohort if year == 2022, mcolor(midgreen) msize(vsmall)) ///
	   (scatter excess cohort if year == 2023, mcolor(magenta) msize(vsmall)) ///
       , xline(1955.5) ///
	   yline(0, lpattern(dash) lcolor(red)) ///
	   xtitle("Birth year") ytitle("Excess number of deaths") ///
       xlabel(1940(10)1970, grid) xscale(reverse) yscale(range(0 .)) ///
       xsize(15) ysize(10) scale(0.75) ///
       plotregion(margin(5 5 5 5)) /// Increase margins to move axis titles outside the plot
       legend(size(small) order(1 "2019: Pre-COVID, No Curfew" 2 "2020: COVID, Curfew" 3 "2021: COVID, No Curfew" 4 "2022: COVID, No Curfew" 5 "2023: Post-COVID, No Curfew") ///
              pos(6) col(2))
	   graph export "6graphs/level/02_Fig2_ExcessMortalityCombined.eps", as(eps) name("Graph") preview(off) fontface("Georgia")replace

***************************************************************** Table 1: Excess Mortality Tests

python: import sklearn
set seed 6683
use "4finaldata/STATA_data/deaths_by_age.dta", clear


keep if age != 90
replace cohort=year-age
gen tdata= year<2020

gen year_n= year - 2008
gen year_n_sq= year_n^2

tabulate age, generate(age_t)
tabulate cohort, generate(cohort_t)
tabulate pcode, generate(pcode_t)

drop age_t1
drop cohort_t1
drop pcode_t1

gen training=0
replace training=1 if year<2019

egen year_n_sq_std= std(year_n_sq)
egen year_n_std= std(year_n)

pystacked deaths_imp age_t* cohort_t* pcode_t* year_n_sq year_n if training==1, type(regress) method(lassoic)
predict death_pc_imp_pred, xb

gen treat=1 if cohort<1956
keep if year>=2019
replace treat=0  if treat==.
keep if cohort < 1979 & cohort > 1933
 
drop age_t*
drop pcode_t*
drop cohort_t*

gen dif = deaths_imp - death_pc_imp_pred


areg dif i.treat##i.year, abs(pcode)
outreg2 using "6graphs/level/03_Tab1_TripleDifreg.tex", replace


test 1.treat#2020.year = 1.treat#2021.year
gen F_1= r(F)
gen p_1= r(p)

test 1.treat#2020.year = 1.treat#2022.year
gen F_2= r(F)
gen p_2= r(p)

test 1.treat#2020.year = 1.treat#2023.year
gen F_3= r(F)
gen p_3= r(p)

test 1.treat#2020.year = 1.treat#2021.year = 1.treat#2022.year
gen F_4= r(F)
gen p_4= r(p)

test 1.treat#2020.year = 1.treat#2021.year = 1.treat#2023.year
gen F_5= r(F)
gen p_5= r(p)

test 1.treat#2020.year = 1.treat#2021.year = 1.treat#2022.year = 1.treat#2023.year
gen F_6= r(F)
gen p_6= r(p)


forvalues i=1/6 {
	preserve
	keep F_* p_*
	duplicates drop
	keep F_`i' p_`i'
	replace F_`i'=round(F_`i', 0.01)
	replace p_`i'=round(p_`i', 0.01)
	rename F_`i' F_statistic
	rename p_`i' p_value
	gen test=`i'
	tempfile temp_`i'
	save `temp_`i'', replace
	restore
}

preserve
use `temp_1', clear

forvalues i=2/6 {
	append using `temp_`i''
	save `temp_1', replace
}

use `temp_1', clear
gen term=""
replace term="Test 1: 2020 = 2021" if test==1
replace term="Test 2: 2020 = 2022" if test==2
replace term="Test 3: 2020 = 2023" if test==3
replace term="Test 4: 2020 = 2021 = 2022" if test==4
replace term="Test 5: 2020 = 2021 = 2023" if test==5
replace term="Test 6: 2020 = 2021 = 2022 = 2023" if test==6

drop test
order term F_statistic p_value

texsave  using "6graphs/level/03_Tab1_TripleDif.tex", replace


***************************************************************** Table 2: Mortality Tests	 

set seed 6683
use "4finaldata/STATA_data/deaths_by_age.dta", clear
keep if age != 90
gen tdata = year < 2020
gen curfew = year == 2020
gen heaper = mod(cohort, 5) == 0

gen di1=1955-cohort
gen treat = cohort <= 1955

keep if age != 90
keep if cohort < 1979 & cohort > 1933

areg deaths_imp i.treat#i.year i.year, abs(pcode) 
outreg2 using "6graphs/level/07_Tab2_tretmentReg.tex", replace

 test 1.treat#2020.year = 1.treat#2019.year
 gen F_0= r(F)
 gen p_0= r(p)
 
 test 1.treat#2020.year = 1.treat#2021.year
 gen F_1= r(F)
 gen p_1= r(p)

 test 1.treat#2020.year = 1.treat#2022.year
gen F_2= r(F)
gen p_2= r(p)

  test 1.treat#2020.year = 1.treat#2023.year
  gen F_3= r(F)
gen p_3= r(p)

 test 1.treat#2020.year  = 1.treat#2021.year = 1.treat#2022.year
 gen F_4= r(F)
gen p_4= r(p)
 test 1.treat#2020.year  = 1.treat#2021.year = 1.treat#2023.year
 gen F_5= r(F)
gen p_5= r(p)
 test 1.treat#2020.year  = 1.treat#2021.year = 1.treat#2022.year = 1.treat#2023.year
 gen F_6= r(F)
gen p_6= r(p)

test 1.treat#2019.year = 1.treat#2023.year
 gen F_7= r(F)
 gen p_7= r(p)
 


forvalues i=0/7 {
	preserve
	keep F_* p_*
	duplicates drop
	keep F_`i' p_`i'
	replace F_`i'=round(F_`i', 0.01)
	replace p_`i'=round(p_`i', 0.01)
	rename F_`i' F_statistic
	rename p_`i' p_value
	gen test=`i'
	tempfile temp_`i'
	save `temp_`i'', replace
	restore
}

preserve
use `temp_0', clear

forvalues i=1/7 {
	append using `temp_`i''
	save `temp_0', replace
}

use `temp_0', clear
gen term=""
replace term="Test 1: 2020 = 2019" if test==0
replace term="Test 2: 2020 = 2021" if test==1
replace term="Test 3: 2020 = 2022" if test==2
replace term="Test 4: 2020 = 2023" if test==3
replace term="Test 5: 2020 = 2021 = 2022" if test==4
replace term="Test 6: 2020 = 2021 = 2023" if test==5
replace term="Test 7: 2020 = 2021 = 2022 = 2023" if test==6
replace term="Test 8: 2019 = 2023" if test==7

drop test
order term F_statistic p_value

texsave  using "6graphs/level/07_Tab2_tretment_impact.tex", replace
restore  
	   
	  
***************************************************************** Figure 4: Regression Discontinuity
	   

forvalues year = 2016/2023 {
use "4finaldata/STATA_data/deaths_by_age.dta", clear
keep if age != 90
gen tdata = year < 2020
gen treat = age > 64
gen curfew = year == 2020
gen heaper = mod(cohort, 5) == 0

/* 
reg death_pc_imp i.pcode i.cohort
predict res, residuals
*/

gen di1=1955-cohort

keep if age != 90
keep if year == `year'
keep if abs(di1) < 13
sort di1
bys di1: egen rd_bin_mean = mean(deaths_imp)

*collapse (sum) deaths_imp, by(age year treat di1)

areg deaths_imp di1 if di1 < 0, absorb(pcode)
predict yhat1, xb
predict l_se, stdp
gen ub1 = yhat1 + 1.96 * l_se
gen lb1 = yhat1 - 1.96 * l_se 

areg deaths_imp di1 if di1 > 0, absorb(pcode)
predict yhat2, xb
predict r_se, stdp
gen ub2 = yhat2 + 1.96 * r_se
gen lb2 = yhat2 - 1.96 * r_se

twoway (line yhat1 di1 if di1 <= 0, lcolor(black)) ///
       (line yhat2 di1 if di1 >= 0, lcolor(black)) ///
       (line ub1 di1 if di1 <= 0, lcolor(gs12) lpattern(solid)) ///
       (line lb1 di1 if di1 <= 0, lcolor(gs12) lpattern(solid)) ///
       (line ub2 di1 if di1 >= 0, lcolor(gs12) lpattern(solid)) ///
       (line lb2 di1 if di1 >= 0, lcolor(gs12) lpattern(solid)) ///
       (scatter rd_bin_mean di1, mcolor(black) msize(tiny)) ///
       (scatter rd_bin_mean di1, mcolor(black) msize(tiny)), ///
       xtitle("") ytitle("") ///
       title("`year', deaths", size(medium) margin(0 0 5 0)) ///
       graphregion(color(white)) plotregion(margin(5 5 5 5)) ///
	   legend(off) ///
	   ysize(5) xsize(6) ///
       xlabel(-10(5)10, nogrid) ylabel(, nogrid) ///
	   xline(0, lcolor(gs10) lwidth(medium) lpattern(3pt))
	   
graph save "Graph" "6graphs/level/05_`year'_deaths.gph", replace
}


	
	
graph combine "6graphs/level/05_2018_deaths.gph" ///
"6graphs/level/05_2019_deaths.gph" ///
"6graphs/level/05_2020_deaths.gph" ///
"6graphs/level/05_2021_deaths.gph" ///
"6graphs/level/05_2022_deaths.gph" ///
"6graphs/level/05_2023_deaths.gph", col(3) ///
imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(5) xsize(6.5)

graph export "6graphs/level/05__combined.eps", as(eps) name("Graph") preview(off) fontface("Georgia")replace
	  
	  
***************************************************************** Figure 6: Bandwidths - Triangular
use "4finaldata/STATA_data/deaths_by_age.dta", clear
// Create a dataset to hold the results
tempfile results
save `results', emptyok

keep if age != 90
gen tdata = year < 2020
gen treat = age > 64
gen curfew = year == 2020
gen heaper = mod(cohort, 5) == 0

gen di1=1955-cohort

foreach h of numlist 5/20 {
	gen est_`h' = .
	gen se_`h' = .
	gen se_`h'_l = .
	gen se_`h'_h = .
}

egen pcode_dummies = group(pcode)

forvalues year = 2016/2023 {
	foreach h of numlist 5/20 {	
		rdrobust deaths_imp di1 if year == `year', c(0) h(`h') vce("hc0") covs(pcode_dummies)
		replace est_`h' =  e(tau_cl) if year== `year'
		replace se_`h' = e(se_tau_cl) if year== `year'
		replace se_`h'_l = est_`h' - se_`h' if year== `year'
		replace se_`h'_h = est_`h' + se_`h' if year== `year'
		
	}
}

forvalues year = 2016/2023 {
	foreach h of numlist 5/20 {	
		preserve
		keep est_`h' year se_`h' se_`h'_l se_`h'_h
		rename est_`h' est
		rename se_`h' se
		rename se_`h'_l se_l
		rename se_`h'_h se_h
		keep if year== `year'
		gen bandwidth= `h'
		duplicates drop
		tempfile bandwidth_`year'_`h'
		save `bandwidth_`year'_`h''
		restore
	}
}


preserve
keep year
keep if year==0
tempfile bandwidths
save `bandwidths'
restore


use `bandwidths', clear
forvalues year = 2016/2023 {
	foreach h of numlist 5/20 {	
		append using `bandwidth_`year'_`h''
	}
}

save `bandwidths', replace


forvalues year = 2016/2023 {
twoway (rcap se_l se_h bandwidth if year==`year', lcolor(black)) (scatter est bandwidth if year==`year', msymbol(O) msize(small) mcolor(black)), ///
       xtitle("Bandwidths") ytitle("") ///
       title("`year'", size(medium) margin(0 0 5 0)) ///
       graphregion(color(white)) plotregion(margin(5 5 5 5)) ///
	   yline(0, lpattern(dash)) ///
	   legend(off) ///
	   ysize(5) xsize(6) ///
       xlabel(, nogrid) ylabel(-60(30)60, nogrid) 
	   graph save "Graph" "6graphs/level/06_`year'_bandwidth.gph", replace
}
 
 graph combine "6graphs/level/06_2018_bandwidth.gph" ///
"6graphs/level/06_2019_bandwidth.gph" ///
"6graphs/level/06_2020_bandwidth.gph" ///
"6graphs/level/06_2021_bandwidth.gph" ///
"6graphs/level/06_2022_bandwidth.gph" ///
"6graphs/level/06_2023_bandwidth.gph", col(3) ///
imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(6.5) xsize(9)

graph export "6graphs/level/06__combined_bandwidth.eps", as(eps) name("Graph") preview(off) fontface("Georgia")replace

	   
***************************************************************** Figure 6: Bandwidths - Uniform
use "4finaldata/STATA_data/deaths_by_age.dta", clear
// Create a dataset to hold the results
tempfile results
save `results', emptyok

keep if age != 90
gen tdata = year < 2020
gen treat = age > 64
gen curfew = year == 2020
gen heaper = mod(cohort, 5) == 0

gen di1=1955-cohort

foreach h of numlist 5/20 {
	gen est_`h' = .
	gen se_`h' = .
	gen se_`h'_l = .
	gen se_`h'_h = .
}

egen pcode_dummies = group(pcode)

forvalues year = 2016/2023 {
	foreach h of numlist 5/20 {	
		rdrobust deaths_imp di1 if year == `year', c(0) h(`h') kernel("uni") vce("hc0") covs(pcode_dummies)
		replace est_`h' =  e(tau_cl) if year== `year'
		replace se_`h' = e(se_tau_cl) if year== `year'
		replace se_`h'_l = est_`h' - se_`h' if year== `year'
		replace se_`h'_h = est_`h' + se_`h' if year== `year'
		
	}
}

forvalues year = 2016/2023 {
	foreach h of numlist 5/20 {	
		preserve
		keep est_`h' year se_`h' se_`h'_l se_`h'_h
		rename est_`h' est
		rename se_`h' se
		rename se_`h'_l se_l
		rename se_`h'_h se_h
		keep if year== `year'
		gen bandwidth= `h'
		duplicates drop
		tempfile bandwidth_`year'_`h'
		save `bandwidth_`year'_`h''
		restore
	}
}


preserve
keep year
keep if year==0
tempfile bandwidths
save `bandwidths'
restore


use `bandwidths', clear
forvalues year = 2016/2023 {
	foreach h of numlist 5/20 {	
		append using `bandwidth_`year'_`h''
	}
}

save `bandwidths', replace


forvalues year = 2016/2023 {
twoway (rcap se_l se_h bandwidth if year==`year', lcolor(black)) (scatter est bandwidth if year==`year', msymbol(O) msize(small) mcolor(black)), ///
       xtitle("Bandwidths") ytitle("") ///
       title("`year'", size(medium) margin(0 0 5 0)) ///
       graphregion(color(white)) plotregion(margin(5 5 5 5)) ///
	   yline(0, lpattern(dash)) ///
	   legend(off) ///
	   ysize(5) xsize(6) ///
       xlabel(, nogrid) ylabel(-60(30)60, nogrid) 
	   graph save "Graph" "6graphs/level/06_`year'_bandwidth.gph", replace
}
 
 graph combine "6graphs/level/06_2018_bandwidth.gph" ///
"6graphs/level/06_2019_bandwidth.gph" ///
"6graphs/level/06_2020_bandwidth.gph" ///
"6graphs/level/06_2021_bandwidth.gph" ///
"6graphs/level/06_2022_bandwidth.gph" ///
"6graphs/level/06_2023_bandwidth.gph", col(3) ///
imargin(small) graphregion(color(white) lcolor(white) lwidth(thick)) ysize(6.5) xsize(9)

graph export "6graphs/level/06__combined_bandwidth.eps", as(eps) name("Graph") preview(off) fontface("Georgia")replace
        
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   