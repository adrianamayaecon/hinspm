global jmp "$dropbox/JMP"
global hinsaq "$jmp/HINS_AQ"
global data "$hinsaq/data"
global pums "$data/acs-pums"
global figures "$hinsaq/figures"
global wfdeaths "$jmp/MortalityWildfire"
global mitigate "$jmp/Medicaid-Mitigation"

cd "$data"


*ssc install coefplot
*grc1leg2
*xlincom

graph set eps fontface Times


global demographics racwht racasn racblk hisp racnatind racsor racnum female age* noc* married foreignborn citizen childlessadult parent
global economics misseduc college nohs hs magi* *povpip unemployed notinlf lf student pincp
global other mv* mig expanded expansion* *_eligible *elig_* type mil mar obs hoh acr ags valp insp
global regressors hicov pubcov privcov medicaid* medicare esi direct* geoid year st pwgtp* serialno *wildfirepm* *days* *spell* division

local allvars $demographics $economics $other $regressors 
global controls racasn racblk hisp racnatind racsor female agep noc married foreignborn citizen college nohs povpip unemployed notinlf student expansion

*********************************************
*********************************************
*********************************************

local binned_lags = 0
local daysoverp = 0


local mortality_hetero = 0



local medicaid_mitigation = 0
local mortality_motivation = 0


*********************************************
*********************************************
*********************************************




if `daysoverp' == 1{
	use `allvars' using "reg_mcpm" if magimedicaid_eligible==1 & inrange(mv, 3, 7) & agep<65, clear
	foreach lvl in "geo" "nat"{
		if "`lvl'" == "geo" local plottitle "(b) PUMA Percentiles"
		if "`lvl'" == "nat" local plottitle "(a) National Percentiles"
		eststo clear
		local estreg
		foreach n of numlist 70 90 95 99{
			rename lag1_daysover_p`n'_`lvl' percentile_`lvl'_`n'
			eststo p`n': reghdfe medicaid percentile_`lvl'_`n' $controls [pw=pwgtp], absorb(year geoid) vce(cluster geoid)
			local estreg `estreg' p`n'
		}
		coefplot `estreg', keep(percentile*) vertical rename(^percentile_`lvl'_([0-9]+)$ = \1{superscript:th}, regex) xtitle("Days over n{superscript:th} percentile", size(large)) ytitle("Coefficient estimate", margin(t=3) size(large)) title(`plottitle') msymbol(S) mcolor(white) msize(small) levels(99 90) ciopts(lcolor(black black) lwidth(*1 *6)) yline(0, lcolor(gs7) lwidth(thin)) nokey graphregion(color(white) lcolor(black)) plotregion(color(white)) yscale(titlegap(3)) yscale(range(0 .)) ylab(0(.001).004, labsize(large) grid glcolor(gs15)) xlab(,labsize(large)) xscale(titlegap(3)) name(gp`lvl', replace)
		graph export "$figures/coefplot_daysoverp_`lvl'.eps", replace as(eps) 
		
	}
	
	graph combine gpnat gpgeo, graphregion(color(white)) xsize(11) ysize(5) scale(1.5)
	graph export "$figures/coefplot_daysoverp.eps", replace as(eps)

}


if `binned_lags' == 1{
	eststo clear
	use `allvars' using "reg_mcpm" if acsmedicaid_eligible==1 & inrange(mv, 3, 7) & agep<65, clear

	*egen pm = cut(lag1_wildfirepm), at(0, 0.2, 0.4, 0.6, 0.8, 100) icodes
	xtile pm = lag1_wildfirepm [pw=pwgtp], nq(5)
	_pctile lag1_wildfirepm [pw=pwgtp], p(20 40 60 80)
		
	eststo: reghdfe medicaid ib1.pm $controls [pw=pwgtp], absorb(year geoid) vce(cluster geoid) nosample
	coefplot, msymbol(S) mcolor(white) msize(small) levels(99 95) ciopts(lcolor(edkblue edkblue) lwidth(*1 *6)) yline(0, lcolor(black) lwidth(thin)) nokey drop(_cons $controls) label vertical graphregion(color(white) lcolor(black)) plotregion(color(white)) xtitle("Wildfire-PM bins (quintiles)", size(large)) ytitle("Coefficient estimate", margin(t=3) size(large)) yscale(titlegap(3)) yscale(range(0 .)) ylab(#5,labsize(large) grid glcolor(gs15)) xlab(,labsize(large)) xscale(titlegap(3)) ///
	coeflabels( ///
	2.pm = "2nd" ///
	3.pm = "3rd" ///
	4.pm = "4th" ///
	5.pm = "5th") name(gbin, replace) title("(b) Binned regression", box bexpand)
	graph export "$figures/nonlinearpm.eps", as(eps) replace

	local lags
	foreach xvar in lag3_ lag2_ lag1_ " " lead1_{
		label variable `xvar'wildfirepm_z "`xvar'"
		local lags `lags' `xvar'wildfirepm_z
	}
	display "`lags'"
	eststo: qui reghdfe medicaid `lags' $controls [pw=pwgtp], absorb(geoid year) vce(cluster geoid) nosample
	coefplot, msymbol(S) mcolor(white) msize(small) levels(99 90)  ciopts(lcolor(edkblue edkblue) lwidth(*1 *6)) yline(0, lcolor(black) lwidth(thin)) nokey drop(_cons $controls) label vertical graphregion(color(white) lcolor(black)) plotregion(color(white)) xtitle("Wildfire-PM lags", size(large)) ytitle("Coefficient estimate", size(large)) ylab(#5) ylab( ,grid glcolor(gs15) labsize(large)) yscale(titlegap(3)) xscale(titlegap(3)) xlab(,labsize(large)) ///
	coeflabels( ///
	lag3_wildfirepm_z = "t{&minus}3" ///
	lag2_wildfirepm_z = "t{&minus}2" ///
	lag1_wildfirepm_z = "t{&minus}1" /// 
	wildfirepm_z = "t" ///
	lead1_wildfirepm_z = "t+1") name(glag, replace) title("(a) Distributed-lag regression", box bexpand)
	graph export "$figures/pmlags.eps", as(eps) replace
	
	
	graph combine glag gbin, graphregion(color(white)) xsize(16) ysize(9) scale(1.3)
	graph export "$figures/binned_lags.eps", as(eps) replace
	graph export "$figures/binned_lags.jpg", as(jpg) replace

	eststo clear
}


if `mortality_hetero'==1{
	
	local cause "respcirc"

	use "$wfdeaths/data/reg-data-`cause'_annual", replace
	merge m:1 geoid using "$mitigate/data/lowinc_nohins_other_25_64", nogen
	merge m:1 geoid using "$mitigate/data/medicaid-enrollment", nogen
	merge m:1 geoid using "$mitigate/data/employment", nogen

	drop if wildfirepm==.
	bysort geoid (year): gen obs = _N
	tab obs
	drop if year>=2020

************************************

	merge m:1 st using "$hinsaq/data/kff/kff_expansion_states", keep(1 3) keepusing(expansionyr) nogen


	replace expansionyr=. if inrange(expansionyr, 2019,2022)
	replace expansionyr=2014 if expansionyr == 2011
	gen expansion = (year >= expansionyr)
	label variable expansion "1 if medicaid expansion in that year"
	gen expanded = !missing(expansionyr)
	gen post = (year>=2014) if !missing(year)

	*egen pm = cut(wildfirepm), at(0,0.2,0.4,0.6,0.8,999) icodes
	*egen pm = cut(wildfirepm), group(5)
	xtile pm = wildfirepm, nq(5)

	
************************************
************************************
	local varlist
	foreach var in college_z lowinc_z highinc_z noins_z married_z female_z black_z hispanic_z asian_z white_z native_z enrollment_rate_z unemployment_rate_z fulltime_rate_z{
		
		if "`var'" == "lowinc_z" local plottitle = "Low income"
		if "`var'" == "highinc_z" local plottitle = "High income"
		if "`var'" == "noins_z" local plottitle = "Uninsured"
		if "`var'" == "enrollment_rate_z" local plottitle = "Medicaid enroll."
		if "`var'" == "unemployment_rate_z" local plottitle = "Unemployment"
		if "`var'" == "fulltime_rate_z" local plottitle = "Full-time emp."
		
		if "`var'" == "married_z" local plottitle = "Married"
		if "`var'" == "female_z" local plottitle = "Female"
		if "`var'" == "black_z" local plottitle = "Black"
		if "`var'" == "hispanic_z" local plottitle = "Hispanic"
		if "`var'" == "asian_z" local plottitle = "Asian"
		if "`var'" == "white_z" local plottitle = "White"
		if "`var'" == "college_z" local plottitle = "College"
		

	histogram `var', freq name(`var'1, replace)

	eststo clear
	reghdfe a25_64_agerate i.pm##c.`var' [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid) coeflegend

	mat A = J(4,3,.)
	

	lincom _cons + _b[2.pm] + _b[2.pm#c.`var'], level(95)
	
	mat A[1, 1] = r(estimate)
	mat A[1, 2] = r(lb)
	mat A[1, 3] = r(ub)

	lincom _cons + _b[3.pm] + _b[3.pm#c.`var'], level(95)
	mat A[2, 1] = r(estimate)
	mat A[2, 2] = r(lb)
	mat A[2, 3] = r(ub)

	lincom _cons +_b[4.pm] + _b[4.pm#c.`var'], level(95)
	mat A[3, 1] = r(estimate)
	mat A[3, 2] = r(lb)
	mat A[3, 3] = r(ub)

	lincom _cons + _b[5.pm] + _b[5.pm#c.`var'], level(95)
	mat A[4, 1] = r(estimate)
	mat A[4, 2] = r(lb)
	mat A[4, 3] = r(ub)


	preserve
	clear

	svmat double A, names(col)
	gen pm = _n + 1

	twoway rarea c2 c3 pm, color(gs14) ||  rcap c2 c3 pm || scatter c1 pm , connect(l) lc(ebblue) msymbol(S) mcolor(gs6) msize(small) ||, graphregion(color(white) lcolor(black)) xtitle("") ytitle("") plotregion(color(white)) xlab(2 "2nd" 3 "3rd" 4 "4th" 5 "5th") ylab( ,grid glcolor(gs15)) yscale(titlegap(3)) legend(off) yline(0, lcolor(black)) title(`plottitle', box bexpand size(medsmall)) name(`var', replace)


	restore

	local varlist `varlist' `var'
	}


	global varlist enrollment_rate_z lowinc_z unemployment_rate_z noins_z highinc_z fulltime_rate_z 
	graph combine $varlist, ycommon graphregion(color(white)) ysize(15) xsize(19) iscale(0.8) l1("Additional deaths per 100,000 adults", size(medsmall)) b1("Wildfire-PM bins (quintiles)", size(medsmall))

	graph export "$figures/coefplot_mortality_heterogeneity.pdf", replace as(pdf)
	graph export "$figures/coefplot_mortality_heterogeneity.eps", replace as(eps)

	********************************************

	global varlist black_z hispanic_z asian_z white_z married_z
	graph combine $varlist, ycommon graphregion(color(white)) ysize(15) xsize(19) iscale(0.8) l1("Additional deaths per 100,000 adults", size(medsmall)) b1("Wildfire-PM bins (quintiles)", size(medsmall))

	graph export "$figures/coefplot_mortality_heterogeneity2.pdf", replace as(pdf)
	graph export "$figures/coefplot_mortality_heterogeneity2.eps", replace as(eps)



}


if `medicaid_mitigation' == 1{
	
	
	local cause "respcirc"

	use "$wfdeaths/data/reg-data-`cause'_annual", replace
	merge m:1 geoid using "$mitigate/data/lowinc_nohins_25_64", nogen
	merge m:1 geoid using "$mitigate/data/medicaid-enrollment", nogen

	drop if wildfirepm==.
	bysort geoid (year): gen obs = _N
	tab obs
	drop if year>=2020
	
	
	
	merge m:1 st using "$hinsaq/data/kff/kff_expansion_states", keep(1 3) keepusing(expansionyr) nogen

	gen expanded = (expansionyr==2014 & !missing(expansionyr))
	gen post = (year>=2014) 

	xtile pm = wildfirepm, nq(5)
	gen pmtext = "1st" if pm==1
	replace pmtext = "2nd" if pm==2
	replace pmtext = "3rd" if pm==3
	replace pmtext = "4th" if pm==4
	replace pmtext = "5th" if pm==5
	
preserve

	keep if expansionyr==2014 | missing(expansionyr) | expansionyr>=2020

	keep geoid year wildfirepm daysover_0 max_spell expanded post
	duplicates drop
	collapse (mean) wildfirepm daysover_0 max_spell, by(year expanded post)
	
twoway lfit wildfirepm year if expanded == 1, lcolor(ebblue) || /// 
    scatter wildfirepm year if expanded == 1, msymbol(O) mcolor(ebblue) || /// 
	lfit wildfirepm year if expanded == 0, lcolor(orange) || /// 
    scatter wildfirepm year if expanded == 0, msymbol(T) mcolor(orange) ///  
    legend(order(2 "Expansion states" 4 "Non-expansion states") size(large)) ///
    graphregion(color(white) lcolor(black)) plotregion(color(white)) ///
	xline(2014, lpattern(dashed) lcolor(gs10)) ///
	title("(a) Average wildfire-PM", box bexpand size(medsmall)) ///
	name(g0a, replace)
	
twoway lfit daysover_0 year if expanded==1, lcolor(ebblue) || /// 
    scatter daysover_0 year if expanded == 1, msymbol(O) mcolor(ebblue) || ///
	lfit daysover_0 year if expanded==0, lcolor(orange) || /// 
    scatter daysover_0 year if expanded == 0, msymbol(T) mcolor(orange) ///  
    legend(order(2 "Expansion states" 4 "Non-expansion states") size(large)) ///
    graphregion(color(white) lcolor(black)) plotregion(color(white)) ///
	xline(2014, lpattern(dashed) lcolor(gs10)) ///
	title("(b) Wildfire-PM days", box bexpand size(medsmall)) ///
	name(g0b, replace)
		
twoway lfit max_spell year if expanded==1, lcolor(ebblue) || /// 
    scatter max_spell year if expanded == 1, msymbol(O) mcolor(ebblue) || /// 
	lfit max_spell year if expanded==0, lcolor(orange) || /// 
    scatter max_spell year if expanded == 0, msymbol(T) mcolor(orange) ///  
    legend(order(2 "Expansion states" 4 "Non-expansion states") size(large)) ///
    graphregion(color(white) lcolor(black)) plotregion(color(white)) ///
	xline(2014, lpattern(dashed) lcolor(gs10)) ///
	title("(c) Max. consec. wildfire-PM days", box bexpand size(medsmall)) ///
	name(g0c, replace)
	
	grc1leg2 g0a g0b g0c, graphregion(color(white) lcolor(black)) plotregion(color(white)) iscale(1)
	
	graph export "$figures/coefplot_wfpm_expansion.pdf", replace as(pdf)
	graph export "$figures/coefplot_wfpm_expansion.eps", replace as(eps)

	********************************************


	
restore

	eststo clear
	reghdfe a25_64_agerate ib1.pm##i.post##i.expanded if expansionyr==2014 | missing(expansionyr) | expansionyr>=2020 [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid) coeflegend
	
	mat A = J(10,3,.)
/*
	lincom _b[2.pm], level(95)
	mat A[1, 1] = r(estimate)
	mat A[1, 2] = r(lb)
	mat A[1, 3] = r(ub)
	
	lincom _b[3.pm], level(95)
	mat A[2, 1] = r(estimate)
	mat A[2, 2] = r(lb)
	mat A[2, 3] = r(ub)
	
	lincom _b[4.pm], level(95)
	mat A[3, 1] = r(estimate)
	mat A[3, 2] = r(lb)
	mat A[3, 3] = r(ub)
	
	lincom _b[5.pm], level(95)
	mat A[4, 1] = r(estimate)
	mat A[4, 2] = r(lb)
	mat A[4, 3] = r(ub)
***************************************************
	lincom _b[2.pm] + _b[2.pm#1.expanded], level(95)	
	mat A[5, 1] = r(estimate)
	mat A[5, 2] = r(lb)
	mat A[5, 3] = r(ub)
	
	lincom _b[3.pm] + _b[3.pm#1.expanded], level(95)	
	mat A[6, 1] = r(estimate)
	mat A[6, 2] = r(lb)
	mat A[6, 3] = r(ub)
	
	lincom _b[4.pm] + _b[4.pm#1.expanded], level(95)	
	mat A[7, 1] = r(estimate)
	mat A[7, 2] = r(lb)
	mat A[7, 3] = r(ub)
	
	lincom _b[5.pm] + _b[5.pm#1.expanded], level(95)	
	mat A[8, 1] = r(estimate)
	mat A[8, 2] = r(lb)
	mat A[8, 3] = r(ub)

***************************************************
	lincom _b[2.pm#1.post], level(95)	
	mat A[9, 1] = r(estimate)
	mat A[9, 2] = r(lb)
	mat A[9, 3] = r(ub)
	
	lincom _b[3.pm#1.post], level(95)	
	mat A[10, 1] = r(estimate)
	mat A[10, 2] = r(lb)
	mat A[10, 3] = r(ub)
	
	lincom _b[4.pm#1.post], level(95)	
	mat A[11, 1] = r(estimate)
	mat A[11, 2] = r(lb)
	mat A[11, 3] = r(ub)
	
	lincom _b[5.pm#1.post], level(95)	
	mat A[12, 1] = r(estimate)
	mat A[12, 2] = r(lb)
	mat A[12, 3] = r(ub)
	*/
	
***************************************************
	lincom _cons, level(95)	
	mat A[1, 1] = r(estimate)
	mat A[1, 2] = r(lb)
	mat A[1, 3] = r(ub)

	lincom _cons + _b[2.pm] + _b[2.pm#1.post], level(95)	
	mat A[2, 1] = r(estimate)
	mat A[2, 2] = r(lb)
	mat A[2, 3] = r(ub)
	
	lincom _cons + _b[3.pm] +_b[3.pm#1.post], level(95)	
	mat A[3, 1] = r(estimate)
	mat A[3, 2] = r(lb)
	mat A[3, 3] = r(ub)
	
	lincom _cons + _b[4.pm] +_b[4.pm#1.post], level(95)	
	mat A[4, 1] = r(estimate)
	mat A[4, 2] = r(lb)
	mat A[4, 3] = r(ub)
	
	lincom _cons + _b[5.pm] +_b[5.pm#1.post], level(95)	
	mat A[5, 1] = r(estimate)
	mat A[5, 2] = r(lb)
	mat A[5, 3] = r(ub)
***************************************************

	lincom _cons, level(95)	
	mat A[6, 1] = r(estimate)
	mat A[6, 2] = r(lb)
	mat A[6, 3] = r(ub)

	lincom _cons + _b[2.pm] + _b[2.pm#1.post] + _b[2.pm#1.expanded] + _b[2.pm#1.post#1.expanded], level(95)	
	mat A[7, 1] = r(estimate)
	mat A[7, 2] = r(lb)
	mat A[7, 3] = r(ub)
	
	lincom _cons + _b[3.pm] +_b[3.pm#1.post] + _b[3.pm#1.expanded] + _b[3.pm#1.post#1.expanded], level(95)	
	mat A[8, 1] = r(estimate)
	mat A[8, 2] = r(lb)
	mat A[8, 3] = r(ub)
	
	lincom _cons + _b[4.pm] +_b[4.pm#1.post] + _b[4.pm#1.expanded] + _b[4.pm#1.post#1.expanded], level(95)	
	mat A[9, 1] = r(estimate)
	mat A[9, 2] = r(lb)
	mat A[9, 3] = r(ub)
	
	lincom _cons + _b[5.pm] +_b[5.pm#1.post] + _b[5.pm#1.expanded] + _b[5.pm#1.post#1.expanded], level(95)	
	mat A[10, 1] = r(estimate)
	mat A[10, 2] = r(lb)
	mat A[10, 3] = r(ub)

	

	preserve
	clear

	svmat double A, names(col)
	gen id = _n
	gen time = 0 if inrange(id, 1, 8)
	replace time = 1 if inrange(id, 9, 16)
	
	label define tlab 0 "(a) Pre-expansion" 1 " (b) Post-expansion"
	label values time tlab

	*gen group = 0 if inrange(id, 1, 4) | inrange(id, 9, 12)
	*replace group = 1 if inrange(id, 5, 8) | inrange(id, 13, 16)
	
	gen group = 0 if inrange(id, 1, 5)
	replace group = 1 if inrange(id, 6, 10)
	
	bysort group (id): gen position=_n
	
	bysort group (id): gen pm = _n
	clonevar x = pm
	replace x = cond(group == 0, x - 0.1, x + 0.1)

/*	
twoway rcap c2 c3 x if group==0, lcolor(orange) || ///
    scatter c1 x if group==0, msymbol(T) mcolor(orange) msize(small) || ///
    rcap c2 c3 x if group==1, lcolor(ebblue) || ///
    scatter c1 x if group==1, msymbol(S) mcolor(ebblue) msize(small) ///
    by(time, graphregion(color(white) lcolor(black)) plotregion(color(white)) note("") iscale(1.1)) ///
	 subtitle(, box bexpand size(med) lcolor(black)) ///
    xtitle("Wildfire-PM bins (quintiles)", size(med)) ///
	ytitle("Additional deaths per 100,000 adults", size(med)) ///
	xlab(2 "2nd" 3 "3rd" 4 "4th" 5 "5th") ylab(, grid glcolor(gs15)) ///
    yscale(titlegap(3)) yline(0, lcolor(black)) ///
    name(g1, replace) ///
    legend(order(2 "Non-expansion states" 4 "Expansion states") ///
           col(2) size(med))
*/

twoway rcap c2 c3 x if group==0, lcolor(orange) || ///
    scatter c1 x if group==0, msymbol(T) mcolor(orange) msize(small) || ///
    rcap c2 c3 x if group==1, lcolor(ebblue) || ///
    scatter c1 x if group==1, msymbol(S) mcolor(ebblue) msize(small) ///
    graphregion(color(white) lcolor(black)) plotregion(color(white)) note("") ///
	subtitle(, box bexpand size(med) lcolor(black)) ///
    xtitle("Wildfire-PM bins (quintiles)", size(med)) ///
	ytitle("Annual deaths per 100,000 adults", size(med)) ///
	xlab(1 "1st" 2 "2nd" 3 "3rd" 4 "4th" 5 "5th") ylab(, grid glcolor(gs15)) ///
    yscale(titlegap(3)) yline(0, lcolor(black)) ///
    name(g1, replace) ///
    legend(order(2 "Non-expansion states" 4 "Expansion states") ///
           col(2) size(med))

	graph export "$figures/coefplot_mortality_mitigation.pdf", replace as(pdf)
	graph export "$figures/coefplot_mortality_mitigation.eps", replace as(eps)

	********************************************
	
	restore


	
}

if `mortality_motivation' == 1{
	
	
	local cause "respcirc"

	use "$wfdeaths/data/reg-data-`cause'_annual", replace
	merge m:1 geoid using "$mitigate/data/lowinc_nohins_25_64", nogen
	merge m:1 geoid using "$mitigate/data/medicaid-enrollment", nogen

	drop if wildfirepm==.
	bysort geoid (year): gen obs = _N
	tab obs
	drop if year>=2020
	
	
	merge m:1 st using "$hinsaq/data/kff/kff_expansion_states", keep(1 3) keepusing(expansionyr) nogen


	replace expansionyr=. if inrange(expansionyr, 2019,2022)
	replace expansionyr=2014 if expansionyr == 2011
	gen expansion = (year >= expansionyr)
	label variable expansion "1 if medicaid expansion in that year"
	gen expanded = !missing(expansionyr)
	replace expanded=0 if missing(expanded)
	gen post = (year>=2014) if !missing(year)
		
	xtile pm = wildfirepm, nq(5)
	gen pmtext = "1st" if pm==1
	replace pmtext = "2nd" if pm==2
	replace pmtext = "3rd" if pm==3
	replace pmtext = "4th" if pm==4
	replace pmtext = "5th" if pm==5

	eststo clear
	reghdfe a25_64_agerate ib1.pm [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid)
	eststo all: xlincom ///
	(1o = _cons) ///
	(1a = _cons +_b[2.pm]) ///
	(1b = _cons +_b[3.pm]) ///
	(1c = _cons +_b[4.pm]) ///
	(1d= _cons +_b[5.pm]), post
	
	reghdfe a25_64_agerate ib1.pm##c.noins_z [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid)  
	eststo lowins: xlincom ///
	(2o = _cons) ///
	(2a = _cons +_b[2.pm] + _b[2.pm#c.noins_z]) ///
	(2b = _cons +_b[3.pm] + _b[3.pm#c.noins_z]) ///
	(2c = _cons +_b[4.pm] + _b[4.pm#c.noins_z]) ///
	(2d= _cons +_b[5.pm] + _b[5.pm#c.noins_z]), post
	reghdfe a25_64_agerate ib1.pm##c.lowinc_z [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid)  
	eststo lowinc: xlincom ///
	(3o = _cons) ///
	(3a = _cons +_b[2.pm] + _b[2.pm#c.lowinc_z]) ///
	(3b = _cons +_b[3.pm] + _b[3.pm#c.lowinc_z]) ///
	(3c = _cons +_b[4.pm] + _b[4.pm#c.lowinc_z]) ///
	(3d = _cons +_b[5.pm] + _b[5.pm#c.lowinc_z]), post

	
	coefplot ///
	(all, msymbol(S) mfcolor(white) mcolor(white) msize(medium) ///
		levels(99 95) ciopts(lcolor(emidblue emidblue) lwidth(*1 *6)) ///
		graphregion(color(white) lcolor(black))) ///
	(lowinc, msymbol(T) mfcolor(white) mcolor(white) msize(medium) ///
		levels(99 95)  ciopts(lcolor(erose erose) lwidth(*1 *6)) ///
		graphregion(color(white) lcolor(black))) ///
	(lowins, msymbol(O) mfcolor(white) mcolor(white) msize(medium) ///
		levels(99 95)  ciopts(lcolor(eltgreen eltgreen) lwidth(*1 *6)) ///
		graphregion(color(white) lcolor(black))), ///
	drop(_cons) noomitted nobaselevels vertical ///
	b1title("Wildfire-PM bins (quintiles)", size(med)) ///
	ytitle("Annual deaths per 100,000 adults", margin(t=3) size(med)) ///
	plotregion(color(white)) legend(off) ///
	xlab(, labsize(medlarge)) ylab(#4 ,grid glcolor(gs15) labsize(large)) ///
	yscale(titlegap(3)) xscale(titlegap(3) alt axis(2)) ///
	yline(0, lcolor(black))  ///
		coeflabels( ///
		1o = "1st" ///
		2o = "1st" ///
		3o = "1st" ///
		1a = "2nd" ///
		2a = "2nd" ///
		3a = "2nd" ///
		1b = "3rd" ///
		2b = "3rd" ///
		3b = "3rd" ///
		1c = "4th" ///
		2c = "4th" ///
		3c = "4th" ///
		1d = "5th" ///
		2d = "5th" ///
		3d = "5th" ///
		) ///
		groups(1o 1d = "{bf: Baseline}" ///
				2o 2d = "{bf: +1 {&sigma} uninsured}" ///
				3o 3d = "{bf: +1 {&sigma} low-income}" ) name(g0a, replace)


	graph export "$figures/coefplot_mortality_hetero.pdf", replace as(pdf)
	graph export "$figures/coefplot_mortality_hetero.eps", replace as(eps)

			
******************************************************
		stop
******************************************************
		
	eststo clear
	reghdfe a25_64_agerate ib1.pm##c.noins_z [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid)  
	eststo lowins: xlincom ///
	(2 = _b[2.pm] + _b[2.pm#c.noins_z]) ///
	(3 = _b[3.pm] + _b[3.pm#c.noins_z]) ///
	(4 = _b[4.pm] + _b[4.pm#c.noins_z]) ///
	(5 = _b[5.pm] + _b[5.pm#c.noins_z]), post
		
		
	coefplot ///
	(lowins, msymbol(O) mfcolor(white) mcolor(white) msize(medium) levels(99 95)  ciopts(lcolor(emidblue emidblue) lwidth(*1 *6)) graphregion(color(white) lcolor(black))), ///
	drop(_cons) noomitted nobaselevels vertical ///
	title("Uninsured rates", box bexpand) ///
	xtitle("Wildfire-PM bins (quintiles)", size(large)) ///
	ytitle("Additional deaths per 100,000 adults", margin(t=3) size(large)) ///
	plotregion(color(white)) xlab(, labsize(large)) ylab(#4 ,grid glcolor(gs15) labsize(large)) yscale(titlegap(3)) yscale(range(0 .)) xscale(titlegap(3)) yline(0, lcolor(black))  ///
		coeflabels( ///
		2 = "2nd" ///
		3 = "3rd" ///
		4 = "4th" ///
		5 = "5th") name(g0b, replace)

		
******************************************************


	eststo clear
	reghdfe a25_64_agerate ib1.pm##c.lowinc_z [aw=totalpop_25_64], absorb(geoid st#year) vce(cluster geoid)  
	eststo lowinc: xlincom ///
	(2 = _b[2.pm] + _b[2.pm#c.lowinc_z]) ///
	(3 = _b[3.pm] + _b[3.pm#c.lowinc_z]) ///
	(4 = _b[4.pm] + _b[4.pm#c.lowinc_z]) ///
	(5 = _b[5.pm] + _b[5.pm#c.lowinc_z]), post
		
		
	coefplot ///
	(lowinc, msymbol(T) mfcolor(white) mcolor(white) msize(medium) levels(99 95)  ciopts(lcolor(emidblue emidblue) lwidth(*1 *6)) graphregion(color(white) lcolor(black))), ///
	drop(_cons) noomitted nobaselevels vertical ///
	title("Low income rates", box bexpand) ///
	xtitle("Wildfire-PM bins (quintiles)", size(large)) ///
	ytitle("Additional deaths per 100,000 adults", margin(t=3) size(large)) ///
	plotregion(color(white)) xlab(, labsize(large)) ylab(#4 ,grid glcolor(gs15) labsize(large)) yscale(titlegap(3)) yscale(range(0 .)) xscale(titlegap(3)) yline(0, lcolor(black))  ///
		coeflabels( ///
		2 = "2nd" ///
		3 = "3rd" ///
		4 = "4th" ///
		5 = "5th") name(g0c, replace)


		
	graph display g3, xsize(7) scale(1.1)
	graph export "$figures/coefplot_mortality_lowins.pdf", replace as(pdf)
	graph export "$figures/coefplot_mortality_lowins.eps", replace as(eps)



	
}
