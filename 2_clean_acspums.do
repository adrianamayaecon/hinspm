global hinsaq "$dropbox/JMP/HINS_AQ"
global data "$hinsaq/data/acs-pums"

//HINS AQ
//Adrian Amaya
//August 10, 2023

//Clean ACS-PUMS data, merge with HH data, merge with KFF, merge with PUMA-level controls
 
foreach yr of numlist 12/19{
	cd "$data/singleyear/dta_pus"
	use "acspums_pus_`yr'", clear
	label define yesno 1 "YES" 0 "NO"
	
	//Health Insurance
	foreach var in hicov pubcov privcov{
		recode `var' 2 = 0
		label values `var' yesno
	}
	foreach n of numlist 1/7{
		recode hins`n' 2 = 0
		label values hins`n' yesno
		if `n' == 2{
			local others hins1 hins3 hins4 hins5 hins6 hins7
			rename hins2 direct_acs
			egen otherins = anymatch(`others'), values(1)
			gen hins2 = 1 if direct_acs == 1 & otherins == 0
			replace hins2 = 0 if missing(hins2)
			sum hins2 direct_acs
		}
	}
	egen uninsured = anymatch(hicov), values(0)

	label variable hins1 "ESI"
	label variable hins2 "Direct only"
	label variable hins3 "Medicare"
	label variable hins4 "Medicaid"
	label variable hins5 "Tricare"
	label variable hins6 "VA"
	label variable hins7 "IHS"
	label variable hicov "Any Health Coverage"
	label variable privcov "Private Coverage"
	label variable pubcov "Public Coverage"
	* privcov = esi, direct, tricare
	* pubcov = Medicare, Medicaid, VA
	rename (hins1 hins2 hins5) (esi direct tricare)
	rename (hins3 hins4 hins6) (medicare medicaid va)
	rename hins7 ihs
	
	//Population and flows 
	gen population = 1
	bysort geoid: egen obs = total(population)
	
	//Age
	label variable agep "Age"
	foreach n of numlist 0/9{
		local lower = 10*`n'
		local upper = `lower' + 9
		gen age`n' = inrange(agep, `lower', `upper')
	}
	gen senior = (agep>64)
	gen adult = inrange(agep, 25, 64)
	gen youth = inrange(agep, 16, 24)
	gen children = inrange(agep, 0, 15)
	
	//Educational attainment
	recode schl (1/24 = 0) (. = 1), gen(misseduc)
	label variable misseduc "Missing education ($<$3yo)"
	label values misseduc yesno
	recode schl (. 16/24 = 0) (1/15 = 1), gen(nohs)
	label variable nohs "No HS degree"
	label values nohs yesno
	recode schl (. 1/15 20/24 = 0) (16/19 = 1), gen(hs)
	label variable hs "HS degree"
	label values hs yesno
	recode schl (. 1/19 = 0) (20/24 = 1), gen(college)
	label variable college "College degree"
	label values college yesno
	
	//Gender
	recode sex (1 = 0) (2 = 1), gen(female)
	label variable female "Female"
	label values female yesno
	recode sex (2 = 0), gen(male)
	label variable male "Female"
	label values male yesno
	
	//Marriage
	gen married = inlist(mar, 1)
	label variable married "Married"
	label values married yesno

	//Employment
	recode esr (. 1 2 4/6 = 0) (3 = 1), gen(unemployed)
	label variable unemployed "Unemployed"
	label values unemployed yesno
	recode esr (1/5 = 0) (. 6 = 1), gen(notinlf)
	label variable notinlf "Not in labor force"
	label values notinlf yesno
	recode esr (. 4/6 = 0) (1/3 = 1), gen(lf)
	label variable lf "Labor Force"
	label values lf yesno
	
	//POW-PUMA
	tostring powpuma, replace format(%05.0f)
	tostring powsp, replace format(%03.0f)
	gen powpuma_geoid=powsp+powpuma
	label variable powpuma_geoid "Place of work PUMA"
	
	//MIG-PUMA
	tostring migpuma, replace format(%05.0f)
	tostring migsp, replace format(%03.0f)
	gen migpuma_geoid=migsp+migpuma
	replace migpuma_geoid = "" if migpuma_geoid == ".."
	destring migpuma_geoid, replace
	destring migsp, replace
	label variable migpuma_geoid "Migration PUMA"
	
	//Family income
	label variable povpip "Perc. above FPL"
	destring povpip, replace
	foreach n of numlist 0/5{
		local lower = 100*`n'
		local upper = `lower' + 99
		gen pov`n' = inrange(povpip, `lower', `upper')
	}
	
	//Race
	recode hisp (1 = 0) (2/24 = 1)
	label variable hisp "Hispanic"
	label values hisp yesno
	egen racnatind = anymatch(racaian racnh racpi), values(1)
	label variable racnatind "Native/Indigenous"
	label values racnatind yesno
	label variable racasn "Asian"
	label values racasn yesno
	label variable racblk "Non-hispanic Black"
	replace racblk = 0 if hisp == 1
	label values racblk yesno
	label variable racsor "Other race"
	label values racsor yesno
	label variable racwht "Non-hispanic White"
	replace racwht = 0 if hisp == 1
	label values racwht yesno
	recode racnum (1 = 0) (2/6 = 1)
	label variable racnum "More than one race"
	
	label variable racaian "Native American/Alaskan"
	label values racaian yesno
	label variable racnh "Native Hawaiian"
	label values racnh yesno
	label variable racpi "Pacific Islander"
	label values racpi yesno
	
	//Foreign born
	gen foreignborn = inlist(nativity, 2)
	label variable foreignborn "Foreign Born"
	
	//Citizen
	recode cit (5 = 0) (1/4 = 1), gen(citizen)
	label variable citizen "Citizen"
	
	//Student
	recode sch (. 1 = 0) (2 3 = 1), gen(student)
	label variable student "Student"
	
	//Foster kids
	if `yr' == 19 recode relshipp (20/34 36/38 = 0) (35 = 1), gen(fosterchild)
	else recode relp (0/13 15/17 = 0) (14 = 1), gen(fosterchild)
	
	*****************************************************************
	//HH data
	cd "$data/singleyear/dta_hus"
	merge m:1 serialno using "acspums_hus_`yr'", nogen keep(3)
	
	//Number of own children in HH
	destring noc, replace
	label variable noc "Number of own children"
	foreach n of numlist 1/4{
		gen noc`n' = inlist(noc, `n')
	}
	gen noc5 = inrange(noc, 5, .)

	
	//Parents
	if `yr' == 19 {
		recode relshipp (20 21 23 = 1) (22 24 25/38 = 0), gen(hoh) //Heads of households and spouses
	} 
	else{
		recode relp (0 1 = 1) (2/17 = 0), gen(hoh) //Heads of households and spouses
	}
	gen parent1 = (hoh==1 & noc>0)
	gen parent2 = inrange(paoc, 1, 3)
	gen parent3 = inrange(sfr, 2, 3)
	egen parent = anymatch(parent1 parent2 parent3), values(1)
		
	//Single childless adults
	gen childlessadult = (agep>18 & parent==0 & married==0)
	
	//MAGI
	gen is_subfamily = !missing(sfr)
	gen is_subfamily1 = (sfn==1)
	gen is_subfamily2 = (sfn==2)
	gen is_subfamily3 = (sfn==3)
	gen is_subfamily4 = (sfn==4)
	if `yr' == 19 {
		gen is_family = (hoh==1 | (inrange(relshipp, 25, 27) & agep<19)) //parents and young children
		replace is_family = 0 if is_subfamily == 1
		recode relshipp (22 24 34/38 = 0) (20/21 23 25/33 = 1), gen(is_related) //related
	} 
	else {
		gen is_family = (hoh==1 | (inrange(relp, 2, 4) & agep<19)) //parents and own children
		replace is_family = 0 if is_subfamily == 1
		recode relp (0/10 = 1) (11/17 = 0), gen(is_related) //related
	}
	gen is_dependent = inlist(dout, ., 1) //independent living difficulty
	replace is_dependent = 1 if (agep<19 & married==0) | (agep<24 & married==0 & student==1)
	replace is_family = 1 if (is_family==0 & is_subfamily==0 & is_dependent==1 & is_related==1) //dependent related
	gen is_single = 1 if (is_family==0 & is_subfamily==0)
	
	bysort serialno: egen num_family = total(is_family)
	bysort serialno: egen num_subfamily1 = total(is_subfamily1)
	bysort serialno: egen num_subfamily2 = total(is_subfamily2)
	bysort serialno: egen num_subfamily3 = total(is_subfamily3)
	bysort serialno: egen num_subfamily4 = total(is_subfamily4)
	bysort serialno: egen num_single = total(is_single)
	egen totalhhcount = rowtotal(num_family num_subfamily* num_single)
	assert totalhhcount == np
	bysort serialno is_single: gen singlenumber = _n if is_single==1

	gen household_size = num_family if is_family==1
	gen category = "family" if is_family==1
	foreach i of numlist 1/4{
		replace household_size = num_subfamily1 if is_subfamily1==1
		replace category = "subfamily`i'" if is_subfamily`i'==1
	}
	sum singlenumber
	foreach i of numlist 1/`r(max)'{
		replace category = "single`i'" if singlenumber == `i'
	}
	replace household_size = 1 if is_single==1
	replace household_size = 2 if is_single==1 & married==1
	replace category = "marriedcouple" if is_single==1 & married==1
	
	egen family = group(serialno household_size category), missing
	
	destring pincp pap ssip oip, replace
	bysort family: egen grossincome = total(pincp)
	by family: egen publicassistance = total(pap)
	by family: egen suppsecurity = total(ssip)
	replace oip = 0 if oip == pincp
	by family: egen other = total(oip)
	gen hhmagi = grossincome - publicassistance - suppsecurity - other
	replace hhmagi = 0 if hhmagi<0
	merge m:1 household_size year using "$data/poverty_guidelines_us49", nogen keep(3)
	gen magipovpip = (hhmagi/poverty_threshold) * 100
	replace magipovpip = 501 if magipovpip>501
	
	foreach n of numlist 0/5{
		local lower = 100*`n'
		local upper = `lower' + 99
		gen magipov`n' = inrange(magipovpip, `lower', `upper')
	}
	
	//Property value
	label variable valp "Property value"
	destring valp, replace
	
	//Tenure
	recode ten 2/4 = 0, gen(mortgagor)
	label variable mortgagor "Mortgagors"
	recode ten (1 3/4 = 0) (2 = 1), gen(owner)
	label variable owner "Owners"
	recode ten (1/2 4 = 0) (3 = 1), gen(renter)
	label variable renter "Renters"
	recode ten (1/3 = 0) (4 = 1), gen(occupied)
	label variable occupied "Occupied"
	
	//Fire Insurance
	destring insp, replace
	replace insp=0 if insp==.
	label variable insp "Fire/hazard/flood insurance"
		
	//Migration
	recode mv 2/7 = 0, gen(mv1)
	label variable mv1 "Moved 0-12 months ago"
	label values mv1 yesno
	recode mv (1 3/7 = 0) (2 = 1), gen(mv2)
	label variable mv2 "Moved 13-23 months ago"
	label values mv2 yesno
	recode mv (1/2 4/7 = 0) (3 = 1), gen(mv4)
	label variable mv4 "Moved 2-4 years ago"
	label values mv4 yesno
	recode mv (1/3 5/7 = 0) (4 = 1), gen(mv9)
	label variable mv9 "Moved 5-9 years ago"
	label values mv9 yesno
	recode mv (1/4 = 0) (5/7 = 1), gen(mv10)
	label variable mv10 "Moved 10+ years ago"
	label values mv10 yesno
	
	//Flows
	gen inflow = inlist(mv1, 1)			
	
	//KFF data
	cd "$hinsaq/data/kff"
	* Expansion year & mandate
	merge m:1 st using "kff_expansion_states", keep(1 3) keepusing(expansionyr) nogen
	gen expansion = (year >= expansionyr)
	label variable expansion "1 if medicaid expansion in that year"
	gen expanded = (expansionyr==2011)
	replace expanded = 2 if expansionyr==2014
	replace expanded = 3 if expansionyr==2015
	replace expanded = 4 if expansionyr==2016
	replace expanded = 5 if expansionyr>2016
	label variable expanded "0 never, 1 early, 2 2014, 3 2015, 4 2016, 5 later"
	gen mandate = inrange(year, 2014, 2018)
	* Eligibility
	egen famunitacs = group(serialno povpip) 
	bysort famunitacs: egen publicassistanceacs = total(pap)
	local acsedits acseditselig_other
	gen `acsedits' = 0
	replace `acsedits' = 1 if fosterchild==1 
	replace `acsedits' = 1 if ssip>0 & ssip<. & parent == 0
	replace `acsedits' = 1 if ssip>0 & ssip<. & parent==1 & (unemployed==1|notinlf==1|dis==1)
	replace `acsedits' = 1 if citizen==1 & parent==1 & pap>0 & pap<.
	replace `acsedits' = 1 if citizen==1 & married==1 & publicassistanceacs>0 & publicassistanceacs<.
	replace `acsedits' = 1 if agep<19 & married==0 & publicassistanceacs>0 & publicassistanceacs<.
	
	
	foreach method in acs magi{
		if "`method'" == "acs"{
			local incfpl povpip
		} 
		if "`method'" == "magi"{
			local incfpl magipovpip
		}
		foreach group in adults parents pregnant children ssi{
			local limit lim_elig_`group'
			if "`group'" == "adults"{
				merge m:1 st year using "kff_medicaid_eligibility_`group'", keep(1 3) nogen
				gen `method'elig_`group'=(`incfpl'<= `limit')
				gen `method'justinelig_`group'= inrange(`incfpl',`limit'+1,`limit'+100)
			}
			if "`group'" == "parents"{
				merge m:1 st year using "kff_medicaid_eligibility_`group'", keep(1 3) nogen
				gen `method'elig_`group'=((`incfpl'<= `limit') & parent==1)
				gen `method'justinelig_`group'= (inrange(`incfpl',`limit'+1,`limit'+100) & parent==1)
			}
			if "`group'" == "pregnant"{
				merge m:1 st year using "kff_medicaid_eligibility_`group'", keep(1 3) nogen
				gen `method'elig_`group'=((`incfpl'<= `limit') & fer==1)
				gen `method'justinelig_`group'= (inrange(`incfpl',`limit'+1,`limit'+100) & fer==1)
			}
			if "`group'" == "children"{
				merge m:1 st year using "kff_medicaid_eligibility_`group'", keep(1 3) nogen
				gen `method'elig_`group'=((`incfpl'<= `limit') & inrange(agep, 0, 18))
				gen `method'justinelig_`group'= (inrange(`incfpl',`limit'+1,`limit'+100) & inrange(agep, 0, 18))
			}
			if "`group'" == "ssi"{
				destring ssip, replace
				gen `method'elig_`group'=(ssip>0 & ssip<.)
			}
					   
		}
		egen `method'medicaid_eligible = anymatch(`method'elig_adults `method'elig_parents `method'elig_pregnant `method'elig_children `method'elig_ssi acseditselig_other), values(1)
		egen `method'public_eligible = anymatch(`method'medicaid_eligible senior), values(1)
		egen `method'just_ineligible = anymatch(`method'justinelig_adults `method'justinelig_parents `method'justinelig_pregnant `method'justinelig_children), values(1)
		replace `method'just_ineligible = 0 if acseditselig_other == 1
			
		}
	save "$data/acspums_`yr'_clean", replace
}


