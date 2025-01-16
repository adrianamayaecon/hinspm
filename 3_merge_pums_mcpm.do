global jmp "$dropbox/JMP"
global hinsaq "$jmp/HINS_AQ"
global data "$hinsaq/data"
cd "$data"
//HINS AQ
//Adrian Amaya
//Oct 8, 2022


global demographics rac2p racwht racasn racblk hisp racnatind racsor racnum female age* noc* married foreignborn citizen childlessadult parent hoh senior adult youth children
global economics misseduc college hs nohs magi* pov* unemployed notinlf wkhp lf student
global other mv* mig expanded expansion* *elig* type mil obs uninsured population acr ags valp insp pincp rntp mrgp bld sporder
global regressors hicov pubcov privcov medicaid medicare esi direct direct_acs va ihs tricare geoid st state year pwgtp* serialno division

local allvars $demographics $economics $other $regressors 



foreach yr of numlist 12/19{
	*Non-institutional civilian population in housing units 27 and older
	use `allvars' using "acs-pums/acspums_`yr'_clean" if agep >= 27 & agep<65 & inlist(type,1) & inlist(mil,.,2,3,4,5), clear 
	destring mrgp rntp bld sporder wkhp, replace
qui merge m:1 geoid year using "$data/mcpm/mcpm_consecutive_days_puma_year", nogen keep(3)
qui merge m:1 geoid year using "$data/mcpm/mcpm_daysover_ptiles_puma_year", nogen keep(3)
qui merge m:1 geoid year using "$data/mcpm/mcpm_daysover_threshhold_puma_year", nogen keep(3)
qui merge m:1 geoid year using "$data/mcpm/mcpm_average_puma_year", nogen keep(3)

gen privcov_acs = privcov
drop privcov
egen privcov = anymatch(direct esi tricare), values(1)

gen esi_acs = esi
drop esi
egen esi = anymatch(esi tricare), values(1)



	if `yr' == 12 {
		bysort geoid: egen totalpopw = total(acsmedicaid_eligible * pwgtp)
		bysort geoid: egen totalins = total(medicaid * acsmedicaid_eligible * pwgtp)
		by geoid: gen medicaid_takeup_2012_acs = totalins/totalpopw
		
		drop totalpopw totalins
		
		bysort geoid: egen totalpopw = total(magimedicaid_eligible * pwgtp)
		bysort geoid: egen totalins = total(medicaid * magimedicaid_eligible * pwgtp)
		by geoid: gen medicaid_takeup_2012_magi = totalins/totalpopw
		
		save "reg_mcpm", replace
	} 
	else {
		append using "reg_mcpm"
		bysort geoid: fillmissing medicaid_takeup_2012_acs, with(min)
		bysort geoid: fillmissing medicaid_takeup_2012_magi, with(min)

		save "reg_mcpm", replace
	}
}