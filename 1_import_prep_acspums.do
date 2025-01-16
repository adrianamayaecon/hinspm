global hinsaq "$dropbox/HINS_AQ"
global data "$hinsaq/data/acs-pums"

//HINS AQ
//Adrian Amaya
//August 10, 2023

//IMPORT & PREP ACS-PUMS DATA
*** ACS PUMS data comes in single-year files. Each year has individual-level files (a, b) and household-level files (a, b). 
* Using data from 2012-2019 keeps PUMA geographies consistent and avoids using COVID years

* Migpuma to puma crosswalk
import excel "$data/puma_migpuma1_pwpuma00.xls", sheet("PUMA_POWPUMA_MIGPUMA") cellrange(A3:G2392) firstrow clear
drop if PUMA == ""
gen geoid = StateofResidenceST + PUMA
gen crosswalk_geoid = PlaceofWorkStatePWSTATE2o + PWPUMA00orMIGPUMA1
rename PlaceofWorkStatePWSTATE2o crosswalk_st
keep geoid crosswalk_geoid crosswalk_st
destring geoid crosswalk_geoid crosswalk_st, replace
save "$data/puma_migpuma_crosswalk", replace

* Import individual and household data for each batch while creating geoid and year variables. 
foreach yr of numlist 12/19{
	foreach file in hus pus{
		foreach i in a b{
		    cd "$data/singleyear/csv_`file'"
			import delimited "ss`yr'`file'`i'", clear
			tostring serialno, replace format(%13.0f)
			tostring puma, replace format(%05.0f)
			tostring st, replace format(%02.0f)
			gen year=20`yr'
			gen geoid=st+puma
			destring year geoid puma st, replace
			cd "$data/singleyear/dta_`file'"
			if "`i'" == "a"{
				save "acspums_`file'_`yr'", replace
			}
			else{
				append using "acspums_`file'_`yr'"
				save "acspums_`file'_`yr'", replace
			}			
		}
	}
}
