clear
sysuse auto

foreach x of varlist * {
	forvalues i = 10(10)100 {
		gen `x'`i' = `x' * `i'
		}
	}
	
