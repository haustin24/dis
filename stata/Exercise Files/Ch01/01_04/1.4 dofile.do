clear

forvalues i = 1/20 {
	gen x`i' = rnormal()
	}
	
forvalues i = 10(10)100 {
	di `i'
	}
	
forvalues i = 100(-10)10 {
	di `i'
	}	
	
forvalues i = 0.2 0.4 : 2 {
	di `i'
	}
		
