*Clear data
clear

*Load Auto Training data
sysuse auto

*Loop all variables over a generating code
foreach var of varlist * {
	gen `var'10 = `var' * 10
}
