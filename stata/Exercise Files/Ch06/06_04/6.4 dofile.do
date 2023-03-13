*Load data
clear
use count

*generate truncated doctor visit count
gen tdocvis = docvis if docvis > 0
*generate censored doctor visit count
gen cdocvis = docvis
replace cdocvis = 20 if docvis > 20

*tabulate truncated and censored variables
tab tdocvis
tab cdocvis

*Truncated poisson regresion
tpoisson tdocvis age educyr income female, ll(0) 

*Truncated negative binomial regression
tnbreg tdocvis age educyr income female, ll(0) 

*Censored poisson regresion
cpoisson cdocvis age educyr income female, ul(20)

