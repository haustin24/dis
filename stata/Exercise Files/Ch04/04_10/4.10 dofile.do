*Clear data
clear

*Load NLSWORK panel data
webuse nlswork.dta

*Set panel data
xtset idcode year

*Fixed-effects (FE) model
xtreg ln_wage age collgrad union, fe
estimates store fe

*Random-Effects (RE) 
xtreg ln_wage age collgrad union, re
estimates store re

*Hausman test
hausman fe re
