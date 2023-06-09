*Clear data
clear

*Load NLSWORK panel data
webuse nlswork.dta

*Set panel data
xtset idcode year

*Pooled OLS
reg ln_wage age collgrad, cluster(idcode)  

*Pooled OLS with time as a variable
reg ln_wage age collgrad i.year, cluster(idcode)  

*Fixed-effects (FE) model
xtreg ln_wage age collgrad i.year, fe

*Random-Effects (RE) 
xtreg ln_wage age collgrad i.year, re
