*Clear data
clear

*Load NLSWORK panel data
webuse nlswork.dta

*Set panel data
xtset idcode year

*Panel tabulate the variable south
xttab south

*Generate lead of south
gen f1south = f1.south

*Tabulate transition to future south
tab south f1south, row

*Pooled OLS of hours, south, and lag and lead of south
reg ln_wage hours l(-1/1).south, cluster(idcode)

*RE & FE of hours, south,  and lag and lead of south
xtreg ln_wage hours l(-1/1).south, re
estimates store re
xtreg ln_wage hours l(-1/1).south, fe
estimates store fe

*Hausman test of FE vs RE
hausman fe re
