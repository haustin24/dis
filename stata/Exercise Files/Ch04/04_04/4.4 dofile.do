*Clear data
clear

*Load NLSWORK panel data
webuse nlswork.dta

*Set panel data
xtset idcode year

*Describe pattern of xt data
xtdescribe  

*Summarize xt data
xtsum hours 

*Tabulate xt data
xttab union
