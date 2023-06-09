*Clear data
clear

*Load NLSWORK panel data
webuse nlswork.dta

*Set panel data
xtset idcode year

*Generate lag, lead, difference and seasonal difference
gen l1nev_mar = l1.nev_mar
gen f1nev_mar = f1.nev_mar
gen d1nev_mar = d1.nev_mar
gen s1nev_mar = s1.nev_mar

*List first individual and explore newly created variables
list idcode year age *nev_mar in 1/12

*Explore transition of never married
tab nev_mar f1nev_mar, row

*Correlated never married with future values
corr nev_mar f(1/5).nev_mar 

*Regress log wages against weeks unemployed last year
reg ln_wage l(-2/2).(wks_ue south)
 
