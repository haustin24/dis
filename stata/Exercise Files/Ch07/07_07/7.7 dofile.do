*Clear
clear

*Load Heart Transplant Data
use heart

*Set survival data
stset stime, id(id) failure(died==1)

*Parametric survival regression with exponential distributed baseline hazard
streg i.transplant age, distribution(exponential)

*Plot survivor, hazard and cumulative hazard function
stcurve, survival at1(transplant=0) at2(transplant=1)
stcurve, hazard at1(transplant=0) at2(transplant=1)
stcurve, cumhaz at1(transplant=0) at2(transplant=1)

*Display information criteria
estat ic

*Testing different parametric survival models
streg i.transplant age, distribution(weibull)
estat ic

