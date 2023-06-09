*Clear
clear

*Load Heart Transplant Data
use heart

*Set survival data
stset stime, id(id) failure(died==1)

*Cox proportional hazards regression with no covariates
stcox, estimate

*Plot estimated survival function
stcurve, survival 
*Compare to Kaplan-Meier survival function
sts graph

*Cox proportional hazards regression with covariates
stcox i.transplant age

*Plot survivor, hazard, cumulative hazard function
stcurve, survival at1(transplant=0) at2(transplant=1)
stcurve, hazard at1(transplant=0) at2(transplant=1)
stcurve, cumhaz at1(transplant=0) at2(transplant=1)
stcurve, survival at1(transplant=1 age=18) at2(transplant=1 age=65)  

