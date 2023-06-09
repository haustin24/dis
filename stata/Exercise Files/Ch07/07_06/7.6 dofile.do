*Clear
clear

*Load Heart Transplant Data
use heart

*Set survival data
stset stime, id(id) failure(died==1)

*Cox proportional hazards regression with covariates
stcox i.transplant age

*Plot a log-log plot adjusted for continuous covariate age
stphplot, by(transplant) 

*Plot a comparison of Kaplan-Meier vs Cox regression predicted survival functions
stcoxkm, by(transplant)

*Test of proportional-hazards assumption with detail
stcox i.transplant age
estat phtest, detail
