*Clear data
clear

*Load nlsw data
sysuse nlsw

*Estimate a regression model of wages with grade, union membership, marital status and experience as covariates. Grade, union and married are fully factorally interacted.
regress wage c.grade##i.union##i.married ttl_exp

*Estimate margins for union and married interaction at different grade points
margins union#married, at(grade=(0(1)20)) 

*Plot results by married
marginsplot, by(married)

*Estimate a regression model of wages with grade, union membership, marital status and experience as covariates. Grade, union and married are fully factorally interacted but grade has a cubic polynomial.
regress wage c.grade##c.grade##c.grade##i.union##i.married ttl_exp

*Estimate margins for union and married interaction at different grade points
margins union#married, at(grade=(0(1)20)) 

*Plot results by married with no confidence intervals
marginsplot, by(married) noci
