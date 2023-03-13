*Clear data
clear

*Load nlsw data
sysuse nlsw

*Estimate a regression model of wages with grade, experience and union membership and ethnicity fully interacted
regress wage grade ttl_exp i.union##i.country

*computing predicted wages of race and union
margins country union

*computing predicted wages of only interaction effects
margins country#union

*computing predicted wages of all effects for race and union
margins country##union

*computing predicted wages of only interaction effects
margins country#union

*plot predicted wages
marginsplot

*Splitting marginsplot by union
marginsplot, by(union)
