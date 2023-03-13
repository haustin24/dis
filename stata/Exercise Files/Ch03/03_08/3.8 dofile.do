*Clear
clear

*Load nlsw data
use nlsw

*Tabulate union membership
tab union

*Estimate logit model of union membership with wage, country, married and grade
logit union c.wage##c.wage##i.country##i.married grade 

*Summarize wage
su wage

*Compute predicted probabilities of interaction effects at different wage points
margins country#married, at(wage=(1(2)40))

*plot effects by country
marginsplot, by(country)
