*Clear data
clear

*Load nlsw data
use nlsw

*Estimate a regression model of wages with grade, experience, their interaction and union membership as epxlanatory variables
regress wage c.grade##c.ttl_exp i.union

*Compute predicted wage at grades 0 to 20 and experience 0 to 30 and plot predictions
margins, at(grade=(0(1)20) ttl_exp=(0(5)30))
marginsplot

*Compute predicted wage at grades 0 to 20 and experience 0 and 30 and plot predictions
margins, at(grade=(0(1)20) ttl_exp=(0 30))
marginsplot

*Estimate a regression model of wages with grade, experience, their standard and squared interaction and union membership as epxlanatory variables
regress wage c.grade##c.grade##c.ttl_exp i.union

*Compute predicted wage at grades 0 to 20 and experience 0 and 30 and plot predictions
margins, at(grade=(0(1)20) ttl_exp=(0 30))
marginsplot
