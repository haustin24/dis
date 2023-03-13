*Clear data
clear

*Load nlsw data
use nlsw

*Estimate a regression model of wages with quadratic grade polynomials, age and union membership as covariates
regress wage c.grade##c.grade age i.union

*Compute predicted wage at grades 0 to 20 using margins.
margins, at(grade=(0(1)20))

*Plot predicted grades using marginsplot
marginsplot

*Compute and plot predicted wage at grades 0 to 20 for union and non-union members.
margins union, at(grade=(0(1)20))
marginsplot

*Estimate a regression model of wages with cubic grade polynomials, age and union membership as covariates
regress wage c.grade##c.grade##c.grade age i.union

*Compute predicted wage at grade 0 to 20 and plot results
margins, at(grade=(0(1)20))
marginsplot

