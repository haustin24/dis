*Load data
use count

*Describe data
describe

*Tabulate count variable
tab docvis

*Estimate poisson model 
poisson docvis age educyr income female

*Compute (average) marginal effects
margins, dydx(*)

*Examine goodness-of-fit statistics
estat gof

*Install overdispersion command
net install http://fmwww.bc.edu/RePEc/bocode/o/overdisp

*Conduct formal test of equidispersion of model
overdisp docvis age educyr income female

