*Load data
clear
use count

*Estimate poisson model 
nbreg docvis age educyr income female

*Compute (average) marginal effects
margins, dydx(*)

*Predict counts
predict count

*Correlate predicted to actual 
correlate count docvis

*Square correltion
di 0.0875^2
