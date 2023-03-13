*Load data
clear
use count

*Hurdle model
*First part of two-part hurdle model; logit model predicting probability to see a doctor or not
logit docvis age educyr income female

*Compute marginal effects of logit model
margins, dydx(*)

*Second part of two-part hurdle model; truncated negative binomial model of doctor visit count (greater than 0)
tnbreg docvis age educyr income female if docvis > 0

*Compute marginal effects of truncated negative binomial model
margins, dydx(*)

*Zero-inflated negative binomial model
zinb docvis age educyr income female, inflate(age educyr income female)

*Compute marginal effects of truncated negative binomial model
margins, dydx(*)
margins, dydx(*) predict(pr)

