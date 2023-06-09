*Clear data
clear

*Load NLSWORK panel data
webuse nlswork.dta

*Set panel data
xtset idcode year

*Pooled logit model 
logit union age grade south, cluster(idcode)
*Pooled logit model, odds ratios
logit union age grade south, cluster(idcode) or
*Marginal effects
margins, dydx(*)

*Fixed effects logit model
xtlogit union age grade south, fe
*Fixed effects logit model, odd ratios
xtlogit union age grade south, fe or

*Random effects logit model
xtlogit union age grade south, re
*Random effects logit model, odd ratios
xtlogit union age grade south, re or
*Marginal effects
margins, dydx(*)

