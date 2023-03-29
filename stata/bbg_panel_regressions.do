clear

use bbg_dataset_stata.dta
xtset ISIN Dates

*FEM panel regression: YIELD 

xtreg yld_ytm_mid holding_ratio res_mat, fe 

*allowing for the relationship between holding ratio and YLD to change across programmes
xtreg yld_ytm_mid c.holding_ratio#programme res_mat, fe


** apprently xtreg does not produce the correct R2 value, below retrieves this...
areg yld_ytm_mid c.holding_ratio#programme res_mat, absorb(ISIN)

*with non-linear holding_ratio term
xtreg yld_ytm_mid c.holding_ratio#programme c.holding_ratio#c.holding_ratio res_mat, fe


*Bid ask 
xtreg bid_ask_v1 holding_ratio res_mat, fe 

xtreg bid_ask_v3 holding_ratio res_mat, fe 

*bid ask spreads by programme 
xtreg bid_ask_v3 c.holding_ratio#programme c.holding_ratio##c.holding_ratio res_mat, fe 

xtreg bid_ask_v1 c.holding_ratio#programme res_mat, fe 

** potential to also do this by maturity sector ?



*** regression with control variables 

*merge control variable dta
merge m:m Dates using controls.dta, keep(match)

ds

vl create controls = (UK10Y UKGDP UKCPI FTSE_ALL_SHARE_30D_VOL SONIA_WEDGE UKBANK_RATE)

*tests for multicollinearity between independent variables
*VIF: 1 = non 1-5 = Some but not requiring attention
*Condition number > 30 problematic 
collin holding_ratio res_mat $controls
*No VIF that requires attention
*Condition number < 30 


*correlation matrix between independent variables 
cor holding_ratio res_mat $controls
*No highly correlated IV's that require attention

xtreg yld_ytm_mid holding_ratio res_mat, fe
xtreg yld_ytm_mid holding_ratio res_mat $controls, fe

xtreg yld_ytm_mid c.holding_ratio#programme res_mat $controls, fe

*YIELD REGRESSION USE:
xtreg yld_ytm_mid c.holding_ratio#programme c.holding_ratio#c.holding_ratio#programme res_mat $controls, fe



xtreg bid_ask_v3 holding_ratio res_mat, fe
xtreg bid_ask_v3 holding_ratio res_mat $controls, fe

xtreg bid_ask_v3 c.holding_ratio#programme res_mat $controls, fe

*BID-ASK REGRESSION USE 
xtreg bid_ask_v3 c.holding_ratio#programme c.holding_ratio##c.holding_ratio#programme res_mat $controls, fe 



