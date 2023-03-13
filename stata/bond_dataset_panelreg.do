clear
use bond_dataset_v3.dta

describe

destring bid_cover, replace force

format operation_date %td

rename percent_of_freefloat holding_ratio
rename 1DCHNG_YIELD_BID_ASK bid_ask_1dchng

encode ISIN, generate(isin)

* keep [variable] keeps 
* drop [variable] drops 
*start with just QE1 to simplify 
keep if (prog == "QE1")

*set panel variables 
tset isin operation_date

*describes pattern of data
xtdescribe

xtreg YLD_YTM_MID holding_ratio proceeds_purchases, fe

xtreg YLD_YTM_MID holding_ratio proceeds_purchases res_mat, fe

xtreg YLD_BID_ASK holding_ratio proceeds_purchases res_mat, fe

xtreg YLD_BID_ASK holding_ratio proceeds_purchases res_mat

xtreg 1DCHNG_YIELD_BID_ASK holding_ratio proceeds_purchases res_mat
