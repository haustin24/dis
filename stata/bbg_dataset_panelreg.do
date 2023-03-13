clear
use bbg_dataset.dta

*format dates for viewing
format Dates %td
format maturity %td

rename percent_of_freefloat holding_ratio

*reformat ISIN
encode ISIN, generate(isin)

*set panel variables 
tset isin Dates

describe

*preserve
*start with QE1
keep if (prog == "QE1")


*describes pattern of data
xtdescribe

*panel regression

*yield
xtreg YLD_YTM_MID holding_ratio res_mat, fe

*bid ask
xtreg bid_ask_v1 holding_ratio res_mat, fe

xtreg bid_ask_v2 holding_ratio res_mat, fe

*restore
