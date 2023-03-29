clear
use boe_dataset.dta

describe

format operation_date %td

encode ISIN, generate(isin)
encode prog, generate(programme)

summarize

*summary statistics by programme 
by programme, sort : summarize res_mat nominal_purchases holding_ratio YLD_YTM_MID bid_ask_v1 bid_ask_v2 bid_ask_v3


*save to word doc
*using "asdoc _______ , save(filename)"
asdoc by programme, sort : summarize res_mat nominal_purchases holding_ratio YLD_YTM_MID bid_ask_v1 bid_ask_v2 bid_ask_v3, save(stata_results.doc)


*holding ratio table
table () ( programme ) (), nototals statistic(frequency) statistic(mean holding_ratio) statistic(sd holding_ratio) statistic(skewness holding_ratio) statistic(q1 holding_ratio) statistic(q2 holding_ratio) statistic(q3 holding_ratio) statistic(max holding_ratio)


*histogram of holding ratio per programme
*more likely do charts in R
histogram holding_ratio, by(programme)
