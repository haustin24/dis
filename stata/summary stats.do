clear
use boe_dataset.dta

describe

format operation_date %td

encode ISIN, generate(isin)
encode prog, generate(programme)

summarize

*summary statistics by programme 
by programme, sort : summarize res_mat nominal_purchases holding_ratio YLD_YTM_MID bid_ask_v1 bid_ask_v2 bid_ask_v3

putdocx table table1=etable
*save to word doc
*using "asdoc _______ , save(filename)"
asdocx by programme, sort : summarize res_mat nominal_purchases holding_ratio YLD_YTM_MID bid_ask_v1 bid_ask_v2 bid_ask_v3, save(stata_results.docx)

