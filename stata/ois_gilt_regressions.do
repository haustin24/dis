clear 
use ois_gilt_hr.dta
format Dates %td

rename percent_of_freefloat holding_ratio

*reformat ISIN
encode ISIN, generate(isin)

*remove if prog = NA 
keep if prog != "NA"

* prog string to factor (dummy)
egen programme = group(prog)

xtset isin Dates

xtreg gilt_ois_spread c.holding_ratio##programme, fe

merge m:m Dates using controls.dta, keep(match)

vl create controls = (UK10Y UKGDP UKCPI FTSE_ALL_SHARE_30D_VOL SONIA_WEDGE UKBANK_RATE)


collin holding_ratio $controls

xtreg gilt_ois_spread c.holding_ratio##programme $controls, fe