clear
cd "~/R/Dissertation/stata"

*import from csv due to date issues 
import delimited "~/R/Dissertation/output/bbg_dataset.csv", colrange(2:)
destring, replace ignore("NA" "Inf")

*reformat string to date
generate Dates = date(dates, "YMD")
generate Maturity = date(maturity, "YMD")

*format dates for viewing
format Dates %td
format Maturity %td

rename percent_of_freefloat holding_ratio

*reformat ISIN
encode isin, generate(ISIN)

*remove if prog = NA 
keep if prog != "NA"

* prog string to factor (dummy)
egen programme = group(prog)

*drop original dates column 
drop dates

*save
save "bbg_dataset_stata.dta", replace



