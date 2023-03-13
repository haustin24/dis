*Clear data
clear
*Set 1000 observations
set obs 200
*Generate count variable that is Poisson distributed with mean of 3
gen count = rpoisson(0.61)
*Tabulate count data
tab count
*Summarize count data
su count


*Clear data
clear
*Load count data
use smoking
*Describe data
describe
*Tabulate deaths
tab deaths 
*Regress deaths on smoking and age using OLS
reg deaths smokes i.agecat
*predict count
predict count
*Tabulate count 
tab count

