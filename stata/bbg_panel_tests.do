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

*save
save 

use bbg_dataset_stata.dta

*** Breusch-Pagan LM test ***
*Test for heterogeneity 


*step 1: Estimate Pooled OLS 
reg yld_ytm_mid holding_ratio res_mat

*step 2: Estimate REM

*set panel variables 
xtset ISIN Dates

*** Previously there were duplicates, code to help identify and solve
*isid isin Dates

*duplicates report isin Dates

*duplicates list isin Dates

xtreg yld_ytm_mid holding_ratio res_mat, re

*LM test for covariance
xttest0 
*results show chibar2 = 0.0000
*REJECT NULL HYPOTHESIS THAT VARIANCE OF INDIVIDUAL EFFECTS IS NOT STATISTICALLY DIFFERENT FROM ZERO (Hetorogeneity exists)


*** F test *** 
*Test for heterogeneity correlated with Xs 


xtreg yld_ytm_mid holding_ratio res_mat, fe

*F = 0.000 
*REJECT NULL HYPOTHESIS
*USE FE 


*** Hausman test *** 
*Test whether the individual effects are correlated with the Xs or not

*Step 1: estimate FEM 
xtreg yld_ytm_mid holding_ratio res_mat, fe

*Step 2: Save results 
estimates store fixed
*Step 3: Estimate the REM
xtreg yld_ytm_mid holding_ratio res_mat, re

estimates store random

*Step 4: Hausman test
hausman fixed random, sigmamore

*CHI2 value = 0.0000 
*FE model better suited. 




