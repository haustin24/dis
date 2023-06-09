*Open Auto Data
clear
sysuse auto

*Summarize price and return stored results
su price
return list 

*Perform computation with previously stored mean
di r(mean) * 2

*Regress price against mpg and length and return stored general & estimation results
regress price mpg length
ereturn list

*Explore stored matrices
matrix list e(b)
matrix list e(V)

*Perform computations with stored system variables
di _b[mpg] * 2
di _se[mpg] * 2
