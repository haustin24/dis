********** EXAMPLE 1 - Endogeneity of x2
clear

*set observations to 1000
set obs 1000

*generate random variables where x2 is endogenous
gen e1 = rnormal()
gen x1 = rnormal()
gen x2 = rnormal() + 1*e1 

*generate variable y as a function of all previous variables
gen y = 1 + 1*x1 - 2*x2 + 1*e1

*estimate the relationship between x1, x2 and y assuming e1 and e2 are unobservable
regress y x1 x2


********** EXAMPLE 2 - Multicollinearity of x2
clear

*set observations to 1000
set obs 1000

*generate random variables where x2 is endogenous
gen e1 = rnormal()
gen x1 = rnormal()
gen x2 = x1 + 0.1*rnormal()

*generate variable y as a function of all previous variables
gen y = 1 + 1*x1 - 2*x2 + 1*e1 

*estimate the relationship between x1, x2 and y assuming e1 is unobservable
regress y x1 x2


