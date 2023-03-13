*set observations to 1000
set obs 1000

*generate random variables
gen x1 = runiform()
gen x2 = runiform()
gen e1 = rnormal()

*generate variable y as a function of all previous variables
gen y = 1 + 1*x1 - 2*x2 + 1*e1

*estimate the relationship between x1, x2 and y assuming e is observable
regress y x1 x2 e1

*estimate the relationship between x1, x2 and y assuming e is unobservable
regress y x1 x2
