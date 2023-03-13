
*Initial simulation with noise 1
clear
set obs 1000
gen e = rnormal()
gen x = rnormal()
gen y = 1*x + 1*e
reg y x 

*Simulation with noise 10
clear
set obs 1000
gen e = rnormal()
gen x = rnormal()
gen y = 1*x + 10*e
reg y x 

*Simulation with noise 10 and 100 times the observations
clear
set obs 100000
gen e = rnormal()
gen x = rnormal()
gen y = 1*x + 10*e
reg y x 

*Simulation with a non-normal error term
clear
set obs 1000
gen e =  runiform()
gen x = rnormal()
gen y = 1*x + 1*e
reg y x 

