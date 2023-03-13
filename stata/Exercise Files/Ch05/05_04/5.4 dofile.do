
*Monte Carlo Simulation
*Write a custom programme that generates a random data structure and then estimates OLS
program define simulate1, rclass
	clear
	set obs 1000
	gen e1 = rnormal()
	gen x1 = rnormal()
	gen x2 = rnormal() + 1*e1
	gen y = 1 + 1*x1 + 1*x2 + 1*e1
	reg y x1 x2
end

*Use the simulate command to run the programme "simulate1" 1000 times
simulate, reps(500): simulate1

*Explore the simulation results
summarize
kdensity _b_x2
