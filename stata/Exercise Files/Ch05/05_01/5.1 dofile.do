*Viewing all available functions including random number functions
help functions

*Calling a single random number from the normal distribution
di rnormal()
*repeat
di rnormal()

*Setting a seed
set seed 1234

*Generating multiple draws of random numbers from the normal distribution
set obs 1000
gen x1 = rnormal()

*Generating multiple draws of random numbers from the uniform distribution
gen x2 = runiform()

*Summarize x1 and x2
su
