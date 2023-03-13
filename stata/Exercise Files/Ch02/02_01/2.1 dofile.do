*Clear
clear

*Load auto training data
sysuse auto

*Original scatter plot
tw (sc price mpg)

*Examine marker options
help scatter

*Scatter plot with X markers
tw (sc price mpg, msymbol(X)) 

*Scatter plot with X markers and very large
tw (sc price mpg, msymbol(X) msize(vlarge)) 

*Scatter plot with X markers and 5 times default size
tw (sc price mpg, msymbol(X) msize(*5)) 

*Multiple scatter plots
tw (sc price mpg if foreign == 0, msymbol(X) msize(*5)) (sc price mpg if foreign == 1, msymbol(D) msize(*5))  
