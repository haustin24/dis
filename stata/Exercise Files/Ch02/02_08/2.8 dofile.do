*Load auto data
sysuse auto, clear

*Plot two categorical variables on scatterplot
tw (sc foreign rep78) 

*Use jitter to reveal overlapping data points 
tw (sc foreign rep78, jitter(10)) 

*Plot "confidential" price against mpg
tw (sc price mpg)

*Use jitter to anonymise "confidential" data points but keep relationship similar
tw (sc price mpg) (sc price mpg, jitter(6))
