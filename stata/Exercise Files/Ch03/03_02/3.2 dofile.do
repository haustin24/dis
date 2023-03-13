*Clear data
clear

*Load nlsw data
use nlsw

*Estimate a logit model of being a union member
logit union age wage i.collgrad i.country

*Computing the average probaility of union membership in the sample
margins

*Computing the average probaility of union membership if everyone in the data was treated as if they were 40
margins, at(age=40)

*Computing the average probaility of union membership if everyone in the data was treated as if they were 35 to 45
margins, at(age=(35(1)45))

*Plotting these effects using marginsplot
marginsplot

*Margins with a categorical variable
margins country
marginsplot

*Margins with a categorical variable examined at different ages
margins country, at(age=(35(1)45))
marginsplot

