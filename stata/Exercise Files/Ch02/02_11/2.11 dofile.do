*Clear data
clear

*Load auto training data
sysuse auto

*Scatterplit and linear fit
tw (sc price weight) (lfit price weight)

*Plotting graph by variable foreign
tw (sc price weight) (lfit price weight, by(foreign))

*Adding title and ytitle text
tw (sc price weight) (lfit price weight, by(foreign) title(Price vs Weight) ytitle(Price))

*Adding custom 45 degree line
tw (sc price weight) (lfit price weight, by(foreign) title(Price vs Weight) ytitle(Price)) (fn y = x, range(2000 5000))

*Formatting the legend and labelling the 45 degree line
tw (sc price weight) (lfit price weight, by(foreign) title(Price vs Weight) ytitle(Price) legend(label(3 "45 Degree Line") row(1))) (fn y = x, range(2000 5000))