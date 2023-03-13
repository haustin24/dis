*Load auto data
sysuse auto, clear

*Generate base graph
tw (scatter price mpg)

*Scatter plot of price against miles per gallon with labelled markers
tw (scatter price mpg, mlabel(make))

*Scatter plot of price against miles per gallon with titles
tw (scatter price mpg, title(Price vs MPG) subtitle(Cars) xtitle(MPG) ytitle(Price) note(Autodata))

*Scatter plot of price against miles per gallon with added textbox
tw (scatter price mpg, text(10000 30 "Something" "Important"))

*A list of all special symbols and characters supported by SMCL in Stata Graphs 
help graph_text

*Scatter plot of price against miles per gallon with added textbox, bold and italic font 
tw (scatter price mpg, text(10000 30 "{bf:Something}" "{it:Important}"))



