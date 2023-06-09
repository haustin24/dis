*Load auto data
sysuse auto, clear

*Generate base graph
tw (scatter price mpg) 

*Change the overall graph scheme
graph query, schemes
tw (scatter price mpg, scheme(s2mono))

*Search schemes
search scheme

*Exploring Stata colors
help colorstyle

*Set marker color to green
tw (scatter price mpg, mcolor(green))
*Adjusting opacity
tw (scatter price mpg, mcolor(green%50))
*Adjusting Intensity
tw (scatter price mpg, mcolor(green*0.5)) 
*Color as RGB values  
tw (scatter price mpg, mcolor("0 255 0")) 
