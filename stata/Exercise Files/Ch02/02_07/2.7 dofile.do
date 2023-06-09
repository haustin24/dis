*Load auto data
sysuse auto, clear

*Generate two exemplar scatter plots with linear fits and store in memory (replace if already stored)
tw (scatter price mpg if foreign == 0, title(Domestic)) (qfit price mpg if foreign == 0), name(graph1) 
tw (scatter price mpg if foreign == 1, title(Foreign)) (qfit price mpg if foreign == 1), name(graph2) 

*Combine both memory graphs
graph combine graph1 graph2

*Combine both memory graphs forcing a common y- and x-axis
graph combine graph1 graph2, ycommon xcommon

*Combine both memory graphs forcing a common y- and x-axis and shrink text appropriately
graph combine graph1 graph2, ycommon xcommon altshrink

*Combine both memory graphs forcing a common y- and x-axis, shrink text appropriately and use only 1 column
graph combine graph1 graph2, ycommon xcommon altshrink col(1)

*Combine both memory graphs forcing a common y- and x-axis, shrink text appropriately and use leave a hole at position 2 and 3
graph combine graph1 graph2, ycommon xcommon altshrink hole(2 3)

