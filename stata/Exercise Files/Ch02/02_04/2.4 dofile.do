*Load auto data
sysuse auto, clear

*Generate base graph
tw (scatter price mpg) (scatter price turn) 

*Label the legend
tw (scatter price mpg, legend(label(1 "MPG"))) (scatter price turn, legend(label(2 "TURN"))) 
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN"))) 

*Order the legend
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN") order(2 1))) 

*Have legend in 1 column
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN") order(2 1) col(1))) 

*Place text before symbol in the legend
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN") order(2 1) col(1) textfirst))

*Stack text and symbols in the legened
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN") order(2 1) col(1) textfirst stack))  

*Place the legend at the 3 o'clock position
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN") order(2 1) col(1) textfirst stack position(3))) 

*Place the legend inside the graph region at the 5 o'clock position
tw (scatter price mpg) (scatter price turn, legend(label(1 "MPG") label(2 "TURN") order(2 1) col(1) textfirst stack position(5) ring(0))) 

*Turn the legend off
tw (scatter price mpg) (scatter price turn, legend(off)) 


