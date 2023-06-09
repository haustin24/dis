*Load auto data
sysuse auto, clear

*Generate base graph
tw (scatter price mpg) (qfit price mpg)

*Base graph with aspectratio option
tw (scatter price mpg) (qfit price mpg), aspectratio(1)
tw (scatter price mpg) (qfit price mpg), aspectratio(0.25)

*Base graph with ysize and xsize options
tw (scatter price mpg) (qfit price mpg), ysize(1) xsize(3)
tw (scatter price mpg) (qfit price mpg), ysize(3) xsize(1)

*Base graph with ysize and xsize options and scale option
tw (scatter price mpg) (qfit price mpg), ysize(3) xsize(1) scale(2)
tw (scatter price mpg) (qfit price mpg), ysize(3) xsize(1) scale(0.5)
