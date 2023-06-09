*Load auto data
sysuse auto, clear

*Generate base graph
tw (scatter price mpg) (qfit price mpg)

*Base graph by each category of rep78
tw (scatter price mpg) (qfit price mpg, by(rep78))

*Base graph by each category of rep78 scaled to 50%
tw (scatter price mpg) (qfit price mpg, by(rep78, iscale(0.5)))

*Base graph by each category of rep78 scaled to 50% with a total graph added
tw (scatter price mpg) (qfit price mpg, by(rep78, iscale(0.5) total))

*Base graph by each category of rep78 scaled to 50% with a total and missing graph added
tw (scatter price mpg) (qfit price mpg, by(rep78, iscale(0.5) total missing))

*Base graph by each category of rep78 scaled to 50% with a total and missing graph added presented over 2 rows
tw (scatter price mpg) (qfit price mpg, by(rep78, iscale(0.5) total missing row(2)))

*Base graph by each category of rep78 scaled to 50% with a total and missing graph added presented over 2 rows using a compact style
tw (scatter price mpg) (qfit price mpg, by(rep78, iscale(0.5) total missing row(2) compact))

*Base graph by each category of rep78 and foreign scaled to 50% with a total and missing graph added presented over 2 rows using a compact style
tw (scatter price mpg) (qfit price mpg, by(rep78 foreign, iscale(0.5) total missing row(3) compact))

