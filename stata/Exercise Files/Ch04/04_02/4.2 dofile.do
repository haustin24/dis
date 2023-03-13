*Clear data
clear

*Load reshape1 training data
webuse reshape1

*Explore the wide form data
list

*Reshape data from wide to long 
reshape long inc ue, i(id) j(year)

*Explore the long form data
list, sep(3)

*Set panel data
xtset id year
