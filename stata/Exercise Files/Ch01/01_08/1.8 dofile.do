*clear data
clear

*Load auto training data
sysuse auto

*compute the square root of the variance of weight
su weight
return list
di sqrt(r(Var))

*generate weight quartiles
egen quartiles = cut(weight), group(4)
tab quartiles

*loop over weight quartiles
	forvalues i = 0/3 {
		reg price mpg i.rep78 headroom if quartiles == `i'
	}

*loop over weight quartiles and foreign
forvalues j = 0/1 {
	forvalues i = 0/3 {
		reg price mpg i.rep78 headroom if quartiles == `i' & foreign == `j'
	}
}

