*Clear data
clear

*Input 3 numbers
set obs 3
generate var1 = 1.587 in 1
replace var1 = 10 in 2
replace var1 = 12345678 in 3

*Examine format of var1 and list numbers
describe var1
list var1

*Format var1 to include no more than 2 decimal places
format var1 %9.2g
list var1

*Format var1 to fixed format with maximum 13 characters including 2 decimal places
format var1 %13.2f
list var1

*Format var1 to fixed format with maximum 13 characters including 2 decimal places and left-alignment
format var1 %-13.2f
list var1

*Format var1 to fixed format with maximum 13 characters including 2 decimal places and left-alignment and European comma's
format var1 %-13,2f
list var1

*Format var1 to fixed format with maximum 13 characters including 2 decimal places and left-alignment and European comma's and large number breaks
format var1 %-13,2fc
list var1

