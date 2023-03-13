*Clear data
clear

*Load Auto training data
use date

*Examine data
describe
list

*Convert string date to numeric date
gen date2 = date(datestring1, "DM20Y")
list

*Format date2 to day format
format date2 %td
list

*Split year, month and day information into three separate variables
gen year = year(date2)
gen month = month(date2)
gen day = day(date2)
list

*Generate date variable date3 from numeric year, month and day information
gen date3 =mdy(month, day, year)
list

