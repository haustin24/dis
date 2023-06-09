*Clear
clear

*Load Heart Transplant Data
use heart

*List first 10 observations
list in 1/10

*Set survival data
stset stime

*Set survival data with failure option
stset stime, failure(died==1) 

*Set survival data with failure option and id option
stset stime, failure(died==1) id(id)

*Set survival data with failure option and id option with custom risk time
stset stime, failure(died==1) id(id) origin(time 10) 

