*Clear
clear

*Load Heart Transplant Data
use heart

*Set survival data
stset stime, id(id) failure(died==1)

*Summarize survival variables
su _*

*Describe survival-time data
stdescribe

*Summarize survival-time data
stsum
stsum, by(transplant)

*Calculate person-time, incidence rates, and SMR
stptime, by(transplant) per(1000) 




