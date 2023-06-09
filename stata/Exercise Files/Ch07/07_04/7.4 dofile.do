*Clear
clear

*Load Heart Transplant Data
use heart

*Set survival data
stset stime, id(id) failure(died==1)

*Plot Kaplan–Meier survivor function
sts graph

*Plot Kaplan–Meier survivor function by transplant status
sts graph, by(transplant) 

*Plot Kaplan–Meier survivor function with risktable
sts graph, by(transplant) risktable 

*Plot Kaplan–Meier survivor function with confidence intervals
sts graph, by(transplant) ci

*Plot Nelson–Aalen estimate of the cumulative hazard
sts graph, by(transplant) cumhaz

*Plot an estimate of the hazard function
sts graph, by(transplant) hazard 


