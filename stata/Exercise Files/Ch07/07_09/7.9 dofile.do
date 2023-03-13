*Clear
clear

*Load cancer data
use cancer
describe

*Set survival data
stset studytime, failure(died==1)

*Describe and summarize data
stdescribe
stsum, by(drug)

*Non-parametric analysis with risktable
sts graph, by(drug) risktable 

*Semi-parametric analysis
stcox i.drug age

*Plot survival function
stcurve, survival at1(drug=1) at2(drug=2)  at3(drug=3) 

*Test proportional hazards assumption
estat phtest, detail
