*Clear the data
clear

*Load the articles data
use articles

*Explore the data
des
su
tab art

*Estimate poisson regression
poisson art fem mar i.kid5 phd ment

*Compute marginal effects
margins, dydx(*)

*Compute goodnes-of-fit test 
estat gof

*Test for overdispersion
overdisp art fem mar i.kid5 phd ment

*Estimate negative binomial regression
nbreg art fem mar i.kid5 phd ment

*Compute marginal effects
margins, dydx(*)






