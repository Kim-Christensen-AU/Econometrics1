****
*OLS estimators of linear panel data models
****
****
*The data: 545 individuals from 1980 through 1987 to model log(wage)
*x_{it} contains education, race dummies, experience, union membership, marital status, year dummies, and more
****

****
*Load the data and define as panel by setting i and t variables
****
use "wagepan.dta",clear
xtset nr year

*Pooled OLS estimator
reg lwage educ black hisp exper expersq married union d81-d87

*FE estimator (store results)
xtreg lwage educ black hisp exper expersq married union d81-d87, fe
estimates store FErgress

*RE estimator (store results)
xtreg lwage educ black hisp exper expersq married union d81-d87, re
estimates store RErgress

*Hausman test to decide between FE and RE
hausman FErgress RErgress, sigmamore
