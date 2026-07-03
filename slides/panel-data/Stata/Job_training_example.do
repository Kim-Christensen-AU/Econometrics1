clear all
****
*Estimation of linear panel data models: Impact of Job Training on Firm Productivity
*OLS, FE and RE estimators
****

********************************************************************************************************
*The data
**three years, 1987, 1988, and 1989
**54 firms reporting scrap rates each year
**No grant prior to 1988, 19 firms received grants in 1988 and 10 in 1989
***Job training in 1988 could have made workers more productive in 1989-include lag grant indicator
**We include year dummies
********************************************************************************************************


*Load the job training data
drop _all
cls
set more off
use "Job_training_example.dta", clear

/*
Variable Description
lscrap_it	Log of the scrap rate for firm i at time t
grant_it	Dummy variable, equal to 1 if firm i received a grant at time t
grant_(i,t-1)	Dummy variable, equal to 1 if firm i received a grant at time t-1
union_it	Dummy variable, equal to 1 if firm i was a member of a union at time t
d88_it	Dummy variable, equal to 1 if year 1988
d89_it	Dummy variable, equal to 1 if year 1989
fcode_it	Identifier of firm i
year_it	
*/

*** Define data as a panel by setting individual and time variables: i and t
xtset FCODE YEAR 

*** Pooled OLS estimation
reg LSCRAP D88 D89 GRANT GRANT_1 UNION 

*** Fixed effects estimator: any regressor that is time invariant will be automatically dropped
xtreg LSCRAP D88 D89 GRANT GRANT_1 UNION, fe
*** Store estimation results
estimates store FErgress 
	
*** First difference model: we CANNOT include a constant
reg d.LSCRAP d.D88 d.D89 d.GRANT d.GRANT_1, nocons
	
*** Random effects estimator
xtreg LSCRAP D88 D89 GRANT GRANT_1 UNION, re
*** Store estimation results
estimates store RErgress 

*** Hausman test: random or fixed effects
hausman FErgress RErgress, sigmamore 
	
