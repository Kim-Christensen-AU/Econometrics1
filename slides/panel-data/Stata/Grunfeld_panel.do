****
* Load the Grunfeld data
* firm
* year
* i: investment
* f: firm value
* c: capital stock
****
use "Grunfeld_panel.dta"

*** Define as panel data by setting individual and time variables: i and t
xtset firm year

*** Pooled OLS estimation
reg i f c

*** Fixed effects model (store results)
xtreg i f c, fe
estimates store FEresults 

*** Random effects model (store results)
xtreg i f c, re
estimates store REresults 

*** Hausman test: random or fixed effects
hausman FEresults  REresults, sigmamore