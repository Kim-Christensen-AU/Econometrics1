# ============================================================
# Grunfeld Panel Data Analysis
# Variables:
#   firm  - firm identifier
#   year  - year
#   i     - investment
#   f     - firm value
#   c     - capital stock
# ============================================================

# Install required packages if not already installed
# install.packages(c("plm", "lmtest"))

library(plm)
library(lmtest)

# --- Load data -----------------------------------------------
df <- read.csv("Grunfeld_panel.csv")

# --- Define as panel data ------------------------------------
pdata <- pdata.frame(df, index = c("firm", "year"))

# --- Pooled OLS ----------------------------------------------
pooled <- lm(i ~ f + c, data = pdata)
summary(pooled)

# --- Fixed Effects model -------------------------------------
fe <- plm(i ~ f + c, data = pdata, model = "within")
summary(fe)

# --- Random Effects model ------------------------------------
re <- plm(i ~ f + c, data = pdata, model = "random")
summary(re)

# --- Hausman test: Fixed vs Random Effects -------------------
phtest(fe, re)
