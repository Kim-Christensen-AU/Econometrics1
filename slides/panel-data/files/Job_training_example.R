# ============================================================
# Job Training Panel Data Analysis
# Impact of Job Training on Firm Productivity
# Three years: 1987, 1988, 1989 | 54 firms
#
# Variables:
#   LSCRAP   - Log of scrap rate for firm i at time t
#   GRANT    - 1 if firm i received a grant at time t
#   GRANT_1  - 1 if firm i received a grant at time t-1
#   UNION    - 1 if firm i was unionised at time t
#   D88      - Year dummy for 1988
#   D89      - Year dummy for 1989
#   FCODE    - Firm identifier
#   YEAR     - Year
# ============================================================

# Install required packages if not already installed
# install.packages(c("plm", "lmtest"))

library(plm)
library(lmtest)

# --- Load data -----------------------------------------------
df <- read.csv("Job_training_example.csv")

# --- Define as panel data ------------------------------------
pdata <- pdata.frame(df, index = c("FCODE", "YEAR"))

# --- Pooled OLS ----------------------------------------------
pooled <- lm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 + UNION, data = pdata)
summary(pooled)

# --- Fixed Effects model -------------------------------------
# Note: time-invariant regressors (e.g. UNION if constant across time)
# are automatically dropped in the FE estimator
fe <- plm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 + UNION,
          data = pdata, model = "within")
summary(fe)

# --- First Difference model (no intercept) -------------------
fd <- plm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 - 1,
          data = pdata, model = "fd")
summary(fd)

# --- Random Effects model ------------------------------------
re <- plm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 + UNION,
          data = pdata, model = "random")
summary(re)

# --- Hausman test: Fixed vs Random Effects -------------------
phtest(fe, re)
