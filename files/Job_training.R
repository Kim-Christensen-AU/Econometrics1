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

library(plm)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  setwd(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
}

# --- Load data -----------------------------------------------
df <- read.csv("Job_training_example.csv")

# --- Define as panel data ------------------------------------
pdata <- pdata.frame(df, index = c("FCODE", "YEAR"))

# --- Pooled OLS ----------------------------------------------
pooled <- lm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 + UNION, data = pdata)

# --- Fixed Effects model -------------------------------------
# Note: time-invariant regressors (e.g. UNION if constant across time)
# are automatically dropped in the FE estimator
fe <- plm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 + UNION,
          data = pdata, model = "within")

# --- First Difference model (no intercept) -------------------
fd <- plm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 - 1,
          data = pdata, model = "fd")

# --- Random Effects model ------------------------------------
re <- plm(LSCRAP ~ D88 + D89 + GRANT + GRANT_1 + UNION,
          data = pdata, model = "random")
hausman <- phtest(fe, re)

# --- Print standard R output ---------------------------------
print(summary(pooled))
print(summary(fe))
print(summary(fd))
print(summary(re))
print(hausman)

# --- Helpers --------------------------------------------------
fmt <- function(x, digits = 4) {
  formatC(x, digits = digits, format = "f")
}

fmt_p <- function(p) {
  fmt(max(p, 0.001), 3)
}

var_label <- function(x) {
  labels <- c(
    "(Intercept)" = "Intercept",
    "D88" = "$D88_t$",
    "D89" = "$D89_t$",
    "GRANT" = "$grant_{it}$",
    "GRANT_1" = "$grant_{i,t-1}$",
    "UNION" = "$union_i$"
  )
  ifelse(x %in% names(labels), labels[x], x)
}

coef_table <- function(model, call_text, output_file, statistic_label) {
  coefs <- coef(summary(model))
  rows <- rownames(coefs)
  stat_col <- grep("^(t|z)[ -]value$", colnames(coefs), value = TRUE)
  p_col <- grep("^Pr", colnames(coefs), value = TRUE)

  lines <- c(
    "\\begin{itemize}",
    paste0("\\item R: ", call_text),
    "\\end{itemize}",
    "",
    "\\begin{center}",
    "\\scriptsize",
    "\\resizebox{0.96\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    paste0("Variable & Estimate & Std. Error & ", statistic_label, " & $P$-value \\\\"),
    "\\midrule"
  )

  for (r in rows) {
    lines <- c(lines, paste0(
      var_label(r), " & ",
      fmt(coefs[r, "Estimate"]), " & ",
      fmt(coefs[r, "Std. Error"]), " & ",
      fmt(coefs[r, stat_col]), " & ",
      "$", fmt_p(coefs[r, p_col]), "$ \\\\"
    ))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{center}"
  )

  writeLines(lines, output_file)
}

hausman_table <- function(output_file) {
  stat <- as.numeric(hausman$statistic)
  df <- as.numeric(hausman$parameter)
  p <- as.numeric(hausman$p.value)

  lines <- c(
    "\\begin{center}",
    "\\scriptsize",
    "\\resizebox{0.70\\textwidth}{!}{%",
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "Test & $\\chi^2$ & df & $P$-value \\\\",
    "\\midrule",
    paste0("Hausman & ", fmt(stat), " & ", df, " & $", fmt_p(p), "$ \\\\"),
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{center}"
  )

  writeLines(lines, output_file)
}

# --- Write LaTeX tables --------------------------------------
coef_table(
  pooled,
  "lm(LSCRAP $\\sim$ D88 + D89 + GRANT + GRANT\\_1 + UNION)",
  "job_training_pooled.tex",
  "$t$"
)
coef_table(
  fe,
  "plm(..., model = ``within'')",
  "job_training_fe.tex",
  "$t$"
)
coef_table(
  fd,
  "plm(LSCRAP $\\sim$ D88 + D89 + GRANT + GRANT\\_1 - 1, model = ``fd'')",
  "job_training_fd.tex",
  "$t$"
)
coef_table(
  re,
  "plm(..., model = ``random'')",
  "job_training_re.tex",
  "$z$"
)
hausman_table("job_training_hausman.tex")
