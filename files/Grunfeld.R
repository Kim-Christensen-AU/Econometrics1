# ============================================================
# Grunfeld Panel Data Analysis
#
# Variables:
#   firm  - firm identifier
#   year  - year
#   i     - investment
#   f     - firm value
#   c     - capital stock
#
# The script prints the standard R/plm output and writes LaTeX
# tables used in the panel-data lecture slides.
# ============================================================

library(plm)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  setwd(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
}

# --- Load data -----------------------------------------------
df <- read.csv("Grunfeld_panel.csv")

# --- Define as panel data ------------------------------------
pdata <- pdata.frame(df, index = c("firm", "year"))

# --- Estimate models -----------------------------------------
pooled <- lm(i ~ f + c, data = pdata)
fe <- plm(i ~ f + c, data = pdata, model = "within")
re <- plm(i ~ f + c, data = pdata, model = "random")
hausman <- phtest(fe, re)

# --- Print standard R output ---------------------------------
print(summary(pooled))
print(summary(fe))
print(summary(re))
print(hausman)

# --- Helpers --------------------------------------------------
fmt <- function(x, digits = 4) {
  formatC(x, digits = digits, format = "f")
}

fmt_p <- function(p) {
  ifelse(p < 0.001, "$<0.001$", fmt(p, 3))
}

stars <- function(p) {
  ifelse(p < 0.01, "$^{***}$",
    ifelse(p < 0.05, "$^{**}$",
      ifelse(p < 0.10, "$^{*}$", "")))
}

var_label <- function(x) {
  labels <- c("(Intercept)" = "Intercept", "f" = "$F_{it}$", "c" = "$C_{it}$")
  ifelse(x %in% names(labels), labels[x], x)
}

coef_table <- function(model, model_label, output_file, statistic_label) {
  coefs <- coef(summary(model))
  rows <- rownames(coefs)
  stat_col <- grep("^(t|z)[ -]value$", colnames(coefs), value = TRUE)
  p_col <- grep("^Pr", colnames(coefs), value = TRUE)

  lines <- c(
    "\\begin{center}",
    "\\scriptsize",
    paste0("\\textbf{", model_label, "}\\\\[0.15cm]"),
    "\\resizebox{0.92\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    paste0("Variable & Estimate & Std. Error & ", statistic_label, " & $p$-value \\\\"),
    "\\midrule"
  )

  for (r in rows) {
    lines <- c(lines, paste0(
      var_label(r), " & ",
      fmt(coefs[r, "Estimate"]), stars(coefs[r, p_col]), " & ",
      fmt(coefs[r, "Std. Error"]), " & ",
      fmt(coefs[r, stat_col]), " & ",
      fmt_p(coefs[r, p_col]), " \\\\"
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

summary_table <- function(output_file) {
  pooled_coef <- coef(summary(pooled))
  fe_coef <- coef(summary(fe))
  re_coef <- coef(summary(re))

  coef_or_blank <- function(tab, row, col) {
    if (row %in% rownames(tab)) fmt(tab[row, col]) else ""
  }

  se_or_blank <- function(tab, row) {
    if (row %in% rownames(tab)) paste0("(", fmt(tab[row, "Std. Error"]), ")") else ""
  }

  lines <- c(
    "\\begin{center}",
    "\\scriptsize",
    "\\resizebox{0.86\\textwidth}{!}{%",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & Pooled OLS & FE & RE \\\\",
    "\\midrule",
    paste0("$F_{it}$ & ", coef_or_blank(pooled_coef, "f", "Estimate"), " & ",
      coef_or_blank(fe_coef, "f", "Estimate"), " & ",
      coef_or_blank(re_coef, "f", "Estimate"), " \\\\"),
    paste0(" & ", se_or_blank(pooled_coef, "f"), " & ",
      se_or_blank(fe_coef, "f"), " & ",
      se_or_blank(re_coef, "f"), " \\\\[0.08cm]"),
    paste0("$C_{it}$ & ", coef_or_blank(pooled_coef, "c", "Estimate"), " & ",
      coef_or_blank(fe_coef, "c", "Estimate"), " & ",
      coef_or_blank(re_coef, "c", "Estimate"), " \\\\"),
    paste0(" & ", se_or_blank(pooled_coef, "c"), " & ",
      se_or_blank(fe_coef, "c"), " & ",
      se_or_blank(re_coef, "c"), " \\\\"),
    "\\midrule",
    paste0("Firm effects & No & Yes & Random \\\\"),
    paste0("Firms & ", pdim(pdata)$nT$n, " & ", pdim(pdata)$nT$n, " & ", pdim(pdata)$nT$n, " \\\\"),
    paste0("Years & ", pdim(pdata)$nT$T, " & ", pdim(pdata)$nT$T, " & ", pdim(pdata)$nT$T, " \\\\"),
    paste0("Observations & ", pdim(pdata)$nT$N, " & ", pdim(pdata)$nT$N, " & ", pdim(pdata)$nT$N, " \\\\"),
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
    "Test & $\\chi^2$ & df & $p$-value \\\\",
    "\\midrule",
    paste0("Hausman & ", fmt(stat), " & ", df, " & ", fmt_p(p), " \\\\"),
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{center}"
  )

  writeLines(lines, output_file)
}

# --- Write LaTeX tables --------------------------------------
coef_table(pooled, "R output: pooled OLS, lm(i $\\sim$ f + c)", "grunfeld_pooled_table.tex", "$t$")
coef_table(fe, "R output: fixed effects, plm(..., model = ``within'')", "grunfeld_fe_table.tex", "$t$")
coef_table(re, "R output: random effects, plm(..., model = ``random'')", "grunfeld_re_table.tex", "$z$")
hausman_table("grunfeld_hausman_table.tex")
summary_table("grunfeld_summary_table.tex")
