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
df <- read.csv("Grunfeld.csv")

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
fmt <- function(x, digits = 3) {
  formatC(x, digits = digits, format = "f")
}

fmt_p <- function(p) {
  fmt(max(p, 0.001), 3)
}

var_label <- function(x) {
  labels <- c("(Intercept)" = "Intercept", "f" = "$F_{it}$", "c" = "$C_{it}$")
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
    "\\resizebox{0.92\\textwidth}{!}{%",
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
    "\\begin{itemize}",
    "\\item R: phtest(fe, re)",
    "\\end{itemize}",
    "",
    "\\begin{center}",
    "\\scriptsize",
    "\\resizebox{0.70\\textwidth}{!}{%",
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "Test & $\\chi^2$ & df & $P$-value \\\\",
    "\\midrule",
    paste0("$\\xi_{\\text{H}}$ & ", fmt(stat), " & ", df, " & $", fmt_p(p), "$ \\\\"),
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    "\\end{center}"
  )

  writeLines(lines, output_file)
}

# --- Write LaTeX tables --------------------------------------
coef_table(pooled, "lm(i $\\sim$ f + c)", "grunfeld_pooled.tex", "$t$")
coef_table(fe, "plm(..., model = ``within'')", "grunfeld_fe.tex", "$t$")
coef_table(re, "plm(..., model = ``random'')", "grunfeld_re.tex", "$z$")
hausman_table("grunfeld_hausman.tex")
summary_table("grunfeld_summary.tex")
