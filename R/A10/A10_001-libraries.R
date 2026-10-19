# A10_001: packages and helpers for Analysis A10 (data sharing runs).

pacman::p_load(tidyverse, haven, knitr, survey)

options(survey.lonely.psu = "adjust")

# Plain numeric copy of a (possibly haven-labelled) vector
num <- function(x) as.numeric(haven::zap_labels(x))

# HHID/PN join key that ignores numeric vs. character storage
hhidpn_key <- function(hhid, pn) {
  stringr::str_c(sprintf("%06d", as.integer(as.character(hhid))),
                 sprintf("%03d", as.integer(as.character(pn))))
}

wave_year <- c(P = 2016L, Q = 2018L, R = 2020L, S = 2022L)

dx_labels <- c("Normal", "MCI", "Dementia")
dx_factor <- function(x) {
  f <- factor(x, levels = 1:3, labels = dx_labels)
  forcats::fct_na_value_to_level(f, "Not classified")
}

# Display helpers for inline values
fmt_n   <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digits = 1) formatC(100 * x, format = "f", digits = digits)
fmt_cut <- function(x) ifelse(x %% 1 == 0, formatC(x, format = "d"), formatC(x, format = "f", digits = 1))
fmt_k   <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt_sci <- function(x) {
  if (is.na(x)) return("not available")
  if (x == 0) "0" else format(signif(x, 2), scientific = TRUE)
}
