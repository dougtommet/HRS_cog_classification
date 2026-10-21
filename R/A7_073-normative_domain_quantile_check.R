# Analysis A7 side script (not run by the driver): do the cut points in the
# file shared with Lindsay Kobayashi (T < 37, T < 48) come from impaired-domain
# shares in the HRS-HCAP robust normative sample?
#
# Usage, from the project root, after Analyses A0 and A7:
#   Rscript R/A7_073-normative_domain_quantile_check.R
#
# PRE-SPECIFIED TEST (written 2026-10-20, before any result was seen)
#
# Hypothesis (Rich, 2026-10-20): the share of normative-sample participants
# with 2 or more impaired domains, and with 1 or more impaired domains, was
# used as a percentile target on the Core T score, with the percentile taken
# in the normative sample (weighted or unweighted), possibly in the A1_029
# non-imputed sample (n = 2,993).
#
#   Sample:   HRS-HCAP Wave 1 participants (inHCAP == 1), robust normative
#             sample (normexcld == 0), imputed_flag == 1 (the A1_029 sample,
#             as in A7_072), with a Core T score (TF) and a domain count
#             (Stata/w073_hcapdx.dta: n21 = 2+ domains, n22 = exactly 1).
#   Shares:   p2 = P(2+ domains), p1plus = P(1+ domains) in that sample.
#             N1 uses unweighted shares; N2 uses HCAP16WGTR-weighted shares.
#   N1:       unweighted quantiles of TF at the unweighted shares.
#   N2:       HCAP16WGTR-weighted quantiles of TF at the weighted shares.
#   Rounding: to the nearest integer.
#   Success:  N1 or N2, rounded, equals T < 37 and T < 48 exactly.
#
# Expectation (written before results): each domain is normed in this sample
# with impairment at -1.5 SD, so about 6.7% per domain; P(1+) is then at most
# about 33% and T is about N(50, 10) here, so the moderate cut should fall at
# or below about 46. Reproducing 48 needs P(1+) of about 42%, and 37 needs
# P(2+) of about 9.7%. We expect FAIL, at least for the moderate cut.
#
# Reported for information only (not part of the success criterion): the same
# rules in all normative-sample participants with T and a domain count (no
# imputed_flag restriction); mixed rules (unweighted shares, weighted
# quantiles, and the reverse); the share with exactly 1 impaired domain.
#
# Earlier tests of other rules (A7_072, 2026-10-19) failed. A pass here would
# be one of several rules tried and would support "consistent with", not
# "chosen by".
#
# Writes R_objects/A7_073_normative_domain_quantile_check.rds and prints a summary.

suppressPackageStartupMessages(library(tidyverse))

num <- function(x) as.numeric(haven::zap_labels(x))
key <- function(h, p) stringr::str_c(sprintf("%06d", as.integer(as.character(h))),
                                     sprintf("%03d", as.integer(as.character(p))))

target_cuts <- c(severe = 37, moderate = 48)   # shared file, 2026-07-10

hrs16 <- readRDS(here::here("R_objects", "A7_055_hrs16_merged.rds")) %>%
  transmute(k = key(HHID, PN), inHCAP = num(inHCAP), TF = num(TF),
            normexcld = num(normexcld), HCAP16WGTR = num(HCAP16WGTR))

flags <- readRDS(here::here("R_objects", "A0_014_hrshcap.rds")) %>%
  rename_with(tolower, any_of(c("HHID", "PN"))) %>%
  transmute(k = key(hhid, pn), imputed_flag = num(imputed_flag))

domains <- haven::read_dta(here::here("Stata", "w073_hcapdx.dta")) %>%
  rename_with(tolower, any_of(c("HHID", "PN"))) %>%
  transmute(k = key(hhid, pn), n21 = num(n21), n22 = num(n22)) %>%
  mutate(CogImp = case_when(n21 == 0 & n22 == 0 ~ 0,
                            n21 == 0 & n22 == 1 ~ 1,
                            n21 == 1            ~ 2))

norm_all <- hrs16 %>%
  filter(inHCAP == 1, normexcld == 0, !is.na(TF)) %>%
  left_join(flags, by = "k") %>%
  left_join(domains, by = "k") %>%
  filter(!is.na(CogImp))

sample_pre <- norm_all %>% filter(imputed_flag == 1)

wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}
wmean <- function(x, w) { ok <- !is.na(w) & w > 0; sum(x[ok] * w[ok]) / sum(w[ok]) }

shares <- function(d) c(
  p2_unw = mean(d$CogImp == 2), p1plus_unw = mean(d$CogImp >= 1), p1only_unw = mean(d$CogImp == 1),
  p2_w = wmean(d$CogImp == 2, d$HCAP16WGTR), p1plus_w = wmean(d$CogImp >= 1, d$HCAP16WGTR),
  p1only_w = wmean(d$CogImp == 1, d$HCAP16WGTR)
)

run_rules <- function(d) {
  sh <- shares(d)
  pu <- unname(sh[c("p2_unw", "p1plus_unw")]); pw <- unname(sh[c("p2_w", "p1plus_w")])
  q_u <- function(p) unname(stats::quantile(d$TF, probs = p))
  q_w <- function(p) wtd_quantile(d$TF, d$HCAP16WGTR, p)
  tibble(
    rule = c("N1: unweighted shares, unweighted quantiles",
             "N2: weighted shares, weighted quantiles",
             "Info: unweighted shares, weighted quantiles",
             "Info: weighted shares, unweighted quantiles"),
    primary = c(TRUE, TRUE, FALSE, FALSE),
    prob_severe   = c(pu[1], pw[1], pu[1], pw[1]),
    prob_moderate = c(pu[2], pw[2], pu[2], pw[2]),
    cut_severe    = c(q_u(pu)[1], q_w(pw)[1], q_w(pu)[1], q_u(pw)[1]),
    cut_moderate  = c(q_u(pu)[2], q_w(pw)[2], q_w(pu)[2], q_u(pw)[2])
  ) %>%
    mutate(rounded_severe = round(cut_severe),
           rounded_moderate = round(cut_moderate),
           reproduces = rounded_severe == target_cuts[["severe"]] &
                        rounded_moderate == target_cuts[["moderate"]],
           # share of the sample below the shared-file cuts, for comparison
           below_37_unw = mean(d$TF < 37), below_48_unw = mean(d$TF < 48),
           below_37_w = wmean(d$TF < 37, d$HCAP16WGTR), below_48_w = wmean(d$TF < 48, d$HCAP16WGTR))
}

res_pre <- run_rules(sample_pre) %>% mutate(sample = "Pre-specified: normative, imputed_flag == 1")
res_all <- run_rules(norm_all) %>% mutate(sample = "Info: all normative with T and domain count")

success <- any(res_pre$reproduces[res_pre$primary])

A7_073_normative_domain_quantile_check <- list(
  target_cuts = target_cuts,
  n_pre = nrow(sample_pre),
  n_all = nrow(norm_all),
  shares_pre = shares(sample_pre),
  shares_all = shares(norm_all),
  results = bind_rows(res_pre, res_all),
  success = success,
  date = Sys.Date()
)
saveRDS(A7_073_normative_domain_quantile_check,
        here::here("R_objects", "A7_073_normative_domain_quantile_check.rds"))

cat("\nPre-specified sample (normative, imputed_flag == 1): n =", nrow(sample_pre),
    "; all normative with T and domain count: n =", nrow(norm_all), "\n")
cat("Shares, pre-specified sample (%):\n")
print(round(100 * shares(sample_pre), 1))
cat("\n")
print(A7_073_normative_domain_quantile_check$results %>%
        select(sample, rule, prob_severe, prob_moderate, cut_severe, cut_moderate,
               rounded_severe, rounded_moderate, reproduces,
               below_37_unw, below_48_unw, below_37_w, below_48_w) %>%
        mutate(across(where(is.double), ~ round(.x, 3))), n = Inf, width = Inf)
cat("\nPre-specified test (N1 or N2 reproduces T < 37 and T < 48):",
    if (success) "PASS" else "FAIL", "\n")
