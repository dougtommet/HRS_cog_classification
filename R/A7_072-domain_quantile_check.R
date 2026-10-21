# Analysis A7 side script (not run by the driver): does the A1_029 cut-point
# rule reproduce the cut points in the file shared with Lindsay Kobayashi?
#
# Usage, from the project root, after Analyses A0 and A7:
#   Rscript R/A7_072-domain_quantile_check.R
#
# PRE-SPECIFIED TEST (written 2026-10-19, before any result was seen)
#
# Rule (from R/A1_029-finding_algorithm_cutpoints.qmd, "Finding cognitive
# impairment thresholds"): cut the Core T score at the quantiles that match
# the share of HRS-HCAP Wave 1 participants with 2 or more impaired cognitive
# domains (severe cut) and with 1 or more impaired domains (moderate cut),
# as classified by the HCAP algorithm (Stata/w073_hcapdx.dta: n21 = 2+
# domains impaired, n22 = 1 domain impaired).
#
#   Sample:   HRS-HCAP Wave 1 participants (inHCAP == 1) with
#             imputed_flag == 1 (no imputed HCAP inputs; n = 2,993 in A1_029)
#             and a Core T score (TF, Analysis A7).
#   R1:       unweighted quantiles of TF at probs .15 and .35, the values
#             hard-coded in A1_029 (stats::quantile, default type 7).
#   R2:       unweighted quantiles of TF at the observed shares in the same
#             sample: P(2+ domains) and P(1+ domains).
#   Rounding: to the nearest integer.
#   Success:  R1 or R2, rounded, equals the shared file's cut points
#             (T < 37 and T < 48) exactly.
#
# Reported for information only (not part of the success criterion):
# unrounded quantiles, survey-weighted (HCAP16WGTR) quantiles, and the same
# quantiles in all HRS-HCAP Wave 1 participants with a T score.
#
# Writes R_objects/A7_072_domain_quantile_check.rds and prints a summary.

suppressPackageStartupMessages(library(tidyverse))

num <- function(x) as.numeric(haven::zap_labels(x))
key <- function(h, p) stringr::str_c(sprintf("%06d", as.integer(as.character(h))),
                                     sprintf("%03d", as.integer(as.character(p))))

target_cuts <- c(severe = 37, moderate = 48)   # shared file, 2026-07-10

hrs16 <- readRDS(here::here("R_objects", "A7_055_hrs16_merged.rds")) %>%
  transmute(k = key(HHID, PN), inHCAP = num(inHCAP), TF = num(TF),
            HCAP16WGTR = num(HCAP16WGTR))

flags <- readRDS(here::here("R_objects", "A0_014_hrshcap.rds")) %>%
  rename_with(tolower, any_of(c("HHID", "PN"))) %>%
  transmute(k = key(hhid, pn), imputed_flag = num(imputed_flag))

domains <- haven::read_dta(here::here("Stata", "w073_hcapdx.dta")) %>%
  rename_with(tolower, any_of(c("HHID", "PN"))) %>%
  transmute(k = key(hhid, pn), n21 = num(n21), n22 = num(n22)) %>%
  mutate(CogImp = case_when(n21 == 0 & n22 == 0 ~ 0,
                            n21 == 0 & n22 == 1 ~ 1,
                            n21 == 1            ~ 2))

hcap_all <- hrs16 %>%
  filter(inHCAP == 1, !is.na(TF)) %>%
  left_join(flags, by = "k") %>%
  left_join(domains, by = "k")

sample_pre <- hcap_all %>% filter(imputed_flag == 1, !is.na(CogImp))

wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

shares <- function(d) c(p2 = mean(d$CogImp == 2), p1plus = mean(d$CogImp >= 1))

run_rules <- function(d) {
  sh <- shares(d)
  r1 <- unname(stats::quantile(d$TF, probs = c(.15, .35)))
  r2 <- unname(stats::quantile(d$TF, probs = unname(sh)))
  w1 <- wtd_quantile(d$TF, d$HCAP16WGTR, c(.15, .35))
  w2 <- wtd_quantile(d$TF, d$HCAP16WGTR, unname(sh))
  tibble(
    rule = c("R1: probs .15, .35", "R2: observed shares",
             "Weighted, probs .15, .35", "Weighted, observed shares"),
    prob_severe   = c(.15, sh[["p2"]], .15, sh[["p2"]]),
    prob_moderate = c(.35, sh[["p1plus"]], .35, sh[["p1plus"]]),
    cut_severe    = c(r1[1], r2[1], w1[1], w2[1]),
    cut_moderate  = c(r1[2], r2[2], w1[2], w2[2])
  ) %>%
    mutate(rounded_severe = round(cut_severe),
           rounded_moderate = round(cut_moderate),
           reproduces = rounded_severe == target_cuts[["severe"]] &
                        rounded_moderate == target_cuts[["moderate"]])
}

res_pre <- run_rules(sample_pre) %>% mutate(sample = "Pre-specified (imputed_flag == 1)")
res_all <- run_rules(hcap_all %>% filter(!is.na(CogImp))) %>%
  mutate(sample = "All HRS-HCAP Wave 1 with T and domain count")

success <- any(res_pre$reproduces[res_pre$rule %in% c("R1: probs .15, .35", "R2: observed shares")])

A7_072_domain_quantile_check <- list(
  target_cuts = target_cuts,
  n_pre = nrow(sample_pre),
  n_all = nrow(hcap_all %>% filter(!is.na(CogImp))),
  n_no_domain = sum(is.na(hcap_all$CogImp)),
  shares_pre = shares(sample_pre),
  results = bind_rows(res_pre, res_all),
  success = success,
  date = Sys.Date()
)
saveRDS(A7_072_domain_quantile_check,
        here::here("R_objects", "A7_072_domain_quantile_check.rds"))

cat("\nPre-specified sample: n =", nrow(sample_pre),
    "(A1_029 reported 2,993)\n")
cat("Observed shares: 2+ domains =", round(100 * shares(sample_pre)[["p2"]], 1),
    "%; 1+ domains =", round(100 * shares(sample_pre)[["p1plus"]], 1), "%\n\n")
print(A7_072_domain_quantile_check$results %>%
        select(sample, rule, prob_severe, prob_moderate, cut_severe, cut_moderate,
               rounded_severe, rounded_moderate, reproduces) %>%
        mutate(across(where(is.double), ~ round(.x, 3))), n = Inf, width = Inf)
cat("\nPre-specified test (R1 or R2 reproduces T < 37 and T < 48):",
    if (success) "PASS" else "FAIL", "\n")
