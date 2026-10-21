# Analysis A7: Core algorithm T-score cut points and a calibration check.
#
# Algorithm cut points: T < 37 (severe) and T < 48 (moderate), first in A7_075
# at commit 924beb6 (2026-04-17). How they were chosen is not documented; the
# April report shows their weighted class shares beside the Manly-Jones shares
# (Reports/A7_000-Main_control_2026-04-17.html), and the domain-share rules
# tested in A7_072 and A7_073 do not reproduce them (References/SESSION_LOGS.md).
# These are the cut points in the file shared with Lindsay Kobayashi
# (2026-07-10). Decision (Rich, 2026-10-19): keep them.
#
# Domain-share quantiles (added 2026-10-20 for Appendix 1, Step 3): weighted
# share of HRS-HCAP Wave 1 participants with 2+ and 1+ impaired HCAP domains
# (Stata/w073_hcapdx.dta: n21 = 2+, n22 = exactly 1) and the weighted T
# quantiles at those shares. Descriptive only: they show where 37 and 48 sit
# relative to the domain counts; they do not set the cut points.
#
# Calibration check: a grid search over severe and moderate cut points, with
# the self-rated memory rule and the IQCODE fallback held fixed, finds the pair
# whose weighted (HCAP16WGTR) shares are closest to the Manly-Jones shares
# (sum of squared differences; ties go to the higher weighted 3-class kappa).
# The check reports where the algorithm cut points sit on that grid. It does
# not set the algorithm's cut points.
# Caveat: shares and kappas are computed in the same HCAP sample in which the
# grid optimum is found, so they are in-sample.

source(here::here("R", "A7_060-algorithm_functions.R"))

hrs16_merged <- readRDS(here::here("R_objects", "A7_055_hrs16_merged.rds")) %>%
  mutate(jorm = rPjorm,
         D102 = PD102_imp)

calib_sample <- hrs16_merged %>%
  filter(inHCAP == 1, !is.na(HCAP16WGTR), !is.na(vs1hcapdxeap)) %>%
  mutate(target = as.integer(haven::zap_labels(vs1hcapdxeap)))

target_shares <- weighted_shares(calib_sample$target, calib_sample$HCAP16WGTR)

grid_step <- 0.5
cut_grid <- tidyr::expand_grid(
  cut_severe   = seq(25, 50, by = grid_step),
  cut_moderate = seq(30, 60, by = grid_step)
) %>%
  filter(cut_moderate > cut_severe)

evaluate_cuts <- function(cut_severe, cut_moderate) {
  d <- classify_core(calib_sample, cut_severe, cut_moderate) %>%
    mutate(dx = as.integer(haven::zap_labels(dx_v1)))

  sh <- weighted_shares(d$dx, d$HCAP16WGTR)

  tab3 <- xtabs(HCAP16WGTR ~ factor(target, levels = 1:3) + factor(dx, levels = 1:3),
                data = d)
  tab2 <- xtabs(HCAP16WGTR ~ factor(target == 3, levels = c(FALSE, TRUE)) +
                  factor(dx == 3, levels = c(FALSE, TRUE)),
                data = d)

  tibble::tibble(
    cut_severe     = cut_severe,
    cut_moderate   = cut_moderate,
    share_normal   = sh[["1"]],
    share_mci      = sh[["2"]],
    share_dementia = sh[["3"]],
    loss           = sum((sh - target_shares)^2),
    kappa_3class   = weighted_kappa_from_tab(tab3)$quadratic_weighted_kappa,
    kappa_binary   = binary_kappa_from_tab(tab2)$kappa,
    n_unclassified = sum(is.na(d$dx))
  )
}

calibration_grid <- purrr::pmap_dfr(cut_grid, evaluate_cuts)

calibration_chosen <- calibration_grid %>%
  arrange(loss, desc(kappa_3class)) %>%
  slice(1)

# Algorithm cut points (see header) and their position on the grid.
algorithm_cuts <- c(severe = 37, moderate = 48)
calibration_algorithm <- evaluate_cuts(algorithm_cuts[["severe"]],
                                       algorithm_cuts[["moderate"]])
grid_position <- list(
  n_pairs        = nrow(calibration_grid),
  loss_algorithm = calibration_algorithm$loss,
  loss_minimum   = calibration_chosen$loss,
  n_pairs_lower_loss = sum(calibration_grid$loss < calibration_algorithm$loss),
  rank           = sum(calibration_grid$loss < calibration_algorithm$loss) + 1L,
  max_abs_share_diff_algorithm = max(abs(c(calibration_algorithm$share_normal,
                                           calibration_algorithm$share_mci,
                                           calibration_algorithm$share_dementia) - target_shares)),
  max_abs_share_diff_calibrated = max(abs(c(calibration_chosen$share_normal,
                                            calibration_chosen$share_mci,
                                            calibration_chosen$share_dementia) - target_shares))
)

# Kappa-maximizing pair, reported for comparison only.
calibration_max_kappa <- calibration_grid %>%
  arrange(desc(kappa_3class)) %>%
  slice(1)

# Domain-share quantiles (descriptive; see header) ----------------------------
dq_key <- function(h, p) stringr::str_c(sprintf("%06d", as.integer(as.character(h))),
                                        sprintf("%03d", as.integer(as.character(p))))
dq_wquantile <- function(x, w, probs) {
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}
dq_domains <- haven::read_dta(here::here("Stata", "w073_hcapdx.dta")) %>%
  rename_with(tolower, any_of(c("HHID", "PN"))) %>%
  transmute(k = dq_key(hhid, pn),
            n21 = as.numeric(haven::zap_labels(n21)),
            n22 = as.numeric(haven::zap_labels(n22)))
dq_sample <- hrs16_merged %>%
  transmute(k = dq_key(HHID, PN),
            inHCAP = as.numeric(haven::zap_labels(inHCAP)),
            TF = as.numeric(haven::zap_labels(TF)),
            w = as.numeric(haven::zap_labels(HCAP16WGTR))) %>%
  filter(inHCAP == 1, !is.na(TF), !is.na(w), w > 0) %>%
  inner_join(dq_domains, by = "k") %>%
  # Same coding as A7_072 and A1_029: n21 == 1 is 2+ domains whatever n22
  # holds (n22 is missing when n21 == 1, so it must not be used as a filter).
  mutate(n_impaired = case_when(n21 == 1             ~ 2L,
                                n21 == 0 & n22 == 1 ~ 1L,
                                n21 == 0 & n22 == 0 ~ 0L)) %>%
  filter(!is.na(n_impaired))
dq_p2     <- sum(dq_sample$w[dq_sample$n_impaired == 2]) / sum(dq_sample$w)
dq_p1plus <- sum(dq_sample$w[dq_sample$n_impaired >= 1]) / sum(dq_sample$w)
# Guard (2026-10-21): an earlier version dropped the 2+ group and printed a
# 0% share. A7_072 found about 14% (2+) and 34% (1+); stop if far off.
if (dq_p2 < 0.05 || dq_p2 > 0.30 || dq_p1plus < 0.15 || dq_p1plus > 0.60 ||
    nrow(dq_sample) < 2900) {
  stop("A7_070 domain_quantiles: implausible shares or n (2+ = ", round(100 * dq_p2, 1),
       "%, 1+ = ", round(100 * dq_p1plus, 1), "%, n = ", nrow(dq_sample),
       "). Check the w073 coding.")
}
domain_quantiles <- list(
  n = nrow(dq_sample),
  share_2plus = dq_p2,
  share_1plus = dq_p1plus,
  t_2plus = dq_wquantile(dq_sample$TF, dq_sample$w, dq_p2),
  t_1plus = dq_wquantile(dq_sample$TF, dq_sample$w, dq_p1plus),
  share_2plus_unweighted = mean(dq_sample$n_impaired == 2),
  share_1plus_unweighted = mean(dq_sample$n_impaired >= 1),
  t_2plus_unweighted = unname(stats::quantile(dq_sample$TF, mean(dq_sample$n_impaired == 2))),
  t_1plus_unweighted = unname(stats::quantile(dq_sample$TF, mean(dq_sample$n_impaired >= 1))),
  weight = "HCAP16WGTR"
)

A7_070_threshold_calibration <- list(
  domain_quantiles = domain_quantiles,  # descriptive, Appendix 1 Step 3
  cuts = algorithm_cuts,                 # used by A7_075: the algorithm
  algorithm = calibration_algorithm,     # shares and kappas at the algorithm cuts
  calibrated_cuts = c(severe   = calibration_chosen$cut_severe,
                      moderate = calibration_chosen$cut_moderate),
  calibrated = calibration_chosen,       # grid optimum (check only)
  grid_position = grid_position,
  jorm_cuts = c(mild = 3.0, severe = 3.4),
  target_shares = target_shares,
  max_kappa = calibration_max_kappa,
  grid = calibration_grid,
  grid_step = grid_step,
  n_sample = nrow(calib_sample),
  weight = "HCAP16WGTR",
  target_variable = "vs1hcapdxeap",
  date = Sys.Date()
)

saveRDS(A7_070_threshold_calibration,
        here::here("R_objects", "A7_070_threshold_calibration.rds"))
