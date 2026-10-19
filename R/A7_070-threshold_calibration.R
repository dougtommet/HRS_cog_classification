# Analysis A7: calibrate the Core algorithm T-score cut points.
#
# Target: the survey-weighted (HCAP16WGTR) Normal/MCI/Dementia shares of the
# HRS/HCAP Algorithm (Manly et al., 2022; vs1hcapdxeap) in HCAP 2016.
# Method: grid search over the severe and moderate T-score cut points, with the
# self-rated memory rule and the Jorm IQCODE fallback held fixed. The chosen
# pair minimizes the sum of squared differences between the Core and target
# shares; ties go to the higher weighted 3-class kappa.
# Caveat: cut points are chosen and evaluated in the same HCAP sample, so
# agreement statistics computed at the chosen cuts are in-sample.

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

# Cut points used before this calibration (A7_075 through 2026-10-07).
previous_cuts <- c(severe = 37, moderate = 48)
calibration_previous <- evaluate_cuts(previous_cuts[["severe"]],
                                      previous_cuts[["moderate"]])

# Kappa-maximizing pair, reported for comparison only.
calibration_max_kappa <- calibration_grid %>%
  arrange(desc(kappa_3class)) %>%
  slice(1)

A7_070_threshold_calibration <- list(
  cuts = c(severe   = calibration_chosen$cut_severe,
           moderate = calibration_chosen$cut_moderate),
  jorm_cuts = c(mild = 3.0, severe = 3.4),
  target_shares = target_shares,
  chosen = calibration_chosen,
  previous_cuts = previous_cuts,
  previous = calibration_previous,
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
