# Analysis A7: Core algorithm T-score cut points, selected by a rule fixed
# before results (Rich Jones, with Doug Tommet and Lindsay Kobayashi,
# 2026-10-21). Replaces T < 37 and T < 48, whose origin is undocumented
# (Doug Tommet did not recall; References/SESSION_LOGS.md, 2026-10-19 to
# 2026-10-21). The earlier grid search is archived in
# R/Archive_grid_search_2026-10-20/.
#
# PRE-SPECIFICATION (written and approved 2026-10-21, before any result)
# Score: TR = floor(TF + 0.5), the T score rounded to an integer (A7_055).
#   TR is used to select the cut points and to classify in every wave (A7_075).
# Fixed: self-rated memory gate (D102, imputed), IQCODE fallback cut points
#   (3.0, 3.4), the CFA model, and the norms.
# Sample: HRS-HCAP Wave 1 participants with HCAP16WGTR and Manly-Jones
#   vs1hcapdxeap (the reference standard). All shares and kappas are weighted
#   by HCAP16WGTR. Shares are among classified participants.
# Candidates: integer pairs, severe cut 25 to 50, moderate cut 30 to 60,
#   moderate > severe. A participant is below a cut c if TR < c.
# Eligibility: the weighted dementia share is within 0.5 percentage points
#   of the Manly-Jones dementia share, and the MCI share is within 1.0 point
#   of the Manly-Jones MCI share.
# Widening: if no pair is eligible, widen the dementia tolerance to 1.0
#   point with the MCI tolerance at 1.0. The dementia tolerance never exceeds
#   1.0 point (Rich, 2026-10-21). If no pair is eligible, return the dementia
#   tolerance to the smallest value (0.5 or 1.0) at which any pair meets the
#   dementia criterion and widen the MCI tolerance by 0.5 points (1.5, 2.0,
#   ...) until a pair is eligible. If no pair meets the dementia criterion at
#   1.0 point, stop: the rule cannot be met. Report the tolerances used.
# Selection: among eligible pairs, choose the highest weighted binary kappa
#   for dementia (Manly-Jones dementia vs not). Ties (pairs that share a
#   severe cut have the same dementia kappa) go to the higher weighted
#   quadratic 3-class kappa, then the smaller MCI share difference, then the
#   lower moderate cut.
# In-sample: the cut points are selected and evaluated in the same sample,
#   so agreement reported for this sample is optimistic.
# Stability (description only; it does not affect the selection): the whole
#   rule, including the Manly-Jones target shares, is repeated in design-based
#   bootstrap samples (SECU resampled within STRATUM, Rao-Wu with n_h - 1
#   draws; 500 replicates; seed 20261021). Report how often each pair is
#   selected.
#
# Also kept (descriptive, Appendix 1 record): domain-share quantiles, the
# weighted share of HRS-HCAP Wave 1 participants with 2+ and 1+ impaired HCAP
# domains (Stata/w073_hcapdx.dta) and the weighted T quantiles at those shares.

source(here::here("R", "A7_060-algorithm_functions.R"))

hrs16_merged <- readRDS(here::here("R_objects", "A7_055_hrs16_merged.rds")) %>%
  mutate(jorm = rPjorm,
         D102 = PD102_imp)

calib_sample <- hrs16_merged %>%
  filter(inHCAP == 1, !is.na(HCAP16WGTR), !is.na(vs1hcapdxeap)) %>%
  mutate(target = as.integer(haven::zap_labels(vs1hcapdxeap)))

# Pre-specified constants ------------------------------------------------------
sel_spec <- list(
  severe_range   = 25:50,
  moderate_range = 30:60,
  tol_dementia   = 0.005,
  tol_mci        = 0.010,
  tol_step       = 0.005,
  tol_dementia_max = 0.010,   # never exceeded (Rich, 2026-10-21)
  n_boot = 500,
  seed   = 20261021
)

# Fast evaluator ---------------------------------------------------------------
# Applies the same rules as classify_core() (A7_060) to plain vectors so the
# rule can be repeated in bootstrap samples. Checked against classify_core()
# below; A7 stops if they differ.
d102_worse    <- c(-8, 3, 8, 9)
d102_notworse <- c(1, 2)

prep_vectors <- function(d) {
  jorm <- d$jorm
  list(
    TR = d$TR,
    worse = ifelse(d$D102 %in% d102_worse, TRUE,
                   ifelse(d$D102 %in% d102_notworse, FALSE, NA)),
    jdx = dplyr::case_when(jorm >= 3.4 ~ 3L,
                           jorm >  3.0 & jorm < 3.4 ~ 2L,
                           jorm <= 3.0 ~ 1L),
    target = d$target,
    w = d$HCAP16WGTR
  )
}

fast_dx <- function(v, s, m) {
  dx <- rep(NA_integer_, length(v$TR))
  has <- !is.na(v$TR)
  sev <- has & v$TR < s
  mod <- has & v$TR >= s & v$TR < m
  dx[sev & v$worse %in% TRUE]  <- 3L
  dx[sev & v$worse %in% FALSE] <- 2L
  dx[mod & v$worse %in% TRUE]  <- 2L
  dx[mod & v$worse %in% FALSE] <- 1L
  dx[has & v$TR >= m] <- 1L
  dx[!has] <- v$jdx[!has]
  dx
}

wtab3 <- function(target, dx, w) {
  ok <- !is.na(dx) & !is.na(target) & !is.na(w)
  t <- tapply(w[ok], list(factor(target[ok], levels = 1:3),
                          factor(dx[ok], levels = 1:3)), sum)
  t[is.na(t)] <- 0
  t
}

# Collapse to unique (TR, memory, IQCODE class, target) patterns with summed
# weights. Shares and weighted kappas depend only on these sums, so results
# are identical and evaluation is much faster.
collapse_vectors <- function(v) {
  g <- paste(v$TR, v$worse, v$jdx, v$target, sep = "|")   # NA kept as "NA"
  first <- !duplicated(g)
  ws <- tapply(v$w, g, sum)
  list(TR = v$TR[first], worse = v$worse[first], jdx = v$jdx[first],
       target = v$target[first], w = as.numeric(ws[g[first]]))
}

# Plain-number kappas, same formulas as weighted_kappa_from_tab() and
# binary_kappa_from_tab() in A7_060 (checked against them below).
kappa_quad <- function(t) {
  k <- nrow(t); wq <- 1 - outer(seq_len(k), seq_len(k), function(i, j) (i - j)^2) / (k - 1)^2
  n <- sum(t); e <- outer(rowSums(t), colSums(t)) / n
  po <- sum(wq * t) / n; pe <- sum(wq * e) / n
  (po - pe) / (1 - pe)
}
kappa_bin <- function(t) {
  n <- sum(t); e <- outer(rowSums(t), colSums(t)) / n
  po <- sum(diag(t)) / n; pe <- sum(diag(e)) / n
  (po - pe) / (1 - pe)
}

evaluate_pairs <- function(v, pairs) {
  ok <- !is.na(v$w)
  v <- collapse_vectors(lapply(v, function(x) x[ok]))
  ts <- weighted_shares(v$target, v$w)
  out <- lapply(seq_len(nrow(pairs)), function(i) {
    s <- pairs$cut_severe[i]; m <- pairs$cut_moderate[i]
    dx <- fast_dx(v, s, m)
    t3 <- wtab3(v$target, dx, v$w)
    sh <- colSums(t3) / sum(t3)
    t2 <- matrix(c(sum(t3[1:2, 1:2]), sum(t3[3, 1:2]),
                   sum(t3[1:2, 3]),   t3[3, 3]), 2, 2)
    c(cut_severe = s, cut_moderate = m,
      share_normal = sh[[1]], share_mci = sh[[2]], share_dementia = sh[[3]],
      kappa_binary = kappa_bin(t2),
      kappa_3class = kappa_quad(t3),
      share_unclassified = sum(v$w[is.na(dx)]) / sum(v$w))
  })
  res <- tibble::as_tibble(do.call(rbind, out))
  res %>%
    mutate(diff_dementia = share_dementia - ts[["3"]],
           diff_mci      = share_mci - ts[["2"]],
           diff_normal   = share_normal - ts[["1"]])
}

# Apply the selection rule to an evaluated candidate table.
select_cuts <- function(cand, spec = sel_spec) {
  eps <- 1e-9
  elig <- function(td, tm) cand %>%
    filter(abs(diff_dementia) <= td + eps, abs(diff_mci) <= tm + eps)

  td <- spec$tol_dementia; tm <- spec$tol_mci; stage <- "as specified"
  e <- elig(td, tm)
  while (nrow(e) == 0 && td < spec$tol_dementia_max - eps) {
    td <- td + spec$tol_step
    e <- elig(td, tm)
    stage <- "dementia tolerance widened"
  }
  if (nrow(e) == 0) {
    td <- spec$tol_dementia
    while (!any(abs(cand$diff_dementia) <= td + eps)) {
      td <- td + spec$tol_step
      if (td > spec$tol_dementia_max + eps) {
        stop("A7_070: no pair has a dementia share within ",
             100 * spec$tol_dementia_max, " percentage points of Manly-Jones; ",
             "the pre-specified rule cannot be met.", call. = FALSE)
      }
    }
    tm <- spec$tol_mci
    e <- elig(td, tm)
    while (nrow(e) == 0) {
      tm <- tm + spec$tol_step
      e <- elig(td, tm)
    }
    stage <- "MCI tolerance widened"
  }
  chosen <- e %>%
    arrange(desc(round(kappa_binary, 10)), desc(round(kappa_3class, 10)),
            abs(diff_mci), cut_moderate) %>%
    slice(1)
  list(chosen = chosen, eligible = e, tol_dementia = td, tol_mci = tm,
       stage = stage)
}

# Selection in the full sample --------------------------------------------------
cand_pairs <- tidyr::expand_grid(cut_severe   = sel_spec$severe_range,
                                 cut_moderate = sel_spec$moderate_range) %>%
  filter(cut_moderate > cut_severe)

v_full <- prep_vectors(calib_sample)
target_shares <- weighted_shares(calib_sample$target, calib_sample$HCAP16WGTR)
candidates <- evaluate_pairs(v_full, cand_pairs)
selection <- select_cuts(candidates)
selected_cuts <- c(severe   = selection$chosen$cut_severe,
                   moderate = selection$chosen$cut_moderate)

# Check: the fast evaluator matches classify_core() at the selected cuts and
# at 37/48.
for (cc in list(selected_cuts, c(severe = 37, moderate = 48))) {
  ref <- classify_core(calib_sample, cc[["severe"]], cc[["moderate"]], score = "TR")
  fx  <- fast_dx(v_full, cc[["severe"]], cc[["moderate"]])
  if (!identical(as.integer(haven::zap_labels(ref$dx_v1)), fx)) {
    stop("A7_070: fast evaluator disagrees with classify_core() at ",
         cc[["severe"]], "/", cc[["moderate"]], ".")
  }
}

# Check: plain-number kappas match the A7_060 functions at the selected cuts.
# Previous cut points (T < 37, T < 48) for comparison, on TR and on TF.
eval_one <- function(d, s, m, score) {
  x <- classify_core(d, s, m, score = score) %>%
    mutate(dx = as.integer(haven::zap_labels(dx_v1)))
  t3 <- wtab3(x$target, x$dx, x$HCAP16WGTR)
  sh <- colSums(t3) / sum(t3)
  t2 <- matrix(c(sum(t3[1:2, 1:2]), sum(t3[3, 1:2]),
                 sum(t3[1:2, 3]),   t3[3, 3]), 2, 2)
  tibble::tibble(score = score, cut_severe = s, cut_moderate = m,
                 share_normal = sh[[1]], share_mci = sh[[2]], share_dementia = sh[[3]],
                 kappa_binary = binary_kappa_from_tab(t2)$kappa,
                 kappa_3class = weighted_kappa_from_tab(t3)$quadratic_weighted_kappa)
}
previous_cuts <- c(severe = 37, moderate = 48)
comparison <- dplyr::bind_rows(
  eval_one(calib_sample, selected_cuts[["severe"]], selected_cuts[["moderate"]], "TR") %>%
    mutate(set = "Selected cut points (TR)"),
  eval_one(calib_sample, 37, 48, "TR") %>% mutate(set = "Previous cut points (TR)"),
  eval_one(calib_sample, 37, 48, "TF") %>% mutate(set = "Previous cut points (TF, July 2026 file)")
) %>%
  relocate(set)
sel_row <- comparison[comparison$set == "Selected cut points (TR)", ]
if (abs(sel_row$kappa_binary - selection$chosen$kappa_binary) > 1e-10 ||
    abs(sel_row$kappa_3class - selection$chosen$kappa_3class) > 1e-10) {
  stop("A7_070: fast kappas disagree with the A7_060 kappa functions.")
}

# Stability: design-based bootstrap (description only) --------------------------
boot <- NULL
if (all(c("STRATUM", "SECU") %in% names(calib_sample))) {
  set.seed(sel_spec$seed)
  psu <- calib_sample %>%
    distinct(STRATUM, SECU) %>%
    group_by(STRATUM) %>%
    mutate(n_h = n()) %>%
    ungroup()
  boot_rows <- vector("list", sel_spec$n_boot)
  for (b in seq_len(sel_spec$n_boot)) {
    draw <- psu %>%
      group_by(STRATUM) %>%
      reframe(SECU = if (first(n_h) > 1) sample(SECU, first(n_h) - 1, replace = TRUE) else SECU,
              mult = if (first(n_h) > 1) first(n_h) / (first(n_h) - 1) else 1) %>%
      count(STRATUM, SECU, mult, name = "times")
    db <- calib_sample %>%
      inner_join(draw, by = c("STRATUM", "SECU")) %>%
      mutate(HCAP16WGTR = HCAP16WGTR * mult * times)
    # A replicate in which no severe cut reaches the 1.0-point dementia cap
    # is recorded as "rule not met" (no pair selected).
    sb <- tryCatch(select_cuts(evaluate_pairs(prep_vectors(db), cand_pairs)),
                   error = function(e) NULL)
    boot_rows[[b]] <- if (is.null(sb)) {
      tibble::tibble(rep = b, cut_severe = NA_real_, cut_moderate = NA_real_,
                     tol_dementia = NA_real_, tol_mci = NA_real_,
                     stage = "rule not met")
    } else {
      tibble::tibble(rep = b,
                     cut_severe = sb$chosen$cut_severe,
                     cut_moderate = sb$chosen$cut_moderate,
                     tol_dementia = sb$tol_dementia,
                     tol_mci = sb$tol_mci,
                     stage = sb$stage)
    }
  }
  boot_reps <- dplyr::bind_rows(boot_rows)
  boot <- list(
    reps = boot_reps,
    pair_freq = boot_reps %>% count(cut_severe, cut_moderate, name = "n") %>%
      mutate(pct = n / sel_spec$n_boot) %>% arrange(desc(n)),
    severe_freq = boot_reps %>% count(cut_severe, name = "n") %>%
      mutate(pct = n / sel_spec$n_boot),
    moderate_freq = boot_reps %>% count(cut_moderate, name = "n") %>%
      mutate(pct = n / sel_spec$n_boot),
    # Shares are of all replicates; "rule not met" counts as not the same.
    pct_same_pair = mean(boot_reps$cut_severe %in% selected_cuts[["severe"]] &
                           boot_reps$cut_moderate %in% selected_cuts[["moderate"]]),
    pct_within_1 = mean(coalesce(abs(boot_reps$cut_severe - selected_cuts[["severe"]]) <= 1 &
                                   abs(boot_reps$cut_moderate - selected_cuts[["moderate"]]) <= 1, FALSE)),
    n_rule_not_met = sum(boot_reps$stage == "rule not met"),
    stage_freq = boot_reps %>% count(stage, name = "n"),
    n_boot = sel_spec$n_boot,
    seed = sel_spec$seed,
    design = "SECU resampled within STRATUM (Rao-Wu, n_h - 1 draws), HCAP16WGTR"
  )
} else {
  warning("A7_070: STRATUM or SECU not in A7_055_hrs16_merged; bootstrap skipped.")
}

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
  cuts = selected_cuts,                  # used by A7_075: the algorithm
  score = "TR",
  selected = selection$chosen,           # shares, differences, kappas at the cuts
  eligible = selection$eligible,
  tol_dementia_used = selection$tol_dementia,
  tol_mci_used = selection$tol_mci,
  selection_stage = selection$stage,
  spec = sel_spec,
  candidates = candidates,
  previous_cuts = previous_cuts,
  comparison = comparison,               # selected vs 37/48 on TR and TF
  bootstrap = boot,
  domain_quantiles = domain_quantiles,   # descriptive record
  jorm_cuts = c(mild = 3.0, severe = 3.4),
  target_shares = target_shares,
  n_sample = nrow(calib_sample),
  weight = "HCAP16WGTR",
  target_variable = "vs1hcapdxeap",
  date = Sys.Date()
)

saveRDS(A7_070_threshold_calibration,
        here::here("R_objects", "A7_070_threshold_calibration.rds"))
