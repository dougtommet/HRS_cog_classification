#!/usr/bin/env Rscript
# PMM_106_cross_validation.R
#
# Side script (not run by Analysis3_Driver.R). Run from the project root after
# a completed Analysis 3 run:
#   Rscript R/PMM_106_cross_validation.R
#
# Purpose (2026-10-22): 10-fold cross-validation of 2 versions of the cognition
# model with a general cognitive factor (gcp).
#   current  PMM_102 as of 2026-10-22: loadings, item intercepts, thresholds,
#            and residual variances equal across classes; gcp mean 0 and
#            variance 1 in Normal, free in MCI and Dementia; PD102 thresholds
#            by class.
#   change1  As current, but item intercepts and all thresholds (8 cognitive
#            items and PD102) free in each class, and the gcp mean fixed at 0
#            in every class. Loadings stay equal across classes; gcp variance
#            stays 1 in Normal and free in MCI and Dementia.
#
# Decision rule (fixed 2026-10-22, before any cross-validated result was seen):
#   Adopt change1 only if, in the pooled held-out predictions,
#     (a) its 3-class kappa exceeds the current model's by at least 0.02, and
#     (b) its dementia kappa is not lower, and
#     (c) its log score is not lower.
#   Otherwise keep the current model.
#   Kappas use probability allocation (each person counts in each class in
#   proportion to the predicted probabilities) and HCAP16WGTR; the 3-class
#   kappa is quadratic weighted, as in PMM_110. The log score is the weighted
#   mean of log(predicted probability of the person's HRS/HCAP Algorithm class).
#
# Design
#   Sample: HRS/HCAP participants with at least 1 cognition-model indicator
#     (inhcap_cog; the participants the cognition model scores). The informant
#     (Jorm) model is the same for both versions and is not part of this test.
#   Folds: 10, stratified on the HRS/HCAP Algorithm class, seed 20261022.
#   In each fold: fit the known-class calibration model on the 9 training
#     folds (weights, strata, clusters as in PMM_102); fix the class logits at
#     the weighted class shares of the training folds; score the held-out
#     fold with every parameter fixed. Scoring uses no weights or design
#     variables because posterior probabilities with fixed parameters do not
#     depend on them.
#   Fixed across folds: the covariate (norms) coefficients from PMM_100, which
#     come from the robust norms group and do not use the class labels.
#   Also reported: the same measures in-sample (fit and scored on the full
#     sample), and per-fold measures (description only).
#
# Reads:  R_objects/PMM_100.RDS, R_objects/PMM_100_mplus_mod_norms.RDS
# Writes: mplus_output/pmm_106/ (one folder per model and fold)
#         R_objects/PMM_106_cross_validation.RDS (includes held-out
#           probabilities by id; local derived data)
#         Reports/PMM_106_cross_validation_[date].csv (summary statistics only)

setwd(here::here())
source(here::here("R", "A1_001-libraries.R"))
source(here::here("R", "PMM_101_mplus_function.R"))

cv_seed <- 20261022
n_folds <- 10
cv_starts <- "10 2"
out_root <- here::here("mplus_output", "pmm_106")
fs::dir_create(out_root)

read_mplus_model_quietly <- function(model_path) {
  suppressWarnings(MplusAutomation::readModels(model_path))
}

output_flags <- function(out_path) {
  out <- readLines(out_path, warn = FALSE)
  patterns <- c(
    not_terminated = "DID NOT TERMINATE NORMALLY",
    not_pos_def = "NOT POSITIVE DEFINITE",
    se_untrustworthy = "MAY NOT BE TRUSTWORTHY",
    not_replicated = "NOT REPLICATED",
    not_identified = "NOT BE IDENTIFIED"
  )
  purrr::map_lgl(patterns, ~ any(stringr::str_detect(out, stringr::fixed(.x))))
}

# ---------------------------------------------------------------------------
# Data and folds

PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))
mplus_mod_norms <- readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS"))

inhcap_cog <- PMM_100 %>%
  filter(inHCAP == 1) %>%
  mutate(id = as.numeric(id)) %>%
  filter(if_any(all_of(pmm_cog_indicators), ~ !is.na(.x)))

if (anyNA(inhcap_cog$vs1hcapdxeap) || anyNA(inhcap_cog$HCAP16WGTR)) {
  stop("Missing HRS/HCAP Algorithm class or weight in the cognition-model sample.")
}

set.seed(cv_seed)
inhcap_cog <- inhcap_cog %>%
  group_by(vs1hcapdxeap) %>%
  mutate(fold = sample(rep_len(seq_len(n_folds), dplyr::n()))) %>%
  ungroup()

# The model names every threshold; each training set must contain every
# category of every ordinal item.
ordinal_items <- c("vdori", "vdlfl2", "vdlfl3", "vdsevens", "vdcount", "PD102")
n_categories <- function(d) {
  purrr::map_int(ordinal_items, ~ dplyr::n_distinct(stats::na.omit(d[[.x]])))
}
full_categories <- n_categories(inhcap_cog)
expected_categories <- c(vdori = 5, vdlfl2 = 3, vdlfl3 = 3, vdsevens = 6, vdcount = 2, PD102 = 3)
if (!all(full_categories == expected_categories)) {
  stop("Category counts differ from the thresholds named in the model: ",
       paste(ordinal_items, full_categories, collapse = ", "))
}
for (f in seq_len(n_folds)) {
  if (!all(n_categories(dplyr::filter(inhcap_cog, fold != f)) == full_categories)) {
    stop("Training set for fold ", f, " is missing a category of an ordinal item.")
  }
}

# ---------------------------------------------------------------------------
# Mplus specification

common <- "gcp by vdori* vdlfl1z vdlfl2 vdlfl3 vdwddelz vdexf7z vdsevens vdcount;
  gcp@1;
  vdwddelz with vdlfl1z@0 vdexf7z@0;
  vdexf7z with vdlfl1z@0;"

item_means <- "[vdori$1 vdori$2 vdori$3 vdori$4];
   [vdlfl2$1 vdlfl2$2 vdlfl3$1 vdlfl3$2];
   [vdsevens$1 vdsevens$2 vdsevens$3 vdsevens$4 vdsevens$5];
   [vdcount$1 PD102$1 PD102$2];
   [vdlfl1z vdwddelz vdexf7z];"

class_part <- list(
  current = "%cg#1%
   [gcp@0];
   %cg#2%
   [gcp PD102$1 PD102$2];
   gcp;
   %cg#3%
   [gcp PD102$1 PD102$2];
   gcp;",
  change1 = stringr::str_c("%cg#1%
   [gcp@0];
   %cg#2%
   [gcp@0];
   gcp;
   ", item_means, "
   %cg#3%
   [gcp@0];
   gcp;
   ", item_means)
)

model_text <- function(variant) {
  stringr::str_c("%OVERALL%\n", mplus_mod_norms, "\n  ", common, "\n   ", class_part[[variant]])
}

mplus_title <- "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years"

use_items <- c("vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
               "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
               "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
               "x1x4", "x1x5", "x1x6", "x1x7",
               "x2x4", "x2x5", "x2x6", "x2x7",
               "x3x4", "x3x5", "x3x6", "x3x7",
               "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
               "PD102")

variable_cal <- "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
idvariable = id;
classes = cg (3);
knownclass = cg  (vs1hcapdxeap = 1 vs1hcapdxeap = 2 vs1hcapdxeap = 3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

variable_score <- "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
idvariable = id;
classes = c (3);"

analysis_cal <- stringr::str_c("estimator = mlr;
ALGORITHM=INTEGRATION;
starts = ", cv_starts, ";
processors = 12;
TYPE = complex mixture;")

analysis_score <- "estimator = mlr;
ALGORITHM=INTEGRATION;
starts = 0;
processors = 12;
TYPE = mixture;"

run_mplus <- function(obj, dir, stem) {
  fs::dir_create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  MplusAutomation::mplusModeler(obj, modelout = stringr::str_c(stem, ".inp"),
                                run = 1, writeData = "always")
  file.path(dir, stringr::str_c(stem, ".out"))
}

# Mplus infers the categories of an ordinal item from the data it reads. A
# held-out fold that lacks a category the model names (e.g. VDLFL2$2) fails
# with "Unknown threshold value". For each missing category add one placeholder
# row (negative id, covariates copied from the first row, only that item
# observed). With all parameters fixed, a placeholder does not change any other
# participant's posterior probabilities; the rows are dropped after scoring.
add_placeholder_rows <- function(test, train) {
  item_cols <- c("vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwddelz", "vdexf7z",
                 "vdsevens", "vdcount", "PD102")
  placeholders <- list()
  for (item in ordinal_items) {
    missing_values <- setdiff(unique(stats::na.omit(as.numeric(train[[item]]))),
                              unique(stats::na.omit(as.numeric(test[[item]]))))
    for (value in missing_values) {
      row <- test[1, ]
      row[item_cols] <- NA
      row[[item]] <- value
      row$id <- -(length(placeholders) + 1)
      placeholders[[length(placeholders) + 1]] <- row
    }
  }
  dplyr::bind_rows(test, placeholders)
}

# Fits the calibration model on `train` and scores `test` with all
# parameters fixed. Returns the predictions and a one-row run summary.
fit_and_score <- function(variant, label, train, test) {
  dir <- file.path(out_root, variant, label)

  cal_obj <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = model_text(variant),
    VARIABLE = variable_cal,
    ANALYSIS = analysis_cal,
    OUTPUT = "standardized;",
    SAVEDATA = "H5RESULTS = calibration.h5;",
    usevariables = c("id", "vs1hcapdxeap", "HCAP16WGTR", "STRATUM", "SECU_mplus", use_items),
    rdata = train
  )
  cal_path <- run_mplus(cal_obj, dir, "cal")
  cal <- read_mplus_model_quietly(cal_path)
  cal_flags <- output_flags(cal_path)
  ok <- !is.null(cal[["summaries"]][["LL"]]) && !any(cal_flags)

  run <- tibble::tibble(
    variant = variant, label = label,
    n_train = nrow(train), n_test = nrow(test),
    terminated_normally = ok,
    loglikelihood = if (ok) cal[["summaries"]][["LL"]] else NA_real_,
    free_parameters = if (ok) cal[["summaries"]][["Parameters"]] else NA_real_,
    bic = if (ok) cal[["summaries"]][["BIC"]] else NA_real_
  ) |>
    dplyr::bind_cols(tibble::as_tibble_row(rlang::set_names(cal_flags, stringr::str_c("cal_", names(cal_flags)))))

  if (!ok) {
    return(list(pred = NULL, run = run))
  }
  check_prior_sample(train, cal, stringr::str_c(variant, " ", label))

  score_model <- write_lca_model(file.path(dir, "calibration.h5"), mplus_mod_norms)
  score_model <- sub("%OVERALL%",
                     stringr::str_c("%OVERALL% \n ",
                                    weighted_class_logits(train, "vs1hcapdxeap", "HCAP16WGTR")),
                     score_model, fixed = TRUE)

  score_obj <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = score_model,
    VARIABLE = variable_score,
    ANALYSIS = analysis_score,
    SAVEDATA = "SAVE = CPROBABILITIES; file = cprob.dat; FORMAT = F20.12;",
    usevariables = c("id", use_items),
    rdata = add_placeholder_rows(test, train)
  )
  score_path <- run_mplus(score_obj, dir, "score")

  pred <- read_mplus_model_quietly(score_path)[["savedata"]] |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::transmute(id = as.numeric(id), p1 = cprob1, p2 = cprob2, p3 = cprob3) |>
    dplyr::filter(id > 0)

  if (nrow(pred) != nrow(test)) {
    stop(variant, " ", label, ": scored ", nrow(pred), " of ", nrow(test), " participants.")
  }

  list(pred = dplyr::mutate(pred, variant = variant, label = label),
       run = dplyr::mutate(run, score_flags_any = any(output_flags(score_path))))
}

# ---------------------------------------------------------------------------
# Run: 10 folds and the full sample, for each version

variants <- c("current", "change1")
jobs <- tidyr::expand_grid(variant = variants, fold = c(seq_len(n_folds), 0L))

results <- purrr::pmap(jobs, function(variant, fold) {
  if (fold == 0L) {
    message("Fitting ", variant, ": full sample (in-sample)")
    fit_and_score(variant, "full_sample", inhcap_cog, inhcap_cog)
  } else {
    message("Fitting ", variant, ": fold ", fold, " of ", n_folds)
    fit_and_score(variant, sprintf("fold_%02d", fold),
                  dplyr::filter(inhcap_cog, fold != !!fold),
                  dplyr::filter(inhcap_cog, fold == !!fold))
  }
})

runs <- purrr::map_dfr(results, "run")
predictions <- purrr::map_dfr(results, "pred") |>
  dplyr::left_join(
    inhcap_cog |>
      dplyr::transmute(id, fold,
                       algorithmic = as.integer(haven::zap_labels(vs1hcapdxeap)),
                       w = HCAP16WGTR),
    by = "id"
  )

# ---------------------------------------------------------------------------
# Measures

weighted_kappa_from_tab <- function(obs) {   # as in PMM_110, quadratic weights
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  wts <- outer(seq_len(nrow(obs)), seq_len(ncol(obs)),
               function(i, j) 1 - ((i - j)^2 / (nrow(obs) - 1)^2))
  po <- sum(wts * obs) / sum(obs)
  pe <- sum(wts * expected) / sum(obs)
  (po - pe) / (1 - pe)
}

binary_kappa_from_tab <- function(obs) {     # as in PMM_110
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  po <- sum(diag(obs)) / sum(obs)
  pe <- sum(diag(expected)) / sum(obs)
  (po - pe) / (1 - pe)
}

measures <- function(d) {
  P <- as.matrix(d[, c("p1", "p2", "p3")])
  truth <- factor(d$algorithmic, levels = 1:3)
  w <- d$w

  # Probability allocation: expected weighted cross-table (rows: HRS/HCAP
  # Algorithm; columns: PMM).
  tab_prob <- sapply(1:3, function(k) {
    x <- tapply(w * P[, k], truth, sum)
    x[is.na(x)] <- 0
    x
  })
  dem_true <- factor(d$algorithmic == 3L, levels = c(FALSE, TRUE))
  tab_prob_dem <- sapply(list(1 - P[, 3], P[, 3]), function(p) {
    x <- tapply(w * p, dem_true, sum)
    x[is.na(x)] <- 0
    x
  })

  modal <- max.col(P, ties.method = "first")
  tab_modal <- unclass(xtabs(w ~ truth + factor(modal, levels = 1:3)))
  tab_modal_dem <- unclass(xtabs(w ~ dem_true + factor(modal == 3L, levels = c(FALSE, TRUE))))

  p_true <- P[cbind(seq_len(nrow(P)), d$algorithmic)]

  tibble::tibble(
    n = nrow(d),
    kappa_3class = weighted_kappa_from_tab(tab_prob),
    kappa_dementia = binary_kappa_from_tab(tab_prob_dem),
    log_score = sum(w * log(pmax(p_true, 1e-12))) / sum(w),
    kappa_3class_modal = weighted_kappa_from_tab(tab_modal),
    kappa_dementia_modal = binary_kappa_from_tab(tab_modal_dem),
    pct_normal_prob = 100 * sum(w * P[, 1]) / sum(w),
    pct_mci_prob = 100 * sum(w * P[, 2]) / sum(w),
    pct_dementia_prob = 100 * sum(w * P[, 3]) / sum(w),
    pct_normal_modal = 100 * sum(w[modal == 1L]) / sum(w),
    pct_mci_modal = 100 * sum(w[modal == 2L]) / sum(w),
    pct_dementia_modal = 100 * sum(w[modal == 3L]) / sum(w)
  )
}

reference_shares <- inhcap_cog |>
  dplyr::group_by(algorithmic = as.integer(haven::zap_labels(vs1hcapdxeap))) |>
  dplyr::summarise(w = sum(HCAP16WGTR), .groups = "drop") |>
  dplyr::mutate(pct = 100 * w / sum(w))

all_folds_ok <- runs |>
  dplyr::filter(label != "full_sample") |>
  dplyr::pull(terminated_normally) |>
  all()

held_out <- predictions |> dplyr::filter(label != "full_sample")
in_sample <- predictions |> dplyr::filter(label == "full_sample")

summary_cv <- held_out |>
  dplyr::group_by(variant) |>
  dplyr::group_modify(~ measures(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(sample = "cross-validated (pooled held-out folds)")

summary_in <- in_sample |>
  dplyr::group_by(variant) |>
  dplyr::group_modify(~ measures(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(sample = "in-sample (full sample)")

summary_all <- dplyr::bind_rows(summary_cv, summary_in) |>
  dplyr::relocate(sample, variant)

fold_measures <- held_out |>
  dplyr::group_by(variant, fold) |>
  dplyr::group_modify(~ measures(.x)) |>
  dplyr::ungroup()

fold_differences <- fold_measures |>
  dplyr::select(variant, fold, kappa_3class, kappa_dementia, log_score) |>
  tidyr::pivot_wider(names_from = variant, values_from = c(kappa_3class, kappa_dementia, log_score)) |>
  dplyr::transmute(
    fold,
    diff_kappa_3class = kappa_3class_change1 - kappa_3class_current,
    diff_kappa_dementia = kappa_dementia_change1 - kappa_dementia_current,
    diff_log_score = log_score_change1 - log_score_current
  )

# ---------------------------------------------------------------------------
# Decision rule

cv_value <- function(variant_name, measure) {
  summary_cv |> dplyr::filter(variant == variant_name) |> dplyr::pull(!!measure)
}

complete_cv <- all_folds_ok && all(variants %in% summary_cv$variant)
decision <- if (complete_cv) {
  d3 <- cv_value("change1", "kappa_3class") - cv_value("current", "kappa_3class")
  dd <- cv_value("change1", "kappa_dementia") - cv_value("current", "kappa_dementia")
  dl <- cv_value("change1", "log_score") - cv_value("current", "log_score")
  tibble::tibble(
    diff_kappa_3class = d3,
    diff_kappa_dementia = dd,
    diff_log_score = dl,
    criterion_a = d3 >= 0.02,
    criterion_b = dd >= 0,
    criterion_c = dl >= 0,
    adopt_change1 = d3 >= 0.02 && dd >= 0 && dl >= 0
  )
} else {
  tibble::tibble(adopt_change1 = NA,
                 note = "Rule not evaluated: at least 1 fold did not terminate normally.")
}

# ---------------------------------------------------------------------------
# Save and print

spec <- list(seed = cv_seed, n_folds = n_folds, starts = cv_starts,
             rule = "Adopt change1 if CV 3-class kappa gain >= 0.02 and CV dementia kappa and log score not lower.",
             rule_fixed = as.Date("2026-10-22"),
             models = class_part, common = common)

saveRDS(list(spec = spec,
             folds = dplyr::select(inhcap_cog, id, fold),
             runs = runs,
             predictions = predictions,
             summary = summary_all,
             fold_measures = fold_measures,
             fold_differences = fold_differences,
             reference_shares = reference_shares,
             decision = decision,
             run_date = Sys.Date()),
        here::here("R_objects", "PMM_106_cross_validation.RDS"))

csv_path <- here::here("Reports", stringr::str_c("PMM_106_cross_validation_", Sys.Date(), ".csv"))
readr::write_csv(summary_all, csv_path)

options(width = 200)
round_df <- function(d) as.data.frame(dplyr::mutate(d, dplyr::across(where(is.numeric), ~ round(.x, 3))))

cat("\nRuns (calibration models):\n")
print(round_df(dplyr::select(runs, variant, label, n_train, terminated_normally,
                             loglikelihood, free_parameters, bic, dplyr::starts_with("cal_"))))
cat("\nHRS/HCAP Algorithm weighted class shares in this sample (%):\n")
print(round_df(reference_shares))
cat("\nSummary (survey-weighted; kappas by probability allocation unless marked modal):\n")
print(round_df(summary_all))
cat("\nPer-fold differences, change1 minus current (description only):\n")
print(round_df(fold_differences))
cat("\nDecision (rule fixed 2026-10-22):\n")
print(round_df(decision))
cat("Wrote ", csv_path, "\n", sep = "")
