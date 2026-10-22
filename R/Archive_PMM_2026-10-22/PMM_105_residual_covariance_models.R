#!/usr/bin/env Rscript
# PMM_105_residual_covariance_models.R
#
# Side script (not run by Analysis3_Driver.R). Run from the project root after
# a completed Analysis 3 run:
#   Rscript R/PMM_105_residual_covariance_models.R
#
# Question (2026-10-22): does letting the continuous cognitive scores covary
# within class improve recovery of the known class labels (HRS/HCAP Algorithm,
# vs1hcapdxeap), compared with the no-factor model (PMM_103, scores independent
# within class) and the general-factor model (PMM_102)?
#
# Cognition models fitted here (the informant model is pmm_103b_jorm for all):
#   105a  PMM_103 plus residual covariances among the 3 continuous scores
#         (vdlfl1z, vdwddelz, vdexf7z), estimated in HCAP and held equal
#         across classes.
#   105b  As 105a, with the 3 covariances free in each class. Fitted only if
#         105a terminated normally.
# The norms model (PMM_100) also estimates these covariances; PMM_103 fixes
# them at 0. Here they are removed from the fixed norms statements and
# estimated in the calibration model.
# The 5 categorical items and PD102 stay independent within class. Each
# covariance involving a categorical item would add a dimension of numerical
# integration.
#
# Inputs, priors, and scoring follow PMM_103: same indicators, weights, design,
# known class, and class logits fixed at the weighted class shares of the
# participants with at least 1 cognition-model indicator (inhcap_cog).
#
# Comparison (all models, same informant model):
#   - fit of the calibration model: loglikelihood, free parameters, BIC
#   - entropy of the scoring run
#   - survey-weighted kappas with the HRS/HCAP Algorithm, computed as in
#     PMM_110 (probability-proportional allocation, 100 replicates per person,
#     weight HCAP16WGTR, sample with non-missing Langa-Weir), so the PMM_103
#     row reproduces the manuscript values
#   - the same kappas from the most likely class
#   - weighted class shares (most likely class, as in PMM_112)
# All agreement statistics are in-sample.
#
# Reads:  R_objects/PMM_100.RDS, R_objects/PMM_100_mplus_mod_norms.RDS
#         mplus_output/pmm_102/, pmm_103/, pmm_103_jorm/ (.out files)
# Writes: mplus_output/pmm_105/
#         R_objects/PMM_105_comparison.RDS
#         Reports/PMM_105_model_comparison_[date].csv (summary statistics only)

setwd(here::here())
source(here::here("R", "A1_001-libraries.R"))
source(here::here("R", "PMM_101_mplus_function.R"))

out_dir <- here::here("mplus_output", "pmm_105")
fs::dir_create(out_dir)

read_mplus_model_quietly <- function(model_path) {
  suppressWarnings(MplusAutomation::readModels(model_path))
}

read_cprob <- function(model_path) {
  read_mplus_model_quietly(model_path)[["savedata"]] |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::rename(c = tidyselect::any_of("mlcc")) |>
    dplyr::transmute(id = as.integer(id), cprob1, cprob2, cprob3, c = as.integer(c))
}

# ---------------------------------------------------------------------------
# Data (same samples as PMM_103)

PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))
mplus_mod_norms <- readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS"))

inhcap <- PMM_100 %>%
  filter(inHCAP == 1) %>%
  mutate(id = as.numeric(id))

inhcap_cog <- inhcap %>%
  filter(if_any(all_of(pmm_cog_indicators), ~ !is.na(.x)))

# Norms statements without the residual covariances (WITH statements).
norm_lines <- stringr::str_split(mplus_mod_norms, "\n")[[1]]
is_with <- stringr::str_detect(stringr::str_to_lower(norm_lines), " with ")
if (sum(is_with) != 3) {
  stop("Expected 3 WITH statements in the norms model, found ", sum(is_with), ".")
}
mplus_mod_norms_nowith <- stringr::str_c(norm_lines[!is_with], collapse = "\n")

# ---------------------------------------------------------------------------
# Mplus specification (as PMM_103, plus the covariance statement)

cont_with <- "vdlfl1z with vdwddelz vdexf7z;
   vdwddelz with vdexf7z;"

class_block <- function(k, extra = "") {
  stringr::str_c(
    "   %cg#", k, "%\n",
    "   [vdori$1 vdlfl2$1 vdlfl3$1 vdsevens$1 vdcount$1\n",
    "        PD102$1 PD102$2];\n",
    "   [vdwddelz vdlfl1z vdexf7z];\n",
    extra
  )
}

mplus_title <- "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years"

use_common <- c("HCAP16WGTR", "STRATUM", "SECU_mplus",
                "vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
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

variable_fixed <- "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
idvariable = id;
classes = c (3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

analysis <- "estimator = mlr;
ALGORITHM=INTEGRATION;
starts = 80;
processors = 12;
TYPE = complex mixture;"

# Flags in the Mplus output. Only "did not terminate normally" (or no
# loglikelihood) stops the next step; the others are reported.
output_flags <- function(out_path) {
  out <- readLines(out_path, warn = FALSE)
  patterns <- c(
    not_terminated = "DID NOT TERMINATE NORMALLY",
    not_pos_def = "NOT POSITIVE DEFINITE",
    se_untrustworthy = "MAY NOT BE TRUSTWORTHY",
    not_replicated = "NOT REPLICATED"
  )
  purrr::map_lgl(patterns, ~ any(stringr::str_detect(out, stringr::fixed(.x))))
}

run_calibration <- function(stem, by_class) {
  extra_1 <- if (by_class) stringr::str_c("   %cg#1%\n   ", cont_with, "\n") else ""
  extra_k <- if (by_class) stringr::str_c("   ", cont_with, "\n") else ""
  model <- stringr::str_c(
    "%OVERALL%\n",
    mplus_mod_norms_nowith, "\n",
    "   ", cont_with, "\n",
    extra_1,
    class_block(2, extra_k),
    class_block(3, extra_k)
  )

  obj <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = model,
    VARIABLE = variable_cal,
    ANALYSIS = analysis,
    OUTPUT = "standardized;",
    usevariables = c("id", "vs1hcapdxeap", use_common),
    rdata = inhcap
  )

  setwd(out_dir)
  on.exit(setwd(here::here()), add = TRUE)
  MplusAutomation::mplusModeler(obj, modelout = stringr::str_c("pmm_hcap_", stem, ".inp"),
                                run = 1, writeData = "always")

  out_path <- file.path(out_dir, stringr::str_c("pmm_hcap_", stem, ".out"))
  mod <- read_mplus_model_quietly(out_path)
  flags <- output_flags(out_path)
  ok <- !is.null(mod[["summaries"]][["LL"]]) && !flags[["not_terminated"]]
  if (ok) check_prior_sample(inhcap_cog, mod, stem)
  list(mod = mod, out_path = out_path, flags = flags, ok = ok)
}

run_scoring <- function(stem, cal_mod) {
  model <- write_lca_model(cal_mod, mplus_mod_norms_nowith)
  model <- sub("%OVERALL%",
               stringr::str_c("%OVERALL% \n ",
                              weighted_class_logits(inhcap_cog, "vs1hcapdxeap", "HCAP16WGTR")),
               model, fixed = TRUE)

  obj <- MplusAutomation::mplusObject(
    TITLE = mplus_title,
    MODEL = model,
    VARIABLE = variable_fixed,
    ANALYSIS = analysis,
    OUTPUT = "standardized;",
    SAVEDATA = stringr::str_c("SAVE = CPROBABILITIES; file = cprob_", stem, ".dat;"),
    usevariables = c("id", use_common),
    rdata = inhcap
  )

  setwd(out_dir)
  on.exit(setwd(here::here()), add = TRUE)
  MplusAutomation::mplusModeler(obj, modelout = stringr::str_c("pmm_hcap_", stem, ".inp"),
                                run = 1, writeData = "always")

  out_path <- file.path(out_dir, stringr::str_c("pmm_hcap_", stem, ".out"))
  mod <- read_mplus_model_quietly(out_path)
  check_prior_sample(inhcap_cog, mod, stem)
  list(mod = mod, out_path = out_path, flags = output_flags(out_path))
}

# ---------------------------------------------------------------------------
# Fit 105a; fit 105b only if 105a terminated normally

fit_105a <- run_calibration("105a", by_class = FALSE)
message("105a terminated normally: ", fit_105a$ok)
score_105a <- if (fit_105a$ok) run_scoring("105a_b", fit_105a$mod) else NULL

fit_105b <- NULL
score_105b <- NULL
if (fit_105a$ok) {
  fit_105b <- run_calibration("105b", by_class = TRUE)
  message("105b terminated normally: ", fit_105b$ok)
  if (fit_105b$ok) score_105b <- run_scoring("105b_b", fit_105b$mod)
} else {
  message("105b not fitted because 105a did not terminate normally.")
}

# ---------------------------------------------------------------------------
# Comparison

class_labels <- c("Normal", "MCI", "Dementia")

weighted_kappa_from_tab <- function(tab) {   # as in PMM_110
  obs <- unclass(tab)
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  w <- outer(seq_len(nrow(obs)), seq_len(ncol(obs)),
             function(i, j) 1 - ((i - j)^2 / (nrow(obs) - 1)^2))
  po <- sum(w * obs) / sum(obs)
  pe <- sum(w * expected) / sum(obs)
  (po - pe) / (1 - pe)
}

binary_kappa_from_tab <- function(tab) {     # as in PMM_110
  obs <- unclass(tab)
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)
  po <- sum(diag(obs)) / sum(obs)
  pe <- sum(diag(expected)) / sum(obs)
  (po - pe) / (1 - pe)
}

# Sample for the agreement statistics, as in PMM_110 (hcap_sample).
kappa_sample <- PMM_100 |>
  dplyr::filter(inHCAP == 1, !is.na(cogfunction2016), !is.na(vs1hcapdxeap),
                !is.na(HCAP16WGTR)) |>
  dplyr::transmute(id = as.integer(id),
                   algorithmic = as.integer(haven::zap_labels(vs1hcapdxeap)),
                   hcap_w = HCAP16WGTR)

# Sample for the class shares, as in PMM_112 (margins_sample).
share_sample <- PMM_100 |>
  dplyr::filter(inHCAP == 1) |>
  dplyr::transmute(id = as.integer(id), hcap_w = HCAP16WGTR)

jorm_cprob <- read_cprob(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.out"))

combine_cprob <- function(cog_path) {
  cog <- read_cprob(cog_path)
  cog |>
    dplyr::full_join(jorm_cprob, by = "id", suffix = c("", "_jorm")) |>
    dplyr::transmute(
      id,
      c_combined = dplyr::if_else(!is.na(c_jorm), c_jorm, c),
      p1 = dplyr::if_else(!is.na(c_jorm), cprob1_jorm, cprob1),
      p2 = dplyr::if_else(!is.na(c_jorm), cprob2_jorm, cprob2),
      p3 = dplyr::if_else(!is.na(c_jorm), cprob3_jorm, cprob3)
    )
}

agreement <- function(combined) {
  d <- kappa_sample |> dplyr::inner_join(combined, by = "id")

  # Probability-proportional allocation, 100 replicates per person (PMM_110).
  rep <- d |>
    dplyr::slice(rep(seq_len(dplyr::n()), each = 100)) |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      i = dplyr::row_number(),
      cut1 = p1 * 100,
      cut2 = p2 * 100 + cut1,
      model_class = dplyr::case_when(i <= cut1 ~ 1L, i <= cut2 ~ 2L, TRUE ~ 3L),
      w100 = hcap_w / 100
    ) |>
    dplyr::ungroup()

  tab3 <- function(x, y, w) {
    xtabs(w ~ factor(x, levels = 1:3) + factor(y, levels = 1:3)) |> round(1)
  }
  tab2 <- function(x, y, w) {
    xtabs(w ~ factor(x == 3L, levels = c(FALSE, TRUE)) +
              factor(y == 3L, levels = c(FALSE, TRUE))) |> round(1)
  }

  tibble::tibble(
    n_kappa = nrow(d),
    kappa_3class = weighted_kappa_from_tab(tab3(rep$algorithmic, rep$model_class, rep$w100)),
    kappa_dementia = binary_kappa_from_tab(tab2(rep$algorithmic, rep$model_class, rep$w100)),
    kappa_3class_modal = weighted_kappa_from_tab(tab3(d$algorithmic, d$c_combined, d$hcap_w)),
    kappa_dementia_modal = binary_kappa_from_tab(tab2(d$algorithmic, d$c_combined, d$hcap_w))
  )
}

class_shares <- function(combined) {
  d <- share_sample |> dplyr::left_join(combined, by = "id")
  total <- sum(d$hcap_w, na.rm = TRUE)
  purrr::map_dbl(1:3, ~ 100 * sum(d$hcap_w[d$c_combined %in% .x], na.rm = TRUE) / total) |>
    rlang::set_names(stringr::str_c("pct_", class_labels, "_modal"))
}

fit_stats <- function(cal_path, score_path) {
  cal <- read_mplus_model_quietly(cal_path)[["summaries"]]
  sc <- read_mplus_model_quietly(score_path)[["summaries"]]
  tibble::tibble(
    n_cognition = cal[["Observations"]],
    loglikelihood = cal[["LL"]],
    free_parameters = cal[["Parameters"]],
    bic = cal[["BIC"]],
    entropy = if (is.null(sc[["Entropy"]])) NA_real_ else sc[["Entropy"]]
  )
}

models <- tibble::tribble(
  ~model, ~description, ~cal_path, ~score_path,
  "102", "General factor",
    here::here("mplus_output", "pmm_102", "pmm_hcap_102.out"),
    here::here("mplus_output", "pmm_102", "pmm_hcap_102b.out"),
  "103", "No factor, scores independent within class",
    here::here("mplus_output", "pmm_103", "pmm_hcap_103.out"),
    here::here("mplus_output", "pmm_103", "pmm_hcap_103b.out"),
  "105a", "No factor, continuous-score covariances equal across class",
    file.path(out_dir, "pmm_hcap_105a.out"),
    file.path(out_dir, "pmm_hcap_105a_b.out"),
  "105b", "No factor, continuous-score covariances free by class",
    file.path(out_dir, "pmm_hcap_105b.out"),
    file.path(out_dir, "pmm_hcap_105b_b.out")
) |>
  dplyr::mutate(fitted = c(TRUE, TRUE, !is.null(score_105a), !is.null(score_105b)))

comparison <- models |>
  dplyr::filter(fitted) |>
  dplyr::mutate(results = purrr::map2(cal_path, score_path, function(cal, score) {
    combined <- combine_cprob(score)
    dplyr::bind_cols(fit_stats(cal, score),
                     agreement(combined),
                     tibble::as_tibble_row(class_shares(combined)))
  })) |>
  tidyr::unnest(results) |>
  dplyr::select(-cal_path, -score_path, -fitted)

flags <- list(
  fit_105a = fit_105a$flags,
  score_105a = if (!is.null(score_105a)) score_105a$flags,
  fit_105b = if (!is.null(fit_105b)) fit_105b$flags,
  score_105b = if (!is.null(score_105b)) score_105b$flags
)

saveRDS(list(comparison = comparison, flags = flags, run_date = Sys.Date()),
        here::here("R_objects", "PMM_105_comparison.RDS"))

csv_path <- here::here("Reports", stringr::str_c("PMM_105_model_comparison_", Sys.Date(), ".csv"))
readr::write_csv(comparison, csv_path)

options(width = 200)
cat("\nMplus output flags (TRUE = message found in the .out file):\n")
print(flags)
cat("\nComparison (agreement with the HRS/HCAP Algorithm, in-sample, survey-weighted):\n")
print(as.data.frame(dplyr::mutate(comparison, dplyr::across(where(is.numeric), ~ round(.x, 3)))))
cat("\nCheck: the 103 row of kappa_3class and kappa_dementia should match the manuscript (0.450, 0.377).\n")
cat("Wrote ", csv_path, "\n", sep = "")
