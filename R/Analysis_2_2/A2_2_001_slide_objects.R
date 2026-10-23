# A2_2_001_slide_objects.R
#
# Objects used only by the Analysis A2_2 slides (added 2026-10-22). Sourced in
# A2_2_000_Control.qmd after R/MS_MAIN/MS_010_results_objects.R. Every value on
# the slides is computed at render time from saved results:
#   - Appendix 1 objects (a1_*): R/MS_MAIN/MS_020_appendix1_objects.R, which
#     reads the Analysis A7 summaries in R_objects/.
#   - PMM parameters: the cognition-model calibration H5 file named in
#     R/PMM_090_model_registry.R (model results only, no individual data).

# Appendix 1 objects -----------------------------------------------------------
# MS_020 sources R/A7_060-algorithm_functions.R, which defines its own
# weighted_kappa_from_tab() and binary_kappa_from_tab(). Source it in a
# separate environment so it does not replace the PMM versions used by
# MS_010, then copy only the a1_* objects into the slide environment.
a2_2_a1_env <- new.env(parent = globalenv())
source(here::here("R", "MS_MAIN", "MS_020_appendix1_objects.R"), local = a2_2_a1_env)
for (a2_2_name in ls(a2_2_a1_env, pattern = "^a1_")) {
  assign(a2_2_name, get(a2_2_name, envir = a2_2_a1_env))
}
a2_2_weighted_thousands_note <- get("weighted_thousands_note", envir = a2_2_a1_env)
rm(a2_2_name)

# Slide table style (matches the agreement slide) ------------------------------
a2_2_gt_style <- function(tbl, font = 22, width = 95) {
  tbl |>
    gt::opt_stylize(6, color = "gray") |>
    gt::tab_options(
      table.width = gt::pct(width),
      table.font.size = gt::px(font),
      column_labels.font.size = gt::px(font - 2),
      row_group.font.size = gt::px(font - 2),
      stub.font.size = gt::px(font),
      data_row.padding = gt::px(4)
    )
}
a2_2_pct <- function(x, digits = 1) formatC(100 * x, format = "f", digits = digits)
a2_2_dec <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

# Kappa with the Manly-Jones algorithm, from the Table 3 source object.
a2_2_kappa_mj <- function(method, column) {
  x <- analysis2_agreement[[column]][analysis2_agreement$classification_2 == "Manly-Jones algorithm" &
                                       analysis2_agreement$classification_1 == method]
  if (length(x) != 1L) stop("No unique Manly-Jones kappa for ", method)
  x
}

# PMM cognition model: parameters from the calibration H5 file -----------------
source(here::here("R", "PMM_090_model_registry.R"))
source(here::here("R", "PMM_101_mplus_function.R"))   # pmm_h5_parameter_table()

a2_2_class_names <- c("Normal", "MCI", "Dementia")
a2_2_item_labels <- c(stats::setNames(a1_items$label, toupper(a1_items$item)),
                      PD102 = "Self-rated memory")

a2_2_pmm_h5 <- function(stype = NULL) {
  path <- pmm_models$cognition$calibration_h5
  results <- if (is.null(stype)) {
    mplush5::mplus.print.model.results(path)
  } else {
    mplush5::mplus.print.standardized.model.results(path, stype = stype)
  }
  pmm_h5_parameter_table(results) |>
    dplyr::filter(LatentClass %in% c("1", "2", "3")) |>
    dplyr::mutate(class = factor(LatentClass, levels = c("1", "2", "3"),
                                 labels = a2_2_class_names))
}

# Standardized (STDYX) loadings on the general cognitive factor, by class.
a2_2_pmm_loadings <- a2_2_pmm_h5("stdyx") |>
  dplyr::filter(grepl("\\.BY$", paramHeader)) |>
  dplyr::mutate(item = dplyr::coalesce(unname(a2_2_item_labels[toupper(param)]), param)) |>
  dplyr::select(item, class, est) |>
  tidyr::pivot_wider(names_from = class, values_from = est) |>
  dplyr::arrange(match(item, a2_2_item_labels))
if (!nrow(a2_2_pmm_loadings) || !all(a2_2_class_names %in% names(a2_2_pmm_loadings))) {
  stop("PMM STDYX loadings by class not found in the cognition-model H5 file.")
}

# The sign of a factor is arbitrary. If Mplus returned every loading negative,
# reflect the factor for presentation: loadings and class means change sign
# (variances, SEs, and the model do not). Nothing changes when any loading is
# positive.
a2_2_gcp_reflected <- all(unlist(a2_2_pmm_loadings[a2_2_class_names]) < 0)
if (a2_2_gcp_reflected) {
  a2_2_pmm_loadings <- a2_2_pmm_loadings |>
    dplyr::mutate(dplyr::across(dplyr::all_of(a2_2_class_names), ~ 0 - .x))
}
a2_2_reflection_note <- if (a2_2_gcp_reflected) {
  " The sign of the factor is reversed from the Mplus output (loadings and class means) so that loadings are positive; the model is unchanged."
} else {
  ""
}

# Mean and variance of the general cognitive factor (GCP) by class.
a2_2_pmm_gcp <- a2_2_pmm_h5() |>
  dplyr::filter(paramHeader %in% c("Means", "Variances"),
                !grepl("^\\.?\\d", param),
                !toupper(param) %in% names(a2_2_item_labels)) |>
  dplyr::select(class, paramHeader, est, se, est_se)
if (nrow(a2_2_pmm_gcp) != 6L) {
  stop("Expected one factor mean and one factor variance per class in the cognition-model H5 file; found ",
       nrow(a2_2_pmm_gcp), " rows.")
}
a2_2_pmm_gcp <- a2_2_pmm_gcp |>
  tidyr::pivot_wider(names_from = paramHeader, values_from = c(est, se, est_se)) |>
  dplyr::arrange(class) |>
  dplyr::mutate(
    est_Means = if (a2_2_gcp_reflected) 0 - est_Means else est_Means,   # 0 - x avoids "-0"
    fixed_mean = est_se_Means == 999 | se_Means == 0,
    fixed_var = est_se_Variances == 999 | se_Variances == 0,
    sd = sqrt(est_Variances),
    sd_se = se_Variances / (2 * sd)   # delta method
  )
a2_2_gcp_value <- function(est, se, fixed, digits = 2) {
  ifelse(fixed, paste0(a2_2_dec(est, 0), " (fixed)"),
         paste0(a2_2_dec(est, digits), " (", a2_2_dec(se, digits), ")"))
}
a2_2_gcp_mean <- function(cls) a2_2_pmm_gcp$est_Means[a2_2_pmm_gcp$class == cls]
a2_2_gcp_sd   <- function(cls) a2_2_pmm_gcp$sd[a2_2_pmm_gcp$class == cls]

# Pairwise three-class kappa matrix with both PMM summaries (Table 4 plus a
# PMM (most likely class) row and column). Kappas of the most likely class with
# the other methods come from analysis2_agreement (Table 3).
a2_2_pairwise_methods <- append(analysis2_method_order, "PMM (most likely class)", after = 3)
a2_2_modal_kappa <- analysis2_agreement |>
  dplyr::filter(classification_1 == "PMM (most likely class)") |>
  dplyr::mutate(classification_2 = dplyr::recode(classification_2, "HCAP Consensus" = "Consensus diagnosis")) |>
  dplyr::pull(three_class_kappa, name = classification_2)

# Probability allocation vs. most likely class: each person's most likely class
# against the same person's class probabilities, HCAP16WGTR weights.
a2_2_prob_vs_modal <- analysis2_pmm$data$hcap_sample_combined |>
  dplyr::mutate(modal = factor(c_combined, levels = 1:3, labels = analysis2_pmm$labels$class)) |>
  pmm_probability_table("modal", "hcap_w") |>
  weighted_kappa_from_tab()
a2_2_prob_vs_modal <- a2_2_prob_vs_modal$quadratic_weighted_kappa

a2_2_pairwise <- matrix(NA_real_, length(a2_2_pairwise_methods), length(a2_2_pairwise_methods),
                        dimnames = list(a2_2_pairwise_methods, a2_2_pairwise_methods))
base_matrix <- as.matrix(tibble::column_to_rownames(analysis2_agreement_matrix, "method"))
a2_2_pairwise[rownames(base_matrix), colnames(base_matrix)] <- base_matrix
modal_name <- "PMM (most likely class)"
a2_2_pairwise[names(a2_2_modal_kappa), modal_name] <- a2_2_modal_kappa
a2_2_pairwise[modal_name, names(a2_2_modal_kappa)] <- a2_2_modal_kappa
a2_2_pairwise[modal_name, "PMM (probability allocation)"] <- a2_2_prob_vs_modal
a2_2_pairwise["PMM (probability allocation)", modal_name] <- a2_2_prob_vs_modal
diag(a2_2_pairwise) <- 1
if (anyNA(a2_2_pairwise)) stop("Pairwise agreement matrix has missing cells.")
a2_2_pairwise <- tibble::as_tibble(a2_2_pairwise, rownames = "method")
rm(base_matrix, modal_name)
