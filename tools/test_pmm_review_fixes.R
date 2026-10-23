#!/usr/bin/env Rscript
# Synthetic-only checks. Never opens project data, saved probabilities, or H5.
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(tibble) })
source("R/PMM_101_mplus_function.R")
source("R/PMM_109_probability_tools.R")
fails <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))

fixture <- do.call(rbind, lapply(1:3, function(k) data.frame(
  Class = paste0("Latent Class ", k, " (", k, ")"),
  Section = c("", "", "Means", "Thresholds", "Variances", "Residual Variances"),
  Statement = c("GCP BY Y", "Y ON X1", "GCP", "U$1", "GCP", "Y"),
  Estimate = c(0.123456789123, 0.04, c(0, -1, -2)[k], -1.234567890123,
               k, 0.0044592819177))))
fixture$`S.E.` <- .01
fixture$`Estimate/S.E.` <- 1
reported <- pmm_h5_parameter_table(fixture)
stopifnot(any(reported$paramHeader == "Gcp.BY"),
          any(reported$paramHeader == "Residual.Variances"),
          abs(reported$est[reported$param == "Y" & reported$paramHeader == "Residual.Variances"][1] - .0044592819177) < 1e-15)
syntax <- pmm_fixed_model_from_h5_results(fixture, "y on x1 @ 0.04;")
stopifnot(grepl("y @ 0.0044592819177;", syntax, fixed = TRUE),
          grepl("[ u$1 @ -1.234567890123 ];", syntax, fixed = TRUE),
          length(gregexpr("y on x1", syntax, fixed = TRUE)[[1]]) == 1)
fails(pmm_fixed_model_from_h5_results(fixture[fixture$Class != "Latent Class 2 (2)", ]))
bad <- fixture; bad$Estimate[1] <- NA; fails(pmm_fixed_model_from_h5_results(bad))
fails(pmm_fixed_model_from_h5_results(rbind(fixture, fixture[1, ])))
fails(write_lca_model(list(parameters = "printed output")))
pmm_check_h5_parameter_transfer(fixture, fixture[nrow(fixture):1, ])
rounded <- fixture; rounded$Estimate <- round(rounded$Estimate, 3)
fails(pmm_check_h5_parameter_transfer(fixture, rounded))
fails(pmm_check_h5_parameter_transfer(fixture, fixture[-1, ]))
prior <- weighted_class_logits(data.frame(class = 1:3, w = c(7, 2, 1)), "class", "w")
stopifnot(grepl(format_mplus_number(log(7)), prior, fixed = TRUE))
fails(weighted_class_logits(data.frame(class = c(1, 3), w = c(7, 1)), "class", "w"))
fails(weighted_class_logits(data.frame(class = 1:3, w = c(7, 0, 1)), "class", "w"))

labs <- c("Normal", "MCI", "Dementia")
d <- data.frame(id = 1:3, ref = factor(labs, levels = labs), w = c(2, 3, 5),
                cprob1_combined = c(.906, .1, .02),
                cprob2_combined = c(.011, .6, .08),
                cprob3_combined = c(.083, .3, .9), c_combined = 1:3)
tab <- pmm_probability_table(d, "ref", "w")
stopifnot(max(abs(tab[1, ] - c(1.812, .022, .166))) < 1e-12,
          abs(sum(tab) - sum(d$w)) < 1e-12,
          max(abs(rowSums(tab) - d$w)) < 1e-12)
d$dem <- factor(d$ref == "Dementia", levels = c(FALSE, TRUE),
                labels = c("Non-demented", "Demented"))
b <- pmm_probability_table(d, "dem", "w", binary = TRUE)
stopifnot(abs(b[2, 2] - 4.5) < 1e-12, abs(sum(b) - 10) < 1e-12)
fails(pmm_normalize_probabilities(matrix(c(.2, .2, .2), nrow = 1)))
stopifnot(max(abs(pmm_weighted_shares(d$ref, d$w) - c(20, 30, 50))) < 1e-12)

# Evaluate function definitions only from the report script. Its data-access
# and model-reading expressions are never evaluated.
env <- new.env(parent = globalenv())
for (expr in parse("R/PMM_110_Comparison_of_Consensus_Langa_Weir.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, env)
}
env$class_labels <- labs
env$binary_labels <- c("Non-demented", "Demented")
hard <- env$modal_agreement(d, "ref", "w")
stopifnot(all(abs(hard - 1) < 1e-12))
invisible(env$format_weighted_tab(tab, "ref", "Reference", "PMM"))
invisible(env$format_weighted_tab(tab, "ref", "Reference", "PMM", units = "percent"))

# Exercise A2's new common-sample table on synthetic comparison results only.
mm_env <- new.env(parent = globalenv())
mm_env$analysis2_pmm <- list(data = list(hcap_sample_combined = transform(d,
  algorithmic = ref, hcap_w = w)), labels = list(class = labs))
mm_env$analysis2_a7 <- list(data = list(hcap_sample = data.frame(
  id = 1:3, hrs = factor(c("Normal", "Normal", "Dementia"), levels = labs), algorithmic = d$ref)))
start <- FALSE
for (expr in parse("R/MS_MAIN/MS_010_results_objects.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("analysis2_matching_sample"))) start <- TRUE
  if (start) eval(expr, mm_env)
}
stopifnot(nrow(mm_env$analysis2_marginal_matching) == 4,
          all(abs(mm_env$analysis2_marginal_matching[1, 5:7]) < 1e-12),
          grepl("percentage points", mm_env$analysis2_matching_gap_text("Core algorithm"), fixed = TRUE))

# Synthetic Table 2: expected N stays on the participant scale, percentages
# have one decimal, and adding a row does not move the separator styling.
table_env <- new.env(parent = globalenv())
expressions <- parse("R/MS_MAIN/MS_010_results_objects.R")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, table_env)
}
expected <- table_env$analysis2_probability_margin(d, "w", labs)
stopifnot(abs(sum(expected$n) - nrow(d)) < 1e-12,
          abs(sum(expected$weighted_percent) - 100) < 1e-12,
          abs(expected$n[1] - 1.026) < 1e-12,
          abs(expected$weighted_percent[1] - 22.12) < 1e-12)
table_env$analysis2_class_margins <- tibble::tibble(
  method = c("Manly-Jones algorithm", "Core algorithm", "PMM",
             "PMM (probability allocation)", "Langa-Weir", "Hudomiet"),
  Normal_n = c(1, 1, 1, expected$n[1], 1, 1),
  MCI_n = c(1, 1, 1, expected$n[2], 1, 1),
  Dementia_n = c(1, 1, 1, expected$n[3], 1, 1),
  Normal_weighted_percent = c(20, 30, 20, expected$weighted_percent[1], 40, 50),
  MCI_weighted_percent = c(30, 20, 30, expected$weighted_percent[2], 30, 20),
  Dementia_weighted_percent = c(50, 50, 50, expected$weighted_percent[3], 30, 30))
active <- FALSE
for (expr in expressions) {
  if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
  if (identical(expr[[2]], as.name("analysis2_class_margin_display"))) active <- TRUE
  if (identical(expr[[2]], as.name("analysis2_pmm_hudomiet_sample"))) break
  if (active) eval(expr, table_env)
}
stopifnot(table_env$analysis2_probability_row == 4L, table_env$analysis2_secondary_row == 5L,
          nrow(table_env$analysis2_class_margin_display) == 7L)
display_pct <- unlist(table_env$analysis2_class_margin_display[, c("normal_percent", "mci_percent", "dementia_percent")])
stopifnot(all(grepl("^[0-9]+\\.[0-9]%$", stats::na.omit(display_pct))))

# Synthetic Table 4: PMM-to-core is calculated, and the PMM row/column use
# method names rather than relying on a particular ordering of the base matrix.
table_env$analysis2_pmm <- mm_env$analysis2_pmm
table_env$weighted_kappa_from_tab <- env$weighted_kappa_from_tab
table_env$binary_kappa_from_tab <- env$binary_kappa_from_tab
table_env$modal_agreement <- env$modal_agreement
table_env$analysis2_pmm$summary <- list(consensus_combined_kappa = .1,
  algorithmic_combined_kappa = .2, lw_combined_kappa = .3)
table_env$analysis2_a7 <- mm_env$analysis2_a7
base_names <- c("Consensus diagnosis", "Algorithmic diagnosis", "Langa-Weir",
                "HRS classification model", "Hudomiet")
base_matrix <- matrix(.4, 5, 5, dimnames = list(base_names, base_names))
diag(base_matrix) <- 1
table_env$analysis2_a7$matrices <- list(three_class = base_matrix)
table_env$analysis2_pmm_hudomiet_kappa <- .5
table_env$analysis2_method_labels <- c("Algorithmic diagnosis" = "Manly-Jones algorithm",
                                      "HRS classification model" = "Core algorithm")
active <- FALSE
for (expr in expressions) {
  if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
  if (identical(expr[[2]], as.name("analysis2_method_order"))) eval(expr, table_env)
  if (identical(expr[[2]], as.name("analysis2_pmm_core_sample"))) active <- TRUE
  if (identical(expr[[2]], as.name("analysis2_agreement"))) break
  if (active) eval(expr, table_env)
}
active <- FALSE
for (expr in expressions) {
  if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
  if (identical(expr[[2]], as.name("analysis2_extend_agreement_matrix"))) active <- TRUE
  if (identical(expr[[2]], as.name("analysis2_pmm_probability_figure"))) break
  if (active) eval(expr, table_env)
}
mx <- as.matrix(table_env$analysis2_agreement_matrix[, -1])
rownames(mx) <- table_env$analysis2_agreement_matrix$method
pm <- "PMM (probability allocation)"
stopifnot(identical(dim(mx), c(6L, 6L)), isTRUE(all.equal(mx, t(mx))),
          identical(rownames(mx), table_env$analysis2_method_order),
          identical(colnames(mx), table_env$analysis2_method_order),
          mx[pm, "Hudomiet"] == .5, mx[pm, "Manly-Jones algorithm"] == .2,
          mx[pm, "Core algorithm"] == table_env$analysis2_pmm_core_kappa)

# Table 3 uses the same ranking in both columns, regardless of the original
# orientation of a symmetric agreement statistic.
order3 <- append(table_env$analysis2_method_order, "PMM (most likely class)", after = 5)
unordered <- tibble::tibble(
  classification_1 = c("PMM", "Core algorithm", "Hudomiet", "PMM (most likely class)"),
  classification_2 = c("Langa-Weir", "Manly-Jones algorithm", "Manly-Jones algorithm", "Core algorithm"),
  three_class_kappa = c(.2, .3, .4, .5), binary_kappa = c(.1, .2, .3, .4))
ordered <- table_env$analysis2_order_agreement(unordered, order3)
r1 <- match(ordered$classification_1, order3)
r2 <- match(ordered$classification_2, order3)
stopifnot(all(r1 <= r2), identical(order(r1, r2), seq_len(nrow(ordered))),
          ordered$classification_1[1] == "Manly-Jones algorithm",
          ordered$classification_2[1] == "Core algorithm",
          ordered$three_class_kappa[1] == .3)
cat("Synthetic H5 transfer, direct allocation, modal agreement, and A2 marginal-table checks passed.\n")
