if (!exists("analysis2_hcap", inherits = FALSE)) {
  source(here::here("R", "MS_MAIN", "MS_001_analysis_cohort.R"))
}

analysis2_a7 <- readRDS(here::here("R_objects", "A7_100_hcap_tables.rds"))
source(here::here("R", "PMM_110_Comparison_of_Consensus_Langa_Weir.R"))
analysis2_pmm <- PMM_110
source(here::here("R", "PMM_112_Margins.R"))
analysis2_pmm_margins <- PMM_112

analysis2_source_note <- function(text) {
  paste0("Source: ", text)
}

analysis2_flextable <- function(data, title, note) {
  flextable::flextable(data) |>
    flextable::theme_booktabs() |>
    flextable::add_header_lines(title) |>
    flextable::add_footer_lines(values = analysis2_source_note(note)) |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::autofit()
}

analysis2_weighted_margin <- function(data, classification, weight, labels) {
  classification <- rlang::ensym(classification)
  weight <- rlang::ensym(weight)

  data |>
    dplyr::filter(!is.na(!!classification), !is.na(!!weight), !!weight > 0) |>
    dplyr::mutate(classification = as.character(!!classification)) |>
    dplyr::group_by(classification) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted_percent = 100 * sum(!!weight) / sum(data[[rlang::as_string(weight)]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::right_join(tibble::tibble(classification = labels), by = "classification") |>
    dplyr::mutate(
      n = tidyr::replace_na(.data$n, 0L),
      weighted_percent = tidyr::replace_na(.data$weighted_percent, 0)
    )
}

# Expected class counts and weighted margins from probabilities. These are
# exact probability allocations, rather than counts of replicated records.
analysis2_probability_margin <- function(data, weight, labels) {
  keep <- is.finite(data[[weight]]) & data[[weight]] > 0
  d <- data[keep, , drop = FALSE]
  if (!nrow(d)) stop("No eligible observations for probability margins.")
  P <- pmm_normalize_probabilities(d[, c("cprob1_combined", "cprob2_combined", "cprob3_combined")])
  tibble::tibble(classification = labels, n = colSums(P),
    weighted_percent = 100 * colSums(P * d[[weight]]) / sum(d[[weight]]))
}

analysis2_class_margins <- dplyr::bind_rows(
  analysis2_weighted_margin(
    analysis2_a7$data$hcap_sample,
    algorithmic,
    hcap_w,
    analysis2_a7$labels$class
  ) |>
    dplyr::mutate(method = "Manly-Jones algorithm"),
  analysis2_weighted_margin(
    analysis2_a7$data$hcap_sample,
    hrs,
    hcap_w,
    analysis2_a7$labels$class
  ) |>
    dplyr::mutate(method = "Core algorithm"),
  analysis2_weighted_margin(
    analysis2_a7$data$hcap_sample,
    langa_weir,
    hcap_w,
    analysis2_a7$labels$class
  ) |>
    dplyr::mutate(method = "Langa-Weir"),
  analysis2_weighted_margin(
    analysis2_a7$data$hcap_sample,
    hudomiet,
    hcap_w,
    analysis2_a7$labels$class
  ) |>
    dplyr::mutate(method = "Hudomiet"),
  analysis2_weighted_margin(
    analysis2_pmm$data$hcap_sample_combined |>
      dplyr::mutate(
        combined_model = factor(
          c_combined,
          levels = 1:3,
          labels = analysis2_pmm$labels$class,
          ordered = TRUE
        )
      ),
    combined_model,
    hcap_w,
    analysis2_pmm$labels$class
  ) |>
    dplyr::mutate(method = "PMM"),
  analysis2_probability_margin(analysis2_pmm$data$hcap_sample_combined,
    "hcap_w", analysis2_pmm$labels$class) |>
    dplyr::mutate(method = "PMM (probability allocation)")
) |>
  dplyr::select(method, classification, n, weighted_percent) |>
  tidyr::pivot_wider(
    names_from = classification,
    values_from = c(n, weighted_percent),
    names_glue = "{classification}_{.value}"
  )

# "Normal x%, MCI y%, Dementia z%" for one method's HCAP16WGTR-weighted margins.
analysis2_margin_text <- function(method_name) {
  margins <- analysis2_class_margins |>
    dplyr::filter(.data$method == method_name)
  percents <- vapply(
    analysis2_pmm$labels$class,
    function(class) margins[[paste0(class, "_weighted_percent")]],
    numeric(1)
  )
  paste0(names(percents), " ", sprintf("%.1f%%", percents), collapse = ", ")
}

# PMM class proportions fixed in the scoring models (weighted_class_logits() in
# R/PMM_101_mplus_function.R), read back from the model-estimated class
# proportions in the Mplus output. Mplus classes 1-3 are Normal, MCI, Dementia.
analysis2_pmm_prior_text <- function(model_path) {
  class_counts <- suppressWarnings(MplusAutomation::readModels(model_path))[["class_counts"]][["modelEstimated"]]
  paste0(
    analysis2_pmm$labels$class[class_counts$class], " ",
    sprintf("%.1f%%", 100 * class_counts$proportion),
    collapse = ", "
  )
}

# Model outputs are named in R/PMM_090_model_registry.R (sourced by PMM_110).
analysis2_pmm_priors <- c(
  cognition = analysis2_pmm_prior_text(pmm_models$cognition$scoring_out),
  jorm = analysis2_pmm_prior_text(pmm_models$informant$scoring_out)
)

# PMM model samples and design counts (2026-10-22)
analysis2_pmm_n <- c(
  cognition = suppressWarnings(MplusAutomation::readModels(pmm_models$cognition$calibration_out))[["summaries"]][["Observations"]],
  informant = suppressWarnings(MplusAutomation::readModels(pmm_models$informant$calibration_out))[["summaries"]][["Observations"]]
)
analysis2_pmm_n_cov_terms <- length(unique(stringr::str_match_all(
  readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS")), " on (x[0-9x]+) @"
)[[1]][, 2]))


analysis2_class_margin_display <- analysis2_class_margins |>
  dplyr::mutate(
    method = factor(
      method,
      levels = c(
        "Manly-Jones algorithm", "Core algorithm", "PMM", "PMM (probability allocation)", "Langa-Weir", "Hudomiet"
      )
    )
  ) |>
  dplyr::arrange(method) |>
  dplyr::transmute(
    method = dplyr::recode(as.character(method), PMM = "PMM (most likely class)"),
    normal_n = Normal_n,
    normal_percent = Normal_weighted_percent,
    mci_n = MCI_n,
    mci_percent = MCI_weighted_percent,
    dementia_n = Dementia_n,
    dementia_percent = Dementia_weighted_percent
  )

analysis2_class_margin_display <- dplyr::bind_rows(
  analysis2_class_margin_display |>
    dplyr::slice(1:4),
  tibble::tibble(
    method = "Secondary comparators",
    normal_n = NA_real_, normal_percent = NA_real_,
    mci_n = NA_real_, mci_percent = NA_real_,
    dementia_n = NA_real_, dementia_percent = NA_real_
  ),
  analysis2_class_margin_display |>
    dplyr::slice(5:6)
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::ends_with("_percent"),
      ~ dplyr::if_else(is.na(.x), NA_character_, sprintf("%.1f%%", .x))
    )
  )

analysis2_secondary_row <- match("Secondary comparators", analysis2_class_margin_display$method)
analysis2_probability_row <- match("PMM (probability allocation)", analysis2_class_margin_display$method)

analysis2_class_margin_table <- flextable::flextable(analysis2_class_margin_display) |>
  flextable::theme_booktabs() |>
  flextable::set_header_labels(
    method = "Method",
    normal_n = "N", normal_percent = "Weighted %",
    mci_n = "N", mci_percent = "Weighted %",
    dementia_n = "N", dementia_percent = "Weighted %"
  ) |>
  flextable::add_header_row(
    values = c("", "Normal", "MCI", "Dementia"),
    colwidths = c(1, 2, 2, 2)
  ) |>
  flextable::add_header_lines("Table 2. HCAP classification margins by method") |>
  flextable::merge_at(i = analysis2_secondary_row, j = 1:7, part = "body") |>
  flextable::bold(i = analysis2_secondary_row, part = "body") |>
  flextable::bg(i = analysis2_secondary_row, bg = "#E6E6E6", part = "body") |>
  flextable::align(i = analysis2_secondary_row, align = "left", part = "body") |>
  flextable::colformat_double(
    j = c("normal_n", "mci_n", "dementia_n"),
    digits = 0,
    big.mark = ","
  ) |>
  flextable::colformat_double(
    i = analysis2_probability_row, j = c("normal_n", "mci_n", "dementia_n"),
    digits = 1, big.mark = ","
  ) |>
  flextable::width(j = "method", width = 1.35) |>
  flextable::width(
    j = c(
      "normal_n", "normal_percent", "mci_n", "mci_percent",
      "dementia_n", "dementia_percent"
    ),
    width = 0.775
  ) |>
  flextable::add_footer_lines(
    "Note. N values are unweighted participant counts, except the PMM probability-allocation row, where N is the expected count (sum of class probabilities, shown to one decimal). Percentages use HCAP16WGTR and are shown to the nearest tenth. PMM probability allocation uses direct weighted sums, the exact replacement for the former 100-record approximation; PMM most likely class uses assigned classes."
  ) |>
  flextable::add_footer_lines(
    "References. Manly, J. J., Jones, R. N., Langa, K. M., Ryan, L. H., Levine, D. A., McCammon, R., Heeringa, S. G., & Weir, D. (2022). Estimating the prevalence of dementia and mild cognitive impairment in the US: The 2016 Health and Retirement Study Harmonized Cognitive Assessment Protocol Project. JAMA Neurology, 79(12), 1242-1249. https://doi.org/10.1001/jamaneurol.2022.3543"
  ) |>
  flextable::add_footer_lines(
    "Langa, K. M., Weir, D. R., Kabeto, M., & Sonnega, A. (2023). Langa-Weir Classification of Cognitive Function (1995-2020). https://hrsdata.isr.umich.edu/sites/default/files/documentation/data-descriptions/1695907706/Data_Description_Langa_Weir_Classifications2020_V2.pdf"
  ) |>
  flextable::add_footer_lines(
    "Hudomiet, P., Hurd, M. D., & Rohwedder, S. (2022). Trends in inequalities in the prevalence of dementia in the United States. Proceedings of the National Academy of Sciences, 119(46), e2212205119. https://doi.org/10.1073/pnas.2212205119"
  ) |>
  flextable::fontsize(size = 8, part = "all") |>
  flextable::font(fontname = "Arial", part = "all")

analysis2_pmm_hudomiet_sample <- analysis2_pmm$data$hcap_sample_combined |>
  dplyr::inner_join(analysis2_a7$data$hcap_sample |> dplyr::select(id, hudomiet), by = "id") |>
  dplyr::filter(!is.na(hudomiet), is.finite(hcap_w), hcap_w > 0)
analysis2_pmm_hudomiet_tab <- pmm_probability_table(analysis2_pmm_hudomiet_sample, "hudomiet", "hcap_w")
analysis2_pmm_hudomiet_sample$hudomiet_binary <- factor(
  as.integer(analysis2_pmm_hudomiet_sample$hudomiet) == 3L,
  levels = c(FALSE, TRUE), labels = c("Non-demented", "Demented"))
analysis2_pmm_hudomiet_binary_tab <- pmm_probability_table(
  analysis2_pmm_hudomiet_sample, "hudomiet_binary", "hcap_w", binary = TRUE)
analysis2_pmm_hudomiet_kappa <- weighted_kappa_from_tab(analysis2_pmm_hudomiet_tab)$quadratic_weighted_kappa
analysis2_pmm_hudomiet_binary_kappa <- binary_kappa_from_tab(analysis2_pmm_hudomiet_binary_tab)$kappa
analysis2_pmm_hudomiet_modal <- modal_agreement(analysis2_pmm_hudomiet_sample, "hudomiet", "hcap_w")

analysis2_method_order <- c(
  "Manly-Jones algorithm", "Core algorithm", "PMM (probability allocation)",
  "Langa-Weir", "Hudomiet", "Consensus diagnosis"
)
analysis2_pmm_core_sample <- analysis2_pmm$data$hcap_sample_combined |>
  dplyr::inner_join(analysis2_a7$data$hcap_sample |>
    dplyr::select(id, core_class = hrs), by = "id")
analysis2_pmm_core_kappa <- weighted_kappa_from_tab(pmm_probability_table(
  analysis2_pmm_core_sample, "core_class", "hcap_w"))$quadratic_weighted_kappa
analysis2_pmm_core_sample$core_binary <- factor(
  as.integer(analysis2_pmm_core_sample$core_class) == 3L,
  levels = c(FALSE, TRUE), labels = c("Non-demented", "Demented"))
analysis2_pmm_core_binary_kappa <- binary_kappa_from_tab(pmm_probability_table(
  analysis2_pmm_core_sample, "core_binary", "hcap_w", binary = TRUE))$kappa
analysis2_pmm_core_modal <- modal_agreement(analysis2_pmm_core_sample, "core_class", "hcap_w")

analysis2_agreement <- tibble::tribble(
  ~classification_1, ~classification_2, ~three_class_kappa, ~binary_kappa,
  "Core algorithm", "Manly-Jones algorithm", analysis2_a7$summary$algorithmic_hrs_kappa, analysis2_a7$summary$algorithmic_hrs_binary_kappa,
  "PMM", "Manly-Jones algorithm", analysis2_pmm$summary$algorithmic_combined_kappa, analysis2_pmm$summary$algorithmic_combined_binary_kappa,
  "Langa-Weir", "Manly-Jones algorithm", analysis2_a7$summary$algorithmic_lw_kappa, analysis2_a7$summary$algorithmic_lw_binary_kappa,
  "Hudomiet", "Manly-Jones algorithm", analysis2_a7$summary$algorithmic_hudomiet_kappa, analysis2_a7$summary$algorithmic_hudomiet_binary_kappa,
  "HCAP Consensus", "Manly-Jones algorithm", analysis2_a7$summary$algorithmic_consensus_kappa, analysis2_a7$summary$algorithmic_consensus_binary_kappa,
  "Core algorithm", "Langa-Weir", analysis2_a7$summary$lw_hrs_kappa, analysis2_a7$summary$lw_hrs_binary_kappa,
  "PMM", "Langa-Weir", analysis2_pmm$summary$lw_combined_kappa, analysis2_pmm$summary$lw_combined_binary_kappa,
  "Hudomiet", "Langa-Weir", analysis2_a7$summary$lw_hudomiet_kappa, analysis2_a7$summary$lw_hudomiet_binary_kappa,
  "HCAP Consensus", "Langa-Weir", analysis2_a7$summary$lw_consensus_kappa, analysis2_a7$summary$lw_consensus_binary_kappa,
  "Core algorithm", "Hudomiet", analysis2_a7$summary$hrs_hudomiet_kappa, analysis2_a7$summary$hrs_hudomiet_binary_kappa,
  "PMM", "Hudomiet", analysis2_pmm_hudomiet_kappa, analysis2_pmm_hudomiet_binary_kappa,
  "HCAP Consensus", "Hudomiet", analysis2_a7$summary$consensus_hudomiet_kappa, analysis2_a7$summary$consensus_hudomiet_binary_kappa,
  "Core algorithm", "HCAP Consensus", analysis2_a7$summary$consensus_hrs_kappa, analysis2_a7$summary$consensus_hrs_binary_kappa,
  "PMM", "HCAP Consensus", analysis2_pmm$summary$consensus_combined_kappa, analysis2_pmm$summary$consensus_combined_binary_kappa,
  "PMM", "Core algorithm", analysis2_pmm_core_kappa, analysis2_pmm_core_binary_kappa,
  "PMM (most likely class)", "Core algorithm", analysis2_pmm_core_modal[["three_class"]], analysis2_pmm_core_modal[["dementia"]],
  "PMM (most likely class)", "Manly-Jones algorithm", analysis2_pmm$modal_kappas$Manly_Jones[["three_class"]], analysis2_pmm$modal_kappas$Manly_Jones[["dementia"]],
  "PMM (most likely class)", "Langa-Weir", analysis2_pmm$modal_kappas$Langa_Weir[["three_class"]], analysis2_pmm$modal_kappas$Langa_Weir[["dementia"]],
  "PMM (most likely class)", "Hudomiet", analysis2_pmm_hudomiet_modal[["three_class"]], analysis2_pmm_hudomiet_modal[["dementia"]],
  "PMM (most likely class)", "HCAP Consensus", analysis2_pmm$modal_kappas$Consensus[["three_class"]], analysis2_pmm$modal_kappas$Consensus[["dementia"]]
) |>
  dplyr::mutate(
    three_class_kappa = round(three_class_kappa, 3),
    binary_kappa = round(binary_kappa, 3)
  )

# Keep each unordered pair once, with the earlier-listed method in column 1;
# sort column 1 groups and column 2 entries using the same method order.
analysis2_order_agreement <- function(data, method_order) {
  aliases <- c("PMM" = "PMM (probability allocation)", "HCAP Consensus" = "Consensus diagnosis")
  for (column in c("classification_1", "classification_2")) {
    data[[column]] <- dplyr::recode(data[[column]], !!!aliases)
  }
  r1 <- match(data$classification_1, method_order)
  r2 <- match(data$classification_2, method_order)
  if (anyNA(r1) || anyNA(r2)) stop("Unknown classification label in Table 3.")
  first <- data$classification_1
  second <- data$classification_2
  data$classification_1 <- ifelse(r1 <= r2, first, second)
  data$classification_2 <- ifelse(r1 <= r2, second, first)
  data |>
    dplyr::arrange(factor(classification_1, levels = method_order),
                   factor(classification_2, levels = method_order))
}
# Keep the two PMM summaries together, directly after the core algorithm.
analysis2_table3_order <- append(analysis2_method_order, "PMM (most likely class)", after = 3)
analysis2_ordered_agreement <- analysis2_order_agreement(analysis2_agreement, analysis2_table3_order)
analysis2_agreement_groups <- unique(analysis2_ordered_agreement$classification_1)
analysis2_agreement_display <- purrr::map2_dfr(
  analysis2_agreement_groups, seq_along(analysis2_agreement_groups),
  function(method, index) {
    rows <- analysis2_ordered_agreement |>
      dplyr::filter(classification_1 == method)
    if (index < length(analysis2_agreement_groups)) {
      dplyr::bind_rows(rows, tibble::tibble(classification_1 = NA_character_, classification_2 = NA_character_,
        three_class_kappa = NA_real_, binary_kappa = NA_real_))
    } else rows
  }
)

analysis2_agreement_table <- analysis2_flextable(
  analysis2_agreement_display |>
    dplyr::rename(
      "Classification 1" = classification_1,
      "Classification 2" = classification_2,
      "Three-class quadratic weighted kappa" = three_class_kappa,
      "Dementia versus non-dementia kappa" = binary_kappa
    ),
  "Table 3. Agreement among Core classifications and comparison diagnoses",
  paste(
    "A7 rows: R_objects/A7_100_hcap_tables.rds, generated by R/A7_100-comparison_of_diagnoses.R.",
    "PMM rows: R/PMM_110_Comparison_of_Consensus_Langa_Weir.R; PMM-Hudomiet was calculated from the same saved probabilities and A7 comparison data.",
    "Three-class values are quadratic weighted kappas; binary values are kappas for dementia versus non-dementia.",
    "PMM probability allocation uses direct weighted probabilities; most likely PMM assignments are reported separately.",
    "Consensus comparisons use normalized HCAP16WGTR / samplingP weights; other comparisons use HCAP16WGTR."
  )
)

# Display labels for the A7 matrix (the saved A7 object keeps its own labels)
analysis2_method_labels <- c(
  "Algorithmic diagnosis" = "Manly-Jones algorithm",
  "HRS classification model" = "Core algorithm"
)
analysis2_relabel <- function(x) dplyr::recode(x, !!!analysis2_method_labels)

# Add probability-allocation PMM to the A7 pairwise matrix. Every PMM
# off-diagonal cell uses the same direct-weighting definition as Table 3.
analysis2_extend_agreement_matrix <- function(base, pmm_kappas, method_order) {
  base <- as.matrix(base)
  if (anyDuplicated(rownames(base)) || !identical(rownames(base), colnames(base)) ||
      !setequal(names(pmm_kappas), rownames(base))) {
    stop("Pairwise matrix requires matching, unique method names and a PMM kappa for each method.")
  }
  methods <- method_order
  if (!setequal(methods, c(rownames(base), "PMM (probability allocation)")) || anyDuplicated(methods)) {
    stop("Table 4 method order must name every method once.")
  }
  out <- matrix(NA_real_, length(methods), length(methods), dimnames = list(methods, methods))
  out[rownames(base), colnames(base)] <- base
  out[rownames(base), "PMM (probability allocation)"] <- pmm_kappas[rownames(base)]
  out["PMM (probability allocation)", colnames(base)] <- pmm_kappas[colnames(base)]
  diag(out) <- 1
  out
}
analysis2_base_agreement_matrix <- as.matrix(analysis2_a7$matrices$three_class)
dimnames(analysis2_base_agreement_matrix) <- lapply(dimnames(analysis2_base_agreement_matrix), analysis2_relabel)
analysis2_agreement_matrix <- analysis2_extend_agreement_matrix(
  analysis2_base_agreement_matrix,
  c("Consensus diagnosis" = analysis2_pmm$summary$consensus_combined_kappa,
    "Manly-Jones algorithm" = analysis2_pmm$summary$algorithmic_combined_kappa,
    "Langa-Weir" = analysis2_pmm$summary$lw_combined_kappa,
    "Core algorithm" = analysis2_pmm_core_kappa,
    "Hudomiet" = analysis2_pmm_hudomiet_kappa), method_order = analysis2_method_order) |>
  as.data.frame.matrix() |> tibble::rownames_to_column("method")
analysis2_agreement_matrix_table <- analysis2_flextable(
  analysis2_agreement_matrix,
  "Table 4. Pairwise three-class agreement among HCAP comparison methods",
  paste(
    "A7 comparisons: R_objects/A7_100_hcap_tables.rds. PMM comparisons: direct probability allocation from the current A3 probabilities, as in Table 3.",
    "Entries are quadratic weighted kappas; the PMM row and column use probability allocation, not modal class assignments or replicated records.",
    "Consensus comparisons use normalized HCAP16WGTR / samplingP weights; other comparisons use HCAP16WGTR."
  )
) |>
  flextable::set_header_labels(method = "Method") |>
  flextable::colformat_double(j = 2:ncol(analysis2_agreement_matrix), digits = 3) |>
  flextable::width(j = "method", width = 1.65) |>
  flextable::width(j = 2:ncol(analysis2_agreement_matrix), width = 0.8) |>
  flextable::set_table_properties(layout = "fixed")

analysis2_pmm_probability_figure <- analysis2_pmm_margins$figure +
  ggplot2::labs(
    title = "PMM class probabilities in HRS/HCAP",
    caption = paste(
      "Source: R/PMM_112_Margins.R; model outputs named in R/PMM_090_model_registry.R.",
      "Each facet uses HRS/HCAP participant-level PMM probabilities.",
      "Bar heights are weighted counts: HCAP16WGTR rescaled to sum to the sample size."
    )
  )

analysis2_sample_flow <- tibble::tibble(
  sample = c(
    "HRS 2016 analysis input",
    "HRS/HCAP 2016 participants (study sample)"
  ),
  n = c(
    nrow(analysis2_tracker),
    nrow(analysis2_hcap)
  )
)

# Comparison-sample sizes used for the agreement statistics. These should
# equal the study sample; they are reported in the Methods outline.
analysis2_n_compare <- c(
  core_algorithm = nrow(analysis2_a7$data$hcap_sample),
  pmm = nrow(analysis2_pmm$data$hcap_sample_combined)
)

analysis2_sample_flow_figure <- ggplot2::ggplot(
  analysis2_sample_flow,
  ggplot2::aes(x = sample, y = n)
) +
  ggplot2::geom_col(fill = "grey50") +
  ggplot2::geom_text(
    ggplot2::aes(label = scales::comma(n)),
    vjust = -0.3
  ) +
  ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.1))) +
  ggplot2::labs(
    title = "Analysis 2 sample flow",
    x = NULL,
    y = "Participants",
    caption = paste(
      "Source: R_objects/A7_005_hrs16_merged.rds.",
      "Study sample: all HRS/HCAP 2016 participants, including those classified",
      "from informant (Jorm IQCODE) data only."
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))

# Marginal-rate reproduction on a common set of participants and one denominator.
# Computed locally at render time, never copied from the review's diagnostics.
analysis2_matching_sample <- analysis2_pmm$data$hcap_sample_combined |>
  dplyr::inner_join(analysis2_a7$data$hcap_sample |>
    dplyr::select(id, core_class = hrs, reference_a7 = algorithmic), by = "id") |>
  dplyr::filter(!is.na(core_class), !is.na(algorithmic), is.finite(hcap_w), hcap_w > 0)
if (!nrow(analysis2_matching_sample) || anyDuplicated(analysis2_matching_sample$id) ||
    any(as.character(analysis2_matching_sample$reference_a7) != as.character(analysis2_matching_sample$algorithmic))) {
  stop("A2 marginal matching requires unique participants and the same Manly-Jones reference in A3 and A7.")
}
mm <- analysis2_matching_sample
mm_labels <- analysis2_pmm$labels$class
mm_ref <- pmm_weighted_shares(mm$algorithmic, mm$hcap_w)
mm_prob <- pmm_normalize_probabilities(mm[, c("cprob1_combined", "cprob2_combined", "cprob3_combined")])
mm_shares <- rbind(
  mm_ref,
  pmm_weighted_shares(mm$core_class, mm$hcap_w),
  100 * colSums(mm_prob * mm$hcap_w) / sum(mm$hcap_w),
  pmm_weighted_shares(factor(mm$c_combined, levels = 1:3, labels = mm_labels), mm$hcap_w)
)
analysis2_marginal_matching <- tibble::tibble(
  method = c("Manly-Jones algorithm", "Core algorithm", "PMM: mean probabilities", "PMM: most likely class"),
  Normal = mm_shares[, 1], MCI = mm_shares[, 2], Dementia = mm_shares[, 3],
  normal_gap = mm_shares[, 1] - mm_ref[1], mci_gap = mm_shares[, 2] - mm_ref[2],
  dementia_gap = mm_shares[, 3] - mm_ref[3]
)
analysis2_matching_text <- function(method) {
  d <- analysis2_marginal_matching |> dplyr::filter(.data$method == .env$method)
  sprintf("Normal %.1f%%, MCI %.1f%%, Dementia %.1f%%", d$Normal, d$MCI, d$Dementia)
}
analysis2_matching_gap_text <- function(method) {
  d <- analysis2_marginal_matching |> dplyr::filter(.data$method == .env$method)
  sprintf("Normal %+.2f, MCI %+.2f, Dementia %+.2f percentage points", d$normal_gap, d$mci_gap, d$dementia_gap)
}
analysis2_marginal_matching_table <- flextable::flextable(analysis2_marginal_matching) |>
  flextable::theme_booktabs() |>
  flextable::set_header_labels(method = "Method", Normal = "Normal (%)", MCI = "MCI (%)", Dementia = "Dementia (%)",
    normal_gap = "Normal Δ", mci_gap = "MCI Δ", dementia_gap = "Dementia Δ") |>
  flextable::add_header_lines("Table 2b. Reproduction of marginal classification rates") |>
  flextable::colformat_double(j = c("Normal", "MCI", "Dementia"), digits = 1) |>
  flextable::colformat_double(j = c("normal_gap", "mci_gap", "dementia_gap"), digits = 2) |>
  flextable::width(j = 1, width = 2) |> flextable::width(j = 2:7, width = 0.75) |>
  flextable::fontsize(size = 8, part = "all") |> flextable::font(fontname = "Arial", part = "all") |>
  flextable::add_footer_lines(paste0(
    "Note. Common comparison sample, N = ", format(nrow(mm), big.mark = ","),
    "; HCAP16WGTR and the same denominator for every row. Δ is the difference from the Manly-Jones rate in percentage points. ",
    "The core algorithm supplies assigned classes; PMM supplies probabilities and most likely classes. ",
    "Descriptive marginal agreement does not establish individual or subgroup calibration. ",
    "Source: Analysis A7 comparison results and current Analysis A3 probabilities, computed at render time."))
