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

analysis2_class_margins <- dplyr::bind_rows(
  analysis2_weighted_margin(
    analysis2_a7$data$hcap_sample,
    algorithmic,
    hcap_w,
    analysis2_a7$labels$class
  ) |>
    dplyr::mutate(method = "HRS/HCAP Algorithm"),
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
    dplyr::mutate(method = "PMM")
) |>
  dplyr::select(method, classification, n, weighted_percent) |>
  tidyr::pivot_wider(
    names_from = classification,
    values_from = c(n, weighted_percent),
    names_glue = "{classification}_{.value}"
  )

analysis2_class_margin_display <- analysis2_class_margins |>
  dplyr::mutate(
    method = factor(
      method,
      levels = c(
        "HRS/HCAP Algorithm", "Core algorithm", "PMM", "Langa-Weir", "Hudomiet"
      )
    )
  ) |>
  dplyr::arrange(method) |>
  dplyr::transmute(
    method = as.character(method),
    normal_n = Normal_n,
    normal_percent = Normal_weighted_percent,
    mci_n = MCI_n,
    mci_percent = MCI_weighted_percent,
    dementia_n = Dementia_n,
    dementia_percent = Dementia_weighted_percent
  )

analysis2_class_margin_display <- dplyr::bind_rows(
  analysis2_class_margin_display |>
    dplyr::slice(1:3),
  tibble::tibble(
    method = "Secondary comparators",
    normal_n = NA_real_, normal_percent = NA_real_,
    mci_n = NA_real_, mci_percent = NA_real_,
    dementia_n = NA_real_, dementia_percent = NA_real_
  ),
  analysis2_class_margin_display |>
    dplyr::slice(4:5)
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::ends_with("_percent"),
      ~ dplyr::if_else(is.na(.x), NA_character_, sprintf("%.1f%%", .x))
    )
  )

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
  flextable::merge_at(i = 4, j = 1:7, part = "body") |>
  flextable::bold(i = 4, part = "body") |>
  flextable::bg(i = 4, bg = "#E6E6E6", part = "body") |>
  flextable::align(i = 4, align = "left", part = "body") |>
  flextable::colformat_num(
    j = c("normal_n", "mci_n", "dementia_n"),
    digits = 0,
    big.mark = ","
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
    "Note. N values are unweighted HRS/HCAP participant counts. Percentages use HCAP16WGTR; PMM counts and percentages use the non-replicated combined cognition/Jorm model sample."
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

analysis2_pmm_hudomiet_sample <- analysis2_pmm$data$hcap_sample_combined_rep |>
  dplyr::inner_join(
    analysis2_a7$data$hcap_sample |>
      dplyr::select(id, hudomiet),
    by = "id"
  ) |>
  dplyr::filter(!is.na(combined_model), !is.na(hudomiet), !is.na(hcap_w100))

analysis2_pmm_hudomiet_tab <- xtabs(
  hcap_w100 ~ combined_model + hudomiet,
  data = analysis2_pmm_hudomiet_sample
)

analysis2_pmm_hudomiet_binary_data <- analysis2_pmm_hudomiet_sample |>
  dplyr::mutate(
    combined_model_binary = factor(
      dplyr::if_else(as.integer(combined_model) == 3L, 2L, 1L),
      levels = 1:2,
      labels = c("Non-demented", "Demented"),
      ordered = TRUE
    ),
    hudomiet_binary = factor(
      dplyr::if_else(as.integer(hudomiet) == 3L, 2L, 1L),
      levels = 1:2,
      labels = c("Non-demented", "Demented"),
      ordered = TRUE
    )
  )

analysis2_pmm_hudomiet_binary_tab <- xtabs(
  hcap_w100 ~ combined_model_binary + hudomiet_binary,
  data = analysis2_pmm_hudomiet_binary_data
)

analysis2_pmm_hudomiet_kappa <- weighted_kappa_from_tab(
  analysis2_pmm_hudomiet_tab
)$quadratic_weighted_kappa
analysis2_pmm_hudomiet_binary_kappa <- binary_kappa_from_tab(
  analysis2_pmm_hudomiet_binary_tab
)$kappa

analysis2_agreement <- tibble::tribble(
  ~classification_1, ~classification_2, ~three_class_kappa, ~binary_kappa,
  "Core Algorithm", "HRS/HCAP Algorithm", analysis2_a7$summary$algorithmic_hrs_kappa, analysis2_a7$summary$algorithmic_hrs_binary_kappa,
  "PMM", "HRS/HCAP Algorithm", analysis2_pmm$summary$algorithmic_combined_kappa, analysis2_pmm$summary$algorithmic_combined_binary_kappa,
  "Langa-Weir", "HRS/HCAP Algorithm", analysis2_a7$summary$algorithmic_lw_kappa, analysis2_a7$summary$algorithmic_lw_binary_kappa,
  "Hudomiet", "HRS/HCAP Algorithm", analysis2_a7$summary$algorithmic_hudomiet_kappa, analysis2_a7$summary$algorithmic_hudomiet_binary_kappa,
  "HCAP Consensus", "HRS/HCAP Algorithm", analysis2_a7$summary$algorithmic_consensus_kappa, analysis2_a7$summary$algorithmic_consensus_binary_kappa,
  "Core Algorithm", "Langa-Weir", analysis2_a7$summary$lw_hrs_kappa, analysis2_a7$summary$lw_hrs_binary_kappa,
  "PMM", "Langa-Weir", analysis2_pmm$summary$lw_combined_kappa, analysis2_pmm$summary$lw_combined_binary_kappa,
  "Hudomiet", "Langa-Weir", analysis2_a7$summary$lw_hudomiet_kappa, analysis2_a7$summary$lw_hudomiet_binary_kappa,
  "HCAP Consensus", "Langa-Weir", analysis2_a7$summary$lw_consensus_kappa, analysis2_a7$summary$lw_consensus_binary_kappa,
  "Core Algorithm", "Hudomiet", analysis2_a7$summary$hrs_hudomiet_kappa, analysis2_a7$summary$hrs_hudomiet_binary_kappa,
  "PMM", "Hudomiet", analysis2_pmm_hudomiet_kappa, analysis2_pmm_hudomiet_binary_kappa,
  "HCAP Consensus", "Hudomiet", analysis2_a7$summary$consensus_hudomiet_kappa, analysis2_a7$summary$consensus_hudomiet_binary_kappa,
  "Core Algorithm", "HCAP Consensus", analysis2_a7$summary$consensus_hrs_kappa, analysis2_a7$summary$consensus_hrs_binary_kappa,
  "PMM", "HCAP Consensus", analysis2_pmm$summary$consensus_combined_kappa, analysis2_pmm$summary$consensus_combined_binary_kappa
) |>
  dplyr::mutate(
    three_class_kappa = round(three_class_kappa, 3),
    binary_kappa = round(binary_kappa, 3)
  )

analysis2_agreement_comparators <- unique(analysis2_agreement$classification_2)

analysis2_agreement_display <- purrr::map2_dfr(
  analysis2_agreement_comparators,
  seq_along(analysis2_agreement_comparators),
  function(comparator, index) {
    comparison_rows <- analysis2_agreement |>
      dplyr::filter(classification_2 == comparator)

    if (index < length(analysis2_agreement_comparators)) {
      dplyr::bind_rows(
        comparison_rows,
        tibble::tibble(
          classification_1 = NA_character_,
          classification_2 = NA_character_,
          three_class_kappa = NA_real_,
          binary_kappa = NA_real_
        )
      )
    } else {
      comparison_rows
    }
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
    "Consensus comparisons use normalized HCAP16WGTR / samplingP weights; other comparisons use HCAP16WGTR."
  )
)

analysis2_a7_agreement_matrix <- analysis2_a7$matrices$three_class |>
  as.data.frame.matrix() |>
  tibble::rownames_to_column("method") |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

analysis2_a7_agreement_matrix_table <- analysis2_flextable(
  analysis2_a7_agreement_matrix,
  "Table 4. Pairwise three-class agreement among HCAP comparison methods",
  paste(
    "Source: R_objects/A7_100_hcap_tables.rds, generated by R/A7_100-comparison_of_diagnoses.R.",
    "Entries are quadratic weighted kappas. Consensus comparisons use normalized",
    "HCAP16WGTR / samplingP weights; other comparisons use HCAP16WGTR."
  )
)

analysis2_pmm_probability_figure <- analysis2_pmm_margins$figure +
  ggplot2::labs(
    title = "PMM class probabilities in HRS/HCAP",
    caption = paste(
      "Source: R/PMM_112_Margins.R; Mplus outputs pmm_103 and pmm_103_jorm.",
      "Each facet uses HRS/HCAP participant-level combined-model probabilities."
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