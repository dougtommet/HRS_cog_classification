source(here::here("R", "PMM_090_model_registry.R"))
source(here::here("R", "PMM_109_probability_tools.R"))

PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))

class_labels <- c("Normal", "MCI", "Dementia")

normalize_mplus_savedata <- function(savedata) {
  savedata |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::rename(c = tidyselect::any_of("mlcc"))
}

read_mplus_savedata_quietly <- function(model_path) {
  suppressWarnings(MplusAutomation::readModels(model_path))[["savedata"]] |>
    normalize_mplus_savedata()
}

format_margin_table <- function(data) {
  data |>
    gt::gt() |>
    gt::fmt_number(columns = "n", decimals = 0) |>
    gt::fmt_number(columns = "weighted_percent", decimals = 1) |>
    gt::cols_label(
      class = "Class",
      n = "N",
      weighted_percent = "Weighted %"
    )
}

add_margin_note <- function(table, note_text) {
  table |>
    gt::tab_source_note(
      gt::md(note_text)
    )
}

combined_probabilities <- {
  cognition_cprob <- read_mplus_savedata_quietly(
    pmm_models$cognition$scoring_out
  ) |>
    dplyr::select(id, cprob1, cprob2, cprob3, c)

  jorm_cprob <- read_mplus_savedata_quietly(
    pmm_models$informant$scoring_out
  ) |>
    dplyr::rename(
      cprob1_jorm = cprob1,
      cprob2_jorm = cprob2,
      cprob3_jorm = cprob3,
      c_jorm = c
    ) |>
    dplyr::select(id, cprob1_jorm, cprob2_jorm, cprob3_jorm, c_jorm)

  cognition_cprob |>
    dplyr::full_join(jorm_cprob, by = "id") |>
    dplyr::mutate(
      c_combined = dplyr::case_when(
        !is.na(c_jorm) ~ c_jorm,
        !is.na(c) ~ c
      ),
      cprob1_combined = dplyr::case_when(
        !is.na(c_jorm) ~ cprob1_jorm,
        !is.na(c) ~ cprob1
      ),
      cprob2_combined = dplyr::case_when(
        !is.na(c_jorm) ~ cprob2_jorm,
        !is.na(c) ~ cprob2
      ),
      cprob3_combined = dplyr::case_when(
        !is.na(c_jorm) ~ cprob3_jorm,
        !is.na(c) ~ cprob3
      )
    ) |>
    dplyr::select(id, c_combined, cprob1_combined, cprob2_combined, cprob3_combined)
}

margins_sample <- PMM_100 |>
  dplyr::filter(inHCAP == 1) |>
  dplyr::transmute(
    id = as.integer(id),
    hcap_weight = HCAP16WGTR,
    reference = factor(as.integer(haven::zap_labels(vs1hcapdxeap)), levels = 1:3, labels = class_labels),
    langa_weir = haven::zap_labels(cogfunction2016)
  ) |>
  dplyr::left_join(combined_probabilities, by = "id") |>
  dplyr::mutate(
    langa_weir = factor(langa_weir, levels = 1:3, labels = class_labels, ordered = TRUE),
    combined_class = factor(c_combined, levels = 1:3, labels = class_labels, ordered = TRUE)
  )

prob_cols <- c("cprob1_combined", "cprob2_combined", "cprob3_combined")
if (anyDuplicated(combined_probabilities$id) ||
    !setequal(combined_probabilities$id, margins_sample$id) ||
    anyNA(margins_sample$reference) || any(!is.finite(margins_sample$hcap_weight)) ||
    any(margins_sample$hcap_weight <= 0)) stop("Invalid or incomplete PMM margins sample.")
margins_sample[, prob_cols] <- as.data.frame(pmm_normalize_probabilities(margins_sample[, prob_cols]))
posterior_shares <- 100 * colSums(as.matrix(margins_sample[, prob_cols]) * margins_sample$hcap_weight) /
  sum(margins_sample$hcap_weight)
comparison_margin_data <- tibble::tibble(
  Class = class_labels,
  `Manly-Jones (%)` = as.numeric(pmm_weighted_shares(margins_sample$reference, margins_sample$hcap_weight)),
  `PMM mean probability (%)` = as.numeric(posterior_shares),
  `PMM modal class (%)` = as.numeric(pmm_weighted_shares(margins_sample$combined_class, margins_sample$hcap_weight))
)
comparison_margin_table <- comparison_margin_data |>
  gt::gt() |> gt::fmt_number(columns = where(is.numeric), decimals = 2) |>
  gt::tab_source_note(paste0("HCAP16WGTR; identical participants and denominator (N = ",
    format(nrow(margins_sample), big.mark = ","), "). Probability averages and modal assignments are separate estimands."))

# Normalized (sample-scaled) weights: HCAP16WGTR rescaled to sum to the
# sample size, so bar heights are weighted counts on the sample scale.
histogram_data <- margins_sample |>
  dplyr::mutate(sample_weight = hcap_weight * dplyr::n() / sum(hcap_weight)) |>
  dplyr::select(id, sample_weight, cprob1_combined, cprob2_combined, cprob3_combined) |>
  tidyr::pivot_longer(
    cols = c(cprob1_combined, cprob2_combined, cprob3_combined),
    names_to = "class_probability",
    values_to = "probability"
  ) |>
  dplyr::mutate(
    class = factor(
      class_probability,
      levels = c("cprob1_combined", "cprob2_combined", "cprob3_combined"),
      labels = class_labels
    )
  )

margin_plot <- ggplot2::ggplot(
  histogram_data,
  ggplot2::aes(x = probability, weight = sample_weight)
) +
  # 30 bins of width 1/30 starting at 0, so the bins tile [0, 1] exactly.
  # With bins = 30 alone, the end bins straddle 0 and 1 and are dropped by
  # the x-axis limits, which removes probabilities near 0 and near 1.
  ggplot2::geom_histogram(
    binwidth = 1 / 30,
    boundary = 0,
    closed = "left",
    fill = "grey70",
    color = "white",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  # Fixed scales: all panels share one y-axis range, set by the tallest bar.
  ggplot2::facet_wrap(~ class, nrow = 1, scales = "fixed") +
  ggplot2::scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = function(x) dplyr::case_when(
      abs(x - 0) < 1e-8 ~ "0",
      abs(x - 0.5) < 1e-8 ~ ".5",
      abs(x - 1.0) < 1e-8 ~ "1.0",
      TRUE ~ ""
    )
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(
    x = "PMM class probability",
    y = "Weighted count"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

margin_plot_manuscript <- margin_plot +
  ggplot2::labs(
    title = NULL,
    x = "Class probability"
  )

ggplot2::ggsave(
  filename = here::here("Figures", "MS_Main-Figure-2-PMM-Class-Probabilities.png"),
  plot = margin_plot_manuscript,
  width = 10,
  height = 4.5,
  units = "in",
  dpi = 600
)

margin_table_data <- margins_sample |>
  dplyr::filter(!is.na(combined_class), !is.na(hcap_weight)) |>
  dplyr::group_by(combined_class) |>
  dplyr::summarise(
    n = dplyr::n(),
    weighted_percent = 100 * sum(hcap_weight, na.rm = TRUE) / sum(margins_sample$hcap_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::transmute(
    class = as.character(combined_class),
    n = n,
    weighted_percent = weighted_percent
  )

langa_weir_margin_table_data <- margins_sample |>
  dplyr::filter(!is.na(langa_weir), !is.na(hcap_weight)) |>
  dplyr::group_by(langa_weir) |>
  dplyr::summarise(
    n = dplyr::n(),
    weighted_percent = 100 * sum(hcap_weight, na.rm = TRUE) / sum(margins_sample$hcap_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::transmute(
    class = as.character(langa_weir),
    n = n,
    weighted_percent = weighted_percent
  )

combined_margin_note <- paste(
  paste0("Note: N and weighted percentages are computed in the non-replicated HRS/HCAP sample (N = ", format(nrow(margins_sample), big.mark = ","), ")."),
  "Weighted percentages use HCAP16WGTR.",
  "PMM probabilities and most likely class assignments use the cognition model, or the informant model when an IQCODE is present."
)

langa_weir_margin_note <- paste(
  paste0("Note: N and weighted percentages are computed in the non-replicated HRS/HCAP sample (N = ", format(nrow(margins_sample), big.mark = ","), ")."),
  "Weighted percentages use HCAP16WGTR.",
  "Langa-Weir classifications come from cogfunction2016; category 2 is labeled MCI here for consistency with the rest of the report."
)

PMM_112 <- list(
  data = list(
    margins_sample = margins_sample,
    comparison_margin_data = comparison_margin_data,
    posterior_shares = posterior_shares,
    histogram_data = histogram_data,
    margin_table_data = margin_table_data,
    langa_weir_margin_table_data = langa_weir_margin_table_data
  ),
  comparison_table = comparison_margin_table,
  figure = margin_plot,
  manuscript_figure = margin_plot_manuscript,
  table = add_margin_note(format_margin_table(margin_table_data), combined_margin_note),
  langa_weir_table = add_margin_note(format_margin_table(langa_weir_margin_table_data), langa_weir_margin_note)
)
