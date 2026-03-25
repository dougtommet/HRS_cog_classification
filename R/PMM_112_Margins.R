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
    here::here("mplus_output", "pmm_103", "pmm_hcap_103b.out")
  ) |>
    dplyr::select(id, cprob1, cprob2, cprob3, c)

  jorm_cprob <- read_mplus_savedata_quietly(
    here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.out")
  ) |>
    dplyr::rename(
      cprob1_jorm = cprob1,
      cprob2_jorm = cprob2,
      cprob3_jorm = cprob3,
      c_jorm = c
    ) |>
    dplyr::select(id, cprob1_jorm, cprob2_jorm, cprob3_jorm, c_jorm)

  cognition_cprob |>
    dplyr::left_join(jorm_cprob, by = "id") |>
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
    langa_weir = haven::zap_labels(cogfunction2016)
  ) |>
  dplyr::left_join(combined_probabilities, by = "id") |>
  dplyr::mutate(
    langa_weir = factor(langa_weir, levels = 1:3, labels = class_labels, ordered = TRUE),
    combined_class = factor(c_combined, levels = 1:3, labels = class_labels, ordered = TRUE)
  )

histogram_data <- margins_sample |>
  dplyr::select(id, cprob1_combined, cprob2_combined, cprob3_combined) |>
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
  ggplot2::aes(x = probability)
) +
  ggplot2::geom_histogram(
    bins = 30,
    fill = "grey70",
    color = "white",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(~ class, nrow = 1) +
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
  ggplot2::scale_y_continuous(breaks = seq(0, 750, by = 250), expand = c(0, 0)) +
  ggplot2::coord_cartesian(ylim = c(0, 750)) +
  ggplot2::labs(
    x = "Combined-model class probability",
    y = "Count"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
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
  "Note: N and weighted percentages are computed in the non-replicated HRS/HCAP sample (N = 3496).",
  "Weighted percentages use HCAP16WGTR.",
  "Combined-model probabilities and most likely class assignments use the cognition-model and Jorm-model combination rule."
)

langa_weir_margin_note <- paste(
  "Note: N and weighted percentages are computed in the non-replicated HRS/HCAP sample (N = 3496).",
  "Weighted percentages use HCAP16WGTR.",
  "Langa-Weir classifications come from cogfunction2016; category 2 is labeled MCI here for consistency with the rest of the report."
)

PMM_112 <- list(
  data = list(
    margins_sample = margins_sample,
    histogram_data = histogram_data,
    margin_table_data = margin_table_data,
    langa_weir_margin_table_data = langa_weir_margin_table_data
  ),
  figure = margin_plot,
  table = add_margin_note(format_margin_table(margin_table_data), combined_margin_note),
  langa_weir_table = add_margin_note(format_margin_table(langa_weir_margin_table_data), langa_weir_margin_note)
)
