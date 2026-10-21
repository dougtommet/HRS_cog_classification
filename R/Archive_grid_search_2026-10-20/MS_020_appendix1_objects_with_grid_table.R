# Analysis A2: results objects for Appendix 1 (core algorithm).
#
# Every value reported in MS_Appendix-1-Core-Algorithm.qmd comes from here.
# Inputs are written by the current Analysis A7 code (run A0 -> A7 first):
#   R_objects/A7_050_cfa_summary.rds          (A7_050)
#   R_objects/A7_055_norm_summary.rds         (A7_055)
#   R_objects/A7_070_threshold_calibration.rds (A7_070)
#   R_objects/A7_075_algorithm_summary.rds    (A7_075)
#   R_objects/A7_100_hcap_tables.rds          (A7_100)

source(here::here("R", "A7_060-algorithm_functions.R"))

a1_read <- function(file) {
  path <- here::here("R_objects", file)
  if (!file.exists(path)) {
    stop("Required object not found: R_objects/", file,
         ". Run Rscript Analysis7_Driver.R first.", call. = FALSE)
  }
  readRDS(path)
}

a1_cfa  <- a1_read("A7_050_cfa_summary.rds")
a1_norm <- a1_read("A7_055_norm_summary.rds")
a1_cal  <- a1_read("A7_070_threshold_calibration.rds")
a1_alg  <- a1_read("A7_075_algorithm_summary.rds")
a1_a7   <- a1_read("A7_100_hcap_tables.rds")

# Formatting helpers ----------------------------------------------------------
a1_n   <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
a1_pct <- function(x, digits = 1) formatC(100 * x, format = "f", digits = digits)
a1_dec <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
a1_cut <- function(x) formatC(x, format = "fg", digits = 3)
a1_iq  <- function(x) formatC(x, format = "f", digits = 1)   # IQCODE cut points, e.g. 3.0

# Tables fill the 6.5-inch text width (1-inch margins) and Word sets the
# column widths. flextable::autofit() sized columns from the merged title and
# note rows, which pushed Table A1 past the margins.
a1_flextable <- function(data, title, note, col_widths = NULL) {
  note <- sub("\\s*Source:.*$", "", note)   # Rich: no source-file notes in the appendix
  ft <- flextable::flextable(data) |>
    flextable::theme_booktabs() |>
    flextable::add_header_lines(title) |>
    flextable::add_footer_lines(values = note) |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::align(part = "header", i = 1, align = "left") |>
    flextable::align(part = "footer", align = "left") |>
    flextable::valign(part = "body", valign = "top")
  if (!is.null(col_widths)) {
    # Relative widths, rescaled to 6.5 inches.
    ft <- flextable::width(ft, j = seq_along(col_widths),
                           width = 6.5 * col_widths / sum(col_widths)) |>
      flextable::set_table_properties(layout = "fixed")
  } else {
    ft <- flextable::set_table_properties(ft, width = 1, layout = "autofit")
  }
  # Keep each table on one page so Word does not repeat the title row on a
  # second page (the duplicated Table A2 title in the 0917 render).
  if (exists("paginate", envir = asNamespace("flextable"))) {
    ft <- flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
  }
  ft
}

# Cut points ------------------------------------------------------------------
a1_cut_severe   <- a1_alg$cuts[["severe"]]
a1_cut_moderate <- a1_alg$cuts[["moderate"]]
a1_jorm_severe  <- a1_alg$jorm_cuts[["severe"]]
a1_jorm_mild    <- a1_alg$jorm_cuts[["mild"]]
a1_flow         <- a1_alg$hcap_flow

# Table A1: core cognitive items and standardized loadings --------------------
a1_items <- tibble::tribble(
  ~item,      ~label,                                        ~scoring,
  "vdori",    "Orientation to date",                         "Month, day, year, day of week; 0-4 correct (ordinal)",
  "vdlfl1z",  "Animal naming",                               "Animals named minus errors; rescaled to 0-1 (continuous)",
  "vdlfl2",   "Object naming (scissors, cactus)",            "0-2 correct (ordinal)",
  "vdlfl3",   "Naming the President and Vice President",     "0-2 correct (ordinal)",
  "vdwddelz", "Delayed word recall",                         "0-10 words; rescaled to 0-1 (continuous)",
  "vdexf7z",  "Number series",                               "Score rescaled to 0-1 (continuous)",
  "vdsevens", "Serial 7 subtraction",                        "0-5 correct (ordinal)",
  "vdcount",  "Backward counting from 20",                   "Correct on first or second try (binary)"
)

a1_table_items_data <- a1_items |>
  dplyr::left_join(a1_cfa$loadings_mlr |> dplyr::select(item, mlr = est), by = "item") |>
  dplyr::left_join(a1_cfa$loadings_wlsmv |> dplyr::select(item, wlsmv = est), by = "item") |>
  dplyr::transmute(
    Item = label,
    Scoring = scoring,
    `Loading (MLR, used for scoring)` = a1_dec(mlr),
    `Loading (WLSMV)` = a1_dec(wlsmv)
  )

a1_fit <- a1_cfa$fit_wlsmv
a1_loading_range <- range(a1_cfa$loadings_mlr$est)

a1_table_items <- a1_flextable(
  a1_table_items_data,
  "Table A1. HRS core cognitive items and standardized factor loadings",
  col_widths = c(2.0, 2.6, 1.0, 0.9),
  paste0(
    "Single-factor model fit to HRS 2016 respondents aged 65 and older (N = ",
    a1_n(a1_cfa$n_obs), ") with HRS-HCAP weights (weight 0 outside HRS-HCAP). ",
    "Loadings are standardized (STDYX). Continuous items were rescaled to 0-1 using the 2016 range. ",
    "WLSMV fit: chi-square = ", a1_dec(a1_fit$ChiSqM_Value, 1), " (df = ", a1_fit$ChiSqM_DF,
    "), RMSEA = ", a1_dec(a1_fit$RMSEA_Estimate, 3), ", CFI = ", a1_dec(a1_fit$CFI, 3),
    ", TLI = ", a1_dec(a1_fit$TLI, 3), ", SRMR = ", a1_dec(a1_fit$SRMR, 3), "."
  )
)

# Table A2: cut points and weighted class shares --------------------------------
# The algorithm uses the cut points set in April 2026 (a1_cal$cuts, 37 and 48).
# The grid optimum (closest weighted shares) is a check and is not used.
a1_target <- a1_cal$target_shares
a1_used   <- a1_cal$algorithm
a1_opt    <- a1_cal$calibrated
a1_maxk   <- a1_cal$max_kappa
a1_gpos   <- a1_cal$grid_position

a1_table_calibration_data <- dplyr::bind_rows(
  a1_used |> dplyr::mutate(Set = "Core algorithm (cut points used)"),
  a1_opt  |> dplyr::mutate(Set = "Grid: closest shares"),
  a1_maxk |> dplyr::mutate(Set = "Grid: highest 3-class kappa")
) |>
  dplyr::transmute(
    Set,
    `Severe cut` = a1_cut(cut_severe),
    `Moderate cut` = a1_cut(cut_moderate),
    `Normal, %` = a1_pct(share_normal),
    `MCI, %` = a1_pct(share_mci),
    `Dementia, %` = a1_pct(share_dementia),
    `Kappa, 3-class` = a1_dec(kappa_3class),
    `Kappa, dementia` = a1_dec(kappa_binary)
  ) |>
  dplyr::bind_rows(tibble::tibble(
    Set = "Target: Manly-Jones algorithm",
    `Normal, %` = a1_pct(a1_target[["1"]]),
    `MCI, %` = a1_pct(a1_target[["2"]]),
    `Dementia, %` = a1_pct(a1_target[["3"]])
  ))

a1_table_calibration <- a1_flextable(
  a1_table_calibration_data,
  col_widths = c(1.9, 0.6, 0.7, 0.65, 0.55, 0.75, 0.7, 0.75),
  "Table A2. T-score cut points and weighted class shares",
  paste0(
    "HRS-HCAP Wave 1, N = ", a1_n(a1_cal$n_sample), ". Percentages and kappas are weighted (HCAP Wave 1 (2016) sampling weight). ",
    "Kappas compare the core algorithm with the Manly-Jones algorithm ",
    "(3-class: quadratic weights; dementia: dementia versus not). The grid search covered ",
    a1_n(nrow(a1_cal$grid)), " pairs of cut points in steps of ", a1_cal$grid_step,
    " T-score points; its closest-share and kappa-maximizing pairs are shown for comparison. ",
    "Cut points were chosen and evaluated in the same sample."
  )
)

# Figure A2: weighted density of T with the selected cut points -----------------
a1_hcap <- a1_read("A7_075_hrs16_hcap.rds") |>
  dplyr::transmute(TF, w = HCAP16WGTR) |>
  dplyr::filter(!is.na(TF), !is.na(w), w > 0)

a1_band_labels <- c(
  paste0("T < ", a1_cut(a1_cut_severe)),
  paste0(a1_cut(a1_cut_severe), " ≤ T < ", a1_cut(a1_cut_moderate)),
  paste0("T ≥ ", a1_cut(a1_cut_moderate))
)
a1_band <- function(t) {
  factor(dplyr::case_when(t < a1_cut_severe ~ 1L, t < a1_cut_moderate ~ 2L, TRUE ~ 3L),
         levels = 1:3, labels = a1_band_labels)
}
a1_band_share <- tapply(a1_hcap$w, a1_band(a1_hcap$TF), sum) / sum(a1_hcap$w)
a1_n_density  <- nrow(a1_hcap)

a1_tdensity_figure <- function() {
  dens <- stats::density(a1_hcap$TF, weights = a1_hcap$w / sum(a1_hcap$w), n = 1024)
  dd <- tibble::tibble(x = dens$x, y = dens$y, band = a1_band(dens$x))
  top <- max(dd$y) * 1.18
  # Anchor each label at the weighted median T score of its band, inside the
  # filled area; labels sit in a row above the curve and repel along x only.
  lab <- a1_hcap |>
    dplyr::mutate(band = a1_band(TF)) |>
    dplyr::group_by(band) |>
    dplyr::summarise(x = TF[order(TF)][which(cumsum(w[order(TF)]) >= sum(w) / 2)[1]],
                     .groups = "drop") |>
    dplyr::mutate(
      y    = stats::approx(dens$x, dens$y, xout = x)$y * 0.5,
      text = paste0(band, ": ", a1_pct(a1_band_share[as.character(band)]), "%")
    )
  p <- ggplot2::ggplot(dd, ggplot2::aes(x, y)) +
    ggplot2::geom_area(ggplot2::aes(fill = band), alpha = 0.55, position = "identity") +
    ggplot2::geom_line(colour = "grey20") +
    ggplot2::geom_vline(xintercept = c(a1_cut_severe, a1_cut_moderate),
                        colour = "grey20", linetype = "dashed")
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_label_repel(
      data = lab, ggplot2::aes(x = x, y = y, label = text),
      nudge_y = top - lab$y, direction = "x", min.segment.length = 0,
      segment.colour = "grey40", size = 2.8, family = "Arial",
      label.size = 0.15, fill = "white", seed = 1
    )
  } else {
    # Without ggrepel: stagger the labels vertically so they cannot overlap.
    p <- p + ggplot2::geom_label(
      data = dplyr::mutate(lab, y = top * c(0.80, 1.00, 0.80)),
      ggplot2::aes(x = x, y = y, label = text),
      size = 2.8, family = "Arial", label.size = 0.15, fill = "white"
    )
  }
  p +
    ggplot2::scale_fill_manual(values = c("#D55E00", "#E69F00", "#56B4E9"), guide = "none") +
    ggplot2::scale_y_continuous("Density", expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::xlab("Demographically adjusted T score") +
    ggplot2::theme_minimal(base_size = 10, base_family = "Arial")
}

# Table A3: decision rules with participants classified by each rule --------------
a1_table_rules_data <- tibble::tribble(
  ~Route, ~`Cognitive criterion`, ~`Second criterion`, ~Classification, ~n,
  "Cognitive tests", paste0("T < ", a1_cut(a1_cut_severe)), "Memory worse than 2 years ago*", "Dementia", a1_flow$d1,
  "Cognitive tests", paste0("T < ", a1_cut(a1_cut_severe)), "Memory better or the same", "MCI", a1_flow$d2,
  "Cognitive tests", paste0(a1_cut(a1_cut_severe), " ≤ T < ", a1_cut(a1_cut_moderate)), "Memory worse than 2 years ago*", "MCI", a1_flow$d3,
  "Cognitive tests", paste0(a1_cut(a1_cut_severe), " ≤ T < ", a1_cut(a1_cut_moderate)), "Memory better or the same", "Normal", a1_flow$d4,
  "Cognitive tests", paste0("T ≥ ", a1_cut(a1_cut_moderate)), "Any", "Normal", a1_flow$b5,
  "Proxy interview", "No T score", paste0("IQCODE ≥ ", a1_iq(a1_jorm_severe)), "Dementia", a1_flow$c1,
  "Proxy interview", "No T score", paste0(a1_iq(a1_jorm_mild), " < IQCODE < ", a1_iq(a1_jorm_severe)), "MCI", a1_flow$c3,
  "Proxy interview", "No T score", paste0("IQCODE ≤ ", a1_iq(a1_jorm_mild)), "Normal", a1_flow$c4
) |>
  dplyr::mutate(n = a1_n(n))

a1_table_rules <- a1_flextable(
  a1_table_rules_data,
  "Table A3. Core algorithm decision rules and participants classified by each rule, HRS-HCAP Wave 1",
  col_widths = c(1.15, 1.35, 2.0, 1.1, 0.6),
  paste0(
    "n: unweighted HRS-HCAP Wave 1 participants (N = ", a1_n(a1_flow$a1), "; Normal ", a1_n(a1_flow$e1),
    ", MCI ", a1_n(a1_flow$e2), ", Dementia ", a1_n(a1_flow$e3), "). ",
    "T: demographically adjusted T score (mean 50, SD 10 in the HRS-HCAP robust normative sample). ",
    "*Self-rated memory compared with 2 years ago (HRS item D102); don't know, refused, and not ascertained count as worse; missing values were singly imputed. ",
    "IQCODE: 16-item Informant Questionnaire on Cognitive Decline in the Elderly, mean item score (1-5), asked in proxy interviews."
  )
)

# Agreement in HRS-HCAP Wave 1 ---------------------------------------------------
a1_tab_mj_core <- a1_a7$tabs$three_class$algorithmic_hrs   # rows: Manly-Jones; cols: core
a1_tab_mj_lw   <- a1_a7$tabs$three_class$algorithmic_lw    # rows: Manly-Jones; cols: Langa-Weir
a1_n_agree     <- nrow(a1_a7$data$hcap_sample)

a1_capture <- function(tab) tab["Dementia", "Dementia"] / sum(tab["Dementia", ])
a1_ppv     <- function(tab) tab["Dementia", "Dementia"] / sum(tab[, "Dementia"])

a1_agree <- list(
  kappa_core       = a1_a7$summary$algorithmic_hrs_kappa,
  kappa_core_bin   = a1_a7$summary$algorithmic_hrs_binary_kappa,
  kappa_lw         = a1_a7$summary$algorithmic_lw_kappa,
  kappa_lw_bin     = a1_a7$summary$algorithmic_lw_binary_kappa,
  kappa_core_lw    = a1_a7$summary$lw_hrs_kappa,
  kappa_core_lw_bin = a1_a7$summary$lw_hrs_binary_kappa,
  capture_core     = a1_capture(a1_tab_mj_core),
  capture_lw       = a1_capture(a1_tab_mj_lw),
  ppv_core         = a1_ppv(a1_tab_mj_core),
  ppv_lw           = a1_ppv(a1_tab_mj_lw),
  share_core       = colSums(a1_tab_mj_core) / sum(a1_tab_mj_core),
  share_lw         = colSums(a1_tab_mj_lw) / sum(a1_tab_mj_lw),
  share_mj         = rowSums(a1_tab_mj_core) / sum(a1_tab_mj_core)
)

# Table A4: agreement statistics ---------------------------------------------
a1_table_kappa_data <- tibble::tibble(
  Comparison = c("Core algorithm vs. Manly-Jones",
                 "Langa-Weir vs. Manly-Jones",
                 "Core algorithm vs. Langa-Weir"),
  `Kappa, 3-class (quadratic)` = a1_dec(c(a1_agree$kappa_core, a1_agree$kappa_lw, a1_agree$kappa_core_lw)),
  `Kappa, dementia vs. not` = a1_dec(c(a1_agree$kappa_core_bin, a1_agree$kappa_lw_bin, a1_agree$kappa_core_lw_bin)),
  `Manly-Jones dementia classified as dementia, %` = c(a1_pct(a1_agree$capture_core, 0), a1_pct(a1_agree$capture_lw, 0), "n/a"),
  `Comparator dementia that is Manly-Jones dementia, %` = c(a1_pct(a1_agree$ppv_core, 0), a1_pct(a1_agree$ppv_lw, 0), "n/a")
)

a1_table_kappa <- a1_flextable(
  a1_table_kappa_data,
  "Table A4. Agreement of the core algorithm and Langa-Weir with the Manly-Jones algorithm, HRS-HCAP Wave 1",
  col_widths = c(1.9, 1.0, 1.0, 1.3, 1.3),
  paste0(
    "N = ", a1_n(a1_n_agree), " HRS-HCAP Wave 1 participants with all 3 classifications; statistics weighted with HCAP Wave 1 (2016) sampling weight. ",
    "Core algorithm and Langa-Weir use 2016 HRS core data; the Manly-Jones algorithm uses HRS-HCAP data. ",
    "Kappa, 3-class: Normal, MCI, Dementia with quadratic weights (Cohen, 1968). ",
    "The last 2 columns treat the Manly-Jones classification as the reference; n/a, not applicable."
  )
)

a1_crosstab_data <- function(tab, col_title) {
  row_pct <- prop.table(tab, 1)
  out <- tibble::tibble(`Manly-Jones class` = rownames(tab))
  for (j in colnames(tab)) {
    out[[paste0("Comparator class: ", j)]] <-
      paste0(format_weighted_thousands(tab[, j]), " (", a1_pct(row_pct[, j], 0), "%)")
  }
  out[["Total"]] <- format_weighted_thousands(rowSums(tab))
  out
}

a1_table_agreement <- a1_flextable(
  col_widths = c(1.0, 0.9, 1.2, 1.2, 1.2, 0.7),
  data = dplyr::bind_rows(
    a1_crosstab_data(a1_tab_mj_core, "Core") |>
      dplyr::mutate(Comparator = "Core algorithm", .before = 1),
    a1_crosstab_data(a1_tab_mj_lw, "LW") |>
      dplyr::mutate(Comparator = "Langa-Weir", .before = 1)
  ),
  "Table A5. Weighted cross-classification of the core algorithm and Langa-Weir with the Manly-Jones algorithm, HRS-HCAP Wave 1",
  paste0(
    "Rows: Manly-Jones class (HRS-HCAP). Columns: comparator class (core algorithm or Langa-Weir) from 2016 HRS core data. ",
    weighted_thousands_note, " Row percentages in parentheses. N = ", a1_n(a1_n_agree),
    " participants with both classifications (HCAP Wave 1 (2016) sampling weight). Kappas are in Table A4."
  )
)

# Table A6: HRS-HCAP cohort person-waves, 2016-2022 ---------------------------
a1_waves <- a1_alg$hcap_cohort_wave_counts |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    total      = sum(n),
    tests      = sum(n[route == "Cognitive tests"]),
    iqcode     = sum(n[route == "Informant IQCODE"]),
    unclassed  = sum(n[route == "Not classified"]),
    normal     = sum(n[dx %in% "Normal"]),
    mci        = sum(n[dx %in% "MCI"]),
    dementia   = sum(n[dx %in% "Dementia"]),
    .groups = "drop"
  ) |>
  dplyr::mutate(classified = normal + mci + dementia)

a1_table_waves <- a1_flextable(
  col_widths = c(0.6, 0.75, 0.8, 0.7, 0.75, 1.05, 0.95, 0.95),
  data = a1_waves |>
    dplyr::transmute(
      `Core wave` = as.character(year),
      `Person-waves` = a1_n(total),
      `Cognitive tests` = a1_n(tests),
      `IQCODE only` = a1_n(iqcode),
      `Not classified` = a1_n(unclassed),
      Normal = paste0(a1_n(normal), " (", a1_pct(normal / classified), "%)"),
      MCI = paste0(a1_n(mci), " (", a1_pct(mci / classified), "%)"),
      Dementia = paste0(a1_n(dementia), " (", a1_pct(dementia / classified), "%)")
    ),
  "Table A6. Core algorithm classifications of HRS-HCAP Wave 1 participants interviewed in the HRS core, 2016-2022",
  paste(
    "Unweighted counts of person-waves; percentages among classified person-waves.",
    "Not classified: no cognitive tests and no IQCODE.",
    "Number series was not administered in the 2018 core interview, so 2018 T scores rest on 7 items."
  )
)

# Figure A3 and Table A7: EAP shrinkage without number series -------------------
a1_shrink     <- a1_cfa$eap_shrink
a1_shrink_sum <- a1_cfa$eap_shrink_summary
a1_ss <- function(g, v) a1_shrink_sum[[v]][a1_shrink_sum$group == g]

a1_shrink_figure <- function() {
  ggplot2::ggplot(a1_shrink, ggplot2::aes(F, colour = group, linetype = group)) +
    ggplot2::geom_density(linewidth = 0.7) +
    ggplot2::scale_colour_manual(NULL, values = c("#0072B2", "#D55E00")) +
    ggplot2::scale_linetype_manual(NULL, values = c("solid", "dashed")) +
    ggplot2::xlab("EAP factor score (fixed 2016 model)") +
    ggplot2::ylab("Density") +
    ggplot2::theme_minimal(base_size = 10, base_family = "Arial") +
    ggplot2::theme(legend.position = "top")
}

a1_table_shrink <- a1_flextable(
  a1_shrink_sum |>
    dplyr::transmute(
      `Core waves` = as.character(group),
      `Person-waves` = a1_n(n),
      Minimum = a1_dec(min), Q1 = a1_dec(q1), Median = a1_dec(median),
      Q3 = a1_dec(q3), Maximum = a1_dec(max),
      `SD of EAP` = a1_dec(sd),
      `Mean posterior SD` = a1_dec(mean_psd),
      `Var(EAP) + mean posterior variance` = a1_dec(total)
    ),
  "Table A7. EAP factor scores in core waves with and without number series",
  col_widths = c(0.9, 0.7, 0.55, 0.5, 0.55, 0.5, 0.6, 0.6, 0.75, 0.85),
  paste(
    "Core respondents aged 65 or older with a nonzero wave weight and at least 1 cognitive item; unweighted.",
    "2016 and 2020: number series administered; 2018: not administered. 2022 is omitted because number series is missing for most face-to-face interviews.",
    "Under the model, the variance of the EAP scores plus the mean posterior variance equals the factor variance (1), so a larger posterior SD implies a smaller spread of EAP scores (shrinkage toward the mean)."
  )
)

# Table A8: cognitive item coverage by wave ------------------------------------------
a1_cov_labels <- c(
  vdori = "Orientation to date", vdlfl1z = "Animal naming", vdlfl2 = "Object naming",
  vdlfl3 = "President and Vice President", vdwddelz = "Delayed word recall",
  vdexf7z = "Number series", vdsevens = "Serial 7 subtraction",
  vdcount = "Backward counting from 20", vdwdimmz = "Immediate word recall (not in model)",
  any_item = "Any cognitive item"
)
a1_cov <- a1_cfa$item_coverage_65
a1_cov_n <- a1_cov |> dplyr::distinct(year, n_respondents)

a1_table_coverage <- a1_flextable(
  a1_cov |>
    dplyr::mutate(
      Test = factor(a1_cov_labels[item], levels = a1_cov_labels),
      cell = paste0(a1_n(n_present), " (", a1_pct(p_present_weighted), "%)")
    ) |>
    dplyr::select(Test, year, cell) |>
    tidyr::pivot_wider(names_from = year, values_from = cell) |>
    dplyr::arrange(Test) |>
    dplyr::mutate(Test = as.character(Test)),
  "Table A8. Core cognitive tests with a score, by wave: number (weighted %)",
  col_widths = c(2.1, 1.1, 1.1, 1.1, 1.1),
  paste0(
    "Core respondents aged 65 or older with a nonzero wave weight (",
    paste0(a1_cov_n$year, ": n = ", a1_n(a1_cov_n$n_respondents), collapse = "; "),
    "). Counts are unweighted; percentages are weighted with the wave respondent weight. ",
    "Proxy interviews, which have no cognitive tests, are included in the denominator."
  )
)

# Endnote: functional limitations (A7_090) --------------------------------------
a1_fn <- a1_read("A7_090_function_check.rds")
a1_fn_none_mj   <- function(cls) a1_fn$floor_by_mj$pct_none[a1_fn$floor_by_mj$group == cls]
a1_fn_none_band <- a1_fn$floor_by_band$pct_none[1]          # severe band
a1_fn_var <- function(v, col) a1_fn$variants[[col]][a1_fn$variants$variant == v]
a1_sdec <- function(x, digits = 2) {
  x <- round(x, digits)                       # avoids "-0.00" for small negatives
  ifelse(x == 0, formatC(0, format = "f", digits = digits),
         formatC(x, format = "f", digits = digits, flag = "+"))
}
# Split-half: mean and 2.5th/97.5th percentiles of out-of-sample statistics
a1_sh  <- function(v, col) a1_fn$split_summary[[col]][a1_fn$split_summary$variant == v]
a1_shd <- function(v, stat, col) {
  a1_fn$split_diff[[col]][a1_fn$split_diff$variant == v & a1_fn$split_diff$stat == stat]
}
a1_shd_text <- function(v, stat) {
  paste0(a1_sdec(a1_shd(v, stat, "mean")), " (", a1_sdec(a1_shd(v, stat, "lo")),
         " to ", a1_sdec(a1_shd(v, stat, "hi")), ")")
}
# Endnote wording that depends on the direction of the function-rule difference
# (all rules at the core algorithm cut points; A7_090).
a1_fn_diff3 <- a1_fn_var("function", "kappa_3class") - a1_fn_var("memory", "kappa_3class")
a1_fn_lo3   <- a1_shd("function", "kappa_3class", "lo")
a1_fn_hi3   <- a1_shd("function", "kappa_3class", "hi")
a1_fn_effect_text <- if (a1_fn_diff3 > 0 && a1_fn_diff3 < 0.05) {
  "raised agreement by a small amount"
} else if (a1_fn_diff3 >= 0.05) "raised agreement" else "did not raise agreement"
a1_fn_halves_text <- if (a1_fn_diff3 > 0) {
  if (a1_fn_lo3 >= 0) "the gain held in nearly every random half of the sample" else
    "the gain did not hold consistently across random halves of the sample"
} else if (a1_fn_hi3 <= 0) "this held in nearly every random half of the sample" else
  "the difference was not consistent across random halves of the sample"
a1_fn_conclusion_text <- if (a1_fn_diff3 > 0 && a1_fn_lo3 >= 0) {
  "The gain is therefore small but consistent across halves, and our decision rests on the third reason."
} else if (a1_fn_diff3 > 0) {
  "The gain is therefore small and uncertain."
} else {
  "With the cut points held fixed, adding function did not improve agreement, which supports the memory rule apart from the third reason."
}

a1_table_floor <- a1_flextable(
  dplyr::bind_rows(
    a1_fn$floor_by_mj   |> dplyr::mutate(Grouping = "Manly-Jones class"),
    a1_fn$floor_by_band |> dplyr::mutate(Grouping = "Core T-score band")
  ) |>
    dplyr::transmute(
      Grouping, Group = as.character(group), n = a1_n(n),
      `None, %` = a1_pct(pct_none), `1, %` = a1_pct(pct_one),
      `2 or more, %` = a1_pct(pct_twoplus), `Missing, %` = a1_pct(pct_missing)
    ),
  "Table A9. Self-reported ADL and IADL limitations by Manly-Jones class and by core T-score band, HRS-HCAP Wave 1",
  col_widths = c(1.4, 1.4, 0.6, 0.75, 0.6, 0.85, 0.8),
  paste0(
    "Participants with a core T score (n = ", a1_n(a1_fn$n_with_T), "). Number of limitations in activities of daily living (ADL) ",
    "and instrumental activities of daily living (IADL) reported in the 2016 core interview, counted on the ", a1_fn$n_limit_items,
    " items asked in every wave from 2016 to 2022 (dressing, bathing, eating, and using the toilet; preparing meals, ",
    "shopping for groceries, using the telephone, taking medications, and managing money). Missing: no response to any of these items. ",
    "Percentages are weighted (HCAP Wave 1 (2016) sampling weight)."
  )
)

a1_table_variants <- a1_flextable(
  a1_fn$variants |>
    dplyr::transmute(
      `Rule for the second criterion` = label,
      `Severe cut` = a1_cut(cut_severe), `Moderate cut` = a1_cut(cut_moderate),
      `MCI, %` = a1_pct(share_mci), `Dementia, %` = a1_pct(share_dementia),
      `Kappa, 3-class` = a1_dec(kappa_3class), `Kappa, dementia` = a1_dec(kappa_binary),
      `Manly-Jones dementia classified as dementia, %` = a1_pct(capture, 0)
    ),
  "Table A10. Agreement with the Manly-Jones algorithm when functional limitations enter the decision rules, HRS-HCAP Wave 1",
  col_widths = c(2.0, 0.55, 0.65, 0.55, 0.65, 0.6, 0.65, 0.95),
  paste0(
    "N = ", a1_n(a1_fn$n), ". All rules use the core algorithm's cut points (T < ", a1_cut(a1_cut_severe), " and T < ", a1_cut(a1_cut_moderate), "), so the rules differ in class shares as well as agreement. ",
    "Function: 1 or more ADL or IADL limitations on the items in Table A9 (missing counted as none). Memory: memory worse than 2 years ago. ",
    "Statistics are weighted (HCAP Wave 1 (2016) sampling weight); kappa, 3-class uses quadratic weights. ",
    "Table A11 shows how these statistics vary across random halves of the sample."
  )
)

a1_ci <- function(m, lo, hi) paste0(a1_dec(m), " (", a1_dec(lo), ", ", a1_dec(hi), ")")
a1_ci_s <- function(m, lo, hi) paste0(a1_sdec(m), " (", a1_sdec(lo), ", ", a1_sdec(hi), ")")
a1_table_split <- a1_flextable(
  a1_fn$split_summary |>
    dplyr::left_join(
      a1_fn$split_diff |>
        tidyr::pivot_wider(id_cols = variant, names_from = stat,
                           values_from = c(mean, lo, hi, p_higher)),
      by = "variant"
    ) |>
    dplyr::left_join(dplyr::distinct(a1_fn$variants, variant, label), by = "variant") |>
    dplyr::mutate(variant = factor(variant, c("memory", "function", "function_or_memory"))) |>
    dplyr::arrange(variant) |>
    dplyr::transmute(
      `Rule for the second criterion` = label,
      `Kappa, 3-class` = a1_ci(kappa_3class_mean, kappa_3class_lo, kappa_3class_hi),
      `Difference from memory rule` = ifelse(is.na(mean_kappa_3class), "n/a",
        a1_ci_s(mean_kappa_3class, lo_kappa_3class, hi_kappa_3class)),
      `Halves higher, %` = ifelse(is.na(p_higher_kappa_3class), "n/a", a1_pct(p_higher_kappa_3class, 0)),
      `Kappa, dementia` = a1_ci(kappa_binary_mean, kappa_binary_lo, kappa_binary_hi),
      `Difference from memory rule ` = ifelse(is.na(mean_kappa_binary), "n/a",
        a1_ci_s(mean_kappa_binary, lo_kappa_binary, hi_kappa_binary)),
      `Halves higher, % ` = ifelse(is.na(p_higher_kappa_binary), "n/a", a1_pct(p_higher_kappa_binary, 0))
    ),
  "Table A11. Agreement with the Manly-Jones algorithm in repeated random halves, HRS-HCAP Wave 1",
  col_widths = c(1.7, 0.95, 1.05, 0.6, 0.95, 1.05, 0.6),
  paste0(
    a1_fn$n_split, " random halves of the sample, drawn within Manly-Jones class. In each half, agreement was computed for each rule ",
    "at the core algorithm's cut points (T < ", a1_cut(a1_cut_severe), " and T < ", a1_cut(a1_cut_moderate), "). Entries are the mean across halves ",
    "(2.5th and 97.5th percentiles). Differences are paired within half. The halves overlap, so the percentiles ",
    "describe variation across halves and are not confidence intervals. Statistics are weighted (HCAP Wave 1 (2016) sampling weight); kappa, 3-class uses quadratic weights."
  )
)

a1_numser_2018 <- a1_cfa$item_coverage_by_wave |>
  dplyr::filter(wave == "Q") |>
  dplyr::pull(vdexf7z)
