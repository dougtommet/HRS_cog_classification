PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))

class_labels <- c("Normal", "MCI", "Dementia")
binary_labels <- c("Non-demented", "Demented")
agreement_methods <- c(
  "Consensus diagnosis",
  "Algorithmic diagnosis",
  "Langa-Weir",
  "Combined model"
)

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

weighted_kappa_from_tab <- function(tab) {
  obs <- unclass(tab)
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)

  agreement_weights <- outer(
    seq_len(nrow(obs)),
    seq_len(ncol(obs)),
    function(i, j) 1 - ((i - j)^2 / (nrow(obs) - 1)^2)
  )

  po <- sum(agreement_weights * obs) / sum(obs)
  pe <- sum(agreement_weights * expected) / sum(obs)
  kappa_w <- (po - pe) / (1 - pe)

  tibble::tibble(
    observed_weighted_agreement = po,
    expected_weighted_agreement = pe,
    quadratic_weighted_kappa = kappa_w
  )
}

binary_kappa_from_tab <- function(tab) {
  obs <- unclass(tab)
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)

  po <- sum(diag(obs)) / sum(obs)
  pe <- sum(diag(expected)) / sum(obs)
  kappa <- (po - pe) / (1 - pe)

  tibble::tibble(
    observed_agreement = po,
    expected_agreement = pe,
    kappa = kappa
  )
}

format_weighted_tab <- function(tab, row_name, stubhead, spanner) {
  tab_df <- tab |>
    as.data.frame.matrix() |>
    tibble::rownames_to_column(row_name)

  tab_df[["Total"]] <- tab_df[["Normal"]] + tab_df[["MCI"]] + tab_df[["Dementia"]]

  total_row <- tab_df |>
    dplyr::summarise(dplyr::across(dplyr::all_of(c("Normal", "MCI", "Dementia", "Total")), sum))

  total_row[[row_name]] <- "Total"
  total_row <- total_row |>
    dplyr::select(dplyr::all_of(c(row_name, "Normal", "MCI", "Dementia", "Total")))

  dplyr::bind_rows(tab_df, total_row) |>
    gt::gt(rowname_col = row_name) |>
    gt::tab_spanner(spanner, columns = 2:4) |>
    gt::tab_stubhead(stubhead)
}

format_binary_weighted_tab <- function(tab, row_name, stubhead, spanner) {
  tab_df <- tab |>
    as.data.frame.matrix() |>
    tibble::rownames_to_column(row_name)

  tab_df[["Total"]] <- tab_df[["Non-demented"]] + tab_df[["Demented"]]

  total_row <- tab_df |>
    dplyr::summarise(dplyr::across(dplyr::all_of(c("Non-demented", "Demented", "Total")), sum))

  total_row[[row_name]] <- "Total"
  total_row <- total_row |>
    dplyr::select(dplyr::all_of(c(row_name, "Non-demented", "Demented", "Total")))

  dplyr::bind_rows(tab_df, total_row) |>
    gt::gt(rowname_col = row_name) |>
    gt::tab_spanner(spanner, columns = 2:3) |>
    gt::tab_stubhead(stubhead)
}

build_agreement_matrix <- function(kappas, metric_name) {
  agreement_matrix <- matrix(
    NA_real_,
    nrow = length(agreement_methods),
    ncol = length(agreement_methods),
    dimnames = list(agreement_methods, agreement_methods)
  )

  diag(agreement_matrix) <- 1
  agreement_matrix["Consensus diagnosis", "Algorithmic diagnosis"] <- kappas$algorithmic_consensus[[metric_name]]
  agreement_matrix["Algorithmic diagnosis", "Consensus diagnosis"] <- kappas$algorithmic_consensus[[metric_name]]
  agreement_matrix["Consensus diagnosis", "Langa-Weir"] <- kappas$lw_consensus[[metric_name]]
  agreement_matrix["Langa-Weir", "Consensus diagnosis"] <- kappas$lw_consensus[[metric_name]]
  agreement_matrix["Consensus diagnosis", "Combined model"] <- kappas$consensus_combined[[metric_name]]
  agreement_matrix["Combined model", "Consensus diagnosis"] <- kappas$consensus_combined[[metric_name]]
  agreement_matrix["Algorithmic diagnosis", "Langa-Weir"] <- kappas$algorithmic_lw[[metric_name]]
  agreement_matrix["Langa-Weir", "Algorithmic diagnosis"] <- kappas$algorithmic_lw[[metric_name]]
  agreement_matrix["Algorithmic diagnosis", "Combined model"] <- kappas$algorithmic_combined[[metric_name]]
  agreement_matrix["Combined model", "Algorithmic diagnosis"] <- kappas$algorithmic_combined[[metric_name]]
  agreement_matrix["Langa-Weir", "Combined model"] <- kappas$lw_combined[[metric_name]]
  agreement_matrix["Combined model", "Langa-Weir"] <- kappas$lw_combined[[metric_name]]

  agreement_matrix
}

make_binary_sample <- function(data) {
  consensus_num <- as.integer(data[["consensus"]])
  algorithmic_num <- as.integer(data[["algorithmic"]])
  langa_weir_num <- as.integer(data[["langa_weir"]])

  data |>
    dplyr::mutate(
      consensus_binary = factor(
        dplyr::if_else(consensus_num == 3L, 2L, 1L),
        levels = 1:2,
        labels = binary_labels,
        ordered = TRUE
      ),
      algorithmic_binary = factor(
        dplyr::if_else(algorithmic_num == 3L, 2L, 1L),
        levels = 1:2,
        labels = binary_labels,
        ordered = TRUE
      ),
      langa_weir_binary = factor(
        dplyr::if_else(langa_weir_num == 3L, 2L, 1L),
        levels = 1:2,
        labels = binary_labels,
        ordered = TRUE
      )
    )
}

hcap_sample <- PMM_100 |>
  dplyr::filter(
    inHCAP == 1,
    !is.na(cogfunction2016),
    !is.na(vs1hcapdxeap),
    !is.na(HCAP16WGTR)
  ) |>
  dplyr::transmute(
    id = as.integer(id),
    consensus = haven::zap_labels(consensuspaneldx),
    algorithmic = haven::zap_labels(vs1hcapdxeap),
    langa_weir = haven::zap_labels(cogfunction2016),
    hcap_w = HCAP16WGTR,
    samplingP = samplingP,
    consensus_wt_raw = dplyr::if_else(!is.na(samplingP), HCAP16WGTR * samplingP^(-1), NA_real_)
  ) |>
  dplyr::mutate(
    consensus = factor(consensus, levels = 1:3, labels = class_labels, ordered = TRUE),
    algorithmic = factor(algorithmic, levels = 1:3, labels = class_labels, ordered = TRUE),
    langa_weir = factor(langa_weir, levels = 1:3, labels = class_labels, ordered = TRUE)
  )

consensus_sample <- hcap_sample |>
  dplyr::filter(
    !is.na(consensus),
    !is.na(samplingP)
  ) |>
  dplyr::mutate(
    consensus_wt = 100 * consensus_wt_raw / sum(consensus_wt_raw, na.rm = TRUE)
  )

hcap_sample_binary <- make_binary_sample(hcap_sample)
consensus_sample_binary <- make_binary_sample(consensus_sample)

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

combined_cprob <- cognition_cprob |>
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

hcap_sample_combined <- hcap_sample |>
  dplyr::inner_join(combined_cprob, by = "id")

consensus_sample_combined <- consensus_sample |>
  dplyr::inner_join(combined_cprob, by = "id")

hcap_sample_combined_rep <- hcap_sample_combined |>
  dplyr::slice(rep(seq_len(dplyr::n()), each = 100)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    i = dplyr::row_number(),
    p1 = cprob1_combined * 100,
    p2 = cprob2_combined * 100 + p1,
    combined_model = dplyr::case_when(
      i <= p1 ~ 1L,
      i <= p2 ~ 2L,
      TRUE ~ 3L
    ),
    hcap_w100 = hcap_w / 100
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    combined_model = factor(combined_model, levels = 1:3, labels = class_labels, ordered = TRUE)
  )

consensus_sample_combined_rep <- consensus_sample_combined |>
  dplyr::slice(rep(seq_len(dplyr::n()), each = 100)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    i = dplyr::row_number(),
    p1 = cprob1_combined * 100,
    p2 = cprob2_combined * 100 + p1,
    combined_model = dplyr::case_when(
      i <= p1 ~ 1L,
      i <= p2 ~ 2L,
      TRUE ~ 3L
    ),
    consensus_wt100 = consensus_wt / 100
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    combined_model = factor(combined_model, levels = 1:3, labels = class_labels, ordered = TRUE)
  )

 hcap_sample_combined_binary <- make_binary_sample(hcap_sample_combined_rep) |>
  dplyr::mutate(
    combined_model_binary = factor(
      dplyr::if_else(as.integer(combined_model) == 3L, 2L, 1L),
      levels = 1:2,
      labels = binary_labels,
      ordered = TRUE
    )
  )

consensus_sample_combined_binary <- make_binary_sample(consensus_sample_combined_rep) |>
  dplyr::mutate(
    combined_model_binary = factor(
      dplyr::if_else(as.integer(combined_model) == 3L, 2L, 1L),
      levels = 1:2,
      labels = binary_labels,
      ordered = TRUE
    )
  )

tabs_3class <- list(
  lw_consensus = xtabs(consensus_wt ~ langa_weir + consensus, data = consensus_sample) |>
    round(1),
  algorithmic_consensus = xtabs(consensus_wt ~ algorithmic + consensus, data = consensus_sample) |>
    round(1),
  algorithmic_lw = xtabs(hcap_w ~ algorithmic + langa_weir, data = hcap_sample) |>
    round(1),
  consensus_combined = xtabs(consensus_wt100 ~ consensus + combined_model, data = consensus_sample_combined_rep) |>
    round(1),
  algorithmic_combined = xtabs(hcap_w100 ~ algorithmic + combined_model, data = hcap_sample_combined_rep) |>
    round(1),
  lw_combined = xtabs(hcap_w100 ~ langa_weir + combined_model, data = hcap_sample_combined_rep) |>
    round(1)
)

kappas_3class <- list(
  lw_consensus = weighted_kappa_from_tab(tabs_3class$lw_consensus),
  algorithmic_consensus = weighted_kappa_from_tab(tabs_3class$algorithmic_consensus),
  algorithmic_lw = weighted_kappa_from_tab(tabs_3class$algorithmic_lw),
  consensus_combined = weighted_kappa_from_tab(tabs_3class$consensus_combined),
  algorithmic_combined = weighted_kappa_from_tab(tabs_3class$algorithmic_combined),
  lw_combined = weighted_kappa_from_tab(tabs_3class$lw_combined)
)

tabs_binary <- list(
  lw_consensus = xtabs(consensus_wt ~ langa_weir_binary + consensus_binary, data = consensus_sample_binary) |>
    round(1),
  algorithmic_consensus = xtabs(consensus_wt ~ algorithmic_binary + consensus_binary, data = consensus_sample_binary) |>
    round(1),
  algorithmic_lw = xtabs(hcap_w ~ algorithmic_binary + langa_weir_binary, data = hcap_sample_binary) |>
    round(1),
  consensus_combined = xtabs(consensus_wt100 ~ consensus_binary + combined_model_binary, data = consensus_sample_combined_binary) |>
    round(1),
  algorithmic_combined = xtabs(hcap_w100 ~ algorithmic_binary + combined_model_binary, data = hcap_sample_combined_binary) |>
    round(1),
  lw_combined = xtabs(hcap_w100 ~ langa_weir_binary + combined_model_binary, data = hcap_sample_combined_binary) |>
    round(1)
)

kappas_binary <- list(
  lw_consensus = binary_kappa_from_tab(tabs_binary$lw_consensus),
  algorithmic_consensus = binary_kappa_from_tab(tabs_binary$algorithmic_consensus),
  algorithmic_lw = binary_kappa_from_tab(tabs_binary$algorithmic_lw),
  consensus_combined = binary_kappa_from_tab(tabs_binary$consensus_combined),
  algorithmic_combined = binary_kappa_from_tab(tabs_binary$algorithmic_combined),
  lw_combined = binary_kappa_from_tab(tabs_binary$lw_combined)
)

agreement_matrix_3class <- build_agreement_matrix(kappas_3class, "quadratic_weighted_kappa")
agreement_matrix_binary <- build_agreement_matrix(kappas_binary, "kappa")

agreement_matrix_table <- agreement_matrix_3class |>
  as.data.frame() |>
  tibble::rownames_to_column("method") |>
  gt::gt(rowname_col = "method") |>
  gt::fmt_number(columns = agreement_methods, decimals = 3) |>
  gt::tab_stubhead("Consensus sample")

agreement_matrix_binary_table <- agreement_matrix_binary |>
  as.data.frame() |>
  tibble::rownames_to_column("method") |>
  gt::gt(rowname_col = "method") |>
  gt::fmt_number(columns = agreement_methods, decimals = 3) |>
  gt::tab_stubhead("Consensus sample")

PMM_110 <- list(
  data = list(
    hcap_sample = hcap_sample,
    hcap_sample_binary = hcap_sample_binary,
    consensus_sample = consensus_sample,
    consensus_sample_binary = consensus_sample_binary,
    hcap_sample_combined = hcap_sample_combined,
    hcap_sample_combined_rep = hcap_sample_combined_rep,
    hcap_sample_combined_binary = hcap_sample_combined_binary,
    consensus_sample_combined = consensus_sample_combined,
    consensus_sample_combined_rep = consensus_sample_combined_rep,
    consensus_sample_combined_binary = consensus_sample_combined_binary
  ),
  tabs = list(
    three_class = tabs_3class,
    binary = tabs_binary
  ),
  kappas = list(
    three_class = kappas_3class,
    binary = kappas_binary
  ),
  matrices = list(
    three_class = agreement_matrix_3class,
    binary = agreement_matrix_binary
  ),
  tables = list(
    lw_consensus = format_weighted_tab(
      tabs_3class$lw_consensus,
      row_name = "langa_weir",
      stubhead = "Langa-Weir classification",
      spanner = "Consensus panel diagnosis"
    ),
    algorithmic_consensus = format_weighted_tab(
      tabs_3class$algorithmic_consensus,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "Consensus panel diagnosis"
    ),
    algorithmic_lw = format_weighted_tab(
      tabs_3class$algorithmic_lw,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "Langa-Weir diagnosis"
    ),
    consensus_combined = format_weighted_tab(
      tabs_3class$consensus_combined,
      row_name = "consensus",
      stubhead = "Consensus panel diagnosis",
      spanner = "Combined model classification"
    ),
    algorithmic_combined = format_weighted_tab(
      tabs_3class$algorithmic_combined,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "Combined model classification"
    ),
    lw_combined = format_weighted_tab(
      tabs_3class$lw_combined,
      row_name = "langa_weir",
      stubhead = "Langa-Weir classification",
      spanner = "Combined model classification"
    ),
    agreement_matrix = agreement_matrix_table,
    lw_consensus_binary = format_binary_weighted_tab(
      tabs_binary$lw_consensus,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "Consensus panel diagnosis"
    ),
    algorithmic_consensus_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_consensus,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "Consensus panel diagnosis"
    ),
    algorithmic_lw_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_lw,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "Langa-Weir diagnosis"
    ),
    consensus_combined_binary = format_binary_weighted_tab(
      tabs_binary$consensus_combined,
      row_name = "consensus_binary",
      stubhead = "Consensus panel diagnosis",
      spanner = "Combined model classification"
    ),
    algorithmic_combined_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_combined,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "Combined model classification"
    ),
    lw_combined_binary = format_binary_weighted_tab(
      tabs_binary$lw_combined,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "Combined model classification"
    ),
    agreement_matrix_binary = agreement_matrix_binary_table
  ),
  summary = list(
    lw_consensus_kappa = kappas_3class$lw_consensus$quadratic_weighted_kappa,
    algorithmic_consensus_kappa = kappas_3class$algorithmic_consensus$quadratic_weighted_kappa,
    algorithmic_lw_kappa = kappas_3class$algorithmic_lw$quadratic_weighted_kappa,
    consensus_combined_kappa = kappas_3class$consensus_combined$quadratic_weighted_kappa,
    algorithmic_combined_kappa = kappas_3class$algorithmic_combined$quadratic_weighted_kappa,
    lw_combined_kappa = kappas_3class$lw_combined$quadratic_weighted_kappa,
    lw_consensus_binary_kappa = kappas_binary$lw_consensus$kappa,
    algorithmic_consensus_binary_kappa = kappas_binary$algorithmic_consensus$kappa,
    algorithmic_lw_binary_kappa = kappas_binary$algorithmic_lw$kappa,
    consensus_combined_binary_kappa = kappas_binary$consensus_combined$kappa,
    algorithmic_combined_binary_kappa = kappas_binary$algorithmic_combined$kappa,
    lw_combined_binary_kappa = kappas_binary$lw_combined$kappa
  ),
  notes = list(
    consensus_standard = paste(
      "Note: This comparison uses the HRS/HCAP validation subsample.",
      "Weights are computed as HCAP16WGTR * samplingP^(-1) and are normalized within the consensus sample."
    ),
    hcap_standard = paste(
      "Note: This comparison uses the HCAP sample.",
      "Weights are the HCAP population weights HCAP16WGTR.",
      "This weighting differs from the weighting used for tables involving the consensus diagnosis."
    ),
    consensus_combined = paste(
      "Note: This comparison uses the HRS/HCAP validation subsample and the combined-model classification.",
      "Combined-model classifications are generated from the 100-replication sample implied by the combined-model class probabilities.",
      "The table weights are the consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the consensus sample and divided by 100 across replications."
    ),
    hcap_combined = paste(
      "Note: This comparison uses the HCAP sample and the combined-model classification.",
      "Combined-model classifications are generated from the 100-replication sample implied by the combined-model class probabilities.",
      "The table weights are the HCAP population weights HCAP16WGTR divided by 100 across replications.",
      "This is not the same weighting used for the Langa-Weir versus algorithmic comparison, which uses the non-replicated HCAP weights."
    ),
    matrix_three = paste(
      "Note: This matrix combines pairwise quadratic weighted kappas computed under different weighting schemes.",
      "Entries involving the consensus diagnosis use consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the validation subsample.",
      "Entries not involving the consensus diagnosis use HCAP population weights HCAP16WGTR.",
      "Entries involving the combined model are computed on the 100-replication sample and therefore use replicated consensus-sample or replicated HCAP weights as appropriate."
    ),
    matrix_binary = paste(
      "Note: This matrix combines pairwise kappas for dementia versus non-dementia computed under different weighting schemes.",
      "Entries involving the consensus diagnosis use consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the validation subsample.",
      "Entries not involving the consensus diagnosis use HCAP population weights HCAP16WGTR.",
      "Entries involving the combined model are computed on the 100-replication sample and therefore use replicated consensus-sample or replicated HCAP weights as appropriate."
    )
  ),
  labels = list(
    class = class_labels,
    binary = binary_labels,
    methods = agreement_methods
  )
)