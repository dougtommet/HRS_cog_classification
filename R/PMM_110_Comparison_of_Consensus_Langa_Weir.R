source(here::here("R", "PMM_090_model_registry.R"))
source(here::here("R", "PMM_109_probability_tools.R"))

PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))

class_labels <- c("Normal", "MCI", "Dementia")
binary_labels <- c("Non-demented", "Demented")
agreement_methods <- c(
  "Consensus diagnosis",
  "Manly-Jones algorithm",
  "Langa-Weir",
  "PMM"
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

format_pmm_cells <- function(table, units) {
  if (units == "percent") return(gt::fmt_number(table, columns = where(is.numeric), decimals = 1))
  table |>
    gt::fmt(columns = where(is.numeric), fns = function(x) {
      v <- x / 1000
      ifelse(v != 0 & abs(v) < 1, format(signif(v, 2), trim = TRUE, scientific = FALSE),
             format(round(v), big.mark = ",", trim = TRUE, scientific = FALSE))
    }) |>
    gt::tab_source_note("Cell entries are survey-weighted population counts in thousands.")
}

format_weighted_tab <- function(tab, row_name, stubhead, spanner, units = "thousands") {
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
    gt::tab_stubhead(stubhead) |>
    format_pmm_cells(units)
}

format_binary_weighted_tab <- function(tab, row_name, stubhead, spanner, units = "thousands") {
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
    gt::tab_stubhead(stubhead) |>
    format_pmm_cells(units)
}

build_agreement_matrix <- function(kappas, metric_name) {
  agreement_matrix <- matrix(
    NA_real_,
    nrow = length(agreement_methods),
    ncol = length(agreement_methods),
    dimnames = list(agreement_methods, agreement_methods)
  )

  diag(agreement_matrix) <- 1
  agreement_matrix["Consensus diagnosis", "Manly-Jones algorithm"] <- kappas$algorithmic_consensus[[metric_name]]
  agreement_matrix["Manly-Jones algorithm", "Consensus diagnosis"] <- kappas$algorithmic_consensus[[metric_name]]
  agreement_matrix["Consensus diagnosis", "Langa-Weir"] <- kappas$lw_consensus[[metric_name]]
  agreement_matrix["Langa-Weir", "Consensus diagnosis"] <- kappas$lw_consensus[[metric_name]]
  agreement_matrix["Consensus diagnosis", "PMM"] <- kappas$consensus_combined[[metric_name]]
  agreement_matrix["PMM", "Consensus diagnosis"] <- kappas$consensus_combined[[metric_name]]
  agreement_matrix["Manly-Jones algorithm", "Langa-Weir"] <- kappas$algorithmic_lw[[metric_name]]
  agreement_matrix["Langa-Weir", "Manly-Jones algorithm"] <- kappas$algorithmic_lw[[metric_name]]
  agreement_matrix["Manly-Jones algorithm", "PMM"] <- kappas$algorithmic_combined[[metric_name]]
  agreement_matrix["PMM", "Manly-Jones algorithm"] <- kappas$algorithmic_combined[[metric_name]]
  agreement_matrix["Langa-Weir", "PMM"] <- kappas$lw_combined[[metric_name]]
  agreement_matrix["PMM", "Langa-Weir"] <- kappas$lw_combined[[metric_name]]

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

if (anyDuplicated(cognition_cprob$id) || anyDuplicated(jorm_cprob$id) ||
    length(intersect(cognition_cprob$id, jorm_cprob$id))) {
  stop("PMM outputs must have unique IDs and mutually exclusive scoring routes.")
}

combined_cprob <- cognition_cprob |>
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

expected_ids <- as.numeric(PMM_100$id[PMM_100$inHCAP == 1])
if (!setequal(combined_cprob$id, expected_ids)) stop("Incomplete or unexpected HCAP scoring coverage.")
prob_cols <- c("cprob1_combined", "cprob2_combined", "cprob3_combined")
combined_cprob[, prob_cols] <- as.data.frame(pmm_normalize_probabilities(combined_cprob[, prob_cols]))

hcap_sample_combined <- hcap_sample |>
  dplyr::inner_join(combined_cprob, by = "id")

consensus_sample_combined <- consensus_sample |>
  dplyr::inner_join(combined_cprob, by = "id")

# Probability allocation uses exact weighted sums, without synthetic records.
hcap_sample_combined_binary <- make_binary_sample(hcap_sample_combined)
consensus_sample_combined_binary <- make_binary_sample(consensus_sample_combined)

tabs_3class <- list(
  lw_consensus = xtabs(consensus_wt ~ langa_weir + consensus, data = consensus_sample),
  algorithmic_consensus = xtabs(consensus_wt ~ algorithmic + consensus, data = consensus_sample),
  algorithmic_lw = xtabs(hcap_w ~ algorithmic + langa_weir, data = hcap_sample),
  consensus_combined = pmm_probability_table(consensus_sample_combined, "consensus", "consensus_wt"),
  algorithmic_combined = pmm_probability_table(hcap_sample_combined, "algorithmic", "hcap_w"),
  lw_combined = pmm_probability_table(hcap_sample_combined, "langa_weir", "hcap_w")
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
  lw_consensus = xtabs(consensus_wt ~ langa_weir_binary + consensus_binary, data = consensus_sample_binary),
  algorithmic_consensus = xtabs(consensus_wt ~ algorithmic_binary + consensus_binary, data = consensus_sample_binary),
  algorithmic_lw = xtabs(hcap_w ~ algorithmic_binary + langa_weir_binary, data = hcap_sample_binary),
  consensus_combined = pmm_probability_table(consensus_sample_combined_binary, "consensus_binary", "consensus_wt", binary = TRUE),
  algorithmic_combined = pmm_probability_table(hcap_sample_combined_binary, "algorithmic_binary", "hcap_w", binary = TRUE),
  lw_combined = pmm_probability_table(hcap_sample_combined_binary, "langa_weir_binary", "hcap_w", binary = TRUE)
)

kappas_binary <- list(
  lw_consensus = binary_kappa_from_tab(tabs_binary$lw_consensus),
  algorithmic_consensus = binary_kappa_from_tab(tabs_binary$algorithmic_consensus),
  algorithmic_lw = binary_kappa_from_tab(tabs_binary$algorithmic_lw),
  consensus_combined = binary_kappa_from_tab(tabs_binary$consensus_combined),
  algorithmic_combined = binary_kappa_from_tab(tabs_binary$algorithmic_combined),
  lw_combined = binary_kappa_from_tab(tabs_binary$lw_combined)
)

modal_agreement <- function(data, reference, weight) {
  d <- data
  d$modal <- factor(d$c_combined, levels = 1:3, labels = class_labels)
  ref <- d[[reference]]
  w <- d[[weight]]
  tab <- stats::xtabs(w ~ ref + modal, data = d)
  dem_ref <- factor(as.integer(ref) == 3L, levels = c(FALSE, TRUE), labels = binary_labels)
  dem_modal <- factor(d$c_combined == 3L, levels = c(FALSE, TRUE), labels = binary_labels)
  tab_dem <- stats::xtabs(w ~ dem_ref + dem_modal, data = d)
  c(three_class = weighted_kappa_from_tab(tab)$quadratic_weighted_kappa,
    dementia = binary_kappa_from_tab(tab_dem)$kappa)
}
modal_kappas <- list(
  Manly_Jones = modal_agreement(hcap_sample_combined, "algorithmic", "hcap_w"),
  Langa_Weir = modal_agreement(hcap_sample_combined, "langa_weir", "hcap_w"),
  Consensus = modal_agreement(consensus_sample_combined, "consensus", "consensus_wt")
)
modal_agreement_table <- tibble::tibble(
  Comparator = c("Manly-Jones algorithm", "Langa-Weir", "Consensus diagnosis"),
  `Three-class kappa` = vapply(modal_kappas, function(x) x[["three_class"]], numeric(1)),
  `Dementia kappa` = vapply(modal_kappas, function(x) x[["dementia"]], numeric(1))
) |> gt::gt() |> gt::fmt_number(columns = where(is.numeric), decimals = 3) |>
  gt::tab_source_note("Most likely PMM class; HCAP16WGTR, or normalized consensus-sample weights for the consensus comparison.")

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
    hcap_sample_combined_binary = hcap_sample_combined_binary,
    consensus_sample_combined = consensus_sample_combined,
    consensus_sample_combined_binary = consensus_sample_combined_binary
  ),
  modal_kappas = modal_kappas,
  modal_agreement_table = modal_agreement_table,
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
      spanner = "Consensus panel diagnosis", units = "percent"
    ),
    algorithmic_consensus = format_weighted_tab(
      tabs_3class$algorithmic_consensus,
      row_name = "algorithmic",
      stubhead = "Manly-Jones algorithm",
      spanner = "Consensus panel diagnosis", units = "percent"
    ),
    algorithmic_lw = format_weighted_tab(
      tabs_3class$algorithmic_lw,
      row_name = "algorithmic",
      stubhead = "Manly-Jones algorithm",
      spanner = "Langa-Weir diagnosis"
    ),
    consensus_combined = format_weighted_tab(
      tabs_3class$consensus_combined,
      row_name = "consensus",
      stubhead = "Consensus panel diagnosis",
      spanner = "PMM classification", units = "percent"
    ),
    algorithmic_combined = format_weighted_tab(
      tabs_3class$algorithmic_combined,
      row_name = "algorithmic",
      stubhead = "Manly-Jones algorithm",
      spanner = "PMM classification"
    ),
    lw_combined = format_weighted_tab(
      tabs_3class$lw_combined,
      row_name = "langa_weir",
      stubhead = "Langa-Weir classification",
      spanner = "PMM classification"
    ),
    agreement_matrix = agreement_matrix_table,
    lw_consensus_binary = format_binary_weighted_tab(
      tabs_binary$lw_consensus,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "Consensus panel diagnosis", units = "percent"
    ),
    algorithmic_consensus_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_consensus,
      row_name = "algorithmic_binary",
      stubhead = "Manly-Jones algorithm",
      spanner = "Consensus panel diagnosis", units = "percent"
    ),
    algorithmic_lw_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_lw,
      row_name = "algorithmic_binary",
      stubhead = "Manly-Jones algorithm",
      spanner = "Langa-Weir diagnosis"
    ),
    consensus_combined_binary = format_binary_weighted_tab(
      tabs_binary$consensus_combined,
      row_name = "consensus_binary",
      stubhead = "Consensus panel diagnosis",
      spanner = "PMM classification", units = "percent"
    ),
    algorithmic_combined_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_combined,
      row_name = "algorithmic_binary",
      stubhead = "Manly-Jones algorithm",
      spanner = "PMM classification"
    ),
    lw_combined_binary = format_binary_weighted_tab(
      tabs_binary$lw_combined,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "PMM classification"
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
      "Note: This comparison uses the HRS/HCAP validation subsample and the PMM classification.",
      "Each PMM cell is the sum of the survey weight times the posterior class probability.",
      "Weights are HCAP16WGTR * samplingP^(-1), normalized to sum to 100 within the consensus sample. Cells are weighted percentages."
    ),
    hcap_combined = paste(
      "Note: This comparison uses the HCAP sample and the PMM classification.",
      "Each PMM cell is the sum of the survey weight times the posterior class probability.",
      "Weights are HCAP16WGTR, as in the other full-HCAP comparisons."
    ),
    matrix_three = paste(
      "Note: This matrix combines pairwise quadratic weighted kappas computed under different weighting schemes.",
      "Entries involving the consensus diagnosis use consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the validation subsample.",
      "Entries not involving the consensus diagnosis use HCAP population weights HCAP16WGTR.",
      "PMM entries use direct probability allocation with the corresponding survey weights; no replicated records."
    ),
    matrix_binary = paste(
      "Note: This matrix combines pairwise kappas for dementia versus non-dementia computed under different weighting schemes.",
      "Entries involving the consensus diagnosis use consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the validation subsample.",
      "Entries not involving the consensus diagnosis use HCAP population weights HCAP16WGTR.",
      "PMM entries use direct probability allocation with the corresponding survey weights; no replicated records."
    )
  ),
  labels = list(
    class = class_labels,
    binary = binary_labels,
    methods = agreement_methods
  )
)