

hrs16_hcap                 <- readRDS(here::here("R_objects", "A7_075_hrs16_hcap.rds"))

class_labels <- c("Normal", "MCI", "Dementia")
binary_labels <- c("Non-demented", "Demented")
agreement_methods <- c(
  "Consensus diagnosis",
  "Algorithmic diagnosis",
  "Langa-Weir",
  "HRS classification model",
  "Hudomiet"
)


####################################################
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
    gt::tab_stubhead(stubhead) %>%
    gt::fmt_number(decimals = 0)
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
    gt::tab_stubhead(stubhead) %>%
    gt::fmt_number(decimals = 0)
}

build_agreement_matrix <- function(kappas, metric_name) {
  agreement_matrix <- matrix(
    NA_real_,
    nrow = length(agreement_methods),
    ncol = length(agreement_methods),
    dimnames = list(agreement_methods, agreement_methods)
  )

  diag(agreement_matrix) <- 1
  agreement_matrix["Consensus diagnosis", "Algorithmic diagnosis"]      <- kappas$algorithmic_consensus[[metric_name]]
  agreement_matrix["Algorithmic diagnosis", "Consensus diagnosis"]      <- kappas$algorithmic_consensus[[metric_name]]
  agreement_matrix["Consensus diagnosis", "Langa-Weir"]                 <- kappas$lw_consensus[[metric_name]]
  agreement_matrix["Langa-Weir", "Consensus diagnosis"]                 <- kappas$lw_consensus[[metric_name]]
  agreement_matrix["Consensus diagnosis", "HRS classification model"]   <- kappas$consensus_hrs[[metric_name]]
  agreement_matrix["HRS classification model", "Consensus diagnosis"]   <- kappas$consensus_hrs[[metric_name]]
  agreement_matrix["Consensus diagnosis", "Hudomiet"]                   <- kappas$consensus_hudomiet[[metric_name]]
  agreement_matrix["Hudomiet", "Consensus diagnosis"]                   <- kappas$consensus_hudomiet[[metric_name]]

  agreement_matrix["Algorithmic diagnosis", "Langa-Weir"]               <- kappas$algorithmic_lw[[metric_name]]
  agreement_matrix["Langa-Weir", "Algorithmic diagnosis"]               <- kappas$algorithmic_lw[[metric_name]]
  agreement_matrix["Algorithmic diagnosis", "HRS classification model"] <- kappas$algorithmic_hrs[[metric_name]]
  agreement_matrix["HRS classification model", "Algorithmic diagnosis"] <- kappas$algorithmic_hrs[[metric_name]]
  agreement_matrix["Algorithmic diagnosis", "Hudomiet"]                 <- kappas$algorithmic_hudomiet[[metric_name]]
  agreement_matrix["Hudomiet", "Algorithmic diagnosis"]                 <- kappas$algorithmic_hudomiet[[metric_name]]

  agreement_matrix["Langa-Weir", "HRS classification model"]            <- kappas$lw_hrs[[metric_name]]
  agreement_matrix["HRS classification model", "Langa-Weir"]            <- kappas$lw_hrs[[metric_name]]
  agreement_matrix["Langa-Weir", "Hudomiet"]                            <- kappas$lw_hudomiet[[metric_name]]
  agreement_matrix["Hudomiet", "Langa-Weir"]                            <- kappas$lw_hudomiet[[metric_name]]

  agreement_matrix["HRS classification model", "Hudomiet"]              <- kappas$hrs_hudomiet[[metric_name]]
  agreement_matrix["Hudomiet", "HRS classification model"]              <- kappas$hrs_hudomiet[[metric_name]]

  agreement_matrix
}

make_binary_sample <- function(data) {
  consensus_num   <- as.integer(data[["consensus"]])
  algorithmic_num <- as.integer(data[["algorithmic"]])
  langa_weir_num  <- as.integer(data[["langa_weir"]])
  hrs_num         <- as.integer(data[["hrs"]])
  hudomiet_num    <- as.integer(data[["hudomiet"]])

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
      ),
      hrs_binary = factor(
        dplyr::if_else(hrs_num == 3L, 2L, 1L),
        levels = 1:2,
        labels = binary_labels,
        ordered = TRUE
      ),
      hudomiet_binary = factor(
        dplyr::if_else(hudomiet_num == 3L, 2L, 1L),
        levels = 1:2,
        labels = binary_labels,
        ordered = TRUE
      )
    )
}
####################################################
hcap_sample <- hrs16_hcap |>
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
    hrs = haven::zap_labels(dx_v1),
    hudomiet = haven::zap_labels(Hudomiet_classification),
    hcap_w = HCAP16WGTR,
    samplingP = samplingP,
    consensus_wt_raw = dplyr::if_else(!is.na(samplingP), HCAP16WGTR * samplingP^(-1), NA_real_)
  ) |>
  dplyr::mutate(
    consensus   = factor(consensus,   levels = 1:3, labels = class_labels, ordered = TRUE),
    algorithmic = factor(algorithmic, levels = 1:3, labels = class_labels, ordered = TRUE),
    langa_weir  = factor(langa_weir,  levels = 1:3, labels = class_labels, ordered = TRUE),
    hrs         = factor(hrs,         levels = 1:3, labels = class_labels, ordered = TRUE),
    hudomiet    = factor(hudomiet,    levels = 1:3, labels = class_labels, ordered = TRUE)
  )

consensus_sample <- hcap_sample |>
  dplyr::filter(
    !is.na(consensus),
    !is.na(samplingP)
  ) |>
  dplyr::mutate(
    consensus_wt = 100 * consensus_wt_raw / sum(consensus_wt_raw, na.rm = TRUE)
  )

hcap_sample_binary      <- make_binary_sample(hcap_sample)
consensus_sample_binary <- make_binary_sample(consensus_sample)








tabs_3class <- list(
  algorithmic_consensus = xtabs(consensus_wt ~ algorithmic + consensus, data = consensus_sample) |>
    round(1),
  lw_consensus = xtabs(consensus_wt ~ langa_weir + consensus, data = consensus_sample) |>
    round(1),
  consensus_hrs = xtabs(consensus_wt ~ consensus + hrs, data = consensus_sample) |>
    round(1),
  consensus_hudomiet = xtabs(consensus_wt ~ consensus + hudomiet, data = consensus_sample) |>
    round(1),

  algorithmic_lw = xtabs(hcap_w ~ algorithmic + langa_weir, data = hcap_sample) |>
    round(1),

  algorithmic_hrs = xtabs(hcap_w ~ algorithmic + hrs, data = hcap_sample) |>
    round(1),
  algorithmic_hudomiet = xtabs(hcap_w ~ algorithmic + hudomiet, data = hcap_sample) |>
    round(1),

  lw_hrs = xtabs(hcap_w ~ langa_weir + hrs, data = hcap_sample) |>
    round(1),
  lw_hudomiet = xtabs(hcap_w ~ langa_weir + hudomiet, data = hcap_sample) |>
    round(1),

  hrs_hudomiet = xtabs(hcap_w ~ hrs + hudomiet, data = hcap_sample) |>
    round(1)

)

kappas_3class <- list(
  algorithmic_consensus = weighted_kappa_from_tab(tabs_3class$algorithmic_consensus),
  lw_consensus          = weighted_kappa_from_tab(tabs_3class$lw_consensus),
  consensus_hrs         = weighted_kappa_from_tab(tabs_3class$consensus_hrs),
  consensus_hudomiet    = weighted_kappa_from_tab(tabs_3class$consensus_hudomiet),

  algorithmic_lw        = weighted_kappa_from_tab(tabs_3class$algorithmic_lw),
  algorithmic_hrs       = weighted_kappa_from_tab(tabs_3class$algorithmic_hrs),
  algorithmic_hudomiet  = weighted_kappa_from_tab(tabs_3class$algorithmic_hudomiet),

  lw_hrs                = weighted_kappa_from_tab(tabs_3class$lw_hrs),
  lw_hudomiet           = weighted_kappa_from_tab(tabs_3class$lw_hudomiet),

  hrs_hudomiet          = weighted_kappa_from_tab(tabs_3class$hrs_hudomiet)
)

tabs_binary <- list(
  algorithmic_consensus = xtabs(consensus_wt ~ algorithmic_binary + consensus_binary, data = consensus_sample_binary) |>
    round(1),
  lw_consensus = xtabs(consensus_wt ~ langa_weir_binary + consensus_binary, data = consensus_sample_binary) |>
    round(1),
  consensus_hrs = xtabs(consensus_wt ~ consensus_binary + hrs_binary, data = consensus_sample_binary) |>
    round(1),
  consensus_hudomiet = xtabs(consensus_wt ~ consensus_binary + hudomiet_binary, data = consensus_sample_binary) |>
    round(1),

  algorithmic_lw = xtabs(hcap_w ~ algorithmic_binary + langa_weir_binary, data = hcap_sample_binary) |>
    round(1),
  algorithmic_hrs = xtabs(hcap_w ~ algorithmic_binary + hrs_binary, data = hcap_sample_binary) |>
    round(1),
  algorithmic_hudomiet = xtabs(hcap_w ~ algorithmic_binary + hudomiet_binary, data = hcap_sample_binary) |>
    round(1),

  lw_hrs = xtabs(hcap_w ~ langa_weir_binary + hrs_binary, data = hcap_sample_binary) |>
    round(1),
  lw_hudomiet = xtabs(hcap_w ~ langa_weir_binary + hudomiet_binary, data = hcap_sample_binary) |>
    round(1),

  hrs_hudomiet = xtabs(hcap_w ~ hrs_binary + hudomiet_binary, data = hcap_sample_binary) |>
    round(1)
)

kappas_binary <- list(
  algorithmic_consensus = binary_kappa_from_tab(tabs_binary$algorithmic_consensus),
  lw_consensus          = binary_kappa_from_tab(tabs_binary$lw_consensus),
  consensus_hrs         = binary_kappa_from_tab(tabs_binary$consensus_hrs),
  consensus_hudomiet    = binary_kappa_from_tab(tabs_binary$consensus_hudomiet),

  algorithmic_lw        = binary_kappa_from_tab(tabs_binary$algorithmic_lw),
  algorithmic_hrs       = binary_kappa_from_tab(tabs_binary$algorithmic_hrs),
  algorithmic_hudomiet  = binary_kappa_from_tab(tabs_binary$algorithmic_hudomiet),

  lw_hrs                = binary_kappa_from_tab(tabs_binary$lw_hrs),
  lw_hudomiet           = binary_kappa_from_tab(tabs_binary$lw_hudomiet),

  hrs_hudomiet          = binary_kappa_from_tab(tabs_binary$hrs_hudomiet)
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




hcap_tables <- list(
  data = list(
    hcap_sample = hcap_sample,
    hcap_sample_binary = hcap_sample_binary,
    consensus_sample = consensus_sample,
    consensus_sample_binary = consensus_sample_binary
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
    algorithmic_consensus = format_weighted_tab(
      tabs_3class$algorithmic_consensus,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "Consensus panel diagnosis"
    ),
    lw_consensus = format_weighted_tab(
      tabs_3class$lw_consensus,
      row_name = "langa_weir",
      stubhead = "Langa-Weir classification",
      spanner = "Consensus panel diagnosis"
    ),
    consensus_hrs = format_weighted_tab(
      tabs_3class$consensus_hrs,
      row_name = "consensus",
      stubhead = "Consensus panel diagnosis",
      spanner = "HRS classification model"
    ),
    consensus_hudomiet = format_weighted_tab(
      tabs_3class$consensus_hudomiet,
      row_name = "consensus",
      stubhead = "Consensus panel diagnosis",
      spanner = "Hudomiet"
    ),

    algorithmic_lw = format_weighted_tab(
      tabs_3class$algorithmic_lw,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "Langa-Weir diagnosis"
    ),

    algorithmic_hrs = format_weighted_tab(
      tabs_3class$algorithmic_hrs,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "HRS classification model"
    ),
    algorithmic_hudomiet = format_weighted_tab(
      tabs_3class$algorithmic_hudomiet,
      row_name = "algorithmic",
      stubhead = "Algorithmic diagnosis",
      spanner = "Hudomiet"
    ),
    lw_hrs = format_weighted_tab(
      tabs_3class$lw_hrs,
      row_name = "langa_weir",
      stubhead = "Langa-Weir classification",
      spanner = "HRS classification model"
    ),
    lw_hudomiet = format_weighted_tab(
      tabs_3class$lw_hudomiet,
      row_name = "langa_weir",
      stubhead = "Langa-Weir classification",
      spanner = "Hudomiet"
    ),
    hrs_hudomiet = format_weighted_tab(
      tabs_3class$hrs_hudomiet,
      row_name = "HRS",
      stubhead = "HRS classification model",
      spanner = "Hudomiet"
    ),
    agreement_matrix = agreement_matrix_table,

    algorithmic_consensus_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_consensus,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "Consensus panel diagnosis"
    ),
    lw_consensus_binary = format_binary_weighted_tab(
      tabs_binary$lw_consensus,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "Consensus panel diagnosis"
    ),
    consensus_hrs_binary = format_binary_weighted_tab(
      tabs_binary$consensus_hrs,
      row_name = "consensus_binary",
      stubhead = "Consensus panel diagnosis",
      spanner = "HRS classification model"
    ),
    consensus_hudomiet_binary = format_binary_weighted_tab(
      tabs_binary$consensus_hudomiet,
      row_name = "consensus_binary",
      stubhead = "Consensus panel diagnosis",
      spanner = "Hudomiet"
    ),

    algorithmic_lw_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_lw,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "Langa-Weir diagnosis"
    ),
    algorithmic_hrs_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_hrs,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "HRS classification model"
    ),
    algorithmic_hudomiet_binary = format_binary_weighted_tab(
      tabs_binary$algorithmic_hudomiet,
      row_name = "algorithmic_binary",
      stubhead = "Algorithmic diagnosis",
      spanner = "Hudomiet"
    ),
    lw_hrs_binary = format_binary_weighted_tab(
      tabs_binary$lw_hrs,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "HRS classification model"
    ),
    lw_hudomiet_binary = format_binary_weighted_tab(
      tabs_binary$lw_hudomiet,
      row_name = "langa_weir_binary",
      stubhead = "Langa-Weir classification",
      spanner = "Hudomiet"
    ),
    hrs_hudomiet_binary = format_binary_weighted_tab(
      tabs_binary$hrs_hudomiet,
      row_name = "hrs_binary",
      stubhead = "HRS classification model",
      spanner = "Hudomiet"
    ),
    agreement_matrix_binary = agreement_matrix_binary_table
  ),
  summary = list(
    algorithmic_consensus_kappa        = kappas_3class$algorithmic_consensus$quadratic_weighted_kappa,
    lw_consensus_kappa                 = kappas_3class$lw_consensus$quadratic_weighted_kappa,
    consensus_hrs_kappa                = kappas_3class$consensus_hrs$quadratic_weighted_kappa,
    consensus_hudomiet_kappa           = kappas_3class$consensus_hudomiet$quadratic_weighted_kappa,
    algorithmic_lw_kappa               = kappas_3class$algorithmic_lw$quadratic_weighted_kappa,
    algorithmic_hrs_kappa              = kappas_3class$algorithmic_hrs$quadratic_weighted_kappa,
    algorithmic_hudomiet_kappa         = kappas_3class$algorithmic_hudomiet$quadratic_weighted_kappa,
    lw_hrs_kappa                       = kappas_3class$lw_hrs$quadratic_weighted_kappa,
    lw_hudomiet_kappa                  = kappas_3class$lw_hudomiet$quadratic_weighted_kappa,
    hrs_hudomiet_kappa                 = kappas_3class$hrs_hudomiet$quadratic_weighted_kappa,
    algorithmic_consensus_binary_kappa = kappas_binary$algorithmic_consensus$kappa,
    lw_consensus_binary_kappa          = kappas_binary$lw_consensus$kappa,
    consensus_hrs_binary_kappa         = kappas_binary$consensus_hrs$kappa,
    consensus_hudomiet_binary_kappa    = kappas_binary$consensus_hudomiet$kappa,
    algorithmic_lw_binary_kappa        = kappas_binary$algorithmic_lw$kappa,
    algorithmic_hrs_binary_kappa       = kappas_binary$algorithmic_hrs$kappa,
    algorithmic_hudomiet_binary_kappa  = kappas_binary$algorithmic_hudomiet$kappa,
    lw_hrs_binary_kappa                = kappas_binary$lw_hrs$kappa,
    lw_hudomiet_binary_kappa           = kappas_binary$lw_hudomiet$kappa,
    hrs_hudomiet_binary_kappa          = kappas_binary$hrs_hudomiet$kappa
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
    consensus_hrs = paste(
      "Note: This comparison uses the HRS/HCAP validation subsample.",
      "Weights are the consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the consensus sample."
    ),
    hcap_hrs = paste(
      "Note: This comparison uses the HCAP sample.",
      "The table weights are the HCAP population weights HCAP16WGTR."
    ),
    matrix_three = paste(
      "Note: This matrix combines pairwise quadratic weighted kappas computed under different weighting schemes.",
      "Entries involving the consensus diagnosis use consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the validation subsample.",
      "Entries not involving the consensus diagnosis use HCAP population weights HCAP16WGTR."
    ),
    matrix_binary = paste(
      "Note: This matrix combines pairwise kappas for dementia versus non-dementia computed under different weighting schemes.",
      "Entries involving the consensus diagnosis use consensus-sample weights HCAP16WGTR * samplingP^(-1), normalized within the validation subsample.",
      "Entries not involving the consensus diagnosis use HCAP population weights HCAP16WGTR."
    )
  ),
  labels = list(
    class = class_labels,
    binary = binary_labels,
    methods = agreement_methods
  )
)

saveRDS(hcap_tables, here::here("R_objects", "A7_100_hcap_tables.rds"))
