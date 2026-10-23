# Parameter transfer uses H5RESULTS only (References/Mplus_Rules.md).
# The pure table-to-syntax helper also supports tests with synthetic tables.
format_mplus_number <- function(x) sprintf("%.15g", x)

# Normalize H5 parameter tables for report formatting, without parsing .out.
pmm_h5_parameter_table <- function(results) {
  d <- tibble::as_tibble(results)
  cl <- trimws(as.character(d$Class))
  keep <- grepl("^Latent Class [123]( |$)", cl)
  d <- d[keep, , drop = FALSE]
  section <- tolower(trimws(as.character(d$Section)))
  section[is.na(section)] <- ""
  statement <- toupper(gsub("[[:space:]]+", " ", trimws(d$Statement)))
  header <- ifelse(nzchar(section), section, sub(" [^ ]+$", "", statement))
  parameter <- ifelse(nzchar(section), statement, sub("^.* ", "", statement))
  estimate_column <- names(d)[tolower(gsub("[[:space:].]", "", names(d))) %in%
                               c("estimate", "stdyxestimate")]
  if (length(estimate_column) != 1L || !all(c("S.E.", "Estimate/S.E.") %in% names(d))) {
    stop("Unexpected H5 parameter-table schema.")
  }
  header <- vapply(strsplit(tolower(header), " +"), function(x) {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2), collapse = ".")
  }, character(1))
  # MplusAutomation-compatible labels used by existing report selectors.
  header <- sub("\\.By$", ".BY", header)
  header <- sub("\\.On$", ".ON", header)
  header <- sub("\\.With$", ".WITH", header)
  tibble::tibble(paramHeader = header, param = parameter,
    est = d[[estimate_column]], se = d[["S.E."]], est_se = d[["Estimate/S.E."]],
    LatentClass = sub("^Latent Class ([123]).*", "\\1", cl[keep]))
}

pmm_fixed_model_from_h5_results <- function(results, mplus_mod_norms = "") {
  required <- c("Class", "Section", "Statement", "Estimate")
  if (!all(required %in% names(results))) stop("H5 model results lack required columns.")
  cl <- trimws(as.character(results$Class))
  keep <- grepl("^Latent Class [123]( |$)", cl)
  # Accept the H5 labels 'Latent Class 1' and 'Latent Class 1 (1)'.
  d <- results[keep, , drop = FALSE]
  d$class <- as.integer(sub("^Latent Class ([123]).*", "\\1", cl[keep]))
  d$Section <- tolower(trimws(as.character(d$Section)))
  d$Statement <- tolower(gsub("[[:space:]]+", " ", trimws(d$Statement)))
  d$Section[is.na(d$Section)] <- ""
  # Reuse the fixed normative regressions; retain H5 loadings, residual
  # covariances, means, thresholds, and variances at full available precision.
  d <- d[!grepl(" on x[1-7](x[1-7])?$", d$Statement), , drop = FALSE]
  if (!setequal(unique(d$class), 1:3) || any(!is.finite(d$Estimate))) {
    stop("H5 results must contain finite parameters for classes 1, 2, and 3.")
  }
  allowed <- c("", "means", "intercepts", "thresholds", "variances", "residual variances")
  if (any(!d$Section %in% allowed)) stop("Unsupported section in H5 model results.")
  d$key <- paste(d$Section, d$Statement)
  if (anyDuplicated(paste(d$class, d$key))) stop("Duplicate class parameters in H5 results.")
  keys <- lapply(1:3, function(k) sort(d$key[d$class == k]))
  if (!identical(keys[[1]], keys[[2]]) || !identical(keys[[1]], keys[[3]])) {
    stop("H5 class parameter sets differ; fixed scoring would be incomplete.")
  }
  value <- format_mplus_number(d$Estimate)
  bracket <- d$Section %in% c("means", "intercepts", "thresholds")
  d$syntax <- ifelse(bracket,
    paste0("[ ", d$Statement, " @ ", value, " ];"),
    paste0(d$Statement, " @ ", value, ";"))
  # Declare the factor and complete baseline model in OVERALL. Mplus needs
  # the BY structure there; differing class values then override the baseline.
  baseline <- d[d$class == 1, , drop = FALSE]
  varies <- vapply(baseline$key, function(key) {
    length(unique(d$Estimate[d$key == key])) > 1L
  }, logical(1))
  varying_keys <- baseline$key[varies]
  blocks <- vapply(1:3, function(k) {
    statements <- d$syntax[d$class == k & d$key %in% varying_keys]
    if (!length(statements)) return("")
    paste(c(paste0("%c#", k, "%"), statements), collapse = "\n")
  }, character(1))
  paste(c("%OVERALL%", mplus_mod_norms, baseline$syntax, blocks), collapse = "\n")
}

write_lca_model <- function(h5_path, mplus_mod_norms = "") {
  if (!is.character(h5_path) || length(h5_path) != 1L || !file.exists(h5_path)) {
    stop("write_lca_model() requires an existing H5RESULTS path, not an .out model object.")
  }
  results <- mplush5::mplus.print.model.results(h5_path)
  pmm_fixed_model_from_h5_results(results, mplus_mod_norms)
}

# weighted_class_logits(inhcap, "vs1hcapdxeap", "HCAP16WGTR")
# Returns an Mplus statement that fixes the class logits at the survey-weighted
# class proportions of the known-class variable, with the last class as the
# reference, e.g. "[ c#1 @ 2.019 c#2 @ 0.842 ];".
# Used because Mplus KNOWNCLASS estimates of the class logits reproduce the
# unweighted class proportions even when WEIGHT is specified.
weighted_class_logits <- function(data, class_var, weight_var, fixed_class = "c") {
  w <- data[[weight_var]]
  if (any(!is.finite(w)) || any(w <= 0) || anyNA(data[[class_var]])) {
    stop("Class priors require complete labels and finite positive weights.")
  }
  props <- data %>%
    filter(!is.na(.data[[class_var]]), !is.na(.data[[weight_var]])) %>%
    group_by(class = .data[[class_var]]) %>%
    summarise(w = sum(.data[[weight_var]]), .groups = "drop") %>%
    arrange(class) %>%
    mutate(p = w / sum(w))

  if (!identical(as.numeric(haven::zap_labels(props$class)), as.numeric(1:3)) ||
      any(!is.finite(props$w)) || any(props$w <= 0)) {
    stop("Expected class codes 1, 2, 3 with finite positive class weight totals.")
  }

  k <- nrow(props)
  logits <- log(props$p[-k] / props$p[k])
  str_c("[ ", str_c(fixed_class, "#", seq_len(k - 1), " @ ",
                    format_mplus_number(logits), collapse = " "), " ];")
}


# Indicators of the cognition model (PMM_102 and PMM_103), 2026-10-22.
# Mplus drops participants with every indicator missing (the participants who
# did not complete cognitive testing), so the cognition model's class priors are
# computed on participants with at least 1 of these observed.
pmm_cog_indicators <- c("vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwddelz",
                        "vdexf7z", "vdsevens", "vdcount", "PD102")

# Stops if the number of participants used for the priors differs from the
# number of observations Mplus analyzed.
check_prior_sample <- function(prior_data, mod, label) {
  n_mplus <- mod[["summaries"]][["Observations"]]
  if (is.null(n_mplus) || nrow(prior_data) != n_mplus) {
    stop(label, ": priors computed on ", nrow(prior_data),
         " participants, but Mplus analyzed ", n_mplus, ".")
  }
  invisible(TRUE)
}

# Validate each fixed scoring run independently of printed parameter precision.
pmm_check_h5_parameter_transfer <- function(calibration, scoring, tolerance = 1e-7) {
  key_table <- function(d) {
    cl <- trimws(as.character(d$Class))
    keep <- grepl("^Latent Class [123]( |$)", cl)
    section <- tolower(trimws(as.character(d$Section[keep])))
    section[is.na(section)] <- ""
    key <- paste(sub("^Latent Class ([123]).*", "\\1", cl[keep]), section,
                 tolower(gsub("[[:space:]]+", " ", trimws(d$Statement[keep]))))
    if (!length(key) || anyDuplicated(key)) stop("Invalid H5 parameter keys.")
    stats::setNames(d$Estimate[keep], key)
  }
  a <- key_table(calibration)
  b <- key_table(scoring)
  if (!setequal(names(a), names(b))) stop("Calibration and scoring H5 parameter sets differ.")
  b <- b[names(a)]
  if (any(!is.finite(a)) || any(!is.finite(b)) ||
      any(abs(a - b) > tolerance * pmax(1, abs(a)))) {
    stop("Fixed scoring did not preserve the H5 calibration parameters.")
  }
  invisible(TRUE)
}

check_fixed_scoring_run <- function(path, expected_n, calibration_h5, scoring_h5) {
  output <- readLines(path, warn = FALSE)
  mod <- suppressWarnings(MplusAutomation::readModels(path, what = "summaries"))
  if (!any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", output, fixed = TRUE)) ||
      !identical(as.numeric(mod$summaries$Parameters), 0) ||
      !identical(as.numeric(mod$summaries$Observations), as.numeric(expected_n))) {
    stop("Fixed scoring failed, estimated parameters, or analyzed an unexpected sample: ", path)
  }
  pmm_check_h5_parameter_transfer(mplush5::mplus.print.model.results(calibration_h5),
                                  mplush5::mplus.print.model.results(scoring_h5))
  invisible(TRUE)
}
