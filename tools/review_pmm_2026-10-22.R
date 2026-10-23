#!/usr/bin/env Rscript
# USER-RUN ONLY. Agents must not execute this script or read participant files.
if (!"--local-data" %in% commandArgs(trailingOnly = TRUE)) {
  stop("Participant-data analysis: run locally yourself with --local-data; agents must not run this script.")
}
# Read-only audit of existing A3/A3_2 results. Never refits a model or writes
# participant records. The only output is an aggregate JSON review artifact.
# Run from the project root: Rscript tools/review_pmm_2026-10-22.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
source(here::here("R", "PMM_090_model_registry.R"))

read_model <- function(path) suppressWarnings(MplusAutomation::readModels(
  path, what = c("summaries", "parameters", "class_counts", "savedata", "warn_err")))
base <- haven::zap_labels(readRDS(here::here("R_objects", "PMM_100.RDS"))) |>
  filter(inHCAP == 1) |>
  mutate(id = as.numeric(id), truth = as.integer(vs1hcapdxeap))
mods <- lapply(pmm_models, function(m) list(cal = read_model(m$calibration_out),
                                          score = read_model(m$scoring_out)))
probs <- function(m, name) {
  d <- janitor::clean_names(as.data.frame(m$score$savedata))
  d |> transmute(id = as.numeric(id), p1 = cprob1, p2 = cprob2,
                 p3 = cprob3, modal = mlcc, model = name)
}
cog <- probs(mods$cognition, "cognition")
inf <- probs(mods$informant, "informant")
combined <- bind_rows(inf, anti_join(cog, inf, by = "id"))
stopifnot(!anyDuplicated(base$id), !anyDuplicated(combined$id))
d <- base |> left_join(combined, by = "id")
stopifnot(nrow(d) == nrow(base), !anyNA(d$p1), !anyNA(d$truth),
          all(is.finite(d$HCAP16WGTR)), all(d$HCAP16WGTR > 0))

share <- function(P, w) 100 * colSums(P * w) / sum(w)
truth_P <- function(x) outer(x, 1:3, "==") * 1
P <- as.matrix(d[, c("p1", "p2", "p3")])
# Exactly reproduces the i=1,...,100 threshold comparisons, including their
# floating-point behavior, without generating participant replicas.
allocation <- function(P) {
  a1 <- vapply(100 * P[, 1], function(x) sum(seq_len(100) <= x), integer(1))
  a12 <- vapply(100 * P[, 1] + 100 * P[, 2], function(x) sum(seq_len(100) <= x), integer(1))
  cbind(a1, a12 - a1, 100 - a12) / 100
}
kappa <- function(tab) {
  w <- 1 - outer(seq_len(nrow(tab)), seq_len(ncol(tab)), "-")^2 / (nrow(tab) - 1)^2
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  po <- sum(w * tab) / sum(tab)
  pe <- sum(w * expected) / sum(tab)
  (po - pe) / (1 - pe)
}
agreement <- function(truth, P, w, round_cells = FALSE) {
  tab <- crossprod(truth_P(truth), P * w)
  dem_tab <- rbind(colSums(tab[1:2, , drop = FALSE]), tab[3, ])
  dem_tab <- cbind(rowSums(dem_tab[, 1:2, drop = FALSE]), dem_tab[, 3])
  if (round_cells) { tab <- round(tab, 1); dem_tab <- round(dem_tab, 1) }
  c(three_class = kappa(tab), dementia = kappa(dem_tab))
}
audit_model <- function(m, pp) {
  s <- inner_join(base, pp, by = "id")
  p <- as.matrix(s[, c("p1", "p2", "p3")])
  pars <- function(mod) as.data.frame(mod$parameters$unstandardized) |>
    filter(LatentClass %in% 1:3) |>
    select(paramHeader, param, LatentClass, est)
  cmp <- full_join(pars(m$cal), pars(m$score),
                   by = c("paramHeader", "param", "LatentClass"), suffix = c(".cal", ".score"))
  list(n = nrow(s), scoring_free_parameters = m$score$summaries$Parameters,
       prior_pct_from_Mplus = 100 * m$score$class_counts$modelEstimated$proportion,
       reference_pct = share(truth_P(s$truth), s$HCAP16WGTR),
       posterior_pct_saved = share(p, s$HCAP16WGTR),
       posterior_pct_Mplus = 100 * m$score$class_counts$posteriorProb$proportion,
       modal_pct = share(truth_P(s$modal), s$HCAP16WGTR),
       n_parameters_compared = nrow(cmp), missing_parameter_matches = sum(!complete.cases(cmp)),
       max_parameter_difference = max(abs(cmp$est.cal - cmp$est.score), na.rm = TRUE),
       posterior_row_sum_range = range(rowSums(p)),
       n_MCI_over_half = sum(p[, 2] > .5), max_MCI = max(p[, 2]),
       n_modal_disagrees_with_rounded_max = sum(max.col(p, ties.method = "first") != s$modal))
}
A <- allocation(P)
normP <- P / rowSums(P)
comparison <- d |> filter(!is.na(cogfunction2016), !is.na(vs1hcapdxeap), !is.na(HCAP16WGTR))
compare_one <- function(ref, weight, subset_rows = rep(TRUE, nrow(comparison))) {
  s <- comparison[subset_rows, ]
  p <- as.matrix(s[, c("p1", "p2", "p3")])
  w <- s[[weight]]
  list(n = nrow(s),
       exact_saved = agreement(as.integer(s[[ref]]), p, w),
       exact_normalized = agreement(as.integer(s[[ref]]), p / rowSums(p), w),
       replicated = agreement(as.integer(s[[ref]]), allocation(p), w),
       replicated_published = agreement(as.integer(s[[ref]]), allocation(p),
                                        if (weight == "consensus_weight") 100 * w / sum(w) else w,
                                        round_cells = TRUE),
       modal = agreement(as.integer(s[[ref]]), truth_P(s$modal), w))
}
comparison$consensus_weight <- comparison$HCAP16WGTR / comparison$samplingP
con_ok <- !is.na(comparison$consensuspaneldx) & is.finite(comparison$consensus_weight)

# Exact-factor illustrations, with and without the separate class-dependent
# PD102 likelihood. This is not a bound based on a reliability approximation.
calpar <- mods$cognition$cal$parameters$unstandardized
param <- function(header, name) calpar |>
  filter(paramHeader == header, param == name, !is.na(LatentClass)) |>
  arrange(as.integer(LatentClass)) |> pull(est)
mu <- param("Means", "GCP")
sd <- sqrt(param("Variances", "GCP"))
prior <- mods$cognition$score$class_counts$modelEstimated$proportion
mem <- rbind(plogis(param("Thresholds", "PD102$1")),
             plogis(param("Thresholds", "PD102$2")) - plogis(param("Thresholds", "PD102$1")),
             1 - plogis(param("Thresholds", "PD102$2")))
ceiling <- function(prior, memory_likelihood = rep(1, 3)) {
  x <- seq(-15, 8, length.out = 23001)
  densities <- sapply(1:3, function(k) prior[k] * memory_likelihood[k] * dnorm(x, mu[k], sd[k]))
  post <- densities / rowSums(densities)
  i <- which.max(post[, 2])
  c(max_MCI = post[i, 2], factor = x[i])
}
result <- list(
  provenance = list(git_head = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
                    reviewed_report = "Reports/PMM_Analysis_Report_2026-10-22.html",
                    class_order = c("Normal", "MCI", "Dementia")),
  cognition = audit_model(mods$cognition, cog), informant = audit_model(mods$informant, inf),
  routing = list(n_hcap = nrow(base), n_scored = nrow(combined),
                 n_overlap = length(intersect(cog$id, inf$id)),
                 n_cognition = nrow(cog), n_informant = nrow(inf)),
  combined_pct = list(reference = share(truth_P(d$truth), d$HCAP16WGTR),
                      posterior_saved = share(P, d$HCAP16WGTR),
                      posterior_normalized = share(normP, d$HCAP16WGTR),
                      replicated = share(A, d$HCAP16WGTR),
                      modal = share(truth_P(d$modal), d$HCAP16WGTR)),
  agreement = list(Manly_Jones = compare_one("truth", "HCAP16WGTR"),
                   Langa_Weir = compare_one("cogfunction2016", "HCAP16WGTR"),
                   Consensus = compare_one("consensuspaneldx", "consensus_weight", con_ok)),
  MCI_exact_factor_illustrations = list(with_fitted_prior = ceiling(prior),
    equal_prior = ceiling(rep(1/3, 3)),
    PD102_better = ceiling(prior, mem[1, ]), PD102_same = ceiling(prior, mem[2, ]),
    PD102_worse = ceiling(prior, mem[3, ]))
)
# Descriptive subgroup calibration; these are not confidence intervals or
# tests, and fitted-parameter uncertainty is not estimated in this audit.
subgroup <- function(data, variable, group) {
  data$group <- as.character(group)
  data |> group_by(group) |> summarise(
    n = n(), reference_MCI = weighted.mean(100 * (truth == 2), HCAP16WGTR),
    predicted_MCI = weighted.mean(100 * p2, HCAP16WGTR),
    reference_dementia = weighted.mean(100 * (truth == 3), HCAP16WGTR),
    predicted_dementia = weighted.mean(100 * p3, HCAP16WGTR), .groups = "drop") |>
    mutate(variable = variable, MCI_gap = predicted_MCI - reference_MCI,
           dementia_gap = predicted_dementia - reference_dementia)
}
dc <- d |> filter(model == "cognition")
result$cognition_subgroup_calibration <- bind_rows(
  subgroup(dc, "age", cut(dc$rage, breaks = c(-Inf, 74, 84, Inf), labels = c("65-74", "75-84", "85+"))),
  subgroup(dc, "female", dc$female), subgroup(dc, "black", dc$black),
  subgroup(dc, "hispanic", dc$hisp),
  subgroup(dc, "education_years", cut(dc$SCHLYRSimp, c(-Inf, 11, 12, Inf), labels = c("<12", "12", ">12"))),
  subgroup(dc, "number_series_missing", is.na(dc$vdexf7z)))
check_path <- here::here("R_objects", "A3_2_030_hcap16_check.rds")
if (file.exists(check_path)) {
  check <- readRDS(check_path)
  result$A3_2_reproduction <- list(probability_summary = check$probs,
    input_summary = check$inputs)
}
routing_path <- here::here("R_objects", "A3_2_020_routing.rds")
if (file.exists(routing_path)) result$A3_2_routing <- readRDS(routing_path)
cv_path <- here::here("R_objects", "PMM_106_cross_validation.RDS")
if (file.exists(cv_path)) {
  cv <- readRDS(cv_path)
  result$CV_probability_precision <- cv$predictions |>
    filter(label != "full_sample") |>
    group_by(variant) |>
    group_modify(function(data, keys) {
      p <- as.matrix(data[, c("p1", "p2", "p3")])
      pt <- p[cbind(seq_len(nrow(p)), data$algorithmic)]
      w <- data$w
      data.frame(n = nrow(data), n_true_probability_zero = sum(pt == 0),
        weighted_pct_true_probability_zero = 100 * sum(w[pt == 0]) / sum(w),
        log_score_floor_1e12 = sum(w * log(pmax(pt, 1e-12))) / sum(w),
        log_score_floor_5e4 = sum(w * log(pmax(pt, 0.0005))) / sum(w))
    }) |> ungroup()
  result$CV_run_flags <- cv$runs |>
    select(variant, label, terminated_normally, starts_with("cal_"), any_of("score_flags_any"))
  result$CV_Brier_scores <- cv$predictions |>
    filter(label != "full_sample") |> group_by(variant) |>
    group_modify(function(data, keys) {
      p <- as.matrix(data[, c("p1", "p2", "p3")])
      y <- truth_P(data$algorithmic)
      baseline <- colSums(y * data$w) / sum(data$w)
      data.frame(multiclass_Brier = weighted.mean(rowSums((p - y)^2), data$w),
        pooled_prior_only_Brier = weighted.mean(rowSums((matrix(baseline, nrow(p), 3, byrow = TRUE) - y)^2), data$w))
    }) |> ungroup()
}
jsonlite::write_json(result, here::here("Reports", "PMM_Code_Review_diagnostics_2026-10-22.json"),
                     auto_unbox = TRUE, pretty = TRUE, digits = 10, na = "null")
cat("Wrote aggregate diagnostics to Reports/PMM_Code_Review_diagnostics_2026-10-22.json\n")
