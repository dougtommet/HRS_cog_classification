#!/usr/bin/env Rscript
# USER-RUN ONLY. Agents must not execute this script or read participant files.
if (!"--local-data" %in% commandArgs(trailingOnly = TRUE)) {
  stop("Participant-data analysis: run locally yourself with --local-data; agents must not run this script.")
}
# Isolated, zero-free-parameter scoring sensitivity check. Reads calibration
# estimates from H5 and existing Mplus input data. All individual-level scoring
# files stay in R's temporary directory; only aggregate diagnostics are saved.
# Run from project root: Rscript tools/review_pmm_precision_2026-10-22.R
suppressPackageStartupMessages({ library(dplyr); library(tibble); library(stringr) })
source(here::here("R", "PMM_090_model_registry.R"))
source(here::here("R", "PMM_101_mplus_function.R"))
options(digits = 15)
model_path <- pmm_models$cognition$calibration_out
cal <- suppressWarnings(MplusAutomation::readModels(model_path, what = c("parameters", "summaries")))
h5 <- mplush5::mplus.print.model.results(file.path(dirname(model_path), "model_102.h5"))
h5 <- h5[grepl("^Latent Class [123]", h5$Class), ]
h5$LatentClass <- as.integer(sub("^Latent Class ([123]).*", "\\1", h5$Class))
parts <- strsplit(trimws(h5$Statement), " +")
h5$header <- ifelse(nzchar(trimws(h5$Section)), h5$Section,
                    vapply(parts, function(p) paste(head(p, -1), collapse = " "), character(1)))
h5$param <- ifelse(nzchar(trimws(h5$Section)), trimws(h5$Statement),
                   vapply(parts, tail, character(1), n = 1))
key <- function(cl, header, param) paste(cl, tolower(gsub("[ .]", "", header)), tolower(param), sep = ":")
h5$key <- key(h5$LatentClass, h5$header, h5$param)
par <- cal$parameters$unstandardized
keep <- par$LatentClass %in% 1:3
idx <- match(key(par$LatentClass[keep], par$paramHeader[keep], par$param[keep]), h5$key)
stopifnot(!anyNA(idx), !anyDuplicated(h5$key), sum(keep) == nrow(h5))
parameter_diff <- data.frame(header = par$paramHeader[keep], parameter = par$param[keep],
                            class = par$LatentClass[keep], printed = par$est[keep], exact = h5$Estimate[idx])
par$est[keep] <- h5$Estimate[idx]
cal$parameters$unstandardized <- par
fixed <- pmm_fixed_model_from_h5_results(h5, readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS")))
input <- readLines(pmm_models$cognition$scoring_inp)
original <- paste(input, collapse = "\n")
prior_line <- input[grepl("c#1 @", input, fixed = TRUE)]
fixed <- sub("%OVERALL%", paste("%OVERALL%", prior_line, sep = "\n"), fixed, fixed = TRUE)
score_model <- function(model = NULL, name) {
  folder <- file.path(tempdir(), paste0("pmm_precision_", name))
  dir.create(folder)
  text <- original
  datafile <- sub('.*FILE = "([^"]+)".*', '\\1', input[grepl('^FILE = ', input)][1])
  stopifnot(file.symlink(file.path(dirname(pmm_models$cognition$scoring_inp), datafile),
                        file.path(folder, "data.dat")))
  text <- sub(paste0('FILE = "', datafile, '"'), 'FILE = "data.dat"', text, fixed = TRUE)
  if (!is.null(model)) text <- sub("(?s)MODEL:.*?OUTPUT:", paste0("MODEL:\n", model, "\nOUTPUT:"), text, perl = TRUE)
  text <- sub("(?s)SAVEDATA:.*$", "SAVEDATA:\nSAVE = CPROBABILITIES;\nFILE = probabilities.dat;\nFORMAT = F20.12;", text, perl = TRUE)
  text <- sub("starts = 80;", "starts = 0;", text, fixed = TRUE)
  inp <- file.path(folder, "score.inp")
  writeLines(text, inp)
  old <- setwd(folder)
  on.exit(setwd(old), add = TRUE)
  status <- system2(Sys.which("mplus"), "score.inp", stdout = "console.log", stderr = "console.log")
  out <- readLines("score.out")
  if (status != 0) stop("Temporary scoring failed: ", paste(tail(out, 15), collapse = "\n"))
  stopifnot(status == 0, any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", out, fixed = TRUE)))
  mod <- suppressWarnings(MplusAutomation::readModels("score.out", what = c("summaries", "savedata")))
  stopifnot(mod$summaries$Parameters == 0)
  d <- janitor::clean_names(as.data.frame(mod$savedata))
  d |> select(id, cprob1, cprob2, cprob3, mlcc)
}
control <- score_model(name = "printed_parameters")
precise <- score_model(fixed, "H5_parameters")
compare <- inner_join(control, precise, by = "id", suffix = c(".printed", ".exact"))
base <- haven::zap_labels(readRDS(here::here("R_objects", "PMM_100.RDS"))) |>
  filter(inHCAP == 1) |> mutate(id = as.numeric(id)) |>
  select(id, HCAP16WGTR, vs1hcapdxeap)
compare <- inner_join(compare, base, by = "id")
stopifnot(nrow(compare) == nrow(control), nrow(control) == nrow(precise))
P <- as.matrix(compare[, paste0("cprob", 1:3, ".printed")])
Q <- as.matrix(compare[, paste0("cprob", 1:3, ".exact")])
w <- compare$HCAP16WGTR
shares <- function(p) 100 * colSums(p * w) / sum(w)
result <- list(n = nrow(compare),
  n_class_parameters_recovered_from_H5 = nrow(parameter_diff),
  continuous_residual_variances = parameter_diff |> filter(header == "Residual.Variances", class == 1) |>
    mutate(percent_error = 100 * (printed / exact - 1)),
  prior_class_order = c("Normal", "MCI", "Dementia"),
  posterior_pct_printed_parameters = shares(P), posterior_pct_exact_parameters = shares(Q),
  max_absolute_probability_difference = max(abs(P - Q)),
  mean_absolute_probability_difference = colSums(abs(P - Q) * w) / sum(w),
  n_modal_class_changes = sum(compare$mlcc.printed != compare$mlcc.exact),
  weighted_pct_modal_class_changes = 100 * sum(w[compare$mlcc.printed != compare$mlcc.exact]) / sum(w),
  scoring_free_parameters = 0,
  individual_results_location = "Temporary directory only; no individual records exported")
jsonlite::write_json(result, here::here("Reports", "PMM_Code_Review_precision_2026-10-22.json"),
                     auto_unbox = TRUE, pretty = TRUE, digits = 10)
cat("Wrote aggregate precision sensitivity to Reports/PMM_Code_Review_precision_2026-10-22.json\n")
