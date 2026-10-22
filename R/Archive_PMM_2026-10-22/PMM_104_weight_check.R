#!/usr/bin/env Rscript
# PMM_104_weight_check.R
#
# Side check, not part of any driver. Run from the project root:
#   Rscript R/PMM_104_weight_check.R
#
# Question: does Mplus apply WEIGHT = HCAP16WGTR to the class-specific
# thresholds and means in the KNOWNCLASS calibration models (pmm_hcap_103 and
# pmm_hcap_103_jorm)? The class logits in those models reproduce the
# unweighted class counts exactly, which suggests the weights may be ignored.
#
# Method, for each calibration model:
#   1. Refit the weighted model as-is (same data, model, and 80 starts), adding
#      OUTPUT: SVALUES to capture the final estimates as starting values.
#   2. Refit the same model with every weight set to 1, starting from the
#      weighted solution (STARTS = 0). Random starts from default values fail
#      for the cognition model: the class 1 covariance matrix cannot be
#      inverted at iteration 1.
#   3. Compare point estimates. Strata and clusters change standard errors,
#      not point estimates, so any difference comes from the weights.
#
# Inputs (read only):
#   mplus_output/pmm_103/pmm_hcap_103.inp and its .dat file
#   mplus_output/pmm_103_jorm/pmm_hcap_103_jorm.inp and its .dat file
# Outputs:
#   mplus_output/pmm_104_weightcheck/   inputs, unit-weight data, and outputs
#   Reports/PMM_104_weight_check_[date].csv   parameter-by-parameter comparison
#
# Runtime is about the same as fitting pmm_hcap_103 and pmm_hcap_103_jorm once.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

out_dir <- here::here("mplus_output", "pmm_104_weightcheck")
fs::dir_create(out_dir)
fs::dir_create(here::here("Reports"))

# Weight is the third column in the MplusAutomation data files
# (NAMES = id vs1hcapdxeap HCAP16WGTR ...). Stop if that changes.
weight_position <- 3

# ---- helpers ---------------------------------------------------------------

section_range <- function(inp, header, next_header) {
  start <- grep(str_c("^", header, ":"), inp, ignore.case = TRUE)
  end <- grep(str_c("^", next_header, ":"), inp, ignore.case = TRUE) - 1
  if (length(start) != 1 || length(end) != 1) {
    stop("Could not find ", header, " section.")
  }
  start:end
}

check_weight_column <- function(inp, src_inp) {
  names_line <- paste(inp[grep("^NAMES", inp):grep("^ *MISSING", inp)], collapse = " ")
  names_vec <- str_split(str_squish(str_remove(names_line, "^NAMES *=")), " ")[[1]]
  if (names_vec[weight_position] != "HCAP16WGTR") {
    stop("HCAP16WGTR is not column ", weight_position, " in ", src_inp)
  }
}

terminated_normally <- function(out_file) {
  any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", readLines(out_file)))
}

# Pull the SVALUES block ("MODEL COMMAND WITH FINAL ESTIMATES USED AS
# STARTING VALUES") from an Mplus output file.
read_svalues <- function(out_file) {
  out <- readLines(out_file)
  start <- grep("MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES", out)
  if (length(start) != 1) stop("No SVALUES block in ", out_file)
  rest <- out[(start + 1):length(out)]
  # The block is indented; it ends at the first non-indented line or at the
  # timing lines Mplus prints at the end of the file.
  stop_at <- which(grepl("^\\S", rest) | grepl("Beginning Time", rest))[1]
  block <- if (is.na(stop_at)) rest else rest[seq_len(stop_at - 1)]
  block[nzchar(str_trim(block))]
}

run_model <- function(inp_lines, stem) {
  inp_path <- file.path(out_dir, str_c(stem, ".inp"))
  writeLines(inp_lines, inp_path)
  MplusAutomation::runModels(inp_path, logFile = NULL)
  out_path <- fs::path_ext_set(inp_path, "out")
  if (!terminated_normally(out_path)) {
    stop("Mplus did not terminate normally: ", out_path)
  }
  out_path
}

# ---- build and run the two fits for one calibration model ------------------

fit_pair <- function(src_inp, stem) {
  inp <- readLines(src_inp)
  check_weight_column(inp, src_inp)

  file_line <- grep("^FILE *=", inp)
  src_dat_name <- str_match(inp[file_line], '"([^"]+)"')[, 2]
  src_dat <- file.path(dirname(src_inp), src_dat_name)
  rel_src_dat <- file.path("..", basename(dirname(src_inp)), src_dat_name)

  # 1. Weighted refit with SVALUES.
  wt <- inp
  wt[file_line] <- str_c('FILE = "', rel_src_dat, '";')
  wt <- str_replace(wt, "H5RESULTS *= *[^;]+;", str_c("H5RESULTS = ", stem, "_wt.h5;"))
  out_rows <- section_range(wt, "OUTPUT", "SAVEDATA")
  wt <- append(wt, "svalues;", after = max(out_rows))
  wt <- c(str_c("! Weighted refit of ", basename(src_inp), " with SVALUES (PMM_104)"), wt)
  wt_out <- run_model(wt, str_c(stem, "_wt"))

  # 2. Unit-weight data (tab delimited, weight column set to 1).
  fields <- str_split(readLines(src_dat), "\t")
  fields <- lapply(fields, function(x) { x[weight_position] <- "1"; x })
  unit_dat <- str_c(stem, "_unitwt.dat")
  writeLines(vapply(fields, paste, character(1), collapse = "\t"),
             file.path(out_dir, unit_dat))

  # 3. Unit-weight fit starting from the weighted solution.
  un <- inp
  un[file_line] <- str_c('FILE = "', unit_dat, '";')
  un <- str_replace(un, "H5RESULTS *= *[^;]+;", str_c("H5RESULTS = ", stem, "_unitwt.h5;"))
  un <- str_replace(un, regex("^\\s*starts\\s*=\\s*\\d+\\s*;", ignore_case = TRUE), "starts = 0;")
  model_rows <- section_range(un, "MODEL", "OUTPUT")
  un <- c(un[seq_len(min(model_rows) - 1)],
          "MODEL:",
          read_svalues(wt_out),
          un[(max(model_rows) + 1):length(un)])
  un <- c(str_c("! Unit-weight copy of ", basename(src_inp),
                ", started at the weighted solution (PMM_104)"), un)
  un_out <- run_model(un, str_c(stem, "_unitwt"))

  list(weighted = wt_out, unit = un_out)
}

# ---- compare ---------------------------------------------------------------

compare_models <- function(weighted_out, unit_out, label) {
  get_par <- function(path) {
    suppressWarnings(MplusAutomation::readModels(path))[["parameters"]][["unstandardized"]] |>
      as_tibble() |>
      select(paramHeader, param, LatentClass, est)
  }

  w <- get_par(weighted_out) |> rename(est_weighted = est)
  u <- get_par(unit_out) |> rename(est_unit = est)

  cmp <- full_join(w, u, by = c("paramHeader", "param", "LatentClass")) |>
    mutate(model = label,
           diff = est_weighted - est_unit,
           type = case_when(
             LatentClass == "Categorical.Latent.Variables" ~ "class logit",
             paramHeader == "Thresholds" ~ "threshold",
             paramHeader %in% c("Means", "Intercepts") ~ "mean/intercept",
             grepl("\\.ON$", paramHeader) ~ "regression (fixed norms)",
             TRUE ~ "other"
           ))

  # Class-specific parameters: estimates that differ across classes 1-3.
  class_specific <- cmp |>
    filter(LatentClass %in% c("1", "2", "3")) |>
    group_by(paramHeader, param) |>
    summarise(class_specific = n_distinct(est_weighted) > 1, .groups = "drop")

  cmp |>
    left_join(class_specific, by = c("paramHeader", "param")) |>
    mutate(class_specific = if_else(type == "class logit", TRUE,
                                    coalesce(class_specific, FALSE)))
}

models <- tibble::tribble(
  ~label,      ~src_inp,                                                            ~stem,
  "cognition", here::here("mplus_output", "pmm_103", "pmm_hcap_103.inp"),           "pmm_104",
  "jorm",      here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103_jorm.inp"), "pmm_104_jorm"
)

results <- purrr::pmap(models, function(label, src_inp, stem) {
  fits <- fit_pair(src_inp, stem)

  # The weighted refit should reproduce the original calibration.
  orig <- suppressWarnings(MplusAutomation::readModels(fs::path_ext_set(src_inp, "out")))
  refit <- suppressWarnings(MplusAutomation::readModels(fits$weighted))
  cat(label, ": loglikelihood original ", orig$summaries$LL,
      ", weighted refit ", refit$summaries$LL,
      ", unit-weight ", suppressWarnings(MplusAutomation::readModels(fits$unit))$summaries$LL,
      "\n", sep = "")

  compare_models(fits$weighted, fits$unit, label)
}) |>
  bind_rows()

csv_path <- here::here("Reports", str_c("PMM_104_weight_check_", Sys.Date(), ".csv"))
readr::write_csv(results, csv_path)

# Mplus prints estimates to 3 decimals, so differences below 0.0005 are
# rounding. Any larger difference means the weights changed the estimate.
summary_tbl <- results |>
  filter(class_specific, !is.na(diff)) |>
  group_by(model, type) |>
  summarise(n_params = n(),
            n_differ = sum(abs(diff) >= 0.0005),
            max_abs_diff = max(abs(diff)),
            .groups = "drop")

cat("\nClass-specific parameters: weighted vs unit-weight calibration\n\n")
print(summary_tbl, n = Inf)

cat("\nInterpretation\n")
cat("  n_differ = 0 for thresholds and means: Mplus ignored the weights for\n")
cat("    the class-specific parameters in the KNOWNCLASS calibration.\n")
cat("  n_differ > 0: the weights entered those estimates.\n")
cat("  Class logits: n_differ = 0 means the priors ignore the weights.\n")
cat("\nFull comparison written to: ", csv_path, "\n", sep = "")

unmatched <- results |> filter(is.na(est_weighted) | is.na(est_unit))
if (nrow(unmatched) > 0) {
  cat("\nWarning: ", nrow(unmatched), " parameters did not match between runs. See CSV.\n", sep = "")
}
