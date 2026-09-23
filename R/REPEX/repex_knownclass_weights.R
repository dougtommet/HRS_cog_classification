#!/usr/bin/env Rscript
# repex_knownclass_weights.R
#
# Reproducible example for Mplus support: with KNOWNCLASS (or TRAINING) and
# WEIGHT, are the class proportions (class logits) estimated with the weights?
#
# Uses simulated data only. Run from the project root:
#   Rscript R/REPEX/repex_knownclass_weights.R
#
# Writes to R/REPEX/:
#   m*.dat                        simulated data, one file per model
#                                 (tab delimited, no header)
#   m1_noweight.inp/.out          KNOWNCLASS, no weight (reference)
#   m2_weight.inp/.out            KNOWNCLASS, WEIGHT, TYPE = MIXTURE
#   m3_complex.inp/.out           KNOWNCLASS, WEIGHT + CLUSTER, TYPE = COMPLEX MIXTURE
#   m4_training.inp/.out          TRAINING (all memberships known), WEIGHT
#   m5_latent.inp/.out            latent classes (no labels), WEIGHT; control
#   repex_results.txt             comparison of Mplus estimates with targets
#
# Design. 3 classes with true proportions .60/.25/.15. The sampling weight
# rises with class and with a within-class covariate z, so (1) the weighted
# class proportions differ from the unweighted ones, and (2) the weighted
# within-class means of y differ from the unweighted ones. Under weighted
# (pseudo-) maximum likelihood with known classes, the class proportions have
# a closed form: the weighted sample proportions. The class-specific means of
# y are the weighted class means. Each Mplus estimate is compared with both
# the weighted and the unweighted target.
#
# Model m5 is a control. Classes are unlabeled but far apart (means 0, 4, 8;
# SD about 1), so posterior membership is close to 0/1 and EM with weights
# should return class proportions close to the weighted proportions.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

repex_dir <- here::here("R", "REPEX")
fs::dir_create(repex_dir)

# ---- simulate ----------------------------------------------------------------

set.seed(20260923)
n <- 3000
true_p <- c(0.60, 0.25, 0.15)
mu <- c(0, 4, 8)
b_class <- c(0, 0.8, 1.6)      # weight rises with class
n_clusters <- 150

sim <- tibble(
  id = seq_len(n),
  cls = sample(1:3, n, replace = TRUE, prob = true_p),
  z = rnorm(n),
  clus = sample(seq_len(n_clusters), n, replace = TRUE)
) |>
  mutate(
    w = exp(b_class[cls] + 0.5 * z),
    y = mu[cls] + 0.6 * z + rnorm(n, sd = 0.8),
    t1 = as.integer(cls == 1),
    t2 = as.integer(cls == 2),
    t3 = as.integer(cls == 3)
  )

# One data file per model, holding only the variables that model names. No
# USEVARIABLES statement is used; every variable on NAMES has a role
# (ID, known class, training, weight, cluster) or is the indicator y.
write_dat <- function(cols, file) {
  write.table(
    sim |> select(all_of(cols)) |> mutate(across(any_of(c("w", "y")), ~ sprintf("%.8f", .x))),
    file.path(repex_dir, file),
    sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE
  )
}

# ---- targets ------------------------------------------------------------------

targets <- sim |>
  group_by(class = cls) |>
  summarise(n = n(), sw = sum(w),
            mean_unweighted = mean(y),
            mean_weighted = weighted.mean(y, w),
            .groups = "drop") |>
  mutate(prop_unweighted = n / sum(n),
         prop_weighted = sw / sum(sw)) |>
  select(class, prop_unweighted, prop_weighted, mean_unweighted, mean_weighted)

# ---- Mplus inputs -------------------------------------------------------------

class_model <- function(cv) {
  str_c("%OVERALL%\n",
        str_c(sprintf("%%%s#%d%%\n  [y*%d];\n  y;", cv, 1:3, mu), collapse = "\n"))
}

make_inp <- function(nm, title, cols, variable, analysis, model) {
  write_dat(cols, str_c(nm, ".dat"))
  c("TITLE:", str_c("  ", title),
    "DATA:", str_c("  FILE = ", nm, ".dat;"),
    "VARIABLE:",
    str_c("  NAMES = ", str_c(cols, collapse = " "), ";"),
    "  IDVARIABLE = id;",
    variable,
    "ANALYSIS:", analysis,
    "MODEL:", model,
    "OUTPUT:", "  TECH1;")
}

known <- "  CLASSES = cg (3);\n  KNOWNCLASS = cg (cls = 1 cls = 2 cls = 3);"
mix <- "  TYPE = MIXTURE;\n  ESTIMATOR = MLR;\n  STARTS = 0;"

inputs <- list(
  m1_noweight = make_inp("m1_noweight",
    "KNOWNCLASS, no weight (reference)",
    c("id", "cls", "y"),
    known, mix, class_model("cg")),
  m2_weight = make_inp("m2_weight",
    "KNOWNCLASS with WEIGHT, TYPE = MIXTURE",
    c("id", "cls", "w", "y"),
    c(known, "  WEIGHT = w;"), mix, class_model("cg")),
  m3_complex = make_inp("m3_complex",
    "KNOWNCLASS with WEIGHT and CLUSTER, TYPE = COMPLEX MIXTURE",
    c("id", "cls", "w", "clus", "y"),
    c(known, "  WEIGHT = w;", "  CLUSTER = clus;"),
    "  TYPE = COMPLEX MIXTURE;\n  ESTIMATOR = MLR;\n  STARTS = 0;",
    class_model("cg")),
  m4_training = make_inp("m4_training",
    "TRAINING (all memberships known) with WEIGHT",
    c("id", "w", "y", "t1", "t2", "t3"),
    c("  CLASSES = c (3);", "  TRAINING = t1-t3;", "  WEIGHT = w;"),
    mix, class_model("c")),
  m5_latent = make_inp("m5_latent",
    "Latent classes, no labels, with WEIGHT (control)",
    c("id", "w", "y"),
    c("  CLASSES = c (3);", "  WEIGHT = w;"),
    mix, class_model("c"))
)

for (nm in names(inputs)) {
  writeLines(inputs[[nm]], file.path(repex_dir, str_c(nm, ".inp")))
}

# ---- run ----------------------------------------------------------------------

MplusAutomation::runModels(repex_dir, recursive = FALSE, logFile = NULL,
                           replaceOutfile = "always")

# ---- read and compare ---------------------------------------------------------

read_one <- function(nm) {
  out_file <- file.path(repex_dir, str_c(nm, ".out"))
  out_txt <- readLines(out_file)
  if (!any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", out_txt))) {
    warning(nm, ": estimation did not terminate normally. See ", out_file)
    return(NULL)
  }
  par <- suppressWarnings(MplusAutomation::readModels(out_file))[["parameters"]][["unstandardized"]] |>
    as_tibble()

  logits <- par |>
    filter(LatentClass == "Categorical.Latent.Variables", paramHeader == "Means") |>
    arrange(param)
  means <- par |>
    filter(LatentClass %in% c("1", "2", "3"), paramHeader == "Means", param == "Y") |>
    mutate(class_mplus = as.integer(LatentClass)) |>
    select(class_mplus, mean_mplus = est)

  lg <- c(logits$est, 0)
  tibble(class_mplus = 1:3,
         prop_mplus = exp(lg) / sum(exp(lg)),
         logit_mplus = lg,
         logit_se = c(logits$se, NA_real_)) |>
    left_join(means, by = "class_mplus") |>
    # Latent classes (m5) may be ordered arbitrarily; label by the mean of y.
    arrange(mean_mplus) |>
    mutate(class = 1:3, model = nm) |>
    select(model, class, prop_mplus, logit_mplus, logit_se, mean_mplus)
}

res <- purrr::map(names(inputs), read_one) |>
  bind_rows() |>
  left_join(targets, by = "class") |>
  mutate(prop_matches = case_when(
           abs(prop_mplus - prop_weighted) < 0.0015 & abs(prop_mplus - prop_unweighted) < 0.0015 ~ "both",
           abs(prop_mplus - prop_weighted) < 0.0015 ~ "weighted",
           abs(prop_mplus - prop_unweighted) < 0.0015 ~ "unweighted",
           TRUE ~ "neither"),
         mean_matches = case_when(
           abs(mean_mplus - mean_weighted) < 0.0015 ~ "weighted",
           abs(mean_mplus - mean_unweighted) < 0.0015 ~ "unweighted",
           TRUE ~ "neither"))

mplus_version <- str_squish(str_remove(
  grep("Mplus VERSION", readLines(file.path(repex_dir, "m1_noweight.out")), value = TRUE)[1],
  "^\\s*"))

report <- c(
  str_c("Reproducible example: KNOWNCLASS/TRAINING class proportions with WEIGHT"),
  str_c("Run: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "; ", mplus_version),
  str_c("Simulated data: N = ", n, "; true class proportions .60/.25/.15; seed 20260923"),
  "",
  "Targets (computed in R from the simulated data):",
  capture.output(print(as.data.frame(targets), digits = 4, row.names = FALSE)),
  "",
  "Mplus estimates (prop = class proportion implied by the class logits):",
  capture.output(print(as.data.frame(
    res |> select(model, class, prop_mplus, prop_matches, logit_mplus, logit_se,
                  mean_mplus, mean_matches)),
    digits = 4, row.names = FALSE)),
  "",
  "Reading the table:",
  "  m1 should match the unweighted targets.",
  "  Under weighted pseudo-ML, m2-m4 class proportions and class means",
  "  should match the weighted targets.",
  "  m5 (control, latent classes) proportions should be close to weighted,",
  "  not exact, because posterior membership is near but not exactly 0/1."
)

writeLines(report, file.path(repex_dir, "repex_results.txt"))
cat(report, sep = "\n")
