# A3_2_020: Score HRS 2016-2022 person-waves with the fixed HCAP16 PMM.
#
# The scoring machine is the MODEL section of the Analysis A3 fixed-parameter
# runs, read verbatim from:
#   mplus_output/pmm_103/pmm_hcap_103b.inp            (cognition model)
#   mplus_output/pmm_103_jorm/pmm_hcap_103b_jorm.inp  (Jorm model)
# Every parameter, including the class logits, is fixed (@) at HCAP16 values.
# Nothing is estimated, so the runs omit the survey weight, strata, and
# cluster: these do not affect the posterior class probabilities.
#
# Model choice per person-wave follows PMM_110: use the Jorm model when a Jorm
# IQCODE is present, otherwise the cognition model.

a3_2       <- readRDS(here::here("R_objects", "A3_2_010_scoring_long.rds"))
a3_2_score <- a3_2$scoring
pmm100     <- readRDS(here::here("R_objects", "PMM_100.RDS"))

a3_2_mplus_dir <- here::here("mplus_output", "A3_2")
fs::dir_create(a3_2_mplus_dir)

# ---------------------------------------------------------------------------
# Read the fixed MODEL section from an Analysis A3 input file
read_fixed_model <- function(inp) {
  x <- readLines(inp)
  start <- grep("^MODEL:", x)
  ends  <- grep("^(OUTPUT|SAVEDATA|PLOT|MONTECARLO|DEFINE):", x)
  end   <- min(ends[ends > start])
  model <- paste(x[(start + 1):(end - 1)], collapse = "\n")
  if (!grepl("c#1 @", model, fixed = TRUE)) {
    stop("A3_2_020: class logits are not fixed in ", inp)
  }
  model
}

model_cog  <- read_fixed_model(here::here("mplus_output", "pmm_103", "pmm_hcap_103b.inp"))
model_jorm <- read_fixed_model(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.inp"))

# ---------------------------------------------------------------------------
# Mplus maps each categorical variable's observed values to 0, 1, 2, ... in
# sorted order. The fixed thresholds are only valid if the scoring file has
# exactly the same set of observed values as the HCAP16 calibration file.
cat_cog  <- c("vdori", "vdlfl2", "vdlfl3", "vdsevens", "vdcount",
              "nPG014", "nPG021", "nPG023", "nPG030", "nPG040",
              "nPG041", "nPG044", "nPG047", "nPG050", "nPG059", "PD102")
cat_jorm <- c("nPG014", "nPG021", "nPG023", "nPG030", "nPG040",
              "nPG041", "nPG044", "nPG047", "nPG050", "nPG059")

x_vars <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7",
            "x1x4", "x1x5", "x1x6", "x1x7", "x2x4", "x2x5", "x2x6", "x2x7",
            "x3x4", "x3x5", "x3x6", "x3x7", "x4x5", "x4x6", "x4x7", "x5x7", "x6x7")
use_cog  <- c("sid", "vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwdimmz",
              "vdwddelz", "vdexf7z", "vdsevens", "vdcount", x_vars, cat_jorm, "PD102")
use_jorm <- c("sid", cat_jorm, "jorm")

check_categories <- function(score_df, ref_df, vars) {
  purrr::map_dfr(vars, function(v) {
    s <- sort(unique(stats::na.omit(num(score_df[[v]]))))
    r <- sort(unique(stats::na.omit(num(ref_df[[v]]))))
    tibble(variable = v,
           scoring  = paste(s, collapse = ", "),
           hcap16   = paste(r, collapse = ", "),
           match    = identical(s, r))
  })
}

score_jorm_rows <- a3_2_score |> dplyr::filter(!is.na(jorm))

a3_2_category_check <- dplyr::bind_rows(
  check_categories(a3_2_score, dplyr::filter(pmm100, inHCAP == 1), cat_cog) |>
    mutate(model = "cognition"),
  check_categories(score_jorm_rows,
                   dplyr::filter(pmm100, inHCAP == 1, !is.na(jorm)), cat_jorm) |>
    mutate(model = "jorm")
)

if (!all(a3_2_category_check$match)) {
  print(dplyr::filter(a3_2_category_check, !match))
  stop("A3_2_020: categorical values in the scoring file differ from HCAP16. ",
       "Fixed thresholds would be misaligned. See a3_2_category_check.")
}

# ---------------------------------------------------------------------------
run_scoring <- function(data, model, categorical, usevars, stem) {
  categorical_line <- paste(
    strwrap(paste(categorical, collapse = " "), width = 65),
    collapse = "\n  "
  )
  obj <- MplusAutomation::mplusObject(
    TITLE    = stringr::str_c("A3_2 scoring, HRS 2016-2022 Core, all parameters fixed at HCAP16 (", stem, ")"),
    VARIABLE = stringr::str_c("categorical = ", categorical_line, ";\n",
                              "idvariable = sid;\nclasses = c (3);"),
    ANALYSIS = "estimator = mlr;\nALGORITHM = INTEGRATION;\nTYPE = mixture;\nstarts = 0;\nprocessors = 4;",
    MODEL    = model,
    SAVEDATA = stringr::str_c("SAVE = CPROBABILITIES;\nFILE = cprob_", stem, ".dat;"),
    usevariables = usevars,
    rdata    = as.data.frame(data[, usevars])
  )
  old_wd <- setwd(a3_2_mplus_dir)
  on_exit_wd <- function() setwd(old_wd)
  tryCatch(
    MplusAutomation::mplusModeler(obj, modelout = stringr::str_c(stem, ".inp"),
                                  run = 1, writeData = "always", hashfilename = FALSE),
    finally = on_exit_wd()
  )
  out_file <- file.path(a3_2_mplus_dir, stringr::str_c(stem, ".out"))
  out <- suppressWarnings(MplusAutomation::readModels(out_file))
  output <- readLines(out_file, warn = FALSE)
  if (!any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", output, fixed = TRUE))) {
    stop("A3_2_020: Mplus did not terminate normally in ", stem, ".out")
  }
  errors <- as.character(out$errors)
  brant_error <- grepl(
    "^ERROR OCCURRED IN THE BRANT WALD TEST FOR PROPORTIONAL ODDS FOR ",
    errors
  )
  fatal_errors <- errors[!brant_error]
  if (length(fatal_errors) > 0) {
    stop("A3_2_020: Mplus errors in ", stem, ".out: ",
         paste(fatal_errors, collapse = "; "))
  }
  if (any(brant_error)) {
    message("A3_2_020: Mplus completed normally; Brant Wald diagnostic failed for ",
            paste(sub(".* FOR ", "", errors[brant_error]), collapse = ", "), ".")
  }
  if (is.null(out$savedata) || nrow(out$savedata) != nrow(data)) {
    stop("A3_2_020: incomplete posterior output in ", stem, ".out; expected ",
         nrow(data), " rows, found ",
         if (is.null(out$savedata)) 0 else nrow(out$savedata), ".")
  }
  out$savedata |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(sid, cprob1, cprob2, cprob3)
}

cprob_cog  <- run_scoring(a3_2_score,      model_cog,  cat_cog,  use_cog,  "A3_2_score_cog")
cprob_jorm <- run_scoring(score_jorm_rows, model_jorm, cat_jorm, use_jorm, "A3_2_score_jorm")

# ---------------------------------------------------------------------------
# Combine: Jorm model when Jorm is present, else cognition model
a3_2_probs <- a3_2_score |>
  dplyr::select(sid, HHID, PN, wave) |>
  dplyr::left_join(dplyr::rename(cprob_cog,  c1 = cprob1, c2 = cprob2, c3 = cprob3), by = "sid") |>
  dplyr::left_join(dplyr::rename(cprob_jorm, j1 = cprob1, j2 = cprob2, j3 = cprob3), by = "sid") |>
  mutate(
    pmm_model  = dplyr::case_when(!is.na(j1) ~ "jorm", !is.na(c1) ~ "cognition"),
    p_normal   = dplyr::if_else(!is.na(j1), j1, c1),
    p_mci      = dplyr::if_else(!is.na(j1), j2, c2),
    p_dementia = dplyr::if_else(!is.na(j1), j3, c3)
  ) |>
  dplyr::transmute(hhid = HHID, pn = PN, wave, p_normal, p_mci, p_dementia, pmm_model)

attr(a3_2_probs$p_normal,   "label") <- "PMM posterior probability: Normal (HCAP16 fixed parameters)"
attr(a3_2_probs$p_mci,      "label") <- "PMM posterior probability: MCI (HCAP16 fixed parameters)"
attr(a3_2_probs$p_dementia, "label") <- "PMM posterior probability: Dementia (HCAP16 fixed parameters)"
attr(a3_2_probs$pmm_model,  "label") <- "PMM model used: jorm if Jorm IQCODE present, else cognition"

saveRDS(a3_2_probs, here::here("R_objects", "A3_2_pmm_probs_hrs16_22.rds"))
haven::write_dta(a3_2_probs, here::here("R_objects", "A3_2_pmm_probs_hrs16_22.dta"))
saveRDS(a3_2_category_check, here::here("R_objects", "A3_2_020_category_check.rds"))
