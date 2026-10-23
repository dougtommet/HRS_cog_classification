# PMM_103_informant_model.R
#
# Informant model of the PMM (2026-10-22). Applies to HRS/HCAP participants
# with no cognition-model indicator who have an informant report: the Jorm
# IQCODE is the only indicator. Known-class calibration (pmm_hcap_103_jorm),
# then scoring with every parameter fixed and the class logits fixed at the
# weighted class shares of this sample (pmm_hcap_103b_jorm).
# Output folder: mplus_output/pmm_103_jorm/ (name kept from earlier versions).
# The no-factor cognition model that shared this file until 2026-10-22 is in
# R/Archive_PMM_2026-10-22/PMM_103_calibration_model.R.

PMM_100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))
mplus_mod_norms <- readRDS(here::here("R_objects", "PMM_100_mplus_mod_norms.RDS"))

normalize_mplus_savedata <- function(savedata) {
  savedata |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::rename(c = tidyselect::any_of("mlcc"))
}

read_mplus_model_quietly <- function(model_path) {
  suppressWarnings(MplusAutomation::readModels(model_path))
}

read_mplus_savedata_quietly <- function(model_path) {
  read_mplus_model_quietly(model_path)[["savedata"]] |>
    normalize_mplus_savedata()
}

inhcap_jorm <- PMM_100 %>%
  filter(inHCAP==1) %>%
  filter(!is.na(jorm))

inhcap_jorm <- inhcap_jorm %>%
  mutate(id = as.numeric(id))


## Mplus model with population weights
fs::dir_create(here::here("mplus_output", "pmm_103_jorm"))
setwd(here::here("mplus_output", "pmm_103_jorm"))

mplus_mod_jorm = "
   %cg#2%
   [jorm];
   %cg#3%
   [jorm];
"


mplus_mod_103_jorm <- str_c("%OVERALL%", " \n ",
                       mplus_mod_jorm)

# The subpopulation option doesn't work with knownclass
mplus_variable_103_jorm = "idvariable = id;
classes = cg (3);
knownclass = cg  (vs1hcapdxeap = 1 vs1hcapdxeap = 2 vs1hcapdxeap = 3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

mplus_analysis_103_jorm = "estimator = mlr;
starts = 80;
processors = 12;
TYPE = complex mixture;"

mplus_output_103_jorm <- "standardized;"

mod1 <- MplusAutomation::mplusObject(
  TITLE = "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years",
  MODEL = mplus_mod_103_jorm,
  VARIABLE = mplus_variable_103_jorm,
  ANALYSIS = mplus_analysis_103_jorm,
  OUTPUT = mplus_output_103_jorm,
  SAVEDATA = "H5RESULTS = model_103_jorm.h5;",
  usevariables = c("id", "vs1hcapdxeap", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "jorm"
  ),
  rdata = inhcap_jorm

)
source(here::here("R", "PMM_091_mplus_run_cache.R"))  # pmm_mplus_run(): reuse unchanged fits
pmm_mplus_run(mod1, "pmm_hcap_103_jorm.inp", outputs = "model_103_jorm.h5")

# mplush5::mplus.view.results(here::here("mplus_output", "pmm_103", "model_103.h5"))
# mplush5::mplus.print.results.in.probability.scale(here::here("mplus_output", "pmm_103", "model_103.h5"))

pmm_103_jorm <- read_mplus_model_quietly(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103_jorm.out"))

check_prior_sample(inhcap_jorm, pmm_103_jorm, "informant model")

mplus_mod_fixed_jorm <- write_lca_model(pmm_models$informant$calibration_h5, "")
# Fix class logits at the survey-weighted class proportions of the Jorm sample.
mplus_mod_fixed_jorm <- sub("%OVERALL%",
                            str_c("%OVERALL% \n ",
                                  weighted_class_logits(inhcap_jorm, "vs1hcapdxeap", "HCAP16WGTR")),
                            mplus_mod_fixed_jorm, fixed = TRUE)
mplus_variable_fixed_jorm = "idvariable = id;
classes = c (3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

mplus_analysis_fixed_jorm = "estimator = mlr;
starts = 80;
processors = 12;
TYPE = complex mixture;"

mod_fixed <- MplusAutomation::mplusObject(
  TITLE = "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years",
  MODEL = mplus_mod_fixed_jorm,
  VARIABLE = mplus_variable_fixed_jorm,
  ANALYSIS = mplus_analysis_fixed_jorm,
  OUTPUT = mplus_output_103_jorm,
  SAVEDATA = "H5RESULTS = model_103b_jorm.h5;
  SAVE = CPROBABILITIES; file = cprob_jorm.dat; FORMAT = F20.12;",
  usevariables = c("id", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "jorm"

  ),
  rdata = inhcap_jorm

)
pmm_mplus_run(mod_fixed, "pmm_hcap_103b_jorm.inp", outputs = c("model_103b_jorm.h5", "cprob_jorm.dat"))

pmm_103b_jorm <- read_mplus_model_quietly(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.out"))

check_fixed_scoring_run(pmm_models$informant$scoring_out, nrow(inhcap_jorm),
                        pmm_models$informant$calibration_h5, pmm_models$informant$scoring_h5)
