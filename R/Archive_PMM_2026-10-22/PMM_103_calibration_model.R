# 2026-10-22: the PMM uses the same inputs as the core algorithm. Not used:
# ADL and IADL items (nPG014-nPG059) and immediate word recall (vdwdimmz).
# Cognition model: 8 cognitive items + self-rated memory (PD102).
# Informant model: Jorm IQCODE only.




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


inhcap <- PMM_100 %>%
  filter(inHCAP==1)

inhcap <- inhcap %>%
  mutate(id = as.numeric(id))

# Participants with at least 1 cognition-model indicator observed. Mplus
# analyzes only these, so the class priors are computed on them.
inhcap_cog <- inhcap %>%
  filter(if_any(all_of(pmm_cog_indicators), ~ !is.na(.x)))


## Mplus model with population weights
fs::dir_create(here::here("mplus_output", "pmm_103"))
setwd(here::here("mplus_output", "pmm_103"))

mplus_mod = "

  vdwddelz with vdlfl1z@0 vdexf7z@0;
  vdexf7z with vdlfl1z@0;

   %cg#2%
   [vdori$1 vdlfl2$1 vdlfl3$1 vdsevens$1 vdcount$1
        PD102$1 PD102$2];
   [vdwddelz vdlfl1z vdexf7z];
   %cg#3%
   [vdori$1 vdlfl2$1 vdlfl3$1 vdsevens$1 vdcount$1
        PD102$1 PD102$2];
   [vdwddelz vdlfl1z vdexf7z];"


mplus_mod_103 <- str_c("%OVERALL%", " \n ",
                        mplus_mod_norms, " \n ",
                        mplus_mod)

# The subpopulation option doesn't work with knownclass
mplus_variable_103 = "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
idvariable = id;
classes = cg (3);
knownclass = cg  (vs1hcapdxeap = 1 vs1hcapdxeap = 2 vs1hcapdxeap = 3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

mplus_analysis_103 = "estimator = mlr;
ALGORITHM=INTEGRATION;
starts = 80;
processors = 12;
TYPE = complex mixture;"

mplus_output_103 <- "standardized;"

mod1 <- MplusAutomation::mplusObject(
  TITLE = "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years",
  MODEL = mplus_mod_103,
  VARIABLE = mplus_variable_103,
  ANALYSIS = mplus_analysis_103,
  OUTPUT = mplus_output_103,
  SAVEDATA = "H5RESULTS = model_103.h5;",
  usevariables = c("id", "vs1hcapdxeap", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
                   "PD102"

  ),
  rdata = inhcap

)
MplusAutomation::mplusModeler(mod1, modelout = "pmm_hcap_103.inp", run = 1, writeData = "always")

# mplush5::mplus.view.results(here::here("mplus_output", "pmm_103", "model_103.h5"))
# mplush5::mplus.print.results.in.probability.scale(here::here("mplus_output", "pmm_103", "model_103.h5"))

pmm_103 <- read_mplus_model_quietly(here::here("mplus_output", "pmm_103", "pmm_hcap_103.out"))
check_prior_sample(inhcap_cog, pmm_103, "pmm_103")

mplus_mod_fixed <- write_lca_model(pmm_103, mplus_mod_norms)
# Fix class logits at the survey-weighted HCAP class proportions so that the
# posterior class probabilities use the weighted calibration priors.
mplus_mod_fixed <- sub("%OVERALL%",
                       str_c("%OVERALL% \n ",
                             weighted_class_logits(inhcap_cog, "vs1hcapdxeap", "HCAP16WGTR")),
                       mplus_mod_fixed, fixed = TRUE)
mplus_variable_fixed = "categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount PD102;
idvariable = id;
classes = c (3);
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;"

mplus_analysis_fixed = "estimator = mlr;
ALGORITHM=INTEGRATION;
starts = 80;
processors = 12;
TYPE = complex mixture;"

mod_fixed <- MplusAutomation::mplusObject(
  TITLE = "x1-x3: age splines
  x4: female
  x5: black
  x6: hispanic
  x7: school years",
  MODEL = mplus_mod_fixed,
  VARIABLE = mplus_variable_fixed,
  ANALYSIS = mplus_analysis_fixed,
  OUTPUT = mplus_output_103,
  SAVEDATA = "H5RESULTS = model_103b.h5;
  SAVE = CPROBABILITIES; file = cprob.dat;",
  usevariables = c("id", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7",
                   "PD102"

  ),
  rdata = inhcap

)
MplusAutomation::mplusModeler(mod_fixed, modelout = "pmm_hcap_103b.inp", run = 1, writeData = "always")

pmm_103b <- read_mplus_model_quietly(here::here("mplus_output", "pmm_103", "pmm_hcap_103b.out"))
check_prior_sample(inhcap_cog, pmm_103b, "pmm_103b")

#####################################

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
MplusAutomation::mplusModeler(mod1, modelout = "pmm_hcap_103_jorm.inp", run = 1, writeData = "always")

# mplush5::mplus.view.results(here::here("mplus_output", "pmm_103", "model_103.h5"))
# mplush5::mplus.print.results.in.probability.scale(here::here("mplus_output", "pmm_103", "model_103.h5"))

pmm_103_jorm <- read_mplus_model_quietly(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103_jorm.out"))

mplus_mod_fixed_jorm <- write_lca_model(pmm_103_jorm, "")
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
  SAVE = CPROBABILITIES; file = cprob_jorm.dat;",
  usevariables = c("id", "HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "jorm"

  ),
  rdata = inhcap_jorm

)
MplusAutomation::mplusModeler(mod_fixed, modelout = "pmm_hcap_103b_jorm.inp", run = 1, writeData = "always")

pmm_103b_jorm <- read_mplus_model_quietly(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.out"))









