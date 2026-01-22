
PMM_45 <- readRDS( here::here("R_objects", "PMM_045.RDS"))

PMM_100 <- PMM_45 %>%
  mutate(id = str_c(HHID, PN),
         SECU_mplus = 10*STRATUM+SECU)

PMM_100 <- PMM_100 %>%
  mutate(PD102 = case_when(PD102==8 ~ NA,
                           TRUE ~ PD102))


robust_norms <- PMM_100 %>%
  filter(normexcld==0)

# table(PMM_100$normexcld)

########################################
mplus_analysis = "estimator = mlr; TYPE = complex;"
mplus_variable = "!idvariable = id;
weight = HCAP16WGTR; stratification = STRATUM; cluster = SECU_mplus;
categorical = vdori vdlfl2 vdlfl3 vdsevens vdcount;
!SUBPOPULATION = normexcld==0;"
mplus_output = "standardized;"
mplus_title = ""
mplus_mod = "vdori on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdlfl1z on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdlfl2 on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdlfl3 on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdwdimmz on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdwddelz on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdexf7z on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdsevens on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;

  vdcount on  x1 x2 x3 x4 x5 x6 x7
  x1x4 x1x5 x1x6 x1x7
  x2x4 x2x5 x2x6 x2x7
  x3x4 x3x5 x3x6 x3x7
  x4x5 x4x6 x4x7 x5x7 x6x7;"

fs::dir_create(here::here("mplus_output", "norm_npb"))
setwd(here::here("mplus_output", "norm_npb"))

# The variable x5x6 has no variability
mod1 <- MplusAutomation::mplusObject(
  TITLE = mplus_title,
  MODEL = mplus_mod,
  VARIABLE = mplus_variable,
  ANALYSIS = mplus_analysis,
  OUTPUT = mplus_output,
  SAVEDATA = "H5RESULTS = norm_npb.h5;",
  usevariables = c("HCAP16WGTR", "STRATUM", "SECU_mplus",
                   "vdori", "vdlfl1z", "vdlfl2", "vdlfl3", "vdwdimmz",
                   "vdwddelz", "vdexf7z", "vdsevens", "vdcount",
                   "x1",  "x2", "x3", "x4", "x5", "x6", "x7",
                   "x1x4", "x1x5", "x1x6", "x1x7",
                   "x2x4", "x2x5", "x2x6", "x2x7",
                   "x3x4", "x3x5", "x3x6", "x3x7",
                   "x4x5", "x4x6", "x4x7",  "x5x7", "x6x7"),
  rdata = robust_norms

)
MplusAutomation::mplusModeler(mod1, modelout = "norm_npb.inp", run = 1, writeData = "always")

## write the mplus code for the npb norming regressions
# mplush5::mplus.view.results(here::here("mplus_output", "norm_npb", "norm_npb.h5"))
results <- mplush5::mplus.print.model.results(here::here("mplus_output", "norm_npb", "norm_npb.h5")) %>%
  tibble()

mplus_mod_norms <- results %>%
  filter(!Section %in% c("Intercepts", "Residual Variances", "Thresholds")) %>%
  mutate(statement_lower = str_to_lower(Statement),
         est = round(Estimate, 5),
         mod = str_c(statement_lower, " @ ", est, "; ")) %>%
  pull(mod) %>%
  str_c(collapse = " \n ")


saveRDS(PMM_100, here::here("R_objects", "PMM_100.RDS"))
saveRDS(mplus_mod_norms, here::here("R_objects", "PMM_100_mplus_mod_norms.RDS"))










