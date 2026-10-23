# PMM_090_model_registry.R
#
# The PMM models used for classification (2026-10-22). Every script that reads
# PMM output takes the paths from here, so switching models is a change in
# this file only. Sourced by PMM_110, PMM_112, the A3 report chapters, and the
# downstream analyses that read PMM output (A2, A2_2, A3_2, Slides2603).
#
# Each participant receives probabilities from 1 model: the informant model
# when they have no cognition-model indicator, otherwise the cognition model.

pmm_models <- list(
  cognition = list(
    label = "Cognition model (general cognitive factor)",
    code = here::here("R", "PMM_102_calibration_model.R"),
    calibration_out = here::here("mplus_output", "pmm_102", "pmm_hcap_102.out"),
    calibration_h5 = here::here("mplus_output", "pmm_102", "model_102.h5"),
    scoring_inp = here::here("mplus_output", "pmm_102", "pmm_hcap_102b.inp"),
    scoring_out = here::here("mplus_output", "pmm_102", "pmm_hcap_102b.out"),
    scoring_h5 = here::here("mplus_output", "pmm_102", "model_102b.h5")
  ),
  informant = list(
    label = "Informant model (Jorm IQCODE)",
    code = here::here("R", "PMM_103_informant_model.R"),
    calibration_out = here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103_jorm.out"),
    calibration_h5 = here::here("mplus_output", "pmm_103_jorm", "model_103_jorm.h5"),
    scoring_inp = here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.inp"),
    scoring_out = here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.out"),
    scoring_h5 = here::here("mplus_output", "pmm_103_jorm", "model_103b_jorm.h5")
  )
)
