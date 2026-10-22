# Archive: PMM code retired on 2026-10-22

Filed away when the general-factor cognition model (PMM_102) became the only
PMM cognition model. Kept for reference; none of these files is sourced by a
driver. Paths inside them refer to the project layout of 2026-10-22, and
scripts here assume they run from `R/`; move a file back to `R/` before
running it.

| File | What it is | Why retired |
|:--|:--|:--|
| PMM_103_calibration_model.R | No-factor cognition model (8 tests and self-rated memory as separate indicators, independent within class) plus the informant model | Replaced by PMM_102 as primary (2026-10-22): BIC 18,672 vs 19,655 with 21 fewer parameters; within-class independence contradicted by the norms data. The informant model now lives in `R/PMM_103_informant_model.R`. |
| PMM_103_calibration_model.qmd | Report chapter for the above: final-model figure and text, parameters, probability plots, unweighted and most-likely-class cross-tables and kappas | Same; agreement now comes only from PMM_110 and PMM_112 (survey-weighted) |
| PMM_102_calibration_model_before_2026-10-22.qmd | PMM_102 report chapter as it stood (commented out of the control file; joined probabilities to data by row position; unweighted kappas) | Rewritten |
| PMM_025_Plan_for_cross_validation.qmd | Report chapter: plan for 10-fold cross-validation | The report no longer contains cross-validation (removed 2026-10-22) |
| PMM_040_Data_cleaning_functional_items.qmd | Report chapter: recoding of the ADL and IADL items (skip logic, nonresponse codes) | The PMM does not use ADL/IADL items (2026-10-22). The recoding code (`PMM_041`) still runs, because Analysis A5 documents these variables |
| A2_2_025_PMM_preliminary_model.qmd | A2_2 slide with the deprecated preliminary-model figure (PMM-012) | Its text moved into the A2_2 covariate-adjustment slide (2026-10-22) |
| PMM_104_weight_check.R | Side check (2026-09-23): Mplus KNOWNCLASS class logits ignore WEIGHT; class-specific parameters use it | Hard-codes the PMM_103 inputs. Its finding (compute weighted priors outside Mplus) still holds and is applied in `weighted_class_logits()` |
| PMM_105_residual_covariance_models.R | No-factor model with residual covariances among the 3 continuous scores | Never run; moot once PMM_102 was chosen |
| PMM_106_cross_validation.R | 10-fold cross-validation of PMM_102 vs "change 1" (item intercepts and thresholds free by class) with a rule fixed before results | Run 2026-10-22; change 1 not adopted (3-class kappa +0.037, dementia +0.046, log score -0.013). Outputs: `mplus_output/pmm_106/`, `R_objects/PMM_106_cross_validation.RDS`, `Reports/PMM_106_*_2026-10-22.*` |

Details and results: `References/SESSION_LOGS.md`, entry of 2026-10-22.
Full pre-change snapshot (code and outputs): `_snapshots/2026-10-22_pre_noADL/`.
