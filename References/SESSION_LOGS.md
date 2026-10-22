# Session Logs

## 2026-10-22 (afternoon): general-factor PMM carried through A3, A3_2, A9, A2, A2_2; terminology

Decisions are recorded in the new `References/decisions.md` (D-2026-10-22-1 to -9).

### Confirmed facts
- PMM_102 identification fix (`[gcp@0]` in Normal): loglikelihood -9169.844 (was -9169.849), 40 parameters (was 41), condition number 2.6E-6 (was 2E-10); GCP means MCI -1.393 (SE 0.092), Dementia -3.037 (SE 0.173); GCP variances 1, 1.507, 2.313. Standardized loadings 0.26 to 0.51 and rise from Normal to Dementia.
- No participant has an MCI probability above about 0.5. With the fitted GCP class distributions and priors, the largest possible MCI posterior is about 0.60 if GCP were known exactly (0.54 with equal priors) and about 0.47 with the measurement error implied by within-class omega of about 0.50 (Normal class). MCI is the most likely class for 276 participants, mean probabilities 0.29/0.46/0.26. Now computed in `R/PMM_114_mci_ceiling.R`.
- PMM_106 cross-validation (n = 3,283, pooled held-out, survey-weighted): current 3-class kappa 0.322, dementia 0.256, log score -0.622; change 1: 0.359, 0.303, -0.634. Shares from probabilities match the Manly-Jones shares (72.2/20.5/7.3 vs 72.0/20.7/7.3); most likely class 88.0/8.2/3.7.
- Manuscript kappas (PMM_110) use probability allocation (100 replicates); class shares (PMM_112) use the most likely class.

### Code and text changes
- A3: `PMM_090_model_registry.R` (new); informant model moved to `PMM_103_informant_model.R`; `PMM_102_calibration_model.qmd` rebuilt (parameters with SEs, probability plots joined by id, MCI section); `PMM_103_informant_model.qmd` (new); PMM_110 and PMM_112 read the registry; `PMM_114_mci_ceiling.R` (new). Text: PMM_100 results introduction rewritten (2 models, rationale "did not include", calibration and scoring, priors, probability allocation); PMM_011 approach (Figure 1 = `PMM-103-PMM-with-latent`; no ADL; 8 x 24 coefficients computed; ML "consistent"; duplicate `fig-fig3` label fixed); abstract (no cross-validation claims; Step 3 "Scoring"); PMM_025 and PMM_040 chapters removed; references add Jorm (1994) and Muthen (2002) (DOI not verified, omitted).
- A3_2: registry paths; no ADL/IADL or vdwdimmz inputs; informant run without categorical/INTEGRATION; person-waves with no model input left unscored; new `R_objects/A3_2_020_routing.rds`; report text updated.
- A9 (Slides2603): rewritten as a current deck; all values computed (A7_075, A7_100, PMM_110, PMM_112, PMM_114); results figure now covers all HRS/HCAP participants (combined probabilities).
- A2: MS_010 reads the registry, PMM Ns, covariate count, MCI ceiling; Methods PMM section rewritten (outline fragments); Results and Discussion MCI items; Appendix 2 without functional items; Figure 1 of the tables document now the current schematic with computed cut points in the caption.
- A2_2: core algorithm slide uses the current schematic with computed cut points; PMM model slide uses Figure 1; preliminary-model slide archived; "five of the eight" tests.
- Terminology applied in A2, A2_2, PMM_110, PMM_112 (display relabel of the saved A7 matrix in MS_010).
- README, `References/instructions.md` (terminology rules; registry, archive, and dropped-indicator workflow rules), `References/decisions.md` (new). `tools/update_readme.py` run; `--check` passes.
- Rich's debugging: placeholder rows for categories missing from a held-out fold in PMM_106; left/full joins in PMM_103 chapter, PMM_110, PMM_112 (morning).

### Problems found
- A wrong-anchor insertion briefly split the Jones et al. (2024) entry in PMM_888; repaired and checked against the snapshot.
- Static figures with typed counts: A2 tables Figure 1 (April 2026 counts, old cut points) replaced. A8's flow figure (April counts) is still static (not in today's scope).

### Open questions
- Rerun and review: A3, A3_2, A9, A2, A2_2 after the last edits (A9 before A2_2).
- Commit and push today's renders; the README links point to untracked files until then.
- A2 appendix figure `figa1.excalidraw.svg` (HRS 2016 subsamples, legacy A1 derivation n = 2,993) may be stale; not reviewed.
- A8 static algorithm flow figure (April counts).
- Manuscript reference list does not exist yet; it will need Muthen (2002) and Jones et al. (2024).
- Deferred analyses: class-specific residual variances; cross-validated agreement for both methods; ADL/IADL sensitivity model.

---

## 2026-10-22 (morning): PMM inputs matched to the core algorithm; general-factor model chosen

### Confirmed facts
- Snapshot before any change: `_snapshots/2026-10-22_pre_noADL/` (git HEAD 3041d2b; code, R_objects, mplus_output, figures, and latest reports for A3, A3_2, A2, A2_2; README has the restore command). Local only; git ignores `_snapshots/`.
- With ADL/IADL removed, the 213 HCAP participants without cognitive testing have no cognition-model indicator, and Mplus drops them. Cognition model N = 3,283; informant (Jorm) model N = 213; no overlap.
- PMM_103 (no factor) fixes all within-class residual covariances at 0, including the 3 continuous scores (explicit `@0` statements override the norms-model values, standardized 0.17 to 0.22 in the robust norms group). Ordinal items are independent within class by Mplus default.
- PMM_102 (general factor, gcp) was not identified: gcp mean free in all 3 known classes with intercepts and thresholds equal across classes. Fixed by `[gcp@0]` in the Normal class: loglikelihood unchanged (-9169.84), parameters 41 to 40, condition number 2E-10 to 2.6E-6, gcp means MCI -1.39 (SE 0.09), Dementia -3.04 (SE 0.17); gcp variances 1.0, 1.5, 2.3. The pre-ADL PMM_102 had the same problem.
- In PMM_102 all measurement parameters (loadings, intercepts, thresholds, residual variances) are equal across classes; only gcp mean and variance and the PD102 thresholds differ by class.
- Manuscript PMM kappas (PMM_110) use probability allocation (100 replicates per person); the class shares in Table 2 (PMM_112) use the most likely class. Most-likely-class assignment undercalls MCI.
- After removing ADL/IADL and vdwdimmz (PMM_103, A2 render 2026-10-22): PMM vs HRS/HCAP Algorithm 3-class kappa 0.509 to 0.450, dementia kappa 0.429 to 0.377 (core algorithm 0.501, 0.337). PMM vs consensus 0.486 to 0.161 (n about 50).

### Decisions (Rich)
- The PMM uses the same inputs as the core algorithm: no ADL/IADL items (nPG014-nPG059) and no immediate word recall (vdwdimmz) in PMM_100 (norms), PMM_102, and PMM_103. Report text says these were "not included" (not "removed").
- Informant model: Jorm IQCODE as the only indicator (one-indicator known-class mixture).
- Outputs overwrite `pmm_102/`, `pmm_103/`, `pmm_103_jorm/`; the snapshot is the undo.
- PMM_102 scoring run fixes class logits at weighted shares, as PMM_103 does. Class priors for both cognition models are computed on the participants with at least 1 cognition-model indicator (`inhcap_cog`, n = 3,283); `check_prior_sample()` stops if this differs from the Mplus N.
- ADL/IADL and vdwdimmz dropped from the PMM_047 descriptives tables.
- The general-factor model (PMM_102) is the primary PMM, unless it develops convergence problems (Rich, 10:38). Reasons: BIC 18,672 vs 19,655 with 21 fewer parameters; within-class independence in PMM_103 is contradicted by the norms data.
- Change 1 (item intercepts and thresholds free by class, gcp mean 0 in every class) tested by 10-fold cross-validation with a rule fixed before results: adopt if CV 3-class kappa gain >= 0.02 and dementia kappa and log score not lower. Result: 3-class +0.037, dementia +0.046, log score -0.013 (worse in 8 of 10 folds). Not adopted. Tweaking the single-factor model is closed.
- Set aside: class-specific residual variances; reporting cross-validated agreement in the manuscript; ADL/IADL sensitivity model.

### Code changes
- `PMM_100_norming_npb.R`, `PMM_102_calibration_model.R`, `PMM_103_calibration_model.R`: ADL/IADL and vdwdimmz removed; Jorm model IQCODE only (no ALGORITHM=INTEGRATION); `[gcp@0]` in class 1 of PMM_102; priors on `inhcap_cog`.
- `PMM_101_mplus_function.R`: `pmm_cog_indicators`, `check_prior_sample()`.
- `PMM_000` abstract Step 2 and Step 3 reworded (two models, no counts in YAML).
- Rich's debugging: `full_join` of cognition and Jorm probabilities in `PMM_103_calibration_model.qmd`, `PMM_110`, `PMM_112`; placeholder rows for categories missing from a held-out fold in `PMM_106`.
- New side scripts (not in any driver): `R/PMM_105_residual_covariance_models.R` (no-factor model with continuous-score covariances; not run) and `R/PMM_106_cross_validation.R` (run 2026-10-22; `R_objects/PMM_106_cross_validation.RDS`, `Reports/PMM_106_cross_validation_2026-10-22.csv`, `Reports/PMM_106_console_2026-10-22.txt`).

### Cross-validation results (PMM_106, n = 3,283, survey-weighted, pooled held-out folds)
- Current (PMM_102): 3-class kappa 0.322, dementia 0.256, log score -0.622; shares from probabilities 72.2/20.5/7.3 (reference 72.0/20.7/7.3); most likely class 88.0/8.2/3.7.
- Change 1: 0.359, 0.303, -0.634; 71.8/20.4/7.8; most likely class 85.1/9.6/5.3.
- Not comparable with the manuscript kappa (0.450): different sample (no informant-model participants), exact probability allocation, and PMM_103.

### Problems found
- Claude did not check which participants had at least 1 observed indicator before dropping 10 items; the 213-person split broke the left joins.
- Claude checked training folds but not held-out folds for missing categories in PMM_106.

### Open questions
- PMM_102 vs PMM_103 agreement on the same sample and measures (not yet computed).
- Refactor A3 and downstream (A3_2, A2, A2_2, A9, A6, README/instructions) to the general-factor model. A3_2 and Slides2603 not yet updated; do not run them.
- Model figure `PMM-103-PMM-no-latent.excalidraw.svg` (Rich, uncommitted edits).

---

## 2026-10-19: AgentKit report archiving

- Added `Reports/archive_reports.R` and `Reports/archive_reports_README.md` as a new AgentKit framework component.
- The base-R script keeps the newest dated `.docx`, `.html`, `.pdf`, and `.pptx` report in each case-insensitive stub/tag/extension family, using filename date and optional time rather than modification time. Plain renders and tagged edits retain separate newest copies.
- Older copies move into `Reports/Archive/`; HTML companion `_files` folders move with their reports. Archive name collisions receive numbered suffixes, with companion-folder links rewritten when needed. Undated files, unsupported formats, and Word lock files are left alone.
- Defaults to the script's own folder; an optional folder argument selects another location. Failed moves are reported and produce a nonzero exit status. Adding the utility does not itself run archiving.
- Validated with temporary fixtures: newest-copy selection, separate tagged edits, excluded files, companion assets, collision relinking, and repeat-run idempotence. Validation did not reorganize the project's reports.

---

## 2026-04-22

### Decisions Made

**A8 slide deck structure finalized.**
- Deck summarizes Analysis A7 (HRS 2016 cognitive classification via CFA + normalization + diagnostic algorithm).
- Slides organized into four chapter files: Introduction, Methods, Results, Conclusion.
- Driver: `Analysis8_Driver.R`; control: `R/A8_Control.qmd`; output: `Reports/Slides_A7summary_[date].html`.

**Label standardization across all A8 slides.**
- "Hudomiet BLVM" — used everywhere instead of plain "Hudomiet".
- "HRS Classification (A7)" — used instead of "HRS classification model" or "V1".
- "HCAP Algorithmic classification" — used instead of "Algorithmic diagnosis".
- These renames were applied to `gt::cols_label()` calls and `gt::text_transform()` stubs in `A8_030_Results.qmd`, figure captions in `A8_020_Methods.qmd`, and text throughout `A8_010_Introduction.qmd` and `A8_040_Conclusion.qmd`.

**Slides deleted from Methods.**
- "Analytic Sample", "CFA Model", and "Classification Algorithm" slides were removed from `A8_020_Methods.qmd` to keep the deck concise. The content exists in the full A7 report.

**Slides deleted from Results.**
- "Reference Standards and Comparators", "3-way: HCAP Algorithmic Classification vs. Comparators", "2-way: HCAP Algorithmic Classification vs. Comparators" were removed. The agreement matrix slides (3-way and 2-way) were retained.

**New slides added to Results.**
1. Unweighted confusion matrix: `HRS Classification (A7) vs. HCAP Algorithmic Classification` — computed inline from `hcap_tables$data$hcap_sample` using `dplyr::count` + `pivot_wider` + `gt`.
2. Complex-sample weighted percentage table: same cross-tab but using `prop.table(xtabs(hcap_w ~ algorithmic + hrs, ...))` to show weighted % rather than counts.

**Hudomiet slide in Introduction enriched.**
- Slide describes Hudomiet BLVM as a joint Bayesian longitudinal latent variable model.
- Key details: latent cognition modeled as a linear function of age, health, and demographics; 901 posterior draws per person-wave; classification based on proportion of draws below threshold (not posterior mean); absolute thresholds (not relative/rank-based); longitudinal design captures change over time; supervised calibration via ADAMS.
- Slide uses `<span style="font-size: 0.7em;">` for the title and Quarto fenced divs (`:::`) for body text and footnotes to enable markdown rendering.

### Bug Fixes

- **gt `summary_rows()` deprecation (gt ≥ 0.9.0):** `summary_rows()` requires row groups to be present; without row groups must use `grand_summary_rows()` only. Removed the `summary_rows()` call from the new confusion matrix chunk.
- **Stray `|` instead of `|>` pipe** in the 3-way agreement matrix gt chain (`A8_030_Results.qmd`, `tbl-kappa-3way` chunk). Caused `gt::text_transform()` to receive no `data` argument. Fixed by replacing `|` with `|>`.
- **Markdown not rendering inside `<div style>` blocks:** Replaced all `<div style>...</div>` blocks in the Hudomiet slide with Quarto fenced divs (`:::`) so that bold, italic, and math render correctly.

### Current Analysis State

- All A8 slides render cleanly (`Analysis8_Driver.R` exits 0).
- Output committed to `main` at `37e2fe0` and pushed to `dougtommet/HRS_cog_classification`.
- Githack link for rendered deck: https://rawcdn.githack.com/dougtommet/HRS_cog_classification/37e2fe0/Reports/Slides_A7summary_2026-04-22.html

### Next Concrete Steps

- Review deck with collaborators; likely further slide edits based on feedback.
- Consider adding a driver-level entry in `instructions.md` for Analysis A0 (currently has no root-level driver).

### Open Questions / Deferred Issues

- The weighted percentage table uses overall `prop.table()` (cells sum to 100%). Row-conditional percentages (rows sum to 100%) may be more interpretable; deferred pending user preference.
- `Analysis1_Driver.R` references the old `R/000-master.qmd` (deleted); noted in README but not yet fixed.

---

## 2026-09-22

### Analysis 2 Manuscript and Appendices

- Reorganized the Analysis 2 manuscript under `R/MS_MAIN/`; its main control is `R/MS_MAIN/MS_Main_Control.qmd`.
- Added nested outline sections: `MS_Introduction.qmd`, `MS_Methods.qmd`, `MS_Results.qmd`, and `MS_Discussion.qmd`.
- Added standalone manuscript appendices: `MS_Appendix-1-Core-Algorithm.qmd`, `MS_Appendix-2-PMM.qmd`, and `MS_Appendix-3-Other-Study-Kappas.qmd`.
- Updated `Analysis2_Driver.R` to render the main manuscript, tables/figures report, and all three appendices as Word-safe DOCX files.
- Added/reused centralized dynamic results and cohort sources in `R/MS_MAIN/` so manuscript values, tables, and figures are generated from stored Analysis 1/7 and PMM results rather than hard-coded results.
- Restored the manuscript author block, abstract, and Word page break; retained outline-mode manuscript content and the preferred terms HRS/HCAP Algorithm, Core algorithm, and PMM.
- Synced manual Word edits back to Appendix 1: top-level labels are bold.
- Synced Appendix 2 with an inline italic PMM question and a compact practical-distinctions list.
- Set Appendix 3 as intentionally title-only: “Appendix 3: Comparisons with other study classifications.”

### Analysis 2 Validation

- Rebuild command: `Rscript Analysis2_Driver.R`.
- Verified successful generation and OOXML validity of `Reports/MS_Main_2026-09-22.docx` and `Reports/MS_Tab_Fig_Apndx_2026-09-22.docx`.
- Verified successful generation and OOXML validity of `Reports/MS_Appendix_1_2026-09-22.docx`, `Reports/MS_Appendix_2_2026-09-22.docx`, and `Reports/MS_Appendix_3_2026-09-22.docx`.

---

## 2026-09-23

### Analysis 3 Render Repair

- Fixed `R/PMM_000_Analysis_Report_Control.qmd`, which still sourced deleted legacy setup files (`R/001-libraries.R` and `R/002-folder_paths.R`).
- Updated the setup chunk to use the maintained `R/A1_001-libraries.R` and `R/A1_002-folder_paths.R` scripts.

### Analysis 3 Validation

- Rebuild command: `Rscript Analysis3_Driver.R`.
- Render completed successfully and created `Reports/PMM_Analysis_Report_2026-09-23.html`.
- The edited PMM report control had no editor diagnostics.
- `Rscript Analysis2_Driver.R` also completed successfully for the current manuscript outputs.

### PMM class priors: fixed at survey-weighted HCAP proportions (Analysis 3)

**Problem found.**
- The PMM scoring models (`pmm_hcap_103b`, `pmm_hcap_103b_jorm`) fixed every measurement parameter at the known-class calibration estimates but left the class logits (`[c#1 c#2]`) free.
- Mplus therefore re-estimated the class proportions by EM without the labels, even though scoring uses the same HCAP sample (N = 3,496) as calibration. Cognition model proportions (Normal/MCI/Dementia) moved from 66.2/23.1/10.7% (labeled) to 77.4/9.1/13.5% (free). The lower MCI prior pushes boundary cases toward Normal and may account for part of the weak PMM agreement at the MCI margin.
- Mplus KNOWNCLASS estimates the class logits from **unweighted** counts even when `WEIGHT = HCAP16WGTR` is specified. The known-class logits (cognition 1.819, 0.768; Jorm −0.479, −0.363) reproduce the unweighted class counts exactly.

**Decision.**
- Classification uses the plug-in Bayes rule (McLachlan, 1992): class profiles and class priors are both fixed at calibration values.
- The calibration priors must reflect the sampling weights (Rich, 2026-09-23).
- For future out-of-sample application (e.g., HRS Core), keep the priors fixed. HCAP is a random subsample of HRS age 65+, so the weighted prevalence transports. Freeing the priors (Saerens et al., 2002) is appropriate only when the target prevalence is expected to differ, such as in another cohort or country.

**Code changes.**
- `R/PMM_101_mplus_function.R`: added `weighted_class_logits(data, class_var, weight_var, fixed_class = "c")`. It computes survey-weighted class proportions in R and returns an Mplus statement fixing the logits (last class is the reference). Also added `write_class_logits()` (fixes logits at the Mplus known-class estimates). It is currently unused because those estimates are unweighted, and it can be deleted.
- `R/PMM_103_calibration_model.R`: inserted the weighted-logit statement into `%OVERALL%` of both scoring models: `weighted_class_logits(inhcap, "vs1hcapdxeap", "HCAP16WGTR")` for 103b and `weighted_class_logits(inhcap_jorm, ...)` for 103b_jorm. `write_lca_model()` is unchanged, so `PMM_102` is unaffected.
- Weighted priors: cognition 69.4/21.4/9.2% (logits 2.019, 0.842); Jorm 31.7/31.6/36.7% (logits −0.147, −0.149).
- Archived the free-prior outputs for comparison: `mplus_output/pmm_103/archive_free_priors/` and `mplus_output/pmm_103_jorm/archive_free_priors/` (`.dat` files are git-ignored).

**Weight check (side script, not in any driver).**
- Added `R/PMM_104_weight_check.R`; run with `Rscript R/PMM_104_weight_check.R`. For each calibration model it refits the weighted model with SVALUES, then refits with all weights set to 1, starting from the weighted solution (`starts = 0`). It then compares point estimates. Outputs: `mplus_output/pmm_104_weightcheck/` and `Reports/PMM_104_weight_check_[date].csv`.
- Result (2026-09-23): the weighted refits reproduced the original loglikelihoods (−16151.36; −1426.505). Class logits were identical weighted vs unweighted, so KNOWNCLASS priors ignore weights. Thresholds (cognition 78/78, max diff 0.79; Jorm 30/30, max 1.30) and most intercepts differed, so the weights do enter the class-specific parameters. Conclusion: only the priors needed correcting.
- The 18 "unmatched" parameters are residual covariances that SVALUES writes in reverse order (`A WITH B` vs `B WITH A`). The values are identical.
- The unweighted cognition model fails from default random starts ("covariance matrix in class 1 could not be inverted", iteration 1). It converges only from the weighted solution.

**Lessons for other analyses.**
- In Mplus mixture scoring runs with fixed parameters, fix the class logits too, unless re-estimating priors in the target sample is intended.
- Do not trust KNOWNCLASS class proportions to be survey-weighted. Compute weighted priors outside Mplus.
- Describe the PMM as maximum likelihood (MLR) estimation with Bayes-rule posterior classification, not as Bayesian estimation.
- The PMM classifications are in-sample (resubstitution). Agreement with the HRS/HCAP Algorithm is optimistic (the apparent error rate).
- `Project_Map.md` and the Analysis 3 description say the PMM is applied to HRS Core (N = 9,972). The current code scores HCAP only (N = 3,496). Update these before the manuscript claims out-of-sample application.

**Methods wording (draft).** Class-specific parameters were estimated with HCAP sampling weights. Class proportions were fixed at the weighted HCAP distribution, computed outside Mplus, because Mplus KNOWNCLASS estimates class proportions from unweighted counts.

**Candidate references.** Muthén (2002, *Behaviormetrika*, 29, 81–117; training data/known class); Hosmer (1973); McLachlan & Basford (1988); McLachlan (1992; plug-in rule, apparent error rate); Hastie & Tibshirani (1996); Fraley & Raftery (2002); Vermunt & Magidson (2003); Saerens, Latinne, & Decaestecker (2002; prior-shift adjustment); Bouveyron et al. (2019).

**Next steps.**
- Rich to rebuild locally: `Rscript Analysis3_Driver.R`, then `Rscript Analysis2_Driver.R`.
- Verify that `pmm_hcap_103b.inp` contains `[ c#1 @ 2.019… c#2 @ 0.842… ]` and that the 103b class proportions are near 69/21/9%.
- Compare the new PMM kappas against the archived free-prior results. Update the manuscript's PMM results and Methods.

### Analysis 2 study sample set to all HRS/HCAP participants (N = 3,496)

- Decision (Rich): the study sample is all 3,496 HRS/HCAP 2016 participants. The Core analytic sample (9,218) and the "HCAP complete-assessment subset" (030_hcap.rds) are dropped from the manuscript.
- Rationale: both Core classification approaches classify every HCAP participant. 213 HCAP participants have no Core cognitive test. All 213 have a Jorm IQCODE and an HRS/HCAP Algorithm diagnosis (57 Normal, 64 MCI, 92 Dementia). The Core algorithm uses its Jorm fallback (`A7_075`), and the PMM uses the Jorm model (`PMM_110`).
- `R/MS_MAIN/MS_001_analysis_cohort.R`: `analysis2_hcap` = all `inHCAP == 1` (checked to equal 3,496, with unique HHID/PN). Removed `analysis2_core` and the at-least-1-cognitive-test rule.
- `R/MS_MAIN/MS_010_results_objects.R`: the sample flow now shows the input and the study sample only. Added `analysis2_n_compare`, the Ns behind the agreement statistics (A7_100 and PMM_110 also require non-missing Langa-Weir).
- `MS_Main_Control.qmd` (Abstract), `MS_Methods.qmd`, and `MS_Results.qmd` now report the single study sample. The Methods outline shows the agreement-statistic Ns so any shortfall from 3,496 is visible.
- `MS_Tab_Fig_Apndx-030-Table-1.qmd`: rewritten as a single study-sample column. Removed the stray gtsummary table and the 2993 helper comment.
- Figure 1 (sample flow) will be dropped and replaced by another figure (Rich). Its caption was updated but otherwise left as is.
- Open: if the agreement-statistic Ns are below 3,496, the missing cases lack a Langa-Weir classification. Decide whether to relax that filter.

### Final 2026-09-23 Analysis 2 Tables, Figures, and Driver Maintenance

- Repaired stale `001-libraries.R` / `002-folder_paths.R` setup references in the Analysis 3, Analysis 5, Analysis 6, and Slides2603 report controls. Analysis 5 now contains a local replacement for its unavailable external `varlablist.r` helper. Analysis 8 already used the maintained setup script.
- Verified the repaired report commands: `Rscript Analysis3_Driver.R`, `Rscript Analysis5_Driver.R`, `Rscript Analysis6_Driver.R render-qmd`, `Rscript Analysis8_Driver.R`, and `Rscript Slides2603_Driver.R`.
- Refined the Analysis 2 tables/figures DOCX:
	- Table 1 reports HRS/HCAP and Langa-Weir classifications, Core Jorm IQCODE mean/SD, and separate dynamic missing counts. It clarifies that Jorm is collected in Core only for cognitive-test non-completers.
	- Table 2 has ordered primary and secondary methods, unweighted participant counts, HCAP-weighted percentages rounded to one decimal place, readable headers, a six-inch layout, and algorithm citations.
	- Table 3 has the requested comparison order, source information in the footer only, dynamically calculated PMM-Hudomiet agreement, and blank rows between comparator groups.
	- All flextables use Arial rather than explicit Cambria overrides, matching the reference DOCX Normal typeface.
- Added a manuscript Figure 2 workflow:
	- `R/PMM_112_Margins.R` exports `Figures/MS_Main-Figure-2-PMM-Class-Probabilities.png` at 600 DPI (6000 x 2700 px), with no plot title and the x-axis label “Class probability.”
	- `MS_Tab_Fig_Apndx-060-Figure-2.qmd` embeds the PNG; Figure 1 and Figure 2 labels are numbered correctly in the tables/figures DOCX.
- Made all five Analysis 2 DOCX controls use `date: '`r Sys.Date()`'`: main manuscript, tables/figures, and Appendices 1–3.

### Final Validation

- `Rscript Analysis2_Driver.R` completed successfully after each final change.
- The five dated DOCXs are valid OOXML and display the render date.
- The tables/figures DOCX passed OOXML and Pandoc checks; Table 1 and Table 2 percentages display to one decimal place, and Figure 2 is embedded at the expected high resolution.

### Table 1 row-order fix

- The Jorm "Missing" row (3,283; participants who completed Core cognitive testing, so no Jorm was collected) sorted above the Jorm Mean (SD) row. It therefore displayed under the Langa-Weir block as a second "Missing" entry. `MS_Tab_Fig_Apndx-030-Table-1.qmd` now orders rows within each variable as: header or Mean (SD) first, categories by descending count, Missing last.
- Page break before Table 1: already present in `MS_Tab_Fig_Apndx_Control.qmd` after the cover page include. It is also present in the 15:03 render of `Reports/MS_Tab_Fig_Apndx_2026-09-23.docx` (the break paragraph follows the Date line).

### PMM Recalculation and Updated Reporting

- Removed `#| eval: false` from `R/PMM_000_Analysis_Report_Control.qmd`. Analysis 3 now evaluates the documented PMM preparation, calibration, and scoring scripts during rendering rather than relying only on pre-existing saved objects.
- `Rscript Analysis3_Driver.R` completed after the guard was removed, refreshing the PMM Mplus outputs, class-probability figures, and `Reports/PMM_Analysis_Report_2026-09-23.html`.
- `Rscript Analysis2_Driver.R` completed using the refreshed PMM outputs; the manuscript, tables/figures report, and three appendices now reflect the updated PMM results.
- Table 1 row ordering was also corrected so the Jorm Mean (SD) appears before the Jorm missingness row; regenerated figures, DOCX reports, and Slides2603 output were refreshed with the current PMM results.

### PMM results updated with weighted priors (refit complete)

- The PMM calibration and scoring models were refit with the weighted-prior code. With the `eval: false` guard removed (see the entry above), `Analysis3_Driver.R` refits them, and Analysis 2 and Slides2603 were rebuilt from the refreshed outputs.
- Verified: `pmm_hcap_103b.inp` fixes `[ c#1 @ 2.01895756 c#2 @ 0.84226366 ]` and `pmm_hcap_103b_jorm.inp` fixes `[ c#1 @ -0.14665973 c#2 @ -0.14869742 ]`. The 103b model-estimated class proportions are 69.4/21.4/9.2% (Normal/MCI/Dementia), matching the weighted HCAP priors.
- The fix made a big difference for MCI. PMM weighted margins (Table 2, HCAP16WGTR), free priors to weighted priors:
  - Normal: 82.3% to 76.7%
  - MCI: 4.7% to 13.6%
  - Dementia: 12.9% to 9.7%
- Agreement changed little, and the pattern holds: PMM agrees with the HRS/HCAP Algorithm at least as well as the Core algorithm does.
  - PMM vs HRS/HCAP Algorithm: 3-class kappa 0.509 to 0.509; binary kappa 0.437 to 0.429. Core algorithm: 0.505 and 0.352 (unchanged).
  - PMM vs HCAP consensus: 3-class 0.528 to 0.486; binary 0.384 to 0.464.
  - PMM vs Langa-Weir: 3-class 0.583 to 0.575; binary 0.487 to 0.480.
  - PMM vs Hudomiet: 3-class 0.633 to 0.623; binary 0.515 to 0.529.
- PMM MCI (13.6%) remains below the HRS/HCAP Algorithm's weighted MCI proportion (21.4%). Modal assignment still favors Normal at the Normal/MCI boundary.
- Updated outputs: `Figures/MS_Main-Figure-2-PMM-Class-Probabilities.png`, the Slides2603 class-probability figures, and the dated Analysis 2 DOCX reports. The free-prior outputs remain in `mplus_output/*/archive_free_priors/`.
- Next: commit and push the refit outputs and rebuilt reports. Update the manuscript's PMM text for the new margins. Each Analysis 3 render now refits all PMM models, so expect longer run times.

### 2026-10-06 Analysis A3_2 created: PMM scoring of HRS Core 2016–2022

- Goal (Rich): treat the A3 PMM as a scoring machine with all parameters fixed at HCAP16 values and produce person-wave Normal/MCI/Dementia probabilities for HRS Core 2016, 2018, 2020, 2022.
- Decisions (Rich): inputs from Analysis A0 (not an extension of A1), with an HCAP 2016 reproduction check against A3; sample is all respondents interviewed in the wave and age 65+ (no weight restriction); class priors fixed at HCAP16 only; `A3_2_` prefix.
- Files: `Analysis3_2_Driver.R`; `R/A3_2/A3_2_000-Main_control.qmd` (letter.css report), `A3_2_001` libraries, `A3_2_010` build scoring file, `A3_2_020` Mplus scoring, `A3_2_030` HCAP16 check. README section added; `tools/update_readme.py` now accepts `A#_#` section ids.
- Implementation notes:
  - The MODEL sections are read verbatim from `pmm_hcap_103b.inp` and `pmm_hcap_103b_jorm.inp`. Scoring runs drop weights/strata/cluster (no free parameters, so posteriors do not depend on them).
  - Centering constants for x4, x5, x6, x4x5, x4x6 are recovered from `PMM_100.RDS` (raw minus centered), so they equal the HCAP16 weighted means.
  - The script stops if any categorical input's observed values differ from HCAP16, because Mplus maps categories by observed values and the fixed thresholds would shift.
  - Structurally missing inputs: `vdexf7z` (number series) in 2018; `nPG040` (IADL maps) in 2022. Mplus treats these as missing for the whole wave.
  - Model choice per person-wave follows PMM_110 (Jorm model when Jorm present).
- Run on 2026-10-06 after rebuilding Analysis A0: all 36,236 person-waves across 2016, 2018, 2020, and 2022 scored. In the HCAP16 reproduction check, 3,470 participants matched across pipelines; model choice and modal class agreed for all, 99.83% of posterior comparisons differed by less than 0.001, and the maximum difference was 0.057.
- Runtime fixes: A0 cognitive recodes used a non-logical `na.rm` value, and the A3_2 score file read a stale tracker object without the current covariates. Mplus also truncated the long categorical-variable list at its line limit. Its Brant Wald diagnostic for `vdsevens` is nonfatal when Mplus terminates normally and writes complete posterior output.
- Report update (same day): inputs absent from a wave's Core file, or missing for every respondent in a wave, are labeled "not administered" (missing by design). Added a number series paragraph that lists the waves with and without number series from the data. Open question: Rich recalls number series as absent in 2014, 2016 (taken from HCAP), 2018, and 2020. The A0 and A1 code reads Core `PNSSCORE`, `RNSSCORE`, and `SNSSCORE`, so this needs to be checked against the raw files.
- Follow-up: the HRS question concordance lists wNSSCORE in 2010, 2012, 2020, and 2022 (Rich). That agrees with A0 for 2018 (absent) and 2020/2022 (present). It does not list 2016, yet A0_009 selects `PNSSCORE` from Core `H16D_R.dta` without error. To check: `codebook PNSSCORE` in H16D_R. Report wording changed from "alternate waves" to "not in every wave".
- Report update: replaced the per-wave scatterplot matrices with one matrix per class (Normal, MCI, Dementia) across the four waves, each on its own page. Probabilities are reshaped long-to-wide inside the report only and probit-transformed (z = qnorm(p)) after clamping to [0.0005, 0.9995] because Mplus saves posteriors to 3 decimals (Rich chose clamp-only over more Mplus precision). Each matrix has a correlation table underneath (r below diagonal, pairwise N above, wave N on diagonal).

### 2026-10-07 Reporting rule: weighted population counts in thousands

- Request (Rich): report survey-weighted population counts in thousands, not exact persons, because grossing up a modest sample to an exact count implies false precision. Apply in the A7 report and add the rule to the project rules. Do not change `_AgentKit`.
- Decisions (Rich): the shared table helpers may carry the new format into other reports; label units with a note under each table; entries below 1 (fewer than 1,000 persons) keep two significant digits.
- Rule added as "Reporting Rules" in `References/instructions.md` and `README.md`. It is framed as a presentation standard; inferences on population counts need a design-based 95% confidence interval.
- Code: `R/A7_100-comparison_of_diagnoses.R` gains `format_weighted_thousands()`, `weighted_thousands_note`, and a `units` argument ("thousands" or "percent") for `format_weighted_tab()` and `format_binary_weighted_tab()`. Tables using HCAP population weights now show thousands with a note. The eight consensus-panel tables keep `units = "percent"` because `consensus_wt` is rescaled to sum to 100.
- Code: `R/A7_075-implement_algorithm.qmd` weighted `tbl_svysummary` (display design with weights / 1,000; percentages now show one decimal) and weighted `dx_v1` by `vs1hcapdxeap` crosstab now show thousands with the note.
- A2 and A8 do not display weighted counts from A7 (they use unweighted n, weighted percentages, and kappas), so their output should not change.
- Fix after first rerun: the Langa-Weir vs Consensus table in `R/A7_100-comparison_of_diagnoses.qmd` called `format_weighted_tab()` directly and so got the thousands format. It now prints the stored `hcap_tables$tables$lw_consensus`, like the other tables.
- Consensus table notes (Rich): "normalized within the consensus sample" replaced by "Cell entries are weighted percentages of the validation subsample." The two Consensus vs Hudomiet tables referenced an undefined note (`notes$consensus_hudomiet`) and printed none; they now use `notes$consensus_standard`.
- Next: Rich reruns `Rscript Analysis7_Driver.R` locally and checks the tables.

### 2026-10-07 Analysis A3 report: final PMM figure

- Request (Rich): add `R/excalidraw/PMM-103-PMM-no-latent.excalidraw.svg` (final PMM, no latent cognition factor; cognition and function model plus informant model) to the A3 report with explanatory text.
- Placement: figure (`@fig-pmm-final`) and text at the top of "Model - Individual item indicators" in `R/PMM_103_calibration_model.qmd`; a callout after Figure 1 in `R/PMM_011_Overall_Approach.qmd` notes that the background figures show the original latent-factor design.
- Figure vs Mplus inputs: the informant panel shows the Jorm only, but `pmm_hcap_103b_jorm.inp` also uses the ten ADL/IADL items. The dashed covariate box shows baa x hisp, which `pmm_hcap_103b.inp` does not include (24 terms). Text follows the Mplus inputs. Open for Rich: revise the drawing.
- Figure label fix (Rich): `R/PMM_011_Overall_Approach.qmd` used `#fig-fig2` twice (preliminary model and classification model). The classification model figure is now `#fig-classification`; no cross-references used `fig-fig2`.
- Next: Rich renders Analysis 3 locally and reviews.

### 2026-10-07 Analysis A2: combined PDF

- Request (Rich): the A2 driver should also produce one dated PDF that joins all A2 DOCX outputs.
- Decisions (Rich): code inline in `Analysis2_Driver.R` (not in a control file, because only the driver runs after all five DOCX files are final); LibreOffice for conversion; output `Reports/MS_Combined_[date].pdf`; order manuscript, tables and figures, Appendices 1 to 3; if LibreOffice is missing, warn with install instructions and skip; delete interim PDFs.
- Implementation: LibreOffice runs headless with a temporary profile, so it works while LibreOffice is open; `qpdf::pdf_combine()` joins the PDFs. On Unix, the call clears `LD_LIBRARY_PATH`, because R's value stopped LibreOffice from loading its libraries in a Linux test.
- Tested on dummy DOCX files: five pages in order; temporary folders removed. Not yet run on the real manuscript files.
- `tools/update_readme.py` lists the combined PDF as an A2 output. README and instructions list the output and the LibreOffice and `qpdf` dependencies.

### 2026-10-07 Analysis A2_2 created: summary slide deck for Analysis A2

- Request (Rich): new subproject in folder `Analysis_2_2` with a driver, a control file, and a Revealjs deck in the A8/A9 format; one slide per included QMD, numbered in steps of 5.
- Content requested: glossary of the three algorithms (HRS/HCAP, Core, PMM); figures `HCAP-algorithm.png`, `HRS-algorithm_2024-12-19.png`, `Slides2603_results_class_probabilities.png`, and excalidraw `PMM-103-PMM-no-latent`, `PMM-013-Approaches_to_constrained_regression`, `PMM-012-Preliminary_Model`; agreement of the Core algorithm and the PMM with the HRS/HCAP Algorithm.
- Files: `Analysis2_2_Driver.R`; `R/Analysis_2_2/A2_2_000_Control.qmd`; slides `A2_2_005` to `A2_2_050`. Output `Reports/Slides_A2summary_[date].html`.
- Decisions (Claude, for Rich to review): folder placed under `R/` per the repository layout rule; "watermark" read as the title-slide background `Figures/Gemini-Brown-Amphibian.png` used in A8 and A9; agreement values come from `R/MS_MAIN/MS_010_results_objects.R` so they match the A2 report; kappas shown to 2 decimals.
- README, instructions, and `tools/update_readme.py` updated with an A2_2 section.
- Next: Rich renders locally and reviews.

### 2026-10-07 PMM residual structure checked

- Question (Rich): does the final PMM let residual covariances differ by known class; are STDYX results available? A table of Cohen's d and q across classes was considered and dropped (Rich).
- Finding (`mplus_output/pmm_103/pmm_hcap_103.inp` and `.out`): only means, intercepts, and thresholds vary by class. Residual variances of the four continuous scores are free but class-invariant (Mplus default). Residual covariances sit in %OVERALL%: only vdwdimmz with vdwddelz is non-zero (fixed at 0.01853, STDYX r = 0.70); the other five are fixed at 0. Categorical indicators have no within-class residual associations. Link is LOGIT. STDYX is printed (OUTPUT: STANDARDIZED).
- Corrected the residual-covariance sentence added earlier today to `R/PMM_103_calibration_model.qmd`.

#### Open Questions / Deferred Issues

- **Possible bug: duplicate residual covariance statements in the PMM cognition model.** The generated `pmm_hcap_103.inp` (and `pmm_hcap_103b.inp`) lists the six residual covariances among vdlfl1z, vdwdimmz, vdwddelz, and vdexf7z twice. The first block fixes all six at robust-norms values (for example, vdwdimmz with vdlfl1z @ 0.00203). A second block then fixes five of them at 0 (all but vdwdimmz with vdwddelz). Mplus applies the later statement, and the output confirms the zeros. To do: confirm whether the zeros are intended. If not, fix the generator (`R/PMM_101_mplus_function.R` or `R/PMM_103_calibration_model.R`), refit Analysis A3, and rerun A3_2, A2, A2_2, and A9, which use the PMM parameters.


---
## 2026-10-17

### Appendix 1 (core algorithm) and A7 repairs

- Found: `A7_100` and the A7 report read `R_objects/A7_075_hrs16_hcap.rds`, dated 2026-04-21. Commit 414183a (2026-08-07) removed the line that wrote it. All core-algorithm agreement statistics in A7, A8, A2, and A2_2 came from the April run (raw PD102; counts 2,453/735/308) rather than the current code (imputed D102; 2,433/747/316). Fixed: `A7_075` writes the object again.
- Decision (Rich): cut points are set by prevalence matching to the weighted HRS/HCAP (Manly-Jones) class shares. New `R/A7_070-threshold_calibration.R` grid-searches severe and moderate T cut points (step 0.5), memory and IQCODE rules fixed; `A7_075` reads the selected cuts. Previous cuts (37, 48) are kept in the calibration object for comparison. The earlier cuts may have been set by eye and not re-examined after upstream changes.
- New `R/A7_060-algorithm_functions.R`: one copy of the classification, flow-count, weighted-share, kappa, and weighted-thousands functions, sourced by A7_070, A7_075, A7_100, and Appendix 1. Fixed an inconsistency: flow counts omitted D102 = -8, which the algorithm counts as worse.
- Decision (Rich): 2018 number series (not administered) left as is; stated as a limitation in Appendix 1.
- Decision (Rich): Appendix 1 compares with Langa-Weir only; algorithm figure is a schematic with no counts (drawn in R with the current cut points), counts in a table.
- Interview mode: not checked (Rich, 2026-10-17); stated as a limitation only.
- Immediate word recall is excluded from the core model for the same reason it is excluded from the HRS-HCAP memory domain: it adds little beyond delayed recall (Jones et al., 2024). The core battery has no visuospatial item because it was first built for telephone administration (Rich).
- Wording of result-dependent sentences in Appendix 1 to be reviewed after the rebuild.
- Summary objects for reports: `A7_050_cfa_summary.rds`, `A7_055_norm_summary.rds`, `A7_070_threshold_calibration.rds`, `A7_075_algorithm_summary.rds`.
- `A0_030` now keeps raw `PD102` (for the count of imputed self-rated memory values).
- The person-wave export is now `Data/Dx_for_LK-[date].csv`. `R/A7_080-compare_dx_exports.R` compares two exports (the June file Lindsay used is not in git; `*.CSV` is ignored).
- Appendix 1 rewritten as prose with inline R values: `R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd`, values from `R/MS_MAIN/MS_020_appendix1_objects.R`. Title per Rich: "Adaptation of the Manly-Jones algorithm to the HRS core survey data" (manuscript supplement contents still read "Adapation").
- Instruction added (Rich): every reported number is computed at render time with inline R; never read an `.rds` that no current script writes.
- Not run here (no R in the linked environment). Rebuild: `Rscript Analysis0_Driver.R`, `Rscript Analysis7_Driver.R`, `Rscript Analysis2_Driver.R`; then A8 and A2_2, which read A7_100.

### Appendix 1 text-table check and export README (later on 2026-10-17)

- After Rich's rebuild: selected cut points T < 38.5 and T < 46.5 (previously 37 and 48); weighted shares 69.6/21.2/9.1% against the Manly-Jones 69.4/21.4/9.2%; 3-class kappa 0.50 and dementia kappa 0.34 (in-sample). Text and tables agreed except the items fixed below.
- Fixes: EAP person-wave count now from the Mplus 2016-2022 scoring run (was the long-file row count); chunk labels renamed so Quarto does not prefix "Figure 1:"/"Table 1"; IQCODE cuts printed as 3.0 and 3.4; conditional wording for zero imputed D102 and zero 2018 number-series scores; kappa comparison tolerance 0.02; "8 scored items" reworded (battery has 9, 8 used); Table A5 headers name Manly-Jones and comparator classes.
- Export: `Data/Dx_for_LK-2026-10-17.csv` (hard-coded name, Rich). `Dx_for_LK-2026-06-11.csv` kept; its content was written 2026-10-07 with cuts 37/48, so it may differ from the file Lindsay used in June. New `R/A7_085-export_readme.R` (sourced by A7_075) writes `Data/Dx_for_LK-2026-10-17_README.md`: columns, changes, weighted shares and kappas, and differential classification against the previous export (all respondents and HRS-HCAP Wave 1 cohort).
- Python check before the README existed (all respondents): 3.6 to 4.3% of person-waves changed per wave; all changes Normal-MCI or MCI-Dementia, as the cut-point shift predicts.
- `tmp_review/` in the repository holds two extracted figure PNGs; delete it.

### Rich's tracked changes to Appendix 1 (edited render 0716)

- Incorporated Rich's tracked changes from `Reports/MS_Appendix_1_2026-10-17_0716_edited.docx` (Purpose, Data, Step 1) into `MS_Appendix-1-Core-Algorithm.qmd`, keeping inline R values. Typos fixed: "Manly et al (2022)" to "et al.", "the the", "estimatd", "fixed at the estimates from the HCAP Wave 1 estimates". Fit-index definitions added as a Quarto footnote; Asparouhov & Muthén (2018), Bentler (1990), Steiger (1990), Tucker & Lewis (1973) added to the reference list.
- New passage on WLSMV versus MLR loadings: number series difference attributed to missing-data handling (pairwise present, MCAR, versus all available responses, MAR); object naming difference not explained by missing data, not examined, possibly related to the skewed response distribution. Values from new `A7_050_cfa_summary.rds` fields `coverage_hcap` (item present among HRS-HCAP participants with any item) and `top_share` (weighted share in the highest category).
- Tables: `a1_flextable()` no longer calls `autofit()` (column widths were sized from the merged title and note rows, so Table A1 ran past 1-inch margins). Tables now use fixed relative widths that sum to 6.5 inches. "(2016/17)" changed to "(2016)" in Table A3 to match Rich's edit.

### Rich's untracked edits (render 0851)

- Compared `Reports/MS_Appendix_1_2026-10-17_0851_edited.docx` with the 12:37 render it was edited from and carried the differences into `MS_Appendix-1-Core-Algorithm.qmd`: new opening sentence; added "Table A1 provides loadings from the MLR and WLSMV models"; Step 2 rewritten (norming follows Manly et al., 2022; "skewed" claim removed; Blom linking equation; denominator named as the standard error of the estimate; residual-SD interpretation, unweighted T means, cross-sectional age gradient); "The second/third problem is..." openers removed. Inline R restored where Rich pasted literal `r ...` text. Step 4 still opens "The fourth problem is...".
- The proposed A7_055 fields for the factor-score ceiling are not needed (Rich dropped the sentence).

## 2026-10-18

### Number series (xNSSCORE) administration across waves: findings and open questions (paused)

- Decision (Rich): stop investigating; ask HRS staff for a full account of the number series design by wave and mode. The HRS concordance is incomplete (it omits 2016, where the variable exists).
- Confirmed from the raw Core files (Rich, Stata):
  - 2016: `PNSSCORE` is in `H16D_R` (all 20,912 respondents; valid range about 409-584). No score for 3,067: 949 system missing (close to the 941 proxy interviews), 945 coded 996, 915 coded 997, 258 coded 999. Codes 996/997/999 have no value labels; meaning unknown. A0_015 sets them to missing.
  - 2018: no number series variable in `H18D_R` (`lookfor SCORE` finds only QD170). Absent by design.
  - 2022: `SNSSCORE` missing for 8,088 of 15,856 (51%), all system missing (no 996/997/999 codes). By interview mode (`SD358`: 1 telephone, 2 face-to-face, blank = inapplicable/partial): telephone 675/5,391 missing (12.5%); face-to-face 6,776/8,402 (80.7%); blank 637/2,063 (31%, derived). Blank may include web interviews (inference).
  - 2020: not checked (A3_2 report: about 19% missing among scored person-waves).
- HRS documentation: web mode offered from 2018; in 2018 and 2020 a random 60% of web-eligible households were assigned to web (COGIMP9222 data description). A study of the 2018 web experiment reports higher web scores on some items, largest for serial 7s.
- Open questions for HRS: meaning of 996/997/999 in 2016; why number series was dropped in 2018; why it is missing for most 2022 face-to-face interviews (routing, subsample, or mode rule); whether it was asked in web mode; 2020 design.
- Not changed: A0 code, Appendix 1 number series text. Fixed (same day): A0 report label for xNSSCORE in `R/A0_009-select_data.qmd` now reads "Number series score" (was "Nurse/proxy cognitive summary score").

### Rich's tracked edits and AGENT tasks (render 0917 edited)

- Carried Rich's tracked edits from `Reports/MS_Appendix_1_2026-10-17_0917_edited.docx` into `MS_Appendix-1-Core-Algorithm.qmd` (Step 2 wording "First/Second/Third", T-score naming, deleted age-gradient sentences; Step 3 wording; memory-rule paragraph deleted; Cohen citation moved; limitations split into paragraphs, practice-effect and kappa-SE limitations deleted, language added; "Source:" notes removed from all tables).
- AGENT tasks done (decisions by Rich via questions):
  - Steps swapped as is: Step 3 = cut points by prevalence matching (Table A2, Figure A2); Step 4 = classification rules (Table A3). Overview step list reordered to match. "The fourth problem is..." opener removed.
  - Old Table A3 (node counts) removed; n per rule added to the decision-rules table (now Table A3). Tables use flextable::paginate() so Word keeps each table on one page (duplicate Table A2 title in the 0917 render).
  - "Previous" cut points removed from text and table; kappa-maximizing pair kept as evidence of the grid search.
  - Figure A2 now one panel: weighted density of T with shaded bands at the cut points, band shares, and a box with core versus target class shares.
  - Langa-Weir paragraph written from Crimmins et al. (2011, pp. i164, i166): 27-point score and cut points, 11-point proxy score, equipercentile matching to ADAMS, no demographic adjustment.
  - New Table A4: kappas and dementia capture (core vs Manly-Jones, Langa-Weir vs Manly-Jones, core vs Langa-Weir). Cross-classification is Table A5.
  - Shrinkage: Figure A3 and Table A7 compare EAP scores (age 65+, nonzero wave weight) in 2016+2020 versus 2018; 2022 left out. Rich wrote "Leave out 2022, combine 2016+2022"; read as 2016+2020 (flagged to Rich). Evidence: mean posterior SD and SD of EAP by group, and Var(EAP) + mean posterior variance (about 1 under the model).
  - New Table A8: tests (8 model items, immediate recall, any item) by wave, unweighted n and weighted % present, age 65+ with nonzero wave weight, proxies in the denominator.
- New objects in `A7_050_cfa_summary.rds`: `item_coverage_65`, `eap_shrink`, `eap_shrink_summary`. MS_020 reads `A7_075_hrs16_hcap.rds` for the T density.

### Functional limitations: endnote to Appendix 1

- Question (Rich): does the core algorithm use ADL/IADL? No. A7 uses the 8 cognitive items, demographics, D102, and the IQCODE (proxy route). ADL/IADL items enter only as predictors in the D102 imputation (A0_019); no Wave 1 participant with a T score needed imputation.
- Evidence found in legacy A1 (Reports/HRS_cognition_2026-10-07.html; A1_029, A1_030, A1_035): prevalence matching of function within cognitive strata could not reach HCAP rates (2+ domains: 65% target, 53.6% with >=1 ADL/IADL limitation; 1 domain: 70% target, 35.8%); kappa with function 0.52 (v1 and v2, unweighted, N = 2,993) versus 0.50 without (A7). No such work in A7.
- Decision (Rich): document as an endnote set off from the main appendix ("what we did" stays uninterrupted). New side analysis `R/A7_090-function_check.R` (sourced last in A7 control) writes `A7_090_function_check.rds`: Part 1, weighted share with 0/1/2+ limitations by Manly-Jones class and by core T band; Part 2, three rule variants (memory, function analog, function or memory), each with its own prevalence-matched cut points, with kappas and dementia capture. Appendix: pointer sentence in Step 4; "Endnote. Why the core algorithm does not use functional limitations" after Limitations with Tables A9 and A10. The endnote's last sentence refers to the A1 work without numbers.

## 2026-10-18 (afternoon): 0953 edits, functional-limitation decision, split-half check

- Decision (Rich): the core algorithm's definitions of normal, MCI, and dementia do not change. Cut points stay matched to the Manly-Jones final class shares (not to 0/1/2+ impaired-domain counts). The function rules are not adopted: the gain in kappa was not judged large enough to justify letting limitations with probable physical causes enter the dementia classification. No new export for Lindsay is needed.
- Carried the tracked edits from Reports/MS_Appendix_1_2026-10-18_0953_edited.docx into MS_Appendix-1-Core-Algorithm.qmd: P1 edits; the step list and data list as separate paragraphs; "and/or age"; Step 1 deletions and "maximum likelihood"; "HCAP 2016 robust normative sample"; Approach/Results headings for Steps 1-3; Figure A1 caption.
- AGENT items: Figure A1 is now Figures/HRS-Core-Algorithm-2026-10-18.png (a1_schematic() is no longer called). Step 2 methods use SEE and words; R^2, SD, and SEE values moved to "Results: Step 2". Cut points named tau1/tau2 in Step 3 and the Figure A1 caption. Figure A2 has no class-share box (class shares are in Table A2), and its band labels use ggrepel (staggered geom_label fallback if ggrepel is not installed). The Manly-Jones rules paragraph moved from Step 4 to the start of Step 3.
- A7_090: limitations are now counted on the 9 ADL/IADL items asked in every wave 2016-2022 (G040, map use, omitted because it is not in 2022); all-missing is now a Missing category rather than 0. Added Part 3, a repeated split-half check (200 splits within Manly-Jones class, seed 20261018): cut points selected in one half, agreement in the other; paired differences from the memory rule. Fast calibration is checked against Part 2 on the full sample (warning if it differs). New list elements: limit_items, n_limit_items, n_changed_by_g040, n_split, split_half, split_summary, split_diff.
- MS_020: Table A11 (a1_table_split) and helpers a1_sdec, a1_sh, a1_shd, a1_shd_text; Table A9 and A10 notes updated.
- Endnote rewritten with 3 reasons (floor; small and uncertain gain, now with out-of-sample results; ADL/IADL items do not separate physical from cognitive causes). Wording that interprets the kappa differences is conditional on the computed values.
- To run: A7 (A7_090 needs rPG014...rPG059 in A7_075_hrs16_hcap.rds; they come through A0_030's everything()), then the appendix QMD.

## 2026-10-18 (evening): 1532 tracked edits

- Carried tracked changes from Reports/MS_Appendix_1_2026-10-18_1532_edited.docx into MS_Appendix-1-Core-Algorithm.qmd (Steps 1-4, Agreement, Application) and the Table A1 note in MS_020 (weight variable name, strata and cluster counts removed). New footnote on the unweighted normative-sample T scaling (Manly et al., 2022 convention). Page break before References. Farron et al. (2026) removed from the reference list because its only citation was deleted.
- Repairs to incomplete edits: "Because no point..." restored as a full sentence and the next paragraph opens "We therefore empirically identified..."; "on the same footing" (both words deleted) became "comparable to".
- Shrinkage paragraph ends with the SEE version agreed in chat; the SD difference is inline (1 - SD2018/SD2016+2020).
- Step 3: "no informant report for respondents who participated in the cognitive assessment" (Rich's wording). Endnote second reason reframed: the out-of-sample gain is small but consistent, so the decision rests on the third reason (physical causes); the wording stays conditional on the split-half 2.5th percentile. Removed the incorrect claim that some rules have "more room to fit" (all rules have the same 2 cut points).

## 2026-10-17 and 2026-10-18: items not logged above (added 2026-10-18, 21:15)

### 2026-10-17

- `bibliography.bib` created at the repository root (18 entries) for Appendix 1; the Steiger (1990) entry is flagged "DOI unverified" (doi.org lookups were rate-limited and blocked by robots during the session). Appendix 1 still lists references by hand.
- Citation corrected (Rich): Jones et al. is 2024, *Journal of the International Neuropsychological Society*, 30(1), 47-55 (not 2023). Ofstedal et al. (2005) cited with doi 10.7302/24800.
- Fit-index footnote (RMSEA, CFI, TLI, SRMR) written at Rich's request, with conventional guides and citations; the "skewed" description of the EAP score was removed after Rich questioned it (no basis in the data). The T-score denominator is the standard error of the estimate (SEE), not the SEM.

### 2026-10-18

- Rich's 1532 edit added the Step 3 point that, with 5 domains, there are 10 ways to have exactly 2 impaired, each implying a different cut on a single score (5 choose 2 = 10; 26 patterns have 2 or more impaired).
- Clarified (no code change): prevalence matching (A7_070) uses HCAP16WGTR for both the Manly-Jones target shares and the core shares, and for the tie-breaking kappa; strata and clusters do not enter (point estimates only). Each grid point runs the full core algorithm (`classify_core()`): imputed self-rated memory (`PD102_imp`) and the IQCODE thresholds are applied, with only the 2 T cut points varying. IQCODE-classified participants enter the shares but do not move with the cut points; participants with neither a T score nor an IQCODE are unclassified (`n_unclassified` in the grid; not checked).
- Step 3 sentence on which share depends on which cut point revised (carried in the 1532 edits): the dementia share depends only on tau1 among the cut points, and every share also depends on the fixed memory and IQCODE rules.
- Figure A2: confirmed the density and band percentages are weighted (`stats::density()` weights). Band percentages differ from Table A2 class shares because bands are not classes (memory rule moves participants between classes within a band) and because the figure excludes the 213 IQCODE-classified participants. Open (Rich to decide): clarify the caption, show class splits within bands, or drop the band percentages.
- Shrinkage paragraph: parenthetical definitions added for mean posterior SD (within-person uncertainty) and SD of EAP scores (between-person spread). Discussed: with several thousand respondents per group the 0.04 SD difference is not sampling noise but is small; the comparison cannot separate the missing item from differences in who was interviewed. Derived variances (0.33/0.24, 0.72/0.79) are not reported because they must come from unrounded values.
- Checked Rich's question about stale numbers: the 19:30 render matched the A7 objects saved at 19:07; no cached values.
- Rendered split-half results (21:09 render): 3-class kappa gain over the memory rule +0.03 (2.5th to 97.5th percentile 0.00 to +0.06), function rule higher in 98% of splits; function or memory +0.02 (-0.00 to +0.05), 93%. Dementia kappa gain +0.06 (0.00 to +0.11; 98%) and +0.08 (+0.04 to +0.13; 100%). Out-of-sample means: memory 0.50, function 0.53, function or memory 0.52 (3-class).
- `a1_sdec()` in MS_020 rounds before adding the sign, so values that round to zero print "0.00" (was "-0.00"). Endnote wording now "the gain held in nearly every split" (Rich approved); it still switches if the 2.5th percentile of the function-rule 3-class gain falls below 0.
- For Rich to verify (not data-generated): the hip arthritis example in the endnote's third reason; the closing claim that the earlier A1 analysis found too few participants with limitations to reach the Manly-Jones class shares (based on 53.6% vs 65% and 35.8% vs 70%).
- `R/MS_MAIN/MS_020_appendix1_objects.R` still defines `a1_schematic()`, which is no longer called (Figure A1 is Rich's PNG); remove when convenient.
- Steiger (1990) DOI 10.1207/s15327906mbr2502_4 confirmed by Rich (2026-10-18); "DOI unverified" note removed from bibliography.bib.
- Closed (Rich, 2026-10-18 21:19): Figure A2 band percentages (revised by Rich; accepted); hip arthritis example (accepted); `a1_schematic()` removed from MS_020. The endnote's earlier-analysis sentence traces to `R/A1_029-finding_algorithm_cutpoints.qmd`, "Finding functional impairment thresholds" (rendered in Reports/HRS_cognition_2026-10-07.html, Tables 67-68): with 1 or more ADL/IADL limitations, 53.6% of the core 2+ domain group vs 65% with functional impairment in HRS-HCAP, and 35.8% of the 1-domain group vs 70% (unweighted, N = 2,993).
- Endnote earlier-analysis sentence revised to state the within-level floor (no hard-coded legacy numbers); item closed.

## 2026-10-19: Analysis A10, data sharing runs (Kobayashi classification file)

- Question (Rich): what core algorithm file did Lindsay Kobayashi receive, did it use ADL/IADL, and how does it differ from the file we would send today, and why.
- Provenance (email thread "HRS Algorithmic Dx"): DT sent `Dx_for_LK-2026-06-11.csv` to RNJ on 2026-06-11 (D102 not imputed; person-waves missing D102 had no class); RNJ forwarded it to LK the same day and asked for D102 imputation. DT sent `Dx_for_LK-2026-07-10.csv` to LK on 2026-07-10 with D102 imputed. No earlier classification file was sent to LK. Neither file is in git (`*.csv` ignored). The export code first appears in DT's commits of 2026-08-07, so both files came from code not yet committed.
- Decision (Rich): treat the July 10 file as the file LK used. Rich saved it to `Data/Dx_for_LK-2026-07-10.csv`. Data must not be downloaded to the cloud; the session read only aggregate checks run on Rich's machine.
- Aggregate checks (Python, on Rich's machine): the July file and `Dx_for_LK-2026-10-17.csv` have the same 66,911 person-waves; T scores agree to 5e-13; D102 and IQCODE identical. The July file's classes equal the 2026-10-07 rerun (`Dx_for_LK-2026-06-11.csv` on disk) for every person-wave. The written rule rebuilds the July file with cuts 37/48 and the current file with 38.5/46.5 with 0 mismatches. So the only difference is the 2 cut points; 2,624 person-waves change (1,493 Normal to MCI, 653 MCI to Normal, 478 MCI to Dementia).
- ADL/IADL: in no version of the rule; they enter only as predictors in the D102 imputation (A0_019, mice, m = 1, seed 2, default predictive mean matching), identical in both files. Predictive mean matching can impute don't know/refused/not ascertained codes, which count as worsening; the report counts these for 2016.
- Decisions (Rich): name "Data sharing runs" (A10); weighted prevalence by wave with wave respondent weights; no incidence analysis (left to LK); no sensitivity analyses of alternative rules. Focus: what was given, what would be given today, and the rationale for the difference.
- New: `Analysis10_Driver.R`; `R/A10/A10_000-Main_control.qmd`, `A10_001-libraries.R`, `A10_005-share_log.R` (share log), `A10_010-compare_shared_current.R` (writes `R_objects/A10_010_comparison.rds`). README, instructions, and `tools/update_readme.py` updated. Not rendered here (no R in the linked environment). Rebuild: `Rscript Analysis10_Driver.R` after A0 and A7.

### Origin of the July cut points (37, 48): pre-specified test (2026-10-19, before results)

- Rich, after reading the A10 report: the change from 37/48 to 38.5/46.5 barely moves the class shares and slightly lowers the kappas, so there is no strong case to change LK's data; the gap is that 37/48 has no documented rationale.
- Hypothesis (Rich): 37/48 came from the A1_029 rule: cut the Core T at the quantiles matching the shares of HRS-HCAP Wave 1 participants with 2+ and 1+ impaired cognitive domains (HCAP algorithm, `Stata/w073_hcapdx.dta`, n21/n22). On the legacy A1 T, that rule gave 36.0 and 43.3.
- Pre-specified test, fixed in the header of new `R/A7_072-domain_quantile_check.R` before any result: sample HCAP Wave 1, imputed_flag == 1, T present; R1 unweighted quantiles at .15/.35 (as hard-coded in A1_029); R2 unweighted quantiles at the observed shares; round to nearest integer; success if R1 or R2 gives exactly 37 and 48. Weighted quantiles and the full HCAP sample reported for information only.
- Decision (Rich): if the test passes and is reproducible, adopt the rule and remove the prevalence-matching procedure (A7_070 grid search) added on 2026-10-17/18; LK's July file stands. If it fails, report that plainly.
- Run by Rich: `Rscript R/A7_072-domain_quantile_check.R`.
- Result (Rich ran A7_072, 2026-10-19 11:44): FAIL. Pre-specified sample n = 2,922 (A1_029 reported 2,993); shares 14.4% (2+ domains) and 34.1% (1+). R1 gives T 36.0 and 43.5; R2 gives 35.6 and 43.2; weighted versions 36.3 to 36.7 and 43.6 to 43.8. The severe cut lands near 37; the moderate cut lands 4 to 5 T points below 48. The domain-matching rule does not reproduce the July cut points. The current-T quantiles (36.0, 43.5) are close to the legacy A1 cuts (36.0, 43.3), which were used with a function rule, not the memory rule.
- Not adopted: the prevalence-matching code (A7_070) stays in place pending Rich's decision. A7_072 kept as the record of the test.
- Follow-up test (Rich): same pre-specified rule on the T score as it existed at commit 924beb6 (2026-04-17), where 37/48 first appear. Correction: the April code already used the Blom transform (inside A7_055); the 2026-08-07 commit moved it to A7_052 and changed the norming, and April A7_055 hard-coded the T denominator (0.9987769, R^2 .366). Setup: `_snapshots/924beb6/` is a `git archive` of that commit (excluded via .git/info/exclude, not .gitignore) plus `run_april_T_check.R`, a `.here` file, and a copy of `Stata/w073_hcapdx.dta`; it reruns April A0 and A7 (2016 model only) and writes only inside the snapshot. Success criterion unchanged (R1 or R2 rounds to 37 and 48). Run by Rich: `cd _snapshots/924beb6 && Rscript run_april_T_check.R`.
- Result (Rich ran the April runner, 2026-10-19 12:53; Rich edited the runner to locate `stata/20240228-040.dta`, which is not in git): FAIL. April T, pre-specified sample n = 2,922: R1 36.3/43.6, R2 36.0/43.3; weighted 37.0/44.0 and 36.7/43.8 (weighted R1 severe = 37.0 is post hoc and not part of the criterion). April T versus current T: r = 1.00, mean difference 0.07, centiles 36.33/43.63 versus 35.96/43.46. Conclusion: the domain-matching rule does not produce 37/48 on any version of the T score; the moderate cut 48 is about 4.5 T points above what it gives.
- Evidence found: `Reports/A7_000-Main_control_2026-04-17.html` (rendered from 924beb6, the commit that introduced 37/48) shows a weighted (HCAP16WGTR0) table of the algorithm's HCAP Wave 1 class shares beside Manly-Jones vs1hcapdxeap: 71% / 21% / 8.0% versus 69% / 21% / 9.2%, weighted kappa 0.505 (raw PD102, no imputation). Consistent with cut points set by inspection to approximate the weighted Manly-Jones final class shares, the criterion A7_070 formalizes. Not proof of how they were chosen; DT can confirm.
- Fixed in A10 report: the sentence claiming the July T scores differed from those the cuts were chosen on (they did not; D102 imputation did change). Rationale paragraph now cites the April report.

### Decision: keep 37/48; prevalence-matching grid kept as a check (2026-10-19, afternoon)

- Decision (Rich): LK's July file stands; the algorithm keeps T < 37 and T < 48. Doug is not to be asked. The 2026-10-17 grid search stays as a calibration check, not as the source of the cut points.
- A7_070: object fields now `cuts` (algorithm, 37/48, used by A7_075), `algorithm` (shares and kappas at 37/48), `calibrated_cuts` and `calibrated` (grid optimum, check only), `grid_position` (rank of 37/48 by loss, largest share difference for 37/48 and for the optimum), plus `target_shares`, `max_kappa`, `grid`. Removed `chosen`, `previous`, `previous_cuts`. Header documents the April origin and the April report.
- A7_075: export renamed `Data/Dx_for_LK-rebuild.csv`, written only so A10 can confirm the rebuild; not a share. A7_085 (README for the withdrawn export) deleted (in git history at 8fe5469); `Data/Dx_for_LK-2026-10-17.csv` and its README kept on disk as a record, marked withdrawn in the A10 share log.
- A7_070.qmd and A7_075.qmd text revised (cut points and calibration check). A7_090: its self-check now compares the memory variant with `calibrated_cuts`; otherwise unchanged pending Rich's decision.
- A10 rewritten: confirms the current code reproduces the July file (identity and reconstruction checks), documents the steps and the rationale (April report; A7_072 domain-quantile test failed), and reports the grid optimum as the alternative not adopted (changes, mechanism, weighted prevalence).
- Appendix 1: carried Rich's tracked changes from `Reports/MS_Appendix_1_2026-10-18_edited.docx` (7 table notes: "HCAP16WGTR" to "HCAP Wave 1 (2016) sampling weight"; new final limitation paragraph). In the new paragraph: "measures measures" corrected; "cut-points" changed to "cut points" (the term used elsewhere); `[@Streiner2002][@Altman2006]` written as "(Streiner, 2002; Altman & Royston, 2006)" because Appendix 1 lists references by hand; both added to the reference list (DOIs verified: 10.1177/070674370204700307; 10.1136/bmj.332.7549.1080). Step 3 approach and results rewritten for 37/48 with the grid as a check; Table A2 now shows cut points used, grid closest-share pair, grid highest-kappa pair, and target (MS_020: `a1_used`, `a1_opt`, `a1_gpos`; `a1_chosen` removed).
- README and instructions updated (A7 purpose and derived files, A7 side scripts, A10 purpose).
- Not rendered here. Rebuild order: `Rscript Analysis7_Driver.R`, `Rscript Analysis2_Driver.R`, `Rscript Analysis10_Driver.R` (then A8 and A2_2).
- Decision (Rich, 2026-10-19): A7_090 holds all 3 rules (memory, function, function or memory) at the algorithm cut points 37/48, rather than giving each its own prevalence-matched cut points. A7_090 Part 2 now evaluates each rule at fixed cuts (grid search removed); the memory variant is checked against dx_v1. Part 3 keeps 200 random halves (within Manly-Jones class, seed 20261018) but computes agreement at the fixed cuts in each half; with no selection it describes sampling variation of the paired differences, not out-of-sample performance. `fast_calibrate()` removed. New list element `cuts`.
- Appendix 1 endnote and Tables A10/A11 notes rewritten to match: rules at T < 37 and T < 48 (inline), rules differ in class shares as well as agreement, "splits" now "halves", "out-of-sample" wording removed. Direction-dependent wording moved to MS_020 (`a1_fn_effect_text`, `a1_fn_halves_text`, `a1_fn_conclusion_text`) because with fixed cuts the function rules may lower agreement. Rich to review the rendered endnote: its second reason may now read as "did not raise agreement".

## 2026-10-20: Appendix 1 reorganized (rules before cut points); 2026-10-19 edited render

- Rich's AGENT notes in `Reports/MS_Appendix_1_2026-10-19_edited.docx` asked whether the cut points had been changed to match the 2+ and 1+ impaired-domain shares. Checked: the 10-19 render (13:20 EDT) followed the last QMD/MS_020 edits (13:08) and the A7 rebuild (13:18), so the report was current. The log records the domain-share rule as a hypothesis that failed its pre-specified test (2026-10-19) and the 2026-10-18 decision to match final Manly-Jones class shares, not domain counts. No code change.
- Rich's second note was valid: Step 3 reported class shares that depend on the D102 rule, which was defined only in Step 4. Decision (Rich): reorganize the presentation. We do not know where the cuts belong; we want to match the Manly-Jones margins; so the full rule set (self-rated memory node, why ADL/IADL are not used) is defined first, then the cut points are evaluated against the final class shares.
- `MS_Appendix-1-Core-Algorithm.qmd`: overview step list reordered (Step 3 = rules, Step 4 = cut points); Figure A1 caption "(Step 4)"; new "Step 3. Classification rules" (Manly-Jones rules, no informant report, core counterparts, D102 imputation, and a paragraph on why ADL/IADL are not used, citing only cut-free evidence: weighted share of Manly-Jones dementia cases with no limitation, `a1_fn_none_mj("Dementia")`, and the physical-cause argument); "Approach/Results: Step 4. Cut points by prevalence matching" (domain paragraph, "with every other element of the rules fixed as described in Step 3", grid check, results); Table A3 moved after Figure A2 because it shows the numeric cuts and counts; Langa-Weir sentence "Step 4". Endnote now says reasons 1 and 3 do not depend on the cut points and reason 2 is a check made after the cuts were fixed. No change to MS_020 or A7.
- Carried Rich's tracked changes: Purpose first sentence deleted; Step 1 domain paragraph deleted; "given the available items" deleted; HCAP weights sentence; object-naming sentence ("Instead, the difference may be related"); Step 2 R^2 parenthetical deleted and "and therefore the SEE"; "has no domains, and it" deleted; "T distribution"; colon before the cut-point names. Both AGENT notes removed (resolved by the reorganization).
- Backup of the pre-edit QMD on the linked machine's session home only. Not rendered here. Rebuild: `Rscript Analysis2_Driver.R`.

## 2026-10-20 (evening): grid search removed from Appendix 1; steps back in original order

- Rich asked whether a post-hoc justification for 37/48 could be found (with or without weights). Discussed: any criterion found after the fact is weak unless pre-specified; unweighted matching would conflict with the population-share rationale. Not pursued.
- Decision (Rich): remove the grid search from Appendix 1. The appendix states that we chose T < 37 and T < 48 and stays vague on the rationale, because the origin is not known. Rich will talk to Doug (this replaces the 2026-10-19 "Doug is not to be asked").
- Order (Rich): Step 1 factor score, Step 2 norms, Step 3 cut points, Step 4 rules (non-use of ADL/IADL first, then self-rated memory D102 and IQCODE).
- `MS_Appendix-1-Core-Algorithm.qmd`: "calibrated to" changed to "compared with" (Purpose); "calibration target" to "reference classification" (Data); overview steps 3 and 4 restated; parameters sentence now separates estimated item parameters and norms from the fixed cut points; Figure A1 caption "(Step 3)". Step 3 (Approach/Results): Manly-Jones rules, the 10-ways paragraph, the cut points stated with inline values and what each band stands in for, Figure A2. Step 4: no informant report, ADL/IADL paragraph, D102 and IQCODE rules, Table A2 (rules), then a sentence and Table A3 with the core and Manly-Jones weighted class shares (description only). Removed: prevalence-matching method, grid, closest-share and kappa-maximizing pairs, the Langa-Weir "same prevalence-matching logic" clause. Limitation 1 now: the cut points may have been chosen with reference to the HRS-HCAP Wave 1 sample, so agreement in Table A4 may be optimistic.
- `MS_020_appendix1_objects.R`: `a1_table_calibration` replaced by `a1_table_shares` (Table A3: core vs Manly-Jones weighted shares, from A7_070 `algorithm` and `target_shares`); `a1_opt`, `a1_maxk`, `a1_gpos` removed; rules table renamed Table A2.
- Not changed: A7_070 still runs the grid (A10 reports the grid optimum as the alternative not adopted). `MS_Main_Control.qmd` line 51 still says the core algorithm is "calibrated to HRS/HCAP" (flagged to Rich).
- Not rendered here. Rebuild: `Rscript Analysis2_Driver.R`.

### Origin of 37/48: normative-sample domain-share test (2026-10-20, before results)

- Hypothesis (Rich): the shares of normative-sample participants with 2+ and 1+ impaired domains were used as percentile targets on the Core T score, with percentiles taken in the normative sample (weighted or unweighted), possibly in the A1_029 sample (n = 2,993).
- Pre-specified test in the header of new `R/A7_073-normative_domain_quantile_check.R`: sample inHCAP == 1, normexcld == 0, imputed_flag == 1, T and domain count present; N1 unweighted shares and quantiles; N2 weighted shares and quantiles (HCAP16WGTR); round to integer; success if N1 or N2 gives exactly 37 and 48. Mixed weighting and the unrestricted normative sample reported for information.
- Expectation written before results: with domain impairment at -1.5 SD normed in this sample (about 6.7% per domain), P(1+) is at most about 33%, so the moderate cut should be at or below about 46; 48 needs about 42% and 37 needs about 9.7% (T is about N(50, 10) in this sample). Expected FAIL.
- Run by Rich: `Rscript R/A7_073-normative_domain_quantile_check.R`.
- Result (Rich ran A7_073, 2026-10-20 18:58): FAIL, as expected. Pre-specified sample n = 1,610 (all normative with T and domain count: 1,776). Shares: 2+ domains 5.7% unweighted, 4.6% weighted; 1+ domains 21.8% and 20.7%. N1 gives T 34.8/42.7 (rounded 35/43); N2 gives 33.5/42.5 (34/43); information rules 33 to 35 and 42 to 43. In this sample 8.5% fall below 37 and 41.6% below 48 (unweighted; weighted 8.5% and 40.2%). No domain-share rule tested (A7_072, A7_073) gives a moderate cut above 43.5; 48 sits 4.5 to 5.5 T points higher. The severe cut from these rules falls between 34 and 36.

### Appendix 1: where the cut points sit relative to domain shares (2026-10-20, 19:05)

- Decision (Rich): Step 3 describes where 37 and 48 sit relative to the impaired-domain shares, without claiming how they were chosen: the severe cut is close to the T score matching the share with 2+ impaired domains; the moderate cut is above the T score matching the 1+ share; a higher moderate cut is needed because the memory rule sends participants below tau2 without memory worsening to Normal, lowering the MCI share. Forward reference to Step 4 accepted.
- `A7_070`: header no longer says the cuts were set to match class shares (origin undocumented); new list element `domain_quantiles` (all HRS-HCAP Wave 1 with T, positive HCAP16WGTR, and a w073 domain count; weighted shares and weighted T quantiles for 2+ and 1+; unweighted values kept for the record). A7 now reads `Stata/w073_hcapdx.dta`.
- `MS_020`: `a1_dq`, `a1_dq_sev_text`, `a1_dq_mod_text` ("lies close to" when within 1.5 T points, else above/below). QMD: new paragraph after the cut-point sentence in Step 3.
- Rebuild: `Rscript Analysis7_Driver.R`, then `Rscript Analysis2_Driver.R` (A10, A8, A2_2 read A7 objects but are unaffected by this element).

### Summary of 2026-10-20 for Rich (where we left off, 20:00)

What happened today, in order:
1. Rich's AGENT notes on the 10-19 Appendix 1 render asked why the cut points were not matched to the 2+ and 1+ impaired-domain shares. The render was current. The domain-share rule had been a hypothesis tested on 10-19 (A7_072) and failed; on 10-18 Rich had decided to match final Manly-Jones class shares instead.
2. Rich's tracked changes from `MS_Appendix_1_2026-10-19_edited.docx` were carried into the QMD.
3. The appendix was first reorganized with rules before cut points (to remove a forward reference to D102), then Rich decided to drop the grid search from the appendix and state only that we chose T < 37 and T < 48. Order restored: factor score, norms, cut points, rules (ADL/IADL not used, then self-rated memory and IQCODE).
4. A post-hoc justification was discussed. Rich proposed a normative-sample domain-share rule; pre-specified as A7_073; FAIL (cuts 34-35 and 42-43). Across A7_072 and A7_073, domain rules put the severe cut at 34 to 37 and the moderate cut at 42 to 43.5; 48 is 4.5 to 5.5 points higher. Likely reason (not proven): the memory gate sends people below tau2 without memory worsening to Normal, so a higher tau2 is needed to reach the Manly-Jones MCI share.
5. Rich approved a descriptive Step 3 paragraph saying exactly that (where the cuts sit, not how they were chosen). It is in the QMD and needs A7 rerun (new A7_070 element `domain_quantiles`).

State of the files at 20:00:
- `Reports/MS_Appendix_1_2026-10-20.docx` (rendered 19:00) is the version Rich is sending to Lindsay Kobayashi today: "we chose 37 and 48", no grid, no domain-share paragraph.
- The QMD and MS_020 now contain the domain-share paragraph. Running A2 alone will fail until A7 is rerun, because MS_020 reads `A7_070$domain_quantiles`. Rebuild for the next version: `Rscript Analysis7_Driver.R`, then `Rscript Analysis2_Driver.R`.
- Grid-search code archived in `R/Archive_grid_search_2026-10-20/` (README there). The live A7_070 still runs the grid, and the A10 report still shows the grid optimum as the alternative not adopted.

Open items:
- Rich to talk to Doug about how 37/48 were chosen (replaces the 10-19 "Doug is not to be asked"). If Doug confirms the cuts were set to approximate class shares, Step 3 can say so, and the grid can come back from the archive.
- `MS_Main_Control.qmd` line 51 still says the core algorithm is "calibrated to HRS/HCAP".
- Decide whether the A10 share log should record the appendix sent to Lindsay today (it now logs only classification files).

## 2026-10-21

- Bug (found by Rich in the 2026-10-21 A2 render): the Step 3 domain-share sentence printed "0%; T ≈ -6; n = 2,786". Cause: A7_070 `domain_quantiles` filtered out rows with missing n22, which removes the 2+ group (n22 is missing when n21 == 1). Fixed to the A7_072/A1_029 coding (n21 == 1 is 2+; n21 == 0 with n22 1 or 0 is 1 or 0) and added a guard that stops A7 if the shares or n are implausible (2+ outside 5-30%, 1+ outside 15-60%, n < 2,900). The appendix sent to Lindsay (2026-10-20 19:00 render) did not contain this paragraph.
- Rebuild: `Rscript Analysis7_Driver.R`, then `Rscript Analysis2_Driver.R`.
- Carried Rich's untracked edits from `Reports/MS_Appendix_1_2026-10-21_edited.docx` (found by paragraph compare with the 10-21 render): "Introduction" heading; Purpose rewritten (self-reported memory as the one Manly-Jones indicator available in core; Langa et al., 2020, no longer cited, so removed from the reference list); "cognitive performance score" in steps 1-2; new paragraph on the HRS-HCAP 2016 sample, normative group, and calibration; IQCODE "(2-year lookback)"; Data paragraph now says the agreement statistics are optimistic because Manly-Jones calibrates the core algorithm, and Langa-Weir was derived independent of HCAP; Step 1 indicators described as binary, ordinal, and continuous, EAP defined, immediate-recall sentence moved to Step 1 results; Step 2 robust normative sample definition; Step 3 paragraphs merged, "by construction" and "No rule fixes..." removed, "MCI classification rate"; Step 4 ADL/IADL reason now "level and kind of impairment fundamentally different" from IQCODE and Blessed Part I, the sentence naming self-rated memory as the second criterion deleted (per Rich's edit); "shares" to "prevalence" in Step 4, Table A3 title, endnote, and Table A10 note; CIND "equivalent to MCI for this comparison"; "Missing Number Series" subheading; factor-variance sentence removed; endnote opener removed, second reason "never enough to justify the apparent difference in measured construct", third reason rewritten (self-report vs informant, change and lookback).
- Rich's `{@Key}` citations written as hand citations (Appendix 1 still had a hand-written reference list; superseded later the same morning by the switch to `[@Key]` and bibliography.bib, below). Fixes: "--" dashes to commas; "that samples'" to "that sample's"; "comparisson"; "Manly et al (2022)" to "et al."; "the Manly et al (2022) is used" to "the Manly et al. (2022) algorithm is used"; "3496" now inline; "is fundamentally" to "are"; closing parenthesis in the third reason; the duplicate "(n = 1,787)" in the normative-sample definition dropped (the n stays inline in the preceding sentence).
- Table A4 "empty": Tables A4 and A5 were adjacent with no paragraph between, so Word merged them and repeated the A4 header rows at the top of the next page with no body. Added an empty openxml paragraph between A4/A5, A9/A10, A10/A11.
- Figures: no fig-dpi was set (Quarto docx default). Added `fig-dpi: 300` to Appendices 1-3 and the tables/figures control. `a1_cut()` now trims padding (table notes printed "T <   37").
- Rebuild: `Rscript Analysis7_Driver.R` (domain_quantiles fix), then `Rscript Analysis2_Driver.R`.
- Step 4: memory rule restored (Rich): "Self-rated memory worsening, an item the Manly-Jones algorithm already includes, is the second criterion at both levels and stands in for the informant report."
- Citations: Appendix 1 now uses `[@Key]` with `bibliography: ../../bibliography.bib` and `csl: ../../templates/apa.csl` (APA 7, downloaded from citation-style-language/styles). Hand reference list replaced by `::: {#refs}`. Mixed parentheticals use an escaped semicolon (`[IQCODE\; @Jorm1994]`), which renders "(IQCODE; Jorm, 1994)". Checked with pandoc citeproc: all 17 keys resolve. Fixed bib entries: Harrell2015 author (now "Harrell, F. E., Jr."), vanBuuren2011 (braced "{van Buuren}" so it is not "Buuren, S. van"; "{mice}" lower case). The Table A4 note cites Cohen (1968) by hand (flextable text is not processed); Cohen is also cited in the text. `instructions.md` Dependencies updated.
- Open item (Rich, 2026-10-21): the Data section now says the Manly-Jones algorithm "is used to calibrate the core algorithm", while Limitation 1 says the cut points "may have been chosen with reference to" the Wave 1 sample. Rich agrees they disagree; leave both as is for now and reconcile later (likely after talking to Doug).
- Closed (2026-10-21): Manly2022 author list completed from Rich's EndNote export (8 authors: Manly, Jones, Langa, Ryan, Levine, McCammon, Heeringa, Weir).

### Summary of 2026-10-21 for Rich (where we left off, 09:00)

What happened today, in order:
1. Domain-share bug: the 10-21 render printed "0%; T ≈ -6" in Step 3 because A7_070 dropped the 2+ domain group. Fixed in A7_070, with a guard that stops A7 on implausible shares or n. Needs an A7 rerun.
2. Questions answered, no file changes:
   - The core CFA has 3 continuous indicators (animal naming `vdlfl1z`, delayed recall `vdwddelz`, number series `vdexf7z`) and 5 categorical (`vdori`, `vdlfl2`, `vdlfl3`, `vdsevens`, `vdcount`), per `mplus_output/A7/model_1a/model1a.inp`.
   - One-sentence definition of the robust normative sample drafted from Manly et al. (2022, eMethods Phase I, eTable 5); Rich put it in Step 2.
   - Meehl & Yonce (1996): Rich's read that the normative vs excluded T difference (50 (10) vs 43 (12), about 0.6 to 0.7 SD) shows cognition alone separates the classes poorly. Discussed: the direction is supported (a 0.63 SD gap gives about 62% correct classification at equal base rates; Meehl & Yonce give 1.33 SD for 75%), but the excluded group is not the impaired taxon (it mixes stroke, Parkinson's, decline, death, and loss to follow-up with cognitively normal participants), so the gap understates the latent separation; the 1.25 to 2 SD guidance is for taxometric detection with 3 or more indicators. Suggested evidence: T mean and SD by Manly-Jones class (Dementia vs Normal separation). Not done.
   - Manly et al. (2022) used Blessed Dementia Rating Scale Part I only (no Part II): functional impairment is IQCODE ≥ 3.4 or Blessed Part I ≥ 2; cognitive concern is IQCODE > 3, Blessed Part I > 0, or PD102 = 3.
   - Figure guidance: the project instructions cover only figure location, inline computed values, and counts on schematic diagrams; nothing on size, dpi, fonts, palette, or captions. No AGENTS.md or R rules file in this project (the _AgentKit copy is in Dropbox, not connected).
3. Rich's untracked edits from `MS_Appendix_1_2026-10-21_edited.docx` carried into the QMD (details above). Memory rule restored in Step 4 at Rich's request.
4. Table A4 "empty" was Word merging adjacent Tables A4 and A5; fixed with separating paragraphs (also A9/A10/A11). `fig-dpi: 300` added to the A2 appendices and tables/figures control.
5. Appendix 1 switched to `[@Key]` citations with `bibliography.bib` and `templates/apa.csl`; bib entries fixed (Harrell, van Buuren, mice) and Manly2022 given all 8 authors. Pandoc test render: all keys resolve.

State of the files at 09:00:
- QMD, MS_020, A7_070, bibliography.bib, templates/apa.csl, and instructions.md changed today; nothing rendered since the 10-21 morning render.
- Rebuild: `Rscript Analysis7_Driver.R`, then `Rscript Analysis2_Driver.R`. A2 alone will fail or print wrong domain shares until A7 is rerun.
- After the render, check: the Step 3 domain-share sentence (expect about 14% and T ≈ 36 to 37 for 2+, about 34% and T ≈ 43 to 44 for 1+); the A7_070 guard (n floor 2,900 is a guess; lower it if A7 stops on n alone); the reference list; Table A4 on one page; figure sharpness.

Open items (carried and new):
- Talk to Doug about how 37/48 were chosen.
- Data section "used to calibrate" vs Limitation 1 "may have been chosen with reference to": reconcile later.
- The Step 3 sentence describing the Manly-Jones rules names only the 1-domain MCI path; Manly-Jones also classifies 2+ impaired domains without informant-rated functional impairment as MCI (eTable 9). Consider adding.
- Optional: T by Manly-Jones class (Dementia vs Normal separation) to support the Meehl & Yonce point.
- Optional: a "Figures" section in the Reporting Rules (theme, size, dpi, palette, captions).
- From 10-20: `MS_Main_Control.qmd` line 51 "calibrated to HRS/HCAP"; whether the A10 share log should record the appendix sent to Lindsay on 10-20.

## 2026-10-21 (late morning): pre-specified cut-point selection replaces 37/48

### Confirmed facts
- Doug Tommet did not recall how T < 37 and T < 48 were chosen. Lindsay Kobayashi agreed to a redo with a documented, rigorous rule and a new data share.
- Before this change, the T score was not rounded anywhere; `A7_060` compared unrounded `TF` with the cut points.
- In `v1_algorithm`, the dementia class depends only on the severe cut (plus the IQCODE route), so all pairs that share a severe cut have the same dementia kappa.

### Decisions (Rich, 2026-10-21; rule fixed before any result)
- Score: TR = floor(TF + 0.5), used for selection and for classification in every wave and in the share. Integer cut points only.
- Fixed: D102 memory gate, IQCODE cuts 3.0/3.4, the CFA model, and the norms.
- Sample and reference: HRS-HCAP Wave 1 with HCAP16WGTR and Manly-Jones `vs1hcapdxeap`.
- Candidates: severe 25 to 50, moderate 30 to 60, moderate above severe.
- Eligible: weighted dementia share within 0.5 percentage points of Manly-Jones, MCI share within 1.0 point.
- Widening: if none, widen dementia to 1.0 (MCI at 1.0); then return dementia to the smallest tolerance (0.5 or 1.0) with any dementia match and widen MCI by 0.5. The dementia tolerance never exceeds 1.0 point (Rich, 2026-10-21, replacing a 3.0 cap Claude had proposed). If no severe cut gives a dementia share within 1.0 point, A7_070 stops.
- Selection: highest weighted dementia kappa; then higher weighted 3-class kappa; then smaller MCI difference; then lower moderate cut.
- Stability: design-based bootstrap (SECU within STRATUM, Rao-Wu n_h - 1, 500 replicates, seed 20261021), description only.
- Share: same columns as the July file (TF unrounded), new dated name, README in `Data/` (untracked is fine), a share-log row, and an A10 old-versus-new comparison. Step 3 domain-share paragraph removed from Appendix 1 (`domain_quantiles` object kept).
- Scope: A7, A10, A2, A8. A2_2 not updated.

### Code changes (not run; Rich runs locally)
- `A7_055`: adds TR to the norm, merged, and long objects.
- `A7_060`: `algorithm_thresholds()` and `classify_core()` take `score` (default "TR"); missing-T routing uses `is.na(cog_threshold)`.
- `A7_070`: rewritten with the pre-specification header, a collapsed fast evaluator with plain-number kappas (stops if it disagrees with `classify_core()` or the A7_060 kappa functions), selection, comparison of the selected cuts with 37/48 on TR and on TF, bootstrap, and `domain_quantiles`. New elements: `cuts`, `score`, `selected`, `eligible`, `tol_dementia_used`, `tol_mci_used`, `selection_stage`, `spec`, `candidates`, `previous_cuts`, `comparison`, `bootstrap`. Removed: `algorithm`, `calibrated`, `calibrated_cuts`, `grid`, `grid_position`, `max_kappa`, `grid_step`.
- `A7_075`: writes `Data/Dx_for_LK-<run date>.csv` (July layout) and sources the restored `A7_085-export_readme.R`, which writes `Data/Dx_for_LK-<run date>_README.md` (cut points, rule, rationale, shares and kappas, bootstrap, changes versus the July file). `export_file` saved in `A7_075_algorithm_summary`. The rebuild file is no longer written.
- `A7_090`: uses TR. A7 chapters `A7_070.qmd` and `A7_075.qmd` rewritten for the rule.
- `A10_005`: new row for the new file ("Not yet sent"); July row marked "to be replaced". `A10_010` and `A10_000` rewritten: July versus new, with checks, a 2-step split (rounding, then cuts), bands on TF where the rules can differ, and prevalence for ages 65 and older.
- `MS_020` and Appendix 1: overview step 3, Step 3 approach (rule, rationale) and results (eligible pairs, selected cuts, kappas, bootstrap), Figure A2 bands on the rounded score (dashed lines at cut - 0.5), Table A3 note, Limitation 1 ("were selected ... is optimistic").
- A8: new slide "Cut Points: Pre-specified Selection" in `A8_020_Methods.qmd`.
- Old versions archived in `R/Archive_grid_search_2026-10-20/` (README updated). README.md and instructions.md updated (A7 and A10).
- Checks done here: every edited R file, chunk, and inline expression parses (R 4 in the cloud workspace, code files only). `A7_070` and `A7_085` ran end to end on simulated data (no project data): selection, both checks, widening, bootstrap (about 0.3 s per replicate), and README text.

### Problems found
- Claude printed 4 rows of `Data/Dx_for_LK-rebuild.csv` while checking the column layout, against the rule that Claude never opens data files. Reported to Rich at once; not used.
- `A8`'s algorithm flow figure is a static PNG from April 2026 with April counts (`Figures/HRS-Core-Diagnostic-Algorithm-With-Ns-HRS-Sample-2026-04-22.png`). Not changed.

### Rebuild
- `Rscript Analysis0_Driver.R` is not needed. Run `Rscript Analysis7_Driver.R`, then `Rscript Analysis10_Driver.R`, `Rscript Analysis2_Driver.R`, `Rscript Analysis8_Driver.R`. A7 now writes the new share and README.

### Open questions
- Review the selected cut points, the tolerance stage, and the bootstrap in the A7 report before sending the file.
- Bootstrap replicates in which no severe cut reaches the 1.0-point dementia cap are recorded as "rule not met" and count as not selecting the same pair (A7_070 `bootstrap$n_rule_not_met`). In the full sample the same condition stops A7.
- When the file is sent, update its row in `A10_005` (date, sender, status).
- Review the rationale text in the README and Appendix 1 Step 3 (written by Claude).
- A8 static flow figure (April counts); A2_2 is stale until rerun.

## 2026-10-21 (12:30): tracked changes from MS_Appendix_1_2026-10-21-1228_edited.docx

- Found by tracked-change extraction and by a paragraph compare against the 12:09 render (`MS_Appendix_1_2026-10-21.docx`). No changes were made outside track changes, and the docx has no comments and no tracked changes in footnotes or endnotes.
- Carried into `MS_Appendix-1-Core-Algorithm.qmd`, Step 3 approach: "by a rule written before any result was seen" changed to "with the following procedure:"; "A pair was eligible" changed to "A threshold pair was a candidate for selection"; "had been eligible" changed to "had been identified as a candidate"; "(and no further)" now in parentheses.
- Step 3 results: "band" changed to "range" (moderate: "range of the T score"); "class prevalence estimates"; "of the bootstrap samples" added twice. The generated sentence in `MS_020` (`a1_widen_text`) now reads "N of 575 pairs of thresholds were identified as candidates" (both branches).
- Endnote: the ordinal openers "First,", "Second,", and "Third," deleted from the 3 reason paragraphs.
- The pre-edit QMD was archived as `R/Archive_grid_search_2026-10-20/MS_Appendix-1_before_1228_edits.qmd`. Both files parse. Not rendered. Rebuild: `Rscript Analysis2_Driver.R`.
- Results from Rich's run, as shown in the render: 5 of 575 pairs were eligible; selected TR < 39 and TR < 48; weighted dementia kappa 0.34, 3-class kappa 0.50; the bootstrap selected the same pair in 8% of samples and a pair within 1 point of both cuts in 50%.

### Open questions
- "Among eligible pairs" in Step 3 still uses the old term; Rich changed the other two uses to "candidate".
- With the ordinals gone, 3 references to the reasons by number remain: the endnote opener ("We set the plan aside for 3 reasons ... The first and third reasons are summarized in Step 4") and `MS_020` `a1_fn_conclusion_text` ("rests on the third reason" or "apart from the third reason").
- The bootstrap shows the selected pair is unstable (8% same pair). Decide whether the appendix and the README for Lindsay should say so more directly.

## 2026-10-21 (14:30): note on why the bootstrap rarely repeats the cut points

- Discussed a bootstrap stratified on Manly-Jones class (fixed margins). Claude's rough estimate: the standard error of the dementia gap falls from about 0.5 to 0.4 points, a small gain, because most of the instability comes from whole-point steps compared with 0.5- and 1.0-point limits. Decision (Rich): no new bootstrap; add an explanatory note instead.
- Added a paragraph after the Step 3 results in Appendix 1 (`MS_020`: `a1_step`, `a1_step_dem`, `a1_step_mci`, computed from `A7_070$candidates` next to the selected cuts) and the same note in the README for Lindsay (`A7_085`: `step_near`, `replicate_text`).
- Rebuild: Appendix 1 needs only `Rscript Analysis2_Driver.R`; the README text appears after the next `Rscript Analysis7_Driver.R`.
