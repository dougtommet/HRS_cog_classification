# Session Logs

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

