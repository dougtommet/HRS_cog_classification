# HRS Cognitive Classification

This repository contains a family of related analysis projects focused on cognitive classification in the Health and Retirement Study (HRS), with particular emphasis on linking HRS Core measures to HCAP-based cognitive classifications.

At a high level, the repository supports several kinds of work:

- development and validation of HRS Core actuarial cognitive classification algorithms
- manuscript production and manuscript appendix generation
- profile mixture modeling (PMM) analyses and reports
- Stata-based ad hoc validation and figure generation
- sharing and data-dictionary style reporting
- temporary and debugging workflows used to inspect intermediate outputs
- revealjs slide deck generation for presentations

The codebase is organized so that source code lives primarily in `R/` and `Stata/`, derived data live in `R_objects/` and `mplus_output/`, rendered outputs live in `Reports/`, and figures live in `Figures/`.

## Project Overview

This repository contains ten related but distinct analysis workflows. Existing source files remain in place. The authoritative automation entrypoints are the root-level drivers listed below. Source file naming follows an `A#_` prefix convention (e.g., `A0_`, `A1_`, `A7_`) that groups files by workflow.

### Run order

Workflows depend on derived data written by earlier workflows. Rebuild upstream first:

1. **Core algorithm (current):** A0 → A7 → A8
   - `Rscript Analysis0_Driver.R`, then `Rscript Analysis7_Driver.R`, then `Rscript Analysis8_Driver.R`
2. **PMM:** A1 → A3 → A9 (Slides2603)
   - A3 reads the unprefixed A1 objects (e.g., `R_objects/025_hrs16_cog.rds`, `014_hrshcap.rds`), not A0 output. `Analysis1_Driver.R` is currently broken (see Analysis A1), so A3 relies on the saved A1 objects.
   - A5 reads `R_objects/PMM_045.RDS` from A3.
3. **Manuscript:** A2 last. It reads A7 results (`R_objects/A7_*.rds`) and re-sources the PMM comparison and margins scripts, which read the A3 Mplus outputs.

A4 (Stata) reads `R_objects/025_hrs16_cog.dta` from A1. A6 is ad hoc debugging.

Links under "Final rendered output" point to the most recent report committed to `main`, served through raw.githack.com. They only work after the report is pushed, and they must be updated by hand when a newer report is committed. DOCX links open in Microsoft's Office Online viewer, which fetches the file from raw.githubusercontent.com (this works only while the repository is public).

### Analysis A0: HRS 2016–2022 data processing

- Purpose:
  - Reads raw HRS 2016–2022 Core and HCAP 2016 files; recodes demographics, cognitive items, ADL/IADL, functional items, Jorm IQCODE, external classifications (Langa-Weir, Hudomiet), and consensus weights; and merges everything into the analytic spine used by Analysis A7.
- Driver: `./Analysis0_Driver.R`
- Control: `./R/A0_000-Main_control.qmd`
  - Source programs: `./R/A0_001-libraries.R`, `./R/A0_002-folder_paths.R`, and `./R/A0_005-read_data.R` through `./R/A0_030-merge_data.R`, each with a corresponding `.qmd` chapter file
- Rebuild command:
  - `Rscript Analysis0_Driver.R`
- Final rendered output:
  - `./Reports/A0_HRS_data_processing_[date].html`
  - Most recent: no driver render committed yet. Pre-driver render (in `R/`): https://raw.githack.com/dougtommet/HRS_cog_classification/main/R/A0_000-Main_control.html
- Data inputs:
  - Raw HRS/HCAP files, Langa-Weir, Hudomiet, and `normexcld.dta`, read from machine-specific Dropbox paths set in `./R/A0_002-folder_paths.R`
- Main derived data products:
  - `./R_objects/A0_030_hrs16_merged.rds`
  - `./R_objects/A0_030_hrs16_22_merged.rds`
  - `./R_objects/A0_030_hcap16_merged.rds`
- Date initiated: 2026-04-16
- Date last updated: 2026-09-30

### Analysis A1: HRS Core actuarial algorithm derivation and validation (legacy)

- Purpose:
  - Original derivation and validation of the HRS Core actuarial algorithm: read and recode data, fit factor models, norm scores, find cut points, and apply and validate the algorithm in HCAP. Superseded by Analysis A7, but its saved objects are still inputs to Analyses A3 and A4.
- Driver: `./Analysis1_Driver.R`
  - Known issue: the driver still targets the deleted `R/000-master.qmd`. Point `render_target` at `R/A1_000-master.qmd` before rebuilding.
- Control: `./R/A1_000-master.qmd`
  - Source programs: `./R/A1_001-libraries.R` through `./R/A1_030-implementing_algorithm_in_HCAP.R`; `./R/A1_005-read_data.qmd` through `./R/A1_035-validation_comparison.qmd`
- Rebuild command:
  - `Rscript Analysis1_Driver.R`
- Final rendered output:
  - `./Reports/HRS_cognition_[date].html`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/HRS_cognition_2025-07-22.html
- Data inputs:
  - Raw HRS 2016 and HCAP 2016 files read from machine-specific Dropbox paths set in `./R/A1_002-folder_paths.R`
- Main derived data products:
  - `./R_objects/0##_*.rds` (unprefixed, e.g., `025_hrs16_cog.rds`, `014_hrshcap.rds`)
  - `./R_objects/025_hrs16_cog.dta`
- Date initiated: 2024-07-25 (first repository commit; the earliest report is dated 2024-07-19)
- Date last updated: 2026-04-17

### Analysis A2: Manuscript, tables/figures, and appendices

- Purpose:
  - Manuscript (outline mode), tables and figures document, and Appendices 1–3. All reported values are computed from saved Analysis A7 and PMM results at render time.
- Driver: `./Analysis2_Driver.R`
- Control:
  - `./R/MS_MAIN/MS_Main_Control.qmd`
  - `./R/MS_MAIN/MS_Tab_Fig_Apndx_Control.qmd`
  - `./R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd`, `MS_Appendix-2-PMM.qmd`, `MS_Appendix-3-Other-Study-Kappas.qmd`
  - Shared results objects: `./R/MS_MAIN/MS_001_analysis_cohort.R`, `./R/MS_MAIN/MS_010_results_objects.R`
- Rebuild command:
  - `Rscript Analysis2_Driver.R`
- Final rendered output:
  - `./Reports/MS_Main_[date].docx`, `MS_Tab_Fig_Apndx_[date].docx`, `MS_Appendix_1_[date].docx`, `MS_Appendix_2_[date].docx`, `MS_Appendix_3_[date].docx`
  - Most recent (opens in the Office Online viewer):
    - [Manuscript](https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2Fdougtommet%2FHRS_cog_classification%2Fmain%2FReports%2FMS_Main_2026-09-30.docx)
    - [Tables and figures](https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2Fdougtommet%2FHRS_cog_classification%2Fmain%2FReports%2FMS_Tab_Fig_Apndx_2026-09-30.docx)
    - [Appendix 1](https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2Fdougtommet%2FHRS_cog_classification%2Fmain%2FReports%2FMS_Appendix_1_2026-09-30.docx)
    - [Appendix 2](https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2Fdougtommet%2FHRS_cog_classification%2Fmain%2FReports%2FMS_Appendix_2_2026-09-30.docx)
    - [Appendix 3](https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2Fdougtommet%2FHRS_cog_classification%2Fmain%2FReports%2FMS_Appendix_3_2026-09-30.docx)
- Data inputs:
  - `./R_objects/A7_005_hrs16_merged.rds`, `./R_objects/A7_100_hcap_tables.rds` (Analysis A7)
  - `./R_objects/PMM_100.RDS`, `./mplus_output/pmm_103/`, `./mplus_output/pmm_103_jorm/` (Analysis A3)
  - `./reference_manuscript.docx` (Word reference document)
- Main derived data products:
  - `./Figures/MS_Main-Figure-2-PMM-Class-Probabilities.png`
- Date initiated: 2026-03-24
- Date last updated: 2026-09-30

### Analysis A3: PMM profile mixture modeling analysis report

- Purpose:
  - Calibrates a known-class profile mixture model (PMM) to the HRS/HCAP Algorithm in HCAP, with class proportions fixed at the survey-weighted HCAP distribution, and scores HCAP participants to obtain Normal/MCI/Dementia probabilities. Separate cognition and Jorm models. Every render refits the Mplus models.
- Driver: `./Analysis3_Driver.R`
- Control: `./R/PMM_000_Analysis_Report_Control.qmd`
  - Source programs: `./R/PMM_027_custom_functions.R`; `./R/PMM_031_Pull_data.R` through `./R/PMM_112_Margins.R`; `./R/PMM_011_*.qmd` through `./R/PMM_888_References.qmd`
  - Side script (not run by the driver): `Rscript R/PMM_104_weight_check.R`
- Rebuild command:
  - `Rscript Analysis3_Driver.R`
- Final rendered output:
  - `./Reports/PMM_Analysis_Report_[date].html`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/PMM_Analysis_Report_2026-09-23.html
- Data inputs:
  - Analysis A1 objects: `./R_objects/025_hrs16_cog.rds`, `013_hrs16_func.rds`, `012_hrs16_iadl.rds`, `014_hrshcap.rds`, `005_tracker.rds`, `005_hc16hp_r.rds`, `005_langa_weir.rds`, `005_hudomiet.rds`
  - `./Stata/20240228-040.dta` (HCAP validation sample)
- Main derived data products:
  - `./R_objects/PMM_*.RDS`
  - `./mplus_output/pmm_102/`, `./mplus_output/pmm_103/`, `./mplus_output/pmm_103_jorm/`
- Date initiated: 2025-07-24
- Date last updated: 2026-09-23

### Analysis A4: Stata ad hoc concordance and figures

- Purpose:
  - Merges Core and HCAP factor scores and evaluates their correlation and impairment-category concordance. Produces four figures; no report.
- Driver: `./Analysis4_Driver.do`
- Control: `./Stata/Analysis4_Control.do`
  - Legacy reference workflow: `./Stata/Ad-Hoc-20241219.do`
- Rebuild command:
  - From the project root in Stata: `do Analysis4_Driver.do`
  - Or from a shell in the project root: `stata-mp -b do Analysis4_Driver.do`
- Final rendered output:
  - `./Figures/Stata_Ad_Hoc_fig1.png` through `./Figures/Stata_Ad_Hoc_fig4.png`
  - Most recent: no driver output committed. Legacy figures: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Stata/fig1.png (also `fig2.png`–`fig4.png`)
- Data inputs:
  - `./Stata/20240228-040.dta`
  - `./Stata/w051-preimputation.dta`
  - `./R_objects/025_hrs16_cog.dta` (Analysis A1)
- Main derived data products:
  - None beyond the figures
- Date initiated: 2024-12-19
- Date last updated: 2026-03-24

### Analysis A5: Sharing and data-dictionary report

- Purpose:
  - Documents the cognitive, functional, covariate, classification, and survey-design variables in the prepared HRS/HCAP data object for sharing.
- Driver: `./Analysis5_Driver.R`
- Control: `./R/ΨMCA25-Sharing.qmd`
- Rebuild command:
  - `Rscript Analysis5_Driver.R`
- Final rendered output:
  - `./Reports/PsiMCA25_Sharing_[date].html`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/PsiMCA25_Sharing_2026-09-23.html
- Data inputs:
  - `./R_objects/PMM_045.RDS` (Analysis A3)
- Main derived data products:
  - `./R_objects/HCAPHRS.RDS`
- Date initiated: 2025-09-11
- Date last updated: 2026-09-23

### Analysis A6: Temporary and debugging workflow

- Purpose:
  - Renders ad hoc QMD content (currently the PMM comparison tables) and summarizes Mplus H5 outputs.
- Driver: `./Analysis6_Driver.R`
- Control:
  - `./R/tmp_control.qmd`
  - `./R/tmp_summarize_norm_npb_h5.R`
- Rebuild command:
  - `Rscript Analysis6_Driver.R render-qmd`
  - `Rscript Analysis6_Driver.R summarize-h5 [path-to-h5]`
- Final rendered output:
  - `./Reports/tmp_pmm_110_comparison_[date].html`
  - `./Reports/tmp_norm_npb_h5_summary_[date].txt`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/tmp_pmm_110_comparison_2026-09-23.html
- Data inputs:
  - Analysis A3 outputs (`./R_objects/PMM_100.RDS`, `./mplus_output/pmm_103*/`)
  - For `summarize-h5`: an Mplus H5 file (defaults to an absolute path to `mplus_output/norm_npb/norm_npb.h5`)
- Main derived data products:
  - None
- Date initiated: 2026-03-23
- Date last updated: 2026-09-23

### Analysis A7: HRS Core cognitive classification (2016 derivation, 2016–2022 application)

- Purpose:
  - Fits a survey-weighted CFA to HRS 2016 Core cognitive items, converts factor scores to demographically adjusted T-scores normed in the HCAP normative sample, and applies the Core algorithm (`dx_v1`: T-score cut points, self-rated memory, Jorm fallback). The 2016 parameters are fixed and applied to 2016–2022 person-waves. Compares the 2016 classification with the HRS/HCAP Algorithm, consensus, Langa-Weir, and Hudomiet in HCAP.
- Driver: `./Analysis7_Driver.R`
- Control: `./R/A7_000-Main_control.qmd`
  - Source programs: `./R/A7_001-libraries.R`, `./R/A7_005-get_data.R`, `./R/A7_050-mplus_model.R` through `./R/A7_100-comparison_of_diagnoses.R`, each with a corresponding `.qmd` chapter file
- Rebuild command:
  - `Rscript Analysis7_Driver.R`
- Final rendered output:
  - `./Reports/A7_HRS_cog_classification_[date].html`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/A7_HRS_cog_classification_2026-08-07.html
- Data inputs:
  - `./R_objects/A0_030_hrs16_merged.rds`, `A0_030_hrs16_22_merged.rds`, `A0_030_hcap16_merged.rds` (Analysis A0)
- Main derived data products:
  - `./R_objects/A7_*.rds` (including `A7_075_hrs16_22_long.rds` and `A7_100_hcap_tables.rds`)
  - `./mplus_output/A7/`
  - `./Data/Dx_for_LK-2026-06-11.csv` (shared export of person-wave `dx_v1` diagnoses, 2016–2022; filename is hard-coded in `A7_075`)
- Date initiated: 2026-04-17
- Date last updated: 2026-09-30

### Analysis A8: Summary slide deck for Analysis A7

- Purpose:
  - Revealjs deck summarizing the methods and results of Analysis A7, in the same format and theme as the Analysis A9 (Slides2603) deck.
- Driver: `./Analysis8_Driver.R`
- Control: `./R/A8_Control.qmd`
  - Slide chapter files: `./R/A8_010_Introduction.qmd`, `A8_020_Methods.qmd`, `A8_030_Results.qmd`, `A8_040_Conclusion.qmd`
- Rebuild command:
  - `Rscript Analysis8_Driver.R`
- Final rendered output:
  - `./Reports/Slides_A7summary_[date].html`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/Slides_A7summary_2026-09-23.html
- Data inputs:
  - `./R_objects/A7_100_hcap_tables.rds` (Analysis A7)
- Main derived data products:
  - None
- Date initiated: 2026-04-22
- Date last updated: 2026-04-22

### Analysis A9: PMM slide deck (Slides2603)

- Purpose:
  - Revealjs deck on the PMM: approach, covariate adjustment, probabilistic classification, margins, and comparator tables.
- Driver: `./Slides2603_Driver.R`
- Control: `./R/Slides2603_Control.qmd`
  - One included `Slides2603_*.qmd` file per slide; shared setup, theme, and revealjs options stay in the control file; add slides by appending include statements.
- Rebuild command:
  - `Rscript Slides2603_Driver.R`
- Final rendered output:
  - `./Reports/Slides2603_[date].html`
  - Most recent: https://raw.githack.com/dougtommet/HRS_cog_classification/main/Reports/Slides2603_2026-09-23.html
- Data inputs:
  - Analysis A3 outputs (`./R_objects/PMM_*.RDS`, `./mplus_output/pmm_103*/`), via `R/PMM_110_Comparison_of_Consensus_Langa_Weir.R` and `R/PMM_112_Margins.R`
- Main derived data products:
  - None
- Date initiated: 2026-03-24
- Date last updated: 2026-09-23

## Workflow Rules

- Do not change existing legacy master or control files unless explicitly requested.
- Prefer adding or updating root-level drivers that wrap the existing project controls.
- Use project-relative paths only.
- The one exception is Stata driver behavior: the driver must explicitly set the working directory, but still use project-relative file references after that.
- Preserve the current repository layout:
  - source code in `./R` and `./Stata`
  - derived data in `./R_objects` and `./mplus_output`
  - rendered reports in `./Reports`
  - figures in `./Figures`
  - references and project guidance in `./References`
  - repository maintenance scripts in `./tools`
- Keep root drivers small and orchestration-focused.
- If a workflow produces multiple final artifacts, the driver may render multiple control files.

## Dependencies And Notes

- R workflows depend on Quarto plus the R packages loaded by the workflow-specific library scripts (e.g., `./R/A0_001-libraries.R`, `./R/A1_001-libraries.R`, `./R/A7_001-libraries.R`) and the scripts they source.
- Analysis A4 depends on Stata and the user-written commands used in the ad hoc script, including:
  - `baplot`
  - `checkvar`
  - `kappaetc`
- This repository does not currently have a `./bibliography.bib` file. Do not assume one exists.

## Typical Rebuild Pattern

For most work in this repository, the expected pattern is:

1. Start from the appropriate root-level driver.
2. Let the driver call the project control file.
3. Let the control file include the analysis-specific child QMD files or call the necessary R/Stata code.
4. Look for final human-readable outputs in `Reports/` or `Figures/`.
5. Stage the new outputs (`git add`), run `python3 tools/update_readme.py`, and commit the outputs, sources, and README together.

That pattern keeps the workflows easier to rebuild, document, and automate without changing the legacy source structure.

## Keeping This README Current

`tools/update_readme.py` refreshes the parts of the Project Overview that change when an analysis is rerun. Everything else in this README is hand-written and is not touched.

- What it updates, for each `### Analysis A#:` section:
  - "Most recent" links: the newest dated report tracked in git (committed or staged). HTML and PNG files link through raw.githack.com; DOCX files link through the Office Online viewer.
  - "Date last updated": the latest commit touching that analysis's source files, or today if any of them has uncommitted changes.
  - It then copies the Project Overview into `References/instructions.md` so the two stay identical.
- Usage, from the project root:
  - `python3 tools/update_readme.py` rewrites both files in place.
  - `python3 tools/update_readme.py --check` changes nothing; it prints a diff and exits 1 if either file is stale.
- Configuration: the `ANALYSES` table at the top of the script maps each analysis to its report filename patterns and source paths. Update it when you add an analysis or rename its outputs. The new analysis also needs its own `### Analysis A#:` section in this README, written by hand.
- Limits:
  - Links point at `main` on GitHub, so they work only after the files are pushed.
  - "Date initiated" is written by hand. Git history does not always show where an analysis began.
  - Requires only Python 3 and git. It retries git natively if an Intel (x86_64) Python on Apple Silicon cannot run `/usr/bin/git`.
