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

This repository contains seven related but distinct analysis workflows. Existing source files remain in place. The authoritative automation entrypoints are the root-level drivers listed below.

### Analysis 1: HRS Core actuarial algorithm derivation and validation

- Driver: `./Analysis1_Driver.R`
- Control: `./R/000-master.qmd`
- Primary source programs:
  - `./R/001-libraries.R` through `./R/030-implementing_algorithm_in_HCAP.R`
  - `./R/_005-read_data.qmd` through `./R/_035-validation_comparison.qmd`
- Rebuild command:
  - `Rscript Analysis1_Driver.R`
- Final rendered output:
  - `./Reports/HRS_cognition_[date].html`
- Main derived data products:
  - `./R_objects/*.rds`
  - `./R_objects/025_hrs16_cog.dta`

### Analysis 2: Manuscript and manuscript tables/figures appendix

- Driver: `./Analysis2_Driver.R`
- Control files:
  - `./R/MS_Main_Control.qmd`
  - `./R/MS_Tab_Fig_Apndx_Control.qmd`
- Rebuild command:
  - `Rscript Analysis2_Driver.R`
- Final rendered outputs:
  - `./Reports/MS_Main_[date].docx`
  - `./Reports/MS_Tab_Fig_Apndx_[date].docx`

### Analysis 3: PMM profile mixture modeling analysis report

- Driver: `./Analysis3_Driver.R`
- Control: `./R/PMM_000_Analysis_Report_Control.qmd`
- Primary source programs:
  - `./R/PMM_027_custom_functions.R`
  - `./R/PMM_031_Pull_data.R` through `./R/PMM_103_calibration_model.R`
  - `./R/PMM_011_*.qmd` through `./R/PMM_888_References.qmd`
- Rebuild command:
  - `Rscript Analysis3_Driver.R`
- Final rendered output:
  - `./Reports/PMM_Analysis_Report_[date].html`
- Main derived data products:
  - `./R_objects/PMM_*.RDS`
  - `./mplus_output/pmm_102/`
  - `./mplus_output/pmm_103/`

### Analysis 4: Stata ad hoc concordance and figure generation workflow

- Driver: `./Analysis4_Driver.do`
- Control: `./Stata/Analysis4_Control.do`
- Legacy reference workflow:
  - `./Stata/Ad-Hoc-20241219.do`
- Rebuild command:
  - From the project root in Stata: `do Analysis4_Driver.do`
  - Or from a shell already in the project root: `stata-mp -b do Analysis4_Driver.do`
- Final rendered outputs:
  - `./Figures/Stata_Ad_Hoc_fig1.png`
  - `./Figures/Stata_Ad_Hoc_fig2.png`
  - `./Figures/Stata_Ad_Hoc_fig3.png`
  - `./Figures/Stata_Ad_Hoc_fig4.png`
- Data inputs used by the Stata driver:
  - `./Stata/20240228-040.dta`
  - `./Stata/w051-preimputation.dta`
  - `./R_objects/025_hrs16_cog.dta`

### Analysis 5: Sharing and data-dictionary style report

- Driver: `./Analysis5_Driver.R`
- Control: `./R/ΨMCA25-Sharing.qmd`
- Rebuild command:
  - `Rscript Analysis5_Driver.R`
- Final rendered output:
  - `./Reports/PsiMCA25_Sharing_[date].html`
- Main derived data products:
  - `./R_objects/HCAPHRS.RDS`
- Important note:
  - The control file currently sources an external helper with an absolute path. The driver is authoritative, but the control file is not yet fully portable across machines.

### Analysis 6: Temporary and debugging workflow

- Driver: `./Analysis6_Driver.R`
- Controls:
  - `./R/tmp_control.qmd`
  - `./R/tmp_summarize_norm_npb_h5.R`
- Rebuild commands:
  - `Rscript Analysis6_Driver.R render-qmd`
  - `Rscript Analysis6_Driver.R summarize-h5 [path-to-h5]`
- Final rendered outputs:
  - `./Reports/tmp_consensus_sample_comparison_[date].html`
  - `./Reports/tmp_norm_npb_h5_summary_[date].txt`
- Purpose:
  - This is a temporary debugging workflow for rendering ad hoc QMD content and inspecting Mplus H5 outputs.

### Analysis 7: Slides2603 revealjs slide deck

- Driver: `./Slides2603_Driver.R`
- Control: `./R/Slides2603_Control.qmd`
- Rebuild command:
  - `Rscript Slides2603_Driver.R`
- Final rendered output:
  - `./Reports/Slides2603_[date].html`
- Purpose:
  - This is a revealjs slide-show workflow scaffolded from an external presentation template and adapted to this repository's relative-path conventions.
- Slide-authoring convention:
  - Use one included `Slides2603_*.qmd` file per slide.
  - Keep shared setup, theme, and revealjs options in the control file.
  - Add new slides by appending additional include statements in the control file.

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
- Keep root drivers small and orchestration-focused.
- If a workflow produces multiple final artifacts, the driver may render multiple control files.

## Dependencies And Notes

- R workflows depend on Quarto plus the R packages used throughout `./R/001-libraries.R` and the scripts they source.
- Analysis 4 depends on Stata and the user-written commands used in the ad hoc script, including:
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

That pattern keeps the workflows easier to rebuild, document, and automate without changing the legacy source structure.
