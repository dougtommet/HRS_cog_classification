# Project Instructions

Read and follow:

1. ./References/instructions.md
2. This prompt, treating it as the operating rules for this project

## Project Overview

This repository contains ten related but distinct analysis workflows. Existing source files remain in place. The authoritative automation entrypoints are the root-level drivers listed below. Source file naming follows an `A#_` prefix convention (e.g., `A0_`, `A1_`, `A7_`) that groups files by workflow.

Note on numbering: "Analysis 7" is the Slides2603 deck; "Analysis A7" is the HRS Core cognitive classification. They are separate workflows with separate drivers.

### Analysis A0: HRS 2016–2022 data processing

- Driver: ./Analysis0_Driver.R
- Control: ./R/A0_000-Main_control.qmd
- Primary source programs:
  - ./R/A0_001-libraries.R
  - ./R/A0_002-folder_paths.R
  - ./R/A0_005-read_data.R through ./R/A0_030-merge_data.R
  - ./R/A0_005-read_data.qmd through ./R/A0_030-merge_data.qmd
- Rebuild command:
  - Rscript Analysis0_Driver.R
- Final rendered output:
  - ./Reports/A0_HRS_data_processing_[date].html
- Main derived data products:
  - ./R_objects/A0_030_hrs16_merged.rds
  - ./R_objects/A0_030_hcap16_merged.rds
- Purpose:
  - Reads raw HRS 2016–2022 and HCAP 2016 files, recodes demographics, cognitive items, ADL/IADL, Jorm, external classifications (Langa-Weir, Hudomiet), and weights, and merges them into the analytic spine used by Analysis A7.
- Important note:
  - Raw data are read from machine-specific Dropbox paths set in ./R/A0_002-folder_paths.R.

### Analysis 1: HRS Core actuarial algorithm derivation and validation

- Driver: ./Analysis1_Driver.R
- Control: ./R/A1_000-master.qmd
- Known issue: Analysis1_Driver.R still targets the deleted ./R/000-master.qmd and must be pointed at ./R/A1_000-master.qmd before it will run.
- Primary source programs:
  - ./R/A1_001-libraries.R through ./R/A1_030-implementing_algorithm_in_HCAP.R
  - ./R/A1_005-read_data.qmd through ./R/A1_035-validation_comparison.qmd
- Rebuild command:
  - Rscript Analysis1_Driver.R
- Final rendered output:
  - ./Reports/HRS_cognition_[date].html
- Main derived data products:
  - ./R_objects/*.rds
  - ./R_objects/025_hrs16_cog.dta

### Analysis 2: Manuscript, tables/figures, and appendices

- Driver: ./Analysis2_Driver.R
- Control files:
  - ./R/MS_MAIN/MS_Main_Control.qmd
  - ./R/MS_MAIN/MS_Tab_Fig_Apndx_Control.qmd
  - ./R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd
  - ./R/MS_MAIN/MS_Appendix-2-PMM.qmd
  - ./R/MS_MAIN/MS_Appendix-3-Other-Study-Kappas.qmd
- Rebuild command:
  - Rscript Analysis2_Driver.R
- Final rendered outputs:
  - ./Reports/MS_Main_[date].docx
  - ./Reports/MS_Tab_Fig_Apndx_[date].docx
  - ./Reports/MS_Appendix_1_[date].docx
  - ./Reports/MS_Appendix_2_[date].docx
  - ./Reports/MS_Appendix_3_[date].docx
- Prerequisite:
  - Analyses A7 and 3 must be current; manuscript values are read from their saved results.

### Analysis 3: PMM profile mixture modeling analysis report

- Driver: ./Analysis3_Driver.R
- Control: ./R/PMM_000_Analysis_Report_Control.qmd
- Primary source programs:
  - ./R/PMM_027_custom_functions.R
  - ./R/PMM_031_Pull_data.R through ./R/PMM_103_calibration_model.R
  - ./R/PMM_011_*.qmd through ./R/PMM_888_References.qmd
- Rebuild command:
  - Rscript Analysis3_Driver.R
- Final rendered output:
  - ./Reports/PMM_Analysis_Report_[date].html
- Main derived data products:
  - ./R_objects/PMM_*.RDS
  - ./mplus_output/pmm_102/
  - ./mplus_output/pmm_103/

### Analysis 4: Stata ad hoc concordance and figure generation workflow

- Driver: ./Analysis4_Driver.do
- Control: ./Stata/Analysis4_Control.do
- Legacy reference workflow:
  - ./Stata/Ad-Hoc-20241219.do
- Rebuild command:
  - From the project root in Stata: do Analysis4_Driver.do
  - Or from a shell already in the project root: stata-mp -b do Analysis4_Driver.do
- Final rendered outputs:
  - ./Figures/Stata_Ad_Hoc_fig1.png
  - ./Figures/Stata_Ad_Hoc_fig2.png
  - ./Figures/Stata_Ad_Hoc_fig3.png
  - ./Figures/Stata_Ad_Hoc_fig4.png
- Data inputs used by the Stata driver:
  - ./Stata/20240228-040.dta
  - ./Stata/w051-preimputation.dta
  - ./R_objects/025_hrs16_cog.dta

### Analysis 5: Sharing and data-dictionary style report

- Driver: ./Analysis5_Driver.R
- Control: ./R/ΨMCA25-Sharing.qmd
- Rebuild command:
  - Rscript Analysis5_Driver.R
- Final rendered output:
  - ./Reports/PsiMCA25_Sharing_[date].html
- Main derived data products:
  - ./R_objects/HCAPHRS.RDS
- Important note:
  - The control file currently sources an external helper with an absolute path. The driver is authoritative, but the control file is not yet fully portable across machines.

### Analysis 6: Temporary and debugging workflow

- Driver: ./Analysis6_Driver.R
- Controls:
  - ./R/tmp_control.qmd
  - ./R/tmp_summarize_norm_npb_h5.R
- Rebuild commands:
  - Rscript Analysis6_Driver.R render-qmd
  - Rscript Analysis6_Driver.R summarize-h5 [path-to-h5]
- Final rendered outputs:
  - ./Reports/tmp_consensus_sample_comparison_[date].html
  - ./Reports/tmp_norm_npb_h5_summary_[date].txt
- Purpose:
  - This is a temporary debugging workflow for rendering ad hoc QMD content and inspecting Mplus H5 outputs.

### Analysis A7: HRS Core cognitive classification (2016 derivation, 2016–2022 application)

- Driver: ./Analysis7_Driver.R
- Control: ./R/A7_000-Main_control.qmd
- Primary source programs:
  - ./R/A7_001-libraries.R
  - ./R/A7_005-get_data.R
  - ./R/A7_050-mplus_model.R through ./R/A7_100-comparison_of_diagnoses.R
  - Corresponding .qmd chapter files for each step
- Rebuild command:
  - Rscript Analysis7_Driver.R
- Final rendered output:
  - ./Reports/A7_HRS_cog_classification_[date].html
- Main derived data products:
  - ./R_objects/A7_*.rds (including A7_075_hrs16_22_long.rds and A7_100_hcap_tables.rds)
  - ./mplus_output/A7/
  - ./Data/Dx_for_LK-2026-06-11.csv (person-wave dx_v1 diagnoses, 2016–2022; filename is hard-coded in A7_075)
- Purpose:
  - Fits a survey-weighted CFA to HRS 2016 Core cognitive items, converts factor scores to demographically adjusted T-scores (TF) normed in the HCAP normative sample, and applies the Core algorithm (dx_v1: TF cut-points, self-rated memory D102, Jorm fallback). The 2016 parameters are fixed and applied to 2016–2022 person-waves. Compares the 2016 classification with the HRS/HCAP Algorithm, consensus, Langa-Weir, and Hudomiet in HCAP.
- Prerequisite:
  - Analysis A0.

### Analysis 7: Slides2603 revealjs slide deck

- Driver: ./Slides2603_Driver.R
- Control: ./R/Slides2603_Control.qmd
- Rebuild command:
  - Rscript Slides2603_Driver.R
- Final rendered output:
  - ./Reports/Slides2603_[date].html
- Purpose:
  - This is a revealjs slide-show workflow scaffolded from an external presentation template and adapted to this repository's relative-path conventions.
- Slide-authoring convention:
  - Use one included `Slides2603_*.qmd` file per slide.
  - Keep shared setup, theme, and revealjs options in the control file.
  - Add new slides by appending additional include statements in the control file.

### Analysis A8: Summary slide deck for Analysis A7

- Driver: ./Analysis8_Driver.R
- Control: ./R/A8_Control.qmd
- Slide chapter files:
  - ./R/A8_010_Introduction.qmd through ./R/A8_040_Conclusion.qmd
- Rebuild command:
  - Rscript Analysis8_Driver.R
- Final rendered output:
  - ./Reports/Slides_A7summary_[date].html
- Prerequisite:
  - Analyses A0 and A7.

## Workflow Rules

- Do not change existing legacy master or control files unless explicitly requested.
- Prefer adding or updating root-level drivers that wrap the existing project controls.
- Use project-relative paths only.
- The one exception is Stata driver behavior: the driver must explicitly set the working directory, but still use project-relative file references after that.
- Preserve the current repository layout:
  - source code in ./R and ./Stata
  - derived data in ./R_objects and ./mplus_output
  - rendered reports in ./Reports
  - figures in ./Figures
  - references and project guidance in ./References
- Keep root drivers small and orchestration-focused.
- If a workflow produces multiple final artifacts, the driver may render multiple control files.

## Dependencies And Notes

- R workflows depend on Quarto plus the R packages loaded by the workflow-specific library scripts (e.g., ./R/A0_001-libraries.R, ./R/A1_001-libraries.R, ./R/A7_001-libraries.R) and the scripts they source.
- Analysis 4 depends on Stata and the user-written commands used in the ad hoc script, including:
  - baplot
  - checkvar
  - kappaetc
- This repository does not currently have a ./bibliography.bib file. Do not assume one exists.

## Agent Response Format

When working in this repository, first report:

- which of the ten analysis drivers is authoritative for the requested task
- the exact rebuild command(s)
- the expected final output artifact(s) and destination path(s)

When proposing code changes:

- prefer small diffs
- keep paths relative
- do not silently repurpose a driver for a different analysis

## References Grounding

References may contain both citable literature and non-citable project resources.

When answering questions grounded in materials under ./References, use extracted artifacts in ./References/llm_out when they exist.

Artifacts available:

- ./References/llm_out/[basename].jsonl
- ./References/llm_out/[basename].txt
- ./References/llm_out/[basename].md
- ./References/llm_out/[basename].meta.json

Rules:

- Use extracted artifacts as the source of truth rather than prior knowledge.
- For factual claims about PDF content, cite page numbers from .jsonl when available, otherwise use page markers in .txt.
- If support is not found in the extracted text, say so and propose keywords to search rather than guessing.
- If .md conflicts with .jsonl or .txt, treat .jsonl and .txt as authoritative.

Internal-source citation format:

- PDFs: (Internal: References/[path], p. 12)
- Sectioned docs: (Internal: References/[path], section "[heading]")
- Code/text: (Internal: References/[path], lines [start]-[end])

Non-PDF and code resources:

- Text-based resources inside ./References can be treated as authoritative directly.
- Quote exact snippets when relying on them.
- If a resource is not text-searchable, convert it to PDF or text before using it as evidence.
