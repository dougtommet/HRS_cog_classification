**Project Map**

The project has two foundational pipelines without root drivers:

- **A0**: data preparation. It builds the four-wave HRS/HCAP analytic spine: demographics, cognitive tests, ADL/IADL, informant/self-report measures, external classifications, and survey/consensus weights. Output: `A0_030_hrs16_merged.rds`, which A7 requires. Control: `A0_000-Main_control.qmd`.
- **A7**: the current HRS Core cognitive-classification analysis. It fits the survey-weighted CFA, creates demographically normed factor scores, applies the T-score/self-report/JORM algorithm, and compares algorithmic, Langa-Weir, HRS model, Hudomiet, and consensus-panel diagnoses. Control: `A7_000-Main_control.qmd`. Latest report: `A7_HRS_cog_classification_2026-08-07.html`.

**Root Drivers**

- **`Analysis1_Driver.R`**: original HRS Core actuarial derivation/validation workflow: read and recode data, fit factor models, norm scores, find cut points, apply and validate the algorithm. It is currently broken because it targets the deleted `R/000-master.qmd`; the active control is `A1_000-master.qmd`.
- **`Analysis2_Driver.R`**: manuscript plus tables/figures appendix. The manuscript frames the key result: weak three-class consensus agreement ($\kappa_w=0.33$), but strong dementia-versus-non-dementia agreement ($\kappa=0.77$, PPV $=0.70$, sensitivity $=0.83$). No current `MS_Main_*.docx` or appendix DOCX is stored in `Reports`.
- **`Analysis3_Driver.R`**: profile mixture model (PMM). Calibrates a known-class model in HCAP, cross-validates it, then applies it to the HRS age-65+ Core sample to return Normal/MCI/Dementia probabilities and classifications. Inputs include cognitive, functional, self/informant, and demographic indicators; output applies to $N=9{,}972$. Latest report: `PMM_Analysis_Report_2026-03-25.html`.
- **`Analysis4_Driver.do`**: Stata diagnostic/concordance workflow. It merges Core and HCAP factor scores, evaluates their correlation and impairment-category concordance, and generates four figures in `Figures`; it does not create a report.
- **`Analysis5_Driver.R`**: sharing/data-dictionary report. Documents available cognitive, functional, covariate, classification, and survey-design variables in the prepared HRS/HCAP object. No current report is stored; its QMD still has a machine-specific helper path.
- **`Analysis6_Driver.R`**: temporary debugging entrypoint. It either renders the PMM comparison tables or summarizes an Mplus H5 file. Latest comparison report: `tmp_pmm_110_comparison_2026-03-25.html`.
- **`Slides2603_Driver.R`**: PMM presentation: approach, covariate adjustment, probabilistic classifications, margins, and comparator tables. Latest deck: `Slides2603_2026-03-25.html`.
- **`Analysis8_Driver.R`**: short A7 results deck. It depends on A7’s saved comparison tables. Latest deck: `Slides_A7summary_2026-04-22.html`.

**Results To Carry Forward**

- A7 uses a survey-weighted CFA on $N=9{,}218$ HRS 2016 respondents aged 65+, with demographic norming explaining about $R^2=0.37$ of factor-score variance. It compares methods in the HCAP subsample ($N=3{,}496$).
- The latest A7 report gives quadratic weighted $\kappa=0.505$ for its HRS classification versus the HCAP algorithmic classification. The A7 manuscript-oriented interpretation is that MCI versus normal remains the difficult boundary, while dementia identification is much stronger.
- PMM’s reported weighted distribution is approximately 68% normal, 22% MCI, and 10% dementia.
- PMM’s three-class agreement with the algorithmic reference is moderate and similar to Langa-Weir ($\kappa_w=0.51$). Against consensus, PMM is reported at $\kappa_w=0.53$ versus Langa-Weir’s $0.10$.
- For binary dementia classification, Langa-Weir performs better against consensus ($\kappa=0.74$) than PMM ($0.38$) or algorithmic diagnosis ($0.54$). The PMM deck’s conclusion is that PMM is not a net improvement over the HCAP-algorithm-in-Core approach, especially for MCI margins.

The practical hierarchy is: run **A0** to prepare data, then **A7** for the current algorithmic analysis, **A8** to present it; use **Analysis3/Slides2603** for the PMM alternative; and **Analysis2** to turn the settled results into manuscript artifacts.