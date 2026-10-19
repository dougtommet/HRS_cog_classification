# Dx_for_LK-2026-10-17.csv

Person-wave core algorithm classifications (`dx_v1`) for HRS core respondents, 2016-2022, written by `R/A7_075-implement_algorithm.R` (Analysis A7) on 2026-10-18. This README is written by `R/A7_085-export_readme.R` in the same run.

## Columns

- `HHID`, `PN`: HRS household and person identifiers.
- `wave`: HRS core wave (P = 2016, Q = 2018, R = 2020, S = 2022).
- `TF`: demographically adjusted T score from the core cognitive items (mean 50, SD 10 in the HRS-HCAP robust normative sample). Missing when no core cognitive tests were completed.
- `jorm`: 16-item IQCODE mean score (1 to 5), collected in proxy interviews.
- `D102`: self-rated memory compared with 2 years ago (1 better, 2 same, 3 worse, 8 don't know, 9 refused, -8 not ascertained), singly imputed when missing.
- `dx_v1`: core algorithm classification (1 Normal, 2 MCI, 3 Dementia). Missing when neither `TF` nor `jorm` is available.

Rows are all HRS core respondents interviewed in each wave (all ages). The algorithm's norms apply to ages 65 and older; restrict to that age range for analysis.

## What changed from the previous export

Previous export: `Dx_for_LK-2026-06-11.csv` (file last written 2026-10-07; the date in its name is not the date its contents were written).

1. **T-score cut points.** Severe: T < 37 before, T < 38.5 now. Moderate: T < 48 before, T < 46.5 now.
2. **How the cut points are set.** They are now chosen by a grid search (`R/A7_070-threshold_calibration.R`, step 0.5 T points) so that the weighted (HCAP16WGTR) Normal/MCI/Dementia shares of the core algorithm in HRS-HCAP Wave 1 match those of the Manly-Jones algorithm (vs1hcapdxeap). The previous cut points were set earlier by inspecting prevalence curves and had not been re-examined after upstream code changes.
3. **Unchanged:** the factor model and item parameters, the norms and T-score formula, the self-rated memory rule (worse, don't know, refused, and not ascertained count as worse), the D102 imputation, and the IQCODE rule for respondents without cognitive tests (3.4 or higher: Dementia; above 3.0: MCI).

Weighted class shares in HRS-HCAP Wave 1:

|Classification                 |Normal, % |MCI, % |Dementia, % |Kappa, 3-class |Kappa, dementia |
|:------------------------------|:---------|:------|:-----------|:--------------|:---------------|
|Manly-Jones algorithm (target) |69.4      |21.4   |9.2         |               |                |
|Core, previous cuts (37, 48)   |71.0      |20.7   |8.3         |0.505          |0.353           |
|Core, new cuts (38.5, 46.5)    |69.6      |21.2   |9.1         |0.501          |0.337           |

Kappas compare the core algorithm with the Manly-Jones algorithm (3-class: quadratic weights). They are in-sample: the cut points were chosen in the same data.

## Differential classification, previous versus new export

A person-wave is counted as changed if its class differs, or if it is classified in one file only.

All respondents in the export:

| Year| Person-waves| Changed|% changed |
|----:|------------:|-------:|:---------|
| 2016|        20912|     892|4.3       |
| 2018|        17146|     676|3.9       |
| 2020|        15723|     578|3.7       |
| 2022|        13130|     478|3.6       |

HRS-HCAP Wave 1 participants (the incidence cohort):

| Year| Person-waves| Changed|% changed |
|----:|------------:|-------:|:---------|
| 2016|         3496|     174|5.0       |
| 2018|         2989|     117|3.9       |
| 2020|         2558|      84|3.3       |
| 2022|         2050|      86|4.2       |

Changes among HRS-HCAP Wave 1 participants, by direction and year:

|Transition      | 2016| 2018| 2020| 2022|
|:---------------|----:|----:|----:|----:|
|MCI to Dementia |   28|   25|   13|   18|
|MCI to Normal   |   49|   32|   21|   31|
|Normal to MCI   |   97|   60|   50|   37|

Check: 0 changed person-waves have a T score outside the two bands between the old and new cut points (expected 0 if the cut points are the only change).

## New export: classes among HRS-HCAP Wave 1 participants

Unweighted person-wave counts.

| Year| Normal| MCI| Dementia| Not classified|
|----:|------:|---:|--------:|--------------:|
| 2016|   2385| 767|      344|              0|
| 2018|   2135| 606|      247|              1|
| 2020|   1891| 466|      201|              0|
| 2022|   1468| 380|      201|              1|

## Source

Method details: Appendix 1, `R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd`. To compare any two exports: `Rscript R/A7_080-compare_dx_exports.R <old.csv> <new.csv>`.

