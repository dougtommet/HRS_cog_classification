# Dx_for_LK-2026-10-21.csv

Person-wave core algorithm classifications (`dx_v1`) for HRS core respondents, 2016-2022, written by `R/A7_075-implement_algorithm.R` (Analysis A7) on 2026-10-21. This README is written by `R/A7_085-export_readme.R` in the same run. It replaces `Dx_for_LK-2026-07-10.csv`.

## Columns

- `HHID`, `PN`: HRS household and person identifiers.
- `wave`: HRS core wave (P = 2016, Q = 2018, R = 2020, S = 2022).
- `TF`: demographically adjusted T score from the core cognitive items (mean 50, SD 10 in the HRS-HCAP robust normative sample), unrounded. Missing when no core cognitive tests were completed. The algorithm compares the rounded score TR = floor(TF + 0.5) with the cut points (see below).
- `jorm`: 16-item IQCODE mean score (1 to 5), collected in proxy interviews.
- `D102`: self-rated memory compared with 2 years ago (1 better, 2 same, 3 worse, 8 don't know, 9 refused, -8 not ascertained), singly imputed when missing.
- `dx_v1`: core algorithm classification (1 Normal, 2 MCI, 3 Dementia). Missing when neither `TF` nor `jorm` is available.

Rows are all HRS core respondents interviewed in each wave (all ages). The algorithm's norms apply to ages 65 and older; restrict to that age range for analysis.

## Cut points

Severe: TR < 39. Moderate: TR < 48. The July file used TF < 37 and TF < 48.

## How the cut points were selected

The rule was written and agreed (Rich Jones, Doug Tommet, Lindsay Kobayashi) on 2026-10-21, before any result was seen. It is in the header of `R/A7_070-threshold_calibration.R`.

1. **Score.** The T score is rounded to an integer (TR = floor(TF + 0.5)), and cut points are integers.
2. **Sample and reference.** HRS-HCAP Wave 1 participants with the HCAP sampling weight (HCAP16WGTR) and a Manly-Jones classification (vs1hcapdxeap), n = 3,496. All shares and kappas are weighted.
3. **Candidates.** Every integer pair with severe cut 25 to 50 and moderate cut 30 to 60 (moderate above severe), with every other part of the algorithm fixed.
4. **Eligibility.** The core algorithm's weighted dementia share is within 0.5 percentage points of the Manly-Jones dementia share, and its MCI share within 1.0 points of the Manly-Jones MCI share. If no pair qualified, the dementia tolerance was to widen to 1.0 point, never more, and then the MCI tolerance by 0.5 points at a time.
5. **Selection.** Among eligible pairs, the highest weighted kappa for dementia versus not dementia; ties go to the higher weighted 3-class kappa (quadratic weights), then the smaller MCI share difference, then the lower moderate cut.

Pairs met both tolerances as specified (dementia 0.5 points, MCI 1.0 points); 5 of 575 candidate pairs were eligible.

## Rationale

- **Matching prevalence first.** The file is used to estimate prevalence and incidence. Requiring the core algorithm to reproduce the weighted Manly-Jones dementia and MCI shares in HRS-HCAP Wave 1 ties its population estimates to the HCAP reference classification.
- **Dementia agreement second.** Several pairs can match the shares. Among them, the rule takes the pair that best agrees with Manly-Jones on dementia, the main outcome, and then on all three classes.
- **Integer T scores.** The T score is measured with error, so fractional cut points imply a precision the score does not have. Integer scores and cut points are also simpler to apply and report.
- **Fixed in advance.** The rule, tolerances, widening steps, and tie-breaks were fixed before the results were seen, so the cut points follow from a stated rule rather than from inspection. The earlier cut points (37, 48) have no documented derivation.

## Weighted class shares and agreement in HRS-HCAP Wave 1

|Classification                        |Normal, % |MCI, % |Dementia, % |Kappa, dementia |Kappa, 3-class |
|:-------------------------------------|:---------|:------|:-----------|:---------------|:--------------|
|Manly-Jones algorithm (reference)     |69.4      |21.4   |9.2         |                |               |
|July file: TF < 37, TF < 48           |71.0      |20.7   |8.3         |0.353           |0.505          |
|Previous cuts on TR: TR < 37, TR < 48 |72.2      |19.8   |7.9         |0.356           |0.510          |
|This file: TR < 39, TR < 48           |68.9      |22.0   |9.1         |0.337           |0.501          |

Kappas compare the core algorithm with the Manly-Jones algorithm (3-class: quadratic weights). They are in-sample: the cut points were selected in the same data, so they are optimistic.

Stability: the full rule, including the Manly-Jones shares, was repeated in 500 design-based bootstrap samples (SECU resampled within STRATUM (Rao-Wu, n_h - 1 draws), HCAP16WGTR). The same pair was selected in 8.0% of samples, and a pair within 1 T point of each selected cut in 49.8%. This describes how firm the cut points are; it did not affect the selection.

Why the same pair recurs in few bootstrap samples: the rule compares prevalence in whole T-score steps against limits of 0.5 and 1.0 percentage points. Next to the selected cut points, a 1-point change in the severe cut changed the weighted dementia prevalence by 0.5 percentage points, and a 1-point change in the moderate cut changed the weighted MCI prevalence by 0.9 points. In a new sample, the prevalence from both algorithms shifts by a fraction of a point, and that shift can move a pair into or out of the candidate set. The rule fixes one pair, but the data do not separate it from its neighbors.

## Unchanged from the July file

The factor model and item parameters, the norms and T-score formula, the self-rated memory rule (worse, don't know, refused, and not ascertained count as worse), the D102 imputation, and the IQCODE rule for respondents without cognitive tests (3.4 or higher: Dementia; above 3.0: MCI).

## Differential classification, July file versus this file

A person-wave is counted as changed if its class differs, or if it is classified in one file only.

All respondents in the export:

| Year| Person-waves| Changed|% changed |
|----:|------------:|-------:|:---------|
| 2016|        20912|     756|3.6       |
| 2018|        17146|     579|3.4       |
| 2020|        15723|     492|3.1       |
| 2022|        13130|     379|2.9       |

HRS-HCAP Wave 1 participants (the incidence cohort):

| Year| Person-waves| Changed|% changed |
|----:|------------:|-------:|:---------|
| 2016|         3496|     147|4.2       |
| 2018|         2989|      92|3.1       |
| 2020|         2558|      72|2.8       |
| 2022|         2050|      67|3.3       |

Changes among HRS-HCAP Wave 1 participants, by direction and year:

|Transition      | 2016| 2018| 2020| 2022|
|:---------------|----:|----:|----:|----:|
|MCI to Dementia |   28|   25|   13|   18|
|MCI to Normal   |   22|    7|    9|   12|
|Normal to MCI   |   97|   60|   50|   37|

Check: 0 changed person-waves have a T score outside the bands where the old rule (TF below an old cut) and the new rule (TR below a new cut) can differ (expected 0 if rounding and the cut points are the only changes).

## This file: classes among HRS-HCAP Wave 1 participants

Unweighted person-wave counts.

| Year| Normal| MCI| Dementia| Not classified|
|----:|------:|---:|--------:|--------------:|
| 2016|   2358| 794|      344|              0|
| 2018|   2110| 631|      247|              1|
| 2020|   1879| 478|      201|              0|
| 2022|   1449| 399|      201|              1|

## Source

Method details: Appendix 1, `R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd`. Weighted prevalence by wave, July file versus this file: Analysis A10 report. To compare any two exports: `Rscript R/A7_080-compare_dx_exports.R <old.csv> <new.csv>`.

