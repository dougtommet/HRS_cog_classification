# Analysis A3 and downstream PMM code review

Reviewed 2026-10-22 at commit 4b23a9b47549d329a38a13da7a504c036bfef09e.

## Main conclusion

The fixed base-rate logits are correct in the current scoring models. The large apparent shortfall in marginal impairment comes principally from assigning each person to the most likely class. The averages of the probabilities reproduce the reference rates closely; the modal assignments do not.

There is also a substantive implementation bug: scoring uses calibration estimates rounded to three decimals. An isolated scoring experiment confirms that this changes individual probabilities and some assignments. A separate probability-allocation bug distorts the agreement tables. Neither explains the large modal MCI deficit.

The model's overall marginal agreement should not be taken as proof that its probabilities are calibrated. Descriptive checks show appreciable underprediction in older respondents and respondents missing number series. The report's mathematical explanation of an MCI probability ceiling is also stronger than its calculations support.

This review changes no production analysis source, fitted model, classification export, or existing report. New review scripts and aggregate diagnostic artifacts accompany it.

## Scope and validation

Read References/instructions.md, README.md, References/SESSION_LOGS.md, References/decisions.md, and the relevant project background and extracted literature. Traced Analysis3_Driver.R through PMM preparation, normative regressions, calibration, fixed scoring, comparison tables, margins, and the MCI explanation. Reviewed the A3_2 scoring and reproduction check, A2 shared results and Appendix 2, A9 slides, A2_2 slides, and the archived cross-validation code that underpins the model-selection decision.

Examined Reports/PMM_Analysis_Report_2026-10-22.html and the current Mplus inputs, aggregate outputs, and calibration H5 estimates. Computed aggregate diagnostics from local derived analysis objects; no individual records are included in this review or its exported artifacts.

The read-only diagnostic script is tools/review_pmm_2026-10-22.R. The isolated precision experiment is tools/review_pmm_precision_2026-10-22.R. Their outputs are:

- Reports/PMM_Code_Review_diagnostics_2026-10-22.json
- Reports/PMM_Code_Review_precision_2026-10-22.json

The precision experiment performed two temporary cognition scoring runs with zero free parameters: one with the existing rounded parameters and one with calibration parameters recovered from H5. Both saved posterior probabilities to twelve decimals. It did not refit calibration models or overwrite production output. The audit did not perform new cross-validation, bootstrap uncertainty estimation, or a new model-specification search.

## What the marginal rates actually show

Percentages below use HCAP16WGTR in all 3,496 HRS/HCAP participants. Columns are Normal, MCI, and Dementia.

| Quantity | Normal | MCI | Dementia |
|:--|--:|--:|--:|
| Manly-Jones reference classification | 69.392 | 21.393 | 9.215 |
| Average PMM posterior probability | 69.651 | 21.221 | 9.130 |
| Existing 100-record probability allocation | 69.178 | 21.236 | 9.586 |
| Most likely PMM class | 85.535 | 8.627 | 5.839 |

The posterior marginal errors are +0.259, -0.172, and -0.085 percentage points. Modal MCI is 12.767 points below the reference; modal dementia is 3.376 points below it. These are different estimands.

For class k, posterior prevalence is the weighted average of p(ik). Modal prevalence is the weighted average of the indicator that k is the largest probability. A person with probabilities 0.45 Normal, 0.40 MCI, and 0.15 Dementia contributes 0.40 to expected MCI membership but zero to modal MCI membership. Correct priors do not make those two operations equivalent.

PMM_112_Margins.R, lines 155-167, deliberately tabulates the modal class; its report caption says "Most likely PMM class membership." That calculation is consistent with its label. The problem for assessing marginal reproduction is that the A3 margins section does not put averaged probabilities and the reference rates alongside it. A2 Table 2 also uses modal PMM assignments. A9 already computes both probability and modal margins.

Recommendation: display reference rates, posterior means, and modal rates together, label each quantity, and use posterior means for probabilistic population prevalence. Keep modal results when individual assignment is the question. Do not change the priors merely to force modal percentages to agree with the reference.

## Findings requiring correction

### 1. [P1] Fixed scoring does not preserve calibration parameter precision

Location: R/PMM_101_mplus_function.R, lines 6-21; called at R/PMM_102_calibration_model.R, line 101. The informant model and archived CV use the same writer. A3_2 copies its generated model.

write_lca_model() takes the unstandardized estimates parsed from the text output. Those estimates are rounded to three decimals. It then fixes the scoring model at those printed values. This is materially coarse for continuous indicators that were rescaled to small numeric ranges.

| Cognition residual variance | Calibration H5 estimate | Scoring value | Relative change |
|:--|--:|--:|--:|
| Animal naming | 0.0044592819 | 0.004000 | -10.30% |
| Delayed recall | 0.0255906694 | 0.026000 | +1.60% |
| Number series | 0.0168971270 | 0.017000 | +0.61% |

An isolated full-precision scoring run changed a person's class probability by as much as 0.054396 and changed 15 cognition-model modal assignments, representing 0.538% of cognition-model survey weight.

The effect on aggregate cognition probabilities was small:

| Cognition scoring parameters | Normal | MCI | Dementia |
|:--|--:|--:|--:|
| Current printed estimates | 72.16396 | 20.52161 | 7.31443 |
| Full-precision H5 estimates | 72.20805 | 20.50107 | 7.29089 |

Thus this is a real scoring defect and matters for individual probabilities, but it is not the explanation for modal MCI near 8%.

Recommended correction: obtain calibration parameters from a sufficiently precise machine-readable result, write enough digits, and verify fixed scoring parameters against that result. Checking text-output estimates against text-output estimates will miss this problem: all 681 class-specific cognition parameter rows and all 6 informant rows match at printed precision in the current files.

Also save posterior probabilities at higher precision. Current saved probabilities have row sums from 0.999 to 1.001 because each component is rounded to three decimals. Normalizing the existing rows barely changes the aggregate findings here; it cannot recover the lost precision.

### 2. [P2] Deterministic 100-record allocation systematically shifts probability mass

Location: R/PMM_110_Comparison_of_Consensus_Langa_Weir.R, lines 233-269.

Replicates are indexed 1 through 100 and assigned using i <= 100*p1 and i <= 100*(p1+p2). This floors the first cumulative probability and the second cumulative probability. Normal loses its fractional remainder; Dementia receives the second remainder. This is systematic discretization, not an unbiased probability allocation.

For probabilities 0.906, 0.011, and 0.083, the code allocates 90/1/9 records: 0.900, 0.010, and 0.090. In the actual full sample, Normal falls by 0.473 percentage points and Dementia rises by 0.456 points relative to the saved posterior means.

Direct probability-weighted cross-tables give the following kappas. The direct calculation below normalizes the three saved probabilities to sum to one; the published calculation includes its existing rounding of table cells.

| Comparator | Published 3-class | Direct 3-class | Published dementia | Direct dementia |
|:--|--:|--:|--:|--:|
| Manly-Jones | 0.36981 | 0.37430 | 0.30239 | 0.30899 |
| Langa-Weir | 0.42548 | 0.43180 | 0.34794 | 0.35660 |
| Consensus, n=50 | 0.13811 | 0.13692 | 0.24907 | 0.25859 |

Recommended correction: compute each cross-table cell as the sum of weight times class probability among respondents in the comparator category. This is exact conditional on the saved probabilities and requires no replication. Compute kappa before display rounding. The archived CV script already uses direct probability allocation.

This affects the A3 agreement report, A2 PMM agreement results including PMM-Hudomiet, and slides that consume those results. It does not cause the modal-margin deficit.

### 3. [P2] The reported MCI measurement-error ceiling is not a bound for the fitted model

Location: R/PMM_114_mci_ceiling.R, lines 36-47 and 62-64; R/PMM_102_calibration_model.qmd, lines 142-164; R/Slides2603_040_Results.qmd, lines 106-111.

The calculation replaces the eight mixed continuous/ordinal tests by an additive Gaussian-error surrogate for GCP. It constructs the error variance from an omega-like calculation based on standardized loadings in the Normal class. That does not reproduce the actual joint response likelihood, its pattern-dependent information, or the full scoring model.

The calculation also omits PD102, whose thresholds differ by class and therefore supply a separate class likelihood. With GCP known exactly, the audit obtains an MCI maximum of about 0.596 ignoring PD102 and about 0.604 conditioning on PD102="worse." These are different calculations. The report's approximate noisy-score ceiling is about 0.46, but the current actual model produces an MCI probability of 0.506 for its highest-scoring participant and has two probabilities above 0.50.

The heading "Why no participant has an MCI probability above one half" is false for the supplied report. The inline counts update, but the heading and explanatory claim do not. The agreement between an approximate illustration and the broad shape of the scores does not establish that the model is correctly specified.

Recommended correction: label the surrogate as an illustration; do not call 0.46 the largest possible probability for the scoring model or say the calculation rules out errors. If a model-specific bound is desired, derive it with the PD102 likelihood and the actual invariant measurement assumptions. Keep the empirical maximum and class-probability summaries.

### 4. [P2] Model-selection eligibility ignores a recorded failed quality check

Location: R/Archive_PMM_2026-10-22/PMM_106_cross_validation.R, lines 240-253 and 384-429.

The saved change1 fold_05 run has both cal_se_untrustworthy=TRUE and cal_not_replicated=TRUE. Nevertheless, terminated_normally is TRUE, and all_folds_ok checks only that field. The decision is therefore evaluated as if all folds were acceptable despite its own recorded convergence-quality flags.

Recommended correction: distinguish successful termination from an acceptable fit; repair or rerun flagged folds before treating the adoption rule as settled. This does not establish that change1 should be adopted. The recorded CV log score is worse and Brier score is also worse. It means the quality gate should be enforced and the comparison reconsidered after correcting scoring precision.

Other CV qualifications:

- Normative regression coefficients are estimated once using the full robust-norms sample and held fixed across folds. This evaluates the classifier conditional on that norms model, not a wholly held-out end-to-end pipeline. Holding an externally fixed norms model is defensible, but the performance claim must match that target.
- Folds are stratified by class at the respondent level, not by household or survey cluster. Their interpretation is prediction for similar respondents within the sampled structure.
- Log scores use three-decimal probabilities and replace zeros by 1e-12. Each variant has one held-out true-class probability recorded as zero. Replacing only those zeros by 0.0005 changes both weighted log scores by the same 0.00546, leaving their difference unchanged. This particular sensitivity does not reverse the recorded decision, but high-precision probabilities are still needed for proper scoring.

## Statistical model concerns that remain after those corrections

### Overall marginal matching hides subgroup miscalibration

The following descriptive results concern cognition-model participants only. Values are weighted percentages, computed using the current scoring model.

| Subgroup | N | Reference MCI | Predicted MCI | Reference dementia | Predicted dementia |
|:--|--:|--:|--:|--:|--:|
| Age 65-74 | 1,517 | 18.51 | 19.32 | 3.96 | 5.61 |
| Age 75-84 | 1,335 | 22.36 | 21.58 | 9.63 | 8.21 |
| Age 85+ | 431 | 28.17 | 24.33 | 19.39 | 14.32 |
| Education <12 years | 643 | 27.22 | 23.65 | 12.35 | 9.79 |
| Number series observed | 2,877 | 19.20 | 19.70 | 5.33 | 5.95 |
| Number series missing | 406 | 33.73 | 27.76 | 24.60 | 19.37 |

These are in-sample diagnostic discrepancies, not significance tests. No uncertainty intervals were estimated. They are large enough to make "the probabilities reproduce the marginal rates" an incomplete validation claim.

There are several plausible mechanisms to examine, without assuming one has been established:

- The model adjusts item responses for demographics but uses constant class priors within each route. It does not model class prevalence conditional on age, schooling, or missingness pattern. Normative adjustment and probability calibration across demographic groups are distinct objectives.
- All eight tests distinguish class only through GCP. Class-invariant loadings, thresholds, intercepts, and residual variances constrain the class-specific test profiles to a single latent direction, apart from PD102. This can lose information about uneven domain impairment even when a factor improves the within-class dependence model.
- Missing-data likelihood calculations use available responses under assumptions about ignorability. They do not automatically make a missingness-pattern subgroup representative of the entire cognition route. The markedly higher reference impairment among those missing number series makes that assumption worth checking.
- GCP variances differ by class. The model is more flexible than equal-variance discriminant analysis but retains strong measurement invariance and one-factor restrictions.
- The informant model uses one bounded, potentially heaped IQCODE score as Gaussian with equal class variances. Its posterior MCI rate is 31.21% versus a 31.62% reference, and posterior dementia is 35.09% versus 36.69%. Its small sample and low entropy deserve separate assessment.

A good overall marginal match is compatible with both a useful classifier and meaningful subgroup error. As a positive check, the existing pooled CV cognition predictions have multiclass Brier score 0.35758 versus 0.43313 for a constant pooled-prior baseline. Thus the model contains predictive information; it is not merely returning the priors. That comparison is descriptive and conditional on the existing CV implementation.

Before considering another model, correct scoring precision and allocation, then assess probability calibration by class, probability range, age, education, and missingness pattern using held-out predictions and design-aware uncertainty. Residual/local-dependence checks and targeted measurement-invariance checks would address the factor specification more directly than a search for priors that make modal counts agree.

The project decision to retain the current factor model is documented. This review identifies reasons to qualify its validation claims; it does not silently reverse that decision or reopen model searching.

### Kappa based on probabilities and kappa based on assigned classes answer different questions

With the current cognition-plus-informant scores, modal agreement against Manly-Jones is about 0.50557 for three classes and 0.43644 for dementia. Direct probability allocation gives about 0.37430 and 0.30899.

Neither number is a substitute for the other. Probability allocation evaluates a fractional expected cross-table; modal kappa evaluates the deterministic assignment rule. A2 compares PMM probability-allocation kappa with core-algorithm hard-class kappa, while its margins table shows hard PMM classes. The individual computations are identifiable, but the presentation invites readers to treat them as one classification procedure.

Recommendation: report both agreement versions explicitly and assess probabilities with proper scores and calibration. Do not interpret lower probability-allocation kappa alone as a defect, or the higher modal kappa as evidence that population prevalence is reproduced.

## Checks that passed, and safeguards still worth adding

- Both current scoring models report zero free parameters. The logits use Normal=1, MCI=2, Dementia=3, with Dementia as the reference.
- Cognition logits are 2.2898740 and 1.0419327 and recover weighted priors 72.02668%, 20.67852%, and 7.29481%. Informant priors are 31.68644%, 31.62194%, and 36.69162%. The weights and the last-class denominator are correct for the current samples.
- There are 3,283 cognition cases and 213 informant cases, zero overlap, no duplicate output IDs, and all 3,496 HCAP cases have probabilities. The recent full joins solve the dropped-informant problem for this run.
- The Normal GCP mean is fixed at zero and its variance at one. The previous free-location identification defect is repaired. Current calibration terminates normally; the information-matrix condition number is about 2.57e-6. Numerical termination does not establish substantive fit.
- Continuous residual covariances are explicitly zero in the final cognition model, as documented. Earlier normative covariance statements are overridden. This is deliberate in the present model, not an unnoticed surviving no-factor-model defect.
- The A3 and A3_2 cognition MODEL sections are identical after whitespace normalization.
- The saved A3_2 reproduction check matches 3,470 of 3,496 HCAP participants. Within the matched sample it reports identical routing and modal assignments; the largest probability discrepancy is 0.032. Two education inputs differ, by up to three schooling years. The other checked inputs agree. The two upstream education-imputation pipelines should be reconciled or their small discrepancy explicitly accepted.
- A3_2 has six person-waves without either model input across the four waves, and zero person-waves with both routes' inputs. These cases are retained as unscored in the current implementation.

Recommended safeguards, rather than explanations of the present discrepancy:

- Require exactly the expected class codes 1,2,3 and finite positive weights in weighted_class_logits(). Its current "at least two classes" check would accept a missing class and write logits for the wrong class layout.
- check_prior_sample() checks row count only. Validate the actual eligible ID set, class availability, covariate completeness, and weight eligibility; add the corresponding informant check.
- Assert zero free parameters after every scoring run. A3_2's input-text check only looks for c#1 @, which is insufficient to certify that the entire model is fixed.
- Enforce unique IDs, routing exclusivity or a documented overlap rule, full coverage, valid probabilities, and explicit unscored denominators before joins and summaries.
- Make the HCAP reproduction check enforce documented tolerances, not merely write a table. Its inner joins currently describe matched participants rather than validating full intended coverage.
- Keep full precision through computation and scale weighted population counts to thousands only at display. PMM_110 currently rounds cells before computing kappa and displays full-unit population counts, contrary to project reporting rules.

## Recommended sequence

1. Preserve the current fit and replace rounded parameter transfer with full-precision transfer. Increase posterior output precision. Confirm zero free parameters and parameter equality against machine-readable calibration results.
2. Replace 100-record allocation with direct weighted probability tables; compute statistics before rounding.
3. Add the joint reference/posterior/modal margins table to A3 and A2; distinguish both kinds of agreement statistics.
4. Correct the ceiling explanation and its false fixed heading in A3 and consuming slides.
5. Reassess the subgroup probability discrepancies and the flagged CV fold. Use uncertainty estimates before making inferential claims. Decide separately whether fixed HCAP priors are the desired scoring convention or an appropriate prevalence model for later waves.
6. Rebuild downstream outputs only after those changes are settled.

Authoritative rebuild order:

- Rscript Analysis3_Driver.R -> Reports/PMM_Analysis_Report_<run-date>.html, refreshed PMM derived objects and Mplus outputs.
- Rscript Analysis3_2_Driver.R -> Reports/A3_2_PMM_scoring_<run-date>.html and R_objects/A3_2_pmm_probs_hrs16_22.rds/.dta.
- Rscript Slides2603_Driver.R -> Reports/Slides2603_<run-date>.html and its exported figures.
- Rscript Analysis2_Driver.R -> manuscript, tables/figures, appendices, and combined PDF in Reports/.
- Rscript Analysis2_2_Driver.R -> Reports/Slides_A2summary_<run-date>.html; run after A9 because it uses the exported probability figure.

No rebuild of those production workflows was performed for this review.

## Source grounding

The numerical findings are reproducible from the two review scripts and their aggregate JSON outputs. The supplied analysis report is the reference for published margins and kappas.

Mplus explicitly distinguishes estimated-model class proportions, posterior probabilities, and most likely membership; when sampling weights are used, its output "counts" for most likely membership are weighted counts scaled to sample size, not raw record counts. This explains why a raw count calculated from saved membership can differ from the count printed in the Mplus summary: [Mplus class-count FAQ](https://www.statmodel.com/download/FAQ-class-counts.pdf).

The default saved-data format is F10.3, and CPROBABILITIES saves the posterior probabilities and most likely membership: [Mplus OUTPUT and SAVEDATA documentation](https://www.statmodel.com/HTML_UG/chapter18V8.htm).

Transport to a new population deserves a separate distinction: fixing the measurement model does not by itself establish that the original priors remain appropriate. The local Saerens, Latinne, and Decaestecker extraction states that target posterior probabilities depend on the target class priors (Internal: References/llm_out/Saerens_2001.txt, PDF pages 1-2). A3_2's fixed-prior convention is an explicit project decision, not a coding error; it needs qualification if interpreted as calibrated prevalence across 2016-2022.
