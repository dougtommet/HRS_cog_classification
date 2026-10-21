# Archived: prevalence-matching grid search for the core algorithm cut points (2026-10-20)

Saved so the grid search can be restored after Rich talks to Doug Tommet about how T < 37 and T < 48 were chosen. Nothing here is run by any driver. Do not source these files: they use `here::here()` paths and would overwrite current objects in `R_objects/`.

| File | What it is |
|:--|:--|
| `A7_070-threshold_calibration.R` | A7_070 as of 2026-10-20 evening: grid search (severe 25-50, moderate 30-60, step 0.5; loss = sum of squared differences from weighted Manly-Jones shares, ties to higher weighted 3-class kappa), grid position of 37/48, kappa-maximizing pair, and the descriptive `domain_quantiles` element. Still identical to the live file at archiving time. |
| `A7_070-threshold_calibration.qmd` | Its A7 report chapter. |
| `MS_020_appendix1_objects_with_grid_table.R` | MS_020 before the grid was removed from Appendix 1: `a1_opt`, `a1_maxk`, `a1_gpos`, and Table A2 "T-score cut points and weighted class shares" (cut points used, grid closest-share pair, grid highest-kappa pair, target). |
| `MS_Appendix-1_2026-10-19_version_grid_as_check.qmd` | Appendix 1 text as rendered 2026-10-19 (cut points in Step 3 with the grid as a check; rules in Step 4). |
| `MS_Appendix-1_2026-10-20_rules_first_with_grid.qmd` | Appendix 1 after the first 2026-10-20 reorganization (rules in Step 3, cut points by prevalence matching in Step 4, grid as a check). |

Grid results last rendered (2026-10-19): closest-share pair T < 38.5 and T < 46.5 (69.6/21.2/9.1% vs Manly-Jones 69.4/21.4/9.2%); 37/48 ranked 54 of 2,250 pairs, largest share difference 1.6 points; highest 3-class kappa 0.52 at T < 36 and T < 42.5. See References/SESSION_LOGS.md, 2026-10-17 to 2026-10-20.
