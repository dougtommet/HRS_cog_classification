# Analysis A7: README for the person-wave dx_v1 export to Lindsay Kobayashi.
#
# Sourced at the end of A7_075. Describes the cut points, how they were
# selected, and why; compares the new export (dx_export_file) with the July
# 2026 file she holds (dx_export_previous); writes
# Data/<export name without .csv>_README.md. Every number is computed here.
# Restored 2026-10-21 from git (8fe5469) and rewritten for the selection rule.

readme_path <- here::here("Data", stringr::str_replace(dx_export_file, "\\.csv$", "_README.md"))

read_export <- function(file) {
  readr::read_csv(here::here("Data", file),
                  col_types = readr::cols(HHID = "c", PN = "c", wave = "c", .default = "d"))
}

new_exp  <- read_export(dx_export_file)
old_path <- here::here("Data", dx_export_previous)
has_old  <- file.exists(old_path)

wave_year <- c(P = 2016L, Q = 2018L, R = 2020L, S = 2022L)
dx_lab <- function(x) factor(x, levels = 1:3, labels = c("Normal", "MCI", "Dementia"))
md   <- function(df) paste(knitr::kable(df, format = "pipe"), collapse = "\n")
pct1 <- function(x) formatC(100 * x, format = "f", digits = 1)
pts1 <- function(x) formatC(100 * x, format = "f", digits = 1)
k3   <- function(x) formatC(x, format = "f", digits = 3)

cal  <- A7_070_threshold_calibration
spec <- cal$spec
prev_cuts <- cal$previous_cuts
new_cuts  <- cal$cuts
cmp  <- cal$comparison
row_sel  <- cmp %>% filter(set == "Selected cut points (TR)")
row_prTR <- cmp %>% filter(set == "Previous cut points (TR)")
row_prTF <- cmp %>% filter(set == "Previous cut points (TF, July 2026 file)")

hcap_keys <- hrs16_hcap %>%
  transmute(HHID = as.character(HHID), PN = as.character(PN), hcap = 1L)

# New export: counts by wave and class, HRS-HCAP Wave 1 cohort ----------------
new_counts <- new_exp %>%
  inner_join(hcap_keys, by = c("HHID", "PN")) %>%
  mutate(Year = wave_year[wave], Class = dx_lab(dx_v1)) %>%
  count(Year, Class) %>%
  mutate(Class = forcats::fct_na_value_to_level(Class, "Not classified")) %>%
  tidyr::pivot_wider(names_from = Class, values_from = n, values_fill = 0)

# Comparison with the July file ------------------------------------------------
# Old rule: TF < old cut. New rule: TR < new cut, i.e. TF < new cut - 0.5.
# A class can change only if TF lies between the two for some cut.
band <- function(tf, c_old, c_new) {
  lo <- min(c_old, c_new - 0.5); hi <- max(c_old, c_new - 0.5)
  tf >= lo & tf < hi
}
if (has_old) {
  old_exp <- read_export(dx_export_previous)
  both <- full_join(
    old_exp %>% select(HHID, PN, wave, dx_old = dx_v1),
    new_exp %>% select(HHID, PN, wave, TF, dx_new = dx_v1),
    by = c("HHID", "PN", "wave")
  ) %>%
    left_join(hcap_keys, by = c("HHID", "PN")) %>%
    mutate(
      hcap = coalesce(hcap, 0L),
      changed = coalesce(dx_old != dx_new, is.na(dx_old) != is.na(dx_new)),
      in_band = band(TF, prev_cuts[["severe"]],   new_cuts[["severe"]]) |
                band(TF, prev_cuts[["moderate"]], new_cuts[["moderate"]])
    )

  change_summary <- function(d) {
    d %>%
      group_by(Year = wave_year[wave]) %>%
      summarise(`Person-waves` = n(),
                Changed = sum(changed),
                `% changed` = pct1(mean(changed)),
                .groups = "drop")
  }
  chg_all  <- change_summary(both)
  chg_hcap <- change_summary(both %>% filter(hcap == 1L))
  n_unexplained <- sum(both$changed & !coalesce(both$in_band, FALSE))

  transitions <- both %>%
    filter(hcap == 1L, changed) %>%
    mutate(Year = wave_year[wave],
           Transition = paste(dx_lab(dx_old), "to", dx_lab(dx_new))) %>%
    count(Year, Transition) %>%
    tidyr::pivot_wider(names_from = Year, values_from = n, values_fill = 0)
}

# Text pieces ------------------------------------------------------------------
tol_text <- if (cal$selection_stage == "as specified") {
  paste0("Pairs met both tolerances as specified (dementia ", pts1(spec$tol_dementia),
         " points, MCI ", pts1(spec$tol_mci), " points); ", nrow(cal$eligible),
         " of ", nrow(cal$candidates), " candidate pairs were eligible.")
} else {
  paste0("No pair met the tolerances as specified, so the pre-specified widening was applied (",
         cal$selection_stage, "). Tolerances used: dementia ", pts1(cal$tol_dementia_used),
         " points, MCI ", pts1(cal$tol_mci_used), " points; ", nrow(cal$eligible),
         " of ", nrow(cal$candidates), " candidate pairs were eligible.")
}
boot_text <- if (is.null(cal$bootstrap)) {
  "The bootstrap check of stability was skipped (survey design variables not found)."
} else {
  paste0("Stability: the full rule, including the Manly-Jones shares, was repeated in ",
         cal$bootstrap$n_boot, " design-based bootstrap samples (", cal$bootstrap$design,
         "). The same pair was selected in ", pct1(cal$bootstrap$pct_same_pair),
         "% of samples, and a pair within 1 T point of each selected cut in ",
         pct1(cal$bootstrap$pct_within_1), "%", if (cal$bootstrap$n_rule_not_met > 0) paste0("; in ", cal$bootstrap$n_rule_not_met, " samples no severe cut point brought the dementia share within 1.0 point, so no pair was selected") else "", ". This describes how firm the cut points are; it did not affect the selection.")
}

# Why the bootstrap rarely repeats the pair (Rich, 2026-10-21): average change
# in weighted prevalence per 1-point step next to the selected cuts.
step_near <- function(var, by, fixed_var, fixed_val, at) {
  x <- cal$candidates[cal$candidates[[fixed_var]] == fixed_val, ]
  v <- x[[var]][match(at + c(-1, 0, 1), x[[by]])]
  mean(abs(diff(v)), na.rm = TRUE)
}
step_dem <- step_near("share_dementia", "cut_severe", "cut_moderate", new_cuts[["moderate"]], new_cuts[["severe"]])
step_mci <- step_near("share_mci", "cut_moderate", "cut_severe", new_cuts[["severe"]], new_cuts[["moderate"]])
replicate_text <- if (is.null(cal$bootstrap)) "" else paste0(
  "Why the same pair recurs in few bootstrap samples: the rule compares prevalence in whole T-score steps against limits of ",
  pts1(spec$tol_dementia), " and ", pts1(spec$tol_mci), " percentage points. Next to the selected cut points, a 1-point change in the severe cut changed the weighted dementia prevalence by ",
  pts1(step_dem), " percentage points, and a 1-point change in the moderate cut changed the weighted MCI prevalence by ", pts1(step_mci),
  " points. In a new sample, the prevalence from both algorithms shifts by a fraction of a point, and that shift can move a pair into or out of the candidate set. The rule fixes one pair, but the data do not separate it from its neighbors.")

txt <- c(
  paste0("# ", dx_export_file),
  "",
  paste0("Person-wave core algorithm classifications (`dx_v1`) for HRS core respondents, 2016-2022, ",
         "written by `R/A7_075-implement_algorithm.R` (Analysis A7) on ", Sys.Date(), ". ",
         "This README is written by `R/A7_085-export_readme.R` in the same run. ",
         "It replaces `", dx_export_previous, "`."),
  "",
  "## Columns",
  "",
  "- `HHID`, `PN`: HRS household and person identifiers.",
  "- `wave`: HRS core wave (P = 2016, Q = 2018, R = 2020, S = 2022).",
  "- `TF`: demographically adjusted T score from the core cognitive items (mean 50, SD 10 in the HRS-HCAP robust normative sample), unrounded. Missing when no core cognitive tests were completed. The algorithm compares the rounded score TR = floor(TF + 0.5) with the cut points (see below).",
  "- `jorm`: 16-item IQCODE mean score (1 to 5), collected in proxy interviews.",
  "- `D102`: self-rated memory compared with 2 years ago (1 better, 2 same, 3 worse, 8 don't know, 9 refused, -8 not ascertained), singly imputed when missing.",
  "- `dx_v1`: core algorithm classification (1 Normal, 2 MCI, 3 Dementia). Missing when neither `TF` nor `jorm` is available.",
  "",
  "Rows are all HRS core respondents interviewed in each wave (all ages). The algorithm's norms apply to ages 65 and older; restrict to that age range for analysis.",
  "",
  "## Cut points",
  "",
  paste0("Severe: TR < ", new_cuts[["severe"]], ". Moderate: TR < ", new_cuts[["moderate"]],
         ". The July file used TF < ", prev_cuts[["severe"]], " and TF < ", prev_cuts[["moderate"]], "."),
  "",
  "## How the cut points were selected",
  "",
  paste0("The rule was written and agreed (Rich Jones, Doug Tommet, Lindsay Kobayashi) on 2026-10-21, before any result was seen. ",
         "It is in the header of `R/A7_070-threshold_calibration.R`."),
  "",
  paste0("1. **Score.** The T score is rounded to an integer (TR = floor(TF + 0.5)), and cut points are integers."),
  paste0("2. **Sample and reference.** HRS-HCAP Wave 1 participants with the HCAP sampling weight (", cal$weight,
         ") and a Manly-Jones classification (", cal$target_variable, "), n = ", format(cal$n_sample, big.mark = ","),
         ". All shares and kappas are weighted."),
  paste0("3. **Candidates.** Every integer pair with severe cut ", min(spec$severe_range), " to ", max(spec$severe_range),
         " and moderate cut ", min(spec$moderate_range), " to ", max(spec$moderate_range), " (moderate above severe), with every other part of the algorithm fixed."),
  paste0("4. **Eligibility.** The core algorithm's weighted dementia share is within ", pts1(spec$tol_dementia),
         " percentage points of the Manly-Jones dementia share, and its MCI share within ", pts1(spec$tol_mci),
         " points of the Manly-Jones MCI share. If no pair qualified, the dementia tolerance was to widen to 1.0 point, never more, and then the MCI tolerance by 0.5 points at a time."),
  paste0("5. **Selection.** Among eligible pairs, the highest weighted kappa for dementia versus not dementia; ties go to the higher weighted 3-class kappa (quadratic weights), then the smaller MCI share difference, then the lower moderate cut."),
  "",
  tol_text,
  "",
  "## Rationale",
  "",
  paste0("- **Matching prevalence first.** The file is used to estimate prevalence and incidence. Requiring the core algorithm to reproduce the weighted Manly-Jones dementia and MCI shares in HRS-HCAP Wave 1 ties its population estimates to the HCAP reference classification."),
  paste0("- **Dementia agreement second.** Several pairs can match the shares. Among them, the rule takes the pair that best agrees with Manly-Jones on dementia, the main outcome, and then on all three classes."),
  paste0("- **Integer T scores.** The T score is measured with error, so fractional cut points imply a precision the score does not have. Integer scores and cut points are also simpler to apply and report."),
  paste0("- **Fixed in advance.** The rule, tolerances, widening steps, and tie-breaks were fixed before the results were seen, so the cut points follow from a stated rule rather than from inspection. The earlier cut points (37, 48) have no documented derivation."),
  "",
  "## Weighted class shares and agreement in HRS-HCAP Wave 1",
  "",
  md(tibble::tibble(
    Classification = c("Manly-Jones algorithm (reference)",
                       paste0("July file: TF < ", prev_cuts[["severe"]], ", TF < ", prev_cuts[["moderate"]]),
                       paste0("Previous cuts on TR: TR < ", prev_cuts[["severe"]], ", TR < ", prev_cuts[["moderate"]]),
                       paste0("This file: TR < ", new_cuts[["severe"]], ", TR < ", new_cuts[["moderate"]])),
    `Normal, %`   = pct1(c(cal$target_shares[["1"]], row_prTF$share_normal, row_prTR$share_normal, row_sel$share_normal)),
    `MCI, %`      = pct1(c(cal$target_shares[["2"]], row_prTF$share_mci, row_prTR$share_mci, row_sel$share_mci)),
    `Dementia, %` = pct1(c(cal$target_shares[["3"]], row_prTF$share_dementia, row_prTR$share_dementia, row_sel$share_dementia)),
    `Kappa, dementia` = c("", k3(row_prTF$kappa_binary), k3(row_prTR$kappa_binary), k3(row_sel$kappa_binary)),
    `Kappa, 3-class`  = c("", k3(row_prTF$kappa_3class), k3(row_prTR$kappa_3class), k3(row_sel$kappa_3class))
  )),
  "",
  "Kappas compare the core algorithm with the Manly-Jones algorithm (3-class: quadratic weights). They are in-sample: the cut points were selected in the same data, so they are optimistic.",
  "",
  boot_text,
  "",
  replicate_text,
  "",
  "## Unchanged from the July file",
  "",
  paste0("The factor model and item parameters, the norms and T-score formula, the self-rated memory rule (worse, don't know, refused, and not ascertained count as worse), ",
         "the D102 imputation, and the IQCODE rule for respondents without cognitive tests (", cal$jorm_cuts[["severe"]], " or higher: Dementia; above ",
         formatC(cal$jorm_cuts[["mild"]], format = "f", digits = 1), ": MCI)."),
  ""
)

if (has_old) {
  txt <- c(txt,
    "## Differential classification, July file versus this file",
    "",
    "A person-wave is counted as changed if its class differs, or if it is classified in one file only.",
    "",
    "All respondents in the export:",
    "",
    md(chg_all),
    "",
    "HRS-HCAP Wave 1 participants (the incidence cohort):",
    "",
    md(chg_hcap),
    "",
    "Changes among HRS-HCAP Wave 1 participants, by direction and year:",
    "",
    md(transitions),
    "",
    paste0("Check: ", n_unexplained, " changed person-waves have a T score outside the bands where the old rule (TF below an old cut) and the new rule (TR below a new cut) can differ ",
           "(expected 0 if rounding and the cut points are the only changes)."),
    ""
  )
} else {
  txt <- c(txt, paste0("`", dx_export_previous, "` was not found in Data/, so no comparison is reported."), "")
}

txt <- c(txt,
  "## This file: classes among HRS-HCAP Wave 1 participants",
  "",
  "Unweighted person-wave counts.",
  "",
  md(new_counts),
  "",
  "## Source",
  "",
  "Method details: Appendix 1, `R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd`. Weighted prevalence by wave, July file versus this file: Analysis A10 report. To compare any two exports: `Rscript R/A7_080-compare_dx_exports.R <old.csv> <new.csv>`.",
  ""
)

writeLines(txt, readme_path)
