# Analysis A7: README for the person-wave dx_v1 export.
#
# Sourced at the end of A7_075. Compares the new export (dx_export_file) with
# the previous one (dx_export_previous) and writes
# Data/<export name without .csv>_README.md. Every number in the README is
# computed here.

readme_path <- here::here("Data", stringr::str_replace(dx_export_file, "\\.csv$", "_README.md"))

read_export <- function(file) {
  readr::read_csv(here::here("Data", file),
                  col_types = readr::cols(HHID = "c", PN = "c", wave = "c", .default = "d"))
}

new_exp <- read_export(dx_export_file)
old_path <- here::here("Data", dx_export_previous)
has_old <- file.exists(old_path)
old_mtime <- if (has_old) format(file.info(old_path)$mtime, "%Y-%m-%d") else NA_character_

wave_year <- c(P = 2016L, Q = 2018L, R = 2020L, S = 2022L)
dx_lab <- function(x) factor(x, levels = 1:3, labels = c("Normal", "MCI", "Dementia"))
md <- function(df) paste(knitr::kable(df, format = "pipe"), collapse = "\n")
pct1 <- function(x) formatC(100 * x, format = "f", digits = 1)

cal <- A7_070_threshold_calibration
prev_cuts <- cal$previous_cuts
new_cuts  <- cal$cuts

hcap_keys <- hrs16_hcap %>%
  transmute(HHID = as.character(HHID), PN = as.character(PN), hcap = 1L)

# New export: counts by wave and class, HRS-HCAP Wave 1 cohort ----------------
new_counts <- new_exp %>%
  inner_join(hcap_keys, by = c("HHID", "PN")) %>%
  mutate(Year = wave_year[wave], Class = dx_lab(dx_v1)) %>%
  count(Year, Class) %>%
  mutate(Class = forcats::fct_na_value_to_level(Class, "Not classified")) %>%
  tidyr::pivot_wider(names_from = Class, values_from = n, values_fill = 0)

# Comparison with the previous export ------------------------------------------
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
      in_band = (TF >= min(prev_cuts[["severe"]], new_cuts[["severe"]]) &
                   TF <  max(prev_cuts[["severe"]], new_cuts[["severe"]])) |
                (TF >= min(prev_cuts[["moderate"]], new_cuts[["moderate"]]) &
                   TF <  max(prev_cuts[["moderate"]], new_cuts[["moderate"]]))
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

# Write ------------------------------------------------------------------------
txt <- c(
  paste0("# ", dx_export_file),
  "",
  paste0("Person-wave core algorithm classifications (`dx_v1`) for HRS core respondents, 2016-2022, ",
         "written by `R/A7_075-implement_algorithm.R` (Analysis A7) on ", Sys.Date(), ". ",
         "This README is written by `R/A7_085-export_readme.R` in the same run."),
  "",
  "## Columns",
  "",
  "- `HHID`, `PN`: HRS household and person identifiers.",
  "- `wave`: HRS core wave (P = 2016, Q = 2018, R = 2020, S = 2022).",
  "- `TF`: demographically adjusted T score from the core cognitive items (mean 50, SD 10 in the HRS-HCAP robust normative sample). Missing when no core cognitive tests were completed.",
  "- `jorm`: 16-item IQCODE mean score (1 to 5), collected in proxy interviews.",
  "- `D102`: self-rated memory compared with 2 years ago (1 better, 2 same, 3 worse, 8 don't know, 9 refused, -8 not ascertained), singly imputed when missing.",
  "- `dx_v1`: core algorithm classification (1 Normal, 2 MCI, 3 Dementia). Missing when neither `TF` nor `jorm` is available.",
  "",
  "Rows are all HRS core respondents interviewed in each wave (all ages). The algorithm's norms apply to ages 65 and older; restrict to that age range for analysis.",
  "",
  "## What changed from the previous export",
  "",
  if (has_old) paste0("Previous export: `", dx_export_previous, "` (file last written ", old_mtime,
                      "; the date in its name is not the date its contents were written).") else
    paste0("Previous export `", dx_export_previous, "` was not found, so no comparison is reported."),
  "",
  paste0("1. **T-score cut points.** Severe: T < ", prev_cuts[["severe"]], " before, T < ", new_cuts[["severe"]],
         " now. Moderate: T < ", prev_cuts[["moderate"]], " before, T < ", new_cuts[["moderate"]], " now."),
  paste0("2. **How the cut points are set.** They are now chosen by a grid search (`R/A7_070-threshold_calibration.R`, step ",
         cal$grid_step, " T points) so that the weighted (", cal$weight, ") Normal/MCI/Dementia shares of the core algorithm in HRS-HCAP Wave 1 match those of the Manly-Jones algorithm (",
         cal$target_variable, "). The previous cut points were set earlier by inspecting prevalence curves and had not been re-examined after upstream code changes."),
  paste0("3. **Unchanged:** the factor model and item parameters, the norms and T-score formula, the self-rated memory rule (worse, don't know, refused, and not ascertained count as worse), ",
         "the D102 imputation, and the IQCODE rule for respondents without cognitive tests (", cal$jorm_cuts[["severe"]], " or higher: Dementia; above ",
         formatC(cal$jorm_cuts[["mild"]], format = "f", digits = 1), ": MCI)."),
  "",
  "Weighted class shares in HRS-HCAP Wave 1:",
  "",
  md(tibble::tibble(
    Classification = c("Manly-Jones algorithm (target)",
                       paste0("Core, previous cuts (", prev_cuts[["severe"]], ", ", prev_cuts[["moderate"]], ")"),
                       paste0("Core, new cuts (", new_cuts[["severe"]], ", ", new_cuts[["moderate"]], ")")),
    `Normal, %`   = pct1(c(cal$target_shares[["1"]], cal$previous$share_normal,   cal$chosen$share_normal)),
    `MCI, %`      = pct1(c(cal$target_shares[["2"]], cal$previous$share_mci,      cal$chosen$share_mci)),
    `Dementia, %` = pct1(c(cal$target_shares[["3"]], cal$previous$share_dementia, cal$chosen$share_dementia)),
    `Kappa, 3-class` = c("", formatC(cal$previous$kappa_3class, format = "f", digits = 3),
                         formatC(cal$chosen$kappa_3class, format = "f", digits = 3)),
    `Kappa, dementia` = c("", formatC(cal$previous$kappa_binary, format = "f", digits = 3),
                          formatC(cal$chosen$kappa_binary, format = "f", digits = 3))
  )),
  "",
  "Kappas compare the core algorithm with the Manly-Jones algorithm (3-class: quadratic weights). They are in-sample: the cut points were chosen in the same data.",
  ""
)

if (has_old) {
  txt <- c(txt,
    "## Differential classification, previous versus new export",
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
    paste0("Check: ", n_unexplained, " changed person-waves have a T score outside the two bands between the old and new cut points ",
           "(expected 0 if the cut points are the only change)."),
    ""
  )
}

txt <- c(txt,
  "## New export: classes among HRS-HCAP Wave 1 participants",
  "",
  "Unweighted person-wave counts.",
  "",
  md(new_counts),
  "",
  "## Source",
  "",
  "Method details: Appendix 1, `R/MS_MAIN/MS_Appendix-1-Core-Algorithm.qmd`. To compare any two exports: `Rscript R/A7_080-compare_dx_exports.R <old.csv> <new.csv>`.",
  ""
)

writeLines(txt, readme_path)
