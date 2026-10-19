# Analysis A7 side script (not run by the driver): compare two dx_v1 exports.
#
# Usage, from the project root:
#   Rscript R/A7_080-compare_dx_exports.R <old.csv> <new.csv>
# Example:
#   Rscript R/A7_080-compare_dx_exports.R ~/Downloads/Dx_for_LK-2026-06-11.csv Data/Dx_for_LK-2026-10-17.csv
#
# Reports, by wave, how many person-waves change class between the two files,
# overall and for HRS-HCAP Wave 1 participants. Writes a CSV of the
# cross-classification to Reports/A7_080_dx_export_comparison_[date].csv.

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Give two CSV paths: <old.csv> <new.csv>", call. = FALSE)

read_dx <- function(path) {
  readr::read_csv(path, col_types = readr::cols(HHID = "c", PN = "c", wave = "c", .default = "d")) |>
    select(HHID, PN, wave, dx_v1)
}

old <- read_dx(args[[1]]) |> rename(dx_old = dx_v1)
new <- read_dx(args[[2]]) |> rename(dx_new = dx_v1)

hcap_ids <- readRDS(here::here("R_objects", "A7_075_hrs16_hcap.rds")) |>
  transmute(HHID = as.character(HHID), PN = as.character(PN), hcap = 1L)

both <- full_join(old, new, by = c("HHID", "PN", "wave")) |>
  left_join(hcap_ids, by = c("HHID", "PN")) |>
  mutate(hcap = coalesce(hcap, 0L),
         # Changed: different class, or classified in one file only.
         changed = coalesce(dx_old != dx_new, is.na(dx_old) != is.na(dx_new)))

summary_tab <- both |>
  group_by(sample = if_else(hcap == 1L, "HRS-HCAP Wave 1", "Other HRS"), wave) |>
  summarise(person_waves = n(),
            changed = sum(changed),
            pct_changed = round(100 * changed / person_waves, 1),
            .groups = "drop")
print(summary_tab, n = Inf)

cross <- both |>
  filter(hcap == 1L) |>
  count(wave, dx_old, dx_new)
print(cross, n = Inf)

fs::dir_create(here::here("Reports"))
write_csv(cross, here::here("Reports", str_c("A7_080_dx_export_comparison_", Sys.Date(), ".csv")))
