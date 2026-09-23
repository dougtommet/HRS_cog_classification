# Analysis 2 study sample: all HRS/HCAP 2016 participants (N = 3,496).
# Both Core classification approaches classify every HCAP participant:
# the Core algorithm falls back to the Jorm IQCODE when no Core cognitive
# test is available, and the PMM uses its Jorm model for those participants.
# No Core cognitive-test requirement is applied here.

analysis2_tracker <- readRDS(
  here::here("R_objects", "A7_005_hrs16_merged.rds")
)

analysis2_hcap <- analysis2_tracker |>
  dplyr::filter(inHCAP == 1) |>
  dplyr::mutate(rage = PA019)

stopifnot(
  nrow(analysis2_hcap) == 3496,
  !anyDuplicated(analysis2_hcap[c("HHID", "PN")])
)
