# Analysis A7: apply the Core algorithm (dx_v1).
#
# Cut points come from A7_070 (prevalence matching to the HRS/HCAP Algorithm).
# Writes every object that later scripts and reports read, including
# A7_075_hrs16_hcap.rds (read by A7_100 and the A7 report).

source(here::here("R", "A7_060-algorithm_functions.R"))

A7_070_threshold_calibration <- readRDS(here::here("R_objects", "A7_070_threshold_calibration.rds"))
cut_severe   <- A7_070_threshold_calibration$cuts[["severe"]]
cut_moderate <- A7_070_threshold_calibration$cuts[["moderate"]]

# Using the n=3496 sample
hrs16_merged  <- readRDS(here::here("R_objects", "A7_055_hrs16_merged.rds"))
hrs16_22_long <- readRDS(here::here("R_objects", "A7_055_hrs16_22_long.rds"))

hrs16_merged <- hrs16_merged %>%
  mutate(jorm = rPjorm,
         D102 = PD102_imp)
hrs16_22_long <- hrs16_22_long %>%
  mutate(D102 = D102_imp)

hrs16_merged  <- classify_core(hrs16_merged,  cut_severe, cut_moderate)
QSPtools::checkvar(hrs16_merged, dx_v1, cog_threshold, D102, jorm_threshold)

hrs16_22_long <- classify_core(hrs16_22_long, cut_severe, cut_moderate)
QSPtools::checkvar(hrs16_22_long, dx_v1, cog_threshold, D102, jorm_threshold)

hrs16_hcap <- hrs16_merged %>%
  filter(inHCAP == 1)

hrs16_flow_diagram_values <- v1_flow_diagram(hrs16_merged)
hcap_flow_diagram_values  <- v1_flow_diagram(hrs16_hcap)

# Summary values for reports -------------------------------------------------
route_label <- function(TF, jorm) {
  case_when(!is.na(TF)   ~ "Cognitive tests",
            !is.na(jorm) ~ "Informant IQCODE",
            TRUE         ~ "Not classified")
}

hcap_ids <- hrs16_hcap %>%
  select(id) %>%
  mutate(hcap_cohort = 1L)

# Person-waves of the HCAP 2016 cohort interviewed in the Core, 2016-2022.
hcap_cohort_waves <- hrs16_22_long %>%
  filter(inHRS == 1) %>%
  inner_join(hcap_ids, by = "id") %>%
  mutate(
    year  = recode(wave, P = 2016L, Q = 2018L, R = 2020L, S = 2022L),
    route = route_label(TF, jorm),
    dx    = factor(as.integer(haven::zap_labels(dx_v1)), levels = 1:3,
                   labels = c("Normal", "MCI", "Dementia"))
  )

hcap_cohort_wave_counts <- hcap_cohort_waves %>%
  count(year, route, dx, .drop = FALSE) %>%
  filter(!(route == "Not classified" & !is.na(dx)))

A7_075_algorithm_summary <- list(
  cuts = c(severe = cut_severe, moderate = cut_moderate),
  jorm_cuts = A7_070_threshold_calibration$jorm_cuts,
  hcap_flow = hcap_flow_diagram_values,
  hcap_n = nrow(hrs16_hcap),
  # Requires raw PD102 (added to A0_030 on 2026-10-17); NA if A0 not rerun.
  hcap_d102_imputed = if ("PD102" %in% names(hrs16_hcap)) {
    sum(is.na(hrs16_hcap$PD102) & !is.na(hrs16_hcap$D102) & !is.na(hrs16_hcap$TF))
  } else NA_integer_,
  hcap_d102_dk_rf_na = sum(hrs16_hcap$D102 %in% c(-8, 8, 9) & !is.na(hrs16_hcap$TF)),
  hcap_cohort_wave_counts = hcap_cohort_wave_counts,
  hcap_cohort_wave_n = hcap_cohort_waves %>% count(year, name = "n_person_waves")
)

saveRDS(hrs16_merged,              here::here("R_objects", "A7_075_hrs16_merged.rds"))
saveRDS(hrs16_hcap,                here::here("R_objects", "A7_075_hrs16_hcap.rds"))
saveRDS(hrs16_22_long,             here::here("R_objects", "A7_075_hrs16_22_long.rds"))
saveRDS(algorithm_thresholds,      here::here("R_objects", "A7_075_algorithm_thresholds.rds"))
saveRDS(v1_algorithm,              here::here("R_objects", "A7_075_v1_algorithm.rds"))
saveRDS(hcap_flow_diagram_values,  here::here("R_objects", "A7_075_hcap_flow_diagram_values.rds"))
saveRDS(hrs16_flow_diagram_values, here::here("R_objects", "A7_075_hrs16_flow_diagram_values.rds"))
saveRDS(A7_075_algorithm_summary,  here::here("R_objects", "A7_075_algorithm_summary.rds"))

# Person-wave export for the incidence analysis (Kobayashi et al.).
# File names are hard-coded so an export already shared is never overwritten.
# For a new export, add a new name here and set dx_export_previous to the
# last file shared; A7_085 writes a README comparing the two.
dx_export_file     <- "Dx_for_LK-2026-10-17.csv"
dx_export_previous <- "Dx_for_LK-2026-06-11.csv"   # content written 2026-10-07 (cut points 37, 48)

dx_export <- hrs16_22_long %>%
  filter(inHRS == 1) %>%
  select(HHID, PN, wave, TF, jorm, D102, dx_v1)
write_csv(dx_export, here::here("Data", dx_export_file))

source(here::here("R", "A7_085-export_readme.R"))
