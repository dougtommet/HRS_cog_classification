# A10_010: check that the current code reproduces the classification file the
# recipient holds, and describe the alternative cut points that were
# considered and not adopted.
#
# Shared file:  Data/<a10_shared_file> (A10_005), read as delivered.
# Current:      R_objects/A7_075_hrs16_22_long.rds (Analysis A7), classified
#               with the algorithm cut points (A7_070 $cuts, 37 and 48).
# Alternative:  the same inputs classified with the A7_070 grid optimum
#               ($calibrated_cuts), considered on 2026-10-17 and not adopted.
# Writes:       R_objects/A10_010_comparison.rds
#
# Requires Analyses A0 and A7 to have been run.

source(here::here("R", "A7_060-algorithm_functions.R"))

d102_worse    <- c(-8, 3, 8, 9)
d102_notworse <- c(1, 2)

shared_path  <- here::here("Data", a10_shared_file)
current_path <- here::here("Data", a10_current_file)
if (!file.exists(shared_path)) {
  stop("Shared file not found: ", shared_path,
       ". Save the delivered file there (it is gitignored).", call. = FALSE)
}

cal    <- readRDS(here::here("R_objects", "A7_070_threshold_calibration.rds"))
long   <- readRDS(here::here("R_objects", "A7_075_hrs16_22_long.rds"))
merged <- readRDS(here::here("R_objects", "A7_005_hrs16_22_merged.rds"))
hcap   <- readRDS(here::here("R_objects", "A7_075_hrs16_hcap.rds"))

cuts_old <- cal$cuts             # algorithm cut points (shared file, current code)
cuts_new <- cal$calibrated_cuts  # alternative: grid optimum, not adopted

# Person-level variables: design, age by wave, raw 2016 D102 -------------------
person <- merged %>%
  transmute(id, SECU = num(SECU), STRATUM = num(STRATUM),
            P = num(page), Q = num(qage), R = num(rage), S = num(sage),
            PD102_raw = num(PD102)) 

ages <- person %>%
  select(id, P, Q, R, S) %>%
  pivot_longer(-id, names_to = "wave", values_to = "age")

hcap_ids <- hcap %>% distinct(id) %>% mutate(hcap_cohort = 1L)

# Current classification --------------------------------------------------------
current <- long %>%
  filter(inHRS == 1) %>%
  transmute(id, key = hhidpn_key(HHID, PN), wave,
            TF_now = num(TF), jorm_now = num(jorm), D102_now = num(D102),
            dx_cur = num(dx_v1), weight = num(weight), from_current = TRUE) %>%
  left_join(ages, by = c("id", "wave")) %>%
  left_join(person %>% select(id, SECU, STRATUM, PD102_raw), by = "id") %>%
  left_join(hcap_ids, by = "id") %>%
  mutate(hcap_cohort = coalesce(hcap_cohort, 0L))

# Shared file, as delivered -----------------------------------------------------
read_dx_file <- function(path) {
  readr::read_csv(path, col_types = readr::cols(HHID = "c", PN = "c", wave = "c",
                                                .default = "d")) %>%
    mutate(key = hhidpn_key(HHID, PN))
}

shared <- read_dx_file(shared_path) %>%
  transmute(key, wave, TF_old = TF, jorm_old = jorm, D102_old = D102, dx_old = dx_v1,
            from_shared = TRUE)

both <- full_join(shared, current, by = c("key", "wave")) %>%
  mutate(year = unname(wave_year[wave]),
         from_shared = coalesce(from_shared, FALSE),
         from_current = coalesce(from_current, FALSE))

# Row coverage ------------------------------------------------------------------
rows <- list(
  n_shared       = nrow(shared),
  n_current      = nrow(current),
  n_only_shared  = sum(both$from_shared & !both$from_current),
  n_only_current = sum(both$from_current & !both$from_shared),
  n_by_wave      = both %>% count(year, name = "person_waves")
)

# Input identity checks ---------------------------------------------------------
same_value <- function(a, b, tol = 0) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= tol)
}
max_abs_diff <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (!any(ok)) return(NA_real_)
  max(abs(a[ok] - b[ok]))
}

inputs <- list(
  tf_max_abs_diff     = max_abs_diff(both$TF_old, both$TF_now),
  tf_missing_mismatch = sum(is.na(both$TF_old) != is.na(both$TF_now)),
  jorm_max_abs_diff   = max_abs_diff(both$jorm_old, both$jorm_now),
  jorm_missing_mismatch = sum(is.na(both$jorm_old) != is.na(both$jorm_now)),
  d102_mismatch       = sum(!same_value(both$D102_old, both$D102_now)),
  n_tf_missing        = sum(is.na(both$TF_now)),
  n_jorm_route        = sum(is.na(both$TF_now) & !is.na(both$jorm_now))
)

# Reconstruction: apply the written rule to each file's own inputs --------------
classify_inputs <- function(TF, jorm, D102, cuts) {
  tibble(TF = TF, jorm = jorm, D102 = D102) %>%
    classify_core(cuts[["severe"]], cuts[["moderate"]]) %>%
    pull(dx_v1) %>%
    num()
}
n_differ <- function(a, b) sum(!same_value(a, b))

both <- both %>%
  mutate(
    dx_old_rebuilt = classify_inputs(TF_old, jorm_old, D102_old, cuts_old),
    dx_cur_rebuilt = classify_inputs(TF_now, jorm_now, D102_now, cuts_old),
    dx_alt         = classify_inputs(TF_now, jorm_now, D102_now, cuts_new)
  )

rebuild <- list(
  old_mismatch      = n_differ(both$dx_old, both$dx_old_rebuilt),
  cur_mismatch      = n_differ(both$dx_cur, both$dx_cur_rebuilt),
  shared_vs_current = n_differ(both$dx_old, both$dx_cur)
)

# Current export on disk, if present: does it match the A7 object? -------------
current_file <- if (file.exists(current_path)) {
  cf <- read_dx_file(current_path) %>% select(key, wave, dx_file = dx_v1)
  chk <- current %>% select(key, wave, dx_cur) %>% full_join(cf, by = c("key", "wave"))
  list(exists = TRUE, n = nrow(cf), mismatch = n_differ(chk$dx_cur, chk$dx_file),
       mtime = format(file.info(current_path)$mtime, "%Y-%m-%d"))
} else list(exists = FALSE)

# Differences in assignment: shared file versus the alternative cut points -----
both <- both %>%
  mutate(
    changed = !same_value(dx_old, dx_alt),
    complaint = case_when(D102_now %in% d102_worse    ~ "Reported worse memory",
                          D102_now %in% d102_notworse ~ "No reported worsening",
                          TRUE                        ~ "D102 missing"),
    cog_old = case_when(TF_now < cuts_old[["severe"]] ~ 2, TF_now < cuts_old[["moderate"]] ~ 1,
                        !is.na(TF_now) ~ 0),
    cog_new = case_when(TF_now < cuts_new[["severe"]] ~ 2, TF_now < cuts_new[["moderate"]] ~ 1,
                        !is.na(TF_now) ~ 0),
    age65 = !is.na(age) & age >= 65
  )

band_text <- function(a, b) paste0("T from ", fmt_cut(min(a, b)), " to under ", fmt_cut(max(a, b)))
severe_band   <- band_text(cuts_old[["severe"]],   cuts_new[["severe"]])
moderate_band <- band_text(cuts_old[["moderate"]], cuts_new[["moderate"]])

both <- both %>%
  mutate(band = case_when(
    is.na(TF_now)                                     ~ "No T score",
    TF_now >= min(cuts_old[["severe"]], cuts_new[["severe"]]) &
      TF_now <  max(cuts_old[["severe"]], cuts_new[["severe"]])     ~ severe_band,
    TF_now >= min(cuts_old[["moderate"]], cuts_new[["moderate"]]) &
      TF_now <  max(cuts_old[["moderate"]], cuts_new[["moderate"]]) ~ moderate_band,
    TRUE                                              ~ "Outside both bands"))

crosstab <- both %>%
  count(Shared = dx_factor(dx_old), Alternative = dx_factor(dx_alt)) %>%
  pivot_wider(names_from = Alternative, values_from = n, values_fill = 0)

change_by <- function(d) {
  d %>%
    group_by(Year = year) %>%
    summarise(`Person-waves` = n(), Changed = sum(changed),
              `% changed` = fmt_pct(mean(changed)), .groups = "drop")
}
changes <- list(
  all   = change_by(both),
  age65 = change_by(both %>% filter(age65)),
  hcap  = change_by(both %>% filter(hcap_cohort == 1L)),
  n_changed = sum(both$changed),
  pct_changed = mean(both$changed),
  n_changed_under65 = sum(both$changed & !both$age65)
)

transitions <- both %>%
  filter(changed) %>%
  mutate(Change = paste(dx_factor(dx_old), "to", dx_factor(dx_alt))) %>%
  count(Change, name = "n")
n_trans <- function(from, to) {
  x <- transitions$n[transitions$Change == paste(from, "to", to)]
  if (length(x) == 0) 0L else x
}

mechanism <- both %>%
  filter(!is.na(TF_now), band != "Outside both bands") %>%
  mutate(Change = if_else(changed,
                          paste(dx_factor(dx_old), "to", dx_factor(dx_alt)),
                          paste("No change,", dx_factor(dx_alt)))) %>%
  count(`T band` = band, `Self-rated memory` = complaint, Change, name = "Person-waves")

n_changed_outside_bands <- sum(both$changed & both$band %in% c("Outside both bands", "No T score"))

# Weighted prevalence, ages 65 and older ----------------------------------------
# Wave respondent weights (xWGTR). Respondents with weight 0 or missing (for
# example, nursing home residents in the HRS respondent weights) are excluded.
prev_sample <- both %>% filter(!is.na(dx_old), !is.na(dx_alt))

weight_excluded <- prev_sample %>%
  filter(age65) %>%
  group_by(Year = year) %>%
  summarise(`Classified, age 65+` = n(),
            `Weight 0 or missing` = sum(is.na(weight) | weight <= 0), .groups = "drop")

prev_wave <- function(w) {
  d <- prev_sample %>%
    filter(wave == w, !is.na(weight), weight > 0, !is.na(SECU), !is.na(STRATUM))
  for (k in 1:3) {
    d[[paste0("old", k)]] <- as.numeric(d$dx_old == k)
    d[[paste0("new", k)]] <- as.numeric(d$dx_alt == k)
    d[[paste0("dif", k)]] <- d[[paste0("new", k)]] - d[[paste0("old", k)]]
  }
  des <- svydesign(ids = ~SECU, strata = ~STRATUM, weights = ~weight,
                   nest = TRUE, data = d)
  des65 <- subset(des, age65)
  d65 <- d %>% filter(age65)
  purrr::map_dfr(1:3, function(k) {
    p_old <- svyciprop(as.formula(paste0("~old", k)), des65, method = "logit")
    p_new <- svyciprop(as.formula(paste0("~new", k)), des65, method = "logit")
    dif   <- svymean(as.formula(paste0("~dif", k)), des65)
    ci_o <- as.numeric(confint(p_old))
    ci_n <- as.numeric(confint(p_new))
    ci_d <- as.numeric(confint(dif))
    tibble(
      year = wave_year[[w]], class = dx_labels[k],
      n = nrow(d65),
      wn_old = sum(d65$weight[d65$dx_old == k]),
      wn_new = sum(d65$weight[d65$dx_alt == k]),
      p_old = as.numeric(p_old), lo_old = ci_o[1], hi_old = ci_o[2],
      p_new = as.numeric(p_new), lo_new = ci_n[1], hi_new = ci_n[2],
      d = as.numeric(coef(dif)), lo_d = ci_d[1], hi_d = ci_d[2]
    )
  })
}
prevalence <- purrr::map_dfr(names(wave_year), prev_wave)

# Assignment steps in the shared file ----------------------------------------------------
p16 <- both %>% filter(wave == "P")
shared_steps <- list(
  d102_missing_after_imputation = sum(is.na(both$D102_now)),
  d102_dk_rf_na = sum(both$D102_now %in% c(-8, 8, 9) & !is.na(both$TF_now)),
  n_2016 = nrow(p16),
  d102_imputed_2016 = sum(is.na(p16$PD102_raw) & !is.na(p16$D102_now)),
  d102_imputed_2016_complaint = sum(is.na(p16$PD102_raw) & p16$D102_now %in% d102_worse),
  d102_imputed_2016_dk_rf_na  = sum(is.na(p16$PD102_raw) & p16$D102_now %in% c(-8, 8, 9)),
  n_unclassified = sum(is.na(both$dx_old)),
  n_unclassified_no_tf_no_jorm = sum(is.na(both$dx_old) & is.na(both$TF_now) & is.na(both$jorm_now)),
  n_unclassified_tf_no_d102 = sum(is.na(both$dx_old) & !is.na(both$TF_now)),
  n_under65 = sum(!both$age65),
  n_under65_classified = sum(!both$age65 & !is.na(both$dx_old)),
  n_age_missing = sum(is.na(both$age))
)

A10_010_comparison <- list(
  files = list(shared = a10_shared_file, current = a10_current_file,
               shared_mtime = format(file.info(shared_path)$mtime, "%Y-%m-%d")),
  cuts_old = cuts_old, cuts_new = cuts_new,
  jorm_cuts = cal$jorm_cuts,
  calibration = cal[c("target_shares", "algorithm", "calibrated", "max_kappa",
                      "grid_position", "grid_step", "n_sample", "weight",
                      "target_variable", "date")],
  rows = rows, inputs = inputs, rebuild = rebuild, current_file = current_file,
  crosstab = crosstab, changes = changes, transitions = transitions,
  mechanism = mechanism, severe_band = severe_band, moderate_band = moderate_band,
  n_changed_outside_bands = n_changed_outside_bands,
  weight_excluded = weight_excluded, prevalence = prevalence,
  shared_steps = shared_steps,
  date = Sys.Date()
)

saveRDS(A10_010_comparison, here::here("R_objects", "A10_010_comparison.rds"))
