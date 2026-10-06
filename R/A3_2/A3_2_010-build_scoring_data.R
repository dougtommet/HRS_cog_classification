# A3_2_010: Build the person-wave scoring file (HRS Core 2016, 2018, 2020, 2022).
#
# Sample: respondents interviewed in the wave (A0 inHRS_xx == 1) and age 65+
#         at that wave. No restriction on sampling weight.
# Inputs: Analysis A0 wave-specific recodes. Each input is renamed to the
#         variable name used in the Analysis A3 Mplus models (pmm_hcap_103b,
#         pmm_hcap_103b_jorm), and coded to match PMM_031 to PMM_100.
# Covariates: built with the PMM_045 rules. Centering constants for x4, x5, x6,
#         x4x5, and x4x6 are recovered from PMM_100.RDS so that they equal the
#         HCAP16 survey-weighted means used in calibration. They are never
#         recomputed in the scoring sample.

a0     <- readRDS(here::here("R_objects", "A0_030_hrs16_22_merged.rds"))
pmm100 <- readRDS(here::here("R_objects", "PMM_100.RDS"))

# ---------------------------------------------------------------------------
# HCAP16 centering constants (raw minus centered value is constant by design)
centering_constant <- function(raw, centered, name) {
  d <- num(raw) - num(centered)
  d <- d[!is.na(d)]
  if (length(d) == 0 || diff(range(d)) > 1e-8) {
    stop("A3_2_010: centering constant for ", name, " is not constant in PMM_100.RDS")
  }
  mean(d)
}

a3_2_centering <- c(
  x4   = centering_constant(pmm100$female, pmm100$x4, "x4"),
  x5   = centering_constant(pmm100$black,  pmm100$x5, "x5"),
  x6   = centering_constant(pmm100$hisp,   pmm100$x6, "x6"),
  x4x5 = centering_constant(num(pmm100$female) * num(pmm100$black), pmm100$x4x5, "x4x5"),
  x4x6 = centering_constant(num(pmm100$female) * num(pmm100$hisp),  pmm100$x4x6, "x4x6")
)

# ---------------------------------------------------------------------------
# Age restricted cubic spline, copied from PMM_045 (knots 70, 78, 86, 94;
# spage1 centered at 70)
a3_2_ageRCS <- function(age) {
  k1 <- 70; k2 <- 78; k3 <- 86; k4 <- 94
  tp <- function(x, knot) pmax((x - knot)^3, 0)
  denom <- (k4 - k1)^2
  spage2 <- (tp(age, k1) - ((k4 - k3)^-1) * (tp(age, k3) * (k4 - k1) - tp(age, k4) * (k3 - k1))) / denom
  spage3 <- (tp(age, k2) - ((k4 - k3)^-1) * (tp(age, k3) * (k4 - k2) - tp(age, k4) * (k3 - k2))) / denom
  spage2[is.na(age)] <- NA
  spage3[is.na(age)] <- NA
  tibble(x1 = age - 70, x2 = spage2, x3 = spage3)
}

# ---------------------------------------------------------------------------
# Map from Mplus model names to A0 source names. "{L}" is the wave letter.
a3_2_inputs <- tribble(
  ~mplus,     ~a0,           ~label,                                         ~type,
  "vdori",    "r{L}vdori",   "Orientation to time (0-4)",                    "ord",
  "vdlfl1z",  "r{L}vdlfl1z", "Animal naming (rescaled)",                     "cont",
  "vdlfl2",   "r{L}vdlfl2",  "Object naming (0-2)",                          "ord",
  "vdlfl3",   "r{L}vdlfl3",  "President/vice-president naming (0-2)",        "ord",
  "vdwdimmz", "r{L}vdwdimmz","Immediate word recall (rescaled)",             "cont",
  "vdwddelz", "r{L}vdwddelz","Delayed word recall (rescaled)",               "cont",
  "vdexf7z",  "r{L}vdexf7z", "Number series (rescaled)",                     "cont",
  "vdsevens", "r{L}vdsevens","Serial sevens (0-5)",                          "ord",
  "vdcount",  "r{L}vdcount", "Backward counting from 20 (0/1)",              "bin",
  "nPG014",   "r{L}G014",    "ADL: dressing",                                "bin",
  "nPG021",   "r{L}G021",    "ADL: bathing",                                 "bin",
  "nPG023",   "r{L}G023",    "ADL: eating",                                  "bin",
  "nPG030",   "r{L}G030",    "ADL: using the toilet",                        "bin",
  "nPG040",   "r{L}G040",    "IADL: using maps",                             "bin",
  "nPG041",   "r{L}G041",    "IADL: preparing meals",                        "bin",
  "nPG044",   "r{L}G044",    "IADL: grocery shopping",                       "bin",
  "nPG047",   "r{L}G047",    "IADL: making phone calls",                     "bin",
  "nPG050",   "r{L}G050",    "IADL: taking medications",                     "bin",
  "nPG059",   "r{L}G059",    "IADL: managing money",                         "bin",
  "PD102",    "{L}D102",     "Memory compared to 2 years ago (1-3)",         "ord",
  "jorm",     "r{L}jorm",    "Jorm IQCODE (proxy informant)",                "cont"
)

a3_2_waves <- tribble(
  ~wave, ~L,  ~age,   ~inhrs,
  2016,  "P", "page", "inHRS_16",
  2018,  "Q", "qage", "inHRS_18",
  2020,  "R", "rage", "inHRS_20",
  2022,  "S", "sage", "inHRS_22"
)

# Record which A0 source variables do not exist in a wave (structurally missing)
a3_2_source_log <- list()

build_wave <- function(wave, L, age, inhrs) {
  d <- a0 |>
    dplyr::filter(num(.data[[inhrs]]) == 1, num(.data[[age]]) >= 65)

  get_var <- function(v) {
    if (v %in% names(d)) num(d[[v]]) else rep(NA_real_, nrow(d))
  }

  out <- tibble(
    HHID   = d$HHID,
    PN     = d$PN,
    wave   = wave,
    inHCAP = get_var("inHCAP"),
    age    = get_var(age),
    female = get_var("female"),
    black  = get_var("black"),
    hisp   = get_var("hisp"),
    SCHLYRSimp = get_var("SCHLYRSimp")
  )

  for (i in seq_len(nrow(a3_2_inputs))) {
    src <- stringr::str_replace(a3_2_inputs$a0[i], stringr::fixed("{L}"), L)
    a3_2_source_log[[length(a3_2_source_log) + 1]] <<- tibble(
      wave = wave, mplus = a3_2_inputs$mplus[i], source = src,
      present = src %in% names(d)
    )
    out[[a3_2_inputs$mplus[i]]] <- get_var(src)
  }
  out
}

a3_2_scoring <- purrr::pmap_dfr(a3_2_waves, build_wave)
a3_2_source_log <- dplyr::bind_rows(a3_2_source_log)

# PD102 as in PMM_100: keep 1 (better), 2 (same), 3 (worse); DK/RF to missing
a3_2_scoring <- a3_2_scoring |>
  mutate(PD102 = dplyr::if_else(PD102 %in% c(1, 2, 3), PD102, NA_real_))

# Covariates as in PMM_045. Interactions use uncentered x4-x6; then x4, x5,
# x6, x4x5, x4x6 are centered at the HCAP16 constants.
a3_2_scoring <- a3_2_scoring |>
  dplyr::bind_cols(a3_2_ageRCS(a3_2_scoring$age)) |>
  mutate(
    x4 = female, x5 = black, x6 = hisp, x7 = SCHLYRSimp - 12,
    x1x4 = x1 * x4, x1x5 = x1 * x5, x1x6 = x1 * x6, x1x7 = x1 * x7,
    x2x4 = x2 * x4, x2x5 = x2 * x5, x2x6 = x2 * x6, x2x7 = x2 * x7,
    x3x4 = x3 * x4, x3x5 = x3 * x5, x3x6 = x3 * x6, x3x7 = x3 * x7,
    x4x5 = x4 * x5, x4x6 = x4 * x6, x4x7 = x4 * x7,
    x5x7 = x5 * x7, x6x7 = x6 * x7
  ) |>
  mutate(
    x4   = x4   - a3_2_centering[["x4"]],
    x5   = x5   - a3_2_centering[["x5"]],
    x6   = x6   - a3_2_centering[["x6"]],
    x4x5 = x4x5 - a3_2_centering[["x4x5"]],
    x4x6 = x4x6 - a3_2_centering[["x4x6"]]
  ) |>
  arrange(wave, HHID, PN) |>
  mutate(sid = dplyr::row_number()) |>   # integer Mplus id
  relocate(sid, HHID, PN, wave)

stopifnot(!anyDuplicated(a3_2_scoring[, c("HHID", "PN", "wave")]))

saveRDS(list(scoring    = a3_2_scoring,
             inputs     = a3_2_inputs,
             source_log = a3_2_source_log,
             centering  = a3_2_centering),
        here::here("R_objects", "A3_2_010_scoring_long.rds"))
