# A3_2_030: Reproduction check in HRS/HCAP 2016.
#
# The 2016 HCAP participants were scored in Analysis A3 from Analysis A1
# inputs. Here they are scored from Analysis A0 inputs with the same fixed
# model. If the two pipelines code the inputs the same way, the posterior
# probabilities agree. Part 1 compares the inputs variable by variable;
# part 2 compares the probabilities.

a3_2       <- readRDS(here::here("R_objects", "A3_2_010_scoring_long.rds"))
a3_2_probs <- readRDS(here::here("R_objects", "A3_2_pmm_probs_hrs16_22.rds"))
pmm100     <- readRDS(here::here("R_objects", "PMM_100.RDS"))

a3_vars <- c(a3_2$inputs$mplus,
             "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x4x5", "x4x6")

a1_side <- pmm100 |>
  dplyr::filter(inHCAP == 1) |>
  mutate(key = hhidpn_key(HHID, PN), id = as.numeric(id)) |>
  dplyr::select(key, id, dplyr::all_of(a3_vars)) |>
  mutate(dplyr::across(dplyr::all_of(a3_vars), num))

a0_side <- a3_2$scoring |>
  dplyr::filter(wave == 2016, inHCAP == 1) |>
  mutate(key = hhidpn_key(HHID, PN)) |>
  dplyr::select(key, dplyr::all_of(a3_vars))

both <- dplyr::inner_join(a1_side, a0_side, by = "key", suffix = c(".a1", ".a0"))

# ---------------------------------------------------------------------------
# Part 1: inputs
a3_2_check_inputs <- purrr::map_dfr(a3_vars, function(v) {
  a <- both[[stringr::str_c(v, ".a1")]]
  b <- both[[stringr::str_c(v, ".a0")]]
  ok <- !is.na(a) & !is.na(b)
  tibble(
    variable        = v,
    n_both          = sum(ok),
    pct_identical   = 100 * mean(abs(a[ok] - b[ok]) < 1e-6),
    max_abs_diff    = if (any(ok)) max(abs(a[ok] - b[ok])) else NA_real_,
    missing_a1_only = sum(is.na(a) & !is.na(b)),
    missing_a0_only = sum(!is.na(a) & is.na(b))
  )
})

# ---------------------------------------------------------------------------
# Part 2: probabilities. A3 posteriors from the fixed-parameter runs, combined
# with the PMM_110 rule (Jorm model when present).
read_cprob <- function(path) {
  suppressWarnings(MplusAutomation::readModels(path))$savedata |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(id, cprob1, cprob2, cprob3)
}
a3_cog  <- read_cprob(here::here("mplus_output", "pmm_103", "pmm_hcap_103b.out"))
a3_jorm <- read_cprob(here::here("mplus_output", "pmm_103_jorm", "pmm_hcap_103b_jorm.out"))

a3_probs <- a1_side |>
  dplyr::select(key, id) |>
  dplyr::left_join(dplyr::rename(a3_cog,  c1 = cprob1, c2 = cprob2, c3 = cprob3), by = "id") |>
  dplyr::left_join(dplyr::rename(a3_jorm, j1 = cprob1, j2 = cprob2, j3 = cprob3), by = "id") |>
  dplyr::transmute(
    key,
    a3_model = dplyr::case_when(!is.na(j1) ~ "jorm", !is.na(c1) ~ "cognition"),
    a3_p1 = dplyr::if_else(!is.na(j1), j1, c1),
    a3_p2 = dplyr::if_else(!is.na(j1), j2, c2),
    a3_p3 = dplyr::if_else(!is.na(j1), j3, c3)
  )

a3_2_probs16 <- a3_2_probs |>
  dplyr::filter(wave == 2016) |>
  mutate(key = hhidpn_key(hhid, pn)) |>
  dplyr::select(key, pmm_model, p_normal, p_mci, p_dementia)

prob_cmp <- dplyr::inner_join(a3_probs, a3_2_probs16, by = "key") |>
  dplyr::filter(!is.na(a3_p1), !is.na(p_normal)) |>
  mutate(
    max_abs_diff = pmax(abs(a3_p1 - p_normal), abs(a3_p2 - p_mci), abs(a3_p3 - p_dementia)),
    a3_modal   = max.col(cbind(a3_p1, a3_p2, a3_p3), ties.method = "first"),
    a3_2_modal = max.col(cbind(p_normal, p_mci, p_dementia), ties.method = "first")
  )

a3_2_check_probs <- tibble(
  n_hcap_a3         = nrow(a1_side),
  n_matched_a0      = nrow(both),
  n_both_scored     = nrow(prob_cmp),
  pct_same_model    = 100 * mean(prob_cmp$a3_model == prob_cmp$pmm_model),
  pct_diff_lt_001   = 100 * mean(prob_cmp$max_abs_diff < 0.001),
  pct_diff_lt_01    = 100 * mean(prob_cmp$max_abs_diff < 0.01),
  max_abs_diff      = max(prob_cmp$max_abs_diff),
  r_normal          = stats::cor(prob_cmp$a3_p1, prob_cmp$p_normal),
  r_mci             = stats::cor(prob_cmp$a3_p2, prob_cmp$p_mci),
  r_dementia        = stats::cor(prob_cmp$a3_p3, prob_cmp$p_dementia),
  pct_same_modal    = 100 * mean(prob_cmp$a3_modal == prob_cmp$a3_2_modal)
)

saveRDS(list(inputs = a3_2_check_inputs, probs = a3_2_check_probs, detail = prob_cmp),
        here::here("R_objects", "A3_2_030_hcap16_check.rds"))
