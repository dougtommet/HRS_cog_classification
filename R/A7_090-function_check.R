# Analysis A7 side analysis: would self-reported ADL/IADL limitations help?
#
# Background (Rich, 2026-10-18): the original plan followed the Manly-Jones
# algorithm and used functional limitations to separate dementia from MCI
# (legacy A1: A1_029, A1_030, A1_035). This script repeats those checks in the
# current sample (HRS-HCAP Wave 1, HCAP16WGTR weights, current T scores) for the
# Appendix 1 endnote. Decision (Rich, 2026-10-18): the core algorithm keeps the
# memory rule; the gain in agreement from adding function was not judged large
# enough to justify letting limitations with probable physical causes enter the
# dementia classification.
#
# Part 1: share with any ADL/IADL limitation, by Manly-Jones class and by core
#         T-score band. Limitations are counted on the 9 items available in
#         every wave 2016-2022 (map use, G040, is omitted; it is not in 2022).
# Part 2: 3 rule variants, all at the core algorithm's cut points (A7_070
#         $cuts, applied to TR, the rounded T score): memory (the core algorithm), function
#         (Manly-Jones analog), and function or memory. Class shares and
#         agreement with the Manly-Jones algorithm. Decision (Rich,
#         2026-10-19): hold the cut points fixed for every rule, rather than
#         giving each rule its own prevalence-matched cut points.
# Part 3: variation of Part 2 across 200 random halves of the sample (drawn
#         within Manly-Jones class), at the same fixed cut points. With no cut
#         points selected, this describes sampling variation of the paired
#         differences; it is not an out-of-sample check.

source(here::here("R", "A7_060-algorithm_functions.R"))
# 2026-10-21: the algorithm classifies on TR (rounded T); TF replaced by TR.

A7_070_threshold_calibration <- readRDS(here::here("R_objects", "A7_070_threshold_calibration.rds"))
cuts <- A7_070_threshold_calibration$cuts

fc <- readRDS(here::here("R_objects", "A7_075_hrs16_hcap.rds")) %>%
  filter(!is.na(HCAP16WGTR), !is.na(vs1hcapdxeap)) %>%
  mutate(
    target    = as.integer(haven::zap_labels(vs1hcapdxeap)),
    mem_worse = D102 %in% c(-8, 3, 8, 9)
  )

# ADL/IADL items available in every core wave 2016-2022 (A0_012 recodes;
# 1 = limitation). G040 (map use) is omitted because it is not in 2022.
common_limit_items <- paste0("rP", c("G014", "G021", "G023", "G030",            # ADL
                                     "G041", "G044", "G047", "G050", "G059"))   # IADL
missing_items <- setdiff(common_limit_items, names(fc))
if (length(missing_items) > 0) {
  stop("A7_090: ADL/IADL items not found in A7_075_hrs16_hcap.rds: ",
       paste(missing_items, collapse = ", "), ". Rerun A0_012 and A0_030.")
}
limit_mat <- sapply(common_limit_items, function(v) as.numeric(haven::zap_labels(fc[[v]])))
fc$n_limit <- ifelse(rowSums(!is.na(limit_mat)) == 0, NA_real_,
                     rowSums(limit_mat, na.rm = TRUE))
fc$adl_any <- coalesce(fc$n_limit, 0) >= 1           # missing counted as no limitation

# How many change status when G040 is dropped (rPiadl_imp includes it in 2016).
n_changed_by_g040 <- sum((coalesce(fc$rPiadl_imp, 0) >= 1) != fc$adl_any)

target_shares <- weighted_shares(fc$target, fc$HCAP16WGTR)

# Part 1 -----------------------------------------------------------------------
limit_group <- function(x) {
  factor(case_when(is.na(x) ~ "Missing", x == 0 ~ "0", x == 1 ~ "1", TRUE ~ "2 or more"),
         levels = c("0", "1", "2 or more", "Missing"))
}
floor_by <- function(d, by) {
  d %>%
    filter(!is.na(TR)) %>%
    mutate(limits = limit_group(n_limit)) %>%
    group_by(group = {{ by }}) %>%
    summarise(
      n = n(),
      pct_none   = sum(HCAP16WGTR[limits == "0"]) / sum(HCAP16WGTR),
      pct_one    = sum(HCAP16WGTR[limits == "1"]) / sum(HCAP16WGTR),
      pct_twoplus = sum(HCAP16WGTR[limits == "2 or more"]) / sum(HCAP16WGTR),
      pct_missing = sum(HCAP16WGTR[limits == "Missing"]) / sum(HCAP16WGTR),
      .groups = "drop"
    )
}
floor_by_mj <- floor_by(
  fc %>% mutate(mj = factor(target, 1:3, c("Normal", "MCI", "Dementia"))), mj
)
floor_by_band <- floor_by(
  fc %>% mutate(band = factor(case_when(TR < cuts[["severe"]] ~ 1L,
                                        TR < cuts[["moderate"]] ~ 2L,
                                        TRUE ~ 3L),
                              1:3, c(paste0("T < ", cuts[["severe"]]),
                                     paste0(cuts[["severe"]], " to < ", cuts[["moderate"]]),
                                     paste0("T >= ", cuts[["moderate"]])))),
  band
)

# Part 2 -----------------------------------------------------------------------
variants <- tibble::tribble(
  ~variant,              ~label,
  "memory",              "Memory worse (core algorithm)",
  "function",            "Function at severe level; function or memory at moderate level",
  "function_or_memory",  "Function or memory at both levels"
)

variant_dx <- function(d, cut_severe, cut_moderate, variant) {
  d <- algorithm_thresholds(d, cut_severe = cut_severe, cut_moderate = cut_moderate)
  sev <- switch(variant,
                memory = d$mem_worse,
                `function` = d$adl_any,
                function_or_memory = d$adl_any | d$mem_worse)
  mod <- switch(variant,
                memory = d$mem_worse,
                `function` = d$adl_any | d$mem_worse,
                function_or_memory = d$adl_any | d$mem_worse)
  case_when(
    d$cog_threshold == 2 & sev ~ 3L,
    d$cog_threshold == 2       ~ 2L,
    d$cog_threshold == 1 & mod ~ 2L,
    d$cog_threshold == 1       ~ 1L,
    d$cog_threshold == 0       ~ 1L,
    is.na(d$TR) & d$jorm_threshold == 2 ~ 3L,
    is.na(d$TR) & d$jorm_threshold == 1 ~ 2L,
    is.na(d$TR) & d$jorm_threshold == 0 ~ 1L
  )
}

evaluate_variant <- function(variant, cut_severe, cut_moderate) {
  dx <- variant_dx(fc, cut_severe, cut_moderate, variant)
  w  <- fc$HCAP16WGTR
  sh <- weighted_shares(dx, w)
  tab3 <- xtabs(w ~ factor(fc$target, levels = 1:3) + factor(dx, levels = 1:3))
  tab2 <- xtabs(w ~ factor(fc$target == 3, levels = c(FALSE, TRUE)) +
                  factor(dx == 3, levels = c(FALSE, TRUE)))
  tibble::tibble(
    variant = variant, cut_severe = cut_severe, cut_moderate = cut_moderate,
    share_normal = sh[["1"]], share_mci = sh[["2"]], share_dementia = sh[["3"]],
    loss = sum((sh - target_shares)^2),
    kappa_3class = weighted_kappa_from_tab(tab3)$quadratic_weighted_kappa,
    kappa_binary = binary_kappa_from_tab(tab2)$kappa,
    capture = tab3[3, 3] / sum(tab3[3, ])
  )
}

variant_results <- purrr::map_dfr(variants$variant, function(v) {
  evaluate_variant(v, cuts[["severe"]], cuts[["moderate"]])
}) %>%
  left_join(variants, by = "variant")

# Check: the memory variant must reproduce the core algorithm (A7_075).
mem_dx <- variant_dx(fc, cuts[["severe"]], cuts[["moderate"]], "memory")
if (!isTRUE(all.equal(mem_dx, as.integer(haven::zap_labels(fc$dx_v1)))))
  warning("A7_090: memory variant does not reproduce dx_v1; check variant_dx().")

# Part 3: repeated random halves --------------------------------------------------
fc$iq_dx <- {
  j <- algorithm_thresholds(fc, cuts[["severe"]], cuts[["moderate"]])$jorm_threshold
  ifelse(is.na(fc$TR), j + 1L, NA_integer_)
}
variant_flags <- function(d, variant) {
  list(
    sev = switch(variant, memory = d$mem_worse, `function` = d$adl_any,
                 function_or_memory = d$adl_any | d$mem_worse),
    mod = switch(variant, memory = d$mem_worse, `function` = d$adl_any | d$mem_worse,
                 function_or_memory = d$adl_any | d$mem_worse)
  )
}
fast_dx <- function(d, variant, cs, cm) {
  f <- variant_flags(d, variant)
  dx <- d$iq_dx
  has_t <- !is.na(d$TR)
  dx[has_t] <- ifelse(d$TR[has_t] < cs, ifelse(f$sev[has_t], 3L, 2L),
                      ifelse(d$TR[has_t] < cm, ifelse(f$mod[has_t], 2L, 1L), 1L))
  dx
}
fast_agree <- function(d, dx) {
  w <- d$HCAP16WGTR
  tab3 <- xtabs(w ~ factor(d$target, levels = 1:3) + factor(dx, levels = 1:3))
  tab2 <- xtabs(w ~ factor(d$target == 3, levels = c(FALSE, TRUE)) +
                  factor(dx == 3, levels = c(FALSE, TRUE)))
  c(kappa_3class = weighted_kappa_from_tab(tab3)$quadratic_weighted_kappa,
    kappa_binary = binary_kappa_from_tab(tab2)$kappa,
    capture      = tab3[3, 3] / sum(tab3[3, ]))
}
# Check: on the full sample the fast classification must reproduce Part 2.
for (v in variants$variant) {
  k_fast <- fast_agree(fc, fast_dx(fc, v, cuts[["severe"]], cuts[["moderate"]]))[["kappa_3class"]]
  k_slow <- variant_results$kappa_3class[variant_results$variant == v]
  if (!isTRUE(all.equal(unname(k_fast), unname(k_slow)))) {
    warning("A7_090: fast classification differs from Part 2 for variant ", v, ".")
  }
}

n_split <- 200
set.seed(20261018)
split_half <- purrr::map_dfr(seq_len(n_split), function(r) {
  train <- unlist(lapply(split(seq_len(nrow(fc)), fc$target),
                         function(ix) ix[sample.int(length(ix), floor(length(ix) / 2))]))
  d_test  <- fc[-train, ]
  purrr::map_dfr(variants$variant, function(v) {
    a  <- fast_agree(d_test, fast_dx(d_test, v, cuts[["severe"]], cuts[["moderate"]]))
    tibble::tibble(split = r, variant = v,
                   cut_severe = cuts[["severe"]], cut_moderate = cuts[["moderate"]],
                   kappa_3class = a[["kappa_3class"]], kappa_binary = a[["kappa_binary"]],
                   capture = a[["capture"]])
  })
})

q025 <- function(x) unname(stats::quantile(x, 0.025))
q975 <- function(x) unname(stats::quantile(x, 0.975))
split_summary <- split_half %>%
  group_by(variant) %>%
  summarise(across(c(kappa_3class, kappa_binary, capture),
                   list(mean = mean, lo = q025, hi = q975)),
            .groups = "drop")
split_diff <- split_half %>%
  select(split, variant, kappa_3class, kappa_binary) %>%
  tidyr::pivot_longer(c(kappa_3class, kappa_binary), names_to = "stat") %>%
  tidyr::pivot_wider(names_from = variant, values_from = value) %>%
  tidyr::pivot_longer(c(`function`, function_or_memory), names_to = "variant",
                      values_to = "value_variant") %>%
  mutate(diff = value_variant - memory) %>%
  group_by(variant, stat) %>%
  summarise(mean = mean(diff), lo = q025(diff), hi = q975(diff),
            p_higher = mean(diff > 0), .groups = "drop")

A7_090_function_check <- list(
  n = nrow(fc),
  n_with_T = sum(!is.na(fc$TR)),
  target_shares = target_shares,
  floor_by_mj = floor_by_mj,
  floor_by_band = floor_by_band,
  variants = variant_results,
  limit_items = common_limit_items,
  n_limit_items = length(common_limit_items),
  n_changed_by_g040 = n_changed_by_g040,
  n_split = n_split,
  split_half = split_half,
  split_summary = split_summary,
  split_diff = split_diff,
  cuts = cuts,
  note = paste("ADL/IADL: count of limitations on the 9 items available in every wave",
               "(G040 omitted); missing counted as none in Parts 2 and 3.",
               "All rules at the core algorithm cut points."),
  date = Sys.Date()
)
saveRDS(A7_090_function_check, here::here("R_objects", "A7_090_function_check.rds"))
