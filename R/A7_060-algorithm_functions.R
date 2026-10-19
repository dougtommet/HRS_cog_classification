# Analysis A7: shared functions for the Core algorithm.
#
# Sourced by A7_070 (threshold calibration), A7_075 (classification), and
# A7_100 (agreement). Keeping one copy of each function ensures that the
# calibration grid and the final classification apply identical rules.
#
# Self-rated memory change (HRS D102: memory compared with 2 years ago):
#   1 = better, 2 = about the same, 3 = worse, 8 = don't know, 9 = refused,
#   -8 = not ascertained. Codes 3, 8, 9, and -8 count as reported worsening.

algorithm_thresholds <- function(df, cut_severe, cut_moderate,
                                 jorm_severe = 3.4, jorm_mild = 3.0) {
  stopifnot(is.numeric(cut_severe), is.numeric(cut_moderate),
            cut_severe < cut_moderate)
  df %>%
    mutate(
      cog_threshold = case_when(
        TF < cut_severe   ~ 2,
        TF < cut_moderate ~ 1,
        !is.na(TF)        ~ 0
      ),
      jorm_threshold = case_when(
        jorm >= jorm_severe                  ~ 2,
        jorm >  jorm_mild & jorm < jorm_severe ~ 1,
        jorm <= jorm_mild                    ~ 0
      )
    )
}

v1_algorithm <- function(df,
                         d102_worse    = c(-8, 3, 8, 9),
                         d102_notworse = c(1, 2)) {
  df <- df %>%
    mutate(dx_v1 = case_when(
      cog_threshold == 2 & D102 %in% d102_worse    ~ 3,
      cog_threshold == 2 & D102 %in% d102_notworse ~ 2,
      cog_threshold == 1 & D102 %in% d102_worse    ~ 2,
      cog_threshold == 1 & D102 %in% d102_notworse ~ 1,
      cog_threshold == 0                           ~ 1,
      is.na(TF) & jorm_threshold == 2              ~ 3,
      is.na(TF) & jorm_threshold == 1              ~ 2,
      is.na(TF) & jorm_threshold == 0              ~ 1
    ))

  df %>%
    labelled::set_variable_labels(dx_v1 = "Algorithm (V1) Dx") %>%
    labelled::set_value_labels(dx_v1 = c("Normal" = 1, "MCI" = 2, "Dementia" = 3))
}

classify_core <- function(df, cut_severe, cut_moderate) {
  df %>%
    algorithm_thresholds(cut_severe = cut_severe, cut_moderate = cut_moderate) %>%
    v1_algorithm()
}

# Counts at each node of the algorithm. The D102 code sets match v1_algorithm.
v1_flow_diagram <- function(df,
                            d102_worse    = c(-8, 3, 8, 9),
                            d102_notworse = c(1, 2)) {
  n_if <- function(cond) sum(cond, na.rm = TRUE)
  with(df, list(
    a1 = nrow(df),
    b1 = n_if(is.na(TF)),
    b2 = n_if(cog_threshold == 2),
    b3 = n_if(cog_threshold %in% c(0, 1)),
    b4 = n_if(cog_threshold == 1),
    b5 = n_if(cog_threshold == 0),
    c1 = n_if(is.na(TF) & jorm_threshold == 2),
    c2 = n_if(is.na(TF) & jorm_threshold %in% c(0, 1)),
    c3 = n_if(is.na(TF) & jorm_threshold == 1),
    c4 = n_if(is.na(TF) & jorm_threshold == 0),
    d1 = n_if(cog_threshold == 2 & D102 %in% d102_worse),
    d2 = n_if(cog_threshold == 2 & D102 %in% d102_notworse),
    d3 = n_if(cog_threshold == 1 & D102 %in% d102_worse),
    d4 = n_if(cog_threshold == 1 & D102 %in% d102_notworse),
    e1 = n_if(dx_v1 == 1),
    e2 = n_if(dx_v1 == 2),
    e3 = n_if(dx_v1 == 3),
    unclassified = n_if(is.na(dx_v1))
  ))
}

# Weighted class shares (proportions), classified cases only.
weighted_shares <- function(class, w, levels = 1:3) {
  class <- as.integer(haven::zap_labels(class))
  ok <- !is.na(class) & !is.na(w)
  s <- tapply(w[ok], factor(class[ok], levels = levels), sum)
  s[is.na(s)] <- 0
  stats::setNames(as.numeric(s / sum(s)), levels)
}

# Agreement statistics from a (weighted) contingency table.
weighted_kappa_from_tab <- function(tab) {
  obs <- unclass(tab)
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)

  agreement_weights <- outer(
    seq_len(nrow(obs)),
    seq_len(ncol(obs)),
    function(i, j) 1 - ((i - j)^2 / (nrow(obs) - 1)^2)
  )

  po <- sum(agreement_weights * obs) / sum(obs)
  pe <- sum(agreement_weights * expected) / sum(obs)
  kappa_w <- (po - pe) / (1 - pe)

  tibble::tibble(
    observed_weighted_agreement = po,
    expected_weighted_agreement = pe,
    quadratic_weighted_kappa = kappa_w
  )
}

binary_kappa_from_tab <- function(tab) {
  obs <- unclass(tab)
  expected <- outer(rowSums(obs), colSums(obs)) / sum(obs)

  po <- sum(diag(obs)) / sum(obs)
  pe <- sum(diag(expected)) / sum(obs)
  kappa <- (po - pe) / (1 - pe)

  tibble::tibble(
    observed_agreement = po,
    expected_agreement = pe,
    kappa = kappa
  )
}

# Display of survey-weighted population counts
#
# Weighted counts from HCAP population weights are shown in thousands
# (project reporting rule; see References/instructions.md). Entries that
# would round below 1 (fewer than 1,000 persons) keep two significant
# digits. Totals are summed in full units before display. The function
# uses base R only, so the gt tables saved to A7_100_hcap_tables.rds
# render in any session that reads them.
format_weighted_thousands <- function(x) {
  k <- x / 1000
  vapply(k, function(v) {
    if (is.na(v)) return("")
    if (v == 0) return("0")
    if (abs(signif(v, 2)) < 1) {
      return(formatC(signif(v, 2), format = "fg", digits = 2, flag = "#"))
    }
    format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)
  }, character(1))
}

weighted_thousands_note <- paste(
  "Cell entries are survey-weighted population counts in thousands.",
  "Entries for fewer than 1,000 persons show two significant digits."
)

