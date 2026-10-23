# PMM_114_mci_ceiling.R
#
# Why no participant has an MCI probability above one half (2026-10-22).
# Computes, from the cognition model output named in R/PMM_090_model_registry.R:
#   - the class distributions of the general cognitive factor (mean, SD) and
#     the class priors (fixed class proportions of the scoring model);
#   - the largest MCI posterior possible if the factor score were known
#     exactly, with the fitted priors and with equal priors;
#   - the approximate within-class reliability (omega) of the 8 tests in the
#     Normal class, from the standardized loadings, and the largest MCI
#     posterior after adding measurement error of that size;
#   - the scored probabilities: largest MCI probability, number above 0.50,
#     and the participants whose most likely class is MCI.
# Sourced by the A3 report (PMM_102_calibration_model.qmd) and the Slides2603
# deck. Returns the list `pmm_mci`.

source(here::here("R", "PMM_090_model_registry.R"))

pmm_mci <- local({
  cal <- suppressWarnings(MplusAutomation::readModels(pmm_models$cognition$calibration_out))
  sco <- suppressWarnings(MplusAutomation::readModels(pmm_models$cognition$scoring_out))

  prior <- sco[["class_counts"]][["modelEstimated"]][["proportion"]]

  gcp <- cal[["parameters"]][["unstandardized"]] |>
    tibble::as_tibble() |>
    dplyr::filter(toupper(param) == "GCP", paramHeader %in% c("Means", "Variances"),
                  LatentClass %in% c("1", "2", "3")) |>
    dplyr::arrange(LatentClass)
  gcp_mean <- gcp |> dplyr::filter(paramHeader == "Means") |> dplyr::pull(est)
  gcp_sd <- sqrt(gcp |> dplyr::filter(paramHeader == "Variances") |> dplyr::pull(est))
  if (length(gcp_mean) != 3 || length(gcp_sd) != 3) {
    stop("PMM_114: expected the gcp mean and variance in each of 3 classes.")
  }

  lam <- cal[["parameters"]][["stdyx.standardized"]] |>
    tibble::as_tibble() |>
    dplyr::filter(grepl("\\.BY$", paramHeader), LatentClass == "1") |>
    dplyr::pull(est)
  omega <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))

  ceiling_mci <- function(p, error_var = 0) {
    x <- seq(-10, 4, by = 0.001)
    d <- sapply(1:3, function(k) p[k] * stats::dnorm(x, gcp_mean[k], sqrt(gcp_sd[k]^2 + error_var)))
    post <- d / rowSums(d)
    i <- which.max(post[, 2])
    list(p = post[i, 2], x = x[i], p_normal = post[i, 1], p_dementia = post[i, 3])
  }

  P <- sco[["savedata"]] |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(cprob1, cprob2, cprob3) |>
    as.matrix()
  modal <- max.col(P, ties.method = "first")

  list(
    gcp_mean = gcp_mean,
    gcp_sd = gcp_sd,
    prior = prior,
    omega = omega,
    ceil_exact = ceiling_mci(prior),
    ceil_equal = ceiling_mci(rep(1 / 3, 3)),
    ceil_rel = ceiling_mci(prior, (1 - omega) / omega),
    n_scored = nrow(P),
    max_p_mci = max(P[, 2]),
    n_mci_over_half = sum(P[, 2] > 0.5),
    n_mci_modal = sum(modal == 2),
    pct_mci_modal = 100 * mean(modal == 2),
    avg_mci_modal = colMeans(P[modal == 2, , drop = FALSE])
  )
})
