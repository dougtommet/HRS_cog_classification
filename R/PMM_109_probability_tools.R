# Pure probability helpers; no data access or model execution on sourcing.
pmm_normalize_probabilities <- function(probabilities, tolerance = 0.002) {
  P <- as.matrix(probabilities)
  if (ncol(P) != 3L || any(!is.finite(P)) || any(P < 0 | P > 1) ||
      any(abs(rowSums(P) - 1) > tolerance)) {
    stop("Expected complete, valid Normal/MCI/Dementia probabilities summing to 1.")
  }
  P / rowSums(P)
}

pmm_probability_table <- function(data, reference, weight, binary = FALSE) {
  cols <- c("cprob1_combined", "cprob2_combined", "cprob3_combined")
  valid <- !is.na(data[[reference]]) & is.finite(data[[weight]]) & data[[weight]] > 0
  d <- data[valid, , drop = FALSE]
  if (!nrow(d)) stop("No eligible observations for the probability-weighted table.")
  P <- pmm_normalize_probabilities(d[, cols])
  labels <- c("Normal", "MCI", "Dementia")
  if (binary) {
    P <- cbind(P[, 1] + P[, 2], P[, 3])
    labels <- c("Non-demented", "Demented")
  }
  ref <- d[[reference]]
  if (!is.factor(ref) || length(levels(ref)) != ncol(P)) {
    stop("Reference classification must be a factor with all expected levels.")
  }
  membership <- outer(as.integer(ref), seq_len(ncol(P)), "==") * 1
  tab <- crossprod(membership, P * d[[weight]])
  dimnames(tab) <- list(levels(ref), labels)
  tab
}

pmm_weighted_shares <- function(classification, weight, labels = c("Normal", "MCI", "Dementia")) {
  keep <- !is.na(classification) & is.finite(weight) & weight > 0
  if (!any(keep)) stop("No eligible observations for class margins.")
  classification <- factor(as.character(classification[keep]), levels = labels)
  if (anyNA(classification)) stop("Unexpected class label in margins.")
  membership <- outer(as.integer(classification), seq_along(labels), "==") * 1
  stats::setNames(100 * colSums(membership * weight[keep]) / sum(weight[keep]), labels)
}
