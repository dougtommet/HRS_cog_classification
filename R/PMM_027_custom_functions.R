# -----------------------------------------------------------------------------

# 1-way table function
tab1 <- function(data, var) {
  var <- rlang::enquo(var)  # capture the variable name
  data |>
    dplyr::count(!!var) |>
    janitor::adorn_totals("row") |>
    janitor::adorn_percentages("col") |>
    janitor::adorn_pct_formatting(digits = 1) |>
    janitor::adorn_ns()
}

# -----------------------------------------------------------------------------

# 2-way table function
# tab2 <- function(data, row_var, col_var) {
#  row_var <- rlang::enquo(row_var)
#  col_var <- rlang::enquo(col_var)
#
#  row_data <- dplyr::pull(data, !!row_var)
#  col_data <- dplyr::pull(data, !!col_var)
#
#  gmodels::CrossTable(
#    row_data, col_data,
#    prop.chisq = FALSE,      # suppress chi-square test
#    prop.t = TRUE,          # no total proportions
#    prop.r = FALSE,           # show row proportions in Row Total
#    prop.c = FALSE,          # show column proportions
#    prop.cell = FALSE,        # show cell proportions
#    format = "SAS",         # cleaner display
#    digits = 3               # show proportions (not percentages)
#  )
#}

tab2 <- function(data, row_var, col_var, prop_option = "t") {
  row_var_quo <- rlang::enquo(row_var)
  col_var_quo <- rlang::enquo(col_var)

  # Extract variable names and pad to 8 characters
  row_var_fullname <- rlang::as_label(row_var_quo)
  col_var_fullname <- rlang::as_label(col_var_quo)
  row_var_name <- stringr::str_pad(substr(row_var_fullname, 1, 8), 8, side = "right")
  col_var_name <- stringr::str_pad(substr(col_var_fullname, 1, 8), 8, side = "right")

  # Get variable labels (before zapping)
  row_label <- attr(data[[row_var_fullname]], "label")
  col_label <- attr(data[[col_var_fullname]], "label")

  # Zap labels to make variables safe for CrossTable
  data <- haven::zap_labels(data)

  # Pull variables and handle missing values
  row_data <- dplyr::pull(data, !!row_var_quo)
  col_data <- dplyr::pull(data, !!col_var_quo)
  row_data <- factor(ifelse(is.na(row_data), "NA", as.character(row_data)))
  col_data <- factor(ifelse(is.na(col_data), "NA", as.character(col_data)))

  # Set proportion flags
  prop.t <- FALSE
  prop.r <- FALSE
  prop.c <- FALSE
  prop.cell <- FALSE
  if (prop_option == "t") {
    prop.t <- TRUE
  } else if (prop_option == "r") {
    prop.r <- TRUE
  } else if (prop_option == "c") {
    prop.c <- TRUE
  } else if (prop_option == "cell") {
    prop.cell <- TRUE
  } else {
    stop('Invalid value for `prop_option`. Choose from "t", "r", "c", or "cell".')
  }

  # Capture CrossTable output
  out <- capture.output(
    gmodels::CrossTable(
      row_data, col_data,
      prop.chisq = FALSE,
      prop.t = prop.t,
      prop.r = prop.r,
      prop.c = prop.c,
      prop.cell = prop.cell,
      format = "SAS",
      digits = 3
    )
  )

  # Replace printed variable names
  out <- gsub("row_data", row_var_name, out, fixed = TRUE)
  out <- gsub("col_data", col_var_name, out, fixed = TRUE)

  # Insert label lines if either label is present
  label_lines <- character()
  if (!is.null(row_label)) {
    label_lines <- c(label_lines, paste0(row_var_name, ": ", row_label))
  }
  if (!is.null(col_label)) {
    label_lines <- c(label_lines, paste0(col_var_name, ": ", col_label))
  }
  if (length(label_lines) > 0) {
    out <- c(label_lines, "", out)
  }

  # Clean spacing: add one blank line at top, collapse extras
  out <- c("", out)
  out <- out[!(duplicated(out) & out == "")]

  # Print
  cat(out, sep = "\n")
}


# -----------------------------------------------------------------------------


