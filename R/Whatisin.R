library(here)
library(purrr)
library(dplyr)
library(haven)    # for variable labels
library(tibble)

# Helper function to inspect one RDS file
inspect_rds_file <- function(file_path) {
  obj <- tryCatch(readRDS(file_path), error = function(e) return(NULL))
  if (is.null(obj)) return(NULL)

  if (is.data.frame(obj)) {
    # Get variable names and labels
    vars <- names(obj)
    labels <- map_chr(vars, ~ attr(obj[[.x]], "label") %||% "")
    n_rows <- nrow(obj)
    tibble(
      file = basename(file_path),
      variable = vars,
      label = labels,
      n_records = n_rows
    )
  } else {
    tibble(
      file = basename(file_path),
      variable = NA_character_,
      label = paste("Object type:", class(obj)[1]),
      n_records = NA_integer_
    )
  }
}

# List all .rds files in the R_objects directory
rds_files <- list.files(here::here("R_objects"), pattern = "\\.rds$", full.names = TRUE)

# Inspect each RDS file and combine the results
rds_summary <- map_dfr(rds_files, inspect_rds_file)

# Save to CSV
output_path <- here::here("R_objects", "whatsin.csv")
readr::write_csv(rds_summary, output_path)

# Print all rows of the result
print(rds_summary, n = Inf)