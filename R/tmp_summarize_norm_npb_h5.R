args <- commandArgs(trailingOnly = TRUE)

h5_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  "/Users/rnj/DWork/GitHub/HRS_cog_classification/mplus_output/norm_npb/norm_npb.h5"
}

if (!file.exists(h5_path)) {
  stop("HDF5 file not found: ", h5_path)
}

summary_path <- "/Summary of Analysis"
obs_path <- paste0(summary_path, "/Number of observations")
input_path <- paste0(summary_path, "/Input data file(s)/Data files")

top_level_sections <- rhdf5::h5ls(h5_path) |>
  dplyr::filter(group == "/", otype == "H5I_GROUP") |>
  dplyr::pull(name)

number_of_observations <- rhdf5::h5read(h5_path, obs_path)

input_files <- tryCatch(
  rhdf5::h5read(h5_path, input_path),
  error = function(...) character()
)

fit_info <- tryCatch(
  mplush5::mplus.print.model.fit.information(h5_path),
  error = function(...) NULL
)

cat("File:\n")
cat(h5_path, "\n\n", sep = "")

cat("Top-level HDF5 sections:\n")
cat(paste0("- ", top_level_sections), sep = "\n")
cat("\n\n")

cat("Number of observations used in the analysis:\n")
cat(number_of_observations, "\n\n", sep = "")

if (length(input_files) > 0) {
  cat("Input data file(s):\n")
  cat(paste0("- ", input_files), sep = "\n")
  cat("\n\n")
}

if (!is.null(fit_info)) {
  cat("Model fit information:\n")
  print(fit_info, row.names = FALSE)
}