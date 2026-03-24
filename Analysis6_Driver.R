#!/usr/bin/env Rscript

setwd(here::here())

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop(
    paste(
      "Usage:",
      "Rscript Analysis6_Driver.R render-qmd",
      "or",
      "Rscript Analysis6_Driver.R summarize-h5 [path-to-h5]"
    )
  )
}

mode <- args[[1]]

fs::dir_create(here::here("Reports"))

if (identical(mode, "render-qmd")) {
  render_target <- here::here("R", "tmp_control.qmd")
  rendered_output <- here::here("R", "tmp_control.html")
  final_output <- here::here(
    "Reports",
    stringr::str_c("tmp_consensus_sample_comparison_", Sys.Date(), ".html")
  )

  quarto::quarto_render(render_target, output_format = "html")

  if (fs::file_exists(final_output)) {
    fs::file_delete(final_output)
  }

  fs::file_move(rendered_output, final_output)
  message("Rendered analysis 6 QMD output to: ", final_output)
} else if (identical(mode, "summarize-h5")) {
  h5_path <- if (length(args) >= 2) {
    args[[2]]
  } else {
    here::here("mplus_output", "norm_npb", "norm_npb.h5")
  }

  output_file <- here::here(
    "Reports",
    stringr::str_c("tmp_norm_npb_h5_summary_", Sys.Date(), ".txt")
  )

  summary_script <- here::here("R", "tmp_summarize_norm_npb_h5.R")
  rscript_path <- file.path(R.home("bin"), "Rscript")

  output_text <- system2(
    rscript_path,
    c(summary_script, h5_path),
    stdout = TRUE,
    stderr = TRUE
  )

  writeLines(output_text, output_file)
  message("Rendered analysis 6 H5 summary to: ", output_file)
} else {
  stop("Unknown mode: ", mode)
}