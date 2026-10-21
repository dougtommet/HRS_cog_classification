#!/usr/bin/env Rscript

# Analysis A10: data sharing runs. Compares the core algorithm file shared with
# Lindsay Kobayashi (Data/Dx_for_LK-2026-07-10.csv, gitignored) with the
# replacement written by the current code (Data/Dx_for_LK-<A7 run date>.csv).
# Requires Analyses A0 and A7 (R_objects/A7_075_*.rds, A7_070_threshold_calibration.rds).

setwd(here::here())

render_target <- here::here("R", "A10", "A10_000-Main_control.qmd")
rendered_output <- here::here("R", "A10", "A10_000-Main_control.html")
final_output <- here::here("Reports", stringr::str_c("A10_Data_sharing_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))

quarto::quarto_render(render_target, output_format = "html")

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered analysis A10 to: ", final_output)
