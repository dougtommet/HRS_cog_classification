#!/usr/bin/env Rscript

setwd(here::here())

render_target <- here::here("R", "A0_000-Main_control.qmd")
rendered_output <- here::here("R", "A0_000-Main_control.html")
final_output <- here::here("Reports", stringr::str_c("A0_HRS_data_processing_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))

quarto::quarto_render(render_target, output_format = "html")

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered analysis A0 to: ", final_output)
