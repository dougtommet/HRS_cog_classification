#!/usr/bin/env Rscript

setwd(here::here())

render_target <- here::here("R", "A7_000-Main_control.qmd")
rendered_output <- here::here("R", "A7_000-Main_control.html")
final_output <- here::here("Reports", stringr::str_c("A7_HRS_cog_classification_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))
# A7_075 writes the shared diagnosis CSV to Data/
fs::dir_create(here::here("Data"))

quarto::quarto_render(render_target, output_format = "html")

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered analysis A7 to: ", final_output)
