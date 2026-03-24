#!/usr/bin/env Rscript

setwd(here::here())

render_target <- here::here("R", "PMM_000_Analysis_Report_Control.qmd")
rendered_output <- here::here("R", "PMM_000_Analysis_Report_Control.html")
final_output <- here::here("Reports", stringr::str_c("PMM_Analysis_Report_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))

quarto::quarto_render(render_target, output_format = "html")

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered analysis 3 to: ", final_output)