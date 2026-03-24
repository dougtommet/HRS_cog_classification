#!/usr/bin/env Rscript

setwd(here::here())

render_target <- here::here("R", "Slides2603_Control.qmd")
rendered_output <- here::here("R", "Slides2603_Control.html")
final_output <- here::here("Reports", stringr::str_c("Slides2603_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))

quarto::quarto_render(
  input = render_target,
  output_format = "revealjs"
)

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered Slides2603 presentation to: ", final_output)