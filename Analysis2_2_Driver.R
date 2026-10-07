#!/usr/bin/env Rscript

# Analysis A2_2: Revealjs slide deck summarizing Analysis A2.
# Requires current Analysis A7 and A3 outputs (read through R/MS_MAIN/MS_010_results_objects.R)
# and Figures/Slides2603_results_class_probabilities.png (written by Slides2603_Driver.R).

setwd(here::here())

render_target <- here::here("R", "Analysis_2_2", "A2_2_000_Control.qmd")
rendered_output <- here::here("R", "Analysis_2_2", "A2_2_000_Control.html")
final_output <- here::here("Reports", stringr::str_c("Slides_A2summary_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))

quarto::quarto_render(
  input = render_target,
  output_format = "revealjs"
)

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered Analysis A2_2 slides to: ", final_output)
