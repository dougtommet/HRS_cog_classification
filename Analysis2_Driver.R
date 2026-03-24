#!/usr/bin/env Rscript

setwd(here::here())

render_and_move <- function(control_file, output_file) {
  quarto::quarto_render(control_file, output_format = "docx")

  if (fs::file_exists(output_file)) {
    fs::file_delete(output_file)
  }

  fs::file_move(
    fs::path_ext_set(control_file, "docx"),
    output_file
  )
}

fs::dir_create(here::here("Reports"))

main_control <- here::here("R", "MS_Main_Control.qmd")
main_output <- here::here("Reports", stringr::str_c("MS_Main_", Sys.Date(), ".docx"))

appendix_control <- here::here("R", "MS_Tab_Fig_Apndx_Control.qmd")
appendix_output <- here::here("Reports", stringr::str_c("MS_Tab_Fig_Apndx_", Sys.Date(), ".docx"))

render_and_move(main_control, main_output)
render_and_move(appendix_control, appendix_output)

message("Rendered analysis 2 main manuscript to: ", main_output)
message("Rendered analysis 2 tables/appendix to: ", appendix_output)