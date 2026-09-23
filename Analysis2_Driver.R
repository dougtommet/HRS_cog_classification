#!/usr/bin/env Rscript

setwd(here::here())

source(here::here("R", "MS_MAIN", "002-Word-safe-docx.R"))

render_and_move <- function(control_file, output_file, reference_doc) {
  quarto::quarto_render(
    input = control_file,
    output_format = "docx",
    metadata = list(
      format = list(docx = list(`reference-doc` = reference_doc))
    )
  )

  if (fs::file_exists(output_file)) {
    fs::file_delete(output_file)
  }

  fs::file_move(
    fs::path_ext_set(control_file, "docx"),
    output_file
  )

  word_safe_docx(output_file, quiet = TRUE)
}

fs::dir_create(here::here("Reports"))

reference_doc <- here::here("reference_manuscript.docx")
main_control <- here::here("R", "MS_MAIN", "MS_Main_Control.qmd")
main_output <- here::here("Reports", stringr::str_c("MS_Main_", Sys.Date(), ".docx"))

appendix_control <- here::here("R", "MS_MAIN", "MS_Tab_Fig_Apndx_Control.qmd")
appendix_output <- here::here("Reports", stringr::str_c("MS_Tab_Fig_Apndx_", Sys.Date(), ".docx"))

manuscript_appendices <- tibble::tribble(
  ~control_file, ~output_file, ~description,
  here::here("R", "MS_MAIN", "MS_Appendix-1-Core-Algorithm.qmd"),
  here::here("Reports", stringr::str_c("MS_Appendix_1_", Sys.Date(), ".docx")),
  "Appendix 1: Core algorithm",
  here::here("R", "MS_MAIN", "MS_Appendix-2-PMM.qmd"),
  here::here("Reports", stringr::str_c("MS_Appendix_2_", Sys.Date(), ".docx")),
  "Appendix 2: PMM",
  here::here("R", "MS_MAIN", "MS_Appendix-3-Other-Study-Kappas.qmd"),
  here::here("Reports", stringr::str_c("MS_Appendix_3_", Sys.Date(), ".docx")),
  "Appendix 3: historical kappa comparisons"
)

render_and_move(main_control, main_output, reference_doc)
render_and_move(appendix_control, appendix_output, reference_doc)

purrr::pwalk(
  manuscript_appendices,
  function(control_file, output_file, description) {
    render_and_move(control_file, output_file, reference_doc)
    message("Rendered analysis 2 ", description, " to: ", output_file)
  }
)

message("Rendered analysis 2 main manuscript to: ", main_output)
message("Rendered analysis 2 tables and figures to: ", appendix_output)