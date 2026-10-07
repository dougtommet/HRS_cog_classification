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

# Combined PDF ---------------------------------------------------------------
# Converts the five rendered DOCX files to PDF with LibreOffice (headless) and
# joins them, in reading order, into Reports/MS_Combined_[date].pdf. Interim
# PDFs are written to a temporary folder and deleted. If LibreOffice or the
# qpdf R package is missing, the driver warns and skips this step; the DOCX
# outputs above are unaffected.

combined_inputs <- c(main_output, appendix_output, manuscript_appendices$output_file)
combined_output <- here::here("Reports", stringr::str_c("MS_Combined_", Sys.Date(), ".pdf"))

find_soffice <- function() {
  candidates <- c(
    Sys.which("soffice"),
    Sys.which("libreoffice"),
    "/Applications/LibreOffice.app/Contents/MacOS/soffice",
    "C:/Program Files/LibreOffice/program/soffice.exe"
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0) NA_character_ else unname(candidates[[1]])
}

soffice <- find_soffice()

if (is.na(soffice)) {
  warning(
    "Skipped the combined PDF: LibreOffice was not found.\n",
    "Install LibreOffice (https://www.libreoffice.org/download/, or on a Mac ",
    "`brew install --cask libreoffice`) and rerun `Rscript Analysis2_Driver.R`.",
    call. = FALSE
  )
} else if (!requireNamespace("qpdf", quietly = TRUE)) {
  warning(
    "Skipped the combined PDF: the R package 'qpdf' is not installed.\n",
    "Run install.packages(\"qpdf\") and rerun `Rscript Analysis2_Driver.R`.",
    call. = FALSE
  )
} else {
  pdf_dir <- fs::path(tempdir(), "analysis2_pdf")
  fs::dir_create(pdf_dir)
  # A separate LibreOffice profile lets the conversion run while LibreOffice is open.
  lo_profile <- fs::path(tempdir(), "analysis2_lo_profile")
  lo_status <- system2(
    soffice,
    c(
      stringr::str_c("-env:UserInstallation=file://", gsub("^([A-Za-z]):", "/\\1:", lo_profile)),
      "--headless", "--convert-to", "pdf", "--outdir", shQuote(pdf_dir),
      shQuote(combined_inputs)
    ),
    stdout = FALSE, stderr = FALSE,
    # R puts its own library folders on LD_LIBRARY_PATH, which stops LibreOffice
    # from loading its libraries on Linux. Clear it for this call only.
    env = if (.Platform$OS.type == "unix") "LD_LIBRARY_PATH=" else character()
  )
  interim_pdfs <- fs::path(pdf_dir, fs::path_ext_set(fs::path_file(combined_inputs), "pdf"))

  if (lo_status != 0 || !all(fs::file_exists(interim_pdfs))) {
    warning(
      "Skipped the combined PDF: LibreOffice did not convert every DOCX file (exit status ",
      lo_status, "). Missing: ",
      paste(fs::path_file(interim_pdfs[!fs::file_exists(interim_pdfs)]), collapse = ", "),
      call. = FALSE
    )
  } else {
    qpdf::pdf_combine(input = interim_pdfs, output = combined_output)
    message("Rendered analysis 2 combined PDF to: ", combined_output)
  }

  fs::dir_delete(pdf_dir)
  if (fs::dir_exists(lo_profile)) fs::dir_delete(lo_profile)
}
