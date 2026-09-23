# Re-saves a rendered DOCX through LibreOffice so Word opens it without an
# unreadable-content recovery prompt. LibreOffice is optional: callers receive
# a validated, unchanged DOCX when it is unavailable or conversion fails.

find_soffice <- function() {
  override <- Sys.getenv("SOFFICE", unset = "")
  if (nzchar(override)) {
    if (!file.exists(override)) {
      stop("SOFFICE is set to a path that does not exist: ", override,
           call. = FALSE)
    }
    return(override)
  }

  candidates <- c(
    "/Applications/LibreOffice.app/Contents/MacOS/soffice",
    path.expand("~/Applications/LibreOffice.app/Contents/MacOS/soffice"),
    Sys.which("soffice"),
    Sys.which("libreoffice")
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) == 0) {
    stop("LibreOffice not found. Install it, or set the SOFFICE environment ",
         "variable to the soffice binary. Searched:\n  ",
         paste(candidates[nzchar(candidates)], collapse = "\n  "),
         call. = FALSE)
  }
  found[1]
}

check_docx <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
  if (file.size(path) == 0) {
    stop(label, " is empty: ", path, call. = FALSE)
  }
  entries <- tryCatch(
    utils::unzip(path, list = TRUE)$Name,
    error = function(e) character(0)
  )
  if (!("word/document.xml" %in% entries)) {
    stop(label, " is not a readable DOCX: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

convert_via_soffice <- function(input, output) {
  soffice <- find_soffice()
  work <- tempfile("word_safe_docx_")
  outdir <- file.path(work, "out")
  profile <- file.path(work, "profile")
  dir.create(outdir, recursive = TRUE)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  args <- c(
    paste0("-env:UserInstallation=file://", profile),
    "--headless",
    "--norestore",
    "--convert-to", shQuote("docx:MS Word 2007 XML"),
    "--outdir", shQuote(outdir),
    shQuote(input)
  )
  log <- suppressWarnings(
    system2(
      soffice,
      args,
      env = c("LD_LIBRARY_PATH=", "DYLD_LIBRARY_PATH="),
      stdout = TRUE,
      stderr = TRUE,
      timeout = 180
    )
  )
  status <- attr(log, "status")
  if (!is.null(status) && status != 0) {
    stop("LibreOffice failed with status ", status, ".\n",
         paste(log, collapse = "\n"), call. = FALSE)
  }

  produced <- file.path(
    outdir,
    paste0(tools::file_path_sans_ext(basename(input)), ".docx")
  )
  check_docx(produced, "LibreOffice output")
  if (!file.copy(produced, output, overwrite = TRUE)) {
    stop("Could not write the converted file to ", output, call. = FALSE)
  }
  check_docx(output, "Converted file")
  invisible(TRUE)
}

plain_copy <- function(input, output) {
  if (normalizePath(input, mustWork = FALSE) ==
      normalizePath(output, mustWork = FALSE)) {
    return(invisible(TRUE))
  }
  if (!file.copy(input, output, overwrite = TRUE)) {
    stop("Could not copy ", input, " to ", output, call. = FALSE)
  }
  invisible(TRUE)
}

notify_missing <- function(output) {
  message(
    "LibreOffice is not installed, so ", basename(output),
    " in Reports/ is Quarto's own DOCX. Install LibreOffice or set SOFFICE ",
    "to automatically produce a Word-safe version."
  )
}

notify_failed <- function(output, detail) {
  message(
    "LibreOffice did not convert ", basename(output),
    "; Reports/ holds Quarto's own DOCX. Error: ", detail
  )
  warning("LibreOffice conversion failed. Reports/ holds the unconverted DOCX.",
          call. = FALSE)
}

word_safe_docx <- function(input, output = input, quiet = FALSE,
                           fallback = TRUE) {
  input <- normalizePath(input, mustWork = FALSE)
  check_docx(input, "Input")

  target_dir <- dirname(output)
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

  soffice <- tryCatch(find_soffice(), error = function(e) e)
  if (inherits(soffice, "error")) {
    if (!fallback) stop(conditionMessage(soffice), call. = FALSE)
    plain_copy(input, output)
    notify_missing(output)
    return(invisible(FALSE))
  }

  result <- tryCatch(convert_via_soffice(input, output), error = function(e) e)
  if (inherits(result, "error")) {
    if (!fallback) stop(conditionMessage(result), call. = FALSE)
    plain_copy(input, output)
    notify_failed(output, conditionMessage(result))
    return(invisible(FALSE))
  }

  if (!quiet) {
    cat("Re-saved", basename(input), "through LibreOffice as",
        basename(output), "\n")
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L && !interactive()) {
  argv <- commandArgs(trailingOnly = TRUE)
  if (length(argv) < 1 || length(argv) > 2) {
    stop("Usage: Rscript 002-Word-safe-docx.R input.docx [output.docx]",
         call. = FALSE)
  }
  word_safe_docx(argv[1], if (length(argv) == 2) argv[2] else argv[1],
                 fallback = FALSE)
}