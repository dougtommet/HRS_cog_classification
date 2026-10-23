# PMM_091_mplus_run_cache.R
#
# Reuse Mplus results in Analysis A3 when a model's syntax and data have not
# changed (decided by Rich, 2026-10-22).
#
# pmm_mplus_run() replaces MplusAutomation::mplusModeler(..., run = 1). It
# follows References/Mplus_Rules.md section 1: write the .inp and .dat with
# run = 0, then run with MplusAutomation::runModels() only when needed.
#
# A run is skipped when all of these hold:
#   1. the refit switch is off (Rscript Analysis3_Driver.R --refit turns it on);
#   2. every required output file (.out, .h5, saved data) exists;
#   3. the MD5 hashes of the newly written .inp and its DATA file match the
#      hashes recorded after the last successful run (<model>.fingerprint).
# Otherwise the model is refit and, if it ends without an Mplus error and
# rewrites every required output, a new fingerprint is recorded.
#
# Changes cascade: a refit that changes estimates changes the syntax that
# later models build from the H5 file (norms -> calibration -> scoring), so
# those later models also refit.
#
# The hashes are computed from file bytes only; no data values are read here.

pmm_refit_requested <- function() {
  tolower(Sys.getenv("PMM_REFIT", "false")) %in% c("true", "1", "yes")
}

# Data file named in the DATA section of an Mplus input file (not SAVEDATA).
pmm_inp_data_file <- function(inp_path) {
  lines <- readLines(inp_path, warn = FALSE)
  start <- grep("^\\s*DATA\\s*:", lines, ignore.case = TRUE)
  if (length(start) != 1L) stop("Cannot find one DATA section in ", inp_path)
  section_heads <- grep(paste0("^\\s*(TITLE|DATA|VARIABLE|DEFINE|ANALYSIS|MODEL|OUTPUT|",
                               "SAVEDATA|PLOT|MONTECARLO)\\b[^:]*:"), lines, ignore.case = TRUE)
  end <- section_heads[section_heads > start]
  end <- if (length(end)) end[1] - 1L else length(lines)
  data_text <- paste(lines[start:end], collapse = " ")
  m <- regmatches(data_text, regexec("FILE\\s*(=|IS)\\s*\"?([^\";]+)\"?\\s*;",
                                     data_text, ignore.case = TRUE))[[1]]
  if (length(m) < 3L) stop("Cannot find the data FILE in ", inp_path)
  file.path(dirname(inp_path), trimws(m[3]))
}

pmm_mplus_fingerprint <- function(inp_path) {
  dat_path <- pmm_inp_data_file(inp_path)
  if (!file.exists(dat_path)) stop("Mplus data file not written: ", dat_path)
  c(inp = unname(tools::md5sum(inp_path)), dat = unname(tools::md5sum(dat_path)))
}

pmm_read_fingerprint <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- read.dcf(path)
  if (!all(c("inp", "dat") %in% colnames(x))) return(NULL)
  c(inp = unname(x[1, "inp"]), dat = unname(x[1, "dat"]))
}

# object: an mplusObject. modelout: the .inp file name, written in the current
# working directory (the model scripts setwd() into their output folder).
# outputs: other result files the downstream code reads (H5, saved data).
pmm_mplus_run <- function(object, modelout, outputs = character()) {
  inp_path <- normalizePath(modelout, mustWork = FALSE)
  out_path <- sub("\\.inp$", ".out", inp_path)
  fp_path <- sub("\\.inp$", ".fingerprint", inp_path)
  required <- c(out_path, file.path(dirname(inp_path), outputs))

  MplusAutomation::mplusModeler(object, modelout = modelout, run = 0L, writeData = "always")
  current <- pmm_mplus_fingerprint(inp_path)
  previous <- pmm_read_fingerprint(fp_path)

  reason <- if (pmm_refit_requested()) {
    "refit requested (--refit)"
  } else if (!all(file.exists(required))) {
    "output missing"
  } else if (is.null(previous)) {
    "no fingerprint from a previous run"
  } else if (!identical(current, previous)) {
    paste0("changed since last fit (",
           paste(names(current)[current != previous], collapse = ", "), ")")
  } else {
    NA_character_
  }

  if (is.na(reason)) {
    message("Mplus ", basename(inp_path), ": syntax and data unchanged; reusing saved output.")
    return(invisible(FALSE))
  }

  message("Mplus ", basename(inp_path), ": fitting (", reason, ").")
  if (file.exists(fp_path)) file.remove(fp_path)
  started <- Sys.time()
  MplusAutomation::runModels(target = inp_path, logFile = NULL)

  out_lines <- if (file.exists(out_path)) readLines(out_path, warn = FALSE) else character()
  fresh <- file.exists(required) & file.mtime(required) >= started - 1
  problem <- c(
    if (!length(out_lines)) "no .out file",
    if (any(grepl("\\*\\*\\* ERROR", out_lines))) "Mplus reported an error (see .out)",
    if (!all(fresh)) paste("not rewritten:", paste(basename(required[!fresh]), collapse = ", "))
  )
  if (length(problem)) {
    warning("Mplus ", basename(inp_path), ": ", paste(problem, collapse = "; "),
            ". No fingerprint recorded, so the next run will refit.", call. = FALSE)
    return(invisible(TRUE))
  }
  write.dcf(data.frame(inp = current[["inp"]], dat = current[["dat"]],
                       fitted = format(started, "%Y-%m-%d %H:%M:%S %Z")), fp_path)
  invisible(TRUE)
}
