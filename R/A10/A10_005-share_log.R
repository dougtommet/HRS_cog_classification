# A10_005: log of person-wave classification files shared from this project.
#
# One row per file. Dates and senders come from the email record
# (thread "HRS Algorithmic Dx", June to July 2026). Add a row for each new
# share, and set a10_shared_file to the file the recipient is using.
#
# Data files are gitignored (*.csv); this log is the record that a file
# was sent.

# Replacement file written by A7_075 (named with the run date; cut points
# selected by the pre-specified rule of 2026-10-21, classification on TR).
a10_new_file <- readRDS(here::here("R_objects", "A7_075_algorithm_summary.rds"))$export_file

a10_share_log <- tibble::tribble(
  ~date,        ~file,                       ~sent_by,                              ~sent_to,                                   ~status,                         ~local_copy,
  "2026-06-11", "Dx_for_LK-2026-06-11.csv",  "Douglas Tommet, forwarded by Richard Jones", "Lindsay Kobayashi",                   "Sent; replaced by the July file", "None. The local file with this name holds a rerun written 2026-10-07.",
  "2026-07-10", "Dx_for_LK-2026-07-10.csv",  "Douglas Tommet",                      "Lindsay Kobayashi (copy to Richard Jones)", "Sent; assumed used in the incidence analysis; to be replaced (decision 2026-10-21)", "Data/Dx_for_LK-2026-07-10.csv, saved from the email on 2026-10-19",
  "2026-10-17", "Dx_for_LK-2026-10-17.csv",  "Not sent",                            "Lindsay Kobayashi (planned)",              "Withdrawn 2026-10-19", "Data/Dx_for_LK-2026-10-17.csv (kept as a record)",
  "Not yet sent", a10_new_file,              "Not yet sent",                        "Lindsay Kobayashi (planned)",              "Prepared; replaces the July file once sent. Update this row when sent.", paste0("Data/", a10_new_file, " and its _README.md")
)

a10_share_notes <- c(
  "2026-06-11" = "Self-rated memory (D102) was not imputed. Person-waves with D102 missing had no class.",
  "2026-07-10" = "D102 singly imputed (A0_019). Cut points T < 37 and T < 48.",
  "2026-10-17" = "Cut points from the A7_070 grid optimum (T < 38.5 and T < 46.5). Not adopted (Rich, 2026-10-19): shares and agreement nearly the same as the July file.",
  "Not yet sent" = "T score rounded to an integer (TR); integer cut points selected by the pre-specified rule of 2026-10-21 (A7_070). Agreed with Doug Tommet and Lindsay Kobayashi."
)

# The file the recipient holds. When the new file is sent, set this to
# a10_new_file only if A10 should then compare later files with it.
a10_shared_file <- "Dx_for_LK-2026-07-10.csv"
