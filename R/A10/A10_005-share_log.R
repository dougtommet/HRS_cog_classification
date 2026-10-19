# A10_005: log of person-wave classification files shared from this project.
#
# One row per file. Dates and senders come from the email record
# (thread "HRS Algorithmic Dx", June to July 2026). Add a row for each new
# share, and set a10_shared_file to the file the recipient is using and
# a10_current_file to the file proposed as its replacement.
#
# Data files are gitignored (*.csv); this log is the record that a file
# was sent.

a10_share_log <- tibble::tribble(
  ~date,        ~file,                       ~sent_by,                              ~sent_to,                                   ~status,                         ~local_copy,
  "2026-06-11", "Dx_for_LK-2026-06-11.csv",  "Douglas Tommet, forwarded by Richard Jones", "Lindsay Kobayashi",                   "Sent; replaced by the July file", "None. The local file with this name holds a rerun written 2026-10-07.",
  "2026-07-10", "Dx_for_LK-2026-07-10.csv",  "Douglas Tommet",                      "Lindsay Kobayashi (copy to Richard Jones)", "Sent; assumed used in the incidence analysis", "Data/Dx_for_LK-2026-07-10.csv, saved from the email on 2026-10-19",
  "2026-10-17", "Dx_for_LK-2026-10-17.csv",  "Not sent",                            "Lindsay Kobayashi (planned)",              "Prepared by Analysis A7",        "Data/Dx_for_LK-2026-10-17.csv"
)

a10_share_notes <- c(
  "2026-06-11" = "Self-rated memory (D102) was not imputed. Person-waves with D102 missing had no class.",
  "2026-07-10" = "D102 singly imputed (A0_019). Cut points T < 37 and T < 48.",
  "2026-10-17" = "Cut points re-derived by prevalence matching (A7_070). All other steps as in the July file."
)

# The file the recipient holds, and the file proposed to replace it.
a10_shared_file  <- "Dx_for_LK-2026-07-10.csv"
a10_current_file <- "Dx_for_LK-2026-10-17.csv"
