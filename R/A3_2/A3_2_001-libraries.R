# A3_2_001: Packages for Analysis A3_2 (PMM scoring of HRS 2016-2022 Core).

pacman::p_load(tidyverse, haven, MplusAutomation, knitr)

# Plain numeric copy of a (possibly haven-labelled) vector
num <- function(x) as.numeric(haven::zap_labels(x))

# HHID/PN join key that ignores numeric vs. character storage
hhidpn_key <- function(hhid, pn) {
  stringr::str_c(sprintf("%06d", as.integer(as.character(hhid))),
                 sprintf("%03d", as.integer(as.character(pn))))
}

# Class 1-3 labels follow the HCAP16 knownclass coding (vs1hcapdxeap = 1, 2, 3)
a3_2_class_labels <- c("Normal", "MCI", "Dementia")
