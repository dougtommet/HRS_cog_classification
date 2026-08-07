
hrs16_cog <- readRDS(here::here("R_objects", "A0_009_hrs16_cog.rds"))
hrs18_cog <- readRDS(here::here("R_objects", "A0_009_hrs18_cog.rds"))
hrs20_cog <- readRDS(here::here("R_objects", "A0_009_hrs20_cog.rds"))
hrs22_cog <- readRDS(here::here("R_objects", "A0_009_hrs22_cog.rds"))


# 2026-05-12: The data recoding for the 2016 wave was put in a function to support data recoding across waves
recode_cog_fx <- function(df, x) {

  new_col <- paste0("r", x, "D151")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D151")]] == 1  ~ 1, # Month - correct
      .data[[paste0(x, "D151")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D151")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D151")]] == 9  ~ 0  # refuse
      )
    )
  new_col <- paste0("r", x, "D152")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D152")]] == 1  ~ 1, # Day - correct
      .data[[paste0(x, "D152")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D152")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D152")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D153")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D153")]] == 1  ~ 1, # Year - correct
      .data[[paste0(x, "D153")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D153")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D153")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D154")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D154")]] == 1  ~ 1, # Day of week - correct
      .data[[paste0(x, "D154")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D154")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D154")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D155")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D155")]] == 1  ~ 1, # Scissors - correct
      .data[[paste0(x, "D155")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D155")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D155")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D156")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D156")]] == 1  ~ 1, # Cactus - correct
      .data[[paste0(x, "D156")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D156")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D156")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D157")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D157")]] == 1  ~ 1, # President - correct
      .data[[paste0(x, "D157")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D157")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D157")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D158")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D158")]] == 1  ~ 1, # Vice-president - correct
      .data[[paste0(x, "D158")]] == 5  ~ 0, # incorrect
      .data[[paste0(x, "D158")]] == 8  ~ 0, # don't know
      .data[[paste0(x, "D158")]] == 9  ~ 0  # refuse
    )
    )
  new_col <- paste0("r", x, "D142")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D142")]] == 93  ~ 1, # Serial 7's - 1 - correct
      .data[[paste0(x, "D142")]] == 998  ~ 0, # don't know
      .data[[paste0(x, "D142")]] == 999  ~ 0, # refuse
      !is.na(.data[[paste0(x, "D142")]])  ~ 0  # incorrect number
    )
    )
  new_col <- paste0("r", x, "D143")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D142")]]-7 == .data[[paste0(x, "D143")]]  ~ 1, # Serial 7's - 2 - correct
      .data[[paste0(x, "D143")]] == 998  ~ 0, # don't know
      .data[[paste0(x, "D143")]] == 999  ~ 0, # refuse
      !is.na(.data[[paste0(x, "D143")]])  ~ 0  # incorrect number
    )
    )
  new_col <- paste0("r", x, "D144")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D143")]]-7 == .data[[paste0(x, "D144")]]  ~ 1, # Serial 7's - 3 - correct
      .data[[paste0(x, "D144")]] == 998  ~ 0, # don't know
      .data[[paste0(x, "D144")]] == 999  ~ 0, # refuse
      !is.na(.data[[paste0(x, "D144")]])  ~ 0  # incorrect number
    )
    )
  new_col <- paste0("r", x, "D145")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D144")]]-7 == .data[[paste0(x, "D145")]]  ~ 1, # Serial 7's - 4 - correct
      .data[[paste0(x, "D145")]] == 998  ~ 0, # don't know
      .data[[paste0(x, "D145")]] == 999  ~ 0, # refuse
      !is.na(.data[[paste0(x, "D145")]])  ~ 0  # incorrect number
    )
    )
  new_col <- paste0("r", x, "D146")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D145")]]-7 == .data[[paste0(x, "D146")]]  ~ 1, # Serial 7's - 5 - correct
      .data[[paste0(x, "D146")]] == 998  ~ 0, # don't know
      .data[[paste0(x, "D146")]] == 999  ~ 0, # refuse
      !is.na(.data[[paste0(x, "D146")]])  ~ 0  # incorrect number
    )
    )
  new_col <- paste0("r", x, "D198")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D198")]] == 98  ~ 1, # Animal errors, considering the 98 to be a missing value
      is.na(.data[[paste0(x, "D198")]])  ~ 1, # Replacing the missing values with the median value
      TRUE ~ .data[[paste0(x, "D198")]]
    )
    )
  # There was an error in the recoding of vdcount involving xD129
  new_col <- paste0("r", x, "vdcount")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D124")]] == 1 | .data[[paste0(x, "D129")]] == 1 ~ 1, # Count backwards, 1st or 2nd try - correct
      .data[[paste0(x, "D124")]] == 5  ~ 0, # 1st try - incorrect
      .data[[paste0(x, "D124")]] == 6  ~ 0, # 1st try - wants to start over
      .data[[paste0(x, "D124")]] == 9  ~ 0, # 1st try - refuse
      .data[[paste0(x, "D129")]] == 5  ~ 0, # 2nd try - incorrect
      .data[[paste0(x, "D129")]] == 9  ~ 0, # 2nd try - refuse
    )
    )
  new_col <- paste0("r", x, "vdlfl1")
  df <- df %>%
    mutate(tempvar = .data[[paste0(x, "D196")]] - .data[[paste0("r", x, "D198")]], # Animals
      !!new_col := case_when(tempvar < 0 ~ 0, # If number of animals correct is less than number of errors, then 0
                             TRUE ~ tempvar)
    ) %>%
    select(-tempvar)

  new_col <- paste0("r", x, "vdwdimm")
  df <- df %>%
    mutate(!!new_col := .data[[paste0(x, "D174")]] # Word list, Immediate recall
    )
  new_col <- paste0("r", x, "vdwddel")
  df <- df %>%
    mutate(!!new_col := .data[[paste0(x, "D184")]], # Word list, Delayed recall
    )

  if (x!="Q") {
    new_col <- paste0("r", x, "vdexf7")
    df <- df %>%
      mutate(!!new_col := case_when(.data[[paste0(x, "NSSCORE")]] %in% c(996, 997, 999) ~ NA,
                                !is.na(.data[[paste0(x, "NSSCORE")]]) ~ .data[[paste0(x, "NSSCORE")]])
      )
  }

  new_col <- paste0("r", x, "vdori")
  df <- df %>%
    rowwise() %>%
    mutate(
      tempvar = sum(
        .data[[paste0("r", x, "D151")]],
        .data[[paste0("r", x, "D152")]],
        .data[[paste0("r", x, "D153")]],
        .data[[paste0("r", x, "D154")]],
        na.rm = "exclude"),
      !!new_col := case_when(is.na(.data[[paste0("r", x, "D151")]]) & is.na(.data[[paste0("r", x, "D152")]]) & is.na(.data[[paste0("r", x, "D153")]]) & is.na(.data[[paste0("r", x, "D154")]]) ~ NA,
                        TRUE ~ tempvar)
      ) %>%
    ungroup() %>%
    select(-tempvar)

  new_col <- paste0("r", x, "vdlfl2")
  df <- df %>%
    rowwise() %>%
    mutate(
      tempvar = sum(
        .data[[paste0("r", x, "D155")]],
        .data[[paste0("r", x, "D156")]],
        na.rm = "exclude"),
      !!new_col := case_when(is.na(.data[[paste0("r", x, "D155")]]) & is.na(.data[[paste0("r", x, "D156")]]) ~ NA,
                         TRUE ~ tempvar)
    ) %>%
    ungroup() %>%
    select(-tempvar)

  new_col <- paste0("r", x, "vdlfl3")
  df <- df %>%
    rowwise() %>%
    mutate(
      tempvar = sum(
        .data[[paste0("r", x, "D157")]],
        .data[[paste0("r", x, "D158")]],
        na.rm = "exclude"),
      !!new_col := case_when(is.na(.data[[paste0("r", x, "D157")]]) & is.na(.data[[paste0("r", x, "D158")]]) ~ NA,
                         TRUE ~ tempvar)
    ) %>%
    ungroup() %>%
    select(-tempvar)

  new_col <- paste0("r", x, "vdsevens")
  df <- df %>%
    rowwise() %>%
    mutate(
      tempvar = sum(
        .data[[paste0("r", x, "D142")]],
        .data[[paste0("r", x, "D143")]],
        .data[[paste0("r", x, "D144")]],
        .data[[paste0("r", x, "D145")]],
        .data[[paste0("r", x, "D146")]],
        na.rm = "exclude"),
      !!new_col := case_when(is.na(.data[[paste0("r", x, "D142")]]) & is.na(.data[[paste0("r", x, "D143")]]) & is.na(.data[[paste0("r", x, "D144")]]) & is.na(.data[[paste0("r", x, "D145")]]) & is.na(.data[[paste0("r", x, "D146")]]) ~ NA,
                           TRUE ~ tempvar)
    ) %>%
    ungroup() %>%
    select(-tempvar)

  if (x != "Q"){
    var_labels <- setNames(
      list(
        "Month",
        "Day",
        "Year",
        "Day of week",
        "Scissors",
        "Cactus",
        "President",
        "Vice-president",
        "Serial 7's - 1",
        "Serial 7's - 2",
        "Serial 7's - 3",
        "Serial 7's - 4",
        "Serial 7's - 5",
        "Animal naming errors",
        "Orientation to Time - number correct",
        "Object naming - Scissors, cactus",
        "Naming - President, Vice-president",
        "Count backwards from 20",
        "Serial 7's - Number correct",
        "Animal naming (correct - errors)",
        "Word recall - Immediate",
        "Word recall - Delayed",
        "Number series"
      ),
      c(paste0("r", x, c("D151", "D152", "D153", "D154", "D155", "D156", "D157", "D158",
                         "D142", "D143", "D144", "D145", "D146", "D198",
                         "vdori", "vdlfl2", "vdlfl3", "vdcount", "vdsevens", "vdlfl1",
                         "vdwdimm", "vdwddel", "vdexf7"))
      )
    )
  } else {
    var_labels <- setNames(
      list(
        "Month",
        "Day",
        "Year",
        "Day of week",
        "Scissors",
        "Cactus",
        "President",
        "Vice-president",
        "Serial 7's - 1",
        "Serial 7's - 2",
        "Serial 7's - 3",
        "Serial 7's - 4",
        "Serial 7's - 5",
        "Animal naming - errors",
        "Orientation to Time - number correct",
        "Object naming - Scissors, cactus",
        "Naming - President, Vice-president",
        "Count backwards from 20",
        "Serial 7's - Number correct",
        "Animal naming (correct - errors)",
        "Word recall - Immediate",
        "Word recall - Delayed"
      ),
      c(paste0("r", x, c("D151", "D152", "D153", "D154", "D155", "D156", "D157", "D158",
                         "D142", "D143", "D144", "D145", "D146", "D198",
                         "vdori", "vdlfl2", "vdlfl3", "vdcount", "vdsevens",
                         "vdlfl1", "vdwdimm", "vdwddel"))
      )
    )
  }

  # Columns that get Correct/Incorrect value labels
  correct_incorrect_cols <- paste0("r", x, c("D151", "D152", "D153", "D154", "D155",
                                             "D156", "D157", "D158", "D142", "D143",
                                             "D144", "D145", "D146", "vdcount"))

  val_labels <- setNames(
    rep(list(c("Correct" = 1, "Incorrect" = 0)), length(correct_incorrect_cols)),
    c(correct_incorrect_cols)
  )


  for (col in names(var_labels)) {
    df[[col]] <- labelled::set_variable_labels(df[[col]], .labels = var_labels[[col]])
  }

  for (col in names(val_labels)) {
    df[[col]] <- labelled::set_value_labels(df[[col]], .labels = val_labels[[col]])
  }

  df
}


hrs16_cog <- recode_cog_fx(hrs16_cog, "P")
hrs18_cog <- recode_cog_fx(hrs18_cog, "Q")
hrs20_cog <- recode_cog_fx(hrs20_cog, "R")
hrs22_cog <- recode_cog_fx(hrs22_cog, "S")



hrs16_cog_notes <- tribble(~v, ~notes,
                           "vdori", "This is the number of correct responses to the orientation to time items - month/day/year/day of week.  It ranges from 0 - 4.",
                           "vdlfl2", "This is the number of correct responses to the object naming items - scissors and cactus.  It ranges from 0 - 2.",
                           "vdlfl3", "This is the number of correct responses to the president/vice-president naming items.  It ranges from 0 - 2.",
                           "vdcount", "This is an indicator for whether the count backwards from 20 item was correct on either the first or second try.",
                           "vdsevens", "This is the number of correct responses to the serial sevens subtraction items.  It ranges from 0 - 5.",
                           "vdlfl1", "This is the score on the animal naming item. It is calculated as the number of animals correctly named minus the errors. Missing values on errors are set to 1 (the median value of errors).",
                           "vdwdimm", "This is the number of words correctly recalled on immediate recall.  It ranges from 0 - 10.",
                           "vdwddel", "This is the number of words correctly recalled on delayed recall.  It ranges from 0 - 10.",
                           "vdexf7", "This is the score on the number series test."
)



saveRDS(hrs16_cog,       here::here("R_objects", "A0_015_hrs16_cog.rds"))
saveRDS(hrs16_cog_notes, here::here("R_objects", "A0_015_hrs16_cog_notes.rds"))
saveRDS(hrs18_cog,       here::here("R_objects", "A0_015_hrs18_cog.rds"))
saveRDS(hrs20_cog,       here::here("R_objects", "A0_015_hrs20_cog.rds"))
saveRDS(hrs22_cog,       here::here("R_objects", "A0_015_hrs22_cog.rds"))







