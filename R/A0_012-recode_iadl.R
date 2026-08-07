
hrs16_iadl <- readRDS(here::here("R_objects", "A0_009_hrs16_iadl.rds"))
hrs18_iadl <- readRDS(here::here("R_objects", "A0_009_hrs18_iadl.rds"))
hrs20_iadl <- readRDS(here::here("R_objects", "A0_009_hrs20_iadl.rds"))
hrs22_iadl <- readRDS(here::here("R_objects", "A0_009_hrs22_iadl.rds"))


# The recoding in this file was updated from the PMM_041_Data_cleaning_functional_items.R file
# 2026-05-12: The data recoding for the 2016 wave was put in a function to support data recoding across waves

recode_iadl_fx <- function(df, x) {

  new_col <- paste0("r", x, "G014")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G014")]] == 1  ~ 1,
      .data[[paste0(x, "G014")]] == 5  ~ 0,
      .data[[paste0(x, "G014")]] == 6  ~ 1,
      .data[[paste0(x, "G014")]] == 7  ~ NA,
      .data[[paste0(x, "G014")]] == 8  ~ NA,
      .data[[paste0(x, "G014")]] == 9  ~ 1,
      is.na(.data[[paste0(x, "G014")]]) & .data[[paste0(x, "G013")]] == 0 ~ 0)
      )
  new_col <- paste0("r", x, "G021")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G021")]] == 1  ~ 1,
      .data[[paste0(x, "G021")]] == 5  ~ 0,
      .data[[paste0(x, "G021")]] == 6  ~ 1,
      .data[[paste0(x, "G021")]] == 7  ~ NA,
      .data[[paste0(x, "G021")]] == 8  ~ NA,
      .data[[paste0(x, "G021")]] == 9  ~ 1,
      is.na(.data[[paste0(x, "G021")]]) & .data[[paste0(x, "G013")]] == 0 ~ 0,
      .data[[paste0(x, "G014")]] == 5 & (is.na(.data[[paste0(x, "G021")]]) |.data[[paste0(x, "G021")]] == 7 | .data[[paste0(x, "G021")]] == 8) ~ 0)
    )
  new_col <- paste0("r", x, "G023")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G023")]] == 1  ~ 1,
      .data[[paste0(x, "G023")]] == 5  ~ 0,
      .data[[paste0(x, "G023")]] == 6  ~ 1,
      .data[[paste0(x, "G023")]] == 7  ~ NA,
      .data[[paste0(x, "G023")]] == 8  ~ NA,
      .data[[paste0(x, "G023")]] == 9  ~ 1,
      is.na(.data[[paste0(x, "G023")]]) & .data[[paste0(x, "G013")]] == 0 ~ 0,
      .data[[paste0(x, "G014")]] == 5 & (is.na(.data[[paste0(x, "G023")]]) |.data[[paste0(x, "G023")]] == 7 | .data[[paste0(x, "G023")]] == 8) ~ 0)
    )
  new_col <- paste0("r", x, "G030")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G030")]] == 1  ~ 1,
      .data[[paste0(x, "G030")]] == 5  ~ 0,
      .data[[paste0(x, "G030")]] == 6  ~ 1,
      .data[[paste0(x, "G030")]] == 7  ~ NA,
      .data[[paste0(x, "G030")]] == 8  ~ NA,
      .data[[paste0(x, "G030")]] == 9  ~ 1,
      is.na(.data[[paste0(x, "G030")]]) & .data[[paste0(x, "G013")]] == 0 ~ 0,
      .data[[paste0(x, "G014")]] == 5 & (is.na(.data[[paste0(x, "G030")]]) |.data[[paste0(x, "G030")]] == 7 | .data[[paste0(x, "G030")]] == 8) ~ 0)
    )
  if (x!="S") {
    new_col <- paste0("r", x, "G040")
    df <- df %>%
      mutate(!!new_col := case_when(
        .data[[paste0(x, "G040")]] == 1  ~ 1,
        .data[[paste0(x, "G040")]] == 5  ~ 0,
        .data[[paste0(x, "G040")]] == 6  ~ 1,
        .data[[paste0(x, "G040")]] == 7  ~ NA,
        .data[[paste0(x, "G040")]] == 8  ~ NA,
        .data[[paste0(x, "G040")]] == 9  ~ 1
        )
      )
  }
  new_col <- paste0("r", x, "G041")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G041")]] == 1  ~ 1,
      .data[[paste0(x, "G041")]] == 5  ~ 0,
      .data[[paste0(x, "G041")]] == 6  ~ 1,
      .data[[paste0(x, "G041")]] == 7  ~ NA,
      .data[[paste0(x, "G041")]] == 8  ~ NA,
      .data[[paste0(x, "G041")]] == 9  ~ 1
      )
    )
  new_col <- paste0("r", x, "G044")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G044")]] == 1  ~ 1,
      .data[[paste0(x, "G044")]] == 5  ~ 0,
      .data[[paste0(x, "G044")]] == 6  ~ 1,
      .data[[paste0(x, "G044")]] == 7  ~ NA,
      .data[[paste0(x, "G044")]] == 8  ~ NA,
      .data[[paste0(x, "G044")]] == 9  ~ 1
    )
    )
  new_col <- paste0("r", x, "G047")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G047")]] == 1  ~ 1,
      .data[[paste0(x, "G047")]] == 5  ~ 0,
      .data[[paste0(x, "G047")]] == 6  ~ 1,
      .data[[paste0(x, "G047")]] == 7  ~ NA,
      .data[[paste0(x, "G047")]] == 8  ~ NA,
      .data[[paste0(x, "G047")]] == 9  ~ 1
    )
    )
  new_col <- paste0("r", x, "G050")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G050")]] == 1  ~ 1,
      .data[[paste0(x, "G050")]] == 5  ~ 0,
      .data[[paste0(x, "G050")]] == 6  ~ 1,
      # .data[[paste0(x, "G050")]] == 7  ~ NA,
      .data[[paste0(x, "G050")]] == 8  ~ NA,
      .data[[paste0(x, "G050")]] == 9  ~ 1,
      .data[[paste0(x, "G050")]] == 7 & .data[[paste0(x, "G051")]] == 1 ~ 1,
      .data[[paste0(x, "G050")]] == 7 & .data[[paste0(x, "G051")]] == 5 ~ 0,
      .data[[paste0(x, "G050")]] == 7 & .data[[paste0(x, "G051")]] == 8 ~ NA,
    )
    )
  new_col <- paste0("r", x, "G059")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "G059")]] == 1  ~ 1,
      .data[[paste0(x, "G059")]] == 5  ~ 0,
      .data[[paste0(x, "G059")]] == 6  ~ 1,
      .data[[paste0(x, "G059")]] == 7  ~ NA,
      .data[[paste0(x, "G059")]] == 8  ~ NA,
      .data[[paste0(x, "G059")]] == 9  ~ 1
    )
    )
  if (x=="S") {
    new_col <- paste0("r", x, "iadl_imp")
    df <- df %>%
      rowwise() %>%
      mutate(!!new_col := sum(
        .data[[paste0("r", x, "G014")]],
        .data[[paste0("r", x, "G021")]],
        .data[[paste0("r", x, "G023")]],
        .data[[paste0("r", x, "G030")]],
        .data[[paste0("r", x, "G041")]],
        .data[[paste0("r", x, "G044")]],
        .data[[paste0("r", x, "G047")]],
        .data[[paste0("r", x, "G050")]],
        .data[[paste0("r", x, "G059")]],
        na.rm = TRUE
      )) %>%
      ungroup()
  } else {
    new_col <- paste0("r", x, "iadl_imp")
    df <- df %>%
      rowwise() %>%
      mutate(!!new_col := sum(
        .data[[paste0("r", x, "G014")]],
        .data[[paste0("r", x, "G021")]],
        .data[[paste0("r", x, "G023")]],
        .data[[paste0("r", x, "G030")]],
        .data[[paste0("r", x, "G040")]],
        .data[[paste0("r", x, "G041")]],
        .data[[paste0("r", x, "G044")]],
        .data[[paste0("r", x, "G047")]],
        .data[[paste0("r", x, "G050")]],
        .data[[paste0("r", x, "G059")]],
        na.rm = TRUE
      )) %>%
      ungroup()
  }


  if (x=="S") {
    var_list <- c("G014", "G021", "G023", "G030", "G041", "G044", "G047", "G050", "G059")
    var_labels <- setNames(
      list(
        "ADL: Difficulty Dressing",
        "ADL: Difficulty Bathing",
        "ADL: Difficulty Eating",
        "ADL: Difficulty Using Toilet",
        "IADL: Difficulty Meal Prep",
        "IADL: Difficulty Grocery Shopping",
        "IADL: Difficulty Making Phone Calls",
        "IADL: Difficulty Taking Meds",
        "IADL: Difficulty Managing Money",
        "Sum of ADL/IADL impairments"
      ),
      c(paste0("r", x, var_list),
        paste0("r", x, "iadl_imp"))
    )

    val_labels <- setNames(
      rep(list(c("Impaired" = 1, "Not impaired" = 0)), length(var_list)),
      paste0("r", x,  var_list)
    )
  } else {
    var_list <- c("G014", "G021", "G023", "G030", "G040", "G041", "G044", "G047", "G050", "G059")
    var_labels <- setNames(
      list(
        "ADL: Difficulty Dressing",
        "ADL: Difficulty Bathing",
        "ADL: Difficulty Eating",
        "ADL: Difficulty Using Toilet",
        "IADL: Difficulty Using Maps",
        "IADL: Difficulty Meal Prep",
        "IADL: Difficulty Grocery Shopping",
        "IADL: Difficulty Making Phone Calls",
        "IADL: Difficulty Taking Meds",
        "IADL: Difficulty Managing Money",
        "Sum of ADL/IADL impairments"
      ),
      c(paste0("r", x, var_list),
        paste0("r", x, "iadl_imp"))
    )

    val_labels <- setNames(
      rep(list(c("Impaired" = 1, "Not impaired" = 0)), length(var_list)),
      paste0("r", x,  var_list)
    )
  }

  # Apply variable labels
  for (col in names(var_labels)) {
    df[[col]] <- labelled::set_variable_labels(df[[col]], .labels = var_labels[[col]])
  }

  # Apply value labels
  for (col in names(val_labels)) {
    df[[col]] <- labelled::set_value_labels(df[[col]], .labels = val_labels[[col]])
  }
  df


}

hrs16_iadl <- recode_iadl_fx(hrs16_iadl, "P")
hrs18_iadl <- recode_iadl_fx(hrs18_iadl, "Q")
hrs20_iadl <- recode_iadl_fx(hrs20_iadl, "R")
hrs22_iadl <- recode_iadl_fx(hrs22_iadl, "S")

saveRDS(hrs16_iadl, here::here("R_objects", "A0_012_hrs16_iadl.rds"))
saveRDS(hrs18_iadl, here::here("R_objects", "A0_012_hrs18_iadl.rds"))
saveRDS(hrs20_iadl, here::here("R_objects", "A0_012_hrs20_iadl.rds"))
saveRDS(hrs22_iadl, here::here("R_objects", "A0_012_hrs22_iadl.rds"))

