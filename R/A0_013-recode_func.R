

hrs16_func <- readRDS(here::here("R_objects", "A0_009_hrs16_func.rds"))
hrs18_func <- readRDS(here::here("R_objects", "A0_009_hrs18_func.rds"))
hrs20_func <- readRDS(here::here("R_objects", "A0_009_hrs20_func.rds"))
hrs22_func <- readRDS(here::here("R_objects", "A0_009_hrs22_func.rds"))


# 2026-05-12: The data recoding for the 2016 wave was put in a function to support data recoding across waves

recode_func_fx <- function(df, x) {


  new_col <- paste0("r", x, "jorm1")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D506")]] == 1  & .data[[paste0(x, "D507")]] == 1 ~ 1,
        .data[[paste0(x, "D506")]] == 1  & .data[[paste0(x, "D507")]] == 2 ~ 2,
        .data[[paste0(x, "D506")]] == 2  ~ 3,
        .data[[paste0(x, "D506")]] == 3  & .data[[paste0(x, "D508")]] == 4 ~ 4,
        .data[[paste0(x, "D506")]] == 3  & .data[[paste0(x, "D508")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm2")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D509")]] == 1  & .data[[paste0(x, "D510")]] == 1 ~ 1,
        .data[[paste0(x, "D509")]] == 1  & .data[[paste0(x, "D510")]] == 2 ~ 2,
        .data[[paste0(x, "D509")]] == 2  ~ 3,
        .data[[paste0(x, "D509")]] == 3  & .data[[paste0(x, "D511")]] == 4 ~ 4,
        .data[[paste0(x, "D509")]] == 3  & .data[[paste0(x, "D511")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm3")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D512")]] == 1  & .data[[paste0(x, "D513")]] == 1 ~ 1,
        .data[[paste0(x, "D512")]] == 1  & .data[[paste0(x, "D513")]] == 2 ~ 2,
        .data[[paste0(x, "D512")]] == 2  ~ 3,
        .data[[paste0(x, "D512")]] == 3  & .data[[paste0(x, "D514")]] == 4 ~ 4,
        .data[[paste0(x, "D512")]] == 3  & .data[[paste0(x, "D514")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm4")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D515")]] == 1  & .data[[paste0(x, "D516")]] == 1 ~ 1,
        .data[[paste0(x, "D515")]] == 1  & .data[[paste0(x, "D516")]] == 2 ~ 2,
        .data[[paste0(x, "D515")]] == 2  ~ 3,
        .data[[paste0(x, "D515")]] == 3  & .data[[paste0(x, "D517")]] == 4 ~ 4,
        .data[[paste0(x, "D515")]] == 3  & .data[[paste0(x, "D517")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm5")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D518")]] == 1  & .data[[paste0(x, "D519")]] == 1 ~ 1,
        .data[[paste0(x, "D518")]] == 1  & .data[[paste0(x, "D519")]] == 2 ~ 2,
        .data[[paste0(x, "D518")]] == 2  ~ 3,
        .data[[paste0(x, "D518")]] == 3  & .data[[paste0(x, "D520")]] == 4 ~ 4,
        .data[[paste0(x, "D518")]] == 3  & .data[[paste0(x, "D520")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm6")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D521")]] == 1  & .data[[paste0(x, "D522")]] == 1 ~ 1,
        .data[[paste0(x, "D521")]] == 1  & .data[[paste0(x, "D522")]] == 2 ~ 2,
        .data[[paste0(x, "D521")]] == 2  ~ 3,
        .data[[paste0(x, "D521")]] == 3  & .data[[paste0(x, "D523")]] == 4 ~ 4,
        .data[[paste0(x, "D521")]] == 3  & .data[[paste0(x, "D523")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm7")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D524")]] == 1  & .data[[paste0(x, "D525")]] == 1 ~ 1,
        .data[[paste0(x, "D524")]] == 1  & .data[[paste0(x, "D525")]] == 2 ~ 2,
        .data[[paste0(x, "D524")]] == 2  ~ 3,
        .data[[paste0(x, "D524")]] == 3  & .data[[paste0(x, "D526")]] == 4 ~ 4,
        .data[[paste0(x, "D524")]] == 3  & .data[[paste0(x, "D526")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm8")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D527")]] == 1  & .data[[paste0(x, "D528")]] == 1 ~ 1,
        .data[[paste0(x, "D527")]] == 1  & .data[[paste0(x, "D528")]] == 2 ~ 2,
        .data[[paste0(x, "D527")]] == 2  ~ 3,
        .data[[paste0(x, "D527")]] == 3  & .data[[paste0(x, "D529")]] == 4 ~ 4,
        .data[[paste0(x, "D527")]] == 3  & .data[[paste0(x, "D529")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm9")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D530")]] == 1  & .data[[paste0(x, "D531")]] == 1 ~ 1,
        .data[[paste0(x, "D530")]] == 1  & .data[[paste0(x, "D531")]] == 2 ~ 2,
        .data[[paste0(x, "D530")]] == 2  ~ 3,
        .data[[paste0(x, "D530")]] == 3  & .data[[paste0(x, "D532")]] == 4 ~ 4,
        .data[[paste0(x, "D530")]] == 3  & .data[[paste0(x, "D532")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm10")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D533")]] == 1  & .data[[paste0(x, "D534")]] == 1 ~ 1,
        .data[[paste0(x, "D533")]] == 1  & .data[[paste0(x, "D534")]] == 2 ~ 2,
        .data[[paste0(x, "D533")]] == 2  ~ 3,
        .data[[paste0(x, "D533")]] == 3  & .data[[paste0(x, "D535")]] == 4 ~ 4,
        .data[[paste0(x, "D533")]] == 3  & .data[[paste0(x, "D535")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm11")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D536")]] == 1  & .data[[paste0(x, "D537")]] == 1 ~ 1,
        .data[[paste0(x, "D536")]] == 1  & .data[[paste0(x, "D537")]] == 2 ~ 2,
        .data[[paste0(x, "D536")]] == 2  ~ 3,
        .data[[paste0(x, "D536")]] == 3  & .data[[paste0(x, "D538")]] == 4 ~ 4,
        .data[[paste0(x, "D536")]] == 3  & .data[[paste0(x, "D538")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm12")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D539")]] == 1  & .data[[paste0(x, "D540")]] == 1 ~ 1,
        .data[[paste0(x, "D539")]] == 1  & .data[[paste0(x, "D540")]] == 2 ~ 2,
        .data[[paste0(x, "D539")]] == 2  ~ 3,
        .data[[paste0(x, "D539")]] == 3  & .data[[paste0(x, "D541")]] == 4 ~ 4,
        .data[[paste0(x, "D539")]] == 3  & .data[[paste0(x, "D541")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm13")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D542")]] == 1  & .data[[paste0(x, "D543")]] == 1 ~ 1,
        .data[[paste0(x, "D542")]] == 1  & .data[[paste0(x, "D543")]] == 2 ~ 2,
        .data[[paste0(x, "D542")]] == 2  ~ 3,
        .data[[paste0(x, "D542")]] == 3  & .data[[paste0(x, "D544")]] == 4 ~ 4,
        .data[[paste0(x, "D542")]] == 3  & .data[[paste0(x, "D544")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm14")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D545")]] == 1  & .data[[paste0(x, "D546")]] == 1 ~ 1,
        .data[[paste0(x, "D545")]] == 1  & .data[[paste0(x, "D546")]] == 2 ~ 2,
        .data[[paste0(x, "D545")]] == 2  ~ 3,
        .data[[paste0(x, "D545")]] == 3  & .data[[paste0(x, "D547")]] == 4 ~ 4,
        .data[[paste0(x, "D545")]] == 3  & .data[[paste0(x, "D547")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm15")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D548")]] == 1  & .data[[paste0(x, "D549")]] == 1 ~ 1,
        .data[[paste0(x, "D548")]] == 1  & .data[[paste0(x, "D549")]] == 2 ~ 2,
        .data[[paste0(x, "D548")]] == 2  ~ 3,
        .data[[paste0(x, "D548")]] == 3  & .data[[paste0(x, "D550")]] == 4 ~ 4,
        .data[[paste0(x, "D548")]] == 3  & .data[[paste0(x, "D550")]] == 5 ~ 5)
    )

  new_col <- paste0("r", x, "jorm16")
  df <- df %>%
    mutate(!!new_col := case_when(
        .data[[paste0(x, "D551")]] == 1  & .data[[paste0(x, "D552")]] == 1 ~ 1,
        .data[[paste0(x, "D551")]] == 1  & .data[[paste0(x, "D552")]] == 2 ~ 2,
        .data[[paste0(x, "D551")]] == 2  ~ 3,
        .data[[paste0(x, "D551")]] == 3  & .data[[paste0(x, "D553")]] == 4 ~ 4,
        .data[[paste0(x, "D551")]] == 3  & .data[[paste0(x, "D553")]] == 5 ~ 5),

    )

  new_col <- paste0("r", x, "jorm")
  df <- df %>%
    rowwise() %>%
    mutate(!!new_col := mean(c(
      .data[[paste0("r", x, "jorm1")]],
      .data[[paste0("r", x, "jorm2")]],
      .data[[paste0("r", x, "jorm3")]],
      .data[[paste0("r", x, "jorm4")]],
      .data[[paste0("r", x, "jorm5")]],
      .data[[paste0("r", x, "jorm6")]],
      .data[[paste0("r", x, "jorm7")]],
      .data[[paste0("r", x, "jorm8")]],
      .data[[paste0("r", x, "jorm9")]],
      .data[[paste0("r", x, "jorm10")]],
      .data[[paste0("r", x, "jorm11")]],
      .data[[paste0("r", x, "jorm12")]],
      .data[[paste0("r", x, "jorm13")]],
      .data[[paste0("r", x, "jorm14")]],
      .data[[paste0("r", x, "jorm15")]],
      .data[[paste0("r", x, "jorm16")]]), na.rm=TRUE)) %>%
    ungroup()

  # Rich Modified 2025-07-21
  new_col <- paste0("r", x, "self_concerns")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D101")]] %in% c(4, 5, 8, 9) ~ 1, # fair or poor memory
      .data[[paste0(x, "D102")]] %in% c(3, 8, 9) ~ 1,    # worse than 2 years ago
      .data[[paste0(x, "D101")]] %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ))

  # 2026-05-12 adding in three items about wandering off/being left alone
  new_col <- paste0("r", x, "D554")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D554")]] == 1  ~ 1,
      .data[[paste0(x, "D554")]] == 5  ~ 0,
      .data[[paste0(x, "D554")]] == 8  ~ NA,
      .data[[paste0(x, "D554")]] == 9  ~ NA
      )
    )
  new_col <- paste0("r", x, "D555")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D555")]] == 1  ~ 1,
      .data[[paste0(x, "D555")]] == 5  ~ 0,
      .data[[paste0(x, "D555")]] == 8  ~ NA,
      .data[[paste0(x, "D555")]] == 9  ~ NA
    )
  )
  # This item is being reversed, so that 1 means the participant can't be left alone
  new_col <- paste0("r", x, "D556")
  df <- df %>%
    mutate(!!new_col := case_when(
      .data[[paste0(x, "D556")]] == 1  ~ 0,
      .data[[paste0(x, "D556")]] == 5  ~ 1,
      .data[[paste0(x, "D556")]] == 8  ~ NA,
      .data[[paste0(x, "D556")]] == 9  ~ NA
    )
    )




  var_list <- c(paste0("r", x, "jorm", 1:16),
                paste0("r", x, "jorm"),
                paste0("r", x, "self_concerns"),
                paste0("r", x, c("D554", "D555", "D556"))
                )
  var_labels <- setNames(
    list(
      "Remembering things about family",
      "Remembering things that happened recently",
      "Recalling conversations a few day later",
      "Remembering telephone number",
      "Remembering day and month",
      "Remembering where things are kept",
      "Remembering where to find things",
      "Knowing how to work familar machines around the house",
      "Learning to use a new gadget",
      "Learning new things in general",
      "Following a story in a book or on TV",
      "Making decisions on everyday matters",
      "Handling money for shopping",
      "Handling financial matters",
      "Handling everyday arthimetic problems",
      "Using intelligence to understand what's going on",
      "Jorm score (HRS)",
      "Composite of (Compared to two years ago, would you say your memory is ...?) OR (How would you rate your memory at the present time?)",
      "Ever get lost in a familiar environment?",
      "Ever wander off and not return by [herself/himself]?",
      "Can't be left alone for an hour or so?"

    ),
    var_list
  )
  jorm_val_labels <- setNames(
    rep(list(c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3,
               "A bit worse" = 4, "Much worse" = 5)), 16),
    paste0("r", x, "jorm", 1:16)
  )

  self_concerns_val_labels <- setNames(
    list(c("Worse or (Fair or Poor)" = 1,
                      "(Same or Better) AND (Excellent, Very Good, Good)" = 0)),
    paste0("r", x, "self_concerns")
  )
  wander_val_labels <- setNames(
    rep(list(c("Yes" = 1, "No" = 0)), 3),
    paste0("r", x, c("D554", "D555", "D556"))
  )

  val_labels <- c(jorm_val_labels, self_concerns_val_labels, wander_val_labels)


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

hrs16_func <- recode_func_fx(hrs16_func, "P")
hrs18_func <- recode_func_fx(hrs18_func, "Q")
hrs20_func <- recode_func_fx(hrs20_func, "R")
hrs22_func <- recode_func_fx(hrs22_func, "S")

saveRDS(hrs16_func, here::here("R_objects", "A0_013_hrs16_func.rds"))
saveRDS(hrs18_func, here::here("R_objects", "A0_013_hrs18_func.rds"))
saveRDS(hrs20_func, here::here("R_objects", "A0_013_hrs20_func.rds"))
saveRDS(hrs22_func, here::here("R_objects", "A0_013_hrs22_func.rds"))

