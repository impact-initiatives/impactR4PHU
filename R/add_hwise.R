
#' add_hwise
#'
#' This function will calculate the HWISE-4 and HWISE-12 scores and categories based on user-specified column names and response values.
#'
#' @param data
#' @param hwise_worry_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently did you or anyone in your household worry you would not have enough water for all of your household needs?
#' @param hwise_plans_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have you or anyone in your household had to change schedules or plans due to problems with your water situation? (Activities that may have been interrupted include caring for others, doing household chores, agricultural work, income-generating activities, sleeping, etc.)"
#' @param hwise_hands_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have you or anyone in your household had to go without washing hands after dirty activities (e.g., defecating or changing diapers, cleaning animal dung) because of problems with water?"
#' @param hwise_drink_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently has there not been as much water to drink as you would like for you or anyone in your household?"
#' @param hwise_interrupt_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently has your main water source been interrupted or limited (e.g. water pressure, less water than expected, river dried up)?"
#' @param hwise_clothes_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have problems with water meant that clothes could not be washed?"
#' @param hwise_food_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have you or anyone in your household had to change what was being eaten because there were problems with water (e.g., for washing foods, cooking, etc.)?"
#' @param hwise_body_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have you or anyone in your household had to go without washing their body because of problems with water (e.g., not enough water, dirty, unsafe)?"
#' @param hwise_angry_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently did you or anyone in your household feel angry about your water situation?"
#' @param hwise_sleep_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have you or anyone in your household gone to sleep thirsty because there wasn’t any water to drink?"
#' @param hwise_none_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently has there been no useable or drinkable water whatsoever in your household?"
#' @param hwise_shame_col Column name as a character value for HWISE item "In the last 4 weeks, how frequently have problems with water caused you or anyone in your household to feel ashamed/excluded/stigmatized?"
#' @param never_val Character value for survey response option "Never (0 times)"
#' @param rarely_val Character value for survey response option "Rarely (1-2 times)"
#' @param sometimes_val Character value for survey response option "Sometimes (3-10 times)"
#' @param often_val Character value for survey response option "Often (11-20 times)"
#' @param always_val Character value for survey response option "Always (>20 times)"
#'
#' @returns A data frame with added HWISE-4 and HWISE-12 scores and categories
#' @export
#'
#' @examples
#' #' # Example usage:
#' #' # Assuming df is your data frame with appropriate columns
#' #' df <- data.frame(
#' #' #'   id = 1:5,
#' # #' #'   worry = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)"),
#' # #' #'   plans = c("Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)"),
#' # #' #'   hands = c("Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)"),
#' # #' #'   drink = c("Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)"),
#' # #' #'   interrupt = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)"),
#' # #' #'   clothes = c("Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)"),
#' # #' #'   food = c("Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)"),
#' # #' #'   body = c("Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)"),
#' # #' #'   angry = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)"),
#' # #' #'   sleep = c("Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)"),
#' # #' #'   none = c("Sometimes (3-10 times)", "Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)"),
#' # #' #'   shame = c("Often (11-20 times)", "Always (>20 times)", "Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)")
#' # #' # )
#' # #' #
#' # #' # df_hwise <- add_hwise(
#' # #' #   data = df,
#' # #' #   hwise_worry_col = "worry",
#' # #' #   hwise_plans_col = "plans",
#' # #' #   hwise_hands_col = "hands",
#' # #' #   hwise_drink_col = "drink",
#' # #' #   hwise_interrupt_col = "interrupt",
#' # #' #   hwise_clothes_col = "clothes",
#' # #' #   hwise_food_col = "food",
#' # #' #   hwise_body_col = "body",
#' # #' #   hwise_angry_col = "angry",
#' # #' #   hwise_sleep_col = "sleep",
#' # #' #   hwise_none_col = "none",
#' # #' #   hwise_shame_col = "shame",
#' # #' #   never_val = "Never (0 times)",
#' # #' #   rarely_val = "Rarely (1-2 times)",
#' # #' #   sometimes_val = "Sometimes (3-10 times)",
#' # #' #   often_val = "Often (11-20 times)",
#' # #' #   always_val = "Always (>20 times)"
#' # # #' # )
add_hwise <- function(data = NULL,
                      hwise_worry_col = NULL,
                      hwise_plans_col = NULL,
                      hwise_hands_col = NULL,
                      hwise_drink_col = NULL,

                      hwise_interrupt_col = NULL,
                      hwise_clothes_col = NULL,
                      hwise_food_col = NULL,
                      hwise_body_col = NULL,
                      hwise_angry_col = NULL,
                      hwise_sleep_col = NULL,
                      hwise_none_col = NULL,
                      hwise_shame_col = NULL,

                      never_val = NULL,
                      rarely_val = NULL,
                      sometimes_val = NULL,
                      often_val = NULL,
                      always_val = NULL

) {

  # Copy of the original data. It will be used to bind the new columns at the end and return it
  original_df <- data
  
  # Check if data is a data frame
  if(is.null(data)) {
    stop("Please provide a data frame to argument 'data'.")
  } else if(!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }

  # Check if the specified columns exist in the data for HWISE4

  if(is.null(hwise_worry_col) || is.null(hwise_plans_col) || is.null(hwise_hands_col) || is.null(hwise_drink_col)) {
    stop("Please provide column names for the four core HWISE items: hwise_worry_col, hwise_plans_col, hwise_hands_col, hwise_drink_col.")
  } else {

    hwise4_cols <- c(hwise_worry_col, hwise_plans_col, hwise_hands_col, hwise_drink_col)

    if(!all(hwise4_cols %in% colnames(data))) {
      stop("One or more specified core HWISE-4 columns are not found in the data frame.")
    }

    cols_to_check <- intersect(hwise4_cols, colnames(data))

  }

  # Check if the specified columns exist in the data for HWISE12

  

  if(is.null(hwise_worry_col) || is.null(hwise_plans_col) || is.null(hwise_hands_col) || is.null(hwise_drink_col) |
     is.null(hwise_interrupt_col) || is.null(hwise_clothes_col) || is.null(hwise_food_col) || is.null(hwise_body_col) |
     is.null(hwise_angry_col) || is.null(hwise_sleep_col) || is.null(hwise_none_col) || is.null(hwise_shame_col)) {
    warning("Not all columns are specified to calculate HWISE-12. This will be skipped.")
    hwise12_check <- 0
  } else {

    # Creation of the vector with the names of teh HWISE12 columns
    hwise12_cols <- c(hwise_worry_col, hwise_plans_col, hwise_hands_col, hwise_drink_col,
                      hwise_interrupt_col, hwise_clothes_col, hwise_food_col, hwise_body_col,
                      hwise_angry_col, hwise_sleep_col, hwise_none_col, hwise_shame_col)

    if(!all(hwise12_cols %in% colnames(data))) {
      stop("One or more specified core HWISE-12 columns are not found in the data frame.")
    }

    cols_to_check <- intersect(hwise12_cols, colnames(data))
    hwise12_check <- 1

  }

  # After checking the existence of the corresponding columns, we subset the working columns and the uuid to use it as key to bind in the last step
  
  data <- data %>% select(uuid, all_of(hwise4_cols), any_of(hwise12_cols))

  # Check if the response value parameters are provided
  if(is.null(never_val) || is.null(rarely_val) || is.null(sometimes_val) || is.null(often_val) || is.null(always_val)) {
    stop("Please provide values for all response categories: never_val, rarely_val, sometimes_val, often_val, always_val.")
  }

  # Check if non-valid response options are found in provided columns
  valid_responses <- c(never_val, rarely_val, sometimes_val, often_val, always_val)

  invalid_rows <- apply(
    data[cols_to_check],
    1,
    function(row) any(!row %in% valid_responses & !is.na(row))
  )

  if(any(invalid_rows)) {
    warning("Some rows contain values outside the specified response categories. Invalid rows set to NA in cols_to_check.")
    data[invalid_rows, cols_to_check] <- NA
  }

  # Create new HWISE 4 Columns onto dataset

  data <- data %>%
    dplyr::mutate(
      hwise_worry_score = dplyr::case_when(
        !!rlang::sym(hwise_worry_col) == never_val ~ 0,
        !!rlang::sym(hwise_worry_col) == rarely_val ~ 1,
        !!rlang::sym(hwise_worry_col) == sometimes_val ~ 2,
        !!rlang::sym(hwise_worry_col) == often_val ~ 3,
        !!rlang::sym(hwise_worry_col) == always_val ~ 3,
        TRUE ~ NA_real_
      ),
      hwise_plans_score = dplyr::case_when(
        !!rlang::sym(hwise_plans_col) == never_val ~ 0,
        !!rlang::sym(hwise_plans_col) == rarely_val ~ 1,
        !!rlang::sym(hwise_plans_col) == sometimes_val ~ 2,
        !!rlang::sym(hwise_plans_col) == often_val ~ 3,
        !!rlang::sym(hwise_plans_col) == always_val ~ 3,
        TRUE ~ NA_real_
      ),
      hwise_hands_score = dplyr::case_when(
        !!rlang::sym(hwise_hands_col) == never_val ~ 0,
        !!rlang::sym(hwise_hands_col) == rarely_val ~ 1,
        !!rlang::sym(hwise_hands_col) == sometimes_val ~ 2,
        !!rlang::sym(hwise_hands_col) == often_val ~ 3,
        !!rlang::sym(hwise_hands_col) == always_val ~ 3,
        TRUE ~ NA_real_
      ),
      hwise_drink_score = dplyr::case_when(
        !!rlang::sym(hwise_drink_col) == never_val ~ 0,
        !!rlang::sym(hwise_drink_col) == rarely_val ~ 1,
        !!rlang::sym(hwise_drink_col) == sometimes_val ~ 2,
        !!rlang::sym(hwise_drink_col) == often_val ~ 3,
        !!rlang::sym(hwise_drink_col) == always_val ~ 3,
        TRUE ~ NA_real_
      )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hwise4_score = sum(dplyr::c_across(dplyr::all_of(c("hwise_worry_score", "hwise_plans_score", "hwise_hands_score", "hwise_drink_score"))), na.rm = FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hwise4_severity_cat = dplyr::case_when(
      hwise4_score >= 0 & hwise4_score <= 3 ~ "No-to-marginal",
      hwise4_score >= 4 & hwise4_score <= 6 ~ "Low",
      hwise4_score >= 7 & hwise4_score <= 8 ~ "Moderate",
      hwise4_score >= 9 & hwise4_score <= 10 ~ "High",
      hwise4_score >= 11 ~ "Very High",
      TRUE ~ NA_character_
    ),
    hwise4_insecure_cat = dplyr::case_when(
      hwise4_score >= 0 & hwise4_score <= 3 ~ 0,
      hwise4_score >= 4 & hwise4_score <= 12 ~ 1,
      TRUE ~ NA_integer_
    ))

  if(hwise12_check == 1) {

    data <- data %>%
      dplyr::mutate(
        hwise_interrupt_score = dplyr::case_when(
          !!rlang::sym(hwise_interrupt_col) == never_val ~ 0,
          !!rlang::sym(hwise_interrupt_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_interrupt_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_interrupt_col) == often_val ~ 3,
          !!rlang::sym(hwise_interrupt_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_clothes_score = dplyr::case_when(
          !!rlang::sym(hwise_clothes_col) == never_val ~ 0,
          !!rlang::sym(hwise_clothes_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_clothes_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_clothes_col) == often_val ~ 3,
          !!rlang::sym(hwise_clothes_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_food_score = dplyr::case_when(
          !!rlang::sym(hwise_food_col) == never_val ~ 0,
          !!rlang::sym(hwise_food_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_food_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_food_col) == often_val ~ 3,
          !!rlang::sym(hwise_food_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_body_score = dplyr::case_when(
          !!rlang::sym(hwise_body_col) == never_val ~ 0,
          !!rlang::sym(hwise_body_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_body_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_body_col) == often_val ~ 3,
          !!rlang::sym(hwise_body_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_angry_score = dplyr::case_when(
          !!rlang::sym(hwise_angry_col) == never_val ~ 0,
          !!rlang::sym(hwise_angry_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_angry_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_angry_col) == often_val ~ 3,
          !!rlang::sym(hwise_angry_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_sleep_score = dplyr::case_when(
          !!rlang::sym(hwise_sleep_col) == never_val ~ 0,
          !!rlang::sym(hwise_sleep_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_sleep_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_sleep_col) == often_val ~ 3,
          !!rlang::sym(hwise_sleep_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_none_score = dplyr::case_when(
          !!rlang::sym(hwise_none_col) == never_val ~ 0,
          !!rlang::sym(hwise_none_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_none_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_none_col) == often_val ~ 3,
          !!rlang::sym(hwise_none_col) == always_val ~ 3,
          TRUE ~ NA_real_
        ),
        hwise_shame_score = dplyr::case_when(
          !!rlang::sym(hwise_shame_col) == never_val ~ 0,
          !!rlang::sym(hwise_shame_col) == rarely_val ~ 1,
          !!rlang::sym(hwise_shame_col) == sometimes_val ~ 2,
          !!rlang::sym(hwise_shame_col) == often_val ~ 3,
          !!rlang::sym(hwise_shame_col) == always_val ~ 3,
          TRUE ~ NA_real_
        )) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hwise12_score = sum(dplyr::c_across(dplyr::all_of(c("hwise_worry_score", "hwise_plans_score", "hwise_hands_score", "hwise_drink_score",
                                                                 "hwise_interrupt_score", "hwise_clothes_score", "hwise_food_score", "hwise_body_score",
                                                                 "hwise_angry_score", "hwise_sleep_score", "hwise_none_score", "hwise_shame_score"))), na.rm = FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(hwise12_severity_cat = dplyr::case_when(
        hwise12_score >= 0 & hwise12_score <= 2 ~ "No-to-marginal",
        hwise12_score >= 3 & hwise12_score <= 11 ~ "Low",
        hwise12_score >= 12 & hwise12_score <= 23 ~ "Moderate",
        hwise12_score >= 24 & hwise12_score <= 36 ~ "High",
        TRUE ~ NA_character_
      ),
      hwise12_insecure_cat = dplyr::case_when(
        hwise12_score >= 0 & hwise12_score <= 11 ~ 0,
        hwise12_score >= 12 ~ 1,
        TRUE ~ NA_integer_
      ))

  }

  data <- data %>% select(!any_of(hwise12_cols))

  data <- original_df %>% left_join(x=., y=data, by=join_by(uuid))

  return(data)

}
