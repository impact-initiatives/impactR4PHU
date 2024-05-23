#' check_iycf_flags
#'
#' @param .dataset the raw child nutrition loop dataset with all iycf indicators
#' @param age_months the name of the variable that indicates the age of child per month.
#' @param iycf_4 the name of the variable that indicates if the child was breastfed yesterday
#' By default "iycf_4"
#' @param iycf_6a the name of the variable that indicates if the child
#' had water yesterday. By default "iycf_6a"
#' @param iycf_6b the name of the variable that indicates if the child
#' had infant formula yesterday. By default "iycf_6b"
#' @param iycf_6c the name of the variable that indicates if the child
#' had milk from animal yesterday. By default "iycf_6c"
#' @param iycf_6d the name of the variable that indicates if the child
#' had yogurt drink yesterday. By default "iycf_6d"
#' @param iycf_6e the name of the variable that indicates if the child
#' had chocolate flavored drink yesterday. By default "iycf_6e"
#' @param iycf_6f the name of the variable that indicates if the child
#' had fruit juice or fruit flavored drink yesterday. By default "iycf_6f"
#' @param iycf_6g the name of the variable that indicates if the child
#' had sodas/malt/sports/energy drink yesterday. By default "iycf_6g"
#' @param iycf_6h the name of the variable that indicates if the child
#' had tea/coffee/herbal drink yesterday. By default "iycf_6h"
#' @param iycf_6i the name of the variable that indicates if the child
#' had clear broth/soup yesterday. By default "iycf_6i"
#' @param iycf_6j the name of the variable that indicates if the child
#' had other liquids yesterday. By default "iycf_6i"
#' @param iycf_7a the name of the variable that indicates if the child
#' had yogurt as food yesterday. By default "iycf_7a"
#' @param iycf_7b the name of the variable that indicates if the child
#' had porridge/bread/rice/noodles/pasta yesterday. By default "iycf_7b"
#' @param iycf_7c the name of the variable that indicates if the child
#' had pumpkin/carrots/sweet red peppers/squash/sweet potato
#' yesterday. By default "iycf_7c"
#' @param iycf_7d the name of the variable that indicates if the child
#' had plantains/white potato/yams/manioc/cassava
#' yesterday. By default "iycf_7d"
#' @param iycf_7e the name of the variable that indicates if the child
#' had dark green leafy vegetables yesterday. By default "iycf_7e"
#' @param iycf_7f the name of the variable that indicates if the child
#' had other vegetables yesterday. By default "iycf_7f"
#' @param iycf_7g the name of the variable that indicates if the child
#' had ripe mangoes/ripe papayas yesterday. By default "iycf_7g"
#' @param iycf_7h the name of the variable that indicates if the child
#' had other fruits yesterday. By default "iycf_7h"
#' @param iycf_7i the name of the variable that indicates if the child
#' had liver/kidney/heart yesterday. By default "iycf_7i"
#' @param iycf_7j the name of the variable that indicates if the child
#' had sausage/hot dogs/ham/bacon/salami/canned meat
#' yesterday. By default "iycf_7j"
#' @param iycf_7k the name of the variable that indicates if the child
#' had other meat/beef/pork/lamb/goat/chicken/duck
#' yesterday. By default "iycf_7k"
#' @param iycf_7l the name of the variable that indicates if the child
#' had eggs yesterday. By default "iycf_7l"
#' @param iycf_7m the name of the variable that indicates if the child
#' had fresh/dried/shell fish yesterday. By default "iycf_7m"
#' @param iycf_7n the name of the variable that indicates if the child
#' had beans/peas/lentils/nuts/seeds yesterday. By default "iycf_7n"
#' @param iycf_7o the name of the variable that indicates if the child
#' had hard/soft cheese yesterday. By default "iycf_7o"
#' @param iycf_7p the name of the variable that indicates if the child
#' had sweet foods yesterday. By default "iycf_7p"
#' @param iycf_7q the name of the variable that indicates if the child
#' had chips/crisps/puffs/french fries yesterday. By default "iycf_7q"
#' @param iycf_7r the name of the variable that indicates if the child
#' had other solid food yesterday. By default "iycf_7q"
#' @param iycf_8 the name of the variable that indicates if the meal frequency the
#' child had yesterday. By default "iycf_8"
#' @param iycf_6b_num the name of the variable that indicates how many times the child
#' had infant formula yesterday. By default "iycf_6b_num"
#' @param iycf_6c_num the name of the variable that indicates how many times the child
#' had milk from animal yesterday. By default "iycf_6c_num"
#' @param iycf_6d_num the name of the variable that indicates how many times the child
#' had yogurt drink yesterday. By default "iycf_6d_num"
#' @param yes_value the value of the choice "yes" to all the liquid/food categories
#' @param uuid uuid variable
#'
#' @return a dataframe that includes all the added flags related to iycf.
#' This includes:
#' - flag_yes_foods: all foods selected were yes
#' - flag_yes_liquids: all liquids were yes
#' - flag_no_anything: no foods or liquids at all
#' - flag_no_foods: no food at all and mealfreq is more than 0 and age_months more than 5
#' - flag_all_foods_no_meal: all food given but mealfreq is equal to 0
#' - flag_some_foods_no_meal: some food given but mealfreq is equal to 0
#' - flag_high_mdd_low_mmf: High Minimum Dietary Diversity and low mealfreq
#' - flag_under6_nobf_nomilk: child under 6 with no milk or breastfeeding
#' - flag_meats_nostaples: child given meat but no staples
#'
#' @export
#'
#' @examples
#' \dontrun{check_iycf_flags(df1)}

check_iycf_flags <- function(.dataset,
                             age_months = "age_months",
                             iycf_4 = "iycf_4",
                             iycf_6a = "iycf_6a",
                             iycf_6b = "iycf_6b",
                             iycf_6c = "iycf_6c",
                             iycf_6d = "iycf_6d",
                             iycf_6e = "iycf_6e",
                             iycf_6f = "iycf_6f",
                             iycf_6g = "iycf_6g",
                             iycf_6h = "iycf_6h",
                             iycf_6i = "iycf_6i",
                             iycf_6j = "iycf_6j",
                             iycf_7a = "iycf_7a",
                             iycf_7b = "iycf_7b",
                             iycf_7c = "iycf_7c",
                             iycf_7d = "iycf_7d",
                             iycf_7e = "iycf_7e",
                             iycf_7f = "iycf_7f",
                             iycf_7g = "iycf_7g",
                             iycf_7h = "iycf_7h",
                             iycf_7i = "iycf_7i",
                             iycf_7j = "iycf_7j",
                             iycf_7k = "iycf_7k",
                             iycf_7l = "iycf_7l",
                             iycf_7m = "iycf_7m",
                             iycf_7n = "iycf_7n",
                             iycf_7o = "iycf_7o",
                             iycf_7p = "iycf_7p",
                             iycf_7q = "iycf_7q",
                             iycf_7r = "iycf_7r",
                             iycf_8 = "iycf_8",
                             iycf_6b_num = "iycf_6b_num",
                             iycf_6c_num = "iycf_6c_num",
                             iycf_6d_num = "iycf_6d_num",
                             yes_value = "yes",
                             uuid = "uuid") {
  options(warn = -1)
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if(!age_months %in% names(.dataset)) stop("Missing age_months column in dataset")

  if(!uuid %in% names(.dataset)) stop("Missing uuid column in dataset")


  liquids <- c(iycf_6a, iycf_6b,iycf_6c,iycf_6d,iycf_6e,
               iycf_6f,iycf_6g,iycf_6h,iycf_6i,iycf_6j)

  foods <- c(iycf_7a,iycf_7b,iycf_7c,iycf_7d,iycf_7e,iycf_7f,
             iycf_7g,iycf_7h,iycf_7i,iycf_7j,iycf_7k,iycf_7l,
             iycf_7m,iycf_7n,iycf_7o,iycf_7p,iycf_7q,iycf_7r)

  required_columns <- c(foods,liquids)
  missing_columns <- setdiff(required_columns, names(.dataset))

  if(length(missing_columns) > 0) {
    warning("Your dataset appears not to have all the foods/liquids from the standard IYCF 2021 question sequence.\nIt is advised you ask about all recommended foods/liquids or there is a risk of overestimating EBF.")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(
        sum_foods = rowSums(dplyr::across(c(foods))),
        sum_liquids = rowSums(dplyr::across(c(liquids))),
        flag_yes_foods = ifelse(sum_foods == 18, 1, 0),
        flag_yes_liquids = ifelse(sum_liquids == 10, 1, 0),
        flag_no_foods = ifelse(sum_foods == 0, 1, 0),
        flag_no_liquids = ifelse(sum_liquids == 0, 1, 0))
  }

  required_columns <- c(iycf_4,foods,liquids)
  missing_columns <- setdiff(required_columns, names(.dataset))

  if(length(missing_columns) > 0) {
    warning("Your dataset appears not to have either all the foods/liquids or the IYCF 4 from the standard IYCF 2021 question sequence.\nIt is advised you ask about all recommended foods/liquids or there is a risk of overestimating EBF.")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(
        flag_no_anything = ifelse(is.na(!!rlang::sym(iycf_4)), NA,
                                              ifelse(!!rlang::sym(iycf_4) != yes_value &
                                                       flag_no_foods == 1 &
                                                       flag_no_liquids == 1 &
                                                       as.numeric(!!rlang::sym(age_months)) > 5, 1, 0)))
  }

  required_columns <- c(iycf_8,foods,liquids)
  missing_columns <- setdiff(required_columns, names(.dataset))

  if(length(missing_columns) > 0) {
    warning("Your dataset appears not to have either all the foods/liquids or the IYCF 8 from the standard IYCF 2021 question sequence.\nIt is advised you ask about all recommended foods/liquids or there is a risk of overestimating EBF.")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(flag_no_foods_at_all = ifelse(flag_no_foods == 1 &
                                             as.numeric(!!rlang::sym(iycf_8)) > 0 &
                                             as.numeric(!!rlang::sym(age_months)) > 5, 1, 0),
                    flag_all_foods_no_meal = ifelse(flag_yes_foods == 1 &
                                                      as.numeric(!!rlang::sym(iycf_8)) == 0, 1, 0),
                    flag_some_foods_no_meal = ifelse(sum_foods > 0 &
                                                       sum_foods < 18 &
                                                       as.numeric(!!rlang::sym(iycf_8)) == 0 &
                                                       as.numeric(!!rlang::sym(age_months)) > 5, 1, 0))

  }
  required_columns <- c("iycf_mdd_score", iycf_8)
  missing_columns <- setdiff(required_columns, names(.dataset))

  if(length(missing_columns) > 0) {
    warning("Your dataset appears not to have either the IYCF 8 or the iycf_mdd_score columns .\nMake sure that you have the add_iycf() before this function.")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(flag_high_mdd_low_mmf = ifelse(is.na(iycf_mdd_score), NA,
                                                   ifelse(is.na(!!rlang::sym(iycf_8)), NA,
                                                          ifelse(iycf_mdd_score >= 6 & as.numeric(!!rlang::sym(iycf_8)) <= 1, 1, 0))))


  }
  required_columns <- c(iycf_4, iycf_6b_num, iycf_6c_num, iycf_6d_num)
  missing_columns <- setdiff(required_columns, names(.dataset))

  if(length(missing_columns) > 0) {
    warning("Your dataset appears not to have either the IYCF 4 or iycf_6b/6c/6d_num columns .\nMake sure that you run the add_iycf() before this function.")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(flag_under6_nobf_nomilk = ifelse(is.na(!!rlang::sym(age_months)) |
                                                       is.na(!!rlang::sym(iycf_4)) |
                                                       is.na(!!rlang::sym(iycf_6b_num)) |
                                                       is.na(!!rlang::sym(iycf_6c_num)) |
                                                       is.na(!!rlang::sym(iycf_6d_num)), NA,
                                                     ifelse(as.numeric(!!rlang::sym(age_months)) < 6 &
                                                              !!rlang::sym(iycf_4) != yes_value &
                                                              !!rlang::sym(iycf_6b_num) == 0 &
                                                              !!rlang::sym(iycf_6c_num) == 0 &
                                                              !!rlang::sym(iycf_6d_num) == 0, 1, 0)))
  }

  required_columns <- c(iycf_7b, iycf_7d, iycf_7i, iycf_7j, iycf_7k, iycf_7l, iycf_7m)
  missing_columns <- setdiff(required_columns, names(.dataset))

  if(length(missing_columns) > 0) {
    warning("Your dataset appears not to have either the iycf_7b/iycf_7d/iycf_7i/iycf_7j/iycf_7k/iycf_7l/iycf_7m columns.\nIt is advised you ask about all recommended foods/liquids or there is a risk of overestimating EBF.\nMake sure also to run the add_iycf() before this function.")
  }else{
    .dataset <- .dataset %>%
      dplyr::mutate(flag_meats_nostaples = ifelse(is.na(!!rlang::sym(iycf_7b)) |
                                                  is.na(!!rlang::sym(iycf_7d)) |
                                                  is.na(!!rlang::sym(iycf_7i)) |
                                                  is.na(!!rlang::sym(iycf_7j)) |
                                                  is.na(!!rlang::sym(iycf_7k)) |
                                                  is.na(!!rlang::sym(iycf_7l)) |
                                                  is.na(!!rlang::sym(iycf_7m)), NA,
                                                  ifelse(!!rlang::sym(iycf_7b) != 1 &
                                                            !!rlang::sym(iycf_7d) != 1 &
                                                            (!!rlang::sym(iycf_7i) == 1 |
                                                               !!rlang::sym(iycf_7j) == 1 |
                                                               !!rlang::sym(iycf_7k) == 1 |
                                                               !!rlang::sym(iycf_7l) == 1 |
                                                               !!rlang::sym(iycf_7m) == 1), 1, 0)))

  }

  options(warn = 0)
  return(.dataset)
}
