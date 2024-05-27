#' add_iycf
#'
#' @param .dataset the raw child nutrition loop dataset with all iycf indicators
#' @param age_months the name of the variable that indicates the age of child per month.
#' By default it is age_months
#' @param iycf_1 the name of the variable that indicates if the child ever breastfed
#' By default "iycf_1"
#' @param iycf_2 the name of the variable that indicates how long the child started
#' breastfeeding after birth. By default "iycf_2"
#' @param iycf_3  the name of the variable that indicates Exclusive Breastfeeding First 2 Days
#' By default "iycf_3"
#' @param iycf_4 the name of the variable that indicates if the child was breastfed yesterday
#' By default "iycf_4"
#' @param iycf_5 the name of the variable that indicates if the child
#' drink anything from a bottle yesterday. By default "iycf_5"
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
#' @param iycf_6c_swt the name of the variable that indicates if the child
#' had sweet milk yesterday. By default "iycf_6c_swt"
#' @param iycf_6d_swt the name of the variable that indicates if the child
#' had sweet yogurt drink yesterday. By default "iycf_6d_swt"
#' @param iycf_6h_swt the name of the variable that indicates if the child
#' had sweet tea drink yesterday. By default "iycf_6h_swt"
#' @param iycf_6j_swt the name of the variable that indicates if the child
#' had other sweet drink yesterday. By default "iycf_6j_swt"
#' @param yes_value the value of the choice "yes" to all the liquid/food categories
#' @param no_value the value of the choice "no" to all the liquid/food categories
#' @param dnk_value the value of the choice "dont know" to all the liquid/food categories
#' @param pna_value the value of the choice "prefer not to answer" to all the liquid/food categories
#' @param iycf2_immediate_value the value of the choice "immediately" to the indicator iycf_2
#' @param iycf2_lessday_value the value of the choice "less than a day" to the indicator iycf_2
#' @param iycf2_moreday_value the value of the choice "more than a day"" to the indicator iycf_2
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#' @param loop_index unique identifier for each individual in the iycf loop.
#' By default, loop_index
#'
#' @return a dataframe that includes all the added iycf indicators.
#' This includes:
#' - iycf_evbf / Ever Breastfed
#' - iycf_eibf / Early Initiation of Breastfeeding
#' - iycf_ebf2d / Exclusive Breastfeeding First 2 Days After Birth
#' - iycf_ebf / Exclusive Breastfeeding
#' - iycf_mixmf / Mixed Milk Feeding
#' - iycf_cbf / Continued Breastfeeding 12-23 months
#' - iycf_isssf / Introduction of Solid, Semi-Solid, or Soft Foods
#' - iycf_mdd_score / Minimum Dietary Diversity 6-23 months Score
#' - iycf_mdd_cat / Minimum Dietary Diversity 6-23 months Category
#' - iycf_mmf / Minimum Meal Frequency 6-23 months
#' - iycf_mmff / Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months
#' - iycf_mad / Minimum Acceptable Diet 6-23 months
#' - iycf_eff / Eggs & Flesh Foods Consumption 6-23 months
#' - iycf_swb / Sweet Beverage Consumption 6-23 months
#' - iycf_ufc / Unhealthy Food Consumption
#' - iycf_zvf / Zero Vegetable or Fruit Consumption 6-23 months
#' - iycf_bof / Bottle Feeding 0-23 months
#'
#' @export
#'
#' @examples
#' \dontrun{add_iycf(df1)}


add_iycf <- function(.dataset,
                     age_months = "age_months",
                     iycf_1 = "iycf_1",
                     iycf_2 = "iycf_2",
                     iycf_3 = "iycf_3",
                     iycf_4 = "iycf_4",
                     iycf_5 = "iycf_5",
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
                     iycf_6c_swt = "iycf_6c_swt",
                     iycf_6d_swt = "iycf_6d_swt",
                     iycf_6h_swt = "iycf_6h_swt",
                     iycf_6j_swt = "iycf_6j_swt",
                     yes_value = "yes",
                     no_value = "no",
                     dnk_value = "dont_know",
                     pna_value = "pna",
                     iycf2_immediate_value = "immediately",
                     iycf2_lessday_value = "less_than_one_day",
                     iycf2_moreday_value = "more_than_one_day",
                     grouping = NULL,
                     uuid = "uuid",
                     loop_index = NULL){
  options(warn = -1)
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
  } else {
    .dataset <- .dataset %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  if(!age_months %in% names(.dataset)) stop("Missing age_months column in dataset")

  if(!uuid %in% names(.dataset)) stop("Missing uuid column in dataset")

  # IYCF Indicator 1: Ever Breastfed
  if(!is.null(iycf_1)){
    if(!iycf_1 %in% names(.dataset)){
      warning("IYCF 1 not found in dataset.\nIYCF Indicator 1: Ever Breastfed not calculated")
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(iycf_evbf = dplyr::case_when(!!rlang::sym(iycf_1) == yes_value ~ 1,
                                                    !!rlang::sym(iycf_1) != yes_value ~ 0,
                                                    TRUE ~ NA),
                      iycf_evbf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                     is.na(!!rlang::sym(age_months)) ~ NA,
                                                   TRUE ~ iycf_evbf))
    }
  }

  # IYCF Indicator 2: Early Initiation of Breastfeeding
  if(!is.null(iycf_2)) {
    if(!iycf_2 %in% names(.dataset)){
      warning("IYCF 2 not found in dataset.\nIYCF Indicator 2: Early Initiation of Breastfeeding not calculated")
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(iycf_eibf =  dplyr::case_when(!!rlang::sym(iycf_2) %in% c(iycf2_immediate_value,
                                                                                iycf2_lessday_value,
                                                                                iycf2_moreday_value) ~ 1,
                                                    !!rlang::sym(iycf_2) %notin% c(iycf2_immediate_value,
                                                                                  iycf2_lessday_value,
                                                                                  iycf2_moreday_value) ~ 0,
                                                    TRUE ~ NA),
                      iycf_eibf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_eibf))
    }
  }

  # IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth
  if(!is.null(iycf_3)){
    if(!iycf_3 %in% names(.dataset)){
      warning("IYCF 3 not found in dataset.\nIYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth not calculated")
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(iycf_ebf2d = dplyr::case_when(!!rlang::sym(iycf_3) == no_value ~ 1,
                                                    !!rlang::sym(iycf_3) != no_value ~ 0,
                                                    TRUE ~ NA),
                      iycf_ebf2d = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                      is.na(!!rlang::sym(age_months)) ~ NA,
                                                    TRUE ~ iycf_ebf2d))
    }
  }

  # IYCF Indicator 4: Exclusive Breastfeeding

  ebf_foods <- c(iycf_7a,iycf_7b,iycf_7c,iycf_7d,iycf_7e,
                 iycf_7f,iycf_7g,iycf_7h,iycf_7i,iycf_7j,
                 iycf_7k,iycf_7l,iycf_7m,iycf_7o,iycf_7n,
                 iycf_7p,iycf_7q,iycf_7r)
  ebf_liquids <- c(iycf_6a,iycf_6b,iycf_6c,iycf_6d,iycf_6e,
                   iycf_6f,iycf_6g,iycf_6h,iycf_6i,iycf_6j)

  required_columns <- c(ebf_foods, ebf_liquids, iycf_4)

  if(length(setdiff(length(required_columns),29)) != 0) {
    warning("Your dataset appears not to have all the foods/liquids from the standard IYCF 2021 question sequence.\nIt is advised you ask about all recommended foods/liquids or there is a risk of overestimating EBF.\nIYCF Indicator 4: Exclusive Breastfeeding not calculated ")
    warning(paste0("Missing the following variables ", setdiff(c(ebf_foods,ebf_liquids), names(.dataset))))
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(iycf_6b_num = as.numeric(!!rlang::sym(iycf_6b)),
                    iycf_6c_num = as.numeric(!!rlang::sym(iycf_6c)),
                    iycf_6d_num = as.numeric(!!rlang::sym(iycf_6d)),
                    iycf_7a_num = as.numeric(!!rlang::sym(iycf_7a)),
                    iycf_7c_zero = !!rlang::sym(iycf_7c),
                    iycf_7e_zero = !!rlang::sym(iycf_7e),
                    iycf_7f_zero = !!rlang::sym(iycf_7f),
                    iycf_7g_zero = !!rlang::sym(iycf_7g),
                    iycf_7h_zero = !!rlang::sym(iycf_7h)) %>%
      dplyr::mutate_at(dplyr::vars(iycf_6a,iycf_6e,iycf_6f,iycf_6g,
                                     iycf_6h,iycf_6i,iycf_6j,iycf_7b,
                                     iycf_7c,iycf_7d,iycf_7e,
                                     iycf_7f,iycf_7g,iycf_7h,iycf_7i,iycf_7j,
                                     iycf_7k,iycf_7l,iycf_7m,iycf_7o,iycf_7n,
                                     iycf_7p,iycf_7q,iycf_7r), ~ dplyr::case_when(. == yes_value ~ 1,
                                                                                  . == no_value ~ 0,
                                                                                  TRUE ~ NA)) %>%
      dplyr::mutate_at(dplyr::vars(iycf_6b,iycf_6c,iycf_6d,iycf_7a), ~ dplyr::case_when(as.numeric(.) > 0 ~ 1,
                                                                                        as.numeric(.) == 0 ~ 0,
                                                                                        TRUE ~ NA)) %>%
      dplyr::mutate(count_foods = rowSums(dplyr::across(ebf_foods, .fns = as.numeric)),
                    count_liquids = rowSums(dplyr::across(ebf_liquids, .fns = as.numeric)),
                    iycf_ebf = dplyr::case_when(!!rlang::sym(iycf_4) == yes_value &
                                                  count_foods == 0 &
                                                  count_liquids == 0 ~ 1,
                                                TRUE ~ 0),
                    iycf_ebf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 6 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_ebf))
  }

  # IYCF Indicator 5: Mixed Milk Feeding (MIxMF)
  required_columns <- c(iycf_4,iycf_6b,iycf_6c)

  if(length(setdiff(length(required_columns),3)) != 0) {
    warning("IYCF 4 or IYCF 6b or IYCF 6c not found in dataset.\nIYCF Indicator 5: Mixed Milk Feeding (MIxMF) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(iycf_mixmf = dplyr::case_when(!!rlang::sym(iycf_4) == yes_value &
                                                    (!!rlang::sym(iycf_6b) == 1 |
                                                       !!rlang::sym(iycf_6c) == 1) ~ 1,
                                                  !!rlang::sym(iycf_4) != yes_value |
                                                    (!!rlang::sym(iycf_6b) != 1 &
                                                       !!rlang::sym(iycf_6c) != 1) ~ 0,
                                                  TRUE ~ NA),
                    iycf_mixmf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 6 |
                                                    is.na(!!rlang::sym(age_months)) ~ NA,
                                                  TRUE ~ iycf_mixmf))
  }


  # IYCF Indicator 6: Continued Breastfeeding 12-23 months
  if(!is.null(iycf_4)){
    if(!iycf_4 %in% names(.dataset)){
      warning("IYCF 4 not found in dataset.\nIYCF Indicator 6: Continued Breastfeeding 12-23 months not calculated")
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(iycf_cbf =  dplyr::case_when(!!rlang::sym(iycf_4) == yes_value ~ 1,
                                                   !!rlang::sym(iycf_4) != yes_value ~ 0,
                                                   TRUE ~ NA),
                      iycf_cbf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 12 |
                                                    as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                    is.na(!!rlang::sym(age_months)) ~ NA,
                                                  TRUE ~ iycf_cbf))
    }
  }

  # IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods (ISSSF)
  required_columns <- c(ebf_foods)

  if(length(setdiff(length(required_columns),18)) != 0) {
    warning("Your dataset appears not to have all the foods from the standard IYCF 2021 question sequence.\nIYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods (ISSSF) not calculated")
    warning(paste0("Missing the following variables ", setdiff(ebf_foods, names(.dataset))))
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(count_foods = rowSums(dplyr::across(ebf_foods, .fns = as.numeric)),
                    iycf_isssf = dplyr::case_when(count_foods > 0 ~ 1,
                                                  count_foods == 0 ~ 0,
                                                  TRUE ~ NA),
                    iycf_isssf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                    as.numeric(!!rlang::sym(age_months)) > 8 |
                                                    is.na(!!rlang::sym(age_months)) ~ NA,
                                                  TRUE ~ iycf_isssf))
  }

  # IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD)

  mdd_columns <- c(iycf_4,iycf_6b,iycf_6c,iycf_6d,iycf_7a,iycf_7b,
                   iycf_7c,iycf_7d,iycf_7e,iycf_7f,iycf_7g,iycf_7h,
                   iycf_7i,iycf_7j,iycf_7k,iycf_7l,iycf_7m,iycf_7n,iycf_7o)


  if(length(setdiff(length(mdd_columns),19)) != 0) {
    warning("Minimum Dietary Diversity related columns not found in dataset.\nIYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(mdd1 = dplyr::case_when(!!rlang::sym(iycf_4) == yes_value ~ 1,
                                            !!rlang::sym(iycf_4) != yes_value ~ 0,
                                            TRUE ~ NA),
                    mdd2 = dplyr::case_when(is.na(!!rlang::sym(iycf_7b)) |
                                              is.na(!!rlang::sym(iycf_7d)) ~ NA_real_,
                                            !!rlang::sym(iycf_7b) == 1 |
                                              !!rlang::sym(iycf_7d) == 1 ~ 1,
                                            TRUE ~ 0),
                    mdd3 = dplyr::case_when(is.na(!!rlang::sym(iycf_7n))  ~ NA_real_,
                                            !!rlang::sym(iycf_7n) == 1 ~ 1,
                                            TRUE ~ 0),
                    mdd4 = dplyr::case_when(is.na(!!rlang::sym(iycf_6b)) |
                                              is.na(!!rlang::sym(iycf_6c)) |
                                              is.na(!!rlang::sym(iycf_6d)) |
                                              is.na(!!rlang::sym(iycf_7a)) |
                                              is.na(!!rlang::sym(iycf_7o)) ~ NA_real_,
                                            !!rlang::sym(iycf_6b) == 1 |
                                              !!rlang::sym(iycf_6c) == 1 |
                                              !!rlang::sym(iycf_6d) == 1 |
                                              !!rlang::sym(iycf_7a) == 1 |
                                              !!rlang::sym(iycf_7o) == 1 ~ 1,
                                            TRUE ~ 0),
                    mdd5 = dplyr::case_when(is.na(!!rlang::sym(iycf_7i)) |
                                              is.na(!!rlang::sym(iycf_7j)) |
                                              is.na(!!rlang::sym(iycf_7k)) |
                                              is.na(!!rlang::sym(iycf_7m)) ~ NA_real_,
                                            !!rlang::sym(iycf_7i) == 1 |
                                              !!rlang::sym(iycf_7j) == 1 |
                                              !!rlang::sym(iycf_7k) == 1 |
                                              !!rlang::sym(iycf_7m) == 1 ~ 1,
                                            TRUE ~ 0),
                    mdd6 = dplyr::case_when(is.na(!!rlang::sym(iycf_7l))  ~ NA_real_,
                                            !!rlang::sym(iycf_7l) == 1 ~ 1,
                                            TRUE ~ 0),
                    mdd7 = dplyr::case_when(is.na(!!rlang::sym(iycf_7c)) |
                                              is.na(!!rlang::sym(iycf_7e)) |
                                              is.na(!!rlang::sym(iycf_7g)) ~ NA_real_,
                                            !!rlang::sym(iycf_7c) == 1 |
                                              !!rlang::sym(iycf_7e) == 1 |
                                              !!rlang::sym(iycf_7g) == 1  ~ 1,
                                            TRUE ~ 0),
                    mdd8 = dplyr::case_when(is.na(!!rlang::sym(iycf_7f)) |
                                              is.na(!!rlang::sym(iycf_7h)) ~ NA_real_,
                                            !!rlang::sym(iycf_7f) == 1 |
                                              !!rlang::sym(iycf_7h) == 1  ~ 1,
                                            TRUE ~ 0)) %>%
      dplyr::mutate(iycf_mdd_score = rowSums(dplyr::across(c(mdd1,mdd2,mdd3,mdd4,
                                                      mdd5,mdd6,mdd7,mdd8), .fns = as.numeric), na.rm = T),
                    iycf_mdd_cat = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 6 &
                                                      as.numeric(!!rlang::sym(age_months)) < 24 &
                                                      iycf_mdd_score >= 5 ~ 1,
                                                    as.numeric(!!rlang::sym(age_months)) >= 6 &
                                                      as.numeric(!!rlang::sym(age_months)) < 24 &
                                                      iycf_mdd_score < 5 ~ 0,
                                                    TRUE ~ NA),
                    iycf_mdd_score = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                       as.numeric(!!rlang::sym(age_months)) >= 24 ~ NA,
                                                     TRUE ~ iycf_mdd_score),
                    iycf_mdd_cat = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                      as.numeric(!!rlang::sym(age_months)) >= 24 ~ NA,
                                                    TRUE ~ iycf_mdd_cat))
  }

  # IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF)
  mmf_columns <- c(iycf_4,"iycf_6b_num","iycf_6c_num",
                   "iycf_6d_num",iycf_8)


  if(length(setdiff(length(mmf_columns),5)) != 0) {
    warning("Minimum Meal Frequency related columns not found in dataset.\nIYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(mmf_bf_6to8months = dplyr::case_when(!!rlang::sym(iycf_4) == yes_value &
                                                           as.numeric(!!rlang::sym(iycf_8)) >= 2 ~1,
                                                         !!rlang::sym(iycf_4) != yes_value |
                                                           as.numeric(!!rlang::sym(iycf_8)) < 2 ~ 0,
                                                         TRUE ~ NA),
                    mmf_bf_6to8months = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                           as.numeric(!!rlang::sym(age_months)) >= 8 |
                                                           is.na(!!rlang::sym(age_months)) ~ NA,
                                                         TRUE ~ mmf_bf_6to8months),
                    mmf_bf_9to23months = dplyr::case_when(!!rlang::sym(iycf_4) == yes_value &
                                                           as.numeric(!!rlang::sym(iycf_8)) >= 3 ~ 1,
                                                          !!rlang::sym(iycf_4) != yes_value |
                                                            as.numeric(!!rlang::sym(iycf_8)) < 3 ~ 0,
                                                         TRUE ~ NA),
                    mmf_bf_9to23months = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 9 |
                                                            as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                            is.na(!!rlang::sym(age_months)) ~ NA,
                                                          TRUE ~ mmf_bf_9to23months),
                    count_6b_6c_6d_8 = rowSums(dplyr::across(c(iycf_6b_num,iycf_6c_num,
                                                               iycf_6d_num,iycf_8), .fns = as.numeric), na.rm = T),
                    mmf_nonbf_6to23months = dplyr::case_when(!!rlang::sym(iycf_4) != yes_value &
                                                               count_6b_6c_6d_8 >= 4 &
                                                               as.numeric(!!rlang::sym(iycf_8)) >= 1 ~ 1,
                                                             !!rlang::sym(iycf_4) == yes_value |
                                                               count_6b_6c_6d_8 < 4 &
                                                               as.numeric(!!rlang::sym(iycf_8)) < 1 ~ 0,
                                                             TRUE ~ NA),
                    mmf_nonbf_6to23months = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                               as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                               is.na(!!rlang::sym(age_months)) ~ NA,
                                                             TRUE ~ mmf_nonbf_6to23months),
                    iycf_mmf = dplyr::case_when(mmf_bf_6to8months == 1 |
                                                  mmf_bf_9to23months == 1 |
                                                  mmf_nonbf_6to23months == 1 ~ 1,
                                                mmf_bf_6to8months != 1 &
                                                  mmf_bf_9to23months != 1 &
                                                  mmf_nonbf_6to23months != 1 ~ 0,
                                                TRUE ~ NA),
                    iycf_mmf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                  as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_mmf))
  }

  # IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF)
  mmff_columns <- c(iycf_4,"iycf_6b_num",
                    "iycf_6c_num","iycf_6d_num","iycf_7a_num")


  if(length(setdiff(length(mmff_columns),5)) != 0) {
    warning("Minimum Milk Feeding Frequency related columns not found in dataset.\nIYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(count_dairy = rowSums(dplyr::across(c(iycf_6b_num,iycf_6c_num,
                                                          iycf_6d_num,iycf_7a_num)), na.rm = T),
                    iycf_mmff = dplyr::case_when(!!rlang::sym(iycf_4) != yes_value &
                                                  count_dairy >= 2 ~ 1,
                                                 !!rlang::sym(iycf_4) == yes_value |
                                                   count_dairy < 2 ~ 0,
                                                TRUE ~ NA),
                    iycf_mmff = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                   as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                   is.na(!!rlang::sym(age_months))  ~ NA,
                                                 TRUE ~ iycf_mmff))
  }

  # IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD)

  ind_11_columns <- c(iycf_4, "iycf_mmf", "iycf_mdd_cat", "iycf_mmff")

  if(length(setdiff(length(ind_11_columns),4)) != 0) {
    warning("Minimum Acceptable Diet 6-23 months related columns not found in dataset.\nIYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(iycf_mad = dplyr::case_when((!!rlang::sym(iycf_4) == yes_value |
                                                   iycf_mmff == 1) &
                                                  iycf_mdd_cat == 1 &
                                                  iycf_mmf == 1  ~ 1,
                                                (!!rlang::sym(iycf_4) != yes_value &
                                                   iycf_mmff != 1) |
                                                  iycf_mdd_cat != 1 &
                                                  iycf_mmf != 1  ~ 0,
                                                TRUE ~ NA),
                    iycf_mad = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                  as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_mad))
  }

  # IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF)
  ind_12_columns <- c(iycf_7i,iycf_7j,iycf_7k,
                      iycf_7l,iycf_7m)



  if(length(setdiff(length(ind_12_columns),5)) != 0) {
    warning("Eggs & Flesh Foods Consumption related columns not found in dataset.\nIYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(iycf_eff = dplyr::case_when(!!rlang::sym(iycf_7i) == 1 |
                                                  !!rlang::sym(iycf_7j) == 1 |
                                                  !!rlang::sym(iycf_7k) == 1 |
                                                  !!rlang::sym(iycf_7l) == 1 |
                                                  !!rlang::sym(iycf_7m) == 1 ~ 1,
                                                !!rlang::sym(iycf_7i) != 1 &
                                                  !!rlang::sym(iycf_7j) != 1 &
                                                  !!rlang::sym(iycf_7k) != 1 &
                                                  !!rlang::sym(iycf_7l) != 1 &
                                                  !!rlang::sym(iycf_7m) != 1 ~ 0,
                                                TRUE ~ NA),
                    iycf_eff = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                  as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_eff))
  }

  # IYCF Indicator 13: Sweet Beverage Consumption 6-23 months (SWB)

  sweet_col <- c(iycf_6c_swt,iycf_6d_swt,iycf_6h_swt,iycf_6j_swt,
                 iycf_6e, iycf_6f,iycf_6g)

  if(length(setdiff(length(sweet_col),7)) != 0) {
    warning("Sweet Beverage Consumption  related columns not found in dataset.\nIYCF Indicator 13: Sweet Beverage Consumption 6-23 months (SWB) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(iycf_swb = dplyr::case_when(!!rlang::sym(iycf_6c_swt) == yes_value |
                                                  !!rlang::sym(iycf_6d_swt) == yes_value |
                                                  !!rlang::sym(iycf_6h_swt) == yes_value |
                                                  !!rlang::sym(iycf_6j_swt) == yes_value |
                                                  !!rlang::sym(iycf_6e) == yes_value |
                                                  !!rlang::sym(iycf_6f) == yes_value |
                                                  !!rlang::sym(iycf_6g) == yes_value ~ 1,
                                                !!rlang::sym(iycf_6c_swt) != yes_value &
                                                  !!rlang::sym(iycf_6d_swt) != yes_value &
                                                  !!rlang::sym(iycf_6h_swt) != yes_value &
                                                  !!rlang::sym(iycf_6j_swt) != yes_value &
                                                  !!rlang::sym(iycf_6e) != yes_value &
                                                  !!rlang::sym(iycf_6f) != yes_value &
                                                  !!rlang::sym(iycf_6g) != yes_value ~ 0,
                                                TRUE ~ NA),
                    iycf_swb = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                  as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_swb))
  }

  # IYCF Indicator 14: Unhealthy Food Consumption (UFC)

    if(length(setdiff(length(c(iycf_7p,iycf_7q)),2)) != 0) {
    warning("Unhealthy Food Consumption related columns not found in dataset.\nIYCF Indicator 14: Unhealthy Food Consumption (UFC) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(iycf_ufc = dplyr::case_when(!!rlang::sym(iycf_7p) == yes_value |
                                                  !!rlang::sym(iycf_7q) == yes_value ~ 1,
                                                !!rlang::sym(iycf_7p) != yes_value &
                                                  !!rlang::sym(iycf_7q) != yes_value ~ 0,
                                                TRUE ~ NA),
                    iycf_ufc = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                  as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months))  ~ NA,
                                                TRUE ~ iycf_ufc))
  }

  # IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF)
  zero_columns <- c("iycf_7c_zero","iycf_7e_zero",
                    "iycf_7f_zero","iycf_7g_zero","iycf_7h_zero")

  if(length(setdiff(length(zero_columns),5)) != 0) {
    warning("Zero Vegetable or Fruit Consumption related columns not found in dataset.\nIYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF) not calculated")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate_at(dplyr::vars(zero_columns), ~ dplyr::case_when(. == no_value ~ 1,
                                                                     . != no_value ~ 0,
                                                                     TRUE ~ NA)) %>%
      dplyr::mutate(zvf_sum = rowSums(dplyr::across(zero_columns, .fns = as.numeric), na.rm = T),
                    iycf_zvf = dplyr::case_when(zvf_sum == 5 ~ 1,
                                                zvf_sum < 5 ~ 0,
                                                TRUE ~ NA),
                    iycf_zvf = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) < 6 |
                                                  as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                  is.na(!!rlang::sym(age_months)) ~ NA,
                                                TRUE ~ iycf_zvf))
  }

  # IYCF Indicator 16: Bottle Feeding 0-23 months
  if(!is.null(iycf_5)){
    if(!iycf_5 %in% names(.dataset)) {
      warning("IYCF 5 column not found in dataset.\nIYCF Indicator 16: Bottle Feeding 0-23 months not calculated")
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(iycf_bof = dplyr::case_when(!!rlang::sym(iycf_5) == yes_value ~ 1,
                                                  !!rlang::sym(iycf_5) != yes_value ~ 0,
                                                  TRUE ~ NA),
                      iycf_bof = dplyr::case_when(as.numeric(!!rlang::sym(age_months)) >= 24 |
                                                    is.na(!!rlang::sym(age_months)) |
                                                    is.na(!!rlang::sym(iycf_5)) ~ NA,
                                                  TRUE ~ iycf_bof))
    }
  }

  if (is.null(loop_index)) {
    ## initiate the return output
    .dataset <- .dataset %>%
      dplyr::mutate(loop_index = paste0("loop_iycf_",dplyr::row_number()))
  }

  options(warn = 0)
  return(.dataset)
}
