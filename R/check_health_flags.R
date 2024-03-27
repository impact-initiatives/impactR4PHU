#' check_health_flags
#'
#' @param .dataset the raw dataset with all add_x indicators functions called in
#' @param monthly_expenditures the vector of the frequent expenditure variables.
#' By default including (food,rent,water,nfi,utilities,fuel,transportation,communication,others)
#' @param health_expenditure_col the name of the variable that indicates the infrequent
#' expenditure on health.
#' @param periodic_expenditures the vector of the infrequent expenditure variables.
#' By default including (shelter,nfi,health,education,debt,others)
#' @param num_period_months the number of months that cover the infrequent expenditure variables.
#' By default: 6
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#'
#' @return a dataframe that includes all the logical flags related to health
#' This includes:
#' - flag_severe_health_exp
#' - flag_catastrophic_health_exp
#'
#' @export
#'
#' @examples
#' \dontrun{check_health_flags(.dataset = df1)}


check_health_flags <- function(.dataset,
                               monthly_expenditures = c("cm_expenditure_frequent_food",
                                                        "cm_expenditure_frequent_rent",
                                                        "cm_expenditure_frequent_water",
                                                        "cm_expenditure_frequent_nfi",
                                                        "cm_expenditure_frequent_utilitiues",
                                                        "cm_expenditure_frequent_fuel",
                                                        "cm_expenditure_frequent_transportation",
                                                        "cm_expenditure_frequent_communication",
                                                        "cm_expenditure_frequent_other"),
                               health_expenditure_col = "cm_expenditure_infrequent_health",
                               periodic_expenditures = c("cm_expenditure_infrequent_shelter",
                                                         "cm_expenditure_infrequent_nfi",
                                                         "cm_expenditure_infrequent_health",
                                                         "cm_expenditure_infrequent_education",
                                                         "cm_expenditure_infrequent_debt",
                                                         "cm_expenditure_infrequent_other"),
                               num_period_months = 6,
                               grouping = NULL,
                               uuid = "uuid") {
  options(warn = -1)
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if (!uuid %in% names(.dataset)) stop("uuid argument incorrect, or not available in the dataset")

  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
  } else {
    .dataset <- .dataset %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  ## initiate the return output
  results <- .dataset %>%
    dplyr::select(uuid, group)

  ## Health Expenditures
  if(!health_expenditure_col %in% names(.dataset)) {
    warning("Missing Health Expenditure Column")
  } else {
    if(!all(monthly_expenditures %in% names(.dataset))){
      warning("Missing Frequent Expenditure Columns")
      results2 <- .dataset %>%
        dplyr::mutate_at(dplyr::vars(health_expenditure_col), as.numeric) %>%
        dplyr::mutate(month_exp = rowSums(dplyr::across(periodic_expenditures), na.rm = T) / as.numeric(num_period_months),
                      health_exp = ifelse(is.na(!!rlang::sym(health_expenditure_col)),0,
                                                                     !!rlang::sym(health_expenditure_col) / as.numeric(num_period_months)),
                      prop_health_exp = ifelse(is.na(month_exp), NA,
                                               health_exp / month_exp),
                      flag_severe_health_exp =  ifelse(is.na(prop_health_exp), NA,
                                                  ifelse(prop_health_exp >= 0.10 & prop_health_exp <=0.25, 1, 0)),
                      flag_catastrophic_health_exp =  ifelse(is.na(prop_health_exp), NA, ifelse(prop_health_exp > 0.25, 1, 0))) %>%
        dplyr::select(flag_severe_health_exp,flag_catastrophic_health_exp)
      if(!exists("results")){
        results <- results2
      } else {
        results <- cbind(results,results2)
      }
    } else {
      results2 <- .dataset %>%
        dplyr::mutate_at(dplyr::vars(health_expenditure_col), as.numeric) %>%
        dplyr::mutate_at(dplyr::vars(periodic_expenditures), as.numeric) %>%
        dplyr::mutate(month_exp_1 = rowSums(dplyr::across(periodic_expenditures), na.rm = T) / as.numeric(num_period_months),
                      month_exp_2 = rowSums(dplyr::across(monthly_expenditures), na.rm = T),
                      month_exp = rowSums(dplyr::across(c(month_exp_1,month_exp_2))),
                      health_exp = ifelse(is.na(!!rlang::sym(health_expenditure_col)),0,
                                          !!rlang::sym(health_expenditure_col) / as.numeric(num_period_months)),
                      prop_health_exp = ifelse(is.na(month_exp), NA,
                                               health_exp / month_exp),
                      flag_severe_health_exp =  ifelse(is.na(prop_health_exp), NA,
                                                  ifelse(prop_health_exp >= 0.10 & prop_health_exp <=0.25, 1, 0)),
                      flag_catastrophic_health_exp =  ifelse(is.na(prop_health_exp), NA, ifelse(prop_health_exp > 0.25, 1, 0))) %>%
        dplyr::select(flag_severe_health_exp,flag_catastrophic_health_exp)

      if(!exists("results")){
        results <- results2
      } else {
        results <- cbind(results,results2)
      }
    }
  }

  ## WASHINTON GROUP QUESTIONS
  #### TO ADD
  options(warn = 0)
  return(results)
}

