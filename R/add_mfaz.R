#' add_mfaz
#'
#' @param .dataset Child Nutrition Loop Dataset
#' @param nut_muac_cm the name of the variable that indicates the MUAC measurement by CM.
#' By default it is nut_muac_cm
#' @param edema_confirm the name of the variable that indicates that edema is confirmed
#' By default it is nut_edema_confirm
#' @param child_age_months the name of the variable that indicates the age of child per month.
#' By default it is child_age_months
#' @param child_sex the name of the variable that indicates the sex of the child
#' By default it is child_sex
#' @param value_male_sex the value of the choice "male" of the sex indicator
#' By default it is m
#' @param value_edema_confirm the value of the choice "yes" of the nut_edema_confirm indicator
#' By default it is "yes
#'
#' @return the dataset with the severe_mfaz, global_mfaz, and moderate_mfaz calcualted
#' @export
#'
#' @examples
#' df1 <- data.frame(
#' uuid = c("uuid_1","uuid_2"),
#' nut_muac_cm = c("12.5","10"),
#' child_sex = c("m","f"),
#' child_age_months = c("14","54"),
#' nut_edema_confirm = c("yes",NA))
#'
#' add_mfaz(.dataset = df1)

add_mfaz <- function(.dataset,
                     nut_muac_cm = "nut_muac_cm",
                     edema_confirm = "nut_edema_confirm",
                     child_age_months = "child_age_months",
                     child_sex = "child_sex",
                     value_male_sex = "m",
                     value_edema_confirm = "yes") {

  options(warn = -1)
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  ## Test if all columns are in the dataset
  if(!nut_muac_cm %in% names(.dataset)) {
    stop("Missing nut_muac_cm column")
  } else {
    .dataset <- .dataset %>%
      dplyr::mutate(!!rlang::sym(nut_muac_cm) := as.numeric(!!rlang::sym(nut_muac_cm)),
                    sex = ifelse(!!rlang::sym(child_sex) == value_male_sex,1,2),
                    age_months = as.numeric(!!rlang::sym(child_age_months)),
                    age_days = as.numeric(!!rlang::sym(child_age_months))* 30.25)
    .dataset <- zscorer::addWGSR(data = .dataset,
                                 sex = "sex",
                                 firstPart = nut_muac_cm,
                                 secondPart = "age_days",
                                 index = "mfa")

    if(!edema_confirm %in% names(.dataset)){
      warning("Missing edema_confirm column")
      .dataset <- .dataset %>%
        dplyr::mutate(severe_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz < -3, 1, 0)),
                      moderate_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz >= -3 & mfaz < -2, 1, 0)),
                      global_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz < -2, 1, 0)),
                      severe_mfaz = ifelse(age_months < 6 | age_months >=60, NA, severe_mfaz),
                      moderate_mfaz = ifelse(age_months < 6 | age_months >=60, NA, moderate_mfaz),
                      global_mfaz = ifelse(age_months < 6 | age_months >=60, NA, global_mfaz))
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(severe_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz < -3, 1, 0)),
                      moderate_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz >= -3 & mfaz < -2, 1, 0)),
                      global_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz < -2, 1, 0)),
                      severe_mfaz = ifelse(is.na(!!rlang::sym(edema_confirm)), severe_mfaz, ifelse(!!rlang::sym(edema_confirm) == value_edema_confirm, 1, severe_mfaz)),
                      global_mfaz = ifelse(is.na(!!rlang::sym(edema_confirm)), global_mfaz, ifelse(!!rlang::sym(edema_confirm) == value_edema_confirm, 1, global_mfaz)),
                      severe_mfaz = ifelse(age_months < 6 | age_months >=60, NA, severe_mfaz),
                      moderate_mfaz = ifelse(age_months < 6 | age_months >=60, NA, moderate_mfaz),
                      global_mfaz = ifelse(age_months < 6 | age_months >=60, NA, global_mfaz))
    }
  }

  options(warn = 0)
  return(.dataset)
}
