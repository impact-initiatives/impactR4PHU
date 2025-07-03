#' add_muac
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
#' By default it is "yes"
#'
#' @return the dataset with the sam_muac, mam_muac, and gam_muac calcualted
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
#' add_muac(.dataset = df1)


add_muac <- function(.dataset,
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
  if(!is.null(nut_muac_cm)){
    if(!nut_muac_cm %in% names(.dataset)) {
      stop("Missing nut_muac_cm column")
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(sex = ifelse(!!rlang::sym(child_sex) == value_male_sex,1,2),
                      age_months = as.numeric(!!rlang::sym(child_age_months)),
                      age_days = as.numeric(!!rlang::sym(child_age_months))* 30.25,
                      nut_muac_cm = !!rlang::sym(nut_muac_cm),
                      nut_muac_mm = as.numeric(nut_muac_cm)*10,
                      child_age_months = as.numeric(!!rlang::sym(child_age_months)))

      if(!is.null(edema_confirm)){
        if(!edema_confirm %in% names(.dataset)){
          warning("Missing edema_confirm column")
          .dataset <- .dataset %>%
            dplyr::mutate(sam_muac = ifelse(is.na(!!rlang::sym(nut_muac_cm)), NA, ifelse(as.numeric(!!rlang::sym(nut_muac_cm)) < 11.5, 1, 0)),
                          mam_muac = ifelse(is.na(!!rlang::sym(nut_muac_cm)), NA, ifelse(as.numeric(!!rlang::sym(nut_muac_cm)) >= 11.5 & !!rlang::sym(nut_muac_cm) < 12.5, 1, 0)),
                          gam_muac = ifelse(is.na(!!rlang::sym(nut_muac_cm)), NA, ifelse(as.numeric(!!rlang::sym(nut_muac_cm)) < 12.5, 1, 0)),
                          sam_muac = ifelse(age_months < 6 | age_months >=60, NA, sam_muac),
                          mam_muac = ifelse(age_months < 6 | age_months >=60, NA, mam_muac),
                          gam_muac = ifelse(age_months < 6 | age_months >=60, NA, gam_muac))
        } else {
          .dataset <- .dataset %>%
            dplyr::mutate(sam_muac = ifelse(is.na(!!rlang::sym(nut_muac_cm)), NA, ifelse(as.numeric(!!rlang::sym(nut_muac_cm)) < 11.5, 1, 0)),
                          mam_muac = ifelse(is.na(!!rlang::sym(nut_muac_cm)), NA, ifelse(as.numeric(!!rlang::sym(nut_muac_cm)) >= 11.5 & !!rlang::sym(nut_muac_cm) < 12.5, 1, 0)),
                          gam_muac = ifelse(is.na(!!rlang::sym(nut_muac_cm)), NA, ifelse(as.numeric(!!rlang::sym(nut_muac_cm)) < 12.5, 1, 0)),
                          sam_muac = ifelse(is.na(!!rlang::sym(edema_confirm)), sam_muac, ifelse(!!rlang::sym(edema_confirm) == value_edema_confirm, 1, sam_muac)),
                          gam_muac = ifelse(is.na(!!rlang::sym(edema_confirm)), gam_muac, ifelse(!!rlang::sym(edema_confirm) == value_edema_confirm, 1, gam_muac)),
                          sam_muac = ifelse(age_months < 6 | age_months >=60, NA, sam_muac),
                          mam_muac = ifelse(age_months < 6 | age_months >=60, NA, mam_muac),
                          gam_muac = ifelse(age_months < 6 | age_months >=60, NA, gam_muac),
                          nut_edema_confirm = !!rlang::sym(edema_confirm))
        }
      }
    }
  }

  options(warn = 0)
  return(.dataset)
}
