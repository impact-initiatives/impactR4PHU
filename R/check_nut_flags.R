#' check_nut_flags
#'
#' @param .dataset the raw child nutrition loop dataset with all add_muac and add_mfaz called
#' @param nut_muac_cm the name of the variable that indicates the MUAC measurement by CM.
#' By default it is nut_muac_cm
#' @param edema_confirm the name of the variable that indicates that edema is confirmed
#' By default it is nut_edema_confirm
#' @param value_edema_confirm the value of the choice "yes" of the nut_edema_confirm indicator
#' By default it is "yes"
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#' @param loop_index unique identifier for each individual in the child nutrition loop.
#' By default, loop_index
#'
#' @return a dataframe that includes all the logical flags related to nutrition
#' This includes:
#' - flag_sd_mfaz
#' - mfaz_who_flag
#' - mfaz_noflag
#' - mean_mfaz_noflag
#' - sd_mfaz_noflag
#' - global_mfaz_noflag
#' - moderate_mfaz_noflag
#' - severe_mfaz_noflag
#' - flag_extreme_muac
#' - muac_noflag
#' - gam_muac_noflag
#' - mam_muac_noflag
#' - sam_muac_noflag
#' - flag_severe_hhs
#' - flag_edema_pitting
#' @export
#'
#' @examples
#' \dontrun{check_nut_flags(df)}
check_nut_flags <- function(.dataset,
                            nut_muac_cm = "nut_muac_cm",
                            edema_confirm = "nut_edema_confirm",
                            value_edema_confirm = "yes",
                            grouping = NULL,
                            uuid = "uuid",
                            loop_index = NULL) {

  options(warn = -1)
  ## Throw an error if a dataset wasn't provided as a first argument
  if (!is.data.frame(.dataset)) {
    stop("First argument should be a dataset")
  }

  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  if (is.null(grouping)) {
    .dataset <- .dataset %>% dplyr::mutate(group = "All")
  } else {
    .dataset <- .dataset %>% dplyr::mutate(group = !!rlang::sym(grouping))
  }

  if (is.null(loop_index)) {
    ## initiate the return output
    results <- .dataset %>%
      dplyr::mutate(loop_index = paste0("loop_nut_",dplyr::row_number())) %>%
      dplyr::select(uuid, group, loop_index,sex,age_months,age_days,edema_confirm)
  } else {
    results <- .dataset %>%
      dplyr::select(uuid, group, loop_index,sex,age_months,age_days,edema_confirm)
  }

  # combine all mfaz_cols together
  mfaz_cols <- c("mfaz","severe_mfaz","moderate_mfaz","global_mfaz")

  ## Test if all columns are in the dataset
  if(!all(mfaz_cols %in% names(.dataset))) {
    stop("Missing mfaz columns")
  } else{
    results2 <- .dataset

    mean_mfaz_dataset <- mean(results2$mfaz, na.rm=T)

    results2 <- results2 %>%
      dplyr::mutate(flag_sd_mfaz = ifelse(is.na(mfaz),NA,
                                          ifelse(mfaz < mean_mfaz_dataset - 4 | mfaz > mean_mfaz_dataset + 3, 1, 0)),
                    mfaz_who_flag = ifelse(is.na(mfaz), NA, ifelse(mfaz < -5 | mfaz > 5, 1, 0)),
                    mfaz_noflag = ifelse(is.na(mfaz) | flag_sd_mfaz == 1, NA, mfaz),
                    mean_mfaz_noflag = round(mean(mfaz_noflag, na.rm = TRUE),3),
                    sd_mfaz_noflag = round(stats::sd(mfaz_noflag, na.rm = TRUE),2),
                    global_mfaz_noflag = ifelse(is.na(global_mfaz), NA, ifelse(is.na(flag_sd_mfaz), global_mfaz, ifelse(flag_sd_mfaz == 1, NA, global_mfaz))),
                    moderate_mfaz_noflag = ifelse(is.na(moderate_mfaz), NA, ifelse(is.na(flag_sd_mfaz), moderate_mfaz, ifelse(flag_sd_mfaz == 1, NA, moderate_mfaz))),
                    severe_mfaz_noflag = ifelse(is.na(severe_mfaz), NA, ifelse(is.na(flag_sd_mfaz), severe_mfaz, ifelse(flag_sd_mfaz == 1, NA, severe_mfaz))))%>%
      dplyr::select(mfaz,severe_mfaz,moderate_mfaz,global_mfaz,flag_sd_mfaz,mfaz_who_flag,
                    mfaz_noflag,mean_mfaz_noflag,sd_mfaz_noflag,
                    global_mfaz_noflag,moderate_mfaz_noflag,severe_mfaz_noflag)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  # combine all muac_cols together
  muac_cols <- c(nut_muac_cm,"sam_muac","mam_muac","gam_muac")
  ## Test if all columns are in the dataset
  if(!all(muac_cols %in% names(.dataset))) {
    stop("Missing muac columns")
  } else{
    results2 <- .dataset %>%
      dplyr::mutate(flag_extreme_muac = ifelse(is.na(nut_muac_cm), NA,
                                               ifelse(nut_muac_cm < 7 | nut_muac_cm > 22, 1, 0)),
                    muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, nut_muac_cm)),
                    gam_muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, gam_muac)),
                    mam_muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, mam_muac)),
                    sam_muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, sam_muac))) %>%
      dplyr::select(nut_muac_cm,sam_muac,mam_muac,gam_muac,nut_muac_mm,gam_muac_noflag,
                    mam_muac_noflag,sam_muac_noflag,flag_extreme_muac,muac_noflag)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## Test if all columns are in the dataset
  if(!edema_confirm %in% names(.dataset)) {
    stop("Missing edema_confirm columns")
  } else{
    results2 <- .dataset %>%
      dplyr::mutate(flag_edema_pitting = ifelse(is.na(!!rlang::sym(edema_confirm)), NA,
                                                ifelse(!!rlang::sym(edema_confirm) == value_edema_confirm,1,0))) %>%
      dplyr::select(flag_edema_pitting)

    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }

  options(warn = 0)
  return(results)
}
