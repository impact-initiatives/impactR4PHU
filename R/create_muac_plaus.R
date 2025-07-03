#' create_muac_plaus
#'
#' @param .dataset raw/clean data with all calculated
#' fcs/rcsi/hhs/hdds/fcm/fclcm add_x indicators
#' @param fsl_fcs_cereal  the name of the variable that indicates the number of days cereals were consumed
#' @param fsl_fcs_legumes  the name of the variable that indicates the number of days legumes were consumed
#' @param fsl_fcs_dairy  the name of the variable that indicates the number of days dairy were consumed
#' @param fsl_fcs_meat  the name of the variable that indicates the number of days meat were consumed
#' @param fsl_fcs_veg  the name of the variable that indicates the number of days vegetables were consumed
#' @param fsl_fcs_fruit  the name of the variable that indicates the number of days fruit were consumed
#' @param fsl_fcs_oil  the name of the variable that indicates the number of days oil was consumed
#' @param fsl_fcs_sugar  the name of the variable that indicates the number of days sugar was consumed
#' @param fsl_rcsi_lessquality Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to rely on less preferred and less expensive food to cope with a lack of food or money to buy it?
#' @param fsl_rcsi_borrow Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to borrow food or rely on help from a relative or friend to cope with a lack of food or money to buy it?
#' @param fsl_rcsi_mealsize Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to limit portion size of meals at meal times to cope with a lack of food or money to buy it?
#' @param fsl_rcsi_mealadult Column representing question- During the last 7 days, were there days (and, if so, how many) when your household had to restrict consumption by adults in order for small children to eat to cope with a lack of food or money to buy it?
#' @param fsl_rcsi_mealnb Column representing question - During the last 7 days, were there days (and, if so, how many) when your household had to reduce number of meals eaten in a day to cope with a lack of food or money to buy it?
#' @param fsl_fcs_score Column representing FCS Score
#' @param fsl_rcsi_score Column representing rCSI Score
#' @param fsl_hhs_score Column representing HHS Score
#' @param fsl_hdds_score Column representing HDDS Score
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#' @param short_report Inputs a boolean value TRUE or FALSE to return just key variables. If FALSE,
#' returns a dataframe of all the variables calculated.
#' @param file_path Inputs an optional character value specifying the file location to save a copy
#' of the results.
#'
#' @return a dataframe with all fsl related plausibility columns
#' @export
#'
#' @examples
#' \dontrun{create_muac_plaus(df)}

create_muac_plaus <- function(
  df,
  grouping = NULL,
  file_path = NULL,
  short_report = NULL
) {
  options(warn = -1)
  if (is.null(short_report)) {
    short_report <- FALSE
  }
  if (!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }
  # check which indexes are present

  if (!(c("nut_edema_confirm") %in% names(df))) {
    df2 <- df %>%
      dplyr::mutate(
        oedemas = ifelse(
          is.na(nut_edema_confirm),
          0,
          ifelse(nut_edema_confirm == "yes", 1, 0)
        )
      ) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(num_oedema = sum(oedemas, na.rm = TRUE))

    if (!exists("results.table")) {
      results.table <- df2
    } else {
      results.table <- merge(results.table, df2)
    }
  } else {
    df <- df %>% dplyr::mutate(oedema = 0)
  }

  if (c("sex") %in% names(df)) {
    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(
        sex_ratio = round(
          as.numeric(nipnTK::sexRatioTest(
            sex,
            codes = c("1", "2"),
            pop = c(1, 1)
          )[1]),
          3
        ),
        sex_ratio.pvalue = round(
          as.numeric(nipnTK::sexRatioTest(
            sex,
            codes = c("1", "2"),
            pop = c(1, 1)
          )[5]),
          2
        )
      )

    if (!exists("results.table")) {
      results.table <- df2
    } else {
      results.table <- merge(results.table, df2)
    }
  }
  if (c("age_months") %in% names(df)) {
    df2 <- df %>%
      dplyr::filter(!is.na(age_months)) %>%
      dplyr::filter(age_months >= 6 & age_months < 60) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(
        age_ratio = nipnTK::ageRatioTest(age_months, ratio = 0.85)[3],
        age_ratio = round(as.numeric(age_ratio), 2),
        age_ratio.pvalue = round(
          as.numeric(nipnTK::ageRatioTest(age_months, ratio = 0.85)[7]),
          2
        ),
      )

    if (!exists("results.table")) {
      results.table <- df2
    } else {
      results.table <- merge(results.table, df2)
    }
  }
  if (c("nut_muac_cm") %in% names(df)) {
    df2 <- df %>%
      dplyr::mutate(
        oedemas = ifelse(
          is.na(nut_edema_confirm),
          0,
          ifelse(nut_edema_confirm == "yes", 1, 0)
        )
      ) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(
        n_children_muac = sum(!is.na(nut_muac_cm), na.rm = TRUE),
        dps_muac = nipnTK::digitPreference(nut_muac_cm)[[1]],
        mean_muac = round(mean(nut_muac_cm, na.rm = TRUE), 3),
        sd_muac = round(stats::sd(nut_muac_cm, na.rm = TRUE), 2),
        sd_muac_mm = round(stats::sd(nut_muac_mm, na.rm = TRUE), 2),
        num_muac_flags = sum(flag_extreme_muac, na.rm = TRUE),
        mean_muac_noflag = round(mean(muac_noflag, na.rm = TRUE), 3),
        sd_muac_noflag = round(stats::sd(muac_noflag, na.rm = TRUE), 2),
        gam_muac_abs = round(mean(gam_muac_noflag, na.rm = TRUE) * 100, 3),
        gam_muac_low = round(
          gam_muac_abs -
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(gam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        gam_muac_upp = round(
          gam_muac_abs +
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(gam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        mam_muac_abs = round(mean(mam_muac_noflag, na.rm = TRUE) * 100, 3),
        mam_muac_low = round(
          mam_muac_abs -
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(mam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        mam_muac_upp = round(
          mam_muac_abs +
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(mam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        sam_muac_abs = round(mean(sam_muac_noflag, na.rm = TRUE) * 100, 3),
        sam_muac_low = round(
          sam_muac_abs -
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(sam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        sam_muac_upp = round(
          sam_muac_abs +
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(sam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        )
      ) %>%
      dplyr::mutate(
        sam_muac_low = ifelse(sam_muac_low < 0, 0, sam_muac_low),
        mam_muac_low = ifelse(mam_muac_low < 0, 0, mam_muac_low),
        gam_muac_low = ifelse(gam_muac_low < 0, 0, gam_muac_low)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        gam_muac_results = paste0(
          gam_muac_abs,
          "% [",
          gam_muac_low,
          " - ",
          gam_muac_upp,
          "]"
        ),
        mam_muac_results = paste0(
          mam_muac_abs,
          "% [",
          mam_muac_low,
          " - ",
          mam_muac_upp,
          "]"
        ),
        sam_muac_results = paste0(
          sam_muac_abs,
          "% [",
          sam_muac_low,
          " - ",
          sam_muac_upp,
          "]"
        ),
        mean_sd_muac = paste0(
          round(mean_muac_noflag, 2),
          "+/-",
          round(sd_muac_noflag, 2)
        ),
        gam_muac_abs = NULL,
        gam_muac_low = NULL,
        gam_muac_upp = NULL,
        mam_muac_abs = NULL,
        mam_muac_low = NULL,
        mam_muac_upp = NULL,
        sam_muac_abs = NULL,
        sam_muac_low = NULL,
        sam_muac_upp = NULL
      )

    if (!exists("results.table")) {
      results.table <- df2
    } else {
      results.table <- merge(results.table, df2)
    }
  }

  if (c("mfaz") %in% names(df)) {
    df2 <- df %>%
      dplyr::mutate(
        oedemas = ifelse(
          is.na(nut_edema_confirm),
          NA,
          ifelse(nut_edema_confirm == "yes", 1, 0)
        )
      ) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(
        n_children_mfaz = sum(!is.na(mfaz), na.rm = TRUE),
        mean_mfaz = round(mean(mfaz, na.rm = TRUE), 3),
        sd_mfaz = round(stats::sd(mfaz, na.rm = TRUE), 2),
        mean_mfaz_noflag = round(mean(mfaz_noflag, na.rm = TRUE), 3),
        sd_mfaz_noflag = round(stats::sd(mfaz_noflag, na.rm = TRUE), 2),
        num_smart_flags_mfaz = sum(flag_sd_mfaz, na.rm = TRUE),
        flag_perc_mfaz_children = round(
          num_smart_flags_mfaz / n_children_mfaz,
          2
        ),
        prop_smart_flags_mfaz = round(
          (sum(flag_sd_mfaz, na.rm = TRUE) / n_children_mfaz) * 100,
          2
        ),
        skewness_mfaz = abs(as.numeric(nipnTK::skewKurt(mfaz_noflag)[1])),
        kurtosis_mfaz = abs(as.numeric(nipnTK::skewKurt(mfaz_noflag)[5])),
        new_global_mfaz = round(mean(global_mfaz_noflag, na.rm = TRUE), 3) *
          100,
        global_mfaz_low = round(
          new_global_mfaz -
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(global_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        global_mfaz_upp = round(
          new_global_mfaz +
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(global_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        moderate_mfaz = round(mean(moderate_mfaz_noflag, na.rm = TRUE), 3) *
          100,
        moderate_mfaz_low = round(
          moderate_mfaz -
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(moderate_mfaz_noflag, na.rm = TRUE) /
                sqrt(dplyr::n())) *
              100),
          2
        ),
        moderate_mfaz_upp = round(
          moderate_mfaz +
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(moderate_mfaz_noflag, na.rm = TRUE) /
                sqrt(dplyr::n())) *
              100),
          2
        ),
        severe_mfaz = round(mean(severe_mfaz_noflag, na.rm = TRUE), 3) * 100,
        severe_mfaz_low = round(
          severe_mfaz -
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(severe_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        ),
        severe_mfaz_upp = round(
          severe_mfaz +
            (stats::qt(p = 0.05 / 2, df = dplyr::n() - 1, lower.tail = F) *
              (stats::sd(severe_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n())) *
              100),
          2
        )
      ) %>%
      dplyr::mutate(
        severe_mfaz_low = ifelse(severe_mfaz_low < 0, 0, severe_mfaz_low),
        moderate_mfaz_low = ifelse(moderate_mfaz_low < 0, 0, moderate_mfaz_low),
        global_mfaz_low = ifelse(global_mfaz_low < 0, 0, global_mfaz_low)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        mfaz_results = paste0(
          new_global_mfaz,
          "% [",
          global_mfaz_low,
          " - ",
          global_mfaz_upp,
          "]"
        ),
        moderate_mfaz_results = paste0(
          moderate_mfaz,
          "% [",
          moderate_mfaz_low,
          " - ",
          moderate_mfaz_upp,
          "]"
        ),
        severe_mfaz_results = paste0(
          severe_mfaz,
          "% [",
          severe_mfaz_low,
          " - ",
          severe_mfaz_upp,
          "]"
        ),
        mean_sd_mfaz = paste0(
          round(mean_mfaz_noflag, 2),
          "+/-",
          round(sd_mfaz_noflag, 2)
        ),
        new_global_mfaz = NULL,
        global_mfaz_low = NULL,
        global_mfaz_upp = NULL,
        moderate_mfaz = NULL,
        moderate_mfaz_low = NULL,
        moderate_mfaz_upp = NULL,
        severe_mfaz = NULL,
        severe_mfaz_low = NULL,
        severe_mfaz_upp = NULL
      )
  }

  if (!exists("results.table")) {
    results.table <- df2
  } else {
    results.table <- merge(results.table, df2)
  }
}
