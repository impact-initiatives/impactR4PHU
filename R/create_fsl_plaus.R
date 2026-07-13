#' create_fsl_plaus
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
#' \dontrun{create_fsl_plaus(df)}

create_fsl_plaus <- function(.dataset,
                             fsl_fcs_cereal = "fsl_fcs_cereal",
                             fsl_fcs_legumes = "fsl_fcs_legumes",
                             fsl_fcs_dairy = "fsl_fcs_dairy",
                             fsl_fcs_meat = "fsl_fcs_meat",
                             fsl_fcs_veg = "fsl_fcs_veg",
                             fsl_fcs_fruit = "fsl_fcs_fruit",
                             fsl_fcs_oil = "fsl_fcs_oil",
                             fsl_fcs_sugar = "fsl_fcs_sugar",
                             fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                             fsl_rcsi_borrow = "fsl_rcsi_borrow",
                             fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                             fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                             fsl_rcsi_mealnb = "fsl_rcsi_mealnb",
                             fsl_fcs_score = "fsl_fcs_score",
                             fsl_rcsi_score = "fsl_rcsi_score",
                             fsl_hhs_score = "fsl_hhs_score",
                             fsl_hdds_score = "fsl_hdds_score",
                             grouping = NULL,
                             uuid = "uuid",
                             short_report = FALSE,
                             file_path = NULL){
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

  if ("fsl_fcs_score" %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(mean_fcs = round(mean(fsl_fcs_score, na.rm = TRUE), 2),
                       sd_fcs = round(stats::sd(fsl_fcs_score, na.rm = TRUE), 2),
                       mean_days_cereals = round(mean(!!rlang::sym(fsl_fcs_cereal), na.rm = TRUE), 2),
                       sd_days_cereals = round(stats::sd(!!rlang::sym(fsl_fcs_cereal), na.rm = TRUE), 2),
                       mean_days_legumes = round(mean(!!rlang::sym(fsl_fcs_legumes), na.rm = TRUE), 2),
                       sd_days_legumes = round(stats::sd(!!rlang::sym(fsl_fcs_legumes), na.rm = TRUE), 2),
                       mean_days_dairy = round(mean(!!rlang::sym(fsl_fcs_dairy), na.rm = TRUE), 2),
                       sd_days_dairy = round(stats::sd(!!rlang::sym(fsl_fcs_dairy), na.rm = TRUE), 2),
                       mean_days_meat = round(mean(!!rlang::sym(fsl_fcs_meat), na.rm = TRUE), 2),
                       sd_days_meat = round(stats::sd(!!rlang::sym(fsl_fcs_meat), na.rm = TRUE), 2),
                       mean_days_veg = round(mean(!!rlang::sym(fsl_fcs_veg), na.rm = TRUE), 2),
                       sd_days_veg = round(stats::sd(!!rlang::sym(fsl_fcs_veg), na.rm = TRUE), 2),
                       mean_days_fruit = round(mean(!!rlang::sym(fsl_fcs_fruit), na.rm = TRUE), 2),
                       sd_days_fruit = round(stats::sd(!!rlang::sym(fsl_fcs_fruit), na.rm = TRUE), 2),
                       mean_days_oils = round(mean(!!rlang::sym(fsl_fcs_oil), na.rm = TRUE), 2),
                       sd_days_oils = round(stats::sd(!!rlang::sym(fsl_fcs_oil), na.rm = TRUE), 2),
                       mean_days_sugar = round(mean(!!rlang::sym(fsl_fcs_sugar), na.rm = TRUE), 2),
                       sd_days_sugar = round(stats::sd(!!rlang::sym(fsl_fcs_sugar), na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if ("fsl_rcsi_score" %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(mean_rcsi = round(mean(fsl_rcsi_score, na.rm = TRUE), 2),
                       sd_rcsi = round(stats::sd(fsl_rcsi_score, na.rm = TRUE), 2),
                       mean_rcsi_lessquality = round(mean(!!rlang::sym(fsl_rcsi_lessquality), na.rm = TRUE), 2),
                       sd_rcsi_lessquality = round(stats::sd(!!rlang::sym(fsl_rcsi_lessquality), na.rm = TRUE), 2),
                       mean_rcsi_borrow = round(mean(!!rlang::sym(fsl_rcsi_borrow), na.rm = TRUE), 2),
                       sd_rcsi_borrow = round(stats::sd(!!rlang::sym(fsl_rcsi_borrow), na.rm = TRUE), 2),
                       mean_rcsi_mealsize = round(mean(!!rlang::sym(fsl_rcsi_mealsize), na.rm = TRUE), 2),
                       sd_rcsi_mealsize = round(stats::sd(!!rlang::sym(fsl_rcsi_mealsize), na.rm = TRUE), 2),
                       mean_rcsi_mealadult = round(mean(!!rlang::sym(fsl_rcsi_mealadult), na.rm = TRUE), 2),
                       sd_rcsi_mealadult = round(stats::sd(!!rlang::sym(fsl_rcsi_mealadult), na.rm = TRUE), 2),
                       mean_rcsi_mealnb = round(mean(!!rlang::sym(fsl_rcsi_mealnb), na.rm = TRUE), 2),
                       sd_rcsi_mealnb = round(stats::sd(!!rlang::sym(fsl_rcsi_mealnb), na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if ("fsl_hhs_score" %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(mean_hhs = round(mean(fsl_hhs_score, na.rm = TRUE), 2),
                       sd_hhs = round(stats::sd(fsl_hhs_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if ("fsl_hdds_score" %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(mean_hdds = round(mean(fsl_hdds_score, na.rm = TRUE), 2),
                       sd_hdds = round(stats::sd(fsl_hdds_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (all(c("fsl_fcs_score", "fsl_rcsi_score") %in% names(.dataset))) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::select(group)

    filtered_group <- .dataset %>%
      dplyr::mutate(fsl_fcs_score_notna = ifelse(!is.na(!!rlang::sym(fsl_fcs_score)),1,0),
                    fsl_rcsi_score_notna = ifelse(!is.na(!!rlang::sym(fsl_rcsi_score)),1,0)) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(fsl_fcs_score_notna = sum(fsl_fcs_score_notna, na.rm = T),
                       fsl_rcsi_score_notna = sum(fsl_rcsi_score_notna, na.rm = T)) %>%
      dplyr::filter(fsl_fcs_score_notna >= 3 & fsl_rcsi_score_notna >= 3) %>%
      dplyr::pull(group)

    draft_result <- .dataset %>%
      dplyr::filter(group %in% filtered_group) %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.fcs_rcsi = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_rcsi_score)[4]), 2),
                       corr.fcs_rcsi.pvalue = as.numeric(stats::cor.test(fsl_fcs_score, fsl_rcsi_score)[3]))

    results2 <- results2 %>%
      dplyr::left_join(draft_result)

    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  } else {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.fcs_rcsi = NA,
                       corr.fcs_rcsi.pvalue = NA)
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if (all(c("fsl_fcs_score", "fsl_hhs_score") %in% names(.dataset))) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::select(group)

    filtered_group <- .dataset %>%
      dplyr::mutate(fsl_fcs_score_notna = ifelse(!is.na(!!rlang::sym(fsl_fcs_score)),1,0),
                    fsl_hhs_score_notna = ifelse(!is.na(!!rlang::sym(fsl_hhs_score)),1,0)) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(fsl_fcs_score_notna = sum(fsl_fcs_score_notna, na.rm = T),
                       fsl_hhs_score_notna = sum(fsl_hhs_score_notna, na.rm = T)) %>%
      dplyr::filter(fsl_fcs_score_notna >= 3 & fsl_hhs_score_notna >= 3) %>%
      dplyr::pull(group)

    draft_result <- .dataset %>%
      dplyr::filter(group %in% filtered_group) %>% dplyr::group_by(group)%>%
      dplyr::summarise(corr.fcs_hhs = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hhs_score)[4]), 2),
                       corr.fcs_hhs.pvalue = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hhs_score)[3]), 6))

    results2 <- results2 %>%
      dplyr::left_join(draft_result)

    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  } else {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.fcs_hhs = NA,
                       corr.fcs_hhs.pvalue = NA)
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if (all(c("fsl_fcs_score", "fsl_hdds_score") %in% names(.dataset))) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::select(group)

    filtered_group <- .dataset %>%
      dplyr::mutate(fsl_fcs_score_notna = ifelse(!is.na(!!rlang::sym(fsl_fcs_score)),1,0),
                    fsl_hdds_score_notna = ifelse(!is.na(!!rlang::sym(fsl_hdds_score)),1,0)) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(fsl_fcs_score_notna = sum(fsl_fcs_score_notna, na.rm = T),
                       fsl_hdds_score_notna = sum(fsl_hdds_score_notna, na.rm = T)) %>%
      dplyr::filter(fsl_fcs_score_notna >= 3 & fsl_hdds_score_notna >= 3) %>%
      dplyr::pull(group)

    draft_result <- .dataset %>%
      dplyr::filter(group %in% filtered_group) %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.fcs_hdds = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hdds_score)[4]), 2),
                       corr.fcs_hdds.pvalue = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hdds_score)[3]), 3))

    results2 <- results2 %>%
      dplyr::left_join(draft_result)

    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  } else {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.fcs_hdds = NA,
                       corr.fcs_hdds.pvalue = NA)
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if (all(c("fsl_hdds_score", "fsl_rcsi_score") %in% names(.dataset))) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::select(group)

    filtered_group <- .dataset %>%
      dplyr::mutate(fsl_hdds_score_notna = ifelse(!is.na(!!rlang::sym(fsl_hdds_score)),1,0),
                    fsl_rcsi_score_notna = ifelse(!is.na(!!rlang::sym(fsl_rcsi_score)),1,0)) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(fsl_hdds_score_notna = sum(fsl_hdds_score_notna, na.rm = T),
                       fsl_rcsi_score_notna = sum(fsl_rcsi_score_notna, na.rm = T)) %>%
      dplyr::filter(fsl_hdds_score_notna >= 3 & fsl_rcsi_score_notna >= 3) %>%
      dplyr::pull(group)

    draft_result <- .dataset %>%
      dplyr::filter(group %in% filtered_group) %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.hdds_rcsi = round(as.numeric(stats::cor.test(fsl_hdds_score, fsl_rcsi_score)[4]), 2),
                       corr.hdds_rcsi.pvalue = round(as.numeric(stats::cor.test(fsl_hdds_score, fsl_rcsi_score)[3]), 3))

    results2 <- results2 %>%
      dplyr::left_join(draft_result)

    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  } else {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.hdds_rcsi = NA,
                       corr.hdds_rcsi.pvalue = NA)
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if (all(c("fsl_hhs_score", "fsl_rcsi_score") %in% names(.dataset))) {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::select(group)

    filtered_group <- .dataset %>%
      dplyr::mutate(fsl_hhs_score_notna = ifelse(!is.na(!!rlang::sym(fsl_hhs_score)),1,0),
                    fsl_rcsi_score_notna = ifelse(!is.na(!!rlang::sym(fsl_rcsi_score)),1,0)) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(fsl_hhs_score_notna = sum(fsl_hhs_score_notna, na.rm = T),
                       fsl_rcsi_score_notna = sum(fsl_rcsi_score_notna, na.rm = T)) %>%
      dplyr::filter(fsl_hhs_score_notna >= 3 & fsl_rcsi_score_notna >= 3) %>%
      dplyr::pull(group)

    draft_result <- .dataset %>%
      dplyr::filter(group %in% filtered_group) %>% dplyr::group_by(group)  %>%
      dplyr::summarise(corr.hhs_rcsi = round(as.numeric(stats::cor.test(fsl_hhs_score, fsl_rcsi_score)[4]), 2),
                       corr.hhs_rcsi.pvalue = round(as.numeric(stats::cor.test(fsl_hhs_score, fsl_rcsi_score)[3]), 3))

    results2 <- results2 %>%
      dplyr::left_join(draft_result)

    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  } else {
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise(corr.hhs_rcsi = NA,
                       corr.hhs_rcsi.pvalue = NA)
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }

  if (any(c("fsl_fcs_score", "fsl_hhs_score", "fsl_hdds_score",
            "fsl_rcsi_score", "flag_lcsi_coherence") %in% names(.dataset))) {
    nms <- .dataset %>% dplyr::select(dplyr::starts_with("flag")) %>%
      names()
    results2 <- .dataset %>% dplyr::group_by(group) %>%
      dplyr::summarise_at(.vars = nms, ~round(mean(., na.rm = TRUE), 3) * 100)
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }

  results2 <- .dataset %>% dplyr::group_by(group) %>%
    dplyr::summarise(n = dplyr::n())
  if (!exists("results")) {
    results <- results2
  } else {
    results <- merge(results, results2)
  }

  results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  if ("flag_fc_cell" %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::mutate(p1 = ifelse(is.na(fsl_fc_phase), NA,
                                                       ifelse(fsl_fc_phase == "Phase 1 FC", 1, 0)),
                                           p2 = ifelse(is.na(fsl_fc_phase), NA,
                                                       ifelse(fsl_fc_phase == "Phase 2 FC", 1, 0)),
                                           p3 = ifelse(is.na(fsl_fc_phase), NA,
                                                       ifelse(fsl_fc_phase == "Phase 3 FC", 1, 0)),
                                           p4 = ifelse(is.na(fsl_fc_phase), NA,
                                                       ifelse(fsl_fc_phase == "Phase 4 FC", 1, 0)),
                                           p5 = ifelse(is.na(fsl_fc_phase), NA,
                                                       ifelse(fsl_fc_phase == "Phase 5 FC", 1, 0))) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fsl_fc_cell), na.rm = TRUE),
                       fews_p1 = round(sum(p1, na.rm = TRUE)/sum(!is.na(fsl_fc_cell)), 2),
                       fews_p2 = round(sum(p2, na.rm = TRUE)/sum(!is.na(fsl_fc_cell)), 2),
                       fews_p3 = round(sum(p3, na.rm = TRUE)/sum(!is.na(fsl_fc_cell)), 2),
                       fews_p4 = round(sum(p4, na.rm = TRUE)/sum(!is.na(fsl_fc_cell)), 2),
                       fews_p5 = round(sum(p5, na.rm = TRUE)/sum(!is.na(fsl_fc_cell)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>%
      dplyr::select(c(1, n, dplyr::everything()))
  }

  results <- local_calculate_plausibility(.dataset = results)
  a <- c("n", "fews_p1", "fews_p2", "fews_p3", "fews_p4",
         "fews_p5", "flag_severe_hhs", "flag_lcsi_severity",
         "plaus_fcs", "plaus_rcsi", "plaus_hhs", "plaus_lcsi",
         "plaus_other_fsl", "plaus_fsl_score", "plaus_fsl_cat")
  b <- intersect(a, colnames(results))
  if (short_report == TRUE & length(setdiff(b, colnames(results))) ==
      0) {
    results <- results %>% dplyr::select(1, b)
  }
  if (!is.null(file_path)) {
    writexl::write_xlsx(results, file_path)
  }

  options(warn = 0)
  return(results)
}
