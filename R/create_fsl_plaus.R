#' create_fsl_plaus
#'
#' @param .dataset raw/clean data with all calculated
#' fcs/rcsi/hhs/hdds/fcm/fclcm add_x indicators
#' @param grouping the name of the variable that indicates the grouping variable - usually "enumerator"
#' @param uuid uuid variable
#'
#' @return a dataframe with all fsl related plausibility columns
#' @export
#'
#' @examples
#' \dontrun{create_fsl_plaus(df)}

create_fsl_plaus <- function(.dataset,
                             grouping = NULL,
                             uuid = "uuid"){
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

  if (c("fsl_fcs_score") %in% colnames(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_fcs = round(mean(fsl_fcs_score, na.rm = TRUE), 2),
                       sd_fcs = round(stats::sd(fsl_fcs_score, na.rm = TRUE), 2),
                       mean_days_cereals = round(mean(fsl_fcs_cereal, na.rm = TRUE), 2),
                       sd_days_cereals = round(stats::sd(fsl_fcs_cereal, na.rm = TRUE), 2),
                       mean_days_legumes = round(mean(fsl_fcs_legumes, na.rm = TRUE), 2),
                       sd_days_legumes = round(stats::sd(fsl_fcs_legumes, na.rm = TRUE), 2),
                       mean_days_dairy = round(mean(fsl_fcs_dairy, na.rm = TRUE), 2),
                       sd_days_dairy = round(stats::sd(fsl_fcs_dairy, na.rm = TRUE), 2),
                       mean_days_meat = round(mean(fsl_fcs_meat, na.rm = TRUE), 2),
                       sd_days_meat = round(stats::sd(fsl_fcs_meat, na.rm = TRUE), 2),
                       mean_days_veg = round(mean(fsl_fcs_veg, na.rm = TRUE), 2),
                       sd_days_veg = round(stats::sd(fsl_fcs_veg, na.rm = TRUE), 2),
                       mean_days_fruit = round(mean(fsl_fcs_fruit, na.rm = TRUE), 2),
                       sd_days_fruit = round(stats::sd(fsl_fcs_fruit, na.rm = TRUE), 2),
                       mean_days_oils = round(mean(fsl_fcs_oil, na.rm = TRUE), 2),
                       sd_days_oils = round(stats::sd(fsl_fcs_oil, na.rm = TRUE), 2),
                       mean_days_sugar = round(mean(fsl_fcs_sugar, na.rm = TRUE), 2),
                       sd_days_sugar = round(stats::sd(fsl_fcs_sugar, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("fsl_rcsi_score") %in% colnames(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_rcsi = round(mean(fsl_rcsi_score, na.rm = TRUE), 2),
                       sd_rcsi = round(stats::sd(fsl_rcsi_score, na.rm = TRUE), 2),
                       mean_rcsi_lessquality = round(mean(fsl_rcsi_lessquality, na.rm = TRUE), 2),
                       sd_rcsi_lessquality = round(stats::sd(fsl_rcsi_lessquality, na.rm = TRUE), 2),
                       mean_rcsi_borrow = round(mean(fsl_rcsi_borrow, na.rm = TRUE), 2),
                       sd_rcsi_borrow = round(stats::sd(fsl_rcsi_borrow, na.rm = TRUE), 2),
                       mean_rcsi_mealsize = round(mean(fsl_rcsi_mealsize, na.rm = TRUE), 2),
                       sd_rcsi_mealsize = round(stats::sd(fsl_rcsi_mealsize, na.rm = TRUE), 2),
                       mean_rcsi_mealadult = round(mean(fsl_rcsi_mealadult, na.rm = TRUE), 2),
                       sd_rcsi_mealadult = round(stats::sd(fsl_rcsi_mealadult, na.rm = TRUE), 2),
                       mean_rcsi_mealnb = round(mean(fsl_rcsi_mealnb, na.rm = TRUE), 2),
                       sd_rcsi_mealnb = round(stats::sd(fsl_rcsi_mealnb, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("fsl_hhs_score") %in% colnames(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_hhs = round(mean(fsl_hhs_score, na.rm = TRUE), 2),
                       sd_hhs = round(stats::sd(fsl_hhs_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("fsl_hdds_score") %in% colnames(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(mean_hdds = round(mean(fsl_hdds_score, na.rm = TRUE), 2),
                       sd_hdds = round(stats::sd(fsl_hdds_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }
  if (length(setdiff(c("fsl_fcs_score", "fsl_rcsi_score"), colnames(.dataset))) == 0) {
    tryCatch(
      {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.fcs_rcsi = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_rcsi_score)[4]), 2),
                           corr.fcs_rcsi.pvalue = as.numeric(stats::cor.test(fsl_fcs_score, fsl_rcsi_score)[3]))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.fcs_rcsi = NA,
                           corr.fcs_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_fcs_score", "fsl_hhs_score"), colnames(.dataset))) == 0) {
    tryCatch(
      {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.fcs_hhs = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hhs_score)[4]), 2),
                           corr.fcs_hhs.pvalue = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hhs_score)[3]), 6))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.fcs_hhs = NA,
                           corr.fcs_hhs.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_fcs_score", "fsl_hdds_score"), colnames(.dataset))) == 0) {
    tryCatch(
      {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.fcs_hdds = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hdds_score)[4]), 2),
                           corr.fcs_hdds.pvalue = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hdds_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.fcs_hdds = NA,
                           corr.fcs_hdds.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_hdds_score", "fsl_rcsi_score"), colnames(.dataset))) == 0) {
    tryCatch(
      {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.hdds_rcsi = round(as.numeric(stats::cor.test(fsl_hdds_score, fsl_rcsi_score)[4]), 2),
                           corr.hdds_rcsi.pvalue = round(as.numeric(stats::cor.test(fsl_hdds_score, fsl_rcsi_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        } else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.hdds_rcsi = NA,
                           corr.hdds_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_hhs_score", "fsl_rcsi_score"), colnames(.dataset))) == 0) {
    tryCatch(
      {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.hhs_rcsi = round(as.numeric(stats::cor.test(fsl_hhs_score, fsl_rcsi_score)[4]), 2),
                           corr.hhs_rcsi.pvalue = round(as.numeric(stats::cor.test(fsl_hhs_score, fsl_rcsi_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        } else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
          dplyr::summarise(corr.hhs_rcsi = NA,
                           corr.hhs_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }

  if (length(setdiff(c("fsl_fcs_score", "fsl_hhs_score", "fsl_hdds_score",
                       "fsl_rcsi_score", "flag_lcsi_coherence"), names(.dataset))) < 5) {
    nms <- .dataset %>% dplyr::select(dplyr::starts_with("flag")) %>%
      names()
    results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise_at(.vars = nms, ~round(mean(., na.rm = TRUE), 3) * 100)
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }
  results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::summarise(n = dplyr::n())
  if (!exists("results")) {
    results <- results2
  }else {
    results <- merge(results, results2)
  }

  results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  if (c("flag_fc_cell") %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::mutate(p1 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 1 FC", 1, 0)),
                                     p2 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 2 FC", 1, 0)),
                                     p3 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 3 FC", 1, 0)),
                                     p4 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 4 FC", 1, 0)),
                                     p5 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 5 FC", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fc_cell), na.rm = TRUE),
                       fews_p1 = round(sum(p1, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p2 = round(sum(p2, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p3 = round(sum(p3, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p4 = round(sum(p4, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p5 = round(sum(p5, na.rm = TRUE)/sum(!is.na(fc_cell)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>%
      dplyr::select(c(1, n, dplyr::everything()))
  }
  if (c("food_exp_share") %in% names(.dataset)) {
    results2 <- .dataset %>% dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fc_cell), na.rm = TRUE),
                       fes_1 = round(sum(food_exp_share == "1", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_2 = round(sum(food_exp_share == "2", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_3 = round(sum(food_exp_share == "3", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_4 = round(sum(food_exp_share == "4", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  }
  results <- calculate_plausibility(.dataset = results)
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
