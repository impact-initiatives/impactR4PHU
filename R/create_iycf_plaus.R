# create_iycf_plaus <- function(){
#   # IYCF Plausibility Criteria
#
#   if(c("mad_ratio.pvalue") %in% colnames(df)) {
#
#     df <- df %>%
#       dplyr::mutate(plaus_mad_ratio.pvalue = ifelse(.data$mad_ratio.pvalue > 0.05, 0,
#                                                     ifelse(.data$mad_ratio.pvalue > 0.001, 5,
#                                                            ifelse(.data$mad_ratio.pvalue > 0.0001, 10,
#                                                                   ifelse(.data$mad_ratio.pvalue <= 0.0001, 20, 0)))))
#
#   }
#
#   if(c("prop_flag_high_mdd_low_mmf") %in% colnames(df)) {
#
#     df <- df %>%
#       dplyr::mutate(plaus_prop_flag_high_mdd_low_mmf = ifelse(.data$prop_flag_high_mdd_low_mmf < 0.01, 0,
#                                                               ifelse(.data$prop_flag_high_mdd_low_mmf <0.05, 5,
#                                                                      ifelse(.data$prop_flag_high_mdd_low_mmf <0.1, 10,
#                                                                             ifelse(.data$prop_flag_high_mdd_low_mmf >= 0.1, 20, 0)))))
#
#   }
#
#   if(c("age_ratio_under6m_6to23m.pvalue") %in% colnames(df)) {
#
#     df <- df %>%
#       dplyr::mutate(plaus_age_ratio_under6m_6to23m.pvalue = ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.05, 0,
#                                                                    ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.01, 2,
#                                                                           ifelse(.data$age_ratio_under6m_6to23m.pvalue > 0.001, 5,
#                                                                                  ifelse(.data$age_ratio_under6m_6to23m.pvalue <= 0.001, 10, 0)))))
#
#
#   }
#
#   if(c("sd_mdd") %in% colnames(df)) {
#
#     df <- df %>%
#       dplyr::mutate(plaus_sdd_mdd = ifelse(.data$sd_mdd > 1 & .data$sd_mdd < 2, 0,
#                                            ifelse(.data$sd_mdd > 0.8 & .data$sd_mdd < 2.2, 5,
#                                                   ifelse(.data$sd_mdd <= 0.8 | .data$sd_mdd >= 2.2, 10, 0))))
#   }
#
#   if(c("prop_iycf_caregiver" %in% colnames(df))) {
#
#     df <- df %>%
#       dplyr::mutate(plaus_prop_iycf_caregiver = ifelse(.data$prop_iycf_caregiver >= 0.9, 0,
#                                                        ifelse(.data$prop_iycf_caregiver >= .8, 2,
#                                                               ifelse(.data$prop_iycf_caregiver >= 0.7, 5,
#                                                                      ifelse(.data$prop_iycf_caregiver >=0.5, 10, 10)))))
#
#   }
#
#   # IYCF Plausibility Score and Classification
#
#   iycf_plaus_vars <- c("plaus_sdd_mdd", "plaus_age_ratio_under6m_6to23m.pvalue", "plaus_sexratio",
#                        "plaus_prop_flag_high_mdd_low_mmf", "plaus_mad_ratio.pvalue")
#
#   if(length(setdiff(iycf_plaus_vars, colnames(df)))==0) {
#
#     if(!(c("plaus_prop_iycf_caregiver") %in% colnames(df))) {
#       df <- df %>% dplyr::mutate(plaus_prop_iycf_caregiver = 10)
#       print("No iycf_caregiver variable was available. Plaus penalty of 10 applied assuming this wasn't done during the survey.")
#     }
#
#     df <- df %>%
#       dplyr::mutate(iycf_plaus_score = .data$plaus_prop_iycf_caregiver + .data$plaus_sdd_mdd + .data$plaus_age_ratio_under6m_6to23m.pvalue + .data$plaus_sexratio + .data$plaus_prop_flag_high_mdd_low_mmf + .data$plaus_mad_ratio.pvalue,
#                     iycf_plaus_cat = ifelse(.data$iycf_plaus_score >=0 & .data$iycf_plaus_score < 10, "Excellent (0-<10)",
#                                             ifelse(.data$iycf_plaus_score >= 10 & .data$iycf_plaus_score < 15, "Good (10-<15)",
#                                                    ifelse(.data$iycf_plaus_score >= 15 & .data$iycf_plaus_score < 25, "Acceptable (15 - <25)",
#                                                           ifelse(.data$iycf_plaus_score >= 25, "Problematic (>=25)", NA)))))
#
#
#   } else {
#     print(paste0("Not all necessary variables for IYCF plausibility score and classification. Skipping this step. The dataframe is missing "))
#     print(setdiff(iycf_plaus_vars, names(df)))
#     print(setdiff(c("prop_iycf_caregiver"), names(df)))
#   }
# }
