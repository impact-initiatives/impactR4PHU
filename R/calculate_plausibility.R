#' Calculate the plausibility for IYCF/MORT/NUT/FSL
#'
#' @param .dataset the output of create_x_plaus
#'
#' @return dataframe with plausibility columns calculated
#' @export
#'
#' @examples
#' \dontrun{calculate_plausibility(df)}


calculate_plausibility <- function(.dataset){
  print("Now Calculating Plausibility Scoring and Classifications.")
  anthro_plaus_vars <- c("flag_perc_mfaz_children","n_children_muac","sd_muac_mm",
                         "age_ratio.pvalue", "sex_ratio.pvalue","dps_muac")

  if (c("age_ratio.pvalue") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_ageratio = ifelse(age_ratio.pvalue > 0.1, 0,
                                                       ifelse(age_ratio.pvalue > 0.05, 2,
                                                              ifelse(age_ratio.pvalue > 0.001, 4,
                                                                     ifelse(age_ratio.pvalue <= 0.001, 10, NA)))))
  }
  if ((c("sex_ratio.pvalue") %in% names(.dataset)) & !(c("cdr") %in%
                                                 names(.dataset))) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_sexratio = ifelse(sex_ratio.pvalue > 0.1, 0,
                                                       ifelse(sex_ratio.pvalue > 0.05, 2,
                                                              ifelse(sex_ratio.pvalue > 0.001, 4,
                                                                     ifelse(sex_ratio.pvalue <= 0.001, 10, NA)))))
  }
  if (c("dps_muac") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_dps_muac = ifelse(dps_muac >= 0 & dps_muac < 8, 0,
                                                       ifelse(dps_muac >= 8 & dps_muac < 13, 2,
                                                              ifelse(dps_muac >= 13 & dps_muac < 20, 4,
                                                                     ifelse(dps_muac >= 20, 10, NA)))))
  }
  if (c("flag_perc_mfaz_children") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_perc_mfaz_children = ifelse(flag_perc_mfaz_children >= 0 & flag_perc_mfaz_children < 8, 0,
                                                                 ifelse(flag_perc_mfaz_children >= 8 & flag_perc_mfaz_children < 13, 2,
                                                                        ifelse(flag_perc_mfaz_children >= 13 & flag_perc_mfaz_children < 20, 4,
                                                                               ifelse(flag_perc_mfaz_children >= 20, 10, NA)))))
  }
  if (c("n_children_muac") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_n_children_muac = ifelse(n_children_muac > 100, 0,
                                                              ifelse(n_children_muac > 80 & n_children_muac <= 100, 2,
                                                                     ifelse(n_children_muac > 50 & n_children_muac <= 80, 4,
                                                                            ifelse(n_children_muac <= 50, 10, NA)))))
  }
  if (c("sd_muac_mm") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_sd_muac_mm = ifelse(sd_muac_mm < 12, 0,
                                                         ifelse(sd_muac_mm < 14,5,
                                                                ifelse(sd_muac_mm < 15, 10,
                                                                       ifelse(sd_muac_mm >= 15, 20, NA)))))
  }
  if (length(setdiff(anthro_plaus_vars, names(.dataset))) == 0) {
    print("Generating anthropometric plausibility score and classification.")
    .dataset <- .dataset %>% dplyr::mutate(plaus_anthro_score = rowSums(dplyr::across(c(plaus_sd_muac_mm, plaus_n_children_muac,
                                                                     plaus_ageratio,plaus_sexratio,plaus_perc_mfaz_children,plaus_dps_muac), .fns = as.numeric), na.rm=T),
                               plaus_anthro_cat = ifelse(plaus_anthro_score >= 0 & plaus_anthro_score <= 9, "Excellent",
                                                         ifelse(plaus_anthro_score > 9 & plaus_anthro_score <= 19, "Good",
                                                                ifelse(plaus_anthro_score > 19 & plaus_anthro_score < 25, "Acceptable",
                                                                       ifelse(plaus_anthro_score >= 25, "Problematic", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for anthropometric plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(anthro_plaus_vars, names(.dataset)))
  }
  mort_plaus_vars <- c("plaus_cdr", "plaus_hh_multiple_death",
                       "plaus_sex_ratio", "plaus_age0to4_5plus_ratio",
                       "plaus_age0to1_2to4_ratio", "plaus_age0to4_5to10_ratio")
  if (c("cdr") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_cdr = ifelse(cdr < 1, 0,
                                                  ifelse(cdr < 2, 5,
                                                         ifelse(cdr < 3.5, 10,
                                                                ifelse(cdr >= 3.5, 20, 0)))))
  }
  if (c("prop_hh_flag_deaths") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_hh_multiple_death = ifelse(prop_hh_flag_deaths < 0.005, 0,
                                                                  ifelse(prop_hh_flag_deaths < 0.01, 2,
                                                                         ifelse(prop_hh_flag_deaths < 0.015, 5,
                                                                                ifelse(prop_hh_flag_deaths >= 1.5, 10, 0)))))
  }
  if (length(setdiff(c("sex_ratio.pvalue", "cdr"), names(.dataset))) == 0) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_sex_ratio = ifelse(sex_ratio.pvalue > 0.05, 0,
                                                               ifelse(sex_ratio.pvalue > 0.001, 2,
                                                                      ifelse(sex_ratio.pvalue > 1e-04, 5,
                                                                             ifelse(sex_ratio.pvalue <= 1e-04, 10, 0)))))
  }
  if (c("age_ratio_0_5.pvalue") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_age0to4_5plus_ratio = ifelse(age_ratio_0_5.pvalue > 0.1, 0,
                                                                   ifelse(age_ratio_0_5.pvalue > 0.05, 2,
                                                                          ifelse(age_ratio_0_5.pvalue > 0.001, 5,
                                                                                 ifelse(age_ratio_0_5.pvalue <= 0.001, 10, 0)))))
  }
  if (c("age_ratio_2_5.pvalue") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_age0to1_2to4_ratio = ifelse(age_ratio_2_5.pvalue > 0.1, 0,
                                                                   ifelse(age_ratio_2_5.pvalue > 0.05, 2,
                                                                          ifelse(age_ratio_2_5.pvalue > 0.001, 5,
                                                                                 ifelse(age_ratio_2_5.pvalue <= 0.001, 10, 0)))))
  }
  if (c("age_ratio_5_10.pvalue") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_age0to4_5to10_ratio = ifelse(age_ratio_5_10.pvalue > 0.1, 0,
                                                                    ifelse(age_ratio_5_10.pvalue > 0.05, 2,
                                                                           ifelse(age_ratio_5_10.pvalue > 0.001, 5,
                                                                                  ifelse(age_ratio_5_10.pvalue <= 0.001, 10, 0)))))
  }
  if (c("mean_hh_size.pvalue") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_mean_hh_size = ifelse(mean_hh_size.pvalue > 0.05, 0,
                                                                  ifelse(mean_hh_size.pvalue > 0.001, 2,
                                                                         ifelse(mean_hh_size.pvalue > 1e-04, 5,
                                                                                ifelse(mean_hh_size.pvalue <= 1e-04, 10, 0)))))
  }
  if (c("prop_join_people") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_prop_joiners = ifelse(prop_join_people < 10, 0,
                                                           ifelse(prop_join_people < 20, 2,
                                                                  ifelse(prop_join_people < 30, 5,
                                                                         ifelse(prop_join_people >= 30, 10, 0)))))
  }
  if (c("prop_left_people") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_prop_leavers = ifelse(prop_left_people < 10, 0,
                                                           ifelse(prop_left_people < 20, 2,
                                                                  ifelse(prop_left_people < 30, 5,
                                                                         ifelse(prop_left_people >= 30, 10, 0)))))
  }

  if (length(setdiff(mort_plaus_vars, names(.dataset))) == 0) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_mort_score = plaus_cdr + plaus_hh_multiple_death + plaus_sex_ratio +
                                 plaus_age0to4_5plus_ratio + plaus_age0to1_2to4_ratio + plaus_age0to4_5to10_ratio,
                               plaus_mort_cat = ifelse(plaus_mort_score >= 0 & plaus_mort_score < 10, "Excellent (0-<10)",
                                                       ifelse(plaus_mort_score >= 10 & plaus_mort_score < 20, "Good (10-<20)",
                                                              ifelse(plaus_mort_score >= 20 & plaus_mort_score < 25, "Acceptable (20 - <25)",
                                                                     ifelse(plaus_mort_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for mortality plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(mort_plaus_vars, names(.dataset)))
  }
  if (c("sd_fcs") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_sd_fcs = dplyr::case_when(sd_fcs < 8 ~ 4,
                                                               sd_fcs >= 8 & sd_fcs < 9 ~ 2,
                                                               sd_fcs >= 9 & sd_fcs < 14 ~ 0,
                                                               sd_fcs >= 14 & sd_fcs < 16 ~ 2,
                                                               sd_fcs >= 16 ~ 4,
                                                               TRUE ~ 0))
  }
  if (c("flag_low_fcs") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_low_fcs = dplyr::case_when(flag_low_fcs < 2 ~ 0,
                                                                     flag_low_fcs >= 2 & flag_low_fcs < 10 ~ 2,
                                                                     flag_low_fcs >= 10 ~ 4,
                                                                     TRUE ~ 0))
  }
  if (c("flag_high_fcs") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_high_fcs = dplyr::case_when(flag_high_fcs < 2 ~ 0,
                                                                      flag_high_fcs >= 2 & flag_high_fcs < 10 ~ 1,
                                                                      flag_high_fcs >= 10 ~ 2,
                                                                      TRUE ~ 0))
  }
  if (c("flag_sd_foodgroup") %in% names(.dataset)) { # maybe only 3 cat
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_sd_foodgroup = dplyr::case_when(flag_sd_foodgroup < 2 ~ 0,
                                                                          flag_sd_foodgroup >= 2 & flag_sd_foodgroup < 10 ~ 4,
                                                                          flag_sd_foodgroup >= 10 ~ 6,
                                                                          TRUE ~ 0))
  }
  if (c("flag_meat_cereal_ratio") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_meat_cereal_ratio = dplyr::case_when(flag_meat_cereal_ratio < 2 ~ 0,
                                                                               flag_meat_cereal_ratio >= 2 & flag_meat_cereal_ratio < 10 ~ 2,
                                                                               flag_meat_cereal_ratio >= 10 ~ 4,
                                                                               TRUE ~ 0))
  }
  if (c("flag_low_sugar_cond_hdds") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_low_sugar_cond_hdds = dplyr::case_when(flag_low_sugar_cond_hdds < 2 ~ 0,
                                                                                 flag_low_sugar_cond_hdds >= 2 & flag_low_sugar_cond_hdds < 10 ~ 2,
                                                                                 flag_low_sugar_cond_hdds >= 10 ~ 4,
                                                                                 TRUE ~ 0))
  }
  fcs_plaus_vars <- c("plaus_sd_fcs", "plaus_flag_low_fcs",
                      "plaus_flag_high_fcs", "plaus_flag_sd_foodgroup",
                      "plaus_flag_meat_cereal_ratio",
                      "plaus_flag_low_sugar_cond_hdds")
  if (length(setdiff(c(fcs_plaus_vars), names(.dataset))) < 6) {
    plaus_nms <- intersect(fcs_plaus_vars, names(.dataset))
    .dataset <- .dataset %>% dplyr::rowwise() %>% dplyr::mutate(plaus_fcs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_high_rcsi") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_high_rcsi = dplyr::case_when(flag_high_rcsi < 2 ~ 0,
                                                                       flag_high_rcsi >= 2 & flag_high_rcsi < 10 ~ 4,
                                                                       flag_high_rcsi >= 10 ~ 5,
                                                                       TRUE ~ 0))
  }
  if (c("flag_sd_rcsicoping") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_sd_rcsicoping = dplyr::case_when(flag_sd_rcsicoping < 2 ~ 0,
                                                                           flag_sd_rcsicoping >= 2 & flag_sd_rcsicoping < 10 ~ 4,
                                                                           flag_sd_rcsicoping >= 10 ~ 6,
                                                                           TRUE ~ 0))
  }
  if (c("sd_rcsi") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_sd_rcsi = dplyr::case_when(sd_rcsi < 8 ~ 3,
                                                                sd_rcsi >= 8 & sd_rcsi < 9 ~ 2,
                                                                sd_rcsi >= 9 & sd_rcsi < 14 ~ 0,
                                                                sd_rcsi >= 14 & sd_rcsi < 16 ~ 2,
                                                                sd_rcsi >= 16 ~ 3,
                                                                TRUE ~ 0))
  }
  if (c("flag_protein_rcsi") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_protein_rcsi = dplyr::case_when(flag_protein_rcsi < 2 ~ 0,
                                                                          flag_protein_rcsi >= 2 & flag_protein_rcsi < 10 ~ 2,
                                                                          flag_protein_rcsi >= 10 ~ 3,
                                                                          TRUE ~ 0))
  }
  if (c("flag_rcsi_children") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_rcsi_children = dplyr::case_when(flag_rcsi_children < 2 ~ 0,
                                                                           flag_protein_rcsi >= 2 & flag_protein_rcsi < 10 ~ 2,
                                                                           flag_protein_rcsi >= 10 ~ 3,
                                                                           TRUE ~ 0))
  }
  rcsi_plaus_vars <- c("plaus_flag_protein_rcsi", "plaus_flag_sd_rcsicoping",
                       "plaus_flag_high_rcsi", "plaus_sd_rcsi","plaus_flag_rcsi_children")
  if (length(setdiff(c(rcsi_plaus_vars), names(.dataset))) < 5) {
    plaus_nms <- intersect(rcsi_plaus_vars, names(.dataset))
    .dataset <- .dataset %>% dplyr::rowwise() %>% dplyr::mutate(plaus_rcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_severe_hhs") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_severe_hhs = dplyr::case_when(flag_severe_hhs < 1 ~ 0,
                                                                        flag_severe_hhs >= 1 & flag_severe_hhs < 5 ~ 6,
                                                                        flag_severe_hhs >= 5 & flag_severe_hhs < 10 ~ 8,
                                                                        flag_severe_hhs >= 10 ~ 10,
                                                                        TRUE ~ 0))
  }

  hhs_plaus_vars <- c("plaus_flag_severe_hhs")
  if (length(setdiff(c(hhs_plaus_vars), names(.dataset))) < 2) {
    plaus_nms <- intersect(hhs_plaus_vars, names(.dataset))
    .dataset <- .dataset %>% dplyr::rowwise() %>% dplyr::mutate(plaus_hhs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_lcsi_liv_livestock") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_liv_livestock = dplyr::case_when(flag_lcsi_liv_livestock < 2 ~ 0,
                                                                                flag_lcsi_liv_livestock >= 2 & flag_lcsi_liv_livestock < 10 ~ 2,
                                                                                flag_lcsi_liv_livestock >= 10 ~ 3,
                                                                                TRUE ~ 0))
  }
  if (c("flag_lcsi_liv_agriculture") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_liv_agriculture = dplyr::case_when(flag_lcsi_liv_agriculture < 2 ~ 0,
                                                                                  flag_lcsi_liv_agriculture >= 2 & flag_lcsi_liv_agriculture < 10 ~ 2,
                                                                                  flag_lcsi_liv_agriculture >= 10 ~ 3,
                                                                                  TRUE ~ 0))
  }
  if (c("flag_lcsi_liv_agriculture") %in% names(.dataset) | c("flag_lcsi_liv_livestock") %in% names(.dataset)) {
    if (length(setdiff(c("flag_lcsi_liv_agriculture", "flag_lcsi_liv_livestock"), names(.dataset))) == 0) {
      .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = dplyr::case_when(plaus_flag_lcsi_liv_livestock >= plaus_flag_lcsi_liv_agriculture ~ plaus_flag_lcsi_liv_livestock,
                                                                                  plaus_flag_lcsi_liv_livestock < plaus_flag_lcsi_liv_agriculture ~ plaus_flag_lcsi_liv_agriculture,
                                                                                  TRUE ~ 0))
    }
    else if (length(setdiff(c("flag_lcsi_liv_agriculture"), names(.dataset))) == 0) {
      .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = plaus_flag_lcsi_liv_agriculture)
    }
    else if (length(setdiff(c("flag_lcsi_liv_livestock"), names(.dataset))) == 0) {
      .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = plaus_flag_lcsi_liv_livestock)
    }
  }
  if (c("flag_lcsi_coherence") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_coherence = dplyr::case_when(flag_lcsi_coherence < 2 ~ 0,
                                                                            flag_lcsi_coherence >= 2 & flag_lcsi_coherence < 10 ~ 5,
                                                                            flag_lcsi_coherence >= 10 ~ 7,
                                                                            TRUE ~ 0))
  }
  if (c("flag_lcsi_na") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_na = dplyr::case_when(flag_lcsi_na < 2 ~ 0,
                                                                     flag_lcsi_na >= 2 & flag_lcsi_na < 10 ~ 3,
                                                                     flag_lcsi_na >= 10 ~ 5,
                                                                     TRUE ~ 0))
  }
  if (c("flag_lcsi_severity") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_lcsi_severity = dplyr::case_when(flag_lcsi_severity < 2 ~ 0,
                                                                           flag_lcsi_severity >= 2 & flag_lcsi_severity < 10 ~ 3,
                                                                           flag_lcsi_severity >= 10 ~ 5,
                                                                           TRUE ~ 0))
  }
  lcs_plaus_vars <- c("plaus_flag_lcsi_agr_livestock", "plaus_flag_lcsi_coherence",
                      "plaus_flag_lcsi_na", "plaus_flag_lcsi_severity")
  if (length(setdiff(lcs_plaus_vars, names(.dataset))) < 4) {
    plaus_nms <- intersect(lcs_plaus_vars, names(.dataset))
    .dataset <- .dataset %>% dplyr::rowwise() %>% dplyr::mutate(plaus_lcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (length(setdiff(c("corr.fcs_rcsi", "corr.fcs_rcsi.pvalue"), names(.dataset))) == 0) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_corr.fcs_rcsi = ifelse(corr.fcs_rcsi < -0.2 & corr.fcs_rcsi.pvalue < 0.05, 0,
                                                            ifelse(corr.fcs_rcsi < -0.2 & corr.fcs_rcsi.pvalue >= 0.05, 1,
                                                                   ifelse(corr.fcs_rcsi >= -0.2 & corr.fcs_rcsi < 0.2 & corr.fcs_rcsi.pvalue >= 0.05, 2,
                                                                          ifelse(corr.fcs_rcsi >= -0.2 & corr.fcs_rcsi < 0.2 & corr.fcs_rcsi.pvalue < 0.05, 3,
                                                                                 ifelse(corr.fcs_rcsi >= 0.2 & corr.fcs_rcsi.pvalue >= 0.05, 4,
                                                                                        ifelse(corr.fcs_rcsi >= 0.2 & corr.fcs_rcsi.pvalue < 0.05, 5, 0)))))))
  }
  if (length(setdiff(c("corr.fcs_hhs", "corr.fcs_hhs.pvalue"), names(.dataset))) == 0) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_corr.fcs_hhs = ifelse(corr.fcs_hhs < -0.2 & corr.fcs_hhs.pvalue < 0.05, 0,
                                                           ifelse(corr.fcs_hhs < -0.2 & corr.fcs_hhs.pvalue >= 0.05, 1,
                                                                  ifelse(corr.fcs_hhs >= -0.2 & corr.fcs_hhs < 0.2 & corr.fcs_hhs.pvalue >= 0.05, 1.5,
                                                                         ifelse(corr.fcs_hhs >= -0.2 & corr.fcs_hhs < 0.2 & corr.fcs_hhs.pvalue < 0.05, 2,
                                                                                ifelse(corr.fcs_hhs >= 0.2 & corr.fcs_hhs.pvalue >= 0.05, 2.5,
                                                                                       ifelse(corr.fcs_hhs >= 0.2 & corr.fcs_hhs.pvalue < 0.05, 3, 0)))))))
  }
  if (length(setdiff(c("corr.hhs_rcsi", "corr.hhs_rcsi.pvalue"), names(.dataset))) == 0) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_corr.hhs_rcsi = ifelse(corr.hhs_rcsi > 0.2 & corr.hhs_rcsi.pvalue < 0.05, 0,
                                                            ifelse(corr.hhs_rcsi > 0.2 & corr.hhs_rcsi.pvalue >= 0.05, 1,
                                                                   ifelse(corr.hhs_rcsi > -0.2 & corr.hhs_rcsi <= 0.2 & corr.hhs_rcsi.pvalue < 0.05, 1.5,
                                                                          ifelse(corr.hhs_rcsi > -0.2 & corr.hhs_rcsi <= 0.2 & corr.hhs_rcsi.pvalue >= 0.05, 2,
                                                                                 ifelse(corr.hhs_rcsi <= -0.2 & corr.hhs_rcsi.pvalue >= 0.05, 2.5,
                                                                                        ifelse(corr.hhs_rcsi <= -0.2 & corr.hhs_rcsi.pvalue < 0.05, 3, 0)))))))
  }
  if (c("prop_fc_flags") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_prop_fc_flags = ifelse(prop_fc_flags < 0.02, 0,
                                                            ifelse(prop_fc_flags < 0.1, 2,
                                                                   ifelse(prop_fc_flags >= 0.1, 4, 0))))
  }
  if (c("flag_fcsrcsi_box") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_fcsrcsi_box = dplyr::case_when(flag_fcsrcsi_box < 2 ~ 0,
                                                                         flag_fcsrcsi_box >= 2 & flag_fcsrcsi_box < 10 ~ 1,
                                                                         flag_fcsrcsi_box >= 10 ~ 3,
                                                                         TRUE ~ 0))
  }
  if (c("flag_fcs_rcsi") %in% names(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_flag_fcs_rcsi = dplyr::case_when(flag_fcs_rcsi < 2 ~ 0,
                                                                      flag_fcs_rcsi >= 2 & flag_fcs_rcsi < 10 ~ 1,
                                                                      flag_fcs_rcsi >= 10 ~ 3,
                                                                      TRUE ~ 0))
  }
  other_fsl_plaus_vars <- c("plaus_prop_fc_flags", "plaus_corr.hhs_rcsi",
                            "plaus_corr.fcs_hhs", "plaus_corr.fcs_rcsi",
                            "plaus_flag_fcsrcsi_box", "plaus_flag_fcs_rcsi")
  if (length(setdiff(other_fsl_plaus_vars, names(.dataset))) < 6) {
    plaus_nms <- intersect(other_fsl_plaus_vars, names(.dataset))
    .dataset <- .dataset %>% dplyr::rowwise() %>% dplyr::mutate(plaus_other_fsl = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()
  }
  all_fsl_plaus_vars <- c("plaus_lcsi", "plaus_fcs", "plaus_rcsi",
                          "plaus_hhs", "plaus_other_fsl")
  if (length(setdiff(all_fsl_plaus_vars, names(.dataset))) < 5) {
    plaus_nms <- intersect(all_fsl_plaus_vars, names(.dataset))
    .dataset <- .dataset %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_fsl_score = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(plaus_fsl_cat = dplyr::case_when(plaus_fsl_score < 20 ~ "Good",
                                                     plaus_fsl_score >= 20 & plaus_fsl_score < 30 ~ "Moderate",
                                                     plaus_fsl_score >= 30 ~ "Problematic"))
  }
  if (c("mad_ratio.pvalue") %in% colnames(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_mad_ratio.pvalue = ifelse(mad_ratio.pvalue > 0.05, 0,
                                                               ifelse(mad_ratio.pvalue > 0.001, 5,
                                                                      ifelse(mad_ratio.pvalue > 1e-04, 10,
                                                                             ifelse(mad_ratio.pvalue <= 1e-04, 20, 0)))))
  }
  if (c("prop_flag_high_mdd_low_mmf") %in% colnames(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_prop_flag_high_mdd_low_mmf = ifelse(prop_flag_high_mdd_low_mmf < 0.01, 0,
                                                                         ifelse(prop_flag_high_mdd_low_mmf < 0.05, 5,
                                                                                ifelse(prop_flag_high_mdd_low_mmf < 0.1, 10,
                                                                                       ifelse(prop_flag_high_mdd_low_mmf >= 0.1, 20, 0)))))
  }
  if (c("age_ratio_under6m_6to23m.pvalue") %in% colnames(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_age_ratio_under6m_6to23m.pvalue = ifelse(age_ratio_under6m_6to23m.pvalue > 0.05, 0,
                                                                              ifelse(age_ratio_under6m_6to23m.pvalue > 0.01, 2,
                                                                                     ifelse(age_ratio_under6m_6to23m.pvalue > 0.001, 5,
                                                                                            ifelse(age_ratio_under6m_6to23m.pvalue <= 0.001, 10, 0)))))
  }
  if (c("sd_mdd") %in% colnames(.dataset)) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_sdd_mdd = ifelse(sd_mdd > 1 & sd_mdd < 2, 0,
                                                      ifelse(sd_mdd > 0.8 & sd_mdd < 2.2, 5,
                                                             ifelse(sd_mdd <= 0.8 | sd_mdd >= 2.2, 10, 0))))
  }
  if (c("prop_iycf_caregiver" %in% colnames(.dataset))) {
    .dataset <- .dataset %>% dplyr::mutate(plaus_prop_iycf_caregiver = ifelse(prop_iycf_caregiver >= 0.9, 0,
                                                                  ifelse(prop_iycf_caregiver >= 0.8, 2,
                                                                         ifelse(prop_iycf_caregiver >= 0.7, 5,
                                                                                ifelse(prop_iycf_caregiver >= 0.5, 10, 10)))))
  }
  iycf_plaus_vars <- c("plaus_sdd_mdd", "plaus_age_ratio_under6m_6to23m.pvalue",
                       "plaus_sexratio", "plaus_prop_flag_high_mdd_low_mmf",
                       "plaus_mad_ratio.pvalue")
  if (length(setdiff(iycf_plaus_vars, colnames(.dataset))) == 0) {
    if (!(c("plaus_prop_iycf_caregiver") %in% colnames(.dataset))) {
      .dataset <- .dataset %>% dplyr::mutate(plaus_prop_iycf_caregiver = 10)
      print("No iycf_caregiver variable was available. Plaus penalty of 10 applied assuming this wasn't done during the survey.")
    }
    .dataset <- .dataset %>% dplyr::mutate(plaus_iycf_score = plaus_prop_iycf_caregiver + plaus_sdd_mdd +
                                 plaus_age_ratio_under6m_6to23m.pvalue + plaus_sexratio +
                                 plaus_prop_flag_high_mdd_low_mmf + plaus_mad_ratio.pvalue,
                               plaus_iycf_cat = ifelse(plaus_iycf_score >= 0 & plaus_iycf_score < 10, "Excellent (0-<10)",
                                                       ifelse(plaus_iycf_score >= 10 & plaus_iycf_score < 15, "Good (10-<15)",
                                                              ifelse(plaus_iycf_score >= 15 & plaus_iycf_score < 25, "Acceptable (15 - <25)",
                                                                     ifelse(plaus_iycf_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for IYCF plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(iycf_plaus_vars, names(.dataset)))
    print(setdiff(c("prop_iycf_caregiver"), names(.dataset)))
  }
  return(.dataset)
}
