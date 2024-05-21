### create ipc_table

ipc_loa <- data.frame(analysis_var = c(),
                      analysis_type = c(),
                      group_var = c())

if ("FCS" %in% FSL_indicators){
  fcs_cat_bit <-  data.frame(analysis_var = c("fsl_fcs_cat",
                                              fsl_fcs_cereal,
                                              fsl_fcs_legumes,
                                              fsl_fcs_dairy,
                                              fsl_fcs_meat,
                                              fsl_fcs_veg,
                                              fsl_fcs_fruit,
                                              fsl_fcs_oil,
                                              fsl_fcs_sugar,
                                              fsl_fcs_condiments,
                                              fsl_fcs_cereal,
                                              fsl_fcs_legumes,
                                              fsl_fcs_dairy,
                                              fsl_fcs_meat,
                                              fsl_fcs_veg,
                                              fsl_fcs_fruit,
                                              fsl_fcs_oil,
                                              fsl_fcs_sugar,
                                              fsl_fcs_condiments),
                             analysis_type = c("prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean"),
                             group_var = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA,NA,NA,NA,NA))
  ipc_loa <- bind_rows(ipc_loa, fcs_cat_bit)
}

if ("rCSI" %in% FSL_indicators){
  rcsi_cat_bit <-  data.frame(analysis_var = c("fsl_rcsi_cat",
                                               fsl_rcsi_lessquality,
                                               fsl_rcsi_borrow,
                                               fsl_rcsi_mealsize,
                                               fsl_rcsi_mealadult,
                                               fsl_rcsi_mealnb,
                                               fsl_rcsi_lessquality,
                                               fsl_rcsi_borrow,
                                               fsl_rcsi_mealsize,
                                               fsl_rcsi_mealadult,
                                               fsl_rcsi_mealnb),
                             analysis_type = c("prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean",
                                               "mean"),
                             group_var = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  ipc_loa <- bind_rows(ipc_loa, rcsi_cat_bit)
}

if ("LCSI" %in% FSL_indicators){
  lcsi_cat_bit <-  data.frame(analysis_var = c("fsl_lcsi_cat",
                                               fsl_lcsi_stress1,
                                               fsl_lcsi_stress2,
                                               fsl_lcsi_stress3,
                                               fsl_lcsi_stress4,
                                               fsl_lcsi_crisis1,
                                               fsl_lcsi_crisis2,
                                               fsl_lcsi_crisis3,
                                               fsl_lcsi_emergency1,
                                               fsl_lcsi_emergency2,
                                               fsl_lcsi_emergency3),
                             analysis_type = c("prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one"),
                             group_var = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  ipc_loa <- bind_rows(ipc_loa, lcsi_cat_bit)
}

if ("HHS" %in% FSL_indicators){
  hhs_cat_bit <-  data.frame(analysis_var = c("fsl_hhs_cat_ipc",
                                              fsl_hhs_nofoodhh,
                                              fsl_hhs_nofoodhh_freq,
                                              fsl_hhs_sleephungry,
                                              fsl_hhs_sleephungry_freq,
                                              fsl_hhs_alldaynight,
                                              fsl_hhs_alldaynight_freq),
                             analysis_type = c("prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one"),
                             group_var = c(NA,NA,NA,NA,NA,NA,NA))
  ipc_loa <- bind_rows(ipc_loa, hhs_cat_bit)
}

if ("HDDS" %in% FSL_indicators){
  hdds_cat_bit <-  data.frame(analysis_var = c("fsl_hdds_cat",
                                               fsl_hdds_cereals,
                                               fsl_hdds_tubers,
                                               fsl_hdds_veg,
                                               fsl_hdds_fruit,
                                               fsl_hdds_meat,
                                               fsl_hdds_eggs,
                                               fsl_hdds_fish,
                                               fsl_hdds_legumes,
                                               fsl_hdds_dairy,
                                               fsl_hdds_oil,
                                               fsl_hdds_sugar,
                                               fsl_hdds_condiments),
                             analysis_type = c("prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one",
                                               "prop_select_one"),
                             group_var = c(NA,NA,NA,NA,NA,NA,NA,NA,
                                           NA,NA,NA,NA,NA))
  ipc_loa <- bind_rows(ipc_loa, hdds_cat_bit)
}
main <- data.list$main %>%
  mutate_at(vars(fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy,
                 fsl_fcs_meat, fsl_fcs_veg, fsl_fcs_fruit,
                 fsl_fcs_oil, fsl_fcs_sugar, fsl_fcs_condiments,
                 fsl_rcsi_lessquality, fsl_rcsi_borrow, fsl_rcsi_mealsize, fsl_rcsi_mealadult,
                 fsl_rcsi_mealnb), as.numeric)

ipc_table_design <- srvyr::as_survey_design(main)
ipc_table_results_analysis <- analysistools::create_analysis(design = ipc_table_design, loa = ipc_loa, sm_separator = "/")


## part 5 outputs
no_na_rows <- ipc_table_results_analysis$results_table %>%
  dplyr::filter(!(analysis_type == "prop_select_one" & is.na(analysis_var_value)))
example_ipc <-  presentresults::create_ipc_table(
  results_table = no_na_rows,
  dataset = main,
  cluster_name = NULL,
  with_fclc = FALSE,
  fcs_cat_var = "fsl_fcs_cat",
  rcsi_cat_var = "fsl_rcsi_cat",
  lcsi_cat_var = "fsl_lcsi_cat",
  hhs_cat_var = "fsl_hhs_cat_ipc",
  fcs_set = c(fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy,
              fsl_fcs_meat, fsl_fcs_veg, fsl_fcs_fruit,
              fsl_fcs_oil, fsl_fcs_sugar, fsl_fcs_condiments),
  rcsi_set = c(fsl_rcsi_lessquality, fsl_rcsi_borrow, fsl_rcsi_mealsize, fsl_rcsi_mealadult,
               fsl_rcsi_mealnb),
  lcsi_set = c(
    fsl_lcsi_stress1,
    fsl_lcsi_stress2,
    fsl_lcsi_stress3,
    fsl_lcsi_stress4,
    fsl_lcsi_crisis1,
    fsl_lcsi_crisis2,
    fsl_lcsi_crisis3,
    fsl_lcsi_emergency1,
    fsl_lcsi_emergency2,
    fsl_lcsi_emergency3
  ),
  lcsi_value_set = c(yes_val, no_val, exhausted_val,
                     not_applicable_val),
  hhs_cat_values = c("None", "No or Little", "Moderate", "Severe", "Very Severe")
)

example_ipc %>%
 presentresults::create_xlsx_group_x_variable(table_name = "ipc_table",
                               file_path = "output/ipc_table.xlsx",
                               overwrite = T)
