library(dplyr)

###### Sad Path #######
testthat::test_that("Check input type -- tool.survey", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = "x"))
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = 0))
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = list()))
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = 1.2))
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = F))
})

testthat::test_that("Check dataframe empty", {
  load(testthat::test_path("testdata", "test_df.rda"))
  tool.survey <- data.frame()
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = tool.survey))
})

testthat::test_that("Check dataframe empty", {

  load(testthat::test_path("testdata", "test_df.rda"))
  tool.survey <- NULL
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = tool.survey))
})

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(check_fsl_flags(.dataset = 0))
  testthat::expect_error(check_fsl_flags(.dataset = "x"))
  testthat::expect_error(check_fsl_flags(.dataset = 1.0))
  testthat::expect_error(check_fsl_flags(.dataset = F))
  testthat::expect_error(check_fsl_flags(.dataset = list()))
})

testthat::test_that("Check dataframe empty", {
  load(testthat::test_path("testdata", "test_survey.rda"))
  df1 <- data.frame()
  testthat::expect_error(check_fsl_flags(.dataset = df1, tool.survey = test_survey))
})


testthat::test_that("UUID variable not available", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))
  testthat::expect_error(check_fsl_flags(
    .dataset = test_df %>% dplyr::select(-uuid),
    tool.survey = test_survey,
    uuid = "uuid"
  ))
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_cereal)
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_legumes),
    fsl_fcs_legumes = "fsl_fcs_legumes"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_veg),
    fsl_fcs_veg = "fsl_fcs_veg"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_fruit),
    fsl_fcs_fruit = "fsl_fcs_fruit"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_meat),
    fsl_fcs_meat = "fsl_fcs_meat"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_dairy),
    fsl_fcs_dairy = "fsl_fcs_dairy"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_sugar),
    fsl_fcs_sugar = "fsl_fcs_sugar"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_oil),
    fsl_fcs_oil = "fsl_fcs_oil"
  ))


  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_nofoodhh),
    fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_nofoodhh_freq),
    fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_sleephungry),
    fsl_hhs_sleephungry = "fsl_hhs_sleephungry"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_sleephungry_freq),
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_alldaynight),
    fsl_hhs_alldaynight = "fsl_hhs_alldaynight"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_alldaynight_freq),
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_lessquality),
    fsl_rcsi_lessquality = "fsl_rcsi_lessquality"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_borrow),
    fsl_rcsi_borrow = "fsl_rcsi_borrow"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_mealsize),
    fsl_rcsi_mealsize = "fsl_rcsi_mealsize"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_mealadult),
    fsl_rcsi_mealadult = "fsl_rcsi_mealadult"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_mealnb),
    fsl_rcsi_mealnb = "fsl_rcsi_mealnb"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_cereals),
    fsl_hdds_cereals = "fsl_hdds_cereals"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_tubers),
    fsl_hdds_tubers = "fsl_hdds_tubers"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_veg),
    fsl_hdds_veg = "fsl_hdds_veg"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_fruit),
    fsl_hdds_fruit = "fsl_hdds_fruit"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_meat),
    fsl_hdds_meat = "fsl_hdds_meat"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_eggs),
    fsl_hdds_eggs = "fsl_hdds_eggs"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_fish),
    fsl_hdds_fish = "fsl_hdds_fish"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_legumes),
    fsl_hdds_legumes = "fsl_hdds_legumes"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_dairy),
    fsl_hdds_dairy = "fsl_hdds_dairy"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_oil),
    fsl_hdds_oil = "fsl_hdds_oil"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_sugar),
    fsl_hdds_sugar = "fsl_hdds_sugar"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_condiments),
    fsl_hdds_condiments = "fsl_hdds_condiments"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress1),
    fsl_lcsi_stress1 = "fsl_lcsi_stress1"
  ))

  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress2),
    fsl_lcsi_stress2 = "fsl_lcsi_stress2"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress3),
    fsl_lcsi_stress3 = "fsl_lcsi_stress3"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress4),
    fsl_lcsi_stress4 = "fsl_lcsi_stress4"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_crisis1),
    fsl_lcsi_crisis1 = "fsl_lcsi_crisis1"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_crisis2),
    fsl_lcsi_crisis2 = "fsl_lcsi_crisis2"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_crisis3),
    fsl_lcsi_crisis3 = "fsl_lcsi_crisis3"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_emergency1),
    fsl_lcsi_emergency1 = "fsl_lcsi_emergency1"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_emergency2),
    fsl_lcsi_emergency2 = "fsl_lcsi_emergency2"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_emergency3),
    fsl_lcsi_emergency3 = "fsl_lcsi_emergency3"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fc_cell),
    fsl_fc_cell = "fc_cell"
  ))
  testthat::expect_warning(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fc_phase),
    fsl_fc_phase = "fc_phase"
  ))
})

testthat::test_that("Check for missing num_children column", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))
  testthat::expect_warning(check_fsl_flags(
    tool.survey = test_survey,
    .dataset = test_df_with_calculation,
    num_children = "x"
  ))
})

