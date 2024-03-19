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
  testthat::expect_error(check_fsl_flags(.dataset = test_df,tool.survey = NA))
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
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_cereal),
    fcs_cereal = "fsl_fcs_cereal"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_legumes),
    fcs_legumes = "fsl_fcs_legumes"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_veg),
    fcs_veg = "fsl_fcs_veg"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_fruit),
    fcs_fruit = "fsl_fcs_fruit"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_meat),
    fcs_meat = "fsl_fcs_meat"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_dairy),
    fcs_dairy = "fsl_fcs_dairy"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_sugar),
    fcs_sugar = "fsl_fcs_sugar"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_fcs_oil),
    fcs_oil = "fsl_fcs_oil"
  ))


  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hhs_nofoodhh),
    hhs_nofoodhh = "fsl_hhs_nofoodhh"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hhs_nofoodhh_freq),
    hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hhs_sleephungry),
    hhs_sleephungry = "fsl_hhs_sleephungry"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hhs_sleephungry_freq),
    hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hhs_alldaynight),
    hhs_alldaynight = "fsl_hhs_alldaynight"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hhs_alldaynight_freq),
    hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_lessquality),
    rcsi_lessquality = "fsl_rcsi_lessquality"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_borrow),
    rcsi_borrow = "fsl_rcsi_borrow"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_mealsize),
    rcsi_mealsize = "fsl_rcsi_mealsize"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_mealadult),
    rcsi_mealadult = "fsl_rcsi_mealadult"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_rcsi_mealnb),
    rcsi_mealnb = "fsl_rcsi_mealnb"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_cereals),
    hdds_cereals = "fsl_hdds_cereals"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_tubers),
    hdds_tubers = "fsl_hdds_tubers"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_veg),
    hdds_veg = "fsl_hdds_veg"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_fruit),
    hdds_fruit = "fsl_hdds_fruit"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_meat),
    hdds_meat = "fsl_hdds_meat"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_eggs),
    hdds_eggs = "fsl_hdds_eggs"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_fish),
    hdds_fish = "fsl_hdds_fish"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_legumes),
    hdds_legumes = "fsl_hdds_legumes"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_dairy),
    hdds_dairy = "fsl_hdds_dairy"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_oil),
    hdds_oil = "fsl_hdds_oil"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_sugar),
    hdds_sugar = "fsl_hdds_sugar"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_hdds_condiments),
    hdds_condiments = "fsl_hdds_condiments"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_stress1),
    lcsi_stress1 = "fsl_lcsi_stress1"
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_stress2),
    lcsi_stress1 = "fsl_lcsi_stress2"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_stress3),
    lcsi_stress1 = "fsl_lcsi_stress3"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_stress4),
    lcsi_stress1 = "fsl_lcsi_stress4"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_crisis1),
    lcsi_stress1 = "fsl_lcsi_crisis1"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_crisis2),
    lcsi_stress1 = "fsl_lcsi_crisis2"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_crisis3),
    lcsi_stress1 = "fsl_lcsi_crisis3"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_emergency1),
    lcsi_stress1 = "fsl_lcsi_emergency1"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_emergency2),
    lcsi_stress1 = "fsl_lcsi_emergency2"
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df %>% dplyr::select(-fsl_lcsi_emergency3),
    lcsi_stress1 = "fsl_lcsi_emergency3"
  ))
})


