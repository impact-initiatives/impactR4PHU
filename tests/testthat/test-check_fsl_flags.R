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

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_cereal),
    fcs_cereal = "fsl_fcs_cereal",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_legumes),
    fcs_legumes = "fsl_fcs_legumes",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_veg),
    fcs_veg = "fsl_fcs_veg",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_fruit),
    fcs_fruit = "fsl_fcs_fruit",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_meat),
    fcs_meat = "fsl_fcs_meat",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_dairy),
    fcs_dairy = "fsl_fcs_dairy",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_sugar),
    fcs_sugar = "fsl_fcs_sugar",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fcs_oil),
    fcs_oil = "fsl_fcs_oil",
    grouping = NULL
  ))


  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_nofoodhh),
    hhs_nofoodhh = "fsl_hhs_nofoodhh",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_nofoodhh_freq),
    hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_sleephungry),
    hhs_sleephungry = "fsl_hhs_sleephungry",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_sleephungry_freq),
    hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_alldaynight),
    hhs_alldaynight = "fsl_hhs_alldaynight",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hhs_alldaynight_freq),
    hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_lessquality),
    rcsi_lessquality = "fsl_rcsi_lessquality",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_borrow),
    rcsi_borrow = "fsl_rcsi_borrow",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_mealsize),
    rcsi_mealsize = "fsl_rcsi_mealsize",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_mealadult),
    rcsi_mealadult = "fsl_rcsi_mealadult",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_rcsi_mealnb),
    rcsi_mealnb = "fsl_rcsi_mealnb",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_cereals),
    hdds_cereals = "fsl_hdds_cereals",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_tubers),
    hdds_tubers = "fsl_hdds_tubers",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_veg),
    hdds_veg = "fsl_hdds_veg",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_fruit),
    hdds_fruit = "fsl_hdds_fruit",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_meat),
    hdds_meat = "fsl_hdds_meat",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_eggs),
    hdds_eggs = "fsl_hdds_eggs",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_fish),
    hdds_fish = "fsl_hdds_fish",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_legumes),
    hdds_legumes = "fsl_hdds_legumes",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_dairy),
    hdds_dairy = "fsl_hdds_dairy",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_oil),
    hdds_oil = "fsl_hdds_oil",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_sugar),
    hdds_sugar = "fsl_hdds_sugar",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_hdds_condiments),
    hdds_condiments = "fsl_hdds_condiments",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress1),
    lcsi_stress1 = "fsl_lcsi_stress1",
    grouping = NULL
  ))

  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress2),
    lcsi_stress1 = "fsl_lcsi_stress2",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress3),
    lcsi_stress1 = "fsl_lcsi_stress3",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_stress4),
    lcsi_stress1 = "fsl_lcsi_stress4",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_crisis1),
    lcsi_stress1 = "fsl_lcsi_crisis1",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_crisis2),
    lcsi_stress1 = "fsl_lcsi_crisis2",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_crisis3),
    lcsi_stress1 = "fsl_lcsi_crisis3",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_emergency1),
    lcsi_stress1 = "fsl_lcsi_emergency1",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_emergency2),
    lcsi_stress1 = "fsl_lcsi_emergency2",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_emergency3),
    lcsi_stress1 = "fsl_lcsi_emergency3",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fc_cell),
    fc_cell = "fc_cell",
    grouping = NULL
  ))
  testthat::expect_error(check_fsl_flags(tool.survey = test_survey,
    .dataset = test_df_with_calculation %>% dplyr::select(-fc_phase),
    fc_phase = "fc_phase",
    grouping = NULL
  ))
})

testthat::test_that("Check for missing num_children column", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))
  testthat::expect_warning(check_fsl_flags(
    tool.survey = test_survey,
    .dataset = test_df_with_calculation,
    num_children = "x",
    grouping = NULL
  ))
})

######### Happy Path #########

testthat::test_that("check column group added if missing grouping", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))
   actual <- "All"
   expected_output <- check_fsl_flags(
     tool.survey = test_survey,
     .dataset = test_df_with_calculation,
     grouping =NULL) %>%
     pull(group) %>% unique()
   testthat::expect_equal(actual, expected_output)
})

#
# testthat::test_that("check column group added if missing grouping", {
#   load(testthat::test_path("testdata", "test_survey.rda"))
#   load(testthat::test_path("testdata", "test_df.rda"))
#   actual <- data.frame(
#     uuid = c("uuid_1","uuid_2","uuid_3"),
#     group = c("All","All","All"),
#     fsl_fcs_cereal = c(2,5, 1),
#     fsl_fcs_legumes = c(1,6,0),
#     fsl_fcs_dairy = c(4,5,1),
#     fsl_fcs_meat = c(2,6,0),
#     fsl_fcs_veg = c(3,6,1),
#     fsl_fcs_fruit = c(1,6,0),
#     fsl_fcs_oil = c(4,7,1),
#     fsl_fcs_sugar = c(5,3,0),
#     fsl_fcs_score = c(39.5,89.0,7.5),
#     fsl_fcs_cat = c("Acceptable","Acceptable","Poor"),
#     flag_meat_cereal_ratio = c(0,1,0),
#     flag_low_cereal = c(1,0,1),
#     flag_low_oil = c(1,0,1),
#     flag_low_fcs = c(0,0,1),
#     flag_high_fcs = c(0,1,0),
#     flag_sd_foodgroup = c(0,0,1)
#     )
#   expected_output <- check_fsl_flags(
#     tool.survey = test_survey,
#     .dataset = data.frame(
#       uuid = c("uuid_1","uuid_2","uuid_3"),
#       group = c("All","All","All"),
#       fsl_fcs_cereal = c(2,5, 1),
#       fsl_fcs_legumes = c(1,6,0),
#       fsl_fcs_dairy = c(4,5,1),
#       fsl_fcs_meat = c(2,6,0),
#       fsl_fcs_veg = c(3,6,1),
#       fsl_fcs_fruit = c(1,6,0),
#       fsl_fcs_oil = c(4,7,1),
#       fsl_fcs_sugar = c(5,3,0),
#       fsl_fcs_score = c(39.5,89.0,7.5),
#       fsl_fcs_cat = c("Acceptable","Acceptable","Poor")),
#     grouping =NULL) %>%
#     select(uuid,group,fsl_fcs_cereal,fsl_fcs_legumes,fsl_fcs_dairy,
#            fsl_fcs_meat,fsl_fcs_veg,fsl_fcs_fruit,fsl_fcs_oil,fsl_fcs_sugar,
#            fsl_fcs_score,fsl_fcs_cat,flag_meat_cereal_ratio,flag_low_cereal,flag_low_oil,
#            flag_low_fcs,flag_high_fcs,flag_sd_foodgroup)
#   testthat::expect_equal(actual, expected_output)
# })
