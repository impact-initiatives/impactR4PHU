library(dplyr)

###### Sad Path #######
testthat::test_that("Check input type -- tool.survey", {
  testthat::expect_error(check_anthro_flags(.dataset = "x"))
  testthat::expect_error(check_anthro_flags(.dataset = 0))
  testthat::expect_error(check_anthro_flags(.dataset = list()))
  testthat::expect_error(check_anthro_flags(.dataset = 1.2))
})

testthat::test_that("Check dataframe empty", {
  test_df <- data.frame()
  testthat::expect_error(check_anthro_flags(.dataset = test_df))
})

testthat::test_that("Check nut_muac_cm in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>% dplyr::select(-nut_muac_cm)))
})

testthat::test_that("Check mam_muac in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>% add_muac() %>%
      add_mfaz() %>% dplyr::select(-mam_muac)))
})

testthat::test_that("Check moderate_mfaz in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>% add_muac() %>%
      add_mfaz() %>% dplyr::select(-moderate_mfaz)))
})

###### Happy Path #######
testthat::test_that("Check nut_edema_confirm in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>%  add_muac() %>%
      add_mfaz() %>% dplyr::select(-nut_edema_confirm)))
})

testthat::test_that("Check loop_index in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>%  add_muac() %>%
      add_mfaz()  %>% dplyr::select(-nut_edema_confirm),
    loop_index = "loop_index"))
})

