library(dplyr)

###### Sad Path #######

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(check_wash_flags(.dataset = 0))
  testthat::expect_error(check_wash_flags(.dataset = "x"))
  testthat::expect_error(check_wash_flags(.dataset = 1.0))
  testthat::expect_error(check_wash_flags(.dataset = F))
  testthat::expect_error(check_wash_flags(.dataset = list()))
})

testthat::test_that("Check dataframe empty", {
  test_df <- data.frame()
  testthat::expect_error(check_wash_flags(.dataset = test_df))
})

testthat::test_that("Check input type -- data_container_loop", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  testthat::expect_error(check_wash_flags(.dataset = test_df, data_container_loop = 0))
  testthat::expect_error(check_wash_flags(.dataset = test_df, data_container_loop = "x"))
  testthat::expect_error(check_wash_flags(.dataset = test_df, data_container_loop = 1.0))
  testthat::expect_error(check_wash_flags(.dataset = test_df, data_container_loop = F))
  testthat::expect_error(check_wash_flags(.dataset = test_df, data_container_loop = list()))
})

testthat::test_that("Check dataframe empty", {
  load(testthat::test_path("testdata", "test_df.rda"))
  test_df_wash <- data.frame()
  testthat::expect_error(check_wash_flags(.dataset = test_df, data_container_loop = test_df_wash))
})

testthat::test_that("UUID variable not available", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  testthat::expect_error(check_wash_flags(
    .dataset = test_df %>% dplyr::select(-uuid),
    data_container_loop = test_df_wash,
    uuid = "uuid"
  ))
})


testthat::test_that("check data_container_loop is null", {
  load(testthat::test_path("testdata", "test_df.rda"))

  testthat::expect_warning(check_wash_flags(
    .dataset = test_df
  ))
})


###### Happy Path #######

testthat::test_that("Happy Path", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  load(testthat::test_path("testdata", "test_df_wash_calc.rda"))
  actual <- check_wash_flags(test_df,test_df_wash)
  expected <- test_df_wash_calc
  testthat::expect_equal(actual,expected)
})






