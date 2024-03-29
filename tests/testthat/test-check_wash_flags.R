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

######### Happy Path #########

testthat::test_that("check column group added if missing grouping", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  actual <- "All"
  expected_output <- check_wash_flags(
    .dataset = test_df,
    data_container_loop = test_df_wash,
    grouping =NULL) %>%
    pull(group) %>% unique()
  testthat::expect_equal(actual, expected_output)
})


testthat::test_that("check column group added if called with a variable", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  actual <- c("1","5","2","3","4")
  expected_output <- check_wash_flags(
    .dataset = test_df,
    data_container_loop = test_df_wash,
    grouping = "enumerator") %>%
    pull(group) %>% unique()
  testthat::expect_equal(actual, expected_output)
})


testthat::test_that("check column group added if missing grouping", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  actual <- c("1","5","2","3","4")
  expected_output <- check_wash_flags(
    .dataset = test_df,
    data_container_loop = test_df_wash,
    grouping = "enumerator") %>%
    pull(group) %>% unique()
  testthat::expect_equal(actual, expected_output)
})

testthat::test_that("check column group added if missing grouping", {
  load(testthat::test_path("testdata", "test_df.rda"))
  load(testthat::test_path("testdata", "test_df_wash.rda"))
  actual <- c("1","5","2","3","4")
  expected_output <- check_wash_flags(
    .dataset = test_df,
    data_container_loop = test_df_wash,
    grouping = "enumerator") %>%
    pull(group) %>% unique()
  testthat::expect_equal(actual, expected_output)
})





