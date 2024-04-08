library(dplyr)

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(create_fsl_plaus(.dataset = 0))
  testthat::expect_error(create_fsl_plaus(.dataset = "x"))
  testthat::expect_error(create_fsl_plaus(.dataset = 1.0))
  testthat::expect_error(create_fsl_plaus(.dataset = F))
  testthat::expect_error(create_fsl_plaus(.dataset = list()))
})

testthat::test_that("Check dataframe empty", {
  load(testthat::test_path("testdata", "test_survey.rda"))
  df1 <- data.frame()
  testthat::expect_error(create_fsl_plaus(.dataset = df1))
})


testthat::test_that("UUID variable not available", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(create_fsl_plaus(
    .dataset = test_df %>% dplyr::select(-uuid),
    uuid = "uuid"
  ))
})

testthat::test_that("Check result", {
  load(testthat::test_path("testdata", "test_df_fsl_flags.rda"))
  load(testthat::test_path("testdata", "test_survey.rda"))
  load(testthat::test_path("testdata", "test_df_fsl_plaus.rda"))
  actual <- test_df_fsl_plaus
  expected <- test_df_fsl_flags %>%
    create_fsl_plaus()
  testthat::expect_equal(actual,expected)
})


testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_fsl_flags.rda"))
  actual <- NA
  expected <- test_df_fsl_flags %>% dplyr::select(-fsl_fcs_score) %>%
    create_fsl_plaus() %>%
    pull(corr.fcs_rcsi) %>% unique()
  testthat::expect_equal(actual,expected)
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_fsl_flags.rda"))
  actual <- NA
  expected <- test_df_fsl_flags %>% dplyr::select(-fsl_hdds_score) %>%
    create_fsl_plaus() %>%
    pull(corr.fcs_hdds) %>% unique()
  testthat::expect_equal(actual,expected)
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_fsl_flags.rda"))
  actual <- NA
  expected <- test_df_fsl_flags %>% dplyr::select(-fsl_hhs_score) %>%
    create_fsl_plaus() %>%
    pull(corr.fcs_hhs) %>% unique()
  testthat::expect_equal(actual,expected)
})
