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


