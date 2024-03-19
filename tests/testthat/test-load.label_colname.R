library(dplyr)

###### Sad Path #######

testthat::test_that("Check input type -- tool.survey", {
  testthat::expect_error(load.label_colname(tool.survey = 0))
  testthat::expect_error(load.label_colname(tool.survey = "x"))
  testthat::expect_error(load.label_colname(tool.survey = 1.0))
  testthat::expect_error(load.label_colname(tool.survey = F))
  testthat::expect_error(load.label_colname(tool.survey = list()))
})

testthat::test_that("Check tool.survey empty", {
  tool.survey <- data.frame()
  testthat::expect_error(load.label_colname(tool.survey  = tool.survey))
})

testthat::test_that("Check tool.survey NULL", {
  tool.survey <- NULL
  testthat::expect_error(load.label_colname(tool.survey  = tool.survey))
})

###### Happy Path #######

testthat::test_that("Check output value", {
  load(testthat::test_path("testdata", "test_survey.rda"))
  actual_output <- "label::English"
  expected_output <- load.label_colname(tool.survey = test_survey)

  testthat::expect_equal(actual_output, expected_output)
})
