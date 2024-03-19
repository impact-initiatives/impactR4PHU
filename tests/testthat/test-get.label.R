library(dplyr)

###### Sad Path #######

testthat::test_that("Check input variable", {
  testthat::expect_error(get.label(variable = NULL))
})

testthat::test_that("Check input type -- tool.survey", {

  testthat::expect_error(get.label(variable = "a", tool.survey = 0))
  testthat::expect_error(get.label(variable = "a", tool.survey = "x"))
  testthat::expect_error(get.label(variable = "a", tool.survey = 1.0))
  testthat::expect_error(get.label(variable = "a", tool.survey = F))
  testthat::expect_error(get.label(variable = "a", tool.survey = list()))
})

testthat::test_that("Check dataframe empty", {
  tool.survey <- data.frame()
  testthat::expect_error(get.label(variable = "a", tool.survey  = tool.survey))
})

testthat::test_that("Check dataframe empty", {
  tool.survey <- NULL
  testthat::expect_error(get.label(variable = "a", tool.survey  = tool.survey))
})

testthat::test_that("Check variable not in tool", {
  load(testthat::test_path("testdata","test_survey.rda"))
  testthat::expect_warning(get.label("a", tool.survey = test_survey))
})

####### Happy path ########

testthat::test_that("Check output of with a non-select multiple variable", {
  load(testthat::test_path("testdata","test_survey.rda"))
  testthat::expect_equal("What is the marital status of the head of household?",
                         get.label("hohh_status",tool.survey = test_survey))
  }
)

testthat::test_that("Check output of with a non-select multiple variable", {
  load(testthat::test_path("testdata","test_survey.rda"))
  testthat::expect_equal("What symptoms did the individual have?",
                         get.label("health_ind_symptom/x_choice",tool.survey = test_survey))
  }
)

