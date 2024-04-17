library(ggplot2)
library(GGally)
library(fs)

testthat::test_that("Test input validation", {
  expect_error(plot_correlogram(NULL))
})

testthat::test_that("Test basic functionality", {
  # Mock data for testing
  data <- data.frame(
    fsl_fcs_score = rnorm(10),
    fsl_rcsi_score = rnorm(10),
    fsl_hhs_score = rnorm(10)
  )

  # Test the plot generation
  result <- plot_correlogram(.dataset = data)
  testthat::expect_s3_class(result, "gg")
})

testthat::test_that("Test file creation", {
  temp_dir <- tempdir()
  file_path <- file.path(temp_dir, "test_plot.png")

  data <- data.frame(
    fsl_fcs_score = rnorm(10),
    fsl_rcsi_score = rnorm(10),
    fsl_hhs_score = rnorm(10)
  )
  plot_correlogram(.dataset = data, file_path = file_path)

  testthat::expect_true(file_exists(file_path))
})

