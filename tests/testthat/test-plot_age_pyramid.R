library(ggplot2)
library(dplyr)
library(tidyr)
library(apyramid)

# Mock dataset
mock_data <- tibble(
  sex = c(1, 1, 1, 2, 2, 2),
  age_years = c(25, 30, 35, 20, 40, 45)
)

# Test if the function returns a ggplot object
testthat::test_that("plot_age_pyramid returns a ggplot object", {
  testthat::expect_true(is.ggplot(plot_age_pyramid(mock_data)))
})


testthat::test_that("Test file output", {
  temp_path <- tempfile(fileext = ".png")
  plot_age_pyramid(mock_data, file_path = temp_path, title = "test_title")
  testthat::expect_true(file.exists(temp_path), "File is created")
})
