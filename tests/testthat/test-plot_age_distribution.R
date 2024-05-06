library(ggplot2)
library(dplyr)
library(tidyr)


# Mock dataset
mock_data <- tibble(
  age_years = c(25, 30, 35, 20, 40, 45),
  age_months = c(12, 24, 36, 48, 60, 72),
  by_group = c("A", "B", "A", "B", "A", "B")
)

# Test if the function returns a ggplot object
testthat::test_that("plot_age_distribution returns a ggplot object", {
  testthat::expect_true(is.ggplot(plot_age_distribution(mock_data, year_or_month = "month")))
})


testthat::test_that("Test file output", {
  temp_path <- tempfile(fileext = ".png")
  plot_age_distribution(mock_data, file_path = temp_path, year_or_month = "year")
  testthat::expect_true(file.exists(temp_path), "File is created")
})
