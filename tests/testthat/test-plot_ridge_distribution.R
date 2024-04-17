library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)

# Mock data for testing
data <- data.frame(
  group = rep(c("A", "B"), each = 5),
  score = rnorm(10),
  category = rep(c("X", "Y"), times = 5)
)



testthat::test_that("Test file output", {
  temp_path <- tempfile(fileext = ".png")
  plot_ridge_distribution(data, numeric_cols = "score", name_groups = "category", name_units = "score", grouping = "group", file_path = temp_path)
  testthat::expect_true(file.exists(temp_path), "File is created")
})

testthat::test_that("Test parameter effects", {
  # Test changes with grouping
  plot1 <- plot_ridge_distribution(data, numeric_cols = "score", name_groups = "category", name_units = "score", grouping = "group")
  plot2 <- plot_ridge_distribution(data, numeric_cols = "score", name_groups = "category", name_units = "score", grouping = NULL)
  testthat::expect_false(identical(plot1, plot2), "Plots are different when grouping is changed")
})

