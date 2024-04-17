library(fs)

testthat::test_that("Test basic functionality", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "test_fsl_folder")

  create_fsl_analysis_template(test_path)

  testthat::expect_true(fs::dir_exists(test_path))
})


testthat::test_that("Test invalid input handling", {
  testthat::expect_error(create_fsl_analysis_template(NULL))
})
