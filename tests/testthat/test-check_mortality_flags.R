library(dplyr)

###### Sad Path #######
testthat::test_that("Check input type -- dataset", {
  testthat::expect_error(check_mortality_flags(df = 0))
  testthat::expect_error(check_mortality_flags(df = "x"))
  testthat::expect_error(check_mortality_flags(df = 1.0))
  testthat::expect_error(check_mortality_flags(df = F))
  testthat::expect_error(check_mortality_flags(df = list()))
})

testthat::test_that("Check dataframe empty", {
  test_df <- data.frame()
  testthat::expect_error(check_mortality_flags(df = test_df))
})


testthat::test_that("UUID variable not available", {
  load(testthat::test_path("testdata", "test_df_mortality_long.rda"))
  testthat::expect_error(check_mortality_flags(
    df = test_df_mortality %>% dplyr::select(-uuid)
  ))
})


testthat::test_that("Enumerator variable not available", {
  load(testthat::test_path("testdata", "test_df_mortality_long.rda"))
  testthat::expect_error(check_mortality_flags(
    df = test_df_mortality %>% dplyr::select(-enumerator)
  ))
})


#### Happy Path ######

# Create a sample dataset
set.seed(123)

df <- data.frame(
  uuid = rep(1:3, each = 3),
  enumerator = rep(letters[1:3], each = 3),
  sex = c(0,0,1,1,0,1,0,1,1),
  death = c(1,1,1,1,1,1,1,1,1),
  death_cause = c("post_partum", "during_pregnancy", "during_delivery",
                  "other_cause", "during_pregnancy", "other_cause",
                  "post_partum", "during_delivery", "other_cause"),
  person_time = c(100, -50, NA, 200, 150, 100, 300, NA, 250),
  stringsAsFactors = FALSE
)

# Define expected output
expected_output <- data.frame(
  uuid = c(1, 2, 3),
  enumerator = c("a", "b", "c"),
  flag_multiple_death = c(1, 1, 1),
  flag_cause_death = c(1, 0, 1),
  flag_negative_pt = c(1, 0, 0)
) %>% as.data.frame()


# Write test cases
testthat::test_that("check_mortality_flags function works correctly", {
  output <- check_mortality_flags(df) %>% as.data.frame()
  testthat::expect_equal(output, expected_output)
})
