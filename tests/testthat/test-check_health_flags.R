library(dplyr)

###### Sad Path #######
testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(check_health_flags(.dataset = 0))
  testthat::expect_error(check_health_flags(.dataset = "x"))
  testthat::expect_error(check_health_flags(.dataset = 1.0))
  testthat::expect_error(check_health_flags(.dataset = F))
  testthat::expect_error(check_health_flags(.dataset = list()))
})

testthat::test_that("Check dataframe empty", {
  test_df <- data.frame()
  testthat::expect_error(check_health_flags(.dataset = test_df))
})


testthat::test_that("UUID variable not available", {
  load(testthat::test_path("testdata", "test_df.rda"))
  testthat::expect_error(check_health_flags(
    .dataset = test_df %>% dplyr::select(-uuid),
    uuid = "uuid"
  ))
})


testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_MSNA.rda"))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_food)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_rent)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_water)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_nfi)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_utilitiues)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_fuel)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_transportation)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_communication)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_frequent_other)
  ))

  testthat::expect_warning(check_health_flags(
    .dataset = test_df_MSNA %>%
      dplyr::select(-cm_expenditure_infrequent_health)
  ))
})

###### Happy Path #######
testthat::test_that("Check correct output", {
  load(testthat::test_path("testdata","test_df_MSNA.rda"))
  df1 <- test_df_MSNA %>%
    select(uuid,
           cm_expenditure_frequent_food,
           cm_expenditure_frequent_rent,
           cm_expenditure_frequent_water,
           cm_expenditure_frequent_nfi,
           cm_expenditure_frequent_utilitiues,
           cm_expenditure_frequent_fuel,
           cm_expenditure_frequent_transportation,
           cm_expenditure_frequent_communication,
           cm_expenditure_frequent_other,
           cm_expenditure_infrequent_shelter,
           cm_expenditure_infrequent_nfi,
           cm_expenditure_infrequent_health,
           cm_expenditure_infrequent_education,
           cm_expenditure_infrequent_debt,
           cm_expenditure_infrequent_other) %>%
    head(5)

  actual <- check_health_flags(
    .dataset = df1,
    num_period_months = 1
  ) %>%
    select(starts_with("flag_")) %>% as.data.frame()
  expected_output <- data.frame(
    flag_severe_health_exp = c(0,0,0,0,0),
    flag_catastrophic_health_exp = c(0,0,0,0,0)
  )
  testthat::expect_equal(actual,expected_output)
})
