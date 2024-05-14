library(dplyr)



testthat::test_that("Check input type -- dataset", {
  testthat::expect_error(check_iycf_flags(.dataset = 0))
  testthat::expect_error(check_iycf_flags(.dataset = "x"))
  testthat::expect_error(check_iycf_flags(.dataset = 1.0))
  testthat::expect_error(check_iycf_flags(.dataset = F))
  testthat::expect_error(check_iycf_flags(.dataset = list()))
})

testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(check_iycf_flags(.dataset = df1))
})

testthat::test_that("UUID variable not available", {
  load(testthat::test_path("testdata", "test_df_iycf.rda"))
  testthat::expect_error(check_iycf_flags(
    .dataset = test_df_iycf %>% dplyr::select(-`_submission__uuid`),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))
})

testthat::test_that("Age Month variable not available", {
  load(testthat::test_path("testdata", "test_df_iycf.rda"))
  testthat::expect_error(check_iycf_flags(
    .dataset = test_df_iycf %>% dplyr::select(-child_age_months_2),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))
})


testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_iycf.rda"))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_4),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_6a),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6b),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6c),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6d),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6e),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6f),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6g),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6h),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6i),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6j),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7a),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7b),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7c),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7d),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7e),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7f),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7g),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7h),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7i),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7j),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7k),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7l),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7m),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7n),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7o),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7p),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7q),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf ,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_7r),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_8),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6b_num),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6c_num),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6d_num),
    age_months = "child_age_months_2",
    uuid = "_submission__uuid"
  ))

})

