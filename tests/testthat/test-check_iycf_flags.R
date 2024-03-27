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
    age_months = "child_age_months_2"
  ))
})


testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_iycf.rda"))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_4),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_6a),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6b),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6c),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6d),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6e),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6f),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6g),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6h),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6i),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6j),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7a),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7b),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7c),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7d),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7e),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7f),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7g),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7h),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7i),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7j),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7k),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7l),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7m),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7n),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7o),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7p),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7q),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf ,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_7r),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_8),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6b_num),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6c_num),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2"
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6d_num),
    uuid = "_submission__uuid",
    age_months = "child_age_months_2",
    grouping = "enum_iycf"
  ))

})

