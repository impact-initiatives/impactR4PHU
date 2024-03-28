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
      dplyr::select(-iycf_4)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_6a)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6b)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6c)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6d)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6e)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6f)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6g)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6h)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6i)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6j)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7a)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7b)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7c)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7d)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7e)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7f)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7g)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7h)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7i)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7j)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7k)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7l)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7m)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7n)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7o)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7p)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_7q)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf ,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2") %>%
      dplyr::select(-iycf_7r)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_8)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6b_num)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6c_num)
  ))

  testthat::expect_warning(check_iycf_flags(
    .dataset = add_iycf(.dataset = test_df_iycf,
                        uuid = "_submission__uuid",
                        age_months = "child_age_months_2")  %>%
      dplyr::select(-iycf_6d_num),
    grouping = "enum_iycf"
  ))

})

