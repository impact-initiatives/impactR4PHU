# library(dplyr)
#
#
# testthat::test_that("Check input type -- dataset", {
#   testthat::expect_error(add_iycf(.dataset = 0))
#   testthat::expect_error(add_iycf(.dataset = "x"))
#   testthat::expect_error(add_iycf(.dataset = 1.0))
#   testthat::expect_error(add_iycf(.dataset = F))
#   testthat::expect_error(add_iycf(.dataset = list()))
# })
#
# testthat::test_that("Check dataframe empty", {
#   df1 <- data.frame()
#   testthat::expect_error(add_iycf(.dataset = df1))
# })
#
# testthat::test_that("UUID variable not available", {
#   load(testthat::test_path("testdata", "test_df_iycf.rda"))
#   testthat::expect_error(add_iycf(
#     .dataset = test_df_iycf %>% dplyr::select(-`_submission__uuid`),
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
# })
#
# testthat::test_that("Age Month variable not available", {
#   load(testthat::test_path("testdata", "test_df_iycf.rda"))
#   testthat::expect_error(add_iycf(
#     .dataset = test_df_iycf %>% dplyr::select(-child_age_months_2),
#     age_months = "child_age_months_2"
#   ))
# })
#
#
# testthat::test_that("Check for missing columns", {
#   load(testthat::test_path("testdata", "test_df_iycf.rda"))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_1),
#     iycf_1 = NULL,
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_2),
#     iycf_2 = NULL,
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_3),
#     iycf_3 = NULL,
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2",
#     grouping = "enum_iycf"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_4),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_5),
#     iycf_5 = NULL,
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6a),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6b),
#     iycf_6b = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6c),
#     iycf_6c = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6d),
#     iycf_6d = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6e),
#     iycf_6e = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6f),
#     iycf_6f = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6g),
#     iycf_6g = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6h),
#     iycf_6h = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6i),
#     iycf_6i = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6j),
#     iycf_6j = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7a),
#     iycf_7a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7b),
#     iycf_7b = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7c),
#     iycf_7c = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7d),
#     iycf_7d = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7e),
#     iycf_7e = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7f),
#     iycf_7f = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7g),
#     iycf_7g = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7h),
#     iycf_7h = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7i),
#     iycf_7i = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7j),
#     iycf_7j = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7k),
#     iycf_7k = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7l),
#     iycf_7l = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7m),
#     iycf_7m = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7n),
#     iycf_7n = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7o),
#     iycf_7o = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7p),
#     iycf_7p = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7q),
#     iycf_7q = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_7r),
#     iycf_7r = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_8),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6c_swt),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6d_swt),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6h_swt),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
#   testthat::expect_warning(add_iycf(
#     .dataset = test_df_iycf %>%
#       dplyr::select(-iycf_6j_swt),
#     iycf_6a = NULL,
#     uuid = "_submission__uuid",
#     age_months = "child_age_months_2"
#   ))
#
# })
