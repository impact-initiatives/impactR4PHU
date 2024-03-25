library(dplyr)

###### Sad Path #######
testthat::test_that("Check input type -- tool.survey", {
  testthat::expect_error(check_anthro_flags(.dataset = "x"))
  testthat::expect_error(check_anthro_flags(.dataset = 0))
  testthat::expect_error(check_anthro_flags(.dataset = list()))
  testthat::expect_error(check_anthro_flags(.dataset = 1.2))
})

testthat::test_that("Check dataframe empty", {
  test_df <- data.frame()
  testthat::expect_error(check_anthro_flags(.dataset = test_df))
})

testthat::test_that("Check nut_muac_cm in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>% dplyr::select(-nut_muac_cm)))
})

testthat::test_that("Check mam_muac in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>% add_muac() %>%
      add_mfaz() %>% dplyr::select(-mam_muac)))
})

testthat::test_that("Check moderate_mfaz in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>% add_muac() %>%
      add_mfaz() %>% dplyr::select(-moderate_mfaz)))
})

###### Happy Path #######
testthat::test_that("Check nut_edema_confirm in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>%  add_muac() %>%
      add_mfaz() %>% dplyr::select(-nut_edema_confirm)))
})

testthat::test_that("Check loop_index in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(check_anthro_flags(
    .dataset = test_df_nut %>%  add_muac() %>%
      add_mfaz()  %>% dplyr::select(-nut_edema_confirm),
    loop_index = "loop_index"))
})


testthat::test_that("check column group added if missing grouping", {
  load(testthat::test_path("testdata", "test_df_nut.rda"))
  actual <- "All"
  expected_output <- test_df_nut %>%
    add_muac() %>%
    add_mfaz()%>%
    check_anthro_flags(grouping =NULL) %>%
    pull(group) %>% unique()
  testthat::expect_equal(actual, expected_output)
})



#
# testthat::test_that("check output correct", {
#   load(testthat::test_path("testdata","test_df_nut.rda"))
#   df1 <- data.frame(
#     uuid = c("uuid_1","uuid_2"),
#     nut_muac_cm = c("12.5","10"),
#     child_sex = c("m","f"),
#     child_age_months = c("14","54"),
#     nut_edema_confirm = c("yes",NA))
#   actual <- check_anthro_flags(.dataset = df1)%>%
#     dplyr::select(sam_muac,mam_muac,gam_muac)
#   expected_output <- data.frame(
#     uuid = c("uuid_1","uuid_2"),
#     nut_muac_cm = c("12.5","10"),
#     child_sex = c("m","f"),
#     child_age_months = c("14","54"),
#     nut_edema_confirm = c("yes",NA),
#     sam_muac = c(1,1),
#     mam_muac = c(0,0),
#     gam_muac = c(1,1)) %>%
#     dplyr::select(sam_muac,mam_muac,gam_muac)
#   testthat::expect_equal(actual, expected_output)
# })
