library(dplyr)

###### Sad Path #######
testthat::test_that("Check input type -- tool.survey", {
  testthat::expect_error(add_mfaz(.dataset = "x"))
  testthat::expect_error(add_mfaz(.dataset = 0))
  testthat::expect_error(add_mfaz(.dataset = list()))
  testthat::expect_error(add_mfaz(.dataset = 1.2))
  testthat::expect_error(add_mfaz(.dataset = F))
})

testthat::test_that("Check dataframe empty", {
  test_df <- data.frame()
  testthat::expect_error(add_mfaz(.dataset = test_df))
})

testthat::test_that("Check nut_muac_cm in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_error(add_mfaz(
    .dataset = test_df_nut %>% dplyr::select(-nut_muac_cm)))
})

###### Happy Path #######
testthat::test_that("Check nut_edema_confirm in the dataset", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  testthat::expect_warning(add_mfaz(
    .dataset = test_df_nut %>% dplyr::select(-nut_edema_confirm)))
})


testthat::test_that("check output correct", {
  load(testthat::test_path("testdata","test_df_nut.rda"))
  df1 <- data.frame(
    uuid = c("uuid_1","uuid_2"),
    nut_muac_cm = c("12.5","10"),
    child_sex = c("m","f"),
    child_age_months = c("14","54"),
    nut_edema_confirm = c("yes",NA))
  actual <- add_mfaz(.dataset = df1)%>%
    dplyr::select(severe_mfaz,moderate_mfaz,global_mfaz)
  expected_output <- data.frame(
    uuid = c("uuid_1","uuid_2"),
    nut_muac_cm = c("12.5","10"),
    child_sex = c("m","f"),
    child_age_months = c("14","54"),
    nut_edema_confirm = c("yes",NA),
    severe_mfaz = c(1,1),
    moderate_mfaz = c(1,0),
    global_mfaz = c(1,1)) %>%
    dplyr::select(severe_mfaz,moderate_mfaz,global_mfaz)
  testthat::expect_equal(actual, expected_output)
})
