library(dplyr)


###### Sad Path #######

testthat::test_that("Check input type -- dataset", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))
  testthat::expect_error(add_fclcm_phase(.dataset = 0))
  testthat::expect_error(add_fclcm_phase(.dataset = "x"))
  testthat::expect_error(add_fclcm_phase(.dataset = 1.0))
  testthat::expect_error(add_fclcm_phase(.dataset = F))
  testthat::expect_error(add_fclcm_phase(.dataset = list()))
})


testthat::test_that("Check dataframe empty", {
  df1 <- data.frame()
  testthat::expect_error(add_fclcm_phase(.dataset = df1))
})

testthat::test_that("Check for missing columns", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  testthat::expect_error(add_fclcm_phase(
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_fc_phase),
    fsl_fc_phase_var = "fsl_fc_phase"
  ))
  testthat::expect_error(add_fclcm_phase(
    .dataset = test_df_with_calculation %>% dplyr::select(-fsl_lcsi_cat),
    lcs_cat_var = "fsl_lcsi_cat"
  ))

})



testthat::test_that("Checking column values fsl_fc_phase - [Phase 1 FC/Phase 2 FC/Phase 3 FC/Phase 4 FC/Phase 5 FC]", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  set.seed(30)
  test_df_with_calculation[sample.int(nrow(test_df_with_calculation), 3), c("fsl_fc_phase")] <- "Phase 6 FC"
  test_df_with_calculation[sample.int(nrow(test_df_with_calculation), 3), c("fsl_fc_phase")] <- "little"
  test_df_with_calculation[sample.int(nrow(test_df_with_calculation), 3), c("fsl_fc_phase")] <- "random_value"
  testthat::expect_error(add_fclcm_phase(
    .dataset = test_df_with_calculation
  ))
})


testthat::test_that("Checking column values fsl_fc_phase - [None/Stress/Crisis/Emergency]", {
  load(testthat::test_path("testdata", "test_df_with_calculation.rda"))

  set.seed(20)
  test_df_with_calculation[sample.int(nrow(test_df_with_calculation), 3), c("fsl_lcsi_cat")] <- "none"
  test_df_with_calculation[sample.int(nrow(test_df_with_calculation), 3), c("fsl_lcsi_cat")] <- "cris"
  test_df_with_calculation[sample.int(nrow(test_df_with_calculation), 3), c("fsl_lcsi_cat")] <- "random_value"

  testthat::expect_error(add_fclcm_phase(
    .dataset = test_df_with_calculation
  ))
})


testthat::test_that("check if function producing expected output with (FCS/RCSI/HHS)", {
  test_df <- expand.grid(
    fsl_fc_phase = c("Phase 1 FC","Phase 2 FC","Phase 3 FC","Phase 4 FC","Phase 5 FC"),
    fsl_lcsi_cat = c("None", "Stress", "Crisis", "Emergency")
  ) |>
    as.data.frame()
  expected_output_success <- test_df |>
    dplyr::mutate(fclcm_phase = dplyr::case_when(is.na(fsl_fc_phase) ~ NA,
                                                 is.na(fsl_lcsi_cat) ~ NA,
                                                 fsl_fc_phase == "Phase 1 FC" & fsl_lcsi_cat %in% c("None", "Stress") ~ "Phase 1 FCLC",
                                                 (fsl_fc_phase == "Phase 1 FC" & fsl_lcsi_cat == "Crisis") |
                                                   fsl_fc_phase == "Phase 2 FC" & fsl_lcsi_cat %in% c("None", "Stress") ~"Phase 2 FCLC",
                                                 (fsl_fc_phase == "Phase 1 FC" & fsl_lcsi_cat == "Emergency") |
                                                   (fsl_fc_phase == "Phase 2 FC" & fsl_lcsi_cat %in% c("Crisis", "Emergency")) |
                                                   (fsl_fc_phase == "Phase 3 FC" & fsl_lcsi_cat %in% c("None", "Stress", "Crisis")) ~ "Phase 3 FCLC",
                                                 (fsl_fc_phase == "Phase 3 FC" & fsl_lcsi_cat == "Emergency") |
                                                   fsl_fc_phase == "Phase 4 FC" ~ "Phase 4 FCLC",
                                                 fsl_fc_phase == "Phase 5 FC" ~ "Phase 5 FCLC",
                                                 TRUE ~ NA))

  testthat::expect_equal(
    add_fclcm_phase(.dataset = test_df),
    expected_output_success
  )
})





