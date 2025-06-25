# Example for Standalone HTML FSL Plausibility Report

# Please adapt with your own dataset and values.
# FSL indicators must be standardized with impactR4PHU before using generating the run_fsl_plaus_html_report function.

rm(list = ls())

library(tidyverse)
library(impactR4PHU)

data.test <- readxl::read_xlsx("2024-08-14 HTI2401 MSNA DEPARTEMENTS Clean recoded data + loops (1).xlsx", sheet = "Clean Data")

data.test2 <- data.test %>%
  dplyr::select(!ends_with("_recoded")) %>%
  impactR4PHU::add_rcsi(fsl_rcsi_mealnb = "fsl_rcsi_mealnb",
                        fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                        fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                        fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                        fsl_rcsi_borrow = "fsl_rcsi_borrow") %>%
  impactR4PHU::add_hhs(
    fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
    fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
    fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
    fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
  ) %>%
  impactR4PHU::add_fcm_phase(
    fcs_column_name = "fsl_fcs_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    hdds_column_name = "fsl_hdds_cat",
    fcs_categories_acceptable = "Acceptable",
    fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    rcsi_categories_low = "No to Low",
    rcsi_categories_medium = "Medium",
    rcsi_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe",
    hdds_categories_low = "Low",
    hdds_categories_medium = "Medium",
    hdds_categories_high = "High"
  )

run_fsl_plaus_html_report(.dataset = data.test2,
                          uuid_var = "uuid", yes_no_team = "no",
                          team_var = "admin2",
                          group_var = "enum_id",
                          output_dir = "reports/", output_file = "testing.html")



