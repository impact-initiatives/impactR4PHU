# Example for Standalone HTML FSL Plausibility Report

# Please adapt with your own dataset and values.
# FSL indicators must be standardized with impactR4PHU before using generating the run_fsl_plaus_html_report function.

rm(list = ls())

library(tidyverse)
library(impactR4PHU)

# Set Parameters

loop_var <- "admin1"
grouping_var <- "enum_id"
uuidVar <- "_uuid"

# Add FSL Indicators for Plausibility

data.test <- readxl::read_xlsx("")

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
  impactR4PHU::add_fcs(cutoffs = "normal",fsl_fcs_cereal = "fsl_fcs_cereal", fsl_fcs_legumes = "fsl_fcs_legumes",
                       fsl_fcs_veg = "fsl_fcs_veg", fsl_fcs_fruit = "fsl_fcs_fruit", fsl_fcs_meat = "fsl_fcs_meat", fsl_fcs_dairy = "fsl_fcs_dairy",
                       fsl_fcs_sugar = "fsl_fcs_sugar", fsl_fcs_oil = "fsl_fcs_oil") %>%
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
  ) %>%
  impactR4PHU::add_lcsi(
    fsl_lcsi_stress1 = "fsl_lcsi_stress1", fsl_lcsi_stress2 = "fsl_lcsi_stress2" , fsl_lcsi_stress3 = "fsl_lcsi_stress3", fsl_lcsi_stress4 = "fsl_lcsi_stress4",
    fsl_lcsi_crisis1 = "fsl_lcsi_crisis1" , fsl_lcsi_crisis2 = "fsl_lcsi_crisis2", fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
    fsl_lcsi_emergency1 = "fsl_lcsi_emergency1", fsl_lcsi_emergency2 = "fsl_lcsi_emergency2", fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
    yes_val = "yes" , no_val = "no_had_no_need", exhausted_val = "no_exhausted" , not_applicable_val = "not_applicable"
  )
# %>%
#   dplyr::rename("fsl_lcsi_stress1" = "fsl_lcsi_stress_1", "fsl_lcsi_stress2" = "fsl_lcsi_stress_2" , "fsl_lcsi_stress3" = "fsl_lcsi_stress_3", "fsl_lcsi_stress4" = "fsl_lcsi_stress_4",
#                 "fsl_lcsi_crisis1" = "fsl_lcsi_crisis_1" , "fsl_lcsi_crisis2" = "fsl_lcsi_crisis_2", "fsl_lcsi_crisis3" = "fsl_lcsi_crisis_3",
#                 "fsl_lcsi_emergency1" = "fsl_lcsi_emergency_1", "fsl_lcsi_emergency2" = "fsl_lcsi_emergency_2", "fsl_lcsi_emergency3" = "fsl_lcsi_emergency_3")


loop_values <- unique(data.test2[[loop_var]])

for (i in 1:length(loop_values)) {

  print(loop_values[[i]])

  dir.create("reports/")

  file_name <- paste0("fsl_plaus_report_", loop_values[[i]], "_", Sys.Date(), ".html")

  data.test3 <- data.test2 %>% dplyr::filter(!!sym(loop_var) == loop_values[[i]])

  run_fsl_plaus_html_report(.dataset = data.test3,
                            uuid_var = uuidVar,
                            group_var = grouping_var,
                            output_dir = "reports/", output_file = file_name)

}




