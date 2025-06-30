# Example for Standalone HTML MUAC Plausibility Report

# Please adapt with your own dataset and values.
# MUAC indicators must be standardized with impactR4PHU before using generating the run_muac_plaus_html_report function.

rm(list = ls())

library(tidyverse)
library(impactR4PHU)

# data.test <- readxl::read_xlsx()
data.test <- impactR4PHU::impactR4PHU_data_nut_template

# View(data.test)

data.test2 <- data.test %>%
  impactR4PHU::add_muac(nut_muac_cm = "nut_muac_cm", child_age_months = "child_age_months", child_sex = "child_sex",
                        value_male_sex = "m", edema_confirm = "nut_edema_confirm", value_edema_confirm = "yes") %>%
  impactR4PHU::add_mfaz(nut_muac_cm = "nut_muac_cm", child_age_months = "child_age_months", child_sex = "child_sex",
                        value_male_sex = "m", edema_confirm = "nut_edema_confirm", value_edema_confirm = "yes") %>%
  impactR4PHU::check_anthro_flags(nut_muac_cm = "nut_muac_cm",edema_confirm = "nut_edema_confirm", value_edema_confirm = "yes",
                                  uuid = "child_person_id")




run_muac_plaus_html_report(.dataset = data.test2,
                          uuid_var = "uuid", yes_no_team = "no",
                          team_var = "admin2",
                          group_var = "enum_id",
                          output_dir = "reports/", output_file = "testing.html")



