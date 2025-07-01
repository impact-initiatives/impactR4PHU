
# Requirements for Running This Script

# 1. Your data must have columns for teams and enumerators, if you want to do enumerator level quality checks.

# 2. Your data must have the following column names at least in the various main, roster, and deaths datasets.
  # list_main_columns <- c( "today", "recall_date", "start", "end")
  # list_roster_columns <- c("ind_gender", "calc_final_age_years", "final_ind_dob",)
  # list_deaths_columns <- c("sex_died","calc_final_age_years_died", "dob_died", "final_date_death", "calc_final_age_years_died")

# 3. You must 'pre-filter' your data by region or team before running the rmarkdown function. Best done with a for loop to iterate through each team.

# 4. You must pre-determine your 'expected' demographic values for some of the statistical tests. STU can support with this step.

#clear environment
rm(list = ls())

#load libraries for tidyverse, ggplot2, and dplyr
library(tidyverse)
library(ggplot2)
library(dplyr)
library(flextable)

# Step 2. Load your datasets into a main, roster, and deaths dataframe. These should be somewhat standardized with mortality modules from IMPACT HQ.

# list_car_data <- readRDS("data/msna/CAR_2025.RDS")
#
# df_main <- list_car_data$main_data
# df_roster <- list_car_data$roster
# df_died <- list_car_data$died_member

# list_car_data <- readRDS("data/msna/CAR_2025.RDS")

df_main <- readxl::read_xlsx("inputs/HTI2502 download data - 2025-06-30.xlsx", sheet = "main")

df_roster <- readxl::read_xlsx("inputs/HTI2502 download data - 2025-06-30.xlsx", sheet = "roster") %>%
  dplyr::rename(final_ind_dob = ind_dob_final,
                calc_final_age_years = ind_under5_age_years)

df_died <- readxl::read_xlsx("inputs/HTI2502 download data - 2025-06-30.xlsx", sheet = "died_member")

# Step 3. Load some additional functions used within the rmarkdown.

source("src/functions_extra.R")

# 4. Set your parameters

uuid_main <- "_index"
uuid_roster <- "_parent_index"
uuid_deaths <- "_parent_index"

lang <- "en" # en / fr
loop_var <- "zone"
grouping_var <- "admin1"
file_path <- "reports"
start_date = "2024-06-01"
end_date = "2026-09-30"

exp_sexRatio <- 0.048 # Estimated sex ratio, assuming 50% male.
exp_ageRatio_01_35 <- 0.42 # Estimated proportion of children under-2 out of all under-5 children.
exp_ageRatio_05_10 <- 0.51 # Estimated proportion of children under-5 out of all under-10 children.
exp_ageRatio_05_5plus <- 0.15 # Estimated proportion of children under-5 out of all people.
exp_meanHH_size <- 4.4 # Estimated average household size.

exp_deathsPer_hh <- 0.057816 # Estimated number of deaths per household, based on estimated CDR, length of recall period, and household size
# Expected deaths per household = CDR * Average HH Size * Number recall days / 10000
# (a <- (0.73*4.4*180) / 10000 )
# 0.057816

exp_birthsPer_hh <- 0.05 # Estimated number of births per household, based on estimated Birth Rate, length of recall period, and household size
# Expected births per household = Births per 1000 people per year * (Number recall days / 365 days per year) * Avg household size
# (b <- (22.2*180*4.4)/(365*1000))
# 5. Iterate through your groups to produce pdf reports for each. Set the file naming conventions and output folder.

loop_values <- unique(df_main[[loop_var]])

df_main <- df_main %>%
  rename(hh_uuid = uuid_main) %>%
  dplyr::mutate(hh_uuid = as.character(hh_uuid))

df_roster <- df_roster %>%
  rename(hh_uuid = uuid_roster) %>%
  dplyr::mutate(hh_uuid = as.character(hh_uuid))

df_died <- df_died %>%
  rename(hh_uuid = uuid_deaths) %>%
  dplyr::mutate(hh_uuid = as.character(hh_uuid))

dir.create(paste0(file_path, "/", Sys.Date()))

for (i in 1:length(loop_values)) {

  print(loop_values[[i]])

  file_name <- paste0("mortality_quality_report_", loop_values[[i]], "_", Sys.Date(), ".html")

  print(paste0("1 ncol: ", nrow(df_main)))

  df_main2 <- df_main %>%
    dplyr::filter(!!sym(loop_var) == loop_values[[i]]) %>%
    dplyr::filter(today >= as.Date(start_date) & today <= as.Date(end_date))

  print(nrow(df_main2))

  # df_main3 <- df_main2 %>%
  #   dplyr::filter(today >= as.Date(params$startDate) & today <= as.Date(params$endDate))

  print(paste0("2 ncol: ", nrow(df_main2)))

  df_roster2 <- df_roster %>% dplyr::filter(hh_uuid %in% df_main$hh_uuid)
  df_died2 <- df_died %>% dplyr::filter(hh_uuid %in% df_main$hh_uuid)

  render_quality_report(main_data = df_main2,
                        roster_data = df_roster2,
                        deaths_data = df_died2,
                        lang = "fr",
                        start_date = start_date,
                        end_date = end_date,
                        group_var = grouping_var,
                        exp_sexRatio = exp_sexRatio,
                        exp_ageRatio_01_35 = exp_ageRatio_01_35,
                        exp_ageRatio_05_10 = exp_ageRatio_05_10,
                        exp_ageRatio_05_5plus = exp_ageRatio_05_5plus,
                        exp_meanHH_size = exp_meanHH_size,
                        exp_deathsPer_hh = exp_deathsPer_hh,
                        exp_birthsPer_hh = exp_birthsPer_hh,
                        output_file = file_name,
                        output_dir = paste0(file_path, "/", Sys.Date()))

}


#
# # Test specific cases CF533 CF224
#
# df_main2 <- df_main %>% dplyr::filter(admin2 == "CF533")
# df_roster2 <- df_roster %>% dplyr::filter(hh_uuid %in% df_main2$hh_uuid)
# df_died2 <- df_died %>% dplyr::filter(hh_uuid %in% df_main2$hh_uuid)
#
# file_name <- paste0("mortality_quality_report_test.pdf")
#
# render_quality_report(main_data = df_main2,
#                       roster_data = df_roster2,
#                       deaths_data = df_died2,
#                       lang = "fr",
#                       start_date = start_date,
#                       end_date = end_date,
#                       group_var = grouping_var,
#                       exp_sexRatio = exp_sexRatio,
#                       exp_ageRatio_01_35 = exp_ageRatio_01_35,
#                       exp_ageRatio_05_10 = exp_ageRatio_05_10,
#                       exp_ageRatio_05_5plus = exp_ageRatio_05_5plus,
#                       exp_meanHH_size = exp_meanHH_size,
#                       exp_deathsPer_hh = exp_deathsPer_hh,
#                       exp_birthsPer_hh = exp_birthsPer_hh,
#                       output_file = file_name,
#                       output_dir = paste0(file_path, "/", Sys.Date()))
#
#
#
# # What cleaning scripts are needed for mortality
#
# # 1. Cluster classification checks...
# # 2. Checks on same population group per cluster...
#
# # 3. Flagging suspect deaths with logical issues for cleaning
# # 4. Deleting or removing deaths outside of recall period
# # 5. Cleaning or flagging incorrect dates, birthdates or survey dates or dates of death
#
# # 6. Checking loop counts between roster/deaths and main
# # 7. Recoding cause of deaths from other deaths
#
#
#
#
#
#
#
#
#
#
#
#
#
