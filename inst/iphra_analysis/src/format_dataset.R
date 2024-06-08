################################################################################
### FORMAT DATASET
################################################################################

# create shorthands to make working with the data easier:
main <- data.list$main %>% 
  filter(respondent_consent == "yes")
hh_roster <- data.list$hh_roster
ind_health <- data.list$ind_health
water_count_loop <- data.list$water_count_loop
child_nutrition <- data.list$child_nutrition
women <- data.list$women
died_member <- data.list$died_member

################################################################################
# age_group
hh_roster <- hh_roster %>% 
  dplyr::mutate(age_group = as.character(cut(as.numeric(calc_final_age_years), 
                                breaks = c(-1,2,5,10,18,59, Inf),
                                labels = c("00-02", "03-05", "06-10", "11-18",
                                           "19-59","59+"))))

ind_health <- ind_health %>% 
  dplyr::mutate(age_group = as.character(cut(as.numeric(health_ind_age_years), 
                                breaks = c(-1,2,5,10,18,59, Inf),
                                labels = c("00-02", "03-05", "06-10", "11-18",
                                           "19-59","59+"))),
                health_ind_sex = dplyr::case_when(health_ind_sex =="m"~"Male",
                                                  health_ind_sex =="f"~"Female"))

child_nutrition_data <- child_nutrition %>% 
  dplyr::mutate(age_group_nut = as.character(cut(as.numeric(child_age_months), 
                                breaks = c(5,23,59),
                                labels = c("06-23", "24-59"))),
                child_sex = dplyr::case_when(child_sex =="m"~"Male",
                                             child_sex =="f"~"Female"))
women <- women %>% 
  mutate(woman_bf = dplyr::case_when(woman_bf =="bf_child_under_23m"~"Breastfeeding child under 23 month",
                                     woman_bf =="bf_child_under_6m"~"Breastfeeding child under 6 month",
                                     woman_bf =="current_pregnant"~"Currently pregnant",
                                     woman_bf =="none"~"None",
                                     TRUE ~ NA_character_))

################################################################################
# residency_Status

residency_status <- main %>% 
  dplyr::select(uuid,residency_status)
hh_roster <- hh_roster %>% 
  dplyr::left_join(residency_status)

ind_health <- ind_health %>% 
  dplyr::left_join(residency_status)

child_nutrition_data <- child_nutrition_data %>% 
  dplyr::left_join(residency_status)


################################################################################
# unmet_needs

unmet_needs <- ind_health %>% 
  mutate(unmet_needs = case_when(health_ind_received_healthcare == "no"~1,
                                 health_ind_received_healthcare %in% c("yes", "dont_know")~0,
                                 TRUE ~ NA)) %>% 
  group_by(uuid) %>% 
  summarise(unmet_needs = sum(unmet_needs, na.rm = T))

main <- main %>% 
  left_join(unmet_needs) %>% 
  mutate(unmet_needs = ifelse(unmet_needs > 0, "HH with Unmet Needs","HH without unmet Needs"))
################################################################################
# FSL
### Food Security Direct changes
fcs_check_columns <- c("fsl_fcs_cereal",
                       "fsl_fcs_legumes",
                       "fsl_fcs_veg",
                       "fsl_fcs_fruit",
                       "fsl_fcs_meat",
                       "fsl_fcs_dairy",
                       "fsl_fcs_sugar",
                       "fsl_fcs_oil")

if(all(fcs_check_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_fcs(cutoffs = "normal")
}

rcsi_check_columns <- c("fsl_rcsi_lessquality",
                        "fsl_rcsi_borrow",
                        "fsl_rcsi_mealsize",
                        "fsl_rcsi_mealadult",
                        "fsl_rcsi_mealnb")

if(all(rcsi_check_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_rcsi()
}

hhs_check_columns <- c("fsl_hhs_nofoodhh",
                       "fsl_hhs_nofoodhh_freq",
                       "fsl_hhs_sleephungry",
                       "fsl_hhs_sleephungry_freq",
                       "fsl_hhs_alldaynight",
                       "fsl_hhs_alldaynight_freq")

if(all(hhs_check_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_hhs()
}

lcsi_check_columns <- c("fsl_lcsi_stress1",
                        "fsl_lcsi_stress2",
                        "fsl_lcsi_stress3",
                        "fsl_lcsi_stress4",
                        "fsl_lcsi_crisis1",
                        "fsl_lcsi_crisis2",
                        "fsl_lcsi_crisis3",
                        "fsl_lcsi_emergency1",
                        "fsl_lcsi_emergency2",
                        "fsl_lcsi_emergency3")

if(all(lcsi_check_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_lcsi()
}

hdds_check_columns <- c("fsl_hdds_cereals",
                        "fsl_hdds_tubers",
                        "fsl_hdds_veg",
                        "fsl_hdds_fruit",
                        "fsl_hdds_meat",
                        "fsl_hdds_eggs",
                        "fsl_hdds_fish",
                        "fsl_hdds_legumes",
                        "fsl_hdds_dairy",
                        "fsl_hdds_oil",
                        "fsl_hdds_sugar",
                        "fsl_hdds_condiments")

if(all(hdds_check_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_hdds()
}

fcm_check_1_columns <- c("fsl_fcs_score",
                         "fsl_rcsi_score")

fcm_check_2_columns <- c("fsl_hdds_score",
                         "fsl_rcsi_score")

fcm_check_3_columns <- c("fsl_fcs_score",
                         "fsl_hhs_score")

fcm_check_4_columns <- c("fsl_hdds_score",
                         "fsl_hhs_score")

fcm_check_5_columns <- c("fsl_hdds_score",
                         "fsl_rcsi_score",
                         "fsl_hhs_score")

fcm_check_6_columns <- c("fsl_fcs_score",
                         "fsl_rcsi_score",
                         "fsl_hhs_score")




if(all(fcm_check_1_columns %in% names(main)) |
   all(fcm_check_2_columns %in% names(main)) |
   all(fcm_check_3_columns %in% names(main)) |
   all(fcm_check_4_columns %in% names(main)) |
   all(fcm_check_5_columns %in% names(main)) |
   all(fcm_check_6_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_fcm_phase()
}

fclcm_check_columns <- c("fc_phase",
                         "fsl_lcsi_cat")
if(all(fclcm_check_columns %in% names(main))) {
  main <- main %>% 
    impactR4PHU::add_fclcm_phase()
}

################################################################################
## FSL
if(all(hdds_check_columns %in% names(main))) {
  # HDDS
  hdds_table <- data.frame()
  hdds_survey <- srvyr::as_survey_design(main)
  for(i in hdds_check_columns){
    # make a long table:
    res.long <- hdds_survey %>% 
      group_by(!!rlang::sym(i), .add = T) %>% 
      # num_samples here is the actual number of responses for each option in each group
      summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>% 
      mutate(prop = paste0(round(prop,2) *100,"%")) %>% 
      select(-num_samples)
    # widen the table:
    res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                         values_fill = "0%") %>% 
      select(-prop_var) %>% 
      rename(No = "no",
             Yes = "yes") %>% 
      mutate(HDDS = i) %>% 
      relocate(HDDS, .before=1)
    hdds_table <- rbind(hdds_table,res.wide)
  }
  
  # HDDS Cat
  hdds_cat_table <- hdds_survey %>% 
    group_by(fsl_hdds_cat, .add = T) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>% 
    select(-c(prop,prop_var))
  
  # HDDS Score
  hdds_score_table <- hdds_survey %>% 
    group_by() %>% 
    summarise(Mean = srvyr::survey_mean(fsl_hdds_score, na.rm=T, vartype ="ci")) %>% 
    mutate_at(vars(starts_with("Mean")),~round(.,2)) %>% 
    mutate(Variable = "fsl_hdds_score") %>% 
    relocate(Variable, .before = 1) 
}

if(all(hhs_check_columns %in% names(main))) {
  # HHS
  hhs_table <- data.frame()
  hhs_survey <- srvyr::as_survey_design(main)
  hhs_column <- hhs_check_columns[!stringr::str_detect(hhs_check_columns,"_freq")]
  for(i in hhs_column){
    # make a long table:
    res.long <- hhs_survey %>% 
      group_by(!!rlang::sym(i), .add = T) %>% 
      # num_samples here is the actual number of responses for each option in each group
      summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>% 
      mutate(prop = paste0(round(prop,2) *100,"%")) %>% 
      select(-num_samples)
    # widen the table:
    res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                         values_fill = "0%") %>% 
      select(-prop_var) %>% 
      rename(No = "no",
             Yes = "yes") %>% 
      mutate(HHS = i) %>% 
      relocate(HHS, .before=1)
    hhs_table <- rbind(hhs_table,res.wide)
  }
  hhs_tabl_Freq <- data.frame()
  hhs_column_Freq <- hhs_check_columns[stringr::str_detect(hhs_check_columns,"_freq")]
  for(i in hhs_column_Freq){
    # make a long table:
    res.long <- hhs_survey %>% 
      filter(!is.na(!!rlang::sym(i))) %>% 
      group_by(!!rlang::sym(i), .add = T) %>% 
      # num_samples here is the actual number of responses for each option in each group
      summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>% 
      mutate(prop = paste0(round(prop,2) *100,"%")) %>% 
      select(-c(num_samples,prop_var))
    # widen the table:
    res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                         values_fill = "0%") %>% 
      mutate(HHS = i) %>%    
      rename(Often = "often",
             Rarely = "rarely",
             Sometimes = "sometimes") %>% 
      relocate(HHS, .before=1)
    hhs_tabl_Freq <- rbind(hhs_tabl_Freq,res.wide)
  }
  
  hhs_table <- hhs_table %>% 
    left_join(hhs_tabl_Freq %>% mutate(HHS = stringr::str_remove(HHS,"_freq")))
  
  # HHS Cat
  hhs_cat_table <- hhs_survey %>% 
    group_by(fsl_hhs_cat_ipc, .add = T) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>% 
    select(-c(prop,prop_var))
}

# FCS
if(all(fcs_check_columns %in% names(main))) {
  fcs_table <- data.frame()
  fcs_survey <- srvyr::as_survey_design(main)
  for(i in fcs_check_columns){
    # make a long table:
    res.long <- fcs_survey %>% 
      select(!!rlang::sym(i)) %>% 
      group_by() %>% 
      summarise(Mean = srvyr::survey_mean(!!rlang::sym(i),na.rm = T, vartype = "ci"),
                Median = srvyr::survey_median(!!rlang::sym(i),na.rm = T)) %>% 
      mutate_at(vars(starts_with("Mean")),~round(.,2)) %>% 
      select(-Median_se) %>% 
      mutate(Name = i) %>% 
      relocate(Name, .before = 1)
  
    fcs_table <- rbind(fcs_table,res.long)
  }
  
  # HDDS Cat
  fcs_cat_table <- fcs_survey %>% 
    group_by(fsl_fcs_cat, .add = T) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>% 
    select(-c(prop,prop_var))
  
  # HDDS Score
  fcs_score_table <- fcs_survey %>% 
    group_by() %>% 
    summarise(Mean = srvyr::survey_mean(fsl_fcs_score, na.rm=T, vartype ="ci")) %>% 
    mutate_at(vars(starts_with("Mean")),~round(.,2)) %>% 
    mutate(Variable = "fsl_fcs_score") %>% 
    relocate(Variable, .before = 1) 
}

if(all(rcsi_check_columns %in% names(main))) {
  # RCSI
  rcsi_table <- data.frame()
  rcsi_survey <- srvyr::as_survey_design(main)
  for(i in rcsi_check_columns){
    # make a long table:
    res.long <- rcsi_survey %>% 
      select(!!rlang::sym(i)) %>% 
      group_by() %>% 
      summarise(Mean = srvyr::survey_mean(!!rlang::sym(i),na.rm = T, vartype = "ci"),
                Median = srvyr::survey_median(!!rlang::sym(i),na.rm = T)) %>% 
      mutate_at(vars(starts_with("Mean")),~round(.,2)) %>% 
      select(-Median_se) %>% 
      mutate(Name = i) %>% 
      relocate(Name, .before = 1)
    
    rcsi_table <- rbind(rcsi_table,res.long)
  }
  
  # HDDS Cat
  rcsi_cat_table <- rcsi_survey %>% 
    filter(!is.na(fsl_rcsi_cat)) %>% 
    group_by(fsl_rcsi_cat) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>% 
    select(-c(prop,prop_var))
  
  # HDDS Score
  rcsi_score_table <- rcsi_survey %>% 
    group_by() %>% 
    summarise(Mean = srvyr::survey_mean(fsl_rcsi_score, na.rm=T, vartype ="ci")) %>% 
    mutate_at(vars(starts_with("Mean")),~round(.,2)) %>% 
    mutate(Variable = "fsl_rcsi_score") %>% 
    relocate(Variable, .before = 1) 
}

if(all(lcsi_check_columns %in% names(main))) {
  # LCSI
  lcsi_table <- data.frame()
  lcsi_survey <- srvyr::as_survey_design(main)
  for(i in lcsi_check_columns){
    # make a long table:
    res.long <- lcsi_survey %>% 
      group_by(!!rlang::sym(i), .add = T) %>% 
      # num_samples here is the actual number of responses for each option in each group
      summarise(num_samples = n(), ## to review later stages survey_total(na.rm = T, vartype = "var")
                prop = srvyr::survey_prop(na.rm = T, vartype = "var")) %>% 
      mutate(prop = paste0(round(prop,2) *100,"%"))%>% 
      select(-c(num_samples,prop_var))
    # widen the table:
    res.wide <- res.long %>% tidyr::pivot_wider(names_from = !!rlang::sym(i), values_from = c(prop),
                                         values_fill = "0%") %>% 
      mutate(LCSI = i) %>% 
      relocate(LCSI, .before=1)
    lcsi_table <- rbind(lcsi_table,res.wide)
  }
  
  # HDDS Cat
  lcsi_cat_table <- lcsi_survey %>% 
    group_by(fsl_lcsi_cat, .add = T) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>% 
    select(-c(prop,prop_var))
}
if(all(fcm_check_1_columns %in% names(main)) |
   all(fcm_check_2_columns %in% names(main)) |
   all(fcm_check_3_columns %in% names(main)) |
   all(fcm_check_4_columns %in% names(main)) |
   all(fcm_check_5_columns %in% names(main)) |
   all(fcm_check_6_columns %in% names(main))) {
  # FC_PHASE
  fc_phase_table <- lcsi_survey %>% 
    filter(!is.na(fc_phase)) %>% 
    group_by(fc_phase, .add = T) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,2) *100,"%")) %>% 
    select(-c(prop,prop_var))
}

if(all(fclcm_check_columns %in% names(main))) {
  # FCLCM_PHASE
  fclcm_phase_table <- lcsi_survey %>% 
    filter(!is.na(fclcm_phase)) %>% 
    group_by(fclcm_phase, .add = T) %>% 
    summarise(num_samples = n(), 
              prop = srvyr::survey_prop(na.rm = T, vartype = "var"))%>% 
    mutate(Percentage = paste0(round(prop,3) * 100,"%")) %>% 
    select(-c(prop,prop_var))
}

################################################################################
# Mortality
if(!is.null(died_member)){
  mortality <- check_mortality_flags(main,
                                     hh_roster,
                                     died_member)
  mortality_data <- mortality$crude %>% 
    mutate(fpc = nrow(hh_roster)/as.numeric(strings['population_estimation'])) %>% 
    mutate(person_time= pt_total + pt_died + pt_left - pt_birth - pt_join,
           person_time_under5 = pt_total_under5 + pt_died_under5)
  
  if(as.numeric(strings['population_estimation']) > 10000){
    res <- srvyr::as_survey_design(mortality_data)
  } else {
    res <- srvyr::as_survey_design(mortality_data,
                            fpc = fpc)
  }
  
  crude <- res %>%
    srvyr::summarise(x = srvyr::survey_ratio(numerator = num_died * 10000,
                                             denominator = person_time,
                                             vartype = "ci",
                                             deff = "replace")) %>% 
    mutate_all(.,~round(.,2)) %>%
    mutate(variable = "cmr")
  
  
  under5 <- res %>%
    srvyr::summarise(x = srvyr::survey_ratio(numerator = num_died_under5 * 10000,
                                             denominator = person_time_under5,
                                             vartype = "ci",
                                             deff = "replace")) %>% 
    mutate_all(.,~round(.,2)) %>%
    mutate(variable = "u5mr")
  
  results.summary <- rbind(crude,under5) %>% 
    select(-x_deff) %>% 
    rename(`point.est` = "x",
           `95%lci` = "x_low",
           `95%uci` = "x_upp") %>% 
    relocate(variable, .before = 1)
  
  mortality_data_sex <- mortality$crude %>% 
    mutate(fpc = nrow(data.list$hh_roster)/as.numeric(strings['population_estimation'])) %>% 
    mutate(person_time_m = pt_total_m + pt_died_m - pt_birth_m,
           person_time_f = pt_total_f + pt_died_f - pt_birth_f,
           person_time_under5_m = pt_total_under5_m + pt_died_under5_m,
           person_time_under5_f = pt_total_under5_f + pt_died_under5_f)
  
  if(as.numeric(strings['population_estimation']) > 10000){
    res_sex <- srvyr::as_survey_design(mortality_data_sex)
  } else {
    res_sex <- srvyr::as_survey_design(mortality_data_sex,
                            fpc = fpc)
  }
  
  crude_m <- res_sex %>%
    srvyr::summarise(x = srvyr::survey_ratio(numerator = num_died_m * 10000,
                                             denominator = person_time_m,
                                             vartype = "ci",
                                             deff = "replace")) %>% 
    mutate_all(.,~round(.,2)) %>%
    mutate(variable = "cmr",
           Sex = "Male")
  
  crude_f <- res_sex %>%
    srvyr::summarise(x = srvyr::survey_ratio(numerator = num_died_f * 10000,
                                             denominator = person_time_f,
                                             vartype = "ci",
                                             deff = "replace")) %>% 
    mutate_all(.,~round(.,2)) %>%
    mutate(variable = "cmr",
           Sex = "Female")
  
  under5_m <- res_sex %>%
    srvyr::summarise(x = srvyr::survey_ratio(numerator = num_died_under5_m * 10000,
                                             denominator = person_time_under5_m,
                                             vartype = "ci",
                                             deff = "replace")) %>% 
    mutate_all(.,~round(.,2)) %>%
    mutate(variable = "u5mr",
           Sex = "Male")
  
  under5_f <- res_sex %>%
    srvyr::summarise(x = srvyr::survey_ratio(numerator = num_died_under5_f * 10000,
                                             denominator = person_time_under5_f,
                                             vartype = "ci",
                                             deff = "replace")) %>% 
    mutate_all(.,~round(.,2)) %>%
    mutate(variable = "u5mr",
           Sex = "Female")
  
  results.summary_bySex <- rbind(crude_m,
                                 crude_f,
                                 under5_m,
                                 under5_f) %>% 
    select(-x_deff) %>% 
    rename(`point.est` = "x",
           `95%lci` = "x_low",
           `95%uci` = "x_upp") %>% 
    relocate(variable, .before = 1)
}


################################################################################
# NUT
nut <- check_nut_flags(child_nutrition)


################################################################################
# Water
if(!is.null(water_count_loop)){
  raw.water_count_loop <- water_count_loop
  water <- main %>% 
    check_WASH_flags(is.loop = T)
main <- main %>% 
  left_join(select(water,c(uuid, litre_per_day_per_person)))
} 


data.list$main <- main
data.list$hh_roster <- hh_roster
data.list$ind_health <- ind_health
data.list$water_count_loop <- water_count_loop
data.list$child_nutrition <- child_nutrition_data
data.list$women <- women
data.list$died_member <- died_member

