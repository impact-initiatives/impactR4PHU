################################################################################
#                                FUNCTIONS TO CHECK                            #
################################################################################
check_fs_flags <- function(.dataset,
                           date_dc_date = "today",
                           fcs_cereal = "fsl_fcs_cereal",
                           fcs_legumes = "fsl_fcs_legumes",
                           fcs_dairy = "fsl_fcs_dairy",
                           fcs_meat = "fsl_fcs_meat",
                           fcs_veg = "fsl_fcs_veg",
                           fcs_fruit = "fsl_fcs_fruit",
                           fcs_oil = "fsl_fcs_oil",
                           fcs_sugar = "fsl_fcs_sugar",
                           rcsi_lessquality = "fsl_rcsi_lessquality",
                           rcsi_borrow = "fsl_rcsi_borrow",
                           rcsi_mealsize = "fsl_rcsi_mealsize",
                           rcsi_mealadult = "fsl_rcsi_mealadult",
                           rcsi_mealnb = "fsl_rcsi_mealnb",
                           hhs_nofoodhh = "fsl_hhs_nofoodhh",
                           hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
                           hhs_sleephungry = "fsl_hhs_sleephungry",
                           hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
                           hhs_alldaynight = "fsl_hhs_alldaynight",
                           hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
                           hdds_cereals = "fsl_hdds_cereals",
                           hdds_tubers = "fsl_hdds_tubers",
                           hdds_legumes = "fsl_hdds_legumes",
                           hdds_veg = "fsl_hdds_veg",
                           hdds_fruit = "fsl_hdds_fruit",
                           hdds_meat = "fsl_hdds_meat",
                           hdds_fish = "fsl_hdds_fish",
                           hdds_dairy = "fsl_hdds_dairy",
                           hdds_eggs = "fsl_hdds_eggs",
                           hdds_sugar = "fsl_hdds_sugar",
                           hdds_oil = "fsl_hdds_oil",
                           hdds_condiments = "fsl_hdds_condiments",
                           lcsi_stress1 = "fsl_lcsi_stress1",
                           lcsi_stress2 = "fsl_lcsi_stress2",
                           lcsi_stress3 = "fsl_lcsi_stress3",
                           lcsi_stress4 = "fsl_lcsi_stress4",
                           lcsi_crisis1 = "fsl_lcsi_crisis1",
                           lcsi_crisis2 = "fsl_lcsi_crisis2",
                           lcsi_crisis3 = "fsl_lcsi_crisis3",
                           lcsi_emergency1 = "fsl_lcsi_emergency1",
                           lcsi_emergency2 = "fsl_lcsi_emergency2",
                           lcsi_emergency3 = "fsl_lcsi_emergency3",
                           lcsi_stress = "fsl_lcsi_stress",
                           lcsi_crisis = "fsl_lcsi_crisis",
                           lcsi_emergency = "fsl_lcsi_emergency",
                           lcsi_cat_yes = "fsl_lcsi_cat_yes",
                           lcsi_cat_exhaust = "fsl_lcsi_cat_exhaust",
                           lcsi_cat = "fsl_lcsi_cat",
                           fcs_cat ="fsl_fcs_cat",
                           fcs_score = "fsl_fcs_score",
                           rcsi_cat = "fsl_rcsi_cat",
                           rcsi_score = "fsl_rcsi_score",
                           hhs_cat = "fsl_hhs_cat",
                           hhs_score = "fsl_hhs_score",
                           hdds_cat = "fsl_hdds_cat",
                           hdds_score = "fsl_hdds_score",
                           fc_cell = "fc_cell",
                           fc_phase = "fc_phase",
                           num_children = "num_children",
                           enumerator = "enumerator",
                           uuid = "uuid") {
  # change df into dataframe
  .dataset <- as.data.frame(.dataset)
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }

  results <- .dataset %>% 
    dplyr::select(uuid, enumerator,date_dc_date)%>% 
    rename(date_dc_date = date_dc_date) %>% 
    mutate(date_dc_date = lubridate::as_date(as.numeric(date_dc_date),origin = "1899-12-30"))
  
  # combine all fcs_columns together
  fcs_flag_columns <- c(fcs_cereal,fcs_legumes,fcs_dairy,fcs_meat,fcs_veg,fcs_fruit,fcs_oil,fcs_sugar,fcs_score)
  
  if(all(fcs_flag_columns %in% colnames(.dataset))) {
    ## flag issues in data with FCS
    results2 <- .dataset %>%
      dplyr::mutate_at(vars(fcs_flag_columns),as.numeric)%>% 
      dplyr::mutate(flag_meat_cereal_ratio = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, ifelse(!!rlang::sym(fcs_cereal) < fcs_meat, 1, 0)),
                    flag_low_cereal = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, ifelse(!!rlang::sym(fcs_cereal) < 5, 1, 0)),
                    flag_low_fcs = ifelse(is.na(!!rlang::sym(fcs_score)),NA, ifelse(!!rlang::sym(fcs_score)<=10,1,0)),
                    flag_high_fcs = ifelse(is.na(!!rlang::sym(fcs_score)),NA, ifelse(!!rlang::sym(fcs_score)>=56,1,0)),
                    flag_low_oil = ifelse(is.na(!!rlang::sym(fcs_cereal)), NA, ifelse(!!rlang::sym(fcs_oil) < 5, 1, 0))) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(sd_foods = sd(c(!!rlang::sym(fcs_cereal), !!rlang::sym(fcs_legumes), !!rlang::sym(fcs_dairy),
                                    !!rlang::sym(fcs_meat), !!rlang::sym(fcs_veg), !!rlang::sym(fcs_fruit), 
                                    !!rlang::sym(fcs_oil), !!rlang::sym(fcs_sugar)), na.rm = TRUE),
                    flag_sd_foodgroup = dplyr::case_when(sd_foods < 0.8 ~ 1,
                                                         .default = 0,
                                                         TRUE ~ NA)) %>%
      dplyr::ungroup() %>% 
      dplyr::select(fcs_flag_columns,
                    fcs_cat,
                    flag_meat_cereal_ratio,
                    flag_low_cereal,
                    flag_low_oil,
                    flag_low_fcs,
                    flag_high_fcs,
                    flag_sd_foodgroup)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  } 
  ## flag issues in data with rCSI
  
  rcsi_flag_columns <- c(rcsi_lessquality,rcsi_borrow,rcsi_mealsize,rcsi_mealadult,rcsi_mealnb,rcsi_score)
  if(all(rcsi_flag_columns %in% names(.dataset)) & num_children %in% names(.dataset)){
    results2 <- .dataset %>% 
      dplyr::mutate_at(vars(rcsi_flag_columns),as.numeric)%>% 
      dplyr::mutate(flag_protein_rcsi = ifelse(is.na(!!rlang::sym(rcsi_score)), NA,
                                               ifelse(is.na(!!rlang::sym(fcs_cereal)), NA,
                                                      ifelse(!!rlang::sym(rcsi_score) >= 19 & ( !!rlang::sym(fcs_dairy) >= 5 | !!rlang::sym(fcs_meat) >= 5), 1, 0 ))),
                    flag_fcs_rcsi = ifelse(is.na(!!rlang::sym(rcsi_score)), NA,
                                           ifelse(is.na(!!rlang::sym(fcs_score)), NA,
                                                  ifelse(!!rlang::sym(fcs_score) < 35 & !!rlang::sym(rcsi_score) <= 4, 1, 0 ))),
                    flag_high_rcsi = ifelse(is.na(!!rlang::sym(rcsi_score)), NA, ifelse(!!rlang::sym(rcsi_score) >= 43, 1, 0)),
                    flag_rcsi_children = ifelse(is.na(!!rlang::sym(rcsi_mealadult)), NA, ifelse(!is.na(!!rlang::sym(rcsi_mealadult)) & as.numeric(num_children) == 0, 1,0)),
                    flag_fcsrcsi_box = dplyr::case_when(as.numeric(!!rlang::sym(rcsi_score)) > 18 & as.numeric(!!rlang::sym(fcs_score)) > 56 ~ 1, .default = 0,
                                                        TRUE ~ NA)) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(sd_rcsicoping = sd(c(!!rlang::sym(rcsi_lessquality), !!rlang::sym(rcsi_borrow), !!rlang::sym(rcsi_mealsize),
                                         !!rlang::sym(rcsi_mealadult), !!rlang::sym(rcsi_mealnb)), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(flag_sd_rcsicoping = dplyr::case_when(sd_rcsicoping < 0.8 & !!rlang::sym(rcsi_score) < 4 ~ 1, .default = 0, TRUE ~ NA)) %>% 
      dplyr::select(rcsi_flag_columns,rcsi_cat,flag_protein_rcsi,flag_fcs_rcsi,flag_high_rcsi,flag_rcsi_children,flag_fcsrcsi_box,flag_sd_rcsicoping)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag issue in data with HHS
  hhs_flag_columns <- c(hhs_nofoodhh,hhs_nofoodhh_freq,hhs_sleephungry,
                        hhs_sleephungry_freq,hhs_alldaynight,hhs_alldaynight_freq,hhs_score,hhs_cat)
  if(all(hhs_flag_columns %in% names(.dataset))){
    results2 <- .dataset %>% 
      dplyr::mutate(flag_severe_hhs = ifelse(is.na(!!rlang::sym(hhs_score)), NA, ifelse(!!rlang::sym(hhs_score) >= 5, 1, 0))) %>% 
      dplyr::select(hhs_flag_columns,flag_severe_hhs)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag issues with LCSI
  lcs_flag_columns <- c(lcsi_stress1,lcsi_stress2,lcsi_stress3,lcsi_stress4,lcsi_crisis1,lcsi_crisis2,
                        lcsi_crisis3,lcsi_emergency1,lcsi_emergency2,lcsi_emergency3,lcsi_stress,
                        lcsi_crisis,lcsi_emergency,lcsi_cat_yes,lcsi_cat_exhaust,lcsi_cat)
  
  if(all(lcs_flag_columns %in% names(.dataset))){
    results2 <- .dataset %>% 
      dplyr::mutate(flag_lcsi_coherence = ifelse(is.na(!!rlang::sym(lcsi_emergency)), NA,
                                                 ifelse(!!rlang::sym(lcsi_emergency) == 1 & !!rlang::sym(lcsi_stress) == 0 | 
                                                          !!rlang::sym(lcsi_emergency) == 1 & !!rlang::sym(lcsi_crisis) == 0 |
                                                          !!rlang::sym(lcsi_crisis) == 1 & !!rlang::sym(lcsi_stress) == 0, 1, 0)),
                    flag_lcsi_severity = dplyr::case_when(!!rlang::sym(lcsi_emergency) == 1 ~ 1, .default = 0,
                                                          TRUE ~ NA))
    
    lcs_variables <- c("fsl_lcsi_stress1","fsl_lcsi_stress2","fsl_lcsi_stress3","fsl_lcsi_stress4","fsl_lcsi_crisis1",
                       "fsl_lcsi_crisis2","fsl_lcsi_crisis3","fsl_lcsi_emergency1","fsl_lcsi_emergency2","fsl_lcsi_emergency3")
    results2$lcsi.count.na <-  apply(results2[c(lcs_variables)], 1, function(x) sum(x == "not_applicable"))
    
    results2 <- results2 %>% 
      dplyr::mutate(flag_lcsi_na = dplyr::case_when(lcsi.count.na == 10 ~ 1, .default = 0, TRUE ~ NA)) 
    
    income_types <- c("first_income_types","second_income_types","third_income_types")
    suppressWarnings(
      agric <- lcs_variables[which(grepl("agriculture|crop|crops|farm",get.label(lcs_variables)))]
    )
    
    suppressWarnings(
      livest <- lcs_variables[which(grepl("livestock|livestocks|animal",get.label(lcs_variables)))]
      
    )
    
    suppressWarnings(
      displ <- lcs_variables[which(grepl("displaced|migration|migrated",get.label(lcs_variables)))]
    )
    
    if(length(agric)>0){
      results2$flag_lcsi_liv_agriculture <- dplyr::case_when(rowSums(sapply(results2[agric], function(i) grepl("yes",i))) > 0 & any(results2[income_types] == "sell_agri_prod") > 0  ~ 1, .default = 0, TRUE ~ NA) ## Fix second part to take only select_one from three columns
    }
    
    if(length(livest)>0){
      results2$flag_lcsi_liv_livestock  <- dplyr::case_when(rowSums(sapply(results2[livest], function(i) grepl("yes",i))) > 0 & any(results2[income_types] == "sell_anim_prod") > 0 ~ 1, .default = 0, TRUE ~ NA) ## Fix second part to take only select_one from three columns
    }
    
    if(length(displ)>0){
      results2$flag_lcsi_displ  <- dplyr::case_when(rowSums(sapply(results2[displ], function(i) grepl("yes",i))) > 0 & results2["residency_status"] == "idp" ~ 1, .default = 0, TRUE ~ NA) ## Fix second part to take only select_one from three columns
    }
    
    if(length(livest)>0 & length(agric)>0 & length(displ)>0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture,flag_lcsi_liv_livestock,flag_lcsi_displ)
    } else if (length(livest)>0 & length(agric)>0 & length(displ) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_livestock,flag_lcsi_liv_agriculture)      
    } else if (length(agric)>0 & length(displ)>0 & length(livest) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture,flag_lcsi_displ)   
    } else if (length(displ)>0 & length(livest)>0 & length(agric) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_displ,flag_lcsi_liv_livestock)   
    } else if (length(livest)>0 & length(agric) ==0 & length(displ) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_livestock)      
    } else if (length(agric)>0 & length(livest) == 0 & length(displ) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_liv_agriculture)   
    } else if (length(displ)>0 & length(livest) == 0 & length(agric) == 0){
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na,flag_lcsi_displ)   
    }  else {
      results2 <- results2 %>% 
        dplyr::select(lcs_flag_columns,flag_lcsi_coherence,flag_lcsi_severity,flag_lcsi_na)   
    }
  
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  fc_phase_col <- c(fc_cell,fc_phase)
  if(all(fc_phase_col %in% names(.dataset))){
    ## flag phase
    results2 <- .dataset %>% 
      dplyr::mutate(flag_fc_cell = ifelse(is.na(fc_cell), NA,
                                          ifelse(fc_cell %in% c(3,4,5,8,9,10), 1, 0))) %>% 
      select(fc_phase_col, flag_fc_cell)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  ## flag hhds 
  hdds_flag_columns <- c(hdds_cereals,hdds_tubers,hdds_legumes,hdds_veg,hdds_fruit,
                         hdds_meat,hdds_fish,hdds_dairy,hdds_eggs,hdds_sugar,
                         hdds_oil,hdds_condiments,hdds_cat,hdds_score) 
  if(all(hdds_flag_columns %in% names(.dataset))) {
    results2 <- .dataset %>% 
      dplyr::mutate(flag_low_sugar_cond_hdds = ifelse(is.na(!!rlang::sym(hdds_score)), NA,
                                                      ifelse((!!rlang::sym(hdds_score) <= 2 & !!rlang::sym(hdds_sugar) == "yes" & !!rlang::sym(hdds_condiments) == "yes") | 
                                                               (!!rlang::sym(hdds_score) <= 1 & !!rlang::sym(hdds_sugar) == "yes") |
                                                               (!!rlang::sym(hdds_score) <= 1 & !!rlang::sym(hdds_condiments) == "yes"), 1, 0))) %>% 
      dplyr::select(hdds_flag_columns,flag_low_sugar_cond_hdds)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  options(warn = 0)
  return(results)
}

## Function to check WATER CONSUMPTION Flags

check_WASH_flags <- function(.dataset,
                             date_dc_date = "today",
                             containers = "wash_containers",
                             container_type = "wash_container_type",
                             container_litre_other = "wash_container_litre_other",
                             container_journey_collection = "wash_container_journey_collection",
                             num_containers = "wash_num_containers",
                             water_source = "wash_on_premise_rain_water",
                             water_collect_time = "wash_water_collect_time",
                             num_hh = "num_hh",
                             enumerator = "enumerator",
                             uuid = "uuid",
                             is.loop = T) {
  # change df into dataframe
  
  .dataset <- as.data.frame(.dataset)
  # change df into dataframe
  if(is.loop){
    data_container_loop <- as.data.frame(raw.water_count_loop)
  }
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  if(is.loop){
    ## Throw an error if the dataset is empty
    if (nrow(data_container_loop) == 0) {
      stop("raw.water_count_loop is empty")
    }
  }
  if(date_dc_date == "start"){
    results <- .dataset %>% 
      dplyr::select(uuid, enumerator,date_dc_date)%>% 
      rename(date_dc_date = date_dc_date) %>% 
      mutate(date_dc_date = lubridate::as_date(date_dc_date))
  } else {
    results <- .dataset %>% 
      dplyr::select(uuid, enumerator,date_dc_date)%>% 
      rename(date_dc_date = date_dc_date) %>% 
      mutate(date_dc_date = lubridate::as_date(as.numeric(date_dc_date),origin = "1899-12-30"))
  }
  if(is.loop){
    ## calculate liters per person per day
    calculate_data_container_loop <- data_container_loop %>% 
      dplyr::rowwise() %>% 
      mutate(container_type_litre = stringr::str_remove(stringr::str_extract(!!rlang::sym(container_type), "([^\\__]+$)"), "l"),
             litre = ifelse(!!rlang::sym(container_type) == "other", as.numeric(!!rlang::sym(container_litre_other)),as.numeric(container_type_litre)),
             litre_per_day = ifelse(is.na(!!rlang::sym(container_journey_collection)), litre, litre * as.numeric(!!rlang::sym(container_journey_collection)))) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(uuid) %>% 
      dplyr::summarise(litre_per_day_per_hh = sum(litre_per_day))
    
    results2 <- .dataset %>% 
      dplyr::left_join(calculate_data_container_loop) %>% 
      dplyr::mutate(litre_per_day_per_person = litre_per_day_per_hh / as.numeric(!!rlang::sym(num_hh)))
      
    ## FLAGS (Litres per person per day)
    mean_litre_dataset <-  mean(results2$litre_per_day_per_person, na.rm = T)
    sd_litre_dataset <- stats::sd(results2$litre_per_day_per_person, na.rm = T)
    
    results2 <- results2 %>% 
      dplyr::mutate(litre_z_score = (litre_per_day_per_person - mean_litre_dataset) / sd_litre_dataset)
    
    mean_litre_zscore <- mean(results2$litre_z_score, na.rm = T)
    
    results2 <- results2 %>% 
      dplyr::mutate(flag_sd_litre = ifelse(is.na(litre_z_score), NA,
                                           ifelse(litre_z_score < mean_litre_zscore - 3 | litre_z_score > mean_litre_zscore + 3, 1, 0)),
                    flag_low_litre = ifelse(is.na(litre_per_day_per_person), NA,
                                            ifelse(litre_per_day_per_person <= 1, 1, 0)),
                    flag_high_litre = ifelse(is.na(litre_per_day_per_person),NA,
                                             ifelse(litre_per_day_per_person >=50, 1, 0)),
                    flag_high_container = ifelse(is.na(!!rlang::sym(num_containers)),NA ,
                                                       ifelse(as.numeric(!!rlang::sym(num_containers)) > 20, 1, 0)),
                    flag_no_container = case_when(!!rlang::sym(water_source) == "1" & is.na(!!rlang::sym(num_containers)) ~ 1, .default = 0,
                                                  TRUE ~ NA),
                    flag_not_immediate = case_when(!!rlang::sym(water_source) == "1" & !!rlang::sym(water_collect_time) != "inside_compound" ~ 1, .default = 0,
                                                   TRUE ~ NA)) %>% 
      dplyr::select(water_source,wash_different_water_sources,
                    num_containers,litre_per_day_per_person,
                    litre_z_score,water_collect_time,flag_sd_litre,flag_low_litre,
                    flag_high_litre,flag_high_container,flag_no_container,flag_not_immediate)
    
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  } else {
    results2 <- .dataset %>% 
      dplyr::mutate(flag_not_immediate = case_when(!!rlang::sym(water_source) == "1" & !!rlang::sym(water_collect_time) != "inside_compound" ~ 1, .default = 0,
                                                   TRUE ~ NA)) %>% 
      dplyr::select(water_source,water_collect_time,flag_not_immediate)
    if(!exists("results")){
      results <- results2
    } else {
      results <- cbind(results,results2)
    }
  }
  options(warn = 0)
  return(results)
}

## Function to check Nutrition/Muac
check_nut_flags <- function(.dataset,
                            nut_muac_cm = "nut_muac_cm",
                            edema_confirm = "nut_edema_confirm",
                            child_age_months = "child_age_months",
                            child_sex = "child_sex",
                            uuid = "uuid",
                            loop_index = "loop_index") {
  # change df into dataframe
  .dataset <- as.data.frame(.dataset)
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  
  results <- .dataset %>% 
    dplyr::select(uuid,loop_index)

  results2 <- .dataset %>% 
    dplyr::mutate(nut_muac_mm = ifelse(is.na(nut_muac_cm), NA, as.numeric(nut_muac_cm) * 10),
                  nut_muac_cm = as.numeric(nut_muac_cm),
                  sex = ifelse(!!rlang::sym(child_sex) == "m",1,2),
                  age_months = as.numeric(!!rlang::sym(child_age_months)),
                  age_days = as.numeric(!!rlang::sym(child_age_months))* 30.25)
  
  ## calculate MUAC-for-age z-scores
  results2 <- zscorer::addWGSR(data = results2,
                               sex = "sex",
                               firstPart = "nut_muac_cm",
                               secondPart = "age_days",
                               index = "mfa")
  
  mean_mfaz_dataset <- mean(results2$mfaz, na.rm=T)

  
  results2 <- results2 %>%
    dplyr::mutate(severe_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz < -3, 1, 0)),
                  moderate_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz >= -3 & mfaz < -2, 1, 0)),
                  global_mfaz = ifelse(is.na(mfaz), NA, ifelse(mfaz < -2, 1, 0)),
                  severe_mfaz = ifelse(is.na(!!rlang::sym(edema_confirm)), severe_mfaz, ifelse(!!rlang::sym(edema_confirm) == "y", 1, severe_mfaz)),
                  global_mfaz = ifelse(is.na(!!rlang::sym(edema_confirm)), global_mfaz, ifelse(!!rlang::sym(edema_confirm) == "y", 1, global_mfaz)),
                  severe_mfaz = ifelse(age_months < 6 | age_months >=60, NA, severe_mfaz),
                  moderate_mfaz = ifelse(age_months < 6 | age_months >=60, NA, moderate_mfaz),
                  global_mfaz = ifelse(age_months < 6 | age_months >=60, NA, global_mfaz),
                  sam_muac = ifelse(is.na(nut_muac_cm), NA, ifelse(nut_muac_cm < 11.5, 1, 0)),
                  mam_muac = ifelse(is.na(nut_muac_cm), NA, ifelse(nut_muac_cm >= 11.5 & nut_muac_cm < 12.5, 1, 0)),
                  gam_muac = ifelse(is.na(nut_muac_cm), NA, ifelse(nut_muac_cm < 12.5, 1, 0)),
                  sam_muac = ifelse(is.na(!!rlang::sym(edema_confirm)), sam_muac, ifelse(!!rlang::sym(edema_confirm) == "yes", 1, sam_muac)),
                  gam_muac = ifelse(is.na(!!rlang::sym(edema_confirm)), gam_muac, ifelse(!!rlang::sym(edema_confirm) == "yes", 1, gam_muac)),
                  sam_muac = ifelse(age_months < 6 | age_months >=60, NA, sam_muac),
                  mam_muac = ifelse(age_months < 6 | age_months >=60, NA, mam_muac),
                  gam_muac = ifelse(age_months < 6 | age_months >=60, NA, gam_muac),
                  flag_sd_mfaz = ifelse(is.na(mfaz),NA,
                                         ifelse(mfaz < mean_mfaz_dataset - 4 | mfaz > mean_mfaz_dataset + 3, 1, 0)),
                  flag_extreme_muac = ifelse(is.na(nut_muac_cm), NA,
                                             ifelse(nut_muac_cm < 7 | nut_muac_cm > 22, 1, 0)),
                  flag_edema_pitting = ifelse(is.na(!!rlang::sym(edema_confirm)), NA,
                                              ifelse(!!rlang::sym(edema_confirm) == "yes",1,0)),
                  mfaz_who_flag = ifelse(is.na(mfaz), NA, ifelse(mfaz < -5 | mfaz > 5, 1, 0)),
                  mfaz_noflag = ifelse(is.na(mfaz) | flag_sd_mfaz == 1, NA, mfaz),
                  muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, nut_muac_cm)),
                  gam_muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, gam_muac)),
                  mam_muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, mam_muac)),
                  sam_muac_noflag = ifelse(is.na(nut_muac_cm), NA, ifelse(flag_extreme_muac == 1, NA, sam_muac)),
                  mean_mfaz_noflag = round(mean(mfaz_noflag, na.rm = TRUE),3),
                  sd_mfaz_noflag = round(stats::sd(mfaz_noflag, na.rm = TRUE),2),
                  global_mfaz_noflag = ifelse(is.na(global_mfaz), NA, ifelse(is.na(flag_sd_mfaz), global_mfaz, ifelse(flag_sd_mfaz == 1, NA, global_mfaz))),
                  moderate_mfaz_noflag = ifelse(is.na(moderate_mfaz), NA, ifelse(is.na(flag_sd_mfaz), moderate_mfaz, ifelse(flag_sd_mfaz == 1, NA, moderate_mfaz))),
                  severe_mfaz_noflag = ifelse(is.na(severe_mfaz), NA, ifelse(is.na(flag_sd_mfaz), severe_mfaz, ifelse(flag_sd_mfaz == 1, NA, severe_mfaz)))) %>% 
    dplyr::select(sex,age_months,age_days,edema_confirm,mfaz,nut_muac_cm,nut_muac_mm,gam_muac_noflag,
                  mam_muac_noflag,sam_muac_noflag,flag_sd_mfaz,flag_extreme_muac,flag_edema_pitting,
                  mfaz_who_flag,mfaz_noflag,severe_mfaz,moderate_mfaz,global_mfaz,mean_mfaz_noflag,sd_mfaz_noflag,
                  muac_noflag,sam_muac,mam_muac,gam_muac,global_mfaz_noflag,moderate_mfaz_noflag,severe_mfaz_noflag)

  
  if(!exists("results")){
    results <- results2
  } else {
    results <- cbind(results,results2)
  }
  
  options(warn = 0)
  return(results)
}


check_mortality_flags <- function(.dataset,
                                  dataset_roster,
                                  dataset_died,
                                  recall_date = "recall_date",
                                  today = "today",
                                  enumerator = "enumerator",
                                  num_hh = "num_hh",
                                  num_left = "num_left",
                                  num_join = "num_join",
                                  ind_sex = "ind_sex",
                                  calc_final_age_years = "calc_final_age_years",
                                  final_ind_dob = "final_ind_dob",
                                  num_died = "num_died",
                                  sex_died = "sex_died",
                                  age_died_years = "age_died_years",
                                  final_date_death = "final_date_death",
                                  cause_death = "cause_death",
                                  location_death = "location_death") {

  .dataset <- as.data.frame(.dataset)
  dataset_roster <- as.data.frame(dataset_roster)
  # change df into dataframe
  dataset_died <- as.data.frame(dataset_died)
  
  options(warn = -1)
  ## Throw an error if the dataset is empty
  if (nrow(.dataset) == 0) {
    stop("Dataset is empty")
  }
  ## Throw an error if the dataset is empty
  if (nrow(dataset_roster) == 0) {
    stop("Dataset is empty")
  }
  if (nrow(dataset_died) == 0) {
    stop("Dataset is empty")
  }

  
  
  results_crude <- .dataset %>% 
    filter(respondent_consent == "yes") %>% 
    dplyr::select(uuid, enumerator, recall_date, today,
                  num_hh, num_left, num_join, num_died) %>% 
    dplyr::mutate(date_recall_event = lubridate::as_date(recall_date)+1,
                  date_dc_date = lubridate::as_date(today)) %>% 
    dplyr::mutate_at(vars(starts_with("num_")),as.numeric)
  
  num_birth <- dataset_roster %>% 
    dplyr::mutate(date_birth_date = lubridate::as_date(final_ind_dob))  %>% 
    dplyr::mutate(birth = ifelse(lubridate::year(date_birth_date)== "2023",1,0),
                  birth_m = ifelse(lubridate::year(date_birth_date) == "2023" & ind_sex == "m",1,0),
                  birth_f = ifelse(lubridate::year(date_birth_date) == "2023" & ind_sex == "f",1,0),
                  m = ifelse(ind_sex == "m",1,0),
                  f = ifelse(ind_sex == "f",1,0),
                  under5_m = ifelse(is_child == "1" & ind_sex == "m",1,0),
                  under5_f = ifelse(is_child == "1" & ind_sex == "f",1,0))%>%
    dplyr::group_by(uuid) %>% 
    dplyr::summarise(num_birth = sum(birth,na.rm = T),
                     num_birth_m = sum(birth_m,na.rm = T),
                     num_birth_f = sum(birth_f,na.rm = T),
                     num_under5 = sum(as.numeric(is_child),na.rm = T),
                     num_under5_m = sum(as.numeric(under5_m),na.rm = T),
                     num_under5_f = sum(as.numeric(under5_f),na.rm = T),
                     num_m = sum(m, na.rm = T),
                     num_f = sum(f, na.rm = T))
  
  num_died <- dataset_died %>% 
    dplyr::mutate(num_days = ifelse(is.na(ind_born_died),lubridate::as_date(final_date_death) - lubridate::as_date(recall_date),
                                    ifelse(ind_born_died == "1",
                                    lubridate::as_date(final_date_death) - lubridate::as_date(dob_died),
                                    lubridate::as_date(final_date_death) - lubridate::as_date(recall_date))),
                  num_died_m = ifelse(sex_died == "m", 1, 0),
                  num_died_f = ifelse(sex_died == "f", 1, 0),
                  num_died_under5 = ifelse(calc_final_age_years_died < 6, 1, 0),
                  num_died_under5_m = ifelse(calc_final_age_years_died < 6 & sex_died == "m", 1, 0),
                  num_died_under5_f = ifelse(calc_final_age_years_died < 6 & sex_died == "f", 1, 0),
                  pt_died = num_days,
                  pt_died_m = ifelse(sex_died == "m",num_days,0),
                  pt_died_f = ifelse(sex_died == "f",num_days,0),
                  pt_died_under5 =ifelse(calc_final_age_years_died < 6, pt_died,0),
                  pt_died_under5_m =ifelse(num_died_under5_m == 1, pt_died_m,0),
                  pt_died_under5_f =ifelse(num_died_under5_f == 1, pt_died_f,0)) %>% 
    group_by(uuid)%>% 
    dplyr::summarise(num_died_under5 = sum(num_died_under5,na.rm = T),
                     num_died_m= sum(num_died_m, na.rm =T),
                     num_died_f= sum(num_died_f, na.rm =T),
                     num_died_under5_m = sum(num_died_under5_m, na.rm=T),
                     num_died_under5_f = sum(num_died_under5_f, na.rm=T),
                     pt_died = sum(pt_died, na.rm=T),
                     pt_died_m = sum(pt_died_m, na.rm=T),
                     pt_died_f = sum(pt_died_f, na.rm=T),
                     pt_died_under5 = sum(pt_died_under5, na.rm=T),
                     pt_died_under5_m = sum(pt_died_under5_m, na.rm=T),
                     pt_died_under5_f = sum(pt_died_under5_f, na.rm=T))
    
  
  results2_crude <- results_crude %>% 
    left_join(num_birth, by = "uuid") %>% 
    left_join(num_died, by="uuid") %>% 
    mutate_at(vars(starts_with("num_")),~ifelse(is.na(.),0,.)) %>%
    mutate_at(vars(starts_with("pt_died")),~ifelse(is.na(.),0,.)) %>%
    mutate(pt_total = num_hh * as.numeric(date_dc_date - date_recall_event),
           pt_total_m = num_m * as.numeric(date_dc_date - date_recall_event),
           pt_total_f = num_f * as.numeric(date_dc_date - date_recall_event),
           pt_total_under5 = num_under5 * as.numeric(date_dc_date - date_recall_event),
           pt_total_under5_m = num_under5_m * as.numeric(date_dc_date - date_recall_event),
           pt_total_under5_f = num_under5_f * as.numeric(date_dc_date - date_recall_event),
           pt_birth = 0.5 * num_birth * as.numeric(date_dc_date - date_recall_event),
           pt_birth_m = 0.5 * num_birth_m * as.numeric(date_dc_date - date_recall_event),
           pt_birth_f = 0.5 * num_birth_f * as.numeric(date_dc_date - date_recall_event),
           pt_join =  0.5 * num_join * as.numeric(date_dc_date - date_recall_event),
           pt_left =  0.5 * num_left * as.numeric(date_dc_date - date_recall_event),
           flag_multiple_death = case_when(is.na(num_died)~NA,
                                           as.numeric(num_died)>1 ~ 1,
                                           TRUE~0))

  sex_age_ratios <- dataset_died %>% 
    dplyr::left_join(select(.dataset, c(uuid,enumerator)), by ="uuid") %>% 
    dplyr::mutate(age_0to5 = ifelse(calc_final_age_years_died < 5,1,0),
                  age_5plus = ifelse(calc_final_age_years_died >=5,1,0),
                  age_0to2 = ifelse(calc_final_age_years_died < 2,1, 0),
                  age_2to5 = ifelse(calc_final_age_years_died >=2 & calc_final_age_years_died < 5,1, 0),
                  age_5to10 = ifelse(calc_final_age_years_died >=5 & calc_final_age_years_died < 10,1, 0)) %>% 
    mutate(flag_cause_death = ifelse(sex_died == "m" & cause_death %in% c("post_partum","during_pregnancy","during_delivery"),1,0))
  
  result <- list("crude" = results2_crude,
                 "ratios" = sex_age_ratios)
  

  return(result)
  
}



################################################################################
#                             CREATE PLAUSIBILITY REPORTS                      #
################################################################################


create_fsl_quality_report_phu <- function (df, grouping = NULL, short_report = NULL, file_path = NULL) {
  options(warn = -1)
  if (is.null(short_report)) {
    short_report <- FALSE
  }
  if (!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }
  if (c("fsl_fcs_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_fcs = round(mean(fsl_fcs_score, na.rm = TRUE), 2),
                       sd_fcs = round(stats::sd(fsl_fcs_score, na.rm = TRUE), 2),
                       mean_days_cereals = round(mean(fsl_fcs_cereal, na.rm = TRUE), 2),
                       sd_days_cereals = round(stats::sd(fsl_fcs_cereal, na.rm = TRUE), 2),
                       mean_days_legumes = round(mean(fsl_fcs_legumes, na.rm = TRUE), 2),
                       sd_days_legumes = round(stats::sd(fsl_fcs_legumes, na.rm = TRUE), 2),
                       mean_days_dairy = round(mean(fsl_fcs_dairy, na.rm = TRUE), 2),
                       sd_days_dairy = round(stats::sd(fsl_fcs_dairy, na.rm = TRUE), 2),
                       mean_days_meat = round(mean(fsl_fcs_meat, na.rm = TRUE), 2),
                       sd_days_meat = round(stats::sd(fsl_fcs_meat, na.rm = TRUE), 2),
                       mean_days_veg = round(mean(fsl_fcs_veg, na.rm = TRUE), 2), 
                       sd_days_veg = round(stats::sd(fsl_fcs_veg, na.rm = TRUE), 2), 
                       mean_days_fruit = round(mean(fsl_fcs_fruit, na.rm = TRUE), 2),
                       sd_days_fruit = round(stats::sd(fsl_fcs_fruit, na.rm = TRUE), 2),
                       mean_days_oils = round(mean(fsl_fcs_oil, na.rm = TRUE), 2),
                       sd_days_oils = round(stats::sd(fsl_fcs_oil, na.rm = TRUE), 2),
                       mean_days_sugar = round(mean(fsl_fcs_sugar, na.rm = TRUE), 2),
                       sd_days_sugar = round(stats::sd(fsl_fcs_sugar, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("fsl_rcsi_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_rcsi = round(mean(fsl_rcsi_score, na.rm = TRUE), 2), 
                       sd_rcsi = round(stats::sd(fsl_rcsi_score, na.rm = TRUE), 2),
                       mean_rcsi_lessquality = round(mean(fsl_rcsi_lessquality, na.rm = TRUE), 2), 
                       sd_rcsi_lessquality = round(stats::sd(fsl_rcsi_lessquality, na.rm = TRUE), 2),
                       mean_rcsi_borrow = round(mean(fsl_rcsi_borrow, na.rm = TRUE), 2),
                       sd_rcsi_borrow = round(stats::sd(fsl_rcsi_borrow, na.rm = TRUE), 2),
                       mean_rcsi_mealsize = round(mean(fsl_rcsi_mealsize, na.rm = TRUE), 2),
                       sd_rcsi_mealsize = round(stats::sd(fsl_rcsi_mealsize, na.rm = TRUE), 2),
                       mean_rcsi_mealadult = round(mean(fsl_rcsi_mealadult, na.rm = TRUE), 2), 
                       sd_rcsi_mealadult = round(stats::sd(fsl_rcsi_mealadult, na.rm = TRUE), 2),
                       mean_rcsi_mealnb = round(mean(fsl_rcsi_mealnb, na.rm = TRUE), 2),
                       sd_rcsi_mealnb = round(stats::sd(fsl_rcsi_mealnb, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("fsl_hhs_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_hhs = round(mean(fsl_hhs_score, na.rm = TRUE), 2),
                       sd_hhs = round(stats::sd(fsl_hhs_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
  }
  if (c("fsl_hdds_score") %in% colnames(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(mean_hdds = round(mean(fsl_hdds_score, na.rm = TRUE), 2), 
                       sd_hdds = round(stats::sd(fsl_hdds_score, na.rm = TRUE), 2))
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }
  if (length(setdiff(c("fsl_fcs_score", "fsl_rcsi_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_rcsi = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_rcsi_score)[4]), 2),
                           corr.fcs_rcsi.pvalue = as.numeric(stats::cor.test(fsl_fcs_score, fsl_rcsi_score)[3]))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_rcsi = NA,
                           corr.fcs_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_fcs_score", "fsl_hhs_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hhs = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hhs_score)[4]), 2), 
                           corr.fcs_hhs.pvalue = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hhs_score)[3]), 6))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hhs = NA,
                           corr.fcs_hhs.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_fcs_score", "fsl_hdds_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hdds = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hdds_score)[4]), 2),
                           corr.fcs_hdds.pvalue = round(as.numeric(stats::cor.test(fsl_fcs_score, fsl_hdds_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.fcs_hdds = NA,
                           corr.fcs_hdds.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_hdds_score", "fsl_rcsi_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hdds_rcsi = round(as.numeric(stats::cor.test(fsl_hdds_score, fsl_rcsi_score)[4]), 2), 
                           corr.hdds_rcsi.pvalue = round(as.numeric(stats::cor.test(fsl_hdds_score, fsl_rcsi_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        } else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hdds_rcsi = NA,
                           corr.hdds_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }
  if (length(setdiff(c("fsl_hhs_score", "fsl_rcsi_score"), colnames(df))) == 0) {
    tryCatch(
      {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hhs_rcsi = round(as.numeric(stats::cor.test(fsl_hhs_score, fsl_rcsi_score)[4]), 2),
                           corr.hhs_rcsi.pvalue = round(as.numeric(stats::cor.test(fsl_hhs_score, fsl_rcsi_score)[3]), 3))
        if (!exists("results")) {
          results <- results2
        } else {
          results <- merge(results, results2)
        }
      }, error = function(e) {
        results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
          dplyr::summarise(corr.hhs_rcsi = NA,
                           corr.hhs_rcsi.pvalue = NA)
        if (!exists("results")) {
          results <- results2
        }else {
          results <- merge(results, results2)
        }
      }
    )
  }

  if (length(setdiff(c("fsl_fcs_score", "fsl_hhs_score", "fsl_hdds_score", 
                       "fsl_rcsi_score", "flag_lcsi_coherence"), names(df))) < 5) {
    nms <- df %>% dplyr::select(dplyr::starts_with("flag")) %>% 
      names()
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise_at(.vars = nms, ~round(mean(., na.rm = TRUE), 3) * 100)
    if (!exists("results")) {
      results <- results2
    } else {
      results <- merge(results, results2)
    }
  }
  results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
    dplyr::summarise(n = dplyr::n())
  if (!exists("results")) {
    results <- results2
  }else {
    results <- merge(results, results2)
  }
  
  results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  if (c("flag_fc_cell") %in% names(df)) {
    results2 <- df %>% dplyr::mutate(p1 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 1 FC", 1, 0)), 
                                     p2 = ifelse(is.na(fc_phase), NA, 
                                                 ifelse(fc_phase == "Phase 2 FC", 1, 0)),
                                     p3 = ifelse(is.na(fc_phase), NA,
                                                 ifelse(fc_phase == "Phase 3 FC", 1, 0)),
                                     p4 = ifelse(is.na(fc_phase), NA, 
                                                 ifelse(fc_phase == "Phase 4 FC", 1, 0)), 
                                     p5 = ifelse(is.na(fc_phase), NA, 
                                                 ifelse(fc_phase == "Phase 5 FC", 1, 0))) %>% 
      dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fc_cell), na.rm = TRUE), 
                       fews_p1 = round(sum(p1, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p2 = round(sum(p2, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p3 = round(sum(p3, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p4 = round(sum(p4, na.rm = TRUE)/sum(!is.na(fc_cell)), 2),
                       fews_p5 = round(sum(p5, na.rm = TRUE)/sum(!is.na(fc_cell)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>% 
      dplyr::select(c(1, n, dplyr::everything()))
  }
  if (c("food_exp_share") %in% names(df)) {
    results2 <- df %>% dplyr::group_by(!!rlang::sym(grouping)) %>% 
      dplyr::summarise(prop_fc_flags = sum(flag_fc_cell, na.rm = TRUE)/sum(!is.na(fc_cell), na.rm = TRUE), 
                       fes_1 = round(sum(food_exp_share == "1", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_2 = round(sum(food_exp_share == "2", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_3 = round(sum(food_exp_share == "3", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2),
                       fes_4 = round(sum(food_exp_share == "4", na.rm = TRUE)/sum(!is.na(food_exp_share)), 2))
    if (!exists("results")) {
      results <- results2
    }else {
      results <- merge(results, results2)
    }
    results <- results %>% dplyr::select(c(1, n, dplyr::everything()))
  }
  results <- calculate_plausibility_report_phu(df = results)
  a <- c("n", "fews_p1", "fews_p2", "fews_p3", "fews_p4", 
         "fews_p5", "flag_severe_hhs", "flag_lcsi_severity", 
         "plaus_fcs", "plaus_rcsi", "plaus_hhs", "plaus_lcsi", 
         "plaus_other_fsl", "plaus_fsl_score", "plaus_fsl_cat")
  b <- intersect(a, colnames(results))
  if (short_report == TRUE & length(setdiff(b, colnames(results))) == 
      0) {
    results <- results %>% dplyr::select(1, b)
  }
  if (!is.null(file_path)) {
    writexl::write_xlsx(results, file_path)
  }
  options(warn = 0)
  return(results)
}


create_anthro_quality_report_phu <- function(df, grouping = NULL, file_path = NULL, short_report = NULL) {
  
  options(warn=-1)
  if(is.null(short_report)) {short_report <- FALSE}
  if (!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }
  # check which indexes are present
  
  if(!(c("nut_edema_confirm") %in% names(df))) {
    
    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(nut_edema_confirm), 0, ifelse(nut_edema_confirm == "yes", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(num_oedema = sum(oedemas, na.rm = TRUE))
    
    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
    
  } else {df <- df %>% dplyr::mutate(oedema = 0)}


  if(c("sex") %in% names(df)) {
    df2 <- df %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(sex_ratio = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = c(1,1))[1]),3),
                       sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = c(1,1))[5]),2))
    
    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
    
  }
  if(c("age_months") %in% names(df)) {
    
    df2 <- df %>%
      dplyr::filter(!is.na(age_months)) %>%
      dplyr::filter(age_months >= 6 & age_months < 60) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(age_ratio = nipnTK::ageRatioTest(age_months, ratio = 0.85)[3],
                       age_ratio = round(as.numeric(age_ratio),2),
                       age_ratio.pvalue = round(as.numeric(nipnTK::ageRatioTest(age_months, ratio = 0.85)[7]),2),)
    
    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
    
  }
  if(c("nut_muac_cm") %in% names(df)) {
    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(nut_edema_confirm), 0, ifelse(nut_edema_confirm == "yes", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_muac = sum(!is.na(nut_muac_cm), na.rm = TRUE),
                       dps_muac = nipnTK::digitPreference(nut_muac_cm)[[1]],
                       mean_muac = round(mean(nut_muac_cm, na.rm = TRUE),3),
                       sd_muac = round(stats::sd(nut_muac_cm, na.rm = TRUE),2),
                       sd_muac_mm = round(stats::sd(nut_muac_mm, na.rm = TRUE),2),
                       num_muac_flags = sum(flag_extreme_muac, na.rm = TRUE),
                       mean_muac_noflag = round(mean(muac_noflag, na.rm = TRUE),3),
                       sd_muac_noflag = round(stats::sd(muac_noflag, na.rm = TRUE),2),
                       gam_muac_abs = round(mean(gam_muac_noflag, na.rm = TRUE),3),
                       gam_muac_low = round(gam_muac_abs - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(gam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))),2),
                       gam_muac_upp = round(gam_muac_abs + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(gam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))),2),
                       mam_muac_abs = round(mean(mam_muac_noflag, na.rm = TRUE),3),
                       mam_muac_low = round(mam_muac_abs - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(mam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))),2),
                       mam_muac_upp = round(mam_muac_abs + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(mam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))),2),
                       sam_muac_abs = round(mean(sam_muac_noflag, na.rm = TRUE),3),
                       sam_muac_low = round(sam_muac_abs - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(sam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))),2),
                       sam_muac_upp = round(sam_muac_abs + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(sam_muac_noflag, na.rm = TRUE) / sqrt(dplyr::n()))),2)) %>%
      dplyr::mutate(sam_muac_low = ifelse(sam_muac_low < 0, 0, sam_muac_low),
                    mam_muac_low = ifelse(mam_muac_low < 0, 0, mam_muac_low),
                    gam_muac_low = ifelse(gam_muac_low < 0, 0, gam_muac_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(gam_muac_results = paste0(gam_muac_abs, "% [",gam_muac_low, " - ", gam_muac_upp, "]"),
                    mam_muac_results = paste0(mam_muac_abs, "% [",mam_muac_low, " - ", mam_muac_upp, "]"),
                    sam_muac_results = paste0(sam_muac_abs, "% [",sam_muac_low, " - ", sam_muac_upp, "]"),
                    mean_sd_muac = paste0(round(mean_muac_noflag, 2), "+/-", round(sd_muac_noflag, 2)),
                    gam_muac_abs = NULL, gam_muac_low = NULL, gam_muac_upp = NULL, mam_muac_abs = NULL, mam_muac_low = NULL, mam_muac_upp = NULL,
                    sam_muac_abs = NULL,  sam_muac_low = NULL, sam_muac_upp = NULL)
    
    
    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
    
  }
  
  if(c("mfaz") %in% names(df)) {
    
    df2 <- df %>%
      dplyr::mutate(oedemas = ifelse(is.na(nut_edema_confirm), NA, ifelse(nut_edema_confirm == "yes", 1, 0))) %>%
      dplyr::group_by(!!rlang::sym(grouping)) %>%
      dplyr::summarise(n_children_mfaz = sum(!is.na(mfaz), na.rm = TRUE),
                       mean_mfaz = round(mean(mfaz, na.rm = TRUE),3),
                       sd_mfaz = round(stats::sd(mfaz, na.rm = TRUE),2),
                       mean_mfaz_noflag = round(mean(mfaz_noflag, na.rm = TRUE),3),
                       sd_mfaz_noflag = round(stats::sd(mfaz_noflag, na.rm = TRUE),2),
                       num_smart_flags_mfaz = sum(flag_sd_mfaz, na.rm = TRUE),
                       flag_perc_mfaz_children = round(num_smart_flags_mfaz/n_children_mfaz ,2 ),
                       prop_smart_flags_mfaz = round((sum(flag_sd_mfaz, na.rm = TRUE) / n_children_mfaz)*100,2),
                       skewness_mfaz = abs(as.numeric(nipnTK::skewKurt(mfaz_noflag)[1])),
                       kurtosis_mfaz = abs(as.numeric(nipnTK::skewKurt(mfaz_noflag)[5])),
                       new_global_mfaz = round(mean(global_mfaz_noflag, na.rm = TRUE),3)*100,
                       global_mfaz_low = round(new_global_mfaz - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(global_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       global_mfaz_upp = round(new_global_mfaz + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(global_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_mfaz = round(mean(moderate_mfaz_noflag, na.rm = TRUE),3)*100,
                       moderate_mfaz_low = round(moderate_mfaz - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(moderate_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       moderate_mfaz_upp = round(moderate_mfaz + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(moderate_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_mfaz = round(mean(severe_mfaz_noflag, na.rm = TRUE),3)*100,
                       severe_mfaz_low = round(severe_mfaz - (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(severe_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2),
                       severe_mfaz_upp = round(severe_mfaz + (stats::qt(p=0.05/2, df = dplyr::n() - 1, lower.tail = F)*(stats::sd(severe_mfaz_noflag, na.rm = TRUE) / sqrt(dplyr::n()))*100),2)) %>%
      dplyr::mutate(severe_mfaz_low = ifelse(severe_mfaz_low <0, 0, severe_mfaz_low),
                    moderate_mfaz_low = ifelse(moderate_mfaz_low <0, 0, moderate_mfaz_low),
                    global_mfaz_low = ifelse(global_mfaz_low <0, 0, global_mfaz_low)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(mfaz_results = paste0(new_global_mfaz, "% [",global_mfaz_low, " - ", global_mfaz_upp, "]"),
                    moderate_mfaz_results = paste0(moderate_mfaz, "% [",moderate_mfaz_low, " - ", moderate_mfaz_upp, "]"),
                    severe_mfaz_results = paste0(severe_mfaz, "% [",severe_mfaz_low, " - ", severe_mfaz_upp, "]"),
                    mean_sd_mfaz = paste0(round(mean_mfaz_noflag, 2), "+/-", round(sd_mfaz_noflag, 2)),
                    new_global_mfaz = NULL, global_mfaz_low = NULL, global_mfaz_upp = NULL, moderate_mfaz = NULL, moderate_mfaz_low = NULL, moderate_mfaz_upp = NULL,
                    severe_mfaz = NULL,  severe_mfaz_low = NULL, severe_mfaz_upp = NULL)
    

    
    if(!exists("results.table")) {results.table <- df2} else {results.table <- merge(results.table, df2)}
    
  }
  
 results.table <- calculate_plausibility_report_phu(results.table)
  
  a <- c("n_children_wfhz", "prop_smart_flags", "mean_sd", "gam_results", "sam_results",
         "n_children_muac", "mean_sd_muac", "gam_muac_results", "sam_muac_results",
         "anthro_plaus_score", "anthro_plaus_cat")
  
  b <- intersect(a, colnames(results.table))
  
  if(short_report == TRUE & length(setdiff(b, colnames(results.table)))==0) {
    
    results.table <- results.table %>%
      dplyr::select(1, b)
  }
  
  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(results.table, file_path)}
  options(warn=0)
  return(results.table)
  
}

create_mortality_quality_report_phu <- function(df,
                                                 data_died,
                                                 grouping = NULL,
                                                 file_path = NULL,
                                                 short_report = NULL) {
  
  options(warn=-1)

  data_died <- raw.mort.flags$ratios
  if(is.null(short_report)) {short_report <- FALSE}
  

  sx_ratio <- c(1,1)
  
  age_under5_ratio <- c(.2,.8)
  
  age_under2to5_ratio <- c(0.4118, 0.5882)

  age_under5to10_ratio <- c(0.5238, 0.4762)

  expected_hh_size <- 5

  if(!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    data_died <- data_died %>% dplyr::mutate(group = "All")
    grouping <- "group"
  }
  
  
  # need to add sex and age ratios, poisson p-values for deaths, proportion of HHs with under-5 child, Avg. household size per grouping
  
  # summarizing individual level indicators


  df2 <- df %>%
    dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::summarize(total_people = sum(num_hh, na.rm = TRUE),
                     total_persontime = sum(pt_total, na.rm = TRUE),
                     avg.persontime = mean(pt_total, na.rm = TRUE),
                     total_under5 = sum(num_under5, na.rm = TRUE),
                     total_under5_persontime = sum(pt_total_under5, na.rm = TRUE),
                     avg.under5_persontime = mean(pt_total_under5, na.rm = TRUE),
                     joins = sum(num_join, na.rm = TRUE),
                     lefts = sum(num_left, na.rm = TRUE),
                     births = sum(num_birth, na.rm = TRUE),
                     deaths = sum(num_died, na.rm = TRUE),
                     deaths_under5 = sum(num_died_under5, na.rm = TRUE),
                     flag_hh_multiple_death = sum(flag_multiple_death, na.rm = T))%>%
    dplyr::mutate(cdr = deaths / (total_persontime),
                  cdr_se = sqrt((cdr * (1 - cdr)) / total_persontime),
                  cdr_lower_ci = round((cdr - 1.96*cdr_se)*10000,3),
                  cdr_upper_ci = round((cdr + 1.96*cdr_se)*10000,3),
                  u5dr = deaths_under5 / (total_under5_persontime),
                  u5dr_se = sqrt((u5dr * (1 - u5dr)) / total_under5_persontime),
                  u5dr_lower_ci = round((u5dr - 1.96*u5dr_se)*10000,3),
                  u5dr_upper_ci = round((u5dr + 1.96*u5dr_se)*10000,3),
                  cdr = round(cdr,6)*10000,
                  u5dr = round(u5dr,6)*10000,
                  cdr_ci = paste0(cdr, " [", cdr_lower_ci, " - ", cdr_upper_ci, "]"),
                  u5dr_ci = paste0(u5dr, " [", u5dr_lower_ci, " - ", u5dr_upper_ci, "]"),
                  prop_join_people = round((joins / total_people),2)*100,
                  prop_left_people = round((lefts / total_people),2)*100,
                  prop_hh_multiple_death = round((flag_hh_multiple_death / total_people),2)*100) %>%
    dplyr::select(cdr_ci, u5dr_ci, dplyr::everything()) %>% 
    dplyr::mutate(plaus_overall_cdr = case_when(cdr<1~0,
                                               cdr<2~5,
                                               cdr<3.5~10,
                                               cdr>3.5~20,
                                               TRUE ~ NA),
                  plaus_hh_multiple_death = case_when(prop_hh_multiple_death<0.5~0,
                                                      prop_hh_multiple_death<1~5,
                                                      prop_hh_multiple_death<1.5~10,
                                                      prop_hh_multiple_death>=1.5~20,
                                                      TRUE~NA))
  
  df3 <- data_died %>% 
    dplyr::group_by(!!rlang::sym(grouping)) %>%
    dplyr::mutate(sex = case_when(sex_died == "m"~1,
                                  sex_died == "f"~2,
                                       TRUE~NA)) %>% 
    dplyr::summarise(sex_ratio = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = sx_ratio)[1]),3),
                  sex_ratio.pvalue = round(as.numeric(nipnTK::sexRatioTest(sex, codes = c("1", "2"), pop = sx_ratio)[5]),2),
                  age_ratio_0_5 = sum(!is.na(age_0to5)) / sum(!is.na(age_5plus)),
                  age_ratio_0_5.pvalue = stats::chisq.test(x = c(sum(!is.na(age_0to5)), sum(!is.na(age_5plus))), p = age_under5_ratio)[3],
                  age_ratio_2_5 = sum(!is.na(age_0to2)) / sum(!is.na(age_2to5)),
                  age_ratio_2_5.pvalue = stats::chisq.test(x = c(sum(!is.na(age_0to2)), sum(!is.na(age_2to5))), p = age_under2to5_ratio)[3],
                  age_ratio_5_10 = sum(!is.na(age_0to5)) / sum(!is.na(age_5to10)),
                  age_ratio_5_10.pvalue = stats::chisq.test(x = c(sum(!is.na(age_0to5)), sum(!is.na(age_5to10))), p = age_under5to10_ratio)[3]) %>% 
    dplyr::mutate(plaus_sex_ratio = case_when(sex_ratio.pvalue>0.1~0,
                                              sex_ratio.pvalue>0.005~1,
                                              sex_ratio.pvalue>0.001~3,
                                              sex_ratio.pvalue<=0.001~5,
                                              TRUE ~ NA),
                  plaus_age0to4_5plus_ratio = case_when(age_ratio_0_5.pvalue>0.1~0,
                                                        age_ratio_0_5.pvalue>0.005~1,
                                                        age_ratio_0_5.pvalue>0.001~3,
                                                        age_ratio_0_5.pvalue<=0.001~5,
                                                        TRUE~NA),
                  plaus_age0to1_2to4_ratio = case_when(age_ratio_0_5.pvalue>0.1~0,
                                                        age_ratio_0_5.pvalue>0.005~1,
                                                        age_ratio_0_5.pvalue>0.001~3,
                                                        age_ratio_0_5.pvalue<=0.001~5,
                                                        TRUE~NA),
                  plaus_age0to4_5to10_ratio = case_when(age_ratio_0_5.pvalue>0.1~0,
                                                        age_ratio_0_5.pvalue>0.005~1,
                                                        age_ratio_0_5.pvalue>0.001~3,
                                                        age_ratio_0_5.pvalue<=0.001~5,
                                                        TRUE~NA))

  df4 <- merge(df2,df3) %>% 
    dplyr::mutate(plaus_mort_score = rowSums(across(c(plaus_overall_cdr,plaus_hh_multiple_death,
                                                      plaus_sex_ratio,plaus_age0to4_5plus_ratio,
                                                      plaus_age0to1_2to4_ratio,plaus_age0to4_5to10_ratio), .fns = as.numeric), na.rm = T),
                  plaus_mort_cat = case_when(plaus_mort_score<10~"Excellent",
                                             plaus_mort_score>=10 & plaus_mort_score<20~"Good",
                                             plaus_mort_score>=20 & plaus_mort_score<25~"Acceptable",
                                             plaus_mort_score>=25~"Problematic",
                                             TRUE~NA))

  return(df4)
  
}

calculate_plausibility_report_phu <- function (df) {
  print("Now Calculating Plausibility Scoring and Classifications.")
  anthro_plaus_vars <- c("flag_perc_mfaz_children","n_children_muac","sd_muac_mm",
                         "age_ratio.pvalue", "sex_ratio.pvalue","dps_muac")

  if (c("age_ratio.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_ageratio = ifelse(age_ratio.pvalue > 0.1, 0, 
                                                       ifelse(age_ratio.pvalue > 0.05, 2, 
                                                              ifelse(age_ratio.pvalue > 0.001, 4,
                                                                     ifelse(age_ratio.pvalue <= 0.001, 10, NA)))))
  }
  if ((c("sex_ratio.pvalue") %in% names(df)) & !(c("cdr") %in% 
                                                 names(df))) {
    df <- df %>% dplyr::mutate(plaus_sexratio = ifelse(sex_ratio.pvalue > 0.1, 0,
                                                       ifelse(sex_ratio.pvalue > 0.05, 2, 
                                                              ifelse(sex_ratio.pvalue > 0.001, 4,
                                                                     ifelse(sex_ratio.pvalue <= 0.001, 10, NA)))))
  }
  if (c("dps_muac") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_dps_muac = ifelse(dps_muac >= 0 & dps_muac < 8, 0,
                                                       ifelse(dps_muac >= 8 & dps_muac < 13, 2,
                                                              ifelse(dps_muac >= 13 & dps_muac < 20, 4,
                                                                     ifelse(dps_muac >= 20, 10, NA)))))
  }
  if (c("flag_perc_mfaz_children") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_perc_mfaz_children = ifelse(flag_perc_mfaz_children >= 0 & flag_perc_mfaz_children < 8, 0,
                                                       ifelse(flag_perc_mfaz_children >= 8 & flag_perc_mfaz_children < 13, 2,
                                                              ifelse(flag_perc_mfaz_children >= 13 & flag_perc_mfaz_children < 20, 4,
                                                                     ifelse(flag_perc_mfaz_children >= 20, 10, NA)))))
  }
  if (c("n_children_muac") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_n_children_muac = ifelse(n_children_muac > 100, 0,
                                                       ifelse(n_children_muac > 80 & n_children_muac <= 100, 2,
                                                              ifelse(n_children_muac > 50 & n_children_muac <= 80, 4,
                                                                     ifelse(n_children_muac <= 50, 10, NA)))))
  }
  if (c("sd_muac_mm") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_sd_muac_mm = ifelse(sd_muac_mm < 12, 0,
                                                       ifelse(sd_muac_mm < 14,5,
                                                              ifelse(sd_muac_mm < 15, 10,
                                                                     ifelse(sd_muac_mm >= 15, 20, NA)))))
  }
  if (length(setdiff(anthro_plaus_vars, names(df))) == 0) {
    print("Generating anthropometric plausibility score and classification.")
    df <- df %>% dplyr::mutate(plaus_anthro_score = rowSums(across(c(plaus_sd_muac_mm, plaus_n_children_muac,
                                 plaus_ageratio,plaus_sexratio,plaus_perc_mfaz_children,plaus_dps_muac), .fns = as.numeric), na.rm=T),
                               plaus_anthro_cat = ifelse(plaus_anthro_score >= 0 & plaus_anthro_score <= 9, "Excellent", 
                                                         ifelse(plaus_anthro_score > 9 & plaus_anthro_score <= 19, "Good",
                                                                ifelse(plaus_anthro_score > 19 & plaus_anthro_score < 25, "Acceptable", 
                                                                         ifelse(plaus_anthro_score >= 25, "Problematic", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for anthropometric plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(anthro_plaus_vars, names(df)))
  }
  mort_plaus_vars <- c("plaus_cdr", "plaus_prop_hh_flag_deaths", 
                       "plaus_sex_ratio.pvalue", "plaus_age_ratio_0_5.pvalue", 
                       "plaus_age_ratio_2_5.pvalue", "plaus_age_ratio_5_10.pvalue", 
                       "plaus_mean_hh_size.pvalue", "plaus_prop_joiners", "plaus_prop_leavers", 
                       "plaus_poisson_pvalues.deaths")
  if (c("cdr") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_cdr = ifelse(cdr < 1, 0,
                                                  ifelse(cdr < 2, 5,
                                                         ifelse(cdr < 3.5, 10,
                                                                ifelse(cdr >= 3.5, 20, 0)))))
  }
  if (c("prop_hh_flag_deaths") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_hh_flag_deaths = ifelse(prop_hh_flag_deaths < 0.005, 0,
                                                                  ifelse(prop_hh_flag_deaths < 0.01, 2,
                                                                         ifelse(prop_hh_flag_deaths < 0.015, 5,
                                                                                ifelse(prop_hh_flag_deaths >= 1.5, 10, 0)))))
  }
  if (length(setdiff(c("sex_ratio.pvalue", "cdr"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_sex_ratio.pvalue = ifelse(sex_ratio.pvalue > 0.05, 0,
                                                               ifelse(sex_ratio.pvalue > 0.001, 2,
                                                                      ifelse(sex_ratio.pvalue > 1e-04, 5,
                                                                             ifelse(sex_ratio.pvalue <= 1e-04, 10, 0)))))
  }
  if (c("age_ratio_0_5.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_0_5.pvalue = ifelse(age_ratio_0_5.pvalue > 0.1, 0,
                                                                   ifelse(age_ratio_0_5.pvalue > 0.05, 2,
                                                                          ifelse(age_ratio_0_5.pvalue > 0.001, 5, 
                                                                                 ifelse(age_ratio_0_5.pvalue <= 0.001, 10, 0)))))
  }
  if (c("age_ratio_2_5.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_2_5.pvalue = ifelse(age_ratio_2_5.pvalue > 0.1, 0,
                                                                   ifelse(age_ratio_2_5.pvalue > 0.05, 2,
                                                                          ifelse(age_ratio_2_5.pvalue > 0.001, 5, 
                                                                                 ifelse(age_ratio_2_5.pvalue <= 0.001, 10, 0)))))
  }
  if (c("age_ratio_5_10.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_5_10.pvalue = ifelse(age_ratio_5_10.pvalue > 0.1, 0,
                                                                    ifelse(age_ratio_5_10.pvalue > 0.05, 2,
                                                                           ifelse(age_ratio_5_10.pvalue > 0.001, 5, 
                                                                                  ifelse(age_ratio_5_10.pvalue <= 0.001, 10, 0)))))
  }
  if (c("mean_hh_size.pvalue") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_mean_hh_size.pvalue = ifelse(mean_hh_size.pvalue > 0.05, 0,
                                                                  ifelse(mean_hh_size.pvalue > 0.001, 2,
                                                                         ifelse(mean_hh_size.pvalue > 1e-04, 5,
                                                                                ifelse(mean_hh_size.pvalue <= 1e-04, 10, 0)))))
  }
  if (c("prop_join_people") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_joiners = ifelse(prop_join_people < 10, 0,
                                                           ifelse(prop_join_people < 20, 2,
                                                                  ifelse(prop_join_people < 30, 5,
                                                                         ifelse(prop_join_people >= 30, 10, 0)))))
  }
  if (c("prop_left_people") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_leavers = ifelse(prop_left_people < 10, 0,
                                                           ifelse(prop_left_people < 20, 2,
                                                                  ifelse(prop_left_people < 30, 5,
                                                                         ifelse(prop_left_people >= 30, 10, 0)))))
  }
  if (length(setdiff(c("poisson_pvalues.deaths"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_poisson_pvalues.deaths = ifelse(poisson_pvalues.deaths > 0.1, 0,
                                                                     ifelse(poisson_pvalues.deaths > 0.05, 2,
                                                                            ifelse(poisson_pvalues.deaths > 0.001, 5,
                                                                                   ifelse(poisson_pvalues.deaths <= 0.001, 10, 0)))))
  }
  if (length(setdiff(mort_plaus_vars, names(df))) == 0) {
    df <- df %>% dplyr::mutate(mort_plaus_score = plaus_cdr + plaus_prop_hh_flag_deaths + plaus_sex_ratio.pvalue + 
                                 plaus_age_ratio_0_5.pvalue + plaus_age_ratio_2_5.pvalue + plaus_age_ratio_5_10.pvalue +
                                 plaus_mean_hh_size.pvalue + plaus_prop_joiners +
                                 plaus_prop_leavers + plaus_poisson_pvalues.deaths,
                               mort_plaus_cat = ifelse(mort_plaus_score >= 0 & mort_plaus_score < 10, "Excellent (0-<10)", 
                                                       ifelse(mort_plaus_score >= 10 & mort_plaus_score < 20, "Good (10-<20)",
                                                              ifelse(mort_plaus_score >= 20 & mort_plaus_score < 25, "Acceptable (20 - <25)", 
                                                                     ifelse(mort_plaus_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for mortality plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(mort_plaus_vars, names(df)))
  }
  if (c("sd_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_sd_fcs = dplyr::case_when(sd_fcs < 8 ~ 3,
                                                               sd_fcs >= 8 & sd_fcs < 9 ~ 2, 
                                                               sd_fcs >= 9 & sd_fcs < 14 ~ 0,
                                                               sd_fcs >= 14 & sd_fcs < 16 ~ 2,
                                                               sd_fcs >= 16 ~ 3,
                                                               TRUE ~ 0)) 
  }
  if (c("flag_low_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_low_fcs = dplyr::case_when(flag_low_fcs < 2 ~ 0,
                                                                     flag_low_fcs >= 2 & flag_low_fcs < 10 ~ 2,
                                                                     flag_low_fcs >= 10 ~ 4,
                                                                     TRUE ~ 0))
  }
  if (c("flag_high_fcs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_high_fcs = dplyr::case_when(flag_high_fcs < 2 ~ 0,
                                                                      flag_high_fcs >= 2 & flag_high_fcs < 10 ~ 1,
                                                                      flag_high_fcs >= 10 ~ 2,
                                                                      TRUE ~ 0))
  }
  if (c("flag_sd_foodgroup") %in% names(df)) { # maybe only 3 cat
    df <- df %>% dplyr::mutate(plaus_flag_sd_foodgroup = dplyr::case_when(flag_sd_foodgroup < 2 ~ 0,
                                                                          flag_sd_foodgroup >= 2 & flag_sd_foodgroup < 10 ~ 4,
                                                                          flag_sd_foodgroup >= 10 ~ 6, 
                                                                          TRUE ~ 0))
  }
  if (c("flag_meat_cereal_ratio") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_meat_cereal_ratio = dplyr::case_when(flag_meat_cereal_ratio < 2 ~ 0,
                                                                               flag_meat_cereal_ratio >= 2 & flag_meat_cereal_ratio < 10 ~ 2,
                                                                               flag_meat_cereal_ratio >= 10 ~ 4, 
                                                                               TRUE ~ 0))
  }
  if (c("flag_low_sugar_cond_hdds") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_low_sugar_cond_hdds = dplyr::case_when(flag_low_sugar_cond_hdds < 2 ~ 0,
                                                                                 flag_low_sugar_cond_hdds >= 2 & flag_low_sugar_cond_hdds < 10 ~ 2,
                                                                                 flag_low_sugar_cond_hdds >= 10 ~ 4,
                                                                                 TRUE ~ 0))
  }
  fcs_plaus_vars <- c("plaus_sd_fcs", "plaus_flag_low_fcs", 
                      "plaus_flag_high_fcs", "plaus_flag_sd_foodgroup", 
                      "plaus_flag_meat_cereal_ratio",
                      "plaus_flag_low_sugar_cond_hdds")
  if (length(setdiff(c(fcs_plaus_vars), names(df))) < 6) {
    plaus_nms <- intersect(fcs_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_fcs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_high_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_high_rcsi = dplyr::case_when(flag_high_rcsi < 2 ~ 0, 
                                                                       flag_high_rcsi >= 2 & flag_high_rcsi < 10 ~ 4,
                                                                       flag_high_rcsi >= 10 ~ 5, 
                                                                       TRUE ~ 0))
  }
  if (c("flag_sd_rcsicoping") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_sd_rcsicoping = dplyr::case_when(flag_sd_rcsicoping < 2 ~ 0, 
                                                                           flag_sd_rcsicoping >= 2 & flag_sd_rcsicoping < 10 ~ 4, 
                                                                           flag_sd_rcsicoping >= 10 ~ 6,
                                                                           TRUE ~ 0))
  }
  if (c("sd_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_sd_rcsi = dplyr::case_when(sd_rcsi < 8 ~ 3,
                                                                sd_rcsi >= 8 & sd_rcsi < 9 ~ 2,
                                                                sd_rcsi >= 9 & sd_rcsi < 14 ~ 0, 
                                                                sd_rcsi >= 14 & sd_rcsi < 16 ~ 2,
                                                                sd_rcsi >= 16 ~ 3,
                                                                TRUE ~ 0))
  }
  if (c("flag_protein_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_protein_rcsi = dplyr::case_when(flag_protein_rcsi < 2 ~ 0, 
                                                                          flag_protein_rcsi >= 2 & flag_protein_rcsi < 10 ~ 2, 
                                                                          flag_protein_rcsi >= 10 ~ 3, 
                                                                          TRUE ~ 0))
  }
  if (c("flag_rcsi_children") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_rcsi_children = dplyr::case_when(flag_rcsi_children < 2 ~ 0, 
                                                                           flag_protein_rcsi >= 2 & flag_protein_rcsi < 10 ~ 2, 
                                                                           flag_protein_rcsi >= 10 ~ 3, 
                                                                          TRUE ~ 0))
  }
  rcsi_plaus_vars <- c("plaus_flag_protein_rcsi", "plaus_flag_sd_rcsicoping", 
                       "plaus_flag_high_rcsi", "plaus_sd_rcsi","plaus_flag_rcsi_children")
  if (length(setdiff(c(rcsi_plaus_vars), names(df))) < 5) {
    plaus_nms <- intersect(rcsi_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_rcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_severe_hhs") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_severe_hhs = dplyr::case_when(flag_severe_hhs < 1 ~ 0, 
                                                                        flag_severe_hhs >= 1 & flag_severe_hhs < 5 ~ 6, 
                                                                        flag_severe_hhs >= 5 & flag_severe_hhs < 10 ~ 8, 
                                                                        flag_severe_hhs >= 10 ~ 10,
                                                                        TRUE ~ 0))
  }

  hhs_plaus_vars <- c("plaus_flag_severe_hhs")
  if (length(setdiff(c(hhs_plaus_vars), names(df))) < 2) {
    plaus_nms <- intersect(hhs_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_hhs = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (c("flag_lcsi_liv_livestock") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_liv_livestock = dplyr::case_when(flag_lcsi_liv_livestock < 2 ~ 0, 
                                                                                flag_lcsi_liv_livestock >= 2 & flag_lcsi_liv_livestock < 10 ~ 2, 
                                                                                flag_lcsi_liv_livestock >= 10 ~ 3, 
                                                                                TRUE ~ 0))
  }
  if (c("flag_lcsi_liv_agriculture") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_liv_agriculture = dplyr::case_when(flag_lcsi_liv_agriculture < 2 ~ 0,
                                                                                  flag_lcsi_liv_agriculture >= 2 & flag_lcsi_liv_agriculture < 10 ~ 2, 
                                                                                  flag_lcsi_liv_agriculture >= 10 ~ 3,
                                                                                  TRUE ~ 0))
  }
  if (c("flag_lcsi_liv_agriculture") %in% names(df) | c("flag_lcsi_liv_livestock") %in% names(df)) {
    if (length(setdiff(c("flag_lcsi_liv_agriculture", "flag_lcsi_liv_livestock"), names(df))) == 0) {
      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = dplyr::case_when(plaus_flag_lcsi_liv_livestock >= plaus_flag_lcsi_liv_agriculture ~ plaus_flag_lcsi_liv_livestock, 
                                                                                  plaus_flag_lcsi_liv_livestock < plaus_flag_lcsi_liv_agriculture ~ plaus_flag_lcsi_liv_agriculture,
                                                                                  TRUE ~ 0))
    }
    else if (length(setdiff(c("flag_lcsi_liv_agriculture"), names(df))) == 0) {
      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = plaus_flag_lcsi_liv_agriculture)
    }
    else if (length(setdiff(c("flag_lcsi_liv_livestock"), names(df))) == 0) {
      df <- df %>% dplyr::mutate(plaus_flag_lcsi_agr_livestock = plaus_flag_lcsi_liv_livestock)
    }
  }
  if (c("flag_lcsi_coherence") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_coherence = dplyr::case_when(flag_lcsi_coherence < 2 ~ 0,
                                                                            flag_lcsi_coherence >= 2 & flag_lcsi_coherence < 10 ~ 5, 
                                                                            flag_lcsi_coherence >= 10 ~ 7, 
                                                                            TRUE ~ 0))
  }
  if (c("flag_lcsi_na") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_na = dplyr::case_when(flag_lcsi_na < 2 ~ 0,
                                                                     flag_lcsi_na >= 2 & flag_lcsi_na < 10 ~ 3, 
                                                                     flag_lcsi_na >= 10 ~ 5,
                                                                     TRUE ~ 0))
  }
  if (c("flag_lcsi_severity") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_lcsi_severity = dplyr::case_when(flag_lcsi_severity < 2 ~ 0,
                                                                           flag_lcsi_severity >= 2 & flag_lcsi_severity < 10 ~ 3, 
                                                                           flag_lcsi_severity >= 10 ~ 5,
                                                                           TRUE ~ 0))
  }
  lcs_plaus_vars <- c("plaus_flag_lcsi_agr_livestock", "plaus_flag_lcsi_coherence", 
                      "plaus_flag_lcsi_na", "plaus_flag_lcsi_severity")
  if (length(setdiff(lcs_plaus_vars, names(df))) < 4) {
    plaus_nms <- intersect(lcs_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_lcsi = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>% dplyr::ungroup()
  }
  if (length(setdiff(c("corr.fcs_rcsi", "corr.fcs_rcsi.pvalue"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_corr.fcs_rcsi = ifelse(corr.fcs_rcsi < -0.2 & corr.fcs_rcsi.pvalue < 0.05, 0,
                                                            ifelse(corr.fcs_rcsi < -0.2 & corr.fcs_rcsi.pvalue >= 0.05, 1,
                                                                   ifelse(corr.fcs_rcsi >= -0.2 & corr.fcs_rcsi < 0.2 & corr.fcs_rcsi.pvalue >= 0.05, 2, 
                                                                          ifelse(corr.fcs_rcsi >= -0.2 & corr.fcs_rcsi < 0.2 & corr.fcs_rcsi.pvalue < 0.05, 3,
                                                                                 ifelse(corr.fcs_rcsi >= 0.2 & corr.fcs_rcsi.pvalue >= 0.05, 4,
                                                                                        ifelse(corr.fcs_rcsi >= 0.2 & corr.fcs_rcsi.pvalue < 0.05, 5, 0)))))))
  }
  if (length(setdiff(c("corr.fcs_hhs", "corr.fcs_hhs.pvalue"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_corr.fcs_hhs = ifelse(corr.fcs_hhs < -0.2 & corr.fcs_hhs.pvalue < 0.05, 0,
                                                           ifelse(corr.fcs_hhs < -0.2 & corr.fcs_hhs.pvalue >= 0.05, 1,
                                                                  ifelse(corr.fcs_hhs >= -0.2 & corr.fcs_hhs < 0.2 & corr.fcs_hhs.pvalue >= 0.05, 1.5,
                                                                         ifelse(corr.fcs_hhs >= -0.2 & corr.fcs_hhs < 0.2 & corr.fcs_hhs.pvalue < 0.05, 2,
                                                                                ifelse(corr.fcs_hhs >= 0.2 & corr.fcs_hhs.pvalue >= 0.05, 2.5, 
                                                                                       ifelse(corr.fcs_hhs >= 0.2 & corr.fcs_hhs.pvalue < 0.05, 3, 0)))))))
  }
  if (length(setdiff(c("corr.hhs_rcsi", "corr.hhs_rcsi.pvalue"), names(df))) == 0) {
    df <- df %>% dplyr::mutate(plaus_corr.hhs_rcsi = ifelse(corr.hhs_rcsi > 0.2 & corr.hhs_rcsi.pvalue < 0.05, 0,
                                                            ifelse(corr.hhs_rcsi > 0.2 & corr.hhs_rcsi.pvalue >= 0.05, 1,
                                                                   ifelse(corr.hhs_rcsi > -0.2 & corr.hhs_rcsi <= 0.2 & corr.hhs_rcsi.pvalue < 0.05, 1.5, 
                                                                          ifelse(corr.hhs_rcsi > -0.2 & corr.hhs_rcsi <= 0.2 & corr.hhs_rcsi.pvalue >= 0.05, 2,
                                                                                 ifelse(corr.hhs_rcsi <= -0.2 & corr.hhs_rcsi.pvalue >= 0.05, 2.5, 
                                                                                        ifelse(corr.hhs_rcsi <= -0.2 & corr.hhs_rcsi.pvalue < 0.05, 3, 0)))))))
  }
  if (c("prop_fc_flags") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_fc_flags = ifelse(prop_fc_flags < 0.02, 0,
                                                                   ifelse(prop_fc_flags < 0.1, 2,
                                                                          ifelse(prop_fc_flags >= 0.1, 4, 0))))
  }
  if (c("flag_fcsrcsi_box") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_fcsrcsi_box = dplyr::case_when(flag_fcsrcsi_box < 2 ~ 0,
                                                                         flag_fcsrcsi_box >= 2 & flag_fcsrcsi_box < 10 ~ 1,
                                                                         flag_fcsrcsi_box >= 10 ~ 3,
                                                                         TRUE ~ 0))
  }
  if (c("flag_fcs_rcsi") %in% names(df)) {
    df <- df %>% dplyr::mutate(plaus_flag_fcs_rcsi = dplyr::case_when(flag_fcs_rcsi < 2 ~ 0,
                                                                      flag_fcs_rcsi >= 2 & flag_fcs_rcsi < 10 ~ 1, 
                                                                      flag_fcs_rcsi >= 10 ~ 3,
                                                                      TRUE ~ 0))
  }
  other_fsl_plaus_vars <- c("plaus_prop_fc_flags", "plaus_corr.hhs_rcsi", 
                            "plaus_corr.fcs_hhs", "plaus_corr.fcs_rcsi",
                            "plaus_flag_fcsrcsi_box", "plaus_flag_fcs_rcsi")
  if (length(setdiff(other_fsl_plaus_vars, names(df))) < 6) {
    plaus_nms <- intersect(other_fsl_plaus_vars, names(df))
    df <- df %>% dplyr::rowwise() %>% dplyr::mutate(plaus_other_fsl = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup()
  }
  all_fsl_plaus_vars <- c("plaus_lcsi", "plaus_fcs", "plaus_rcsi", 
                          "plaus_hhs", "plaus_other_fsl")
  if (length(setdiff(all_fsl_plaus_vars, names(df))) < 5) {
    plaus_nms <- intersect(all_fsl_plaus_vars, names(df))
    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(plaus_fsl_score = sum(!!!rlang::syms(plaus_nms), na.rm = TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(plaus_fsl_cat = dplyr::case_when(plaus_fsl_score < 20 ~ "Good",
                                                     plaus_fsl_score >= 20 & plaus_fsl_score < 30 ~ "Moderate",
                                                     plaus_fsl_score >= 30 ~ "Problematic"))
  }
  if (c("mad_ratio.pvalue") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_mad_ratio.pvalue = ifelse(mad_ratio.pvalue > 0.05, 0,
                                                               ifelse(mad_ratio.pvalue > 0.001, 5,
                                                                      ifelse(mad_ratio.pvalue > 1e-04, 10,
                                                                             ifelse(mad_ratio.pvalue <= 1e-04, 20, 0)))))
  }
  if (c("prop_flag_high_mdd_low_mmf") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_prop_flag_high_mdd_low_mmf = ifelse(prop_flag_high_mdd_low_mmf < 0.01, 0,
                                                                         ifelse(prop_flag_high_mdd_low_mmf < 0.05, 5,
                                                                                ifelse(prop_flag_high_mdd_low_mmf < 0.1, 10, 
                                                                                       ifelse(prop_flag_high_mdd_low_mmf >= 0.1, 20, 0)))))
  }
  if (c("age_ratio_under6m_6to23m.pvalue") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_age_ratio_under6m_6to23m.pvalue = ifelse(age_ratio_under6m_6to23m.pvalue > 0.05, 0,
                                                                              ifelse(age_ratio_under6m_6to23m.pvalue > 0.01, 2,
                                                                                     ifelse(age_ratio_under6m_6to23m.pvalue > 0.001, 5,
                                                                                            ifelse(age_ratio_under6m_6to23m.pvalue <= 0.001, 10, 0)))))
  }
  if (c("sd_mdd") %in% colnames(df)) {
    df <- df %>% dplyr::mutate(plaus_sdd_mdd = ifelse(sd_mdd > 1 & sd_mdd < 2, 0,
                                                      ifelse(sd_mdd > 0.8 & sd_mdd < 2.2, 5,
                                                             ifelse(sd_mdd <= 0.8 | sd_mdd >= 2.2, 10, 0))))
  }
  if (c("prop_iycf_caregiver" %in% colnames(df))) {
    df <- df %>% dplyr::mutate(plaus_prop_iycf_caregiver = ifelse(prop_iycf_caregiver >= 0.9, 0,
                                                                  ifelse(prop_iycf_caregiver >= 0.8, 2, 
                                                                         ifelse(prop_iycf_caregiver >= 0.7, 5,
                                                                                ifelse(prop_iycf_caregiver >= 0.5, 10, 10)))))
  }
  iycf_plaus_vars <- c("plaus_sdd_mdd", "plaus_age_ratio_under6m_6to23m.pvalue", 
                       "plaus_sexratio", "plaus_prop_flag_high_mdd_low_mmf", 
                       "plaus_mad_ratio.pvalue")
  if (length(setdiff(iycf_plaus_vars, colnames(df))) == 0) {
    if (!(c("plaus_prop_iycf_caregiver") %in% colnames(df))) {
      df <- df %>% dplyr::mutate(plaus_prop_iycf_caregiver = 10)
      print("No iycf_caregiver variable was available. Plaus penalty of 10 applied assuming this wasn't done during the survey.")
    }
    df <- df %>% dplyr::mutate(iycf_plaus_score = plaus_prop_iycf_caregiver + plaus_sdd_mdd +
                                 plaus_age_ratio_under6m_6to23m.pvalue + plaus_sexratio +
                                 plaus_prop_flag_high_mdd_low_mmf + plaus_mad_ratio.pvalue,
                               iycf_plaus_cat = ifelse(iycf_plaus_score >= 0 & iycf_plaus_score < 10, "Excellent (0-<10)", 
                                                       ifelse(iycf_plaus_score >= 10 & iycf_plaus_score < 15, "Good (10-<15)",
                                                              ifelse(iycf_plaus_score >= 15 & iycf_plaus_score < 25, "Acceptable (15 - <25)", 
                                                                     ifelse(iycf_plaus_score >= 25, "Problematic (>=25)", NA)))))
  }
  else {
    print(paste0("Not all necessary variables for IYCF plausibility score and classification. Skipping this step. The dataframe is missing "))
    print(setdiff(iycf_plaus_vars, names(df)))
    print(setdiff(c("prop_iycf_caregiver"), names(df)))
  }
  return(df)
}

################################################################################
#                             CREATE PLAUSIBILITY TABLES                       #
################################################################################
create_table_nut <- function(.flextable) {
  for (n in 1:12) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 1)
    }
  }
  for (n in 1:12) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 6)
    }
  }
  
  .flextable <- .flextable%>%
    flextable::add_header_row(
      values = c("", "Values", ""),
      colwidths = c(1,4,1), top = TRUE)%>%
    flextable::align(
      j = 2:6,
      align = "center",
      part = "body") %>%
    flextable::align(
      j = 2:6,
      align = "center",
      part = "header") %>%
    flextable::border_inner_h() %>%
    flextable::border_inner_v() %>% flextable::border_inner_h(part="header") %>%
    flextable::surround(part = "header", border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 2)) %>% 
    flextable::surround(part = "header", border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 2)) %>% 
    flextable::surround(part = "header", border.left  = flextable::fp_border_default(color= "black",
                                                               style = "solid",
                                                               width = 2)) %>% 
    flextable::surround(part = "header", border.right  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 2)) %>% 
    flextable::surround(i = 1, j = 1:6, border.top = flextable::fp_border_default(color= "black",
                                                            style = "solid",
                                                            width = 3)) %>% 
    flextable::surround(i = 12, j = 1:6, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 1:12, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                               style = "solid",
                                                               width = 3)) %>% 
    flextable::surround(i = 1:12, j = 6, border.right  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 14, j = 1:6, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 13:14, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 13:14, j = 6, border.right  = flextable::fp_border_default(color= "black",
                                                                 style = "solid",
                                                                 width = 3)) %>% 
    flextable::bold(j = 1) %>%
    flextable::bold(j = "Score") %>%
    flextable::bold(part = "header")
  
  return(.flextable)

}

create_table_fsl <- function(.flextable) {
  .flextable <- flextable::merge_at(.flextable, i = 1:12, j = 9)
  .flextable <- flextable::merge_at(.flextable, i = 13:22, j = 9) 
  .flextable <- flextable::merge_at(.flextable, i = 23:24, j = 9) 
  .flextable <- flextable::merge_at(.flextable, i = 25:34, j = 9) 
  .flextable <- flextable::merge_at(.flextable, i = 35:46, j = 9) 
  .flextable <- flextable::merge_at(.flextable, i = 47, j = 4:5)
  .flextable <- flextable::merge_at(.flextable, i = 48, j = 4:5)
  .flextable <- flextable::merge_at(.flextable, i = 47, j = 8:9)
  .flextable <- flextable::merge_at(.flextable, i = 48, j = 8:9)
  for (i in 1:36) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 2:3)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 6:7)
  }
  
  for (n in 1:46) {
    k <- as.numeric(n + 1)
    if ((n %% 2) == 0) {
      next
    } else {
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 1)
      .flextable <- flextable::merge_at(.flextable, i = n:k, j = 8)
    }
  }
  
  for (i in 43:48) {
    .flextable <- flextable::merge_at(.flextable, i = i, j = 2:3)
    .flextable <- flextable::merge_at(.flextable, i = i, j = 6:7)
  }
  
  .flextable <- .flextable %>% 
    flextable::delete_part(part = "header")%>%
    flextable::add_header_lines() %>%
    flextable::add_header_row(
      values = c("Criteria", "Excellent", "Good","Acceptable","Problematic","Score","Indicator Score"),
      colwidths = c(1,2,1,1,2,1,1), top = TRUE) %>%
    flextable::add_header_row(
      values = c("", "Values", "", ""),
      colwidths = c(1,6,1,1), top = TRUE)%>%
    flextable::align(
      j = 2:9,
      align = "center",
      part = "body") %>%
    flextable::align(
      j = 2:9,
      align = "center",
      part = "header") %>%
    flextable::align(
      j = 7,
      align = "center",
      part = "header") %>%
    flextable::border_inner_h() %>%
    flextable::border_inner_v() %>%
    flextable::border_outer() %>% flextable::border_inner_h(part="header") %>%
    flextable::surround(part = "header", border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 2)) %>% 
    flextable::surround(part = "header", border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 2)) %>% 
    flextable::surround(part = "header", border.left  = flextable::fp_border_default(color= "black",
                                                               style = "solid",
                                                               width = 2)) %>% 
    flextable::surround(part = "header", border.right  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 2)) %>% 
    flextable::surround(i = 1, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                            style = "solid",
                                                            width = 3)) %>% 
    flextable::surround(i = 12, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 1:12, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                               style = "solid",
                                                               width = 3)) %>% 
    flextable::surround(i = 1:12, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 13, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 3)) %>% 
    flextable::surround(i = 22, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 13:22, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 13:22, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                 style = "solid",
                                                                 width = 3)) %>% 
    flextable::surround(i = 23, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 3)) %>% 
    flextable::surround(i = 24, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 23:24, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 23:24, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                 style = "solid",
                                                                 width = 3)) %>% 
    flextable::surround(i = 25, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 3)) %>% 
    flextable::surround(i = 34, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 25:34, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 25:34, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                 style = "solid",
                                                                 width = 3)) %>% 
    flextable::surround(i = 35, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 3)) %>% 
    flextable::surround(i = 46, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 35:46, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>% 
    flextable::surround(i = 35:46, j = 9, border.right  = flextable::fp_border_default(color= "black",
                                                                 style = "solid",
                                                                 width = 3)) %>% 
    flextable::surround(i = 47, j = 1:9, border.top = flextable::fp_border_default(color= "black",
                                                             style = "solid",
                                                             width = 3)) %>%
    flextable::surround(i = 48, j = 1:9, border.bottom = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>%
    flextable::surround(i = 47:48, j = 8, border.right  = flextable::fp_border_default(color= "black",
                                                                 style = "solid",
                                                                 width = 3)) %>%
    flextable::surround(i = 47:48, j = 1, border.left  = flextable::fp_border_default(color= "black",
                                                                style = "solid",
                                                                width = 3)) %>%
    
    flextable::bold(j = 1) %>%
    flextable::bold(j = "Score") %>%
    flextable::bold(part = "header")
  
  return(.flextable)
}

################################################################################
#                             SOME UPDATED PLOTS                               #
################################################################################
plot_anthro_age_distribution_phu <- function (df, index, file_path = NULL, wdth = NULL, hght = NULL, 
                                              title_name = NULL) 
{
  if (length(setdiff(c("age_months"), colnames(df))) > 0) {
    df <- df %>% dplyr::mutate(age_months = "")
  }
  else {
    df <- df %>% dplyr::mutate(age_months = ifelse(age_months - 
                                                     floor(age_months) >= 0.96, as.numeric(ceiling(age_months)), 
                                                   as.numeric(floor(age_months))))
  }
  df <- df %>% dplyr::mutate(age_group = ifelse(is.na(age_months), 
                                                NA, ifelse(age_months < 6, "<6 months", ifelse(age_months > 
                                                                                                 5 & age_months < 18, "6-17 months", ifelse(age_months > 
                                                                                                                                              17 & age_months < 30, "18-29 months", ifelse(age_months > 
                                                                                                                                                                                             29 & age_months < 42, "30-41 months", ifelse(age_months > 
                                                                                                                                                                                                                                            41 & age_months < 54, "42-53 months", ifelse(age_months > 
                                                                                                                                                                                                                                                                                           53 & age_months < 60, "54-59 months", ifelse(age_months > 
                                                                                                                                                                                                                                                                                                                                          59, ">59 months", NA)))))))), age_group = as.factor(age_group), 
                             age_group = factor(age_group, levels = c("<6 months", 
                                                                      "6-17 months", "18-29 months", "30-41 months", "42-53 months", 
                                                                      "54-59 months")))
  if (index == "muac") {
    df <- df %>% dplyr::filter(muac_flag != 1) %>% 
      dplyr::mutate(cat = ifelse(is.na(gam_muac_noflag), 
                                 NA, ifelse(sam_muac_noflag == "1", "SAM", 
                                            ifelse(mam_muac_noflag == "1", "MAM", 
                                                   ifelse(gam_muac_noflag == "0", "Normal", 
                                                          NA)))), cat = as.factor(cat), cat = factor(cat, 
                                                                                                     levels = c("SAM", "MAM", "Normal")))
    title <- "Nutritional Status: MUAC by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c(SAM = "indianred", MAM = "khaki", 
                       Normal = "palegreen4")
  }
  else if (index == "mfaz") {
    df <- df %>% dplyr::filter(flag_sd_mfaz != 
                                 1) %>% dplyr::mutate(cat = ifelse(is.na(global_mfaz_noflag), 
                                                                   NA, ifelse(severe_mfaz_noflag == "1", "SAM", 
                                                                              ifelse(moderate_mfaz_noflag == "1", "MAM", 
                                                                                     ifelse(global_mfaz_noflag == "0", "Normal", 
                                                                                            NA)))), cat = as.factor(cat), cat = factor(cat, 
                                                                                                                                       levels = c("SAM", "MAM", "Normal")))
    title <- "Nutritional Status: MUAC-for-Age by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c(SAM = "indianred", MAM = "khaki", 
                       Normal = "palegreen4")
  }
  else if (index == "wfhz") {
    df <- df %>% dplyr::filter(wfhz_smart_flag != 
                                 1) %>% dplyr::mutate(gam_wfhz_noflag = as.character(gam_wfhz_noflag), 
                                                      mam_wfhz_noflag = as.character(mam_wfhz_noflag), 
                                                      sam_wfhz_noflag = as.character(sam_wfhz_noflag)) %>% 
      dplyr::mutate(cat = ifelse(is.na(gam_wfhz_noflag), 
                                 NA, ifelse(sam_wfhz_noflag == "1", "SAM", 
                                            ifelse(mam_wfhz_noflag == "1", "MAM", 
                                                   ifelse(gam_wfhz_noflag == "0", "Normal", 
                                                          NA)))), cat = as.factor(cat), cat = factor(cat, 
                                                                                                     levels = c("SAM", "MAM", "Normal")))
    title <- "Nutritional Status: Weight-for-Height by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c(SAM = "indianred", MAM = "khaki", 
                       Normal = "palegreen4")
  }
  else if (index == "hfaz") {
    df <- df %>% dplyr::filter(hfaz_smart_flag != 
                                 1) %>% dplyr::mutate(cat = ifelse(is.na(global_stunting_noflag), 
                                                                   NA, ifelse(severe_stunting_noflag == "1", 
                                                                              "Severe Stunting", ifelse(moderate_stunting_noflag == 
                                                                                                          "1", "Moderate Stunting", ifelse(global_stunting_noflag == 
                                                                                                                                             "0", "Normal", NA)))), cat = as.factor(cat), 
                                                      cat = factor(cat, levels = c("Severe Stunting", 
                                                                                   "Moderate Stunting", "Normal")))
    title <- "Nutritional Status: Stunting by Age Group"
    color_groups <- c("Severe Stunting", "Moderate Stunting", 
                      "Normal")
    color_palette <- c(`Severe Stunting` = "indianred", 
                       `Moderate Stunting` = "khaki", Normal = "palegreen4")
  }
  else if (index == "wfaz") {
    df <- df %>% dplyr::filter(wfaz_smart_flag != 
                                 1) %>% dplyr::mutate(cat = ifelse(is.na(global_underweight_noflag), 
                                                                   NA, ifelse(severe_underweight_noflag == "1", 
                                                                              "Severe Underweight", ifelse(moderate_underweight_noflag == 
                                                                                                             "1", "Moderate Underweight", ifelse(global_underweight_noflag == 
                                                                                                                                                   "0", "Normal", NA)))), cat = as.factor(cat), 
                                                      cat = factor(cat, levels = c("Severe Underweight", 
                                                                                   "Moderate Underweight", "Normal")))
    title <- "Nutritional Status: Underweight by Age Group"
    color_groups <- c("Severe Underweight", "Moderate Underweight", 
                      "Normal")
    color_palette <- c(`Severe Underweight` = "indianred", 
                       `Moderate Underweight` = "khaki", Normal = "palegreen4")
  }
  else if (index == "cgam") {
    df <- df %>% dplyr::filter(wfhz_smart_flag != 
                                 1) %>% dplyr::mutate(cat = ifelse(is.na(c_gam), 
                                                                   NA, ifelse(c_sam == "1", "SAM", ifelse(c_mam == 
                                                                                                            "1", "MAM", ifelse(c_gam == "0", "Normal", 
                                                                                                                               NA)))), cat = as.factor(cat), cat = factor(cat, 
                                                                                                                                                                          levels = c("SAM", "MAM", "Normal")))
    title <- "Nutritional Status: Combined GAM by Age Group"
    color_groups <- c("SAM", "MAM", "Normal")
    color_palette <- c(SAM = "indianred", MAM = "khaki", 
                       Normal = "palegreen4")
  }
  df2 <- df %>% dplyr::group_by(age_group, cat) %>% 
    dplyr::summarise(n = dplyr::n()) %>% dplyr::mutate(pct = n/sum(n)) %>% 
    dplyr::rename(`Nutritional Status` = cat)
  g <- ggplot2::ggplot(data = df2, ggplot2::aes(fill = `Nutritional Status`, 
                                                x = age_group, y = pct, order = `Nutritional Status`)) + 
    ggplot2::geom_bar(position = "fill", stat = "identity") + 
    ggplot2::scale_y_continuous(labels = scales::percent, 
                                name = "Nutrition Status") + ggplot2::scale_fill_manual(values = color_palette) + 
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(pct, 
                                                         3) * 100, "% - (", n, ")")), position = ggplot2::position_stack(vjust = 0.5), 
                       size = 3)
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  return(g)
}

plot_age_years_distribution_phu <- function (df, by_group = NULL, min_age = NULL, max_age = NULL, 
          breaks = NULL, file_path = NULL, wdth = NULL, hght = NULL, 
          title_name = NULL) 
{
  if (!(c("age_years") %in% colnames(df))) {
    stop("There is no age_years column in your dataframe. Please check your inputs.")
  }
  if (is.null(min_age)) {
    min_age <- 0
    print("No minimum age specified. Defaulting to 0 years.")
  }
  if (is.null(max_age)) {
    max_age <- 5
    print("No maximum age specified. Defaulting to 5 years.")
  }
  if (is.null(breaks)) {
    breaks <- 1
  }
  df <- df %>% dplyr::filter(.data$age_years >= min_age & 
                               .data$age_years <= max_age)
  if (is.null(by_group)) {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = get("age_years"))) + 
      ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age, 
                                                                                                  max_age, by = ), breaks = seq(min_age, max_age, 
                                                                                                                                by = breaks), limits = c(min_age, max_age))
  }
  else {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = get("age_years"))) + 
      ggplot2::geom_histogram(binwidth = breaks) + ggplot2::scale_x_continuous(minor_breaks = seq(min_age, 
                                                                                                  max_age, by = 1), breaks = seq(min_age, max_age, 
                                                                                                                                 by = breaks), limits = c(min_age, max_age)) + ggplot2::facet_wrap(~get(by_group), 
                                                                                                                                                                                                   ncol = 1)
  }
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  return(g)
}

plot_age_months_distribution_phu <- function (df, by_group = NULL, file_path = NULL, wdth = NULL, age_min = 0,
                                                age_max = 59, hght = NULL, title_name = NULL) 
{
  if (!(c("age_months") %in% colnames(df))) {
    stop("There is no age_months column in your dataframe. Please check your inputs.")
  }
  if (max(df$age_months, na.rm = TRUE) > 59) {
    # print("Ages >59 months detected, removed for this graph.")
    df <- df %>% dplyr::filter(age_months < 60)
  }
  df <- df %>% 
    filter(age_months >= age_min & age_months <= age_max)
  if (is.null(by_group)) {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = age_months)) + 
      ggplot2::geom_histogram(binwidth = 1) + ggplot2::scale_x_continuous(minor_breaks = seq(age_min, 
                                                                                             age_max, by = 1), breaks = seq(age_min, age_max, by = 12), limits = c(age_min, 
                                                                                                                                                             age_max))
  }
  else {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = age_months)) + 
      ggplot2::geom_histogram(binwidth = 1) + ggplot2::scale_x_continuous(minor_breaks = seq(age_min, 
                                                                                             age_max, by = 1), breaks = seq(age_min, age_max, by = 12), limits = c(age_min, 
                                                                                                                                                             age_max)) + ggplot2::facet_wrap(~get(by_group), ncol = 1)
  }
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  return(g)
}


plot_ridge_distribution_phu <- function (df, numeric_cols, name_groups = NULL, name_units = NULL, 
          grouping = NULL, file_path = NULL, wdth = NULL, hght = NULL, 
          title_name = NULL) 
{
  a <- 0
  if (!methods::hasArg(grouping)) {
    df <- df %>% dplyr::mutate(group = "All")
    grouping <- "group"
    a <- 1
  }
  if (is.null(name_groups)) {
    name_groups <- "Groups"
  }
  if (is.null(name_units)) {
    name_units <- "Units"
  }
  df <- df %>% dplyr::select(grouping, numeric_cols) %>% tidyr::gather(key = !!name_groups, 
                                                                       value = !!name_units, numeric_cols)
  g <- ggplot2::ggplot(df, ggplot2::aes(x = get(name_units), 
                                        y = get(name_groups), fill = get(name_groups))) + ggridges::geom_density_ridges() + 
    ggridges::theme_ridges() + ggplot2::xlab(name_units) + 
    ggplot2::ylab(name_groups) + ggplot2::theme(legend.position = "none", 
                                                legend.title = ggplot2::element_text(name_groups))
  if (a == 0) {
    g <- g + ggplot2::facet_wrap(~get(grouping))
  }
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  return(g)
}


plot_correlogram_phu <- function (df, numeric_cols, file_path = NULL, wdth = NULL, hght = NULL, 
          title_name = NULL) 
{
  g <- GGally::ggpairs(data = df, columns = numeric_cols)
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  return(g)
}

plot_agepyramid_phu <- function (df, age_grouping = NULL, filtering = NULL, file_path = NULL, 
          wdth = NULL, hght = NULL, title_name = NULL) 
{
  if (is.null(age_grouping)) {
    age_grouping <- "age_group"
  }
  if (!is.null(filtering)) {
    print("Please note, if using filtering the variable must be coded numerically by 0s and 1s")
    df <- df %>% dplyr::filter(!is.na(.data$age_years)) %>% 
      dplyr::filter(!!rlang::sym(filtering) == 1) %>% 
      dplyr::arrange(.data$sex)
    g <- apyramid::age_pyramid(data = df, age_group = age_grouping, 
                               proportional = TRUE) + ggplot2::ylab(paste0("Proportion of ", 
                                                                           filtering))
  }
  else {
    df <- df %>% dplyr::arrange(.data$sex)
    g <- apyramid::age_pyramid(data = df, age_group = age_grouping, 
                               proportional = TRUE) + ggplot2::ylab(paste0("Proportion of Population "))
  }
  g <- g + ggplot2::scale_fill_manual(name = "Sex", labels = c("Male", 
                                                               "Female"), values = c("#08bcc4", "#ff746c"))
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  return(g)
}

plot_cumulative_distribution_phu <- function (df, index, flags, grouping = NULL, file_path = NULL, 
          wdth = NULL, hght = NULL, title_name = NULL) 
{
  options(warn = -1)
  anthro_cols <- c("wfhz_noflag", "hfaz_noflag", "wfaz_noflag", 
                   "mfaz_noflag", "muac_noflag")
  valid_indexes <- c("wfhz", "hfaz", "wfaz", "mfaz", "nut_muac_cm")
  if (length(setdiff(index, valid_indexes)) != 0) {
    stop("No valid index was specified. Valid inputs include 'wfhz', 'hfaz', 'wfaz', or 'mfaz'.")
  }
  if (is.null(flags)) {
    flags <- "yes"
  }
  else if (length(setdiff(flags, c("yes", "no"))) != 0) {
    stop("Please include a valid input for the flags argument. Use 'yes' to include flagged values in the plot. Use 'no' to exclude flagged values from the plot.")
  }
  if (index == "wfhz") {
    if (flags == "yes") {
      index <- "wfhz"
    }
    else {
      index <- "wfhz_noflag"
    }
  }
  else if (index == "hfaz") {
    if (flags == "yes") {
      index <- "hfaz"
    }
    else {
      index <- "hfaz_noflag"
    }
  }
  else if (index == "wfaz") {
    if (flags == "yes") {
      index <- "wfaz"
    }
    else {
      index <- "wfaz_noflag"
    }
  }
  else if (index == "mfaz") {
    if (flags == "yes") {
      index <- "mfaz"
    }
    else {
      index <- "mfaz_noflag"
    }
  }
  else if (index == "nut_muac_cm") {
    if (flags == "yes") {
      index <- "nut_muac_cm"
    }
    else {
      index <- "muac_noflag"
    }
  }
  df <- df %>% dplyr::rename(indx = {
    {
      index
    }
  })
  if (index == "nut_muac_cm" | index == "muac_noflag") {
    minval <- 6
    maxval <- 22
    brkval <- 0.5
    severe <- 11.5
    moderate <- 12.5
  }
  else {
    minval <- (-6)
    maxval <- 6
    brkval <- 0.5
    severe <- (-3)
    moderate <- (-2)
  }
  g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$indx, color = "Overall")) + 
    ggplot2::stat_ecdf(geom = "step") + ggplot2::geom_vline(xintercept = severe) + 
    ggplot2::geom_vline(xintercept = moderate)
  if (!missing(grouping)) {
    g <- g + ggplot2::stat_ecdf(geom = "step", ggplot2::aes(x = .data$indx, 
                                                            color = as.factor(get(grouping)), group = get(grouping))) + 
      ggplot2::geom_vline(xintercept = severe) + ggplot2::geom_vline(xintercept = moderate)
    g <- g + ggplot2::labs(color = paste0(grouping))
  }
  values <- df %>% dplyr::select(grouping) %>% t %>% c %>% 
    unique()
  colors <- c("red", "blue", "green", "orange", "purple", 
              "dodgerblue", "magenta", "cyan", "deeppink", "mediumblue", 
              "darkgreen")
  colors <- colors[1:length(values)]
  colors <- c(colors, "black")
  if (missing(grouping)) {
    colors <- "black"
  }
  g <- g + ggplot2::xlim(c(minval, maxval)) + ggplot2::theme_minimal() + 
    ggplot2::xlab(index) + ggplot2::ylab("% Cumulative Proportion")
  g <- g + ggplot2::scale_x_continuous(breaks = seq(minval, 
                                                    maxval, by = brkval))
  g <- g + ggplot2::scale_color_manual(values = colors)
  if (!is.null(title_name)) {
    g <- g + ggplot2::ggtitle(title_name)
  }
  if (is.null(wdth)) {
    wdth <- 5
  }
  if (is.null(hght)) {
    hght <- 5
  }
  if (!is.null(file_path)) {
    ggplot2::ggsave(filename = file_path, width = wdth, 
                    height = hght)
  }
  options(warn = 0)
  return(g)
}


