source("src/init.R")
options(warn=-1)

# ------------------------------------------------------------------------------
# AFTER RECEIVING filled followup requests:

fu.edited <- load.requests(dir.responses, "followup", sheet = "Follow-up")

if(is.data.frame(fu.edited)){
  
  fu.edited <- fu.edited %>%
    mutate(modified = !is.na(invalid) | (!is.na(new.value) & new.value != old.value),
           missed = rowSums(is.na(across(c(new.value,invalid,loops_to_remove)))) == 3 & !is.na(old.value),
           check = !is.na(invalid) & !is.na(new.value) & modified) 
  
  fu.check <- fu.edited %>% filter(check)
  fu.missed <- fu.edited %>% filter(missed)
  if(nrow(fu.check)>0) {
    if(language_assessment == "English"){
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      stop("Please recheck the file that you have filled, \nit is clear that some entries have both changed values and invalid.")
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    } else {
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      stop("Veuillez revérifier le fichier que vous avez rempli, \nil est clair que certaines entrées ont changé de valeur et sont invalides.")
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    }
  }
  if(nrow(fu.missed)>0) {
    if(language_assessment == "English"){
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      stop("Please recheck the file that you have filled, \nit is clear that there are some rows missing to be filled.")
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    } else {
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      stop("Veuillez revérifier le fichier que vous avez rempli, \nil est clair qu'il manque des lignes à remplir.")
      cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    }
  }
  # go ahead only if no warnings found above...
  fu.edited <- fu.edited %>% filter(modified) %>%
    mutate(new.value = ifelse(!is.na(invalid) & invalid == "yes", NA, new.value),
           issue = ifelse(!is.na(explanation), paste0("Information followed up with field team: ", explanation), issue))
  
  ## check and separate num_died removed loops
  
  if(!is.null(raw.died_member)) {
    fu.edited_died <- fu.edited %>% 
      filter(variable == "num_died" &
             as.numeric(new.value) < as.numeric(old.value)) %>% 
      select(uuid, enum_colname,loops_to_remove) %>% 
      tidyr::separate_rows(loops_to_remove, sep = ";\\s*") %>% 
      mutate(reason = "Deleted death entries after followup.") %>% 
      select(-loops_to_remove)
    deletion.whole <- bind_rows(deletion.whole,fu.edited_died)
    openxlsx::write.xlsx(deletion.whole,paste0("output/deletion_log/",dataset.name.short,"_deletion_log_",strings['out_date'],".xlsx"),overwrite = T)
  }
  
  fu.edited <- fu.edited %>% 
    filter(is.na(loops_to_remove))
  
  # recode select_multiples
  fu.multiple <- fu.edited %>% filter(get.type(variable) == "select_multiple")
  
  if(any(is.na(fu.multiple$invalid))) stop("Select multiples cannot be invalid. Set them to 0 instead!")
  cleaning.log.followups_multiple <- tibble()
  
  if(nrow(fu.multiple)>0){
    main_row <- pull.raw(ifelse(is.na(fu.multiple[1,]$loop_index), fu.multiple[1,]$uuid, fu.multiple[1,]$loop_index))
    for(r in 1:nrow(fu.multiple)){
      x <- fu.multiple[r,]
      if(is.na(x$loop_index)){
        if(main_row$uuid %!=na% x$uuid) main_row <- pull.raw(x$uuid)
      } else{
        if (main_row$loop_index %!=na% x$loop_index) {
          main_row <- pull.raw(x$loop_index)
        }
      } 
      cummulative_variable  <-  str_split(x$variable, "/", simplify = T)[1]
      cchoice  <-  str_split(x$variable, "/", simplify = T)[2]
      # TODO: improve recoding here (use group_by and add/remove.choices) to not have to apply changes in every iteration
      if(x$new.value == "0"){
        cl <- recode.multiple.remove.choices(main_row, cummulative_variable, cchoice, issue = x$issue)
        main_row <- main_row %>% apply.changes(cl)
        cleaning.log.followups_multiple <- rbind(cleaning.log.followups_multiple, cl)
      }else{
        cl <- recode.multiple.add.choices(main_row, cummulative_variable, cchoice, issue = x$issue)
        main_row <- main_row %>% apply.changes(cl)
        cleaning.log.followups_multiple <- rbind(cleaning.log.followups_multiple, cl)
      }
    }
  }
  cleaning.log.followups <- fu.edited %>% filter(get.type(variable) != "select_multiple") %>% select(any_of(CL_COLS))
  cleaning.log.followups <- rbind(cleaning.log.followups, cleaning.log.followups_multiple)
  
  # apply changes
  raw.main  <- raw.main  %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.main)))
  if(!is.null(raw.water_count_loop)){
    raw.water_count_loop <- raw.water_count_loop %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.water_count_loop)), is.loop = T)
  }
  raw.child_nutrition <- raw.child_nutrition %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.child_nutrition)), is.loop = T)
  if(!is.null(raw.died_member)){
    raw.died_member <- raw.died_member %>% apply.changes(cleaning.log.followups %>% filter(variable %in% names(raw.died_member)), is.loop = T)
  }
  
  ##------------------------------------------------------------------------------
  ## 4C) Dependencies
  cleaning.log.dependency <- tibble()
  
  # Dependency related to RCSI_SCORE Turned to NA and change all FCS_columns to NA (CHECK_1)
  if("rcsi_score" %in% names(raw.main)){
    check <- raw.main %>% 
      filter(is.na(rcsi_score)) %>% 
      dplyr::mutate(flag = ifelse(!is.na(fsl_rcsi_lessquality), 1, 0)) %>% 
      filter(flag == 1)
    
    rcsi_columns <- names(raw.main)[str_detect(names(raw.main),"rcsi_")]
    if(nrow(check)>0){
      for(i in rcsi_columns){
        check_new <- check %>% 
          dplyr::mutate(loop_index = NA,
                 variable = i,
                 old.value = as.character(!!rlang::sym(i)),
                 new.value = NA,
                 issue = "Dependency") %>% 
          dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
        cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
      }
    }
  }
  
  # Dependency related to RCSI_SCORE Turned to NA and change all FCS_columns to NA (CHECK_1)
  if("fcs_meat" %in% names(raw.main)){
    check <- raw.main %>% 
      filter(is.na(fcs_meat)) %>% 
      dplyr::mutate(flag = ifelse(!is.na(fsl_fcs_cereal), 1, 0)) %>% 
      filter(flag == 1)
    
    fcs_columns <- names(raw.main)[str_detect(names(raw.main),"fcs_")]
    if(nrow(check)>0){
      for(i in fcs_columns){
        check_new <- check %>% 
          dplyr::mutate(loop_index = NA,
                        variable = i,
                        old.value = as.character(!!rlang::sym(i)),
                        new.value = NA,
                        issue = "Dependency") %>% 
          dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
        cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
      }
    }
  }
  
  # Dependency related to RCSI_SCORE Turned to NA and change all FCS_columns to NA (CHECK_1)
  if("fcs_dairy" %in% names(raw.main)){
    check <- raw.main %>% 
      filter(is.na(fcs_dairy)) %>% 
      dplyr::mutate(flag = ifelse(!is.na(fsl_fcs_cereal), 1, 0)) %>% 
      filter(flag == 1)
    
    fcs_columns <- names(raw.main)[str_detect(names(raw.main),"fcs_")]
    if(nrow(check)>0){
      for(i in fcs_columns){
        check_new <- check %>% 
          dplyr::mutate(loop_index = NA,
                        variable = i,
                        old.value = as.character(!!rlang::sym(i)),
                        new.value = NA,
                        issue = "Dependency") %>% 
          dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
        cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
      }
    }
  }
  # Dependency related to FCS_SCORE Turned to NA and change all fcs_columns to NA (CHECK_1_and_3)
  if("fcs_score" %in% names(raw.main)){
    check <- raw.main %>% 
      filter(is.na(fcs_score)) %>% 
      dplyr::mutate(flag = ifelse(!is.na(fsl_fcs_cereal), 1, 0)) %>% 
      filter(flag == 1)
    fcs_columns <- names(raw.main)[str_detect(names(raw.main),"fcs_")]
    if(nrow(check)>0){
      for(i in fcs_columns){
        check_new <- check %>% 
          dplyr::mutate(loop_index = NA,
                 variable = i,
                 old.value = as.character(!!rlang::sym(i)),
                 new.value = NA,
                 issue = "Dependency") %>% 
          dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
        cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
      }
    }
  }
  
  # Dependency related to wash_water_source Turned to NA and change all wash_water_source_other to NA (CHECK_4)
  
  check <- raw.main %>% 
    filter(is.na(wash_water_source)) %>% 
    dplyr::mutate(flag = ifelse(!is.na(wash_water_source_other), 1, 0)) %>% 
    filter(flag == 1)
  
  if(nrow(check)>0){
      check_new <- check %>% 
        dplyr::mutate(loop_index = NA,
               variable = "wash_water_source_other",
               old.value = wash_water_source_other,
               new.value = NA,
               issue = "Dependency") %>% 
        dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
      cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
  }
  
  # Dependency related to nut_edema_confirm Turned to NA and change all edema to NA (CHECK_5)
  
  check <- raw.child_nutrition %>% 
    filter(is.na(nut_edema_confirm)) %>% 
    dplyr::mutate(flag = ifelse(nut_edema != "no", 1, 0)) %>% 
    filter(flag == 1)
  
  if(nrow(check)>0){
    check_new <- check %>% 
      dplyr::mutate(variable = "nut_edema",
             old.value = nut_edema,
             new.value = NA,
             issue = "Dependency") %>% 
      dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
    cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
  }
  
  # Dependency related to nut_edema_confirm Turned to NA and change all edema to NA (CHECK_5)
  
  check <- raw.child_nutrition %>% 
    filter(is.na(nut_edema_confirm)) %>% 
    dplyr::mutate(flag = ifelse(nut_muac_cm >= 12.5 & !is.na(nut_cmam_enrollment), 1, 0)) %>% 
    filter(flag == 1)
  
  if(nrow(check)>0){
    check_new <- check %>% 
      dplyr::mutate(variable = "nut_cmam_enrollment",
                    old.value = nut_cmam_enrollment,
                    new.value = NA,
                    issue = "Dependency") %>% 
      dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
    cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
  }
  
  # Dependency related to Num_Died Turned to NA and change all loop of raw.died_member to NA (CHECK_6)
  if(!is.null(raw.died_member)){
    uuid_check <- raw.main %>% 
      filter(is.na(num_died)) %>% 
      pull(uuid)
    
    check <- raw.died_member %>% 
      filter(uuid %in% uuid_check)
    columns_loop_died <- c("died_position","sex_died","age_died_years","known_dob_died","dob_died_exact","dob_died_approx",
                           "dob_died","age_months_died","age_days_died","calc_final_age_years_died","days_since_recall_died",
                           "days_since_born_died","days_recall_born_died","ind_born_died","died_present","date_death_yn","date_death_exact",
                           "date_death_approx","final_date_death","cause_death","cause_death_other","location_death","location_death_other",
                           "died_healthcare_yn","died_healthcare_location","died_healthcare_other","died_no_healthcare","died_no_healthcare_other","death_details")
    if(nrow(check)>0){
      for(i in columns_loop_died){
        check_new <- check %>% 
          dplyr::mutate(variable = i,
                 old.value = as.character(!!rlang::sym(i)),
                 new.value = NA,
                 issue = "Dependency") %>% 
          dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
        cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
      }
    }
  }
  
  # Dependency related to Cause_death Turned to NA and change all Cause_death_other to NA (CHECK_7)
  if(!is.null(raw.died_member)){
    check <- raw.died_member %>% 
      filter(is.na(cause_death)) %>% 
      dplyr::mutate(flag = ifelse(!is.na(cause_death_other), 1, 0)) %>% 
      filter(flag == 1)
    
    if(nrow(check)>0){
      check_new <- check %>% 
        dplyr::mutate(variable = "cause_death_other",
               old.value = cause_death_other,
               new.value = NA,
               issue = "Dependency") %>% 
        dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
      cleaning.log.dependency <- rbind(cleaning.log.dependency,check_new)
      }
  }
  
  raw.main <- raw.main %>% 
    apply.changes(cleaning.log.dependency)
  
  raw.child_nutrition <- raw.child_nutrition %>% 
    apply.changes(cleaning.log.dependency, is.loop = T)
  if(!is.null(raw.died_member)){
    raw.died_member <- raw.died_member %>% 
      apply.changes(cleaning.log.dependency, is.loop = T)
  }
  
  save.image("output/data_log/final_logical.rda")
  options(warn=0)
  if(language_assessment == "English"){
    cat("\n\n#############################################################################################\n")
    cat("Direct logical checks are cleaned. Please go ahead and run the outlier checks.\n")
    cat("#############################################################################################\n")
  }else{
    cat("\n\n#############################################################################################\n")
    cat("Les contrôles logiques directs sont nettoyés. Veuillez poursuivre et exécuter \nles contrôles des valeurs aberrantes.\n")
    cat("#############################################################################################\n")
  }
} else {
  save.image("output/data_log/final_logical.rda")
  # options(warn=0)
  if(language_assessment == "English"){
    cat("\n\n#############################################################################################\n")
    cat("Direct logical checks are cleaned. Please go ahead and run the outlier checks.\n")
    cat("#############################################################################################\n")
  }else{
    cat("\n\n#############################################################################################\n")
    cat("Les contrôles logiques directs sont nettoyés. Veuillez poursuivre et exécuter \nles contrôles des valeurs aberrantes.\n")
    cat("#############################################################################################\n")
  }
}
