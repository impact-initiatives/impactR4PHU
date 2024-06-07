source("src/init.R")
options(warn=-1)
###-------------------------------------------------------------------------------
# 4) LOGIC CHECKS
################################################################################

# 4A) direct cleaning

## Change to match your assessment

cleaning.log.checks.direct <- tibble()

int_cols_main  <- tool.survey %>% filter(type == "integer" & datasheet == "main") %>% pull(name)
int_cols_hh_roster  <- tool.survey %>% filter(type == "integer" & datasheet == "hh_roster") %>% pull(name)
if(!is.null(raw.water_count_loop)){
  int_cols_water_count_loop  <- tool.survey %>% filter(type == "integer" & datasheet == "water_count_loop") %>% pull(name)
}
if(!is.null(raw.died_member)){
  int_cols_died_member  <- tool.survey %>% filter(type == "integer" & datasheet == "died_member") %>% pull(name)
}


### Cleaning of 999s to NAs
cl_999s <- bind_rows(recode.set.NA.if(raw.main, int_cols_main, "999", "replacing 999 with NA"),
                     recode.set.NA.if(raw.hh_roster, int_cols_hh_roster, "999", "replacing 999 with NA"))

if(!is.null(raw.water_count_loop)){
  cl_999s <- bind_rows(cl_999s,
                       recode.set.NA.if(raw.water_count_loop, int_cols_water_count_loop, "999", "replacing 999 with NA"))
}
if(!is.null(raw.died_member)){
cl_999s <- bind_rows(cl_999s,
                     recode.set.NA.if(raw.died_member, int_cols_died_member, "999", "replacing 999 with NA"))
}


cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_999s)%>% 
  dplyr::mutate(new.value = as.character(new.value))

raw.flag <- raw.main
### Food Security Direct changes
fcs_check_columns <- c("fsl_fcs_cereal",
                       "fsl_fcs_legumes",
                       "fsl_fcs_veg",
                       "fsl_fcs_fruit",
                       "fsl_fcs_meat",
                       "fsl_fcs_dairy",
                       "fsl_fcs_sugar",
                       "fsl_fcs_oil")

if(all(fcs_check_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
    impactR4PHU::add_fcs(cutoffs = "normal")
}

rcsi_check_columns <- c("fsl_rcsi_lessquality",
                        "fsl_rcsi_borrow",
                        "fsl_rcsi_mealsize",
                        "fsl_rcsi_mealadult",
                        "fsl_rcsi_mealnb")

if(all(rcsi_check_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
    impactR4PHU::add_rcsi()
}

hhs_check_columns <- c("fsl_hhs_nofoodhh",
                       "fsl_hhs_nofoodhh_freq",
                       "fsl_hhs_sleephungry",
                       "fsl_hhs_sleephungry_freq",
                       "fsl_hhs_alldaynight",
                       "fsl_hhs_alldaynight_freq")

if(all(hhs_check_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
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

if(all(lcsi_check_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
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

if(all(hdds_check_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
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



if(all(fcm_check_1_columns %in% names(raw.main)) |
   all(fcm_check_2_columns %in% names(raw.main)) |
   all(fcm_check_3_columns %in% names(raw.main)) |
   all(fcm_check_4_columns %in% names(raw.main))|
   all(fcm_check_5_columns %in% names(raw.main)) |
   all(fcm_check_6_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
    impactR4PHU::add_fcm_phase()
}

fclcm_check_columns <- c("fsl_fc_phase",
                         "fsl_lcsi_cat")
if(all(fclcm_check_columns %in% names(raw.main))) {
  raw.flag <- raw.flag %>% 
    impactR4PHU::add_fclcm_phase()
}

## FCS
raw.flag.fcs <- raw.flag %>% 
  impactR4PHU::check_fsl_flags(tool.survey = tool.survey) ## CHANGE by removing date_dc_date

## WASH
if(!is.null(raw.water_count_loop)){
  raw.flag.wash <- raw.main %>% 
    impactR4PHU::check_wash_flags(data_container_loop = raw.water_count_loop)
} else {
  raw.flag.wash <- raw.main %>% 
    impactR4PHU::check_wash_flags()
}

## NUTRITION
if(all(c("nut_muac_cm","nut_edema_confirm") %in% names(raw.child_nutrition))){
  raw.flag.nut <- raw.child_nutrition %>% 
    impactR4PHU::add_muac(nut_muac_cm = "nut_muac_cm",
                          edema_confirm = "nut_edema_confirm") %>%
    impactR4PHU::add_mfaz(nut_muac_cm = "nut_muac_cm",edema_confirm = "nut_edema_confirm") %>% 
    impactR4PHU::check_anthro_flags()
}
#### FCS
### FCS score is 0. 

fcs_columns <- names(raw.flag.fcs)[which(stringr::str_detect(names(raw.flag.fcs),"fsl_fcs_"))]
if("fsl_fcs_score" %in% names(raw.flag.fcs)) {
  check <- raw.flag.fcs %>%
    filter(fsl_fcs_score == 0)
  cl_fcs_all_0 <- data.frame()
  for (i in 1:nrow(check)) {
    cl <-  recode.set.NA.if(check[i,], fcs_columns, check[i,fcs_columns], "replacing fcs columns with NA because all fcs are 0", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_fcs_all_0 <- bind_rows(cl_fcs_all_0,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_fcs_all_0)
}


### FCS score is 7. 

fcs_columns <- names(raw.flag.fcs)[which(stringr::str_starts(names(raw.flag.fcs),"fsl_fcs_"))]
if("fsl_fcs_score" %in% names(raw.flag.fcs)) {
  check <- raw.flag.fcs %>% 
    filter(fsl_fcs_score == 112)
  cl_fcs_all_7 <- data.frame()
  for(i in 1:nrow(check)){
    cl<- recode.set.NA.if(check[i,], fcs_columns, check[i,fcs_columns], "replacing fcs columns with NA because all fcs are 7", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_fcs_all_7 <- bind_rows(cl_fcs_all_7,cl) 
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_fcs_all_7)
}


### All LCSI NA
lcsi_columns <- names(raw.flag.fcs)[which(stringr::str_starts(names(raw.flag.fcs),"fsl_lcsi_"))]
if("flag_lcsi_na" %in% names(raw.flag.fcs)) {
  check <-  raw.flag.fcs %>% 
    filter(flag_lcsi_na == 1)
  cl_lcsi_all_na <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], lcsi_columns, check[i,lcsi_columns], "replacing lcsi columns with NA because all lcsi are na", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_lcsi_all_na <- bind_rows(cl_lcsi_all_na,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_all_na)
}

lcs_variables <- c("fsl_lcsi_stress1","fsl_lcsi_stress2","fsl_lcsi_stress3","fsl_lcsi_stress4","fsl_lcsi_crisis1",
                   "fsl_lcsi_crisis2","fsl_lcsi_crisis3","fsl_lcsi_emergency1","fsl_lcsi_emergency2","fsl_lcsi_emergency3")

### LCSI Displaced but HH not displaced
if("flag_lcsi_displ" %in% names(raw.flag.fcs)){
  displ <- lcs_variables[which(grepl("displaced|migration|migrated",get.label(lcs_variables)))]
  check <-  raw.flag.fcs %>% filter(flag_lcsi_displ == 1)
  cl_lcsi_displ <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], displ, check[i,displ], "replacing lcsi displacement strategy columns with NA because HH not IDP", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_lcsi_displ <- bind_rows(cl_lcsi_displ,cl)
  }
  
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_displ)
}

### LCSI Agriculture but HH not displaced
if("flag_lcsi_liv_agriculture" %in% names(raw.flag.fcs)){
  agric <- lcs_variables[which(grepl("agriculture|crop|crops|farm",get.label(lcs_variables)))]
  check <-  raw.flag.fcs %>% filter(flag_lcsi_liv_agriculture == 1)
  cl_lcsi_agric <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], agric, check[i,agric], "replacing lcsi agricultural strategy columns with NA because HH do not have income from agriculture", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_lcsi_agric <- bind_rows(cl_lcsi_agric,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_agric)
}

### LCSI Livestock but HH not displaced
if("flag_lcsi_liv_livestock" %in% names(raw.flag.fcs)){
  livest <- lcs_variables[which(grepl("livestock|livestocks|animal",get.label(lcs_variables)))]
  check <-  raw.flag.fcs %>% filter(flag_lcsi_liv_livestock == 1)
  cl_lcsi_livest <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.NA.if(check[i,], livest, check[i,livest], "replacing lcsi livestock strategy columns with NA because HH do not have income from livestock", ignore_case = F) %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_lcsi_livest <- bind_rows(cl_lcsi_livest,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_lcsi_livest)
}

if(!is.null(raw.water_count_loop)){ 
    #### WASH
    ### LPD -/+ 3 from the mean of total lpd
    if("flag_sd_litre" %in% names(raw.flag.wash)){
      check_uuid <-  raw.flag.wash %>% filter(flag_sd_litre == 1) %>% pull(uuid)
      check_loop <- raw.water_count_loop %>% 
        filter(uuid %in% check_uuid)
      columns <- c("wash_container_type","wash_container_type_other", "wash_container_litre_other")
      cl_sd_litre <- data.frame()
      if(nrow(check_loop)>0){
        for (i in 1:nrow(check_loop)) {
          cl <- recode.set.NA.if(check_loop[i,], columns, check_loop[i,columns], "replacing container info to NA if the sd of lpd is -/+ 3 from the mean of total lpd", ignore_case = F) %>% 
            filter(!is.na(old.value)) %>% 
            mutate(old.value = as.character(old.value),
                   new.value = as.character(new.value))
          cl_sd_litre <- bind_rows(cl_sd_litre,cl)
        }
        cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_sd_litre)
      }
    }
  
  ### Low LPD
  if("flag_low_litre" %in% names(raw.flag.wash)){
    check_uuid <-  raw.flag.wash %>% filter(flag_low_litre == 1) %>% pull(uuid)
    check_loop <- raw.water_count_loop %>% 
      filter(uuid %in% check_uuid)
    columns <- c("wash_container_type","wash_container_type_other", "wash_container_litre_other")
    cl_low_litre <- data.frame()
    if(nrow(check_loop)>0){
      for (i in 1:nrow(check_loop)) {
        cl <- recode.set.NA.if(check_loop[i,], columns, check_loop[i,columns], "replacing container info to NA if lpd is lower than 1", ignore_case = F) %>% 
          filter(!is.na(old.value)) %>% 
          mutate(old.value = as.character(old.value),
                 new.value = as.character(new.value))
        cl_low_litre <- bind_rows(cl_low_litre,cl)
      }
      cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_low_litre)
    }
  }
  
  ### High LPD
  if("flag_high_litre" %in% names(raw.flag.wash)){
    check_uuid <-  raw.flag.wash %>% filter(flag_high_litre == 1) %>% pull(uuid)
    check_loop <- raw.water_count_loop %>% 
      filter(uuid %in% check_uuid)
    columns <- c("wash_container_type","wash_container_type_other", "wash_container_litre_other")
    cl_high_litre <- data.frame()
    if(nrow(check_loop)>0){
      for (i in 1:nrow(check_loop)) {
        cl <- recode.set.NA.if(check_loop[i,], columns, check_loop[i,columns], "replacing container info to NA if lpd is higher than 50", ignore_case = F) %>% 
          filter(!is.na(old.value)) %>% 
          mutate(old.value = as.character(old.value),
                 new.value = as.character(new.value))
        cl_high_litre <- bind_rows(cl_high_litre,cl)
      }
      cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_high_litre)
    }
  }
  
  ### High num of containers
  if("flag_high_container" %in% names(raw.flag.wash)){
    check_uuid <-  raw.flag.wash %>% filter(flag_high_container == 1) %>% pull(uuid)
    check_loop <- raw.water_count_loop %>% 
      filter(uuid %in% check_uuid)
    columns <- c("wash_container_type","wash_container_type_other", "wash_container_litre_other",
                 "wash_container_journey_info","wash_container_journey_collection","container_position")
    cl_high_container <- data.frame()
    if(nrow(check_loop)>0){
      for (i in 1:nrow(check_loop)) {
        cl <- recode.set.NA.if(check_loop[i,], columns, check_loop[i,columns], "replacing container info to NA if num of containers higher than 20", ignore_case = F) %>% 
          filter(!is.na(old.value)) %>% 
          mutate(old.value = as.character(old.value),
                 new.value = as.character(new.value))
        cl_high_container <- bind_rows(cl_high_container,cl)
      }
      cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_high_container)
    }
    check <-  raw.flag.wash %>% filter(flag_high_container == 1)
    cl_high_container_main <- data.frame()
    for (i in 1:nrow(check)) {
      cl <- recode.set.NA.if(check[i,], "wash_num_containers", check[i,"wash_num_containers"], "replacing container info to NA if num of containers higher than 20", ignore_case = F) %>% 
        filter(!is.na(old.value)) %>% 
        mutate(old.value = as.character(old.value))
      cl_high_container_main <- bind_rows(cl_high_container_main,cl)
    }
    cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_high_container_main)
  }
}
### Water on premise but water collection time is not immediate
if("flag_not_immediate" %in% names(raw.flag.wash)){
  check <-  raw.flag.wash %>% filter(flag_not_immediate == 1)
  cl_not_immediate <- data.frame()
  for (i in 1:nrow(check)) {
    cl <- recode.set.value.regex(check[i,], "wash_water_collect_time",check[i,"wash_water_collect_time"],"dont_know","replacing water collection time info to dont know because water source is on premise") %>% 
      filter(!is.na(old.value)) %>% 
      mutate(old.value = as.character(old.value),
             new.value = as.character(new.value))
    cl_not_immediate <- bind_rows(cl_not_immediate,cl)
  }
  cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_not_immediate)
}


#### NUTRITION
### Extreme MUAC
if(all(c("nut_muac_cm","nut_edema_confirm") %in% names(raw.child_nutrition))){
  if("flag_extreme_muac" %in% names(raw.flag.nut)){
    check <-  raw.flag.nut %>% filter(flag_extreme_muac == 1)
    columns <- c("nut_muac_cm","nut_muac_mm")
    cl_extreme_muac <- data.frame()
    if(nrow(check)>0){
      for (i in 1:nrow(check)) {
        cl <- recode.set.NA.if(check[i,], columns, check[i,columns], "replacing extreme muacs with NA", ignore_case = F) %>% 
          filter(!is.na(old.value)) %>% 
          mutate(old.value = as.character(old.value),
                 new.value = as.character(new.value))
        cl_extreme_muac <- bind_rows(cl_extreme_muac,cl)
      }
      cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_extreme_muac)
    }
  }
}
if(all(c("nut_muac_cm","nut_edema_confirm") %in% names(raw.child_nutrition))){
  ### MUAC-for-Age z-scores is +/- 3 from mean of total MUAC-for-ages z-scores
  if("flag_sd_mfaz" %in% names(raw.flag.nut)){
    check <-  raw.flag.nut %>% filter(flag_sd_mfaz == 1) 
    columns <- c("nut_muac_cm","nut_muac_mm")
    cl_sd_mfaz <- data.frame()
    if(nrow(check)>0){
      for (i in 1:nrow(check)) {
        cl <- recode.set.NA.if(check[i,], columns, check[i,columns], "replacing extreme muacs with NA", ignore_case = F) %>% 
          filter(!is.na(old.value)) %>% 
          mutate(old.value = as.character(old.value),
                 new.value = as.character(new.value))
        cl_sd_mfaz <- bind_rows(cl_sd_mfaz,cl)
      }
      cleaning.log.checks.direct <- bind_rows(cleaning.log.checks.direct, cl_sd_mfaz)
    }
  }
}

raw.main  <- raw.main  %>% apply.changes(cleaning.log.checks.direct)
raw.child_nutrition <- raw.child_nutrition %>% apply.changes(cleaning.log.checks.direct, is.loop = T)
if(!is.null(raw.water_count_loop)){ 
  raw.water_count_loop <- raw.water_count_loop %>% apply.changes(cleaning.log.checks.direct, is.loop = T)
}
raw.flag.fcs <- raw.flag.fcs %>% 
  apply.changes(cleaning.log.checks.direct)

# 4B) FLAG Logical Checks

checks_followups <- tibble()

## FSL
# Check number 1
if("flag_protein_rcsi" %in% names(raw.flag.fcs)){
  check_protein_rcsi <- raw.flag.fcs %>% 
    select(uuid,enum_colname, flag_protein_rcsi)%>% 
    filter(flag_protein_rcsi == 1) %>% 
    left_join(raw.flag %>% select(uuid, fsl_rcsi_score, fsl_fcs_meat, fsl_fcs_dairy))
  
  if(nrow(check_protein_rcsi)>0){
    checks_followups <- rbind(checks_followups,
                              make.logical.check.entry(check_protein_rcsi, 1,  c("fsl_rcsi_score","fsl_fcs_meat","fsl_fcs_dairy"), 
                                                       cols_to_keep = c(enum_colname),"rCSI Score is high while protein consumption is also reported as frequent", F))
  }
}

if("flag_lcsi_coherence" %in% names(raw.flag.fcs)){
  # Check number 2
  check_lcsi_coherence <- raw.flag.fcs %>% 
    select(uuid,enum_colname, flag_lcsi_coherence)%>% 
    filter(flag_lcsi_coherence == 1)%>% 
    left_join(raw.flag %>% select(uuid, 
                                  fsl_lcsi_emergency, fsl_lcsi_stress, fsl_lcsi_crisis,
                                  fsl_lcsi_stress1,fsl_lcsi_stress2,fsl_lcsi_stress3,fsl_lcsi_stress4,
                                  fsl_lcsi_crisis1,fsl_lcsi_crisis2,fsl_lcsi_crisis3,
                                  fsl_lcsi_emergency1,fsl_lcsi_emergency2,fsl_lcsi_emergency3))
  
  if(nrow(check_lcsi_coherence)>0){
    checks_followups <- rbind(checks_followups,
                              make.logical.check.entry(check_lcsi_coherence, 2,  c("fsl_lcsi_emergency", "fsl_lcsi_stress","fsl_lcsi_crisis",
                                                                                   "fsl_lcsi_stress1","fsl_lcsi_stress2","fsl_lcsi_stress3","fsl_lcsi_stress4",
                                                                                   "fsl_lcsi_crisis1","fsl_lcsi_crisis2","fsl_lcsi_crisis3",
                                                                                   "fsl_lcsi_emergency1","fsl_lcsi_emergency2","fsl_lcsi_emergency3"), # to provide all teh strategies
                                                       cols_to_keep = c(enum_colname),"HHs report using crisis or emergency strategies but not stress strategies or Emergency and no crisis.", F))
  }
}
#Check number 3
fcs_flag_columns <- c("fsl_fcs_cereal","fsl_fcs_legumes","fsl_fcs_dairy","fsl_fcs_meat","fsl_fcs_veg",
                      "fsl_fcs_fruit","fsl_fcs_oil","fsl_fcs_sugar","fsl_fcs_score")
rcsi_flag_columns <- c("fsl_rcsi_lessquality","fsl_rcsi_borrow",
                       "fsl_rcsi_mealsize","fsl_rcsi_mealadult","fsl_rcsi_mealnb","fsl_rcsi_score")

if("flag_fcsrcsi_box" %in% names(raw.flag.fcs)) {
  check_fcsrcsi_box <- raw.flag.fcs %>% 
    select(uuid,enum_colname, flag_fcsrcsi_box)%>% 
    filter(flag_fcsrcsi_box == 1)%>% 
    left_join(raw.flag %>% select(uuid, fcs_flag_columns, rcsi_flag_columns))
  
  if(nrow(check_fcsrcsi_box)>0){
    checks_followups <- rbind(checks_followups,
                              make.logical.check.entry(check_fcsrcsi_box, 3,  c(fcs_flag_columns, rcsi_flag_columns), 
                                                       cols_to_keep = c(enum_colname),"HH that would have an acceptable FCS score and a high rCSI score", F))
  }
}
if(!is.null(raw.water_count_loop)){ 
  ## WATER CONSUMPTION
  #check number 4
  if("flag_no_container" %in% names(raw.flag.wash)) {
    check_no_container <- raw.flag.wash %>% 
      select(uuid,enum_colname, flag_no_container)%>% 
      filter(flag_no_container == 1)%>% 
      left_join(raw.main %>% select(uuid, wash_num_containers, wash_water_source,`wash_different_water_sources/piped_dwelling`,`wash_different_water_sources/piped_compound`,
                                    `wash_different_water_sources/rainwater_collection`))
    
    if(nrow(check_no_container)>0){
      checks_followups <- rbind(checks_followups,
                                make.logical.check.entry(check_no_container, 4,  c("wash_num_containers", "wash_water_source",
                                                                                   "wash_different_water_sources/piped_dwelling","wash_different_water_sources/piped_compound",
                                                                                   "wash_different_water_sources/rainwater_collection"), 
                                                         cols_to_keep = c(enum_colname),"HH reported no containers but also reports that water sources are not on premises", F))
    }
  }
}
## Nutrition
#check number 5
if(all(c("nut_muac_cm","nut_edema_confirm") %in% names(raw.child_nutrition))){
  if("nut_edema_confirm" %in% names(raw.child_nutrition)){
    check_eodema <- raw.child_nutrition %>% 
      select(uuid,loop_index, nut_edema_confirm)%>% 
      filter(nut_edema_confirm == "yes")%>% 
      left_join(raw.main %>% select(uuid, enum_colname))
    
    if(nrow(check_eodema)>0){
      checks_followups <- bind_rows(checks_followups,
                                make.logical.check.entry(check_eodema, 5,  c("nut_edema_confirm"), 
                                                         cols_to_keep = c(enum_colname),"Respondent reported children have oedema.", T)) %>% 
        relocate(loop_index, .before = 2)
    }
  }
}
if(!is.null(raw.died_member)){
  ## Mortality
  #check number 6
  
  check_mortality <- raw.main %>% 
    select(uuid,enum_colname, num_died)%>% 
    filter(as.numeric(num_died) >= 2)
  
  if(nrow(check_mortality)>0){
    checks_followups <- bind_rows(checks_followups,
                                  make.logical.check.entry(check_mortality, 6,  c("num_died"), 
                                                           cols_to_keep = c(enum_colname),"Respondent reported more than 2 death in the HH", F))
  }
  #check number 7
  
  check_cause_death <- raw.died_member %>% 
    select(uuid,loop_index, sex_died, cause_death)%>% 
    filter(sex_died == "m" & cause_death %in% c("post_partum","during_pregnancy","during_delivery"))%>% 
    left_join(raw.main %>% select(uuid, enum_colname))
    
  
  if(nrow(check_cause_death)>0){
    checks_followups <- bind_rows(checks_followups,
                                  make.logical.check.entry(check_cause_death, 7,  c("sex_died","cause_death"), 
                                                           cols_to_keep = c(enum_colname),"Respondent reported sex of dead person male and a cause of death related to female only.", T)) %>% 
      relocate(loop_index, .before = 2)
  }
}

if(nrow(checks_followups) >0 ){
  checks_followups <- checks_followups %>% 
    dplyr::mutate(loops_to_remove = NA) %>% 
    dplyr::relocate(loops_to_remove, .before = "explanation")
  
  create.follow.up.requests(checks_followups,loop_data = raw.died_member, paste0(make.short.name("followup_requests"),".xlsm"), use_template = T)
}

save.image("output/data_log/first_logical.rda")
options(warn=0)
if(language_assessment == "English"){
  if(nrow(checks_followups) >0 ){
    cat("\n\n#############################################################################################\n")
    cat("Direct logical checks are flagged and a file is created for follow up in \noutput/checking/requests/ with follow_up_requests in the title. \nPlease check the READ_ME file for information on filling the file.\n")
    cat("#############################################################################################\n")
  } else {
    cat("\n\n#############################################################################################\n")
    cat("Direct logical checks are flagged and no follow_up is needed.\nContinue running the next step.")
    cat("#############################################################################################\n")
  }
}else{
  if(nrow(checks_followups) >0 ){
    cat("\n\n#############################################################################################\n")
    cat("Les contrôles logiques directs sont signalés et un fichier de suivi est créé dans \noutput/checking/requests/ avec follow_up_requests dans le titre. \nVeuillez consulter le fichier READ_ME pour plus d'informations sur le remplissage du fichier.\n")
    cat("#############################################################################################\n")
  } else {
    cat("\n\n#############################################################################################\n")
    cat("Les contrôles logiques directs sont signalés et.\nContinuer avec la prochaine etape.")
    cat("#############################################################################################\n")
  }
  
}

