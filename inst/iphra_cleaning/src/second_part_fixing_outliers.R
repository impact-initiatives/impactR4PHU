source("src/init.R")
options(warn=-1)
#-------------------------------------------------------------------------------
# Recoding and changing data after outliers follow ups

# RUN ONLY IF Anything need to be changed

outlier.recode <- load.requests(dir.responses, "outliers", sheet = "Sheet2")
outlier.check <- load.requests(dir.requests, "outliers", sheet = "Sheet2")

if (nrow(outlier.check) != nrow(outlier.recode)) {
  warning("Number of rows are not matching")
  if(language_assessment == "English"){
    cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    stop("Number of rows are not matching between the output file and the edited.")
    cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  } else {
    cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    stop("Nombre de lignes ne correspondant pas entre le fichier de sortie et le fichier édité.")
    cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  }
}

check <- outlier.recode %>% 
  mutate(missed = is.na(invalid) & is.na(new.value),
         incorrect = !is.na(invalid) & !is.na(new.value))

if(nrow(check %>% filter(incorrect))>0) {
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
if(nrow(check %>% filter(missed))>0) {
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

cleaning.log.outliers <- outlier.recode %>%
  select(uuid,loop_index,variable,issue,old.value,new.value) %>%
  filter(is.na(new.value) | old.value != new.value)

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.outliers)

if(!is.null(raw.water_count_loop)){
  raw.water_count_loop <- raw.water_count_loop %>% 
    apply.changes(cleaning.log.outliers, is.loop = T)
}

raw.child_nutrition <- raw.child_nutrition %>% 
  apply.changes(cleaning.log.outliers, is.loop = T)
  

if(!is.null(raw.women)){
  raw.women <- raw.women %>% 
    apply.changes(cleaning.log.outliers, is.loop = T)
}

if(!is.null(raw.died_member)){
  raw.died_member <- raw.died_member %>% 
    apply.changes(cleaning.log.outliers, is.loop = T)

}

## DEPENDENCY OUTLIERS
cleaning.log.dependency.outlier <- data.frame()

# num_left
check <- raw.main %>% 
  filter(is.na(num_left)) %>% 
  dplyr::mutate(flag = ifelse(left_yn_known  ==  "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "left_yn_known",
                  old.value = left_yn_known,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}

# num_join
check <- raw.main %>% 
  filter(is.na(num_join)) %>% 
  dplyr::mutate(flag = ifelse(left_yn_known == "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "join_yn_known",
                  old.value = join_yn_known,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
# water_interuption_num_days
check <- raw.main %>% 
  filter(is.na(wash_water_interuption_num_days)) %>% 
  dplyr::mutate(flag = ifelse(wash_water_interuption == "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "wash_water_interuption",
                  old.value = wash_water_interuption,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
# num_share_toilet
check <- raw.main %>% 
  filter(is.na(wash_num_share_toilet)) %>% 
  dplyr::mutate(flag = ifelse(wash_share_toilet_facility == "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "wash_share_toilet_facility",
                  old.value = wash_share_toilet_facility,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
# length_long
check <- raw.main %>% 
  filter(is.na(length_long)) %>% 
  dplyr::mutate(flag = ifelse(rect_info_area == "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "rect_info_area",
                  old.value = rect_info_area,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
# width_larg
check <- raw.main %>% 
  filter(is.na(width_larg)) %>% 
  dplyr::mutate(flag = ifelse(rect_info_area == "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "rect_info_area",
                  old.value = rect_info_area,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
# diameter
check <- raw.main %>% 
  filter(is.na(diameter)) %>% 
  dplyr::mutate(flag = ifelse(circ_info_area == "yes", 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "circ_info_area",
                  old.value = circ_info_area,
                  loop_index = NA,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
if(!is.null(raw.water_count_loop)){
  # container_journey_collection
  check <- raw.water_count_loop %>% 
    filter(is.na(wash_container_journey_collection)) %>% 
    dplyr::mutate(flag = ifelse(wash_container_journey_info == "yes", 1, 0)) %>% 
    filter(flag == 1)
  
  if(nrow(check)>0){
    check_new <- check %>% 
      dplyr::mutate(variable = "wash_container_journey_info",
                    old.value = wash_container_journey_info,
                    new.value = NA,
                    issue = "Dependency") %>% 
      dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
    cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
  }
  
  # num_days_water_last
  check <- raw.water_count_loop %>% 
    filter(is.na(wash_num_days_water_last)) %>% 
    dplyr::mutate(flag = ifelse(wash_container_journey_info == "yes", 1, 0)) %>% 
    filter(flag == 1)
  
  if(nrow(check)>0){
    check_new <- check %>% 
      dplyr::mutate(variable = "wash_container_journey_info",
                    old.value = wash_container_journey_info,
                    new.value = NA,
                    issue = "Dependency") %>% 
      dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
    cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
  }
}
# num_days_water_last
check <- raw.child_nutrition %>% 
  filter(is.na(nut_muac_cm)) %>% 
  dplyr::mutate(flag = ifelse(!is.na(nut_muac_mm), 1, 0)) %>% 
  filter(flag == 1)

if(nrow(check)>0){
  check_new <- check %>% 
    dplyr::mutate(variable = "nut_muac_mm",
                  old.value = nut_muac_mm,
                  new.value = NA,
                  issue = "Dependency") %>% 
    dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
  cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
}
if(!is.null(raw.women)){
  # num_days_water_last
  check <- raw.women %>% 
    filter(is.na(woman_muac_cm)) %>% 
    dplyr::mutate(flag = ifelse(!is.na(woman_muac_mm), 1, 0)) %>% 
    filter(flag == 1)
  
  if(nrow(check)>0){
    check_new <- check %>% 
      dplyr::mutate(variable = "woman_muac_mm",
                    old.value = woman_muac_mm,
                    new.value = NA,
                    issue = "Dependency") %>% 
      dplyr::select(uuid, loop_index, variable, old.value, new.value, issue)
    cleaning.log.dependency.outlier <- rbind(cleaning.log.dependency.outlier,check_new)
  }
}

raw.main <- raw.main %>% 
  apply.changes(cleaning.log.dependency.outlier)

raw.child_nutrition <- raw.child_nutrition %>% 
  apply.changes(cleaning.log.dependency.outlier, is.loop = T)

if(!is.null(raw.women)){
  raw.women <- raw.women %>% 
    apply.changes(cleaning.log.dependency.outlier, is.loop = T)
}
if(!is.null(raw.water_count_loop)){
  raw.water_count_loop <- raw.water_count_loop %>% 
    apply.changes(cleaning.log.dependency.outlier, is.loop = T)
}


save.image("output/data_log/final_outliers.rda")
if(language_assessment == "English"){
  cat("\n\n#############################################################################################\n")
  cat("All deletions/changes/checks were done. Please proceed to the last part to \nfinalize and package your whole cleaning files.\n")
  cat("#############################################################################################\n")
}else{
  cat("\n\n#############################################################################################\n")
  cat("Toutes les deletions/changes/checks ont été effectuées. \nVeuillez passer à la dernière partie pour finaliser et emballer l'ensemble de \nvos fichiers de nettoyage.\n")
  cat("#############################################################################################\n")
}
options(warn=0)