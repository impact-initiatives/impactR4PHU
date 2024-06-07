source("src/init_deletion_part.R")
dataset.name.short <- strings['dataset.name.short']
## read raw.data
options(warn = -1)
raw.main <- data.list$main
raw.hh_roster <- data.list$hh_roster
raw.ind_health <- data.list$ind_health
raw.water_count_loop <- data.list$water_count_loop
raw.child_nutrition <- data.list$child_nutrition
raw.women <- data.list$women
raw.died_member <- data.list$died_member

raw.main.no.consent <- raw.main %>% 
  filter(respondent_consent == "no")

raw.main <- raw.main %>% 
  filter(!uuid %in% raw.main.no.consent$uuid)
#-------------------------------------------------------------------------------
# 1) NO CONSENT + DUPLICATES --> deletion log
################################################################################

# check for duplicates 
ids <- raw.main$uuid[duplicated(raw.main$uuid)]
if (length(ids)>0) cat("Duplicate uuids detected: ", length(ids))
# add to deletion log
deletion.log.duplicate <- create.deletion.log(raw.main %>% filter(uuid %in% ids),enum_colname, "Duplicate") # a brand new deletion log
rm(ids)

deletion.log.fast <- bind_rows(deletion.log.duplicate)


####################################################
## run this to remove duplicates and no-consents  ##
raw.main  <- raw.main[!(raw.main$uuid %in% deletion.log.fast$uuid),]
raw.hh_roster  <- raw.hh_roster[!(raw.hh_roster$uuid %in% deletion.log.fast$uuid),]
raw.ind_health  <- raw.ind_health[!(raw.ind_health$uuid %in% deletion.log.fast$uuid),]
if(!is.null(raw.water_count_loop)){
  raw.water_count_loop  <- raw.water_count_loop[!(raw.water_count_loop$uuid %in% deletion.log.fast$uuid),]
}
raw.child_nutrition  <- raw.child_nutrition[!(raw.child_nutrition$uuid %in% deletion.log.fast$uuid),]
if(!is.null(raw.women)){
  raw.women  <- raw.women[!(raw.women$uuid %in% deletion.log.fast$uuid),]
}

if(!is.null(raw.died_member)) {
  raw.died_member  <- raw.died_member[!(raw.died_member$uuid %in% deletion.log.fast$uuid),]
}


#-------------------------------------------------------------------------------
# 2) AUDIT CHECKS
################################################################################

# check if there is audit files in the folder
suppressWarnings(
  audits <- load.audit.files(dir.audits, uuids = raw.main$uuid, track.changes = F)   
)


if(nrow(audits) == 0) {audits.summary <- tibble(uuid = raw.main$uuid, tot.rt = NA)
}else{
  audits.summary <- audits %>% 
    group_by(uuid) %>% 
    group_modify(~process.uuid(.x))
}

data.audit <- raw.main %>% 
  mutate(duration_mins = abs(difftime(as.POSIXct(lubridate::ymd_hms(end)), as.POSIXct(lubridate::ymd_hms(start)), units = 'mins')),
         num_NA_cols = rowSums(is.na(raw.main)),
         num_dk_cols = rowSums(raw.main == "dont_know", na.rm = T),
         num_other_cols = rowSums(!is.na(raw.main[stringr::str_ends(colnames(raw.main), "_other")]), na.rm = T)) # %>%
# select(uuid, !!sym(enum_colname), start, end, duration_mins, num_NA_cols, num_dk_cols, num_other_cols)

audits.summary <- data.audit %>% 
  left_join(audits.summary, by="uuid") %>% select(-contains("/")) %>% 
  relocate(uuid, duration_mins, num_NA_cols, num_dk_cols, num_other_cols, tot.rt) %>% 
  arrange(duration_mins)



# follow up with FPs if there are surveys under 10 minutes or above 1 hour
if(nrow(audits) == 0){
  survey_durations_check <- audits.summary %>% filter(duration_mins < 5 | duration_mins > 60)
} else {
  survey_durations_check <- audits.summary %>% filter(tot.rt < 5 | tot.rt > 60)
}
if(nrow(survey_durations_check) > 0){
  openxlsx::write.xlsx(survey_durations_check, paste0("output/checking/audit/",dataset.name.short, "_survey_durations_", strings['out_date'],".xlsx"),
                       zoom = 90, firstRow = T)
  if(nrow(audits) == 0){
    survey_durations_check <- survey_durations_check %>% 
      select(uuid,enum_colname,duration_mins,start, end) %>% 
      mutate(reason = ifelse(duration_mins < 5, "Submission time less than 10 mins","Submission time more than 60 mins"))
  } else {
    survey_durations_check <- survey_durations_check %>% 
      select(uuid,enum_colname,tot.rt,start, end) %>% 
      mutate(reason = ifelse(tot.rt < 5, "Submission time less than 10 mins","Submission time more than 60 mins"))
  }
  
}else cat("\nThere are no survey durations to check :)")


## Soft duplicates (less than 12 different columns?)

res.soft_duplicates <- find.similar.surveys(raw.main %>% filter(respondent_consent != "no"), tool.survey, uuid = "uuid") %>% 
  filter(number_different_columns <= 12)

if(nrow(res.soft_duplicates) > 0){
  openxlsx::write.xlsx(res.soft_duplicates, paste0("output/checking/audit/",dataset.name.short, "_soft_duplicates_", strings['out_date'],".xlsx"))
  res.soft_duplicates <- res.soft_duplicates %>% 
    select(uuid,enum_colname,num_different_columns) %>% 
    mutate(reason = "Soft duplicates to check - num different columns less than 12.")
}

rm(audits, data.audit)

#-------------------------------------------------------------------------------
# 3) LOOP INCONSITENCIES + SPATIAL CHECKS
################################################################################
## check for inconsistency in loops:

res.inconsistency <- data.frame()

# hh_roster
counts_loop1 <- raw.hh_roster %>%
  group_by(uuid) %>%
  summarize(loop_count = n())
loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), num_hh) %>% left_join(counts_loop1) %>%
  mutate(main_count = ifelse(num_hh == "999", NA, as.numeric(num_hh)),
         reason = "hh_roster loops count not matching with num_hh",
         variable = "num_hh")%>%
  filter(loop_count %!=na% (main_count)) %>% 
  select(uuid, enum_colname,variable, main_count,loop_count, reason)

if(nrow(loop_counts_main) > 0){
  # find loops for inconsistent uuids:
  res.inconsistency <- bind_rows(res.inconsistency,loop_counts_main)
}

# ind_health
counts_loop2 <- raw.ind_health %>%
  group_by(uuid) %>%
  summarize(loop_count = n())

loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), num_hh) %>% left_join(counts_loop2) %>%
  mutate(main_count = ifelse(num_hh == "999", NA, as.numeric(num_hh)),
         reason = "ind_health loops count not matching with num_hh",
         variable = "num_hh")%>%
  filter(loop_count %!=na% (main_count)) %>% 
  select(uuid, enum_colname,variable, main_count,loop_count, reason)

if(nrow(loop_counts_main) > 0){
  # find loops for inconsistent uuids:
  res.inconsistency <- bind_rows(res.inconsistency,loop_counts_main)
}

if(!is.null(raw.water_count_loop)){
  # water_count_loop
  counts_loop3 <- raw.water_count_loop %>%
    group_by(uuid) %>%
    summarize(loop_count = n())
  
  loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), wash_num_containers) %>% left_join(counts_loop3) %>%
    mutate(main_count = ifelse(wash_num_containers == "999", NA, as.numeric(wash_num_containers)),
           reason = "water_count_loop loops count not matching with wash_num_containers",
           variable = "wash_num_containers")%>%
    filter(loop_count %!=na% (main_count))%>% 
    select(uuid, enum_colname,variable, main_count,loop_count, reason)
  
  if(nrow(loop_counts_main) > 0){
    res.inconsistency <- bind_rows(res.inconsistency,loop_counts_main)
  }
}
# child_nutrition
counts_loop4 <- raw.child_nutrition %>%
  group_by(uuid) %>%
  summarize(loop_count = n())

loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), num_hh, num_children) %>% 
  filter(as.numeric(num_children) > 0) %>% left_join(counts_loop4) %>%
  mutate(main_count = ifelse(num_hh == "999", NA, as.numeric(num_hh)),
         reason = "child_nutrition loops count not matching with num_hh",
         variable = "num_hh")%>%
  filter(loop_count %!=na% (main_count))%>% 
  select(uuid, enum_colname,variable, main_count,loop_count, reason)

if(nrow(loop_counts_main) > 0){
  res.inconsistency <- bind_rows(res.inconsistency,loop_counts_main)
}

# women
if(!is.null(raw.women)){
  counts_loop5 <- raw.women %>%
    group_by(uuid) %>%
    summarize(loop_count = n())
  
  loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), num_hh) %>% left_join(counts_loop5) %>%
    mutate(main_count = ifelse(num_hh == "999", NA, as.numeric(num_hh)),
           reason = "women loops count not matching with num_hh",
           variable = "num_hh")%>%
    filter(loop_count %!=na% (main_count))%>% 
    select(uuid, enum_colname,variable, main_count,loop_count, reason)
  
  if(nrow(loop_counts_main) > 0){
    res.inconsistency <- bind_rows(res.inconsistency,loop_counts_main)
  }
}

if(!is.null(raw.died_member)){
  # died_member
  counts_loop6 <- raw.died_member %>%
    group_by(uuid) %>%
    summarize(loop_count = n())
  
  loop_counts_main <- raw.main %>% select(uuid, !!sym(enum_colname), num_died) %>% left_join(counts_loop6) %>%
    mutate(main_count = ifelse(num_died == "999", NA, as.numeric(num_died)),
           reason = "died_member loops count not matching with num_died",
           variable = "num_died")%>%
    filter(loop_count %!=na% (main_count))%>% 
    select(uuid, enum_colname,variable, main_count,loop_count, reason)
  
  if(nrow(loop_counts_main) > 0){
    res.inconsistency <- bind_rows(res.inconsistency,loop_counts_main)
  }
}


if(nrow(res.inconsistency)>0){
  res.inconsistency <- res.inconsistency %>% 
    select(uuid,enum_colname,variable, main_count, loop_count,reason)
}
# DECISION: what to do with these inconsistencies?

res_to_check <- data.frame()

if(nrow(survey_durations_check)>0){
  res_to_check <- bind_rows(res_to_check,survey_durations_check)
} 

if(nrow(res.soft_duplicates) > 0){
  res_to_check <- bind_rows(res_to_check,res.soft_duplicates)
}

if(nrow(res.inconsistency)>0){
  res_to_check <- bind_rows(res_to_check,res.inconsistency)
  
}

## Create the file to check
res_to_check <- res_to_check %>% 
  mutate(remove_or_change = NA,
         loops_to_remove = NA,
         explanation = NA) %>% 
  relocate(reason, .before = "remove_or_change")


save.deletion.requests(res_to_check,make.short.name("deletion_requests"), use_template = T)
if(!is.null(raw.died_member) & !is.null(raw.water_count_loop) & !is.null(raw.women)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "water_count_loop" = raw.water_count_loop ,
                 "child_nutrition" = raw.child_nutrition ,
                 "women" = raw.women,
                 "died_member" = raw.died_member)
}  else if(!is.null(raw.died_member) & !is.null(raw.water_count_loop)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "water_count_loop" = raw.water_count_loop ,
                 "child_nutrition" = raw.child_nutrition ,
                 "died_member" = raw.died_member)
}else if(!is.null(raw.women) & !is.null(raw.water_count_loop)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "water_count_loop" = raw.water_count_loop ,
                 "child_nutrition" = raw.child_nutrition ,
                 "women" = raw.women)
} else if(!is.null(raw.women) & !is.null(raw.died_member)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "water_count_loop" = raw.water_count_loop ,
                 "women" = raw.women,
                 "died_member" = raw.died_member)
} else if(!is.null(raw.women)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "child_nutrition" = raw.child_nutrition ,
                 "women" = raw.women)
} else if(!is.null(raw.died_member)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "child_nutrition" = raw.child_nutrition,
                 "died_member" = raw.died_member)
} else if(!is.null(raw.water_count_loop)){
  sheets <- list("main" = raw.main ,
                 "hh_roster" = raw.hh_roster ,
                 "ind_health" = raw.ind_health ,
                 "water_count_loop" = raw.water_count_loop ,
                 "child_nutrition" = raw.child_nutrition)
} else{ sheets <- list("main" = raw.main ,
                       "hh_roster" = raw.hh_roster ,
                       "ind_health" = raw.ind_health ,
                       "child_nutrition" = raw.child_nutrition)
}

writexl::write_xlsx(sheets, paste0("output/data_log/data/", make.short.name("_data_with_loop_indexes"),".xlsx"))

options(warn = 0)

save.image(file = "output/data_log/first_deletion.rda")
if(language_assessment == "English"){
  cat("\n\n###########################################################################\n")
  cat("Please check the output/checking/requests/ folder for the created file for \ndeletion checks.If the file is empty, this means that all the checks are good. \nIf not, then please follow the instructions in the READ_ME sheet.\n")
  cat("###########################################################################\n")
} else{
  cat("\n\n###########################################################################\n")
  cat("Veuillez vérifier le fichier créé dans le dossier output/checking/requests/ \npour les contrôles de suppression. Si le fichier est vide, cela signifie que tous les contrôles sont bons. \nSi ce n'est pas le cas, veuillez suivre les instructions de la feuille READ_ME.\n")
  cat("###########################################################################\n")
}
