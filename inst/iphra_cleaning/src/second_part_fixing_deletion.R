source("src/init.R")

##-----------------------------------------------------------------------------
# Check previous deletion.log.fast
options(warn = -1)
# files_deletion <- list.files("output/data_log/deletion")
deletion.change <- data.frame()
deletion.whole <- data.frame()


deletion.whole <- rbind(deletion.whole,deletion.log.fast)

# ------------------------------------------------------------------------------
# AFTER RECEIVING FILLED-OUT Deletion requests:

cleaning.log.deletion <- data.frame() 

or.request <- readxl::read_excel(paste0("output/checking/requests/",list.files("output/checking/requests/","deletion_requests")),sheet = "Sheet2", col_types = "text")
or.response <- readxl::read_excel(paste0("output/checking/responses/",list.files("output/checking/responses/","deletion_requests_edited")),sheet = "Sheet2", col_types = "text")
if(nrow(or.response)>0){
  check <- or.response %>% 
    dplyr::mutate(check = rowSums(is.na(select(or.response, c(remove_or_change,loops_to_remove)))))
  
  if(nrow(check %>% filter(check == 2))>0) {
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
  
  if(nrow(check %>% filter(check <= 1)) == nrow(or.response)){
    
    delete_entries <- or.response %>% 
      filter(remove_or_change == "Remove")
    
    if(nrow(delete_entries)>0){
      if("start" %in% names(delete_entries)){
        duration_delete <- delete_entries %>% 
          filter(!is.na(start)) %>% 
          select(uuid, enum_colname, reason)
        deletion.change <- bind_rows(deletion.change,duration_delete)
      }
      if("num_different_columns" %in% names(delete_entries)){
        soft_duplicates_delete <- delete_entries %>% 
          filter(!is.na(num_different_columns)) %>% 
          select(uuid, enum_colname, reason)
        deletion.change <- bind_rows(deletion.change,soft_duplicates_delete)
      }
      if("loop_count" %in% names(delete_entries)) {
        if(nrow(delete_entries %>% filter(!is.na(loop_count)))>0){
          loop_delete <- delete_entries %>% 
            filter(!is.na(loop_count)) %>% 
            select(uuid, enum_colname,reason, loops_to_remove) %>% 
            mutate(loops_to_remove = stringr::str_remove(loops_to_remove," ")) %>% 
            separate_rows(loops_to_remove, sep = ";")
        
          raw.hh_roster <- raw.hh_roster[!(raw.hh_roster$loop_index %in% loop_delete$loops_to_remove),]
          raw.ind_health <- raw.ind_health[!(raw.ind_health$loop_index %in% loop_delete$loops_to_remove),]
          if(!is.null(raw.water_count_loop)) {
            raw.water_count_loop <- raw.water_count_loop[!(raw.water_count_loop$loop_index %in% loop_delete$loops_to_remove),]
          }
          raw.child_nutrition <- raw.child_nutrition[!(raw.child_nutrition$loop_index %in% loop_delete$loops_to_remove),]
          if(!is.null(raw.women)) {
            raw.women <- raw.women[!(raw.women$loop_index %in% loop_delete$loops_to_remove),]
          }
          if(!is.null(raw.died_member)) {
            raw.died_member <- raw.died_member[!(raw.died_member$loop_index %in% loop_delete$loops_to_remove),]
          }
        
          deletion.whole <- bind_rows(deletion.whole,loop_delete)
        }
      }
    }
    
    change_entries <- or.response %>% 
      filter(remove_or_change == "Change")
    
    if(nrow(change_entries)>0){
      cleaning.log.loop_inconsitency <- change_entries %>%
        mutate(loop_index = NA,
               old.value = as.character(main_count),
               new.value = as.character(loop_count),
               reason = "Inconsistency in number of entries in loop") %>% 
        select(uuid, loop_index,enum_colname,variable,old.value,new.value,reason)
      
      raw.main <- raw.main %>% 
        apply.changes(cleaning.log.loop_inconsitency)
      
      cleaning.log.deletion <- rbind(cleaning.log.deletion,cleaning.log.loop_inconsitency)
    }
    
    if(nrow(deletion.change)>0){
      raw.main <- raw.main[!(raw.main$uuid %in% deletion.change$uuid),]
      raw.hh_roster  <- raw.hh_roster[!(raw.hh_roster$uuid %in% deletion.change$uuid),]
      raw.ind_health  <- raw.ind_health[!(raw.ind_health$uuid %in% deletion.change$uuid),]
      if(!is.null(raw.water_count_loop)){
        raw.water_count_loop  <- raw.water_count_loop[!(raw.water_count_loop$uuid %in% deletion.change$uuid),]
      }
      raw.child_nutrition  <- raw.child_nutrition[!(raw.child_nutrition$uuid %in% deletion.change$uuid),]
      if(!is.null(raw.women)){
        raw.women  <- raw.women[!(raw.women$uuid %in% deletion.change$uuid),]
      }
      
      if(!is.null(raw.died_member)) {
        raw.died_member  <- raw.died_member[!(raw.died_member$uuid %in% deletion.change$uuid),]
      }
      deletion.whole <- bind_rows(deletion.whole,deletion.change)
    }
  }
}
options(warn = 0)
writexl::write_xlsx(deletion.whole,paste0("output/deletion_log/",dataset.name.short,"_deletion_log_",strings['out_date'],".xlsx"))
if(language_assessment == "English"){
  cat("###########################################################################\n")
  cat("Deletion part is all done. To check the deletion_log, \nplease go to output/deletion_log/ folder.Next step is cleaning of the others.\n")
  cat("###########################################################################\n")
} else {
  cat("###########################################################################\n")
  cat("La partie suppression est terminée. Pour vérifier le deletion_log, \nallez dans le dossier output/deletion_log/ La prochaine étape est le nettoyage des autres.\n")
  cat("###########################################################################\n")
}
save.image("output/data_log/final_deletion.rda")
