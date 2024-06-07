source("src/init.R")

#-------------------------------------------------------------------------------
# 3) OTHERS AND TRANSLATIONS
################################################################################
options(warn = -1)
other.db <- get.other.db()

other.db.main  <- other.db[other.db$name %in% colnames(raw.main),]
other.db.hh_roster  <- other.db[other.db$name %in% colnames(raw.hh_roster),]
other.db.ind_health  <- other.db[other.db$name %in% colnames(raw.ind_health),]
if(!is.null(raw.died_member)) {
  other.db.died_member <- other.db[other.db$name %in% colnames(raw.died_member),]
}

language <- strings['language_other']

if(language == "French") {
  lang <- "fr"
} else if (language == "Spanish"){
  lang <- "es"
} else {
  lang <- "ar"
} 

other.responses <- rbind(find.responses(raw.main, other.db.main, values_to = paste0("response.",lang)),
                         find.responses(raw.hh_roster, other.db.hh_roster, values_to = paste0("response.",lang),is.loop = T),
                         find.responses(raw.ind_health, other.db.ind_health, values_to = paste0("response.",lang),is.loop = T))

if(!is.null(raw.died_member)){
  other.responses <- rbind(other.responses,
                           find.responses(raw.died_member, other.db.died_member, values_to = paste0("response.",lang),is.loop = T))
}

if(strings['api'] == "No Api"){
  other.responses.j <- other.responses %>%
    mutate(!!sym(paste0("response.",lang,".en")) := NA)
  save.other.requests(create.translate.requests(other.db, other.responses.j, is.loop = T),
                      paste0(dataset.name.short, "_other_requests_",strings["out_date"]), use_template = T)
} else{
  other.responses.j <- other.responses %>% translate.responses_iphra(api_key = as.character(strings['api_key']), api = as.character(strings['api']),  values_from = paste0("response.",lang), language_codes = lang)
  
  save.other.requests(create.translate.requests(other.db, other.responses.j, is.loop = T),
                      paste0(dataset.name.short, "_other_requests_",strings["out_date"]), use_template = T)
  
} 

#-----------------------------------------------------------------------------
# Translation
if(!is.null(raw.died_member)){
  trans.db <- get.trans.db()
  trans.responses <- find.responses(raw.died_member, trans.db, values_to = paste0("response.",lang), is.loop = T)
  if(strings['api'] == "No Api"){
    trans.responses.j <- trans.responses %>%
      mutate(!!sym(paste0("response.",lang,".en")) := NA)
    save.trans.requests(create.translate.requests(trans.db, trans.responses.j, is.loop = T), paste0(dataset.name.short, "_translate_requests_",strings["out_date"]), use_template = T)
  } else{
    trans.responses.j <- trans.responses %>% translate.responses_iphra(api_key = as.character(strings['api_key']), api = as.character(strings['api']),  values_from = paste0("response.",lang), language_codes = lang)
    save.trans.requests(create.translate.requests(trans.db, trans.responses.j, is.loop = T), paste0(dataset.name.short, "_translate_requests_",strings["out_date"]), use_template = T)
  } 
}

save.image("output/data_log/first_translation.rda")
options(warn = 0)
if(language_assessment == "English"){
  cat("\n\n############################################################################################\n")
  cat("Translation for both others and translations are done and a file is created in the folder \noutput/checking/requests/ with other_requests in the title. \nPlease check the READ_ME file for information on filling the file.\n")
  cat("############################################################################################\n")
} else{
  cat("\n\n############################################################################################\n")
  cat("Les traductions pour les autres et les traductions sont effectuées et un fichier est créé dans le dossier \noutput/checking/requests/ avec other_requests dans le titre. \nVeuillez consulter le fichier READ_ME pour obtenir des informations sur le remplissage du fichier.\n")
  cat("############################################################################################\n")
}
