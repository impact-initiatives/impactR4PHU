rm(list = ls())

load("output/data_log/final_outliers.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  filename.dataset = choose.files(caption = if(language_assessment == "English"){
      "Please select the raw data for the Enumerator Performance"
    }else{
      "Veuillez sélectionner les données brutes pour la performance de l'enquêteur."
    }, multi = F),
  pwd = svDialogs::dlgInput(if(language_assessment == "English"){
      "Please provide a password for your zipped file."
    }else{
      "Veuillez fournir un mot de passe pour votre fichier zippé"  
    })$res,
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)    # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/final_part_cleaning.R')
# -----------------------------------------------------------------------------`