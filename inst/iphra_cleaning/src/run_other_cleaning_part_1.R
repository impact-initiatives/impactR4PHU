rm(list = ls())
# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs, stringr)

load("output/data_log/final_deletion.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  language_other = svDialogs::dlg_list(choices = c("French","Arabic", "Spanish"), title= if(language_assessment == "English"){
      "Please select others language"
    }else{
      "Veuillez sélectionner la langue des autre options"
    }, rstudio = getOption("svDialogs.rstudio", TRUE))$res,
  api = svDialogs::dlg_list(choices = c("Microsoft","DeepL","No Api"), title= if(language_assessment == "English"){
      "Please select Translator"
    }else{
      "Veuillez sélectionner un traducteur"
    }, rstudio = getOption("svDialogs.rstudio", TRUE))$res,
  api_key = svDialogs::dlg_input(message=if(language_assessment == "English"){
      "Please input your api:"
    }else{
      "Veuillez saisir votre api:"
    })$res,
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)      # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_other_cleaning.R')
# -----------------------------------------------------------------------------`