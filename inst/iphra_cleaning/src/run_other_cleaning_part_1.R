rm(list = ls())
# loading all packages, functions and the Kobo tool
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(svDialogs, stringr)

load("output/data_log/final_deletion.rda")
translation <- svDialogs::dlg_list(
  choices = c("Yes/Oui", "No/Non"),
  title = if (language_assessment == "English") {
    "Do you need to translate your other answers?"
  } else {
    "Est ce que tu doit traduire les autres options?"
  },
  rstudio = getOption("svDialogs.rstudio", TRUE)
)$res
## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  language_other = if (translation == "Yes/Oui") {
    svDialogs::dlg_list(
      choices = c("French", "Arabic", "Spanish"),
      title = if (language_assessment == "English") {
        "Please select others language"
      } else {
        "Veuillez sélectionner la langue des autre options"
      },
      rstudio = getOption("svDialogs.rstudio", TRUE)
    )$res
  } else {
    "English"
  },
  api = if (translation == "Yes/Oui") {
    svDialogs::dlg_list(
      choices = c("Microsoft", "DeepL", "No Api"),
      title = if (language_assessment == "English") {
        "Please select Translator"
      } else {
        "Veuillez sélectionner un traducteur"
      },
      rstudio = getOption("svDialogs.rstudio", TRUE)
    )$res
  } else {
    "No Api"
  },
  api_key = if (translation == "Yes/Oui") {
    svDialogs::dlg_input(
      message = if (language_assessment == "English") {
        "Please input your api:"
      } else {
        "Veuillez saisir votre api:"
      }
    )$res
  } else {
    NULL
  },
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3) # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here
## TABULAR  -------------------------------------------------------------------
source('src/first_part_other_cleaning.R')
# -----------------------------------------------------------------------------`
