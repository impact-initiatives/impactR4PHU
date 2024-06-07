
rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::restore(prompt = F)
# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(svDialogs)

language_assessment = svDialogs::dlgList(c("English","French"), title = "Please Select the language.", rstudio = getOption("svDialogs.rstudio", TRUE))$res# the filename of your data for 
## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = gsub(" ", "_",dlgInput(if(language_assessment == "English"){
    "Please provide name of assessment (please fill country and dont use special characters)"
    }else{
      "Veuillez indiquer le nom de l'évaluation (veuillez indiquer le pays et ne pas utiliser de caractères spéciaux)."
    }, "IPHRA_COUNTRY_CODE")$res),   # provide a short name for filenames of output documents (e.g. "POL_PDM")      # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = choose.files(caption = if(language_assessment == "English"){
      "Please select the raw data"
    }else{
      "Veuillez sélectionner les données brutes"
    }, multi = F), 
  filename.tool = choose.files(caption = if(language_assessment == "English"){
    "Please select the kobo tool"
    }else{
      "Veuillez sélectionner le kobo tool"
    }, multi = F)
)
params  <- c(
  fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
  combine_folder = "temp/combine/"
)
# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_deletion_log.R')
# -----------------------------------------------------------------------------`
