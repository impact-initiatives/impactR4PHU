rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::restore(prompt = F)
language <- c(
  language_assessment = svDialogs::dlgList(c("English","French"), title = "Please Select the language.", rstudio = getOption("svDialogs.rstudio", TRUE))$res# the filename of your data for 
)
## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = gsub(" ", "_",svDialogs::dlgInput(if(language['language_assessment'] == "English"){
    "Please provide name of assessment (please fill country and dont use special characters)"
  }else{
    "Veuillez indiquer le nom de l'évaluation (veuillez indiquer le pays et ne pas utiliser de caractères spéciaux)."
  }, "IPHRA_COUNTRY_CODE")$res),
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),
  population_estimation = svDialogs::dlgInput(if(language['language_assessment'] == "English"){
      "Please provide an estimation of the population of the assessed area"
    }else{
      "Veuillez fournir une estimation de la population de la zone évaluée."
    },"ex: 1500")$res, # this one is appended to the end of filenames
  filename.data = choose.files(caption = if(language['language_assessment'] == "English"){
    "Please select the cleaned data"
  }else{
    "Veuillez sélectionner les données"
  }, multi = F), 
  filename.tool = choose.files(caption = if(language['language_assessment'] == "English"){
    "Please select the kobo tool"
  }else{
    "Veuillez sélectionner le kobo tool"
  }, multi = F),
  filename.daf.tabular = "./../resources/daf.xlsx"# the filename of your data for
)
# loading all packages, functions and the Kobo tool
# ADDITIONAL PARAMETERS WHICH MAY NEED TO BE CHANGED
params  <- c(
fix_sheet_names_to_match = "data",     # this should be one of "tool", "data", or "none"
combine_folder = "./../temp/combine/"
)

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
cat(getwd())
# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
rmarkdown::render('src/analysis_tabular.Rmd',
output_file = paste0("./../output/", strings['dataset.name.short'], "_Tabular_Analysis_", strings['out_date'],".html"))
cat("\n> tabular analysis completed!")
# -----------------------------------------------------------------------------`

