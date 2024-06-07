
rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::restore(prompt = F)
language <- c(
  language_assessment = svDialogs::dlgList(c("English","French"), title = "Please Select the language.", rstudio = getOption("svDialogs.rstudio", TRUE))$res
)
## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = gsub(" ", "_",svDialogs::dlgInput(if(language['language_assessment'] == "English"){
    "Please provide name of assessment (please fill country and dont use special characters)"
  }else{
    "Veuillez indiquer le nom de l'évaluation (veuillez indiquer le pays et ne pas utiliser de caractères spéciaux)."
  }, "IPHRA_COUNTRY_CODE")$res),
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  filename.data = choose.files(caption  = if(language['language_assessment'] == "English"){
      "Please select the raw data"
    } else{
      "Veuillez sélectionner les données brutes"
    }, multi = F), 
  filename.tool = choose.files(caption = if(language['language_assessment'] == "English"){
    "Please select the kobo tool"
  }else{
    "Veuillez sélectionner le kobo tool"
  }, multi = F)
)

params  <- c(
  fix_sheet_names_to_match = "data"
)

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

rmarkdown::render('src/data_quality_plausibility_check.Rmd',
output_file = paste0("./../output/quality_report/", strings['dataset.name.short'], "_Quality_Check_and_Plausibility_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
  