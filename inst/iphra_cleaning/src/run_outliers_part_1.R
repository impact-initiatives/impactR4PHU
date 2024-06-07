rm(list = ls())

load("output/data_log/final_logical.rda")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  n_sd = svDialogs::dlg_list(c(2,3,4), title = if(language_assessment == "English"){
      "Please Select the Number of standard deviation"
    }else{
      "Veuillez sélectionner le nombre d'écarts types"
    }, rstudio = getOption("svDialogs.rstudio", TRUE))$res,
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)    # this one is appended to the end of filenames
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
source('src/first_part_outliers_checks.R')
# -----------------------------------------------------------------------------`