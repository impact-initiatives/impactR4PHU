rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::restore(prompt = F)
library(tidyverse)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3)
)
params  <- c(
  fix_sheet_names_to_match = "data",
  combine_folder = "temp/combine/"
)
filename.data <- svDialogs::dlg_open(multiple = F, title = "Please select your raw data excel file.")$res
path.tool <- svDialogs::dlg_open(multiple = F, title = "Please select your Kobo tool file.")$res
filename.daf.tabular = "resources/daf.xlsx"
main.sheets <- readxl::excel_sheets(path.raw.main)
label_colname <- tcltk::tk_select.list(names(readxl::read_excel(path.tool,"survey"))[grepl("label",names(readxl::read_excel(path.tool,"survey")))], title = "Label column to choose", multiple = F)
FSL_indicators <- tcltk::tk_select.list(c("FCS","rCSI","HHS","LCSI","HDDS"), title = "FSL indicators", multiple = T)
# # ------------------------------------------------------------------------------
# ## Detect Enumerator column
# enum_colname <- names(raw.main)[grepl("enum|team",names(raw.main))]
#
# if(length(enum_colname) == 1){
#   yes_no <- svDialogs::dlg_message(paste0("Is '", enum_colname, "' the correct enumerator column?"), type = "yesno")$res
#   enum_colname <- enum_colname
# } else if (length(enum_colname) > 1){
#   enum_colname <- tcltk::tk_select.list(enum_colname, title = "Enumerator Columns")
# } else if (length(enum_colname) == 0 | yes_no == "no") {
#   enum_colname <- svDialogs::dlg_input(message= "Enter the name of the Enumerator Column","enumerator")$res
# }
file <- list.files(pattern = ".Rmd", full.names = T)

rmarkdown::render(file,
                  output_file = paste0("output/", "FSL_Analysis_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
