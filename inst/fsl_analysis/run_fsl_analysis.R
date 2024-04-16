rm(list = ls())
chooseCRANmirror(ind = 1)
utils::install.packages("renv")
options(renv.consent = TRUE)
renv::restore(prompt = F)
library(tidyverse)
strings <- c(
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),
  filename.data = svDialogs::dlg_open(multiple = F, title = "Please select your raw data excel file.")$res,
  path.tool = svDialogs::dlg_open(multiple = F, title = "Please select your Kobo tool file.")$res,
  sel_mul_sep =  tcltk::tk_select.list(c("/","_",".","__"), title = "Select multiple separator")
)
params  <- c(
  fix_sheet_names_to_match = "data",
  combine_folder = "temp/combine/"
)

file <- list.files(pattern = ".Rmd", full.names = T)

rmarkdown::render(file,
                  output_file = paste0("output/", "FSL_Analysis_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")
